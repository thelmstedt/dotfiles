#!/usr/bin/env -S uv run --script
# /// script
# dependencies = [
#   "rich",
#   "click"
# ]
# ///

"""
ktopm.py - A script to combine 'kubectl top pods' with memory requests and lifecycle status.
Shows a comparison of used memory vs. requested memory for containers,
including startup status and a watch mode.
"""

import click
import json
import subprocess
import sys
import time
from dataclasses import dataclass, field
from rich.console import Console
from rich.live import Live
from rich.table import Table
from typing import List, Dict, Tuple, Optional


# --- Data Structures ---

@dataclass
class ContainerMetrics:
    """A combined structure for all container data."""
    namespace: str
    pod_name: str
    container_name: str

    # from 'get pods' (spec and status)
    restarts: int = 0
    status: str = "Unknown"
    reason: str = ""
    memory_request: str = "0"
    memory_request_bytes: int = 0

    # from 'top pods' (usage)
    cpu: str = "N/A"
    memory: str = "N/A"
    memory_bytes: int = 0

# --- Utility Functions ---

MEMORY_SUFFIXES = {
    'K': 10**3, 'M': 10**6, 'G': 10**9, 'T': 10**12, 'P': 10**15, 'E': 10**18,
    'Ki': 2**10, 'Mi': 2**20, 'Gi': 2**30, 'Ti': 2**40, 'Pi': 2**50, 'Ei': 2**60,
}

def parse_memory(memory_str: str) -> int:
    """Parses a Kubernetes memory string (e.g., '128Mi', '2Gi') and returns the value in bytes."""
    if not memory_str or not any(char.isdigit() for char in memory_str):
        return 0

    numeric_part_str = "".join(filter(lambda c: c.isdigit() or c == '.', memory_str))
    suffix = "".join(filter(str.isalpha, memory_str))

    if not numeric_part_str:
        return 0

    numeric_part = float(numeric_part_str)

    if suffix in MEMORY_SUFFIXES:
        return int(numeric_part * MEMORY_SUFFIXES[suffix])
    return int(numeric_part) # Assume bytes if no suffix

def format_bytes(b: int) -> str:
    """Converts bytes into a human-readable string (Ki, Mi, Gi)."""
    if abs(b) < MEMORY_SUFFIXES['Ki']:
        return f"{b}B"
    if abs(b) < MEMORY_SUFFIXES['Mi']:
        return f"{b / MEMORY_SUFFIXES['Ki']:.1f}Ki"
    if abs(b) < MEMORY_SUFFIXES['Gi']:
        return f"{b / MEMORY_SUFFIXES['Mi']:.1f}Mi"
    return f"{b / MEMORY_SUFFIXES['Gi']:.1f}Gi"

# --- K8s Data Fetching ---

def run_command(command: str) -> str:
    """Executes a shell command and returns its output."""
    try:
        result = subprocess.run(
            command,
            shell=True,
            check=True,
            capture_output=True,
            text=True,
            timeout=30
        )
        return result.stdout
    except subprocess.CalledProcessError as e:
        console = Console(stderr=True, style="bold red")
        console.print(f"Error executing command: {command}")
        console.print(f"Stderr: {e.stderr.strip() if e.stderr else 'N/A'}")
        if "top" in command:
            console.print("\nHint: This error often means the Kubernetes Metrics Server is not installed or not ready.")
            console.print("Please ensure 'kubectl top pods' works correctly in your cluster.")
        sys.exit(1)
    except FileNotFoundError:
        console = Console(stderr=True, style="bold red")
        console.print("Error: 'kubectl' command not found.")
        console.print("Please ensure kubectl is installed and in your system's PATH.")
        sys.exit(1)
    except subprocess.TimeoutExpired:
        console = Console(stderr=True, style="bold red")
        console.print(f"Error: Command timed out after 30 seconds: {command}")
        sys.exit(1)
    return ""

def get_current_namespace() -> str:
    """Gets the current namespace from the kubectl context."""
    command = "kubectl config view --minify --output 'jsonpath={..namespace}'"
    namespace = run_command(command).strip()
    return namespace if namespace else "default"

def get_pod_details(ns_flag: str) -> Dict[Tuple[str, str, str], ContainerMetrics]:
    """Fetches container specs and statuses from 'kubectl get pods'."""
    command = f"kubectl get pods {ns_flag} -o json"
    output = run_command(command)
    if not output:
        return {}

    pods_data = json.loads(output)
    details = {}

    for pod in pods_data.get('items', []):
        pod_meta = pod.get('metadata', {})
        pod_spec = pod.get('spec', {})
        pod_status = pod.get('status', {})

        namespace = pod_meta.get('namespace', 'N/A')
        pod_name = pod_meta.get('name', 'N/A')

        container_statuses = {cs['name']: cs for cs in pod_status.get('containerStatuses', [])}

        for container in pod_spec.get('containers', []):
            container_name = container['name']

            mem_req = container.get('resources', {}).get('requests', {}).get('memory', '0')

            metrics = ContainerMetrics(
                namespace=namespace,
                pod_name=pod_name,
                container_name=container_name,
                memory_request=mem_req if mem_req != '0' else "0",
                memory_request_bytes=parse_memory(mem_req)
            )

            status_info = container_statuses.get(container_name)
            if status_info:
                metrics.restarts = status_info.get('restartCount', 0)
                state = status_info.get('state', {})
                if 'running' in state:
                    metrics.status = "Running"
                    metrics.reason = ""
                elif 'waiting' in state:
                    metrics.status = "Waiting"
                    metrics.reason = state['waiting'].get('reason', 'N/A')
                elif 'terminated' in state:
                    metrics.status = "Terminated"
                    metrics.reason = state['terminated'].get('reason', 'N/A')
            else:
                metrics.status = pod_status.get('phase', 'Pending')
                metrics.reason = pod_status.get('reason', '')

            details[(namespace, pod_name, container_name)] = metrics

    return details

def get_memory_usage(ns_flag: str, target_namespace: Optional[str] = None) -> Dict[Tuple[str, str, str], Dict]:
    """Fetches container memory usage from 'kubectl top pods'."""
    command = f"kubectl top pods {ns_flag} --containers"
    output = run_command(command)

    lines = output.strip().split('\n')
    if len(lines) < 2:
        return {}

    usage_data = {}
    for line in lines[1:]:
        parts = line.split()
        if not parts:
            continue

        if target_namespace:
            ns, pod, container, cpu, mem = target_namespace, parts[0], parts[1], parts[2], parts[3]
        else:
            ns, pod, container, cpu, mem = parts[0], parts[1], parts[2], parts[3], parts[4]

        usage_data[(ns, pod, container)] = {
            "cpu": cpu,
            "memory": mem,
            "memory_bytes": parse_memory(mem)
        }
    return usage_data

# --- Rendering Logic ---

def style_status(status: str, reason: str) -> str:
    """Applies color to status strings."""
    if status == "Running":
        return f"[green]{status}[/green]"
    if status in ["Terminated", "Failed"] or reason in ["Error", "CrashLoopBackOff", "ImagePullBackOff", "ErrImagePull"]:
        return f"[bold red]{reason or status}[/bold red]"
    if status in ["Waiting", "Pending"] or reason in ["ContainerCreating", "PodInitializing"]:
        return f"[yellow]{reason or status}[/yellow]"
    return status

def render_bytes(overhead_bytes: int) -> str:
    """Renders overhead bytes with color."""
    if overhead_bytes > 0:
        return f"[red]+{format_bytes(overhead_bytes)}[/red]"
    return f"[green]{format_bytes(overhead_bytes)}[/green]"

def render_percent(overhead_percent: float) -> str:
    """Renders overhead percentage with color."""
    if overhead_percent == float('inf'):
        return "[bold red]Inf%[/bold red]"
    if overhead_percent > 25:
        return f"[red]+{overhead_percent:.1f}%[/red]"
    if overhead_percent < -25:
        return f"[green]{overhead_percent:.1f}%[/green]"
    return f"{overhead_percent:+.1f}%"

def generate_table(ns_flag: str, all_namespaces: bool, target_namespace: Optional[str]) -> Table:
    """Fetches all data and generates the rich Table."""
    all_metrics = get_pod_details(ns_flag)
    usage_data = get_memory_usage(ns_flag, None if all_namespaces else target_namespace)

    # Merge usage data into the main metrics dict
    for key, usage in usage_data.items():
        if key in all_metrics:
            all_metrics[key].cpu = usage["cpu"]
            all_metrics[key].memory = usage["memory"]
            all_metrics[key].memory_bytes = usage["memory_bytes"]

    # Sort data for consistent display
    sorted_metrics = sorted(all_metrics.values(), key=lambda m: (m.namespace, m.pod_name, m.container_name))

    table = Table(
        title="Container Memory Usage vs. Requests",
        header_style="bold magenta",
        show_footer=True,
        footer_style="bold"
    )

    # Define columns
    if all_namespaces:
        table.add_column("NAMESPACE", footer="Total")
    table.add_column("POD", footer=f"({len(sorted_metrics)})")
    table.add_column("CONTAINER")
    table.add_column("STATUS")
    table.add_column("RESTARTS", justify="right")
    table.add_column("CPU(cores)")
    table.add_column("MEMORY", footer_style="bold blue")
    table.add_column("REQUESTS", style="cyan", justify="right", footer_style="bold cyan")
    table.add_column("OVERHEAD", justify="right")
    table.add_column("OVERHEAD %", justify="right")

    total_usage_bytes = sum(m.memory_bytes for m in sorted_metrics)
    total_request_bytes = sum(m.memory_request_bytes for m in sorted_metrics)

    for m in sorted_metrics:
        overhead_bytes = m.memory_bytes - m.memory_request_bytes
        overhead_percent = (overhead_bytes / m.memory_request_bytes * 100) if m.memory_request_bytes > 0 else (float('inf') if m.memory_bytes > 0 else 0.0)

        row = [
            m.pod_name,
            m.container_name,
            style_status(m.status, m.reason),
            str(m.restarts),
            m.cpu,
            m.memory,
            m.memory_request,
            render_bytes(overhead_bytes),
            render_percent(overhead_percent)
        ]
        if all_namespaces:
            row.insert(0, m.namespace)

        table.add_row(*row)

    # Calculate and set footer
    total_overhead_bytes = total_usage_bytes - total_request_bytes
    total_overhead_percent = (total_overhead_bytes / total_request_bytes * 100) if total_request_bytes > 0 else (float('inf') if total_usage_bytes > 0 else 0.0)

    mem_col_idx = 6 if all_namespaces else 5
    table.columns[mem_col_idx].footer = format_bytes(total_usage_bytes)
    table.columns[mem_col_idx + 1].footer = format_bytes(total_request_bytes)
    table.columns[mem_col_idx + 2].footer = render_bytes(total_overhead_bytes)
    table.columns[mem_col_idx + 3].footer = render_percent(total_overhead_percent)

    return table

# --- Click Command ---

@click.command(context_settings=dict(help_option_names=['-h', '--help']))
@click.option("-n", "--namespace", default=None, help="Specify the namespace. If omitted, uses the current context's default.")
@click.option("-A", "--all-namespaces", is_flag=True, help="List containers from all namespaces.")
@click.option("-w", "--watch", is_flag=True, help="Watch for changes and refresh the table every 2 seconds.")
def main(namespace: str, all_namespaces: bool, watch: bool):
    """
    Combines 'kubectl top pods' with memory requests and lifecycle status, displaying the result in a table.
    """
    console = Console()

    ns_flag = ""
    target_namespace = None
    if all_namespaces:
        ns_flag = "-A"
    else:
        target_namespace = namespace if namespace else get_current_namespace()
        ns_flag = f"-n {target_namespace}"

    if not watch:
        with console.status("[bold green]Fetching Kubernetes data..."):
            table = generate_table(ns_flag, all_namespaces, target_namespace)
        console.print(table)
    else:
        try:
            with Live(generate_table(ns_flag, all_namespaces, target_namespace), screen=True, auto_refresh=False) as live:
                while True:
                    table = generate_table(ns_flag, all_namespaces, target_namespace)
                    live.update(table, refresh=True)
                    time.sleep(2)
        except KeyboardInterrupt:
            console.print("\n[yellow]Watch stopped.[/yellow]")
            sys.exit(0)
        except Exception as e:
            console.print(f"\n[bold red]An unexpected error occurred: {e}[/bold red]")
            sys.exit(1)


if __name__ == "__main__":
    main()
