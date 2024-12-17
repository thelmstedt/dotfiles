#!/usr/bin/env -S uv run
# /// script
# requires-python = ">=3.12"
# dependencies = [
#     "click", "rich", "dateparser"
# ]
# ///
#!/usr/bin/env python3

import click
from rich.console import Console
from datetime import datetime, timezone
from dateparser import parse
import subprocess
from typing import Optional

console = Console()

def run_git_command(cmd: list[str]) -> str:
    result = subprocess.run(cmd, capture_output=True, text=True)
    if result.returncode != 0:
        console.print(f"[red]Error running git command: {' '.join(cmd)}[/red]")
        console.print(f"[red]Error: {result.stderr}[/red]")
        exit(1)
    return result.stdout.strip()

def get_default_branch() -> str:
    for branch in ['main', 'master']:
        result = subprocess.run(['git', 'show-ref', '--verify', '--quiet', f'refs/remotes/origin/{branch}'])
        if result.returncode == 0:
            return f"origin/{branch}"
    return "origin/main"

def get_branch_counts(branch: str, default_branch: str) -> tuple[int, int]:
    try:
        output = run_git_command(['git', 'rev-list', '--left-right', '--count', f'{default_branch}...{branch}'])
        behind, ahead = map(int, output.split())
        return behind, ahead
    except:
        return 0, 0

def parse_since(since: str) -> datetime:
    parsed_date = parse(since, settings={'RELATIVE_BASE': datetime.now(timezone.utc)})
    if parsed_date is None:
        raise click.BadParameter(f"Could not parse date: {since}")
    return parsed_date.astimezone(timezone.utc)

@click.command()
@click.option('--since', default='6 months ago', help='Show branches updated since (e.g. "2 weeks ago", "3 months ago", "2023-01-01")')
def main(since: str):
    """Show git branch status"""
    since_date = parse_since(since)
    default_branch = get_default_branch()

    branches = run_git_command([
        'git', 'for-each-ref',
        '--sort=-committerdate',
        f'--format=%(committerdate:iso)|%(refname:short)|%(refname)|%(authorname)|%(subject)',
        'refs/remotes/', 'refs/heads/'
    ])

    for line in branches.split('\n'):
        if not line:
            continue

        date_str, branch, fullref, author, message = line.split('|')
        commit_date = datetime.fromisoformat(date_str.strip()).astimezone(timezone.utc)

        if commit_date < since_date:
            continue

        is_remote = fullref.startswith('refs/remotes/')
        branch_style = "bright_blue" if is_remote else "bright_green"

        if branch != default_branch:
            behind, ahead = get_branch_counts(branch, default_branch)
            status = f"[bright_red]↓{behind}[/bright_red] [bright_green]↑{ahead}[/bright_green]"
        else:
            status = "[bright_blue](default)[/bright_blue]"

        relative_date = run_git_command(['git', 'log', '-1', '--format=%cr', branch])

        console.print(
            f"[bright_red]{relative_date}[/bright_red] | "
            f"[{branch_style}]{branch}[/{branch_style}] | "
            f"[bright_magenta]{author}[/bright_magenta] | "
            f"{message} | "
            f"{status}"
        )

if __name__ == '__main__':
    main()