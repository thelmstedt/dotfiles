#!/usr/bin/env python3
import os
import subprocess
import curses
from datetime import datetime
import click

def get_ignored_revs():
    """Get list of revisions to ignore from .git-blame-ignore-revs file"""
    ignore_file = os.path.join(os.getcwd(), '.git-blame-ignore-revs')
    ignored_revs = set()

    if os.path.exists(ignore_file):
        try:
            with open(ignore_file, 'r') as f:
                for line in f:
                    line = line.strip()
                    # Skip comments and empty lines
                    if line and not line.startswith('#'):
                        ignored_revs.add(line)
        except Exception:
            pass

    return ignored_revs

def get_all_commits_batch():
    """Get all commits with their dates and messages in one batch operation"""
    try:
        # Get list of revisions to ignore
        ignored_revs = get_ignored_revs()

        result = subprocess.run(
            ["git", "log", "--name-only", "--format=%H %ct %s"],
            capture_output=True, text=True, check=True
        )

        file_to_commit = {}  # {path: (hash, timestamp, message)}
        current_hash = None
        current_timestamp = None
        current_message = None

        for line in result.stdout.split('\n'):
            if not line.strip():
                continue

            if ' ' in line and not line.startswith(' '):
                # This is a commit line with hash, timestamp, and message
                parts = line.split(' ', 2)  # Split into at most 3 parts
                if len(parts) >= 2:
                    current_hash = parts[0]
                    # Skip this commit if it's in the ignore list
                    if current_hash in ignored_revs:
                        current_hash = None
                        current_timestamp = None
                        current_message = None
                        continue

                    current_timestamp = int(parts[1])
                    current_message = parts[2] if len(parts) > 2 else ""
            else:
                # This is a file path
                file_path = line.strip()
                if file_path and current_hash and current_timestamp:
                    # Get the directory
                    dir_path = os.path.dirname(file_path) or '.'
                    # Only keep the most recent commit
                    if dir_path not in file_to_commit or current_timestamp > file_to_commit[dir_path][1]:
                        file_to_commit[dir_path] = (current_hash, current_timestamp, current_message)

        # Convert to more usable format
        return {
            path: {
                'date': datetime.fromtimestamp(data[1]),
                'hash': data[0],
                'message': data[2]
            }
            for path, data in file_to_commit.items()
        }
    except subprocess.CalledProcessError:
        return {}

class DirNode:
    def __init__(self, name, path, commit_info=None, parent=None):
        self.name = name
        self.path = path
        self.commit_info = commit_info or {}  # {date, hash, message}
        self.parent = parent
        self.children = []
        self.expanded = False

    @property
    def date(self):
        return self.commit_info.get('date')

    @property
    def hash(self):
        return self.commit_info.get('hash', '')

    @property
    def message(self):
        return self.commit_info.get('message', '')

    @property
    def age_days(self):
        if not self.date:
            return 0
        return (datetime.now() - self.date).days

    def add_child(self, name, path, commit_info=None):
        child = DirNode(name, path, commit_info, parent=self)
        self.children.append(child)
        return child

    def sort_children_by_date(self):
        self.children.sort(key=lambda x: x.date or datetime.max)
        for child in self.children:
            child.sort_children_by_date()

def build_tree(dir_commits, root_path, min_days=None):
    now = datetime.now()
    root = DirNode(os.path.basename(root_path) or root_path, root_path)

    # Process the commits
    for path, commit_info in dir_commits.items():
        # Convert git-relative path to absolute path
        abs_path = os.path.normpath(os.path.join(os.getcwd(), path))

        # Check if it's under root_path
        if not abs_path.startswith(root_path):
            continue

        # Check age filter
        if min_days is not None and commit_info.get('date'):
            age_days = (now - commit_info['date']).days
            if age_days < min_days:
                continue

        # Get path relative to root
        rel_path = os.path.relpath(abs_path, root_path)
        if rel_path == '.':
            # This is the root directory itself
            root.commit_info = commit_info
            continue

        # Split into components
        components = rel_path.split(os.sep)

        # Build tree
        current = root
        path_so_far = root_path

        for i, component in enumerate(components):
            path_so_far = os.path.join(path_so_far, component)

            # Look for existing child
            found = None
            for child in current.children:
                if child.name == component:
                    found = child
                    break

            if not found:
                found = current.add_child(component, path_so_far)

            current = found

            # If this is the final component, set the commit info
            if i == len(components) - 1:
                current.commit_info = commit_info

    # Propagate commit info up the tree
    def propagate_dates(node):
        if not node.children:
            return node.date

        child_dates = []
        for child in node.children:
            child_date = propagate_dates(child)
            if child_date:
                child_dates.append((child_date, child.commit_info))

        if child_dates:
            # Get newest date and its commit info
            newest = max(child_dates, key=lambda x: x[0])
            if not node.date or newest[0] > node.date:
                node.commit_info = newest[1]

        return node.date

    propagate_dates(root)
    root.sort_children_by_date()

    return root

def draw_tree(stdscr, tree_root, min_days=None):
    curses.curs_set(0)  # Hide cursor
    curses.start_color()
    curses.use_default_colors()

    # Define color pairs
    curses.init_pair(1, curses.COLOR_GREEN, -1)    # < 180 days
    curses.init_pair(2, curses.COLOR_YELLOW, -1)   # 180-365 days
    curses.init_pair(3, curses.COLOR_RED, -1)      # > 365 days
    curses.init_pair(4, curses.COLOR_WHITE, curses.COLOR_BLUE)  # Selection

    # Initialize visible nodes
    visible_nodes = []

    def update_visible_nodes(node, level=0):
        visible_nodes.append((node, level))
        if node.expanded:
            for child in node.children:
                update_visible_nodes(child, level + 1)

    # Add root node to visible nodes
    tree_root.expanded = True
    update_visible_nodes(tree_root)

    # Initialize state
    current_pos = 0
    offset = 0

    # Main loop
    while True:
        # Get screen dimensions
        max_y, max_x = stdscr.getmaxyx()

        stdscr.clear()

        # Display header (be careful of bottom-right corner)
        header = f" Git Age Explorer - {tree_root.path} "
        if len(header) >= max_x:
            header = header[:max_x-4] + "... "

        # Avoid writing to the bottom-right corner
        if len(header) < max_x:
            stdscr.addstr(0, 0, header.ljust(max_x-1), curses.A_REVERSE)
        else:
            stdscr.addstr(0, 0, header[:max_x-1], curses.A_REVERSE)

        # Display help (careful of the last character position)
        help_text = " UP/DOWN: Navigate | ENTER: Expand/Collapse | q: Quit "
        if len(help_text) >= max_x:
            help_text = help_text[:max_x-1]

        # Avoid writing to the bottom-right corner
        stdscr.addstr(max_y-1, 0, help_text.ljust(max_x-1), curses.A_REVERSE)

        # Display visible nodes
        display_range = min(max_y - 2, len(visible_nodes) - offset)
        for i in range(display_range):
            if i + offset >= len(visible_nodes):
                break

            node, level = visible_nodes[i + offset]

            # Format line
            indent = "  " * level
            if node.children:
                prefix = "+ " if node.expanded else "> "
            else:
                prefix = "  "

            # Get date and commit info
            date_str = node.date.strftime("%Y-%m-%d") if node.date else "N/A"
            age_days = node.age_days

            # Get abbreviated hash and message
            short_hash = node.hash[:7] if node.hash else ""
            message = node.message
            if len(message) > 30:
                message = message[:27] + "..."

            # Choose color based on age
            if age_days > 365:
                color = curses.color_pair(3)
            elif age_days > 180:
                color = curses.color_pair(2)
            else:
                color = curses.color_pair(1)

            # Create line
            line = f"{indent}{prefix}{node.name} [{date_str}] {age_days}d [{short_hash}] {message}"

            # Display with highlight if current position
            attr = color
            if i + offset == current_pos:
                attr = curses.color_pair(4)

            # Ensure line fits in screen width
            if len(line) >= max_x:
                line = line[:max_x-4] + "..."

            # Avoid writing to the last column
            if len(line) < max_x:
                stdscr.addstr(i + 1, 0, line, attr)
            else:
                stdscr.addstr(i + 1, 0, line[:max_x-1], attr)

        # Refresh screen
        stdscr.refresh()

        # Get user input
        key = stdscr.getch()

        # Handle navigation
        if key == curses.KEY_UP and current_pos > 0:
            current_pos -= 1
            if current_pos < offset:
                offset = current_pos
        elif key == curses.KEY_DOWN and current_pos < len(visible_nodes) - 1:
            current_pos += 1
            if current_pos >= offset + max_y - 2:
                offset = current_pos - (max_y - 3)
        elif key == curses.KEY_NPAGE:  # Page Down
            current_pos = min(current_pos + (max_y - 3), len(visible_nodes) - 1)
            offset = min(offset + (max_y - 3), len(visible_nodes) - (max_y - 2))
        elif key == curses.KEY_PPAGE:  # Page Up
            current_pos = max(current_pos - (max_y - 3), 0)
            offset = max(offset - (max_y - 3), 0)
        elif key == 10:  # Enter key
            if current_pos < len(visible_nodes):
                node = visible_nodes[current_pos][0]
                if node.children:
                    node.expanded = not node.expanded

                    # Rebuild visible nodes list
                    visible_nodes = []
                    update_visible_nodes(tree_root)

                    # Adjust current position if needed
                    if current_pos >= len(visible_nodes):
                        current_pos = len(visible_nodes) - 1
        elif key == ord('q'):
            break

@click.command()
@click.option('--root', '-r', default='.', help='Root directory to start from')
@click.option('--days', '-d', type=int, default=None, help='Only show directories older than N days')
def main(root, days):
    """Interactive git age explorer (like ncdu)"""
    # Process git data
    print("processing git history (this might take a while)...")
    dir_commits = get_all_commits_batch()

    # Build directory tree
    root_path = os.path.abspath(root)
    tree = build_tree(dir_commits, root_path, days)

    # Start the curses interface
    try:
        curses.wrapper(lambda stdscr: draw_tree(stdscr, tree, days))
    except Exception as e:
        print(f"error: {e}")

if __name__ == "__main__":
    main()