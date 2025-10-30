# /// script
# requires-python = ">=3.12"
# dependencies = [
# "click",
# "rich",
# ]
# ///
import os
import subprocess
from dataclasses import dataclass
from datetime import datetime, timezone
from enum import Enum
from typing import Optional, List, Dict, Tuple

import click
from rich.console import Console
from rich.table import Table

console = Console()


# =============================================================================
# GIT OPERATIONS
# =============================================================================

def run_git_command(cmd: List[str], silent: bool = False, timeout: int = 10) -> Optional[str]:
    """run git command, return output or None if failed"""
    try:
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=timeout)
    except subprocess.TimeoutExpired:
        if not silent:
            console.print(f"[yellow]git command timed out: {' '.join(cmd)}[/yellow]")
        return None

    if result.returncode != 0:
        is_expected = any(
            [
                cmd[:2] == ["git", "log"] and result.returncode == 1,
                cmd[:3] == ["git", "ls-files", "--error-unmatch"] and result.returncode == 1,
                cmd[:2] == ["git", "rev-list"] and result.returncode == 1,
                cmd[:2] == ["git", "show-ref"] and result.returncode == 1,
                cmd[:2] == ["git", "rev-parse"] and "--verify" in cmd and result.returncode == 1
            ]
        )

        if not is_expected and not silent:
            console.print(f"[red]git error ({' '.join(cmd)}): {result.stderr.strip()}[/red]")
        return None
    return result.stdout.strip()


def get_current_user() -> str:
    """get current git user"""
    return run_git_command(['git', 'config', 'user.name'], silent=True) or "unknown"


def get_current_branch() -> Optional[str]:
    """get current checked out branch"""
    return run_git_command(['git', 'branch', '--show-current'], silent=True)


def get_default_branch() -> str:
    """find the default branch (master or main)"""
    for branch in ['main', 'master']:
        if run_git_command(['git', 'show-ref', '--verify', '--quiet', f'refs/heads/{branch}'], silent=True) is not None:
            return branch

    for branch in ['main', 'master']:
        if run_git_command(
                ['git', 'show-ref', '--verify', '--quiet', f'refs/remotes/origin/{branch}'],
                silent=True
        ) is not None:
            return f'origin/{branch}'

    return 'main'


def get_branch_comparison(branch: str, base_branch: str) -> Optional[Tuple[int, int]]:
    """get behind/ahead counts for branch vs base_branch"""
    output = run_git_command(
        ['git', 'rev-list', '--left-right', '--count', f'{base_branch}...{branch}'],
        silent=True,
        timeout=5
    )
    if not output:
        return None

    try:
        behind, ahead = map(int, output.split())
        return behind, ahead
    except (ValueError, IndexError):
        return None


def branches_differ(local_branch: str, remote_branch: str) -> bool:
    """check if local and remote branches point to different commits"""
    local_hash = run_git_command(['git', 'rev-parse', '--verify', f'refs/heads/{local_branch}'], silent=True)
    remote_hash = run_git_command(['git', 'rev-parse', '--verify', f'refs/remotes/origin/{remote_branch}'], silent=True)

    return local_hash and remote_hash and local_hash != remote_hash


def get_git_root() -> Optional[str]:
    """get git repository root directory"""
    return run_git_command(['git', 'rev-parse', '--show-toplevel'], silent=True)


def is_git_repo() -> bool:
    """check if we're in a git repository"""
    return run_git_command(['git', 'rev-parse', '--is-inside-work-tree'], silent=True) == "true"


def fetch_remotes() -> bool:
    """fetch from all remotes, return success"""
    return run_git_command(['git', 'fetch', '--all', '--prune'], silent=True, timeout=30) is not None


# =============================================================================
# UTILITIES
# =============================================================================

def parse_iso_date(iso_date: Optional[str]) -> float:
    """parse iso date to unix epoch, 0.0 if invalid"""
    if not iso_date:
        return 0.0

    try:
        return datetime.fromisoformat(iso_date).timestamp()
    except ValueError:
        if len(iso_date) > 5 and iso_date[-5] in '+-' and ':' not in iso_date[-3:]:
            try:
                fixed = iso_date[:-2] + ':' + iso_date[-2:]
                return datetime.fromisoformat(fixed).timestamp()
            except ValueError:
                pass
        return 0.0


# author color palette
AUTHOR_COLORS = [
    "green", "yellow", "blue", "magenta", "cyan", "red",
    "bright_green", "bright_yellow", "bright_blue", "bright_magenta",
    "bright_cyan", "bright_red", "spring_green1", "deep_pink2",
    "dark_orange", "sea_green1", "deep_sky_blue2", "gold1"
]


def get_author_color(author: str, color_map: Dict[str, str]) -> str:
    """get consistent color for author"""
    if author not in color_map:
        color_map[author] = AUTHOR_COLORS[len(color_map) % len(AUTHOR_COLORS)]
    return color_map[author]


# =============================================================================
# DATA MODELS
# =============================================================================

class BranchType(Enum):
    LOCAL_ONLY = "local_only"  # has local, no remote
    TRACKED = "tracked"  # has both local and remote
    REMOTE_ONLY = "remote_only"  # has remote, no local


@dataclass
class BranchCommit:
    branch_ref: str
    commit_hash: str
    commit_date: str
    author: str
    branch_type: BranchType
    relative_date_str: Optional[str] = None
    behind: Optional[int] = None
    ahead: Optional[int] = None
    is_diverged: bool = False

    @property
    def commit_epoch(self) -> float:
        return parse_iso_date(self.commit_date)

    @property
    def is_main_branch(self) -> bool:
        return self.branch_ref.endswith(('/main', '/master'))

    @property
    def branch_name(self) -> str:
        for prefix in ('refs/heads/', 'refs/remotes/'):
            if self.branch_ref.startswith(prefix):
                return self.branch_ref[len(prefix):]
        return self.branch_ref

    @property
    def is_valid(self) -> bool:
        return self.commit_epoch > 0.0

    @property
    def relative_date(self) -> str:
        return self.relative_date_str or "unknown"

    @property
    def is_mine(self) -> bool:
        current_user = get_current_user()
        return self.author == current_user

    @property
    def is_current(self) -> bool:
        current_branch = get_current_branch()
        return current_branch and self.branch_name == current_branch

    @property
    def comparison_status(self) -> str:
        """format behind/ahead status"""
        if self.behind is None or self.ahead is None:
            return ""

        parts = []
        if self.behind > 0:
            parts.append(f"↓{self.behind}")
        if self.ahead > 0:
            parts.append(f"↑{self.ahead}")

        return " ".join(parts) if parts else "up to date"

    @property
    def is_merged(self) -> bool:
        """check if branch is merged (only behind, not ahead)"""
        return (self.behind is not None and self.ahead is not None and
                self.behind > 0 and self.ahead == 0)


# =============================================================================
# BRANCH LOGIC
# =============================================================================

def categorize_branches() -> Dict[str, Tuple[BranchType, bool]]:
    """categorize all branches into local_only, tracked, remote_only"""
    local_branches = set()
    remote_branches = set()

    # get all local branches
    local_output = run_git_command(['git', 'for-each-ref', '--format=%(refname:short)', 'refs/heads'])
    if local_output:
        local_branches = set(local_output.splitlines())

    # get all remote branches
    remote_output = run_git_command(['git', 'for-each-ref', '--format=%(refname:short)', 'refs/remotes/origin'])
    if remote_output:
        remote_branches = {branch.replace('origin/', '') for branch in remote_output.splitlines() if
                           not branch.endswith('/HEAD')}

    categories = {}

    # categorize each branch
    for branch in local_branches:
        if branch in remote_branches:
            # tracked branch - check if diverged
            is_diverged = branches_differ(branch, branch)
            categories[branch] = (BranchType.TRACKED, is_diverged)
        else:
            # local only
            categories[branch] = (BranchType.LOCAL_ONLY, False)

    # remote only branches
    for branch in remote_branches:
        if branch not in local_branches:
            categories[f'origin/{branch}'] = (BranchType.REMOTE_ONLY, False)

    return categories


def get_latest_commit(
        branch_name: str,
        branch_type: BranchType,
        filepath: str,
        base_branch: str,
        is_diverged: bool = False
) -> Optional[BranchCommit]:
    """get latest commit for branch"""
    # determine the ref to use
    if branch_type == BranchType.REMOTE_ONLY:
        branch_ref = f'refs/remotes/{branch_name}'
    else:
        # for tracked and local_only, use local ref
        branch_ref = f'refs/heads/{branch_name}'

    cmd = ['git', 'log', '-1', '--pretty=format:%H%x09%cI%x09%aN%x09%cr', branch_ref]
    if filepath != '.':
        cmd.extend(['--', filepath])

    commit_info = run_git_command(cmd, silent=True, timeout=5)
    if not commit_info:
        return None

    parts = commit_info.split('\t')
    if len(parts) < 4:
        return None

    behind, ahead = None, None
    if branch_name != base_branch:
        comparison = get_branch_comparison(branch_name, base_branch)
        if comparison:
            behind, ahead = comparison

    return BranchCommit(
        branch_ref=branch_ref,
        commit_hash=parts[0],
        commit_date=parts[1],
        author=parts[2],
        branch_type=branch_type,
        relative_date_str=parts[3],
        behind=behind,
        ahead=ahead,
        is_diverged=is_diverged
    )


# =============================================================================
# DISPLAY LOGIC
# =============================================================================

def render_main_result(latest: BranchCommit, target: str, base: str, author_colors: Dict[str, str]):
    """render the main latest commit result"""
    console.print(f'\n[green]latest {target}:[/green]')
    console.print(
        f'  branch: [blue]{latest.branch_name}[/blue]' + (
            ' [black on white] CURRENT [/black on white]' if latest.is_current else '')
    )
    console.print(f'  commit: [blue]{latest.commit_hash}[/blue]')

    author_color = get_author_color(latest.author, author_colors)
    author_display = f'[{author_color}]{latest.author}[/{author_color}]'
    if latest.is_mine:
        author_display += ' [black on yellow] YOU [/black on yellow]'
    console.print(f'  author: {author_display}')

    console.print(f'  when: {latest.relative_date}')
    if latest.comparison_status:
        console.print(f'  vs {base}: {latest.comparison_status}')


def render_branch_group(
        commits: List[BranchCommit],
        title: str,
        latest: BranchCommit,
        author_colors: Dict[str, str],
        show_diverged: bool = False
):
    """render a group of branches in a table"""
    if not commits:
        return

    console.print(f'\n[blue]{title}:[/blue]')
    table = Table()
    table.add_column('branch', style='blue')
    table.add_column('commit', style='blue')
    table.add_column('author')
    table.add_column('when', style='black')
    table.add_column('vs base', style='black')

    for commit in commits:
        branch_name = commit.branch_name

        if commit == latest:
            branch_name = f'✨ {branch_name}'
        elif commit.is_current:
            branch_name = f'[black on white]{branch_name} ← CURRENT[/black on white]'
        elif show_diverged and commit.is_diverged:
            branch_name = f'[red]{branch_name} (DIVERGED)[/red]'

        author_color = get_author_color(commit.author, author_colors)
        author_display = f'[{author_color}]{commit.author}[/{author_color}]'
        if commit.is_mine:
            author_display += ' (you)'

        table.add_row(
            branch_name,
            commit.commit_hash[:8],
            author_display,
            commit.relative_date,
            commit.comparison_status
        )

    console.print(table)


def render_merged_branches(commits: List[BranchCommit]):
    """render merged branches for easy deletion"""
    merged = [c for c in commits if c.is_merged and not c.is_main_branch and not c.is_current]

    if not merged:
        console.print('\n[green]no merged branches to delete[/green]')
        return

    console.print(f'\n[yellow]merged branches ({len(merged)} total):[/yellow]')

    # sort by date (oldest first for cleanup)
    merged.sort(key=lambda c: c.commit_epoch)

    local_branches = []
    remote_branches = []

    for commit in merged:
        branch_name = commit.branch_name

        # collect local branches that can be deleted
        if commit.branch_type in (BranchType.LOCAL_ONLY, BranchType.TRACKED):
            if not branch_name.startswith('origin/'):
                local_branches.append(branch_name)

        # collect remote branches (check if corresponding remote exists)
        if commit.branch_type == BranchType.TRACKED:
            # for tracked branches, we know there's a remote
            remote_branches.append(branch_name)
        elif commit.branch_type == BranchType.REMOTE_ONLY and branch_name.startswith('origin/'):
            # for remote-only, strip the origin/ prefix
            remote_branches.append(branch_name.replace('origin/', ''))

        console.print(f'  [dim]{commit.relative_date}[/dim] [red]{branch_name}[/red] [dim]({commit.author})[/dim]')

    if local_branches or remote_branches:
        console.print(f'\n[dim]copy/paste to delete:[/dim]')

        if local_branches:
            console.print(f'git branch -D {" ".join(local_branches)}')

        if remote_branches:
            # modern syntax is cleaner than the old :branch syntax

            for branch in remote_branches:
                console.print(f'git checkout {branch}')

            for branch in remote_branches:
                console.print(f'git push origin --delete {branch}')


def render_commands(latest: BranchCommit, filepath: str):
    """render helpful git commands"""
    console.print(f'\n[dim]commands:[/dim]')
    if filepath != '.':
        console.print(f'  view file: git show {latest.commit_hash}:"./{filepath}"')
    console.print(f'  checkout commit: git checkout {latest.commit_hash}')
    console.print(f'  checkout branch: git checkout {latest.branch_name}')


# =============================================================================
# MAIN CLI
# =============================================================================

@click.command()
@click.option(
    '--single', is_flag=True,
    help='show only the single latest commit (no table)'
)
@click.option(
    '--base', default=None,
    help='base branch to compare against (default: auto-detect master/main)'
)
@click.option(
    '--no-fetch', is_flag=True,
    help='skip fetching from remotes'
)
@click.option(
    '--list-merged', is_flag=True,
    help='show merged branches ready for deletion'
)
@click.argument('filepath', default='.')
def main(single: bool, base: Optional[str], no_fetch: bool, list_merged: bool, filepath: str):
    """find latest commit across all git branches"""

    if not is_git_repo():
        console.print('[red]error: not a git repository[/red]')
        raise click.Abort()

    if base is None:
        base = get_default_branch()

    # handle relative paths properly
    if filepath != '.':
        if not os.path.exists(filepath):
            if not run_git_command(['git', 'ls-files', '--error-unmatch', filepath], silent=True):
                console.print(f'[yellow]warning: {filepath} not found locally or tracked[/yellow]')

        git_root = get_git_root()
        if git_root:
            try:
                abs_path = os.path.abspath(filepath)
                filepath = os.path.relpath(abs_path, git_root).replace(os.path.sep, '/')
            except:
                pass

    target = 'whole repository' if filepath == '.' else f"'{filepath}'"
    console.print(f'[blue]searching for latest {target} (base: {base})[/blue]')

    if not no_fetch:
        console.print('[dim]fetching from remotes...[/dim]')
        if not fetch_remotes():
            console.print('[yellow]fetch failed, using local info[/yellow]')

    # categorize all branches
    branch_categories = categorize_branches()
    if not branch_categories:
        console.print('[red]no branches found[/red]')
        raise click.Abort()

    all_commits = []
    author_colors = {}

    for branch_name, (branch_type, is_diverged) in branch_categories.items():
        if commit := get_latest_commit(branch_name, branch_type, filepath, base, is_diverged):
            all_commits.append(commit)

    valid_commits = [c for c in all_commits if c.is_valid]
    if not valid_commits:
        console.print(f'[red]no commits found for {target}[/red]')
        return

    # group by type for display
    local_only = [c for c in valid_commits if c.branch_type == BranchType.LOCAL_ONLY]
    tracked = [c for c in valid_commits if c.branch_type == BranchType.TRACKED]
    remote_only = [c for c in valid_commits if c.branch_type == BranchType.REMOTE_ONLY]

    # sort each group
    for group in [local_only, tracked, remote_only]:
        group.sort(key=lambda c: (c.commit_epoch, c.is_main_branch), reverse=True)

    # find overall latest
    all_sorted = sorted(valid_commits, key=lambda c: (c.commit_epoch, c.is_main_branch), reverse=True)
    latest = all_sorted[0]

    # render output
    if not list_merged:
        render_main_result(latest, target, base, author_colors)

        if not single:
            render_branch_group(tracked, "tracked branches", latest, author_colors, show_diverged=True)
            render_branch_group(local_only, "local only branches", latest, author_colors)
            render_branch_group(remote_only, "remote only branches", latest, author_colors)

    # always show merged branches if requested
    if list_merged:
        render_merged_branches(valid_commits)


if __name__ == '__main__':
    main()