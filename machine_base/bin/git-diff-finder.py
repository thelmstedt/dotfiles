#!/usr/bin/env python3
import subprocess
import argparse
import re
from dataclasses import dataclass
from typing import Dict, List

@dataclass
class FileDiff:
    filename: str
    diff_lines: List[str]

def get_files_with_pattern(pattern: str, staged: bool = False, case_insensitive: bool = False) -> Dict[str, FileDiff]:
    """Find files and their diff lines containing pattern in git diff"""
    if staged:
        cmd = ['git', 'diff', 'HEAD']
    else:
        cmd = ['git', 'diff']

    try:
        git_diff = subprocess.check_output(cmd, text=True)
    except subprocess.CalledProcessError as e:
        print(f"Error running git diff: {e}")
        return {}

    flags = re.IGNORECASE if case_insensitive else 0
    pattern_re = re.compile(pattern, flags)

    current_file = None
    files_with_pattern: Dict[str, FileDiff] = {}
    in_diff = False

    for line in git_diff.split('\n'):
        if line.startswith('diff --git'):
            current_file = line.split(' b/')[-1]
            in_diff = True
        elif in_diff and line.startswith('+') and pattern_re.search(line):
            if current_file not in files_with_pattern:
                files_with_pattern[current_file] = FileDiff(current_file, [])
            files_with_pattern[current_file].diff_lines.append(line)

    return files_with_pattern

def main():
    parser = argparse.ArgumentParser(description='Find files containing pattern in git diff')
    parser.add_argument('pattern', help='Regex pattern to search for')
    parser.add_argument('--staged', action='store_true', help='Search in staged changes')
    parser.add_argument('-i', '--ignore-case', action='store_true', help='Case insensitive search')
    args = parser.parse_args()

    try:
        files = get_files_with_pattern(args.pattern, args.staged, args.ignore_case)
        if files:
            for filename, diff in sorted(files.items()):
                print(f"\n{filename}:")
                for line in diff.diff_lines:
                    print(f"  {line}")
        else:
            print(f"No files found containing '{args.pattern}' in diff")
    except re.error as e:
        print(f"Invalid regular expression: {e}")

if __name__ == '__main__':
    main()