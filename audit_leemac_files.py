#!/usr/bin/env python3
"""
LeeMac Files Audit Script
Resolves discrepancy between directory contents and categorization file
Generates comprehensive file inventory for YAML manifest migration
"""

import os
import re
import json
from pathlib import Path
from typing import Dict, List, Set, Tuple


def scan_directory_for_lsp_files(directory: str) -> Set[str]:
    """Scan directory recursively for all .lsp files"""
    lsp_files = set()
    for root, dirs, files in os.walk(directory):
        for file in files:
            if file.endswith(".lsp"):
                lsp_files.add(file)
    return lsp_files


def parse_markdown_file_list(markdown_path: str) -> Set[str]:
    """Extract file list from existing categorization markdown"""
    files_in_markdown = set()

    if not os.path.exists(markdown_path):
        print(f"Warning: Markdown file not found at {markdown_path}")
        return files_in_markdown

    with open(markdown_path, "r", encoding="utf-8") as f:
        content = f.read()

    # Extract .lsp filenames from markdown content
    lsp_pattern = r"`([^`]+\.lsp)`"
    matches = re.findall(lsp_pattern, content)

    for match in matches:
        # Extract just the filename from potential paths
        filename = os.path.basename(match)
        if filename.endswith(".lsp"):
            files_in_markdown.add(filename)

    # Also look for filenames without backticks in table entries
    line_pattern = r"([A-Za-z0-9_\-]+V?\d*-?\d*\.lsp)"
    matches = re.findall(line_pattern, content)

    for match in matches:
        files_in_markdown.add(match)

    return files_in_markdown


def extract_command_names(lsp_file_path: str) -> List[str]:
    """Extract command names (defun c:...) from LISP file"""
    commands = []

    try:
        with open(lsp_file_path, "r", encoding="utf-8", errors="ignore") as f:
            content = f.read()

        # Look for (defun c:commandname pattern
        command_pattern = r"\(defun\s+c:([a-zA-Z0-9_\-]+)"
        matches = re.findall(command_pattern, content, re.IGNORECASE)
        commands.extend(matches)

    except Exception as e:
        print(f"Warning: Could not read {lsp_file_path}: {e}")

    return commands


def detect_technologies(lsp_file_path: str) -> List[str]:
    """Detect technologies used in LISP file"""
    technologies = []

    try:
        with open(lsp_file_path, "r", encoding="utf-8", errors="ignore") as f:
            content = f.read()

        # Check for ObjectDBX usage
        if re.search(
            r"ObjectDBX|objectdbx|vla-open.*acad-object", content, re.IGNORECASE
        ):
            technologies.append("ObjectDBX")

        # Check for DCL usage
        if re.search(r"load_dialog|new_dialog|\.dcl", content, re.IGNORECASE):
            technologies.append("DCL")

        # Check for VLR reactors
        if re.search(r"vlr-.*-reactor", content, re.IGNORECASE):
            technologies.append("VLR")

        # Check for ActiveX/COM
        if re.search(r"vlax-|vla-", content, re.IGNORECASE):
            technologies.append("ActiveX")

        # Check for external file operations
        if re.search(r"open.*read.*close|findfile", content, re.IGNORECASE):
            technologies.append("FileIO")

    except Exception as e:
        print(f"Warning: Could not analyze {lsp_file_path}: {e}")

    return technologies


def analyze_file_complexity(lsp_file_path: str) -> Tuple[str, int]:
    """Analyze file complexity based on size and patterns"""
    try:
        with open(lsp_file_path, "r", encoding="utf-8", errors="ignore") as f:
            lines = f.readlines()

        line_count = len(lines)

        # Determine complexity based on line count and patterns
        if line_count < 100:
            complexity = "Low"
        elif line_count < 500:
            complexity = "Medium"
        elif line_count < 1500:
            complexity = "High"
        else:
            complexity = "Very High"

        return complexity, line_count

    except Exception as e:
        print(f"Warning: Could not analyze complexity of {lsp_file_path}: {e}")
        return "Unknown", 0


def main():
    """Main audit function"""
    # Configure paths
    base_dir = r"c:\Users\dea29431\Documents\LOCAL\CAD"
    leemac_dir = os.path.join(base_dir, "LeeMac_lsp_programs")
    markdown_file = os.path.join(
        base_dir, "reports", "LeeMac_Complete_File_Categorization.md"
    )

    print("=== LeeMac Files Audit ===")
    print(f"Scanning directory: {leemac_dir}")
    print(f"Analyzing markdown: {markdown_file}")
    print()

    # Scan filesystem
    directory_files = scan_directory_for_lsp_files(leemac_dir)
    print(f"Files found in directory: {len(directory_files)}")

    # Parse markdown
    markdown_files = parse_markdown_file_list(markdown_file)
    print(f"Files referenced in markdown: {len(markdown_files)}")
    print()

    # Calculate differences
    only_in_directory = directory_files - markdown_files
    only_in_markdown = markdown_files - directory_files
    in_both = directory_files & markdown_files

    print("=== AUDIT RESULTS ===")
    print(f"Files in both: {len(in_both)}")
    print(f"Only in directory: {len(only_in_directory)}")
    print(f"Only in markdown: {len(only_in_markdown)}")
    print()

    if only_in_directory:
        print("FILES MISSING FROM MARKDOWN:")
        for file in sorted(only_in_directory):
            print(f"  - {file}")
        print()

    if only_in_markdown:
        print("FILES REFERENCED BUT NOT FOUND:")
        for file in sorted(only_in_markdown):
            print(f"  - {file}")
        print()

    # Generate detailed analysis for a sample of files
    print("=== SAMPLE TECHNICAL ANALYSIS ===")
    sample_files = list(directory_files)[:5]  # Analyze first 5 files

    detailed_analysis = {}

    for filename in sample_files:
        file_path = os.path.join(leemac_dir, filename)

        commands = extract_command_names(file_path)
        technologies = detect_technologies(file_path)
        complexity, line_count = analyze_file_complexity(file_path)

        detailed_analysis[filename] = {
            "commands": commands,
            "technologies": technologies,
            "complexity": complexity,
            "line_count": line_count,
        }

        print(f"{filename}:")
        print(f"  Commands: {commands}")
        print(f"  Technologies: {technologies}")
        print(f"  Complexity: {complexity} ({line_count} lines)")
        print()

    # Save audit results
    audit_results = {
        "summary": {
            "directory_file_count": len(directory_files),
            "markdown_file_count": len(markdown_files),
            "files_in_both": len(in_both),
            "missing_from_markdown": len(only_in_directory),
            "missing_from_directory": len(only_in_markdown),
        },
        "files": {
            "directory_files": sorted(list(directory_files)),
            "markdown_files": sorted(list(markdown_files)),
            "only_in_directory": sorted(list(only_in_directory)),
            "only_in_markdown": sorted(list(only_in_markdown)),
            "in_both": sorted(list(in_both)),
        },
        "sample_analysis": detailed_analysis,
    }

    # Write to reports directory
    reports_dir = os.path.join(base_dir, "reports")
    os.makedirs(reports_dir, exist_ok=True)

    audit_file = os.path.join(reports_dir, "leemac_files_audit.json")
    with open(audit_file, "w", encoding="utf-8") as f:
        json.dump(audit_results, f, indent=2)

    print(f"Detailed audit results saved to: {audit_file}")
    print()
    print("=== RECOMMENDATIONS ===")
    print("1. Use the 'directory_files' list as the authoritative file inventory")
    print("2. Review 'only_in_markdown' entries for potential typos or moved files")
    print("3. Add 'only_in_directory' files to the categorization system")
    print("4. Use the sample_analysis patterns for automated YAML generation")


if __name__ == "__main__":
    main()
