#!/usr/bin/env python3
"""
Sample YAML Manifest Generator
Creates test YAML manifests for a few representative LeeMac files
"""

import os
import re
import yaml
from datetime import datetime


def extract_commands_from_file(file_path):
    """Extract command names from LISP file"""
    commands = []
    try:
        with open(file_path, "r", encoding="utf-8", errors="ignore") as f:
            content = f.read()

        # Look for (defun c:commandname pattern
        pattern = r"\(defun\s+c:([a-zA-Z0-9_\-]+)"
        matches = re.findall(pattern, content, re.IGNORECASE)
        commands = list(set(matches))  # Remove duplicates

    except Exception as e:
        print(f"Warning: Could not read {file_path}: {e}")

    return commands


def detect_technologies_in_file(file_path):
    """Detect technologies used in LISP file"""
    technologies = []
    try:
        with open(file_path, "r", encoding="utf-8", errors="ignore") as f:
            content = f.read()

        content_lower = content.lower()

        if "objectdbx" in content_lower or "vla-open" in content_lower:
            technologies.append("ObjectDBX")
        if "load_dialog" in content_lower or ".dcl" in content_lower:
            technologies.append("DCL")
        if "vlr-" in content_lower and "reactor" in content_lower:
            technologies.append("VLR")
        if "vlax-" in content_lower or "vla-" in content_lower:
            technologies.append("ActiveX")
        if "ssget" in content_lower:
            technologies.append("SelectionSets")
        if "open" in content_lower and (
            "read" in content_lower or "write" in content_lower
        ):
            technologies.append("FileIO")

    except Exception as e:
        print(f"Warning: Could not analyze {file_path}: {e}")

    return technologies


def get_file_complexity(file_path):
    """Assess file complexity based on size"""
    try:
        with open(file_path, "r", encoding="utf-8", errors="ignore") as f:
            lines = f.readlines()

        line_count = len(lines)

        if line_count < 100:
            return "beginner", line_count
        elif line_count < 500:
            return "intermediate", line_count
        elif line_count < 1500:
            return "advanced", line_count
        else:
            return "expert", line_count

    except Exception as e:
        print(f"Warning: Could not analyze {file_path}: {e}")
        return "unknown", 0


def extract_description_from_file(file_path):
    """Extract description from file header comments"""
    try:
        with open(file_path, "r", encoding="utf-8", errors="ignore") as f:
            content = f.read()

        # Look for common description patterns in first 2000 characters
        lines = content[:2000].split("\n")

        for line in lines:
            line = line.strip()
            if line.startswith(";;") and len(line) > 20:
                # Clean up the line
                desc = line.replace(";;", "").strip()
                # Remove excessive whitespace
                desc = " ".join(desc.split())

                # Look for sentences that describe functionality
                if any(
                    word in desc.lower()
                    for word in [
                        "enables",
                        "allows",
                        "provides",
                        "implements",
                        "creates",
                        "performs",
                    ]
                ):
                    if len(desc) > 30 and len(desc) < 200:
                        return desc

        return "AutoLISP utility program"

    except Exception as e:
        print(f"Warning: Could not extract description from {file_path}: {e}")
        return "AutoLISP utility program"


def determine_category(filename):
    """Determine category based on filename patterns"""
    name_lower = filename.lower()

    if any(
        word in name_lower
        for word in ["text", "align", "label", "numinc", "curve", "mtext", "field"]
    ):
        return "TEXT_ANNOTATION"
    elif any(word in name_lower for word in ["block", "attribute", "batte", "burst"]):
        return "BLOCK_ATTRIBUTE"
    elif any(
        word in name_lower
        for word in [
            "convex",
            "hull",
            "circle",
            "ellipse",
            "fractal",
            "sierpinski",
            "md5",
        ]
    ):
        return "MATHEMATICAL"
    elif any(word in name_lower for word in ["layer", "drawing", "layout", "xref"]):
        return "LAYER_DRAWING"
    elif any(
        word in name_lower
        for word in ["poly", "outline", "offset", "break", "extend", "centerline"]
    ):
        return "GEOMETRIC"
    elif any(word in name_lower for word in ["listbox", "dialog", "dcl", "browser"]):
        return "USER_INTERFACE"
    elif any(
        word in name_lower
        for word in ["copy", "steal", "files", "csv", "wrapper", "log"]
    ):
        return "UTILITIES"
    else:
        return "SPECIALIZED"


def generate_tags(filename, technologies, category):
    """Generate semantic tags"""
    tags = []
    name_lower = filename.lower()

    # Category-based tags
    if category == "TEXT_ANNOTATION":
        tags.extend(["text", "annotation", "labeling"])
    elif category == "BLOCK_ATTRIBUTE":
        tags.extend(["block", "attribute", "objects"])
    elif category == "MATHEMATICAL":
        tags.extend(["algorithm", "mathematics", "computation"])
    elif category == "GEOMETRIC":
        tags.extend(["geometric", "modification", "construction"])

    # Technology-based tags
    if "ObjectDBX" in technologies:
        tags.append("multi-drawing")
    if "DCL" in technologies:
        tags.append("interactive")
    if "VLR" in technologies:
        tags.append("reactive")

    # Functionality tags
    if "batch" in name_lower or "auto" in name_lower:
        tags.append("automation")
    if "inc" in name_lower or "num" in name_lower:
        tags.append("numbering")

    return list(set(tags))  # Remove duplicates


def create_yaml_manifest(file_path, output_dir):
    """Create YAML manifest for a single file"""
    filename = os.path.basename(file_path)
    base_name = filename.replace(".lsp", "")

    # Extract metadata
    commands = extract_commands_from_file(file_path)
    technologies = detect_technologies_in_file(file_path)
    complexity, line_count = get_file_complexity(file_path)
    description = extract_description_from_file(file_path)
    category = determine_category(filename)
    tags = generate_tags(filename, technologies, category)

    # Extract version info
    version_match = re.search(r"V(\d+)-(\d+)", filename)
    if version_match:
        version_info = {
            "major": version_match.group(1),
            "minor": version_match.group(2),
            "version_string": f"V{version_match.group(1)}-{version_match.group(2)}",
        }
    else:
        version_info = {"version_string": "1.0"}

    # Build manifest
    manifest = {
        "id": base_name,
        "filename": filename,
        "title": base_name.replace("V", " V").replace("-", "."),
        "description": description,
        "primary_category": category,
        "commands": commands,
        "tags": tags,
        "technologies": technologies,
        "ui": "DCL" if "DCL" in technologies else "CommandLine",
        "complexity": complexity,
        "line_count": line_count,
        "version_info": version_info,
        "entry_points": {
            "primary": commands[0] if commands else "",
            "complexity": complexity,
        },
        "dependencies": {"requires": [], "integrates_with": [], "conflicts_with": []},
        "usage_context": {
            "typical_workflows": [category.lower().replace("_", "_")],
            "automation_level": (
                "fully_automatic" if "batch" in filename.lower() else "semi_automatic"
            ),
        },
        "generated_date": datetime.now().isoformat(),
        "schema_version": "1.0",
    }

    # Write YAML file
    os.makedirs(output_dir, exist_ok=True)
    output_file = os.path.join(output_dir, f"{filename}.yaml")

    with open(output_file, "w", encoding="utf-8") as f:
        yaml.dump(manifest, f, default_flow_style=False, sort_keys=False, indent=2)

    return output_file, manifest


def main():
    """Generate sample YAML manifests for testing"""
    base_dir = r"c:\Users\dea29431\Documents\LOCAL\CAD"
    leemac_dir = os.path.join(base_dir, "LeeMac_lsp_programs")
    output_dir = os.path.join(base_dir, "sample_manifests")

    print("=== Sample YAML Manifest Generator ===")
    print(f"Source: {leemac_dir}")
    print(f"Output: {output_dir}")
    print()

    # Test with a few representative files
    test_files = [
        "NumIncV4-0.lsp",
        "ConvexHull.lsp",
        "BatchAttributeEditorV1-5.lsp",
        "LayerDirectorV2-1.lsp",
        "CircularWipeoutV1-2.lsp",
    ]

    manifests_created = 0

    for filename in test_files:
        file_path = os.path.join(leemac_dir, filename)

        if os.path.exists(file_path):
            try:
                output_file, manifest = create_yaml_manifest(file_path, output_dir)
                print(f"✓ Created: {os.path.basename(output_file)}")
                print(f"  Commands: {manifest['commands']}")
                print(f"  Technologies: {manifest['technologies']}")
                print(
                    f"  Complexity: {manifest['complexity']} ({manifest['line_count']} lines)"
                )
                print(f"  Category: {manifest['primary_category']}")
                print()
                manifests_created += 1

            except Exception as e:
                print(f"✗ Failed to create manifest for {filename}: {e}")

        else:
            print(f"✗ File not found: {filename}")

    print(f"Generated {manifests_created} sample YAML manifests")

    # Show sample content
    if manifests_created > 0:
        sample_file = os.path.join(output_dir, f"{test_files[0]}.yaml")
        if os.path.exists(sample_file):
            print("\n=== SAMPLE YAML CONTENT ===")
            with open(sample_file, "r", encoding="utf-8") as f:
                content = f.read()
                print(content[:500] + "..." if len(content) > 500 else content)


if __name__ == "__main__":
    main()
