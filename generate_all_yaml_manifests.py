#!/usr/bin/env python3
"""
Complete YAML Manifest Generator
Creates YAML manifests for ALL LeeMac files in the collection
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
        print(f"Warning: Could not analyze technologies in {file_path}: {e}")

    return technologies


def extract_description_from_file(file_path):
    """Extract description from LISP file comments"""
    try:
        with open(file_path, "r", encoding="utf-8", errors="ignore") as f:
            lines = f.readlines()

        # Look in first 50 lines for meaningful descriptions
        for i, line in enumerate(lines[:50]):
            if ";;" in line:
                desc = line.split(";;", 1)[1].strip()
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
                        "displays",
                        "calculates",
                        "generates",
                        "returns",
                        "program",
                        "utility",
                        "function",
                        "tool",
                    ]
                ):
                    if len(desc) > 20 and len(desc) < 200:
                        return desc

        return "Professional AutoLISP utility program"

    except Exception as e:
        print(f"Warning: Could not extract description from {file_path}: {e}")
        return "Professional AutoLISP utility program"


def determine_category(filename, content=""):
    """Determine category based on filename patterns and content"""
    name_lower = filename.lower()

    # Text and annotation related
    if any(
        word in name_lower
        for word in [
            "text",
            "align",
            "label",
            "numinc",
            "curve",
            "mtext",
            "field",
            "areas2field",
            "areafunctions",
        ]
    ):
        return "TEXT_ANNOTATION"

    # Block and attribute related
    elif any(
        word in name_lower
        for word in ["block", "attribute", "batte", "burst", "areas2attribute"]
    ):
        return "BLOCK_ATTRIBUTE"

    # Mathematical and algorithmic
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
            "minenc",
            "tangent",
        ]
    ):
        return "MATHEMATICAL"

    # Layer and drawing management
    elif any(
        word in name_lower for word in ["layer", "drawing", "layout", "xref", "copy2"]
    ):
        return "LAYER_DRAWING"

    # Geometric operations and curves
    elif any(
        word in name_lower
        for word in [
            "poly",
            "outline",
            "offset",
            "break",
            "extend",
            "centerline",
            "curve",
            "arc",
            "projection",
        ]
    ):
        return "GEOMETRIC_CURVE"

    # Selection and entity management
    elif any(
        word in name_lower
        for word in ["select", "find", "bfind", "chain", "filter", "count"]
    ):
        return "SELECTION_ENTITY"

    # User interface and dialog
    elif any(word in name_lower for word in ["dialog", "dcl", "browse", "listbox"]):
        return "USER_INTERFACE"

    # Import/export and file operations
    elif any(
        word in name_lower for word in ["export", "import", "copy", "extract", "dump"]
    ):
        return "IMPORT_EXPORT"

    # Default fallback
    return "SPECIALIZED"


def determine_complexity(file_path, line_count):
    """Determine complexity based on file size and content analysis"""
    try:
        with open(file_path, "r", encoding="utf-8", errors="ignore") as f:
            content = f.read()

        content_lower = content.lower()
        complexity_indicators = 0

        # Check for advanced features
        if "objectdbx" in content_lower:
            complexity_indicators += 2
        if "vlr-" in content_lower and "reactor" in content_lower:
            complexity_indicators += 2
        if "load_dialog" in content_lower:
            complexity_indicators += 1
        if len(re.findall(r"\(defun", content)) > 10:
            complexity_indicators += 1
        if "error" in content_lower and "handler" in content_lower:
            complexity_indicators += 1

        # Base on line count and indicators
        if line_count > 1500 or complexity_indicators >= 4:
            return "expert"
        elif line_count > 500 or complexity_indicators >= 2:
            return "advanced"
        elif line_count > 100 or complexity_indicators >= 1:
            return "intermediate"
        else:
            return "beginner"

    except Exception as e:
        # Fallback based on line count only
        if line_count > 1500:
            return "expert"
        elif line_count > 500:
            return "advanced"
        elif line_count > 100:
            return "intermediate"
        else:
            return "beginner"


def parse_version_info(filename):
    """Parse version information from filename"""
    # Match patterns like V1-2, V4-0, etc.
    version_match = re.search(r"V(\d+)-(\d+)", filename, re.IGNORECASE)
    if version_match:
        return {
            "major": version_match.group(1),
            "minor": version_match.group(2),
            "version_string": f"V{version_match.group(1)}-{version_match.group(2)}",
        }
    return {"version_string": "1.0"}


def create_yaml_manifest(file_path, output_dir):
    """Create YAML manifest for a single LISP file"""
    filename = os.path.basename(file_path)
    base_name = os.path.splitext(filename)[0]

    # Count lines
    try:
        with open(file_path, "r", encoding="utf-8", errors="ignore") as f:
            lines = f.readlines()
        line_count = len([line for line in lines if line.strip()])
    except:
        line_count = 0

    # Extract metadata
    commands = extract_commands_from_file(file_path)
    technologies = detect_technologies_in_file(file_path)
    description = extract_description_from_file(file_path)
    category = determine_category(filename)
    complexity = determine_complexity(file_path, line_count)
    version_info = parse_version_info(filename)

    # Determine UI type
    ui_type = "DCL" if "DCL" in technologies else "CommandLine"

    # Generate tags based on category and content
    tags = []
    if category == "TEXT_ANNOTATION":
        tags.extend(["annotation", "text", "labeling"])
    elif category == "BLOCK_ATTRIBUTE":
        tags.extend(["block", "attribute", "objects"])
    elif category == "MATHEMATICAL":
        tags.extend(["algorithm", "mathematics", "computation"])
    elif category == "GEOMETRIC_CURVE":
        tags.extend(["geometry", "curve", "construction"])

    if "DCL" in technologies:
        tags.append("interactive")
    if "ObjectDBX" in technologies:
        tags.append("automation")
    if "VLR" in technologies:
        tags.append("reactive")

    # Create manifest structure
    manifest = {
        "id": base_name,
        "filename": filename,
        "title": f"{base_name.replace('V', ' V').replace('-', '.')}",
        "description": description,
        "primary_category": category,
        "commands": commands,
        "tags": list(set(tags)),  # Remove duplicates
        "technologies": technologies,
        "ui": ui_type,
        "complexity": complexity,
        "line_count": line_count,
        "version_info": version_info,
        "entry_points": {
            "primary": commands[0] if commands else "",
            "complexity": complexity,
        },
        "dependencies": {"requires": [], "integrates_with": [], "conflicts_with": []},
        "usage_context": {
            "typical_workflows": [category.lower()],
            "automation_level": (
                "fully_automatic" if "ObjectDBX" in technologies else "semi_automatic"
            ),
        },
        "generated_date": datetime.now().isoformat(),
        "schema_version": "1.0",
    }

    # Save YAML manifest
    output_file = os.path.join(output_dir, f"{base_name}.yaml")
    with open(output_file, "w", encoding="utf-8") as f:
        yaml.dump(manifest, f, default_flow_style=False, allow_unicode=True, indent=2)

    return output_file, manifest


def main():
    """Generate YAML manifests for ALL LeeMac files"""
    # Setup paths
    current_dir = os.path.dirname(os.path.abspath(__file__))
    leemac_dir = os.path.join(current_dir, "LeeMac_lsp_programs")
    output_dir = os.path.join(current_dir, "sample_manifests")

    # Create output directory
    os.makedirs(output_dir, exist_ok=True)

    print(f"Scanning LeeMac directory: {leemac_dir}")
    print(f"Output directory: {output_dir}")
    print()

    # Get ALL .lsp files
    if not os.path.exists(leemac_dir):
        print(f"Error: LeeMac directory not found: {leemac_dir}")
        return

    lsp_files = [f for f in os.listdir(leemac_dir) if f.endswith(".lsp")]
    total_files = len(lsp_files)

    print(f"Found {total_files} .lsp files to process")
    print("=" * 50)

    manifests_created = 0
    category_stats = {}
    technology_stats = {}
    complexity_stats = {}

    for i, filename in enumerate(sorted(lsp_files), 1):
        file_path = os.path.join(leemac_dir, filename)

        try:
            output_file, manifest = create_yaml_manifest(file_path, output_dir)

            # Update statistics
            category = manifest["primary_category"]
            complexity = manifest["complexity"]

            category_stats[category] = category_stats.get(category, 0) + 1
            complexity_stats[complexity] = complexity_stats.get(complexity, 0) + 1

            for tech in manifest["technologies"]:
                technology_stats[tech] = technology_stats.get(tech, 0) + 1

            # Progress indicator
            print(f"[{i:3d}/{total_files}] ✓ {filename}")
            if manifest["commands"]:
                print(f"          Commands: {', '.join(manifest['commands'])}")
            if manifest["technologies"]:
                print(f"          Technologies: {', '.join(manifest['technologies'])}")
            print(
                f"          Category: {category} | Complexity: {complexity} | Lines: {manifest['line_count']}"
            )
            print()

            manifests_created += 1

        except Exception as e:
            print(f"[{i:3d}/{total_files}] ✗ Failed: {filename} - {e}")

    print("=" * 50)
    print(f"GENERATION COMPLETE!")
    print(f"Successfully created {manifests_created} YAML manifests")
    print()

    # Print statistics
    print("📊 CATEGORY DISTRIBUTION:")
    for category, count in sorted(category_stats.items()):
        print(f"  {category}: {count} files")
    print()

    print("🔧 TECHNOLOGY USAGE:")
    for tech, count in sorted(
        technology_stats.items(), key=lambda x: x[1], reverse=True
    ):
        print(f"  {tech}: {count} files")
    print()

    print("⚡ COMPLEXITY LEVELS:")
    for complexity, count in sorted(complexity_stats.items()):
        print(f"  {complexity}: {count} files")
    print()

    print(f"Next step: Run build_enhanced_catalog.py to generate the complete catalog")


if __name__ == "__main__":
    main()
