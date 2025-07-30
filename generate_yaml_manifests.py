#!/usr/bin/env python3
"""
YAML Manifest Generator for LeeMac AutoLISP Scripts
Generates structured metadata manifests for the script collection
"""

import os
import re
import json
import yaml
from pathlib import Path
from typing import Dict, List, Optional, Any


class YAMLManifestGenerator:
    def __init__(self, leemac_dir: str, output_dir: str):
        self.leemac_dir = leemac_dir
        self.output_dir = output_dir
        self.category_mappings = self._load_category_mappings()

    def _load_category_mappings(self) -> Dict[str, str]:
        """Define category mappings based on existing categorization"""
        return {
            # Text & Annotation
            "text": "TEXT_ANNOTATION",
            "align": "TEXT_ANNOTATION",
            "label": "TEXT_ANNOTATION",
            "numinc": "TEXT_ANNOTATION",
            "curve": "TEXT_ANNOTATION",
            "mtext": "TEXT_ANNOTATION",
            "field": "TEXT_ANNOTATION",
            # Block & Attribute
            "block": "BLOCK_ATTRIBUTE",
            "attribute": "BLOCK_ATTRIBUTE",
            "batte": "BLOCK_ATTRIBUTE",
            "burst": "BLOCK_ATTRIBUTE",
            "extract": "BLOCK_ATTRIBUTE",
            # Mathematical & Algorithmic
            "convex": "MATHEMATICAL",
            "hull": "MATHEMATICAL",
            "encl": "MATHEMATICAL",
            "circle": "MATHEMATICAL",
            "ellipse": "MATHEMATICAL",
            "fractal": "MATHEMATICAL",
            "sierpinski": "MATHEMATICAL",
            "koch": "MATHEMATICAL",
            "md5": "MATHEMATICAL",
            # Layer & Drawing
            "layer": "LAYER_DRAWING",
            "drawing": "LAYER_DRAWING",
            "layout": "LAYER_DRAWING",
            "xref": "LAYER_DRAWING",
            "director": "LAYER_DRAWING",
            # Geometric Operations
            "poly": "GEOMETRIC",
            "outline": "GEOMETRIC",
            "offset": "GEOMETRIC",
            "break": "GEOMETRIC",
            "extend": "GEOMETRIC",
            "centerline": "GEOMETRIC",
            "mirror": "GEOMETRIC",
            "array": "GEOMETRIC",
            # UI & DCL
            "listbox": "USER_INTERFACE",
            "dialog": "USER_INTERFACE",
            "dcl": "USER_INTERFACE",
            "browser": "USER_INTERFACE",
            # Utilities & System
            "copy": "UTILITIES",
            "steal": "UTILITIES",
            "files": "UTILITIES",
            "csv": "UTILITIES",
            "wrapper": "UTILITIES",
            "log": "UTILITIES",
            "autoload": "UTILITIES",
            # Specialized
            "mastermind": "SPECIALIZED",
            "lotto": "SPECIALIZED",
            "clock": "SPECIALIZED",
            "star": "SPECIALIZED",
            "logistic": "SPECIALIZED",
        }

    def extract_metadata(self, file_path: str) -> Dict[str, Any]:
        """Extract comprehensive metadata from LISP file"""
        filename = os.path.basename(file_path)
        base_name = filename.replace(".lsp", "")

        try:
            with open(file_path, "r", encoding="utf-8", errors="ignore") as f:
                content = f.read()
        except Exception as e:
            print(f"Warning: Could not read {file_path}: {e}")
            content = ""

        # Extract basic info
        metadata = {
            "id": base_name,
            "filename": filename,
            "title": self._generate_title(base_name),
            "description": self._extract_description(content),
            "primary_category": self._determine_category(base_name.lower()),
            "commands": self._extract_commands(content),
            "tags": self._generate_tags(base_name, content),
            "technologies": self._detect_technologies(content),
            "ui": self._detect_ui_type(content),
            "complexity": self._assess_complexity(content),
            "version_info": self._extract_version(filename),
            "entry_points": self._build_entry_points(content),
            "dependencies": self._analyze_dependencies(content),
            "usage_context": self._infer_usage_context(base_name, content),
        }

        return metadata

    def _generate_title(self, base_name: str) -> str:
        """Generate human-friendly title from filename"""
        # Remove version suffixes
        title = re.sub(r"V\d+-\d+$", "", base_name)

        # Add spaces before capitals
        title = re.sub(r"([a-z])([A-Z])", r"\1 \2", title)

        # Handle special cases
        title = (
            title.replace("V1", "")
            .replace("V2", "")
            .replace("V3", "")
            .replace("V4", "")
        )
        title = title.replace("-", " ").strip()

        return title

    def _extract_description(self, content: str) -> str:
        """Extract description from file header comments"""
        # Look for common description patterns in Lee Mac files
        patterns = [
            r";;.*?([A-Z][^;]*(?:enables|allows|provides|implements|creates|performs)[^;]*)",
            r";;.*?([A-Z][^;]*(?:tool|utility|function|program)[^;]*)",
            r";;.*?([A-Z][^;]*\.)",
        ]

        for pattern in patterns:
            matches = re.findall(pattern, content[:2000], re.MULTILINE | re.IGNORECASE)
            if matches:
                # Clean up the description
                desc = matches[0].strip()
                desc = re.sub(r"\s+", " ", desc)  # Normalize whitespace
                if len(desc) > 20 and len(desc) < 200:
                    return desc

        return "AutoLISP utility program"

    def _determine_category(self, name_lower: str) -> str:
        """Determine primary category based on filename"""
        for keyword, category in self.category_mappings.items():
            if keyword in name_lower:
                return category
        return "UTILITIES"  # Default category

    def _extract_commands(self, content: str) -> List[str]:
        """Extract command names from (defun c:...) definitions"""
        commands = []
        pattern = r"\(defun\s+c:([a-zA-Z0-9_\-]+)"
        matches = re.findall(pattern, content, re.IGNORECASE)
        return list(set(matches))  # Remove duplicates

    def _generate_tags(self, base_name: str, content: str) -> List[str]:
        """Generate semantic tags for the script"""
        tags = []
        name_lower = base_name.lower()
        content_lower = content.lower()

        # Functional tags
        if any(word in name_lower for word in ["text", "label", "annotation"]):
            tags.extend(["text", "annotation", "labeling"])
        if any(word in name_lower for word in ["block", "attribute"]):
            tags.extend(["block", "attribute", "objects"])
        if any(word in name_lower for word in ["layer", "drawing"]):
            tags.extend(["layer", "organization", "management"])
        if any(word in name_lower for word in ["copy", "move", "array"]):
            tags.extend(["transformation", "duplication"])
        if any(word in name_lower for word in ["offset", "extend", "break"]):
            tags.extend(["geometric", "modification"])

        # Technology tags
        if "objectdbx" in content_lower:
            tags.append("multi-drawing")
        if "dcl" in content_lower or "dialog" in content_lower:
            tags.append("interactive")
        if "vlr-" in content_lower:
            tags.append("reactive")

        # Complexity tags
        if len(content) > 3000:
            tags.append("advanced")
        elif len(content) < 500:
            tags.append("simple")

        return list(set(tags))  # Remove duplicates

    def _detect_technologies(self, content: str) -> List[str]:
        """Detect technologies used in the script"""
        technologies = []
        content_lower = content.lower()

        if re.search(r"objectdbx|vla-open.*acad-object", content_lower):
            technologies.append("ObjectDBX")
        if re.search(r"load_dialog|new_dialog|\.dcl", content_lower):
            technologies.append("DCL")
        if re.search(r"vlr-.*-reactor", content_lower):
            technologies.append("VLR")
        if re.search(r"vlax-|vla-", content_lower):
            technologies.append("ActiveX")
        if re.search(r"ssget|ssname|sslength", content_lower):
            technologies.append("SelectionSets")
        if re.search(r"open.*read.*close|findfile", content_lower):
            technologies.append("FileIO")
        if re.search(r"entmake|entmod|entget", content_lower):
            technologies.append("EntityManipulation")

        return technologies

    def _detect_ui_type(self, content: str) -> str:
        """Detect the user interface type"""
        content_lower = content.lower()

        if "load_dialog" in content_lower or ".dcl" in content_lower:
            return "DCL"
        elif "grread" in content_lower or "grdraw" in content_lower:
            return "Graphics"
        elif "getpoint" in content_lower or "getstring" in content_lower:
            return "CommandLine"
        else:
            return "CommandLine"  # Default

    def _assess_complexity(self, content: str) -> str:
        """Assess script complexity"""
        line_count = len(content.splitlines())

        if line_count < 100:
            return "beginner"
        elif line_count < 500:
            return "intermediate"
        elif line_count < 1500:
            return "advanced"
        else:
            return "expert"

    def _extract_version(self, filename: str) -> Dict[str, str]:
        """Extract version information from filename"""
        version_match = re.search(r"V(\d+)-(\d+)", filename)
        if version_match:
            return {
                "major": version_match.group(1),
                "minor": version_match.group(2),
                "version_string": f"V{version_match.group(1)}-{version_match.group(2)}",
            }
        return {"version_string": "1.0"}

    def _build_entry_points(self, content: str) -> Dict[str, Any]:
        """Build entry points information"""
        commands = self._extract_commands(content)

        entry_points = {}
        if commands:
            entry_points["primary"] = commands[0]
            if len(commands) > 1:
                entry_points["aliases"] = commands[1:]
            entry_points["complexity"] = self._assess_complexity(content)

        return entry_points

    def _analyze_dependencies(self, content: str) -> Dict[str, List[str]]:
        """Analyze script dependencies"""
        dependencies = {"requires": [], "integrates_with": [], "conflicts_with": []}

        # Look for load statements
        load_pattern = r'\(load\s+"([^"]+)"'
        loads = re.findall(load_pattern, content, re.IGNORECASE)
        dependencies["requires"] = [
            f"{load}.lsp" for load in loads if not load.endswith(".lsp")
        ]

        return dependencies

    def _infer_usage_context(self, base_name: str, content: str) -> Dict[str, Any]:
        """Infer usage context and workflows"""
        context = {
            "typical_workflows": [],
            "industry_focus": [],
            "automation_level": "semi_automatic",
        }

        name_lower = base_name.lower()

        # Workflow inference
        if any(word in name_lower for word in ["label", "annotation", "text"]):
            context["typical_workflows"].append("drawing_annotation")
        if any(word in name_lower for word in ["block", "attribute"]):
            context["typical_workflows"].append("block_management")
        if any(word in name_lower for word in ["layer", "drawing"]):
            context["typical_workflows"].append("drawing_organization")

        # Industry focus
        if any(word in name_lower for word in ["elevation", "centerline", "dimension"]):
            context["industry_focus"].extend(["civil", "mechanical"])
        if any(word in name_lower for word in ["titleblock", "layout"]):
            context["industry_focus"].extend(["architecture", "engineering"])

        # Automation level
        if "batch" in name_lower or "auto" in name_lower:
            context["automation_level"] = "fully_automatic"
        elif any(
            word in content.lower() for word in ["getpoint", "getstring", "getent"]
        ):
            context["automation_level"] = "manual"

        return context

    def generate_yaml_manifest(self, file_path: str) -> str:
        """Generate YAML manifest for a single file"""
        metadata = self.extract_metadata(file_path)

        # Convert to YAML with custom formatting
        yaml_content = f"""# AutoLISP Script Manifest
# Generated from: {metadata['filename']}

id: "{metadata['id']}"
filename: "{metadata['filename']}"
title: "{metadata['title']}"
description: "{metadata['description']}"

# Categorization
primary_category: "{metadata['primary_category']}"
tags: {yaml.dump(metadata['tags'], default_flow_style=True).strip()}

# Technical Information
technologies: {yaml.dump(metadata['technologies'], default_flow_style=True).strip()}
ui: "{metadata['ui']}"
complexity: "{metadata['complexity']}"

# Entry Points
entry_points:
  primary: "{metadata['entry_points'].get('primary', '')}"
  complexity: "{metadata['entry_points'].get('complexity', 'intermediate')}"
{f'  aliases: {yaml.dump(metadata["entry_points"].get("aliases", []), default_flow_style=True).strip()}' if metadata['entry_points'].get('aliases') else ''}

# Version Information
version_info: {yaml.dump(metadata['version_info'], default_flow_style=False, indent=2)}

# Dependencies and Context
dependencies: {yaml.dump(metadata['dependencies'], default_flow_style=False, indent=2)}

usage_context: {yaml.dump(metadata['usage_context'], default_flow_style=False, indent=2)}

# Metadata
generated_date: "{yaml.dump(None)}"
schema_version: "1.0"
"""

        return yaml_content

    def generate_all_manifests(self):
        """Generate YAML manifests for all LISP files"""
        os.makedirs(self.output_dir, exist_ok=True)

        manifest_count = 0

        for filename in os.listdir(self.leemac_dir):
            if filename.endswith(".lsp"):
                file_path = os.path.join(self.leemac_dir, filename)
                yaml_content = self.generate_yaml_manifest(file_path)

                output_file = os.path.join(self.output_dir, f"{filename}.yaml")

                with open(output_file, "w", encoding="utf-8") as f:
                    f.write(yaml_content)

                manifest_count += 1
                print(f"Generated: {output_file}")

        print(f"\nGenerated {manifest_count} YAML manifests in {self.output_dir}")


def main():
    """Main function"""
    base_dir = r"c:\Users\dea29431\Documents\LOCAL\CAD"
    leemac_dir = os.path.join(base_dir, "LeeMac_lsp_programs")
    output_dir = os.path.join(base_dir, "manifests")

    print("=== YAML Manifest Generator ===")
    print(f"Source directory: {leemac_dir}")
    print(f"Output directory: {output_dir}")
    print()

    generator = YAMLManifestGenerator(leemac_dir, output_dir)
    generator.generate_all_manifests()

    print("\n=== NEXT STEPS ===")
    print("1. Review generated YAML files for accuracy")
    print("2. Manually curate descriptions and tags")
    print("3. Run build script to generate catalog.json and enhanced markdown")


if __name__ == "__main__":
    main()
