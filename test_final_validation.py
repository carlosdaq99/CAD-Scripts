#!/usr/bin/env python3
"""
Final validation test for GeologicalHatchStrata Civil 3D implementation
Tests all AutoLISP files for syntax errors and proper structure
"""

import os
import re
import sys
from pathlib import Path


def test_autolisp_syntax():
    """Test AutoLISP files for basic syntax validation"""
    base_path = Path(r"c:\Users\dea29431\Documents\LOCAL\CAD")

    test_files = [
        "GeologicalHatchStrata.lsp",
        "TestCivil3DCOM.lsp",
        "test_autolisp_imports.lsp",
    ]

    results = []

    for filename in test_files:
        filepath = base_path / filename

        if not filepath.exists():
            results.append(f"❌ {filename}: File not found")
            continue

        try:
            with open(filepath, "r", encoding="utf-8") as f:
                content = f.read()

            # Basic syntax checks
            issues = []

            # Check parentheses balance
            open_parens = content.count("(")
            close_parens = content.count(")")
            if open_parens != close_parens:
                issues.append(
                    f"Unbalanced parentheses: {open_parens} open, {close_parens} close"
                )

            # Check for common command definitions
            if filename == "GeologicalHatchStrata.lsp":
                if "(defun C:GEOHATCH" not in content:
                    issues.append("GEOHATCH command not found")
                if "*geological-config*" not in content:
                    issues.append("Geological config not found")

            elif filename == "TestCivil3DCOM.lsp":
                required_commands = ["C:TESTCOM", "C:TESTPROFILES", "C:TESTGEO"]
                for cmd in required_commands:
                    if f"(defun {cmd}" not in content:
                        issues.append(f"{cmd} command not found")

            # Check for Lee Mac patterns
            if "vlax-" in content and "(vl-load-com)" not in content:
                issues.append("COM functions used but vl-load-com not found")

            # File size check
            file_size = len(content.encode("utf-8"))
            if file_size > 15000:  # 15KB limit from instructions
                issues.append(f"File size {file_size} bytes exceeds 15KB guideline")

            if issues:
                results.append(f"⚠️  {filename}: {', '.join(issues)}")
            else:
                results.append(f"✅ {filename}: Syntax validation passed")

        except Exception as e:
            results.append(f"❌ {filename}: Error reading file - {e}")

    return results


def test_geological_config():
    """Test geological configuration structure"""
    base_path = Path(r"c:\Users\dea29431\Documents\LOCAL\CAD")
    filepath = base_path / "GeologicalHatchStrata.lsp"

    if not filepath.exists():
        return ["❌ GeologicalHatchStrata.lsp not found for config test"]

    try:
        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        # Extract geological config
        config_match = re.search(r"\*geological-config\*.*?\)\s*\)", content, re.DOTALL)
        if not config_match:
            return ["❌ Geological configuration not found"]

        config_text = config_match.group(0)

        # Check for required geological materials
        required_materials = ["AL", "KC", "HD", "CG", "RTD"]
        found_materials = []

        for material in required_materials:
            if f'"{material}"' in config_text:
                found_materials.append(material)

        results = []
        if len(found_materials) == len(required_materials):
            results.append(
                f"✅ All {len(required_materials)} geological materials configured"
            )
        else:
            missing = set(required_materials) - set(found_materials)
            results.append(f"⚠️  Missing materials: {', '.join(missing)}")

        # Check configuration structure
        if re.search(r'"[A-Z]+".*?\(.*?".*?".*?\d+.*?\d+.*?".*?"\)', config_text):
            results.append("✅ Geological configuration structure valid")
        else:
            results.append("⚠️  Geological configuration structure may be invalid")

        return results

    except Exception as e:
        return [f"❌ Error testing geological config: {e}"]


def test_file_structure():
    """Test overall file structure and documentation"""
    base_path = Path(r"c:\Users\dea29431\Documents\LOCAL\CAD")

    expected_files = {
        "GeologicalHatchStrata.lsp": "Main implementation",
        "TestCivil3DCOM.lsp": "Testing framework",
        "test_autolisp_imports.lsp": "Syntax validation",
        "reports/GeologicalHatchStrata_Implementation_Guide.md": "Documentation",
    }

    results = []

    for filepath, description in expected_files.items():
        full_path = base_path / filepath
        if full_path.exists():
            size = full_path.stat().st_size
            results.append(f"✅ {filepath}: {description} ({size} bytes)")
        else:
            results.append(f"❌ {filepath}: Missing {description}")

    # Check for debug directory
    debug_path = base_path / "debug"
    if debug_path.exists():
        results.append("✅ Debug directory exists for logging")
    else:
        results.append("⚠️  Debug directory not found (logging may fail)")

    return results


def main():
    """Run all validation tests"""
    print("=" * 60)
    print("GEOLOGICAL HATCH STRATA - FINAL VALIDATION TEST")
    print("=" * 60)

    # Test 1: AutoLISP Syntax
    print("\n1. AutoLISP Syntax Validation:")
    print("-" * 40)
    syntax_results = test_autolisp_syntax()
    for result in syntax_results:
        print(f"   {result}")

    # Test 2: Geological Configuration
    print("\n2. Geological Configuration Test:")
    print("-" * 40)
    config_results = test_geological_config()
    for result in config_results:
        print(f"   {result}")

    # Test 3: File Structure
    print("\n3. File Structure Test:")
    print("-" * 40)
    structure_results = test_file_structure()
    for result in structure_results:
        print(f"   {result}")

    # Summary
    print("\n" + "=" * 60)
    print("VALIDATION SUMMARY")
    print("=" * 60)

    all_results = syntax_results + config_results + structure_results

    passed = len([r for r in all_results if r.startswith("✅")])
    warnings = len([r for r in all_results if r.startswith("⚠️")])
    failed = len([r for r in all_results if r.startswith("❌")])

    print(f"✅ Passed: {passed}")
    print(f"⚠️  Warnings: {warnings}")
    print(f"❌ Failed: {failed}")

    if failed == 0:
        print("\n🎉 IMPLEMENTATION READY FOR CIVIL 3D TESTING!")
        print("\nNext steps:")
        print('1. Load files in Civil 3D: (load "GeologicalHatchStrata.lsp")')
        print("2. Run tests: TESTCOM, TESTPROFILES, TESTGEO")
        print("3. Execute: GEOHATCH")
    else:
        print(f"\n❌ {failed} critical issues found. Please resolve before testing.")
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())
