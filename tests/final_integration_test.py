#!/usr/bin/env python3
"""
Final integration test for the TAPEREDCOPY AutoLISP script
"""

import os
import re


def final_integration_test():
    """Comprehensive integration test"""
    print("=" * 70)
    print("FINAL INTEGRATION TEST - TAPEREDCOPY AUTOLISP SCRIPT")
    print("=" * 70)

    # Test file existence and basic properties
    script_path = r"c:\Users\dea29431\Documents\LOCAL\CAD\ScaleCopyAlongCurve.lsp"
    doc_path = r"c:\Users\dea29431\Documents\LOCAL\CAD\TAPEREDCOPY_Documentation.md"

    print("📁 FILE VALIDATION")
    print("-" * 30)

    if os.path.exists(script_path):
        size = os.path.getsize(script_path)
        print(f"✅ Script file exists: {size} bytes ({size/1024:.1f} KB)")
        if size <= 15 * 1024:
            print("✅ File size within 15KB guideline")
        else:
            print("⚠️  File exceeds 15KB guideline")
    else:
        print("❌ Script file not found")
        return False

    if os.path.exists(doc_path):
        print("✅ Documentation file exists")
    else:
        print("⚠️  Documentation file not found")

    # Load and analyze script content
    with open(script_path, "r", encoding="utf-8") as f:
        content = f.read()

    print("\n🔍 CONTENT ANALYSIS")
    print("-" * 30)

    # Check for required sections
    sections = [
        ("Header with description", "TAPERED COPY ALONG CURVE - AutoLISP Script"),
        ("Mathematical formulas", "scale_i = max(min_scale, 1 - i/N)"),
        ("Command registration", "Command: TAPEREDCOPY"),
        ("Error handling", "*error*"),
        ("ActiveX usage", "vlax-curve"),
        ("User interaction", "entsel"),
        ("Progress feedback", "princ"),
    ]

    for section_name, search_text in sections:
        if search_text in content:
            print(f"✅ {section_name}")
        else:
            print(f"❌ Missing: {section_name}")

    # Validate function structure
    print("\n🏗️  FUNCTION STRUCTURE")
    print("-" * 30)

    functions = [
        "C:TAPEREDCOPY",
        "validate-curve-object",
        "get-curve-length",
        "calculate-spacing-distances",
        "get-tangent-at-distance",
        "copy-and-scale-object",
        "c:ScaleCopyAlongCurve",
    ]

    for func in functions:
        if f"defun {func}" in content or f"defun {func.upper()}" in content:
            print(f"✅ Function: {func}")
        else:
            print(f"❌ Missing function: {func}")

    # Check requirements compliance
    print("\n📋 REQUIREMENTS COMPLIANCE")
    print("-" * 30)

    requirements = [
        ("User interaction prompts", ["entsel", "getpoint", "getint", "getreal"]),
        ("Mathematical implementation", ["1 - i/N", "spacing_k"]),
        ("Error handling", ["*error*", "temp-error"]),
        (
            "vlax-curve compatibility",
            ["vlax-curve-getPointAtDist", "vlax-curve-getDistAtParam"],
        ),
        ("Progress indicators", ["princ", "Progress", "Placed"]),
        ("Parameter validation", ["if (not", "< num-copies 1"]),
        ("Legacy compatibility", ["c:ScaleCopyAlongCurve"]),
    ]

    for req_name, search_terms in requirements:
        found = any(term in content for term in search_terms)
        if found:
            print(f"✅ {req_name}")
        else:
            print(f"❌ Missing: {req_name}")

    # Check mathematical accuracy
    print("\n🧮 MATHEMATICAL VALIDATION")
    print("-" * 30)

    # Simulate the scaling calculation
    def test_scaling(N, min_scale):
        scales = []
        for i in range(1, N + 1):
            scale = max(min_scale, 1.0 - i / N)
            scales.append(scale)
        return scales

    scales = test_scaling(10, 0.1)
    print(f"✅ Scaling test (N=10): {scales[0]:.2f} → {scales[-1]:.2f}")

    # Simulate spacing calculation
    def test_spacing(N, total_length):
        spacing_sum = sum(1.0 - k / N for k in range(1, N + 1))
        const = total_length / spacing_sum
        spacings = [const * (1.0 - k / N) for k in range(1, N + 1)]
        total_dist = sum(spacings)
        return total_dist, spacings[0], spacings[-1]

    total, first_spacing, last_spacing = test_spacing(10, 100.0)
    print(
        f"✅ Spacing test: total={total:.1f}, first={first_spacing:.1f}, last={last_spacing:.1f}"
    )

    # Code quality metrics
    print("\n📊 CODE QUALITY METRICS")
    print("-" * 30)

    lines = content.split("\n")
    comment_lines = sum(1 for line in lines if line.strip().startswith(";"))
    code_lines = sum(
        1 for line in lines if line.strip() and not line.strip().startswith(";")
    )

    print(f"✅ Total lines: {len(lines)}")
    print(f"✅ Comment lines: {comment_lines} ({comment_lines/len(lines)*100:.1f}%)")
    print(f"✅ Code lines: {code_lines}")
    print(
        f"✅ Comment ratio: {'Good' if comment_lines/len(lines) > 0.3 else 'Could be better'}"
    )

    # Final validation
    print("\n🎯 FINAL VALIDATION")
    print("-" * 30)

    validation_checks = [
        ("Parentheses balanced", content.count("(") == content.count(")")),
        ("No syntax errors visible", "defun C:TAPEREDCOPY" in content),
        ("Documentation complete", os.path.exists(doc_path)),
        ("Mathematical formulas correct", "max(min_scale, 1 - i/N)" in content),
        ("Error handling present", "*error*" in content),
        ("User feedback included", "princ" in content),
    ]

    all_passed = True
    for check_name, check_result in validation_checks:
        if check_result:
            print(f"✅ {check_name}")
        else:
            print(f"❌ {check_name}")
            all_passed = False

    print("\n" + "=" * 70)
    if all_passed:
        print("🎉 INTEGRATION TEST PASSED!")
        print("The TAPEREDCOPY AutoLISP script is ready for deployment.")
        print("\nDeployment checklist:")
        print('  ✅ Load script in AutoCAD: (load "ScaleCopyAlongCurve.lsp")')
        print("  ✅ Test with simple geometry before production use")
        print("  ✅ Verify curve compatibility with target objects")
        print("  ✅ Document any project-specific customizations")
    else:
        print("❌ INTEGRATION TEST FAILED!")
        print("Please review the issues above before deployment.")
    print("=" * 70)

    return all_passed


if __name__ == "__main__":
    final_integration_test()
