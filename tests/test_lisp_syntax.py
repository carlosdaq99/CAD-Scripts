#!/usr/bin/env python3
"""
Test script to validate AutoLISP syntax and structure
"""

import os
import re


def test_lisp_syntax(file_path):
    """Test basic AutoLISP syntax validation"""
    print(f"Testing AutoLISP file: {file_path}")

    if not os.path.exists(file_path):
        print(f"❌ File not found: {file_path}")
        return False

    with open(file_path, "r", encoding="utf-8") as f:
        content = f.read()

    # Test 1: Balanced parentheses
    open_parens = content.count("(")
    close_parens = content.count(")")
    print(f"📊 Parentheses balance: {open_parens} open, {close_parens} close")

    if open_parens != close_parens:
        print("❌ Unbalanced parentheses detected!")
        return False
    else:
        print("✅ Parentheses are balanced")

    # Test 2: Check for required function definitions
    required_functions = [
        "C:TAPEREDCOPY",
        "validate-curve-object",
        "get-curve-length",
        "calculate-spacing-distances",
        "copy-and-scale-object",
    ]

    for func in required_functions:
        if f"defun {func}" in content:
            print(f"✅ Function found: {func}")
        else:
            print(f"❌ Missing function: {func}")
            return False

    # Test 3: Check for proper vlax-curve usage
    vlax_functions = [
        "vlax-curve-getPointAtDist",
        "vlax-curve-getDistAtParam",
        "vlax-curve-getEndParam",
        "vlax-curve-getParamAtDist",
    ]

    for func in vlax_functions:
        if func in content:
            print(f"✅ VLAX function used: {func}")
        else:
            print(f"⚠️  VLAX function not found: {func}")

    # Test 4: Check for error handling
    if "*error*" in content:
        print("✅ Error handling implemented")
    else:
        print("⚠️  No error handling found")

    # Test 5: Check mathematical formulas in comments
    if "scale_i = max(min_scale, 1 - i/N)" in content:
        print("✅ Scaling formula documented")
    else:
        print("⚠️  Scaling formula not documented")

    if "spacing_k = const * (1 - k/N)" in content:
        print("✅ Spacing formula documented")
    else:
        print("⚠️  Spacing formula not documented")

    # Test 6: File size check
    file_size = len(content.encode("utf-8"))
    print(f"📁 File size: {file_size} bytes ({file_size/1024:.1f} KB)")

    if file_size > 15 * 1024:
        print("⚠️  File exceeds 15KB guideline")
    else:
        print("✅ File size within guidelines")

    return True


def main():
    """Main test execution"""
    file_path = r"c:\Users\dea29431\Documents\LOCAL\CAD\ScaleCopyAlongCurve.lsp"

    print("=" * 60)
    print("AutoLISP Syntax and Structure Validation")
    print("=" * 60)

    success = test_lisp_syntax(file_path)

    print("\n" + "=" * 60)
    if success:
        print("🎉 All tests passed! The AutoLISP file appears to be valid.")
    else:
        print("❌ Some tests failed. Please review the issues above.")
    print("=" * 60)


if __name__ == "__main__":
    main()
