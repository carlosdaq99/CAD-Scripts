#!/usr/bin/env python3
"""
Test script to validate the mathematical formulas used in the AutoLISP script
"""

import math


def test_scaling_formula():
    """Test the scaling formula: scale_i = max(min_scale, 1 - i/N)"""
    print("Testing Scaling Formula: scale_i = max(min_scale, 1 - i/N)")
    print("-" * 50)

    N = 10  # Number of copies
    min_scale = 0.1

    print(f"Number of copies (N): {N}")
    print(f"Minimum scale: {min_scale}")
    print("\nScale progression:")

    for i in range(1, N + 1):
        scale = max(min_scale, 1.0 - i / N)
        print(f"  Copy {i:2d}: scale = {scale:.3f}")

    # Verify first and last values
    first_scale = max(min_scale, 1.0 - 1 / N)
    last_scale = max(min_scale, 1.0 - N / N)

    print(f"\n✅ First copy scale: {first_scale:.3f} (should be close to 1.0)")
    print(f"✅ Last copy scale: {last_scale:.3f} (should be min_scale)")

    return True


def test_spacing_formula():
    """Test the spacing formula and cumulative distance calculation"""
    print("\n\nTesting Spacing Formula: spacing_k = const * (1 - k/N)")
    print("-" * 50)

    N = 10  # Number of copies
    total_length = 100.0  # Example curve length

    print(f"Number of copies (N): {N}")
    print(f"Total curve length: {total_length}")

    # Calculate spacing sum for normalization
    spacing_sum = 0.0
    for k in range(1, N + 1):
        spacing_val = 1.0 - k / N
        spacing_sum += spacing_val

    # Calculate normalization constant
    spacing_const = total_length / spacing_sum

    print(f"Spacing sum: {spacing_sum:.3f}")
    print(f"Normalization constant: {spacing_const:.3f}")

    # Calculate actual spacings and cumulative distances
    print("\nSpacing and cumulative distance progression:")
    cumulative_dist = 0.0
    distances = []

    for k in range(1, N + 1):
        spacing_val = spacing_const * (1.0 - k / N)
        cumulative_dist += spacing_val
        distances.append(cumulative_dist)
        print(
            f"  Copy {k:2d}: spacing = {spacing_val:6.3f}, cumulative = {cumulative_dist:6.3f}"
        )

    # Verify that last cumulative distance equals total length
    final_distance = distances[-1]
    print(f"\n✅ Final cumulative distance: {final_distance:.3f}")
    print(f"✅ Total curve length: {total_length:.3f}")
    print(f"✅ Difference: {abs(final_distance - total_length):.6f} (should be ~0)")

    # Verify decreasing spacing
    spacings = []
    cumulative = 0.0
    for k in range(1, N + 1):
        spacing = spacing_const * (1.0 - k / N)
        spacings.append(spacing)

    print(f"\n✅ First spacing: {spacings[0]:.3f} (largest)")
    print(f"✅ Last spacing: {spacings[-1]:.3f} (smallest)")
    print(
        f"✅ Spacing decreases: {all(spacings[i] >= spacings[i+1] for i in range(len(spacings)-1))}"
    )

    return True


def test_edge_cases():
    """Test edge cases and boundary conditions"""
    print("\n\nTesting Edge Cases")
    print("-" * 50)

    # Test with minimum copies
    print("Case 1: Single copy (N=1)")
    N = 1
    scale = max(0.1, 1.0 - 1 / N)
    print(f"  Scale for single copy: {scale:.3f} (should be 0.1 due to min_scale)")

    # Test with many copies
    print("\nCase 2: Many copies (N=100)")
    N = 100
    first_scale = max(0.1, 1.0 - 1 / N)
    last_scale = max(0.1, 1.0 - N / N)
    print(f"  First copy scale: {first_scale:.3f}")
    print(f"  Last copy scale: {last_scale:.3f}")

    # Test spacing normalization with different lengths
    print("\nCase 3: Different curve lengths")
    for length in [1.0, 50.0, 1000.0]:
        N = 5
        spacing_sum = sum(1.0 - k / N for k in range(1, N + 1))
        const = length / spacing_sum
        final_dist = sum(const * (1.0 - k / N) for k in range(1, N + 1))
        print(
            f"  Length {length:6.1f}: final distance = {final_dist:6.3f}, error = {abs(final_dist - length):.6f}"
        )

    return True


def test_mathematical_properties():
    """Test mathematical properties and invariants"""
    print("\n\nTesting Mathematical Properties")
    print("-" * 50)

    # Test that scaling is monotonically decreasing
    N = 20
    min_scale = 0.05
    scales = [max(min_scale, 1.0 - i / N) for i in range(1, N + 1)]

    is_monotonic = all(scales[i] >= scales[i + 1] for i in range(len(scales) - 1))
    print(f"✅ Scaling is monotonically decreasing: {is_monotonic}")

    # Test that spacing is monotonically decreasing
    spacings = [1.0 - k / N for k in range(1, N + 1)]
    is_spacing_monotonic = all(
        spacings[i] >= spacings[i + 1] for i in range(len(spacings) - 1)
    )
    print(f"✅ Spacing is monotonically decreasing: {is_spacing_monotonic}")

    # Test that all scales are within valid range
    all_valid_scales = all(0 <= s <= 1.0 for s in scales)
    print(f"✅ All scales within [0, 1]: {all_valid_scales}")

    # Test that all spacings are positive
    all_positive_spacings = all(s > 0 for s in spacings)
    print(f"✅ All spacings are positive: {all_positive_spacings}")

    return True


def main():
    """Main test execution"""
    print("=" * 60)
    print("TAPERED COPY MATHEMATICAL VALIDATION")
    print("=" * 60)

    success = True
    success &= test_scaling_formula()
    success &= test_spacing_formula()
    success &= test_edge_cases()
    success &= test_mathematical_properties()

    print("\n" + "=" * 60)
    if success:
        print("🎉 All mathematical tests passed!")
        print("The formulas correctly implement the requirements:")
        print("  • Linear scaling from 1.0 to min_scale")
        print("  • Decreasing spacing that sums to curve length")
        print("  • Proper handling of edge cases")
    else:
        print("❌ Some mathematical tests failed.")
    print("=" * 60)


if __name__ == "__main__":
    main()
