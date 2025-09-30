package example.java.math;

import static java.lang.Math.max;
import static java.lang.Math.min;

public class MathFunctions {
    public static long gcd(long x, long y) {
        if (min(x, y) == 0) return max(x, y);
        return gcd(min(x, y), max(x, y) % min(x, y));
    }
}
