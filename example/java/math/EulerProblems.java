package example.java.math;

import java.util.LinkedList;

public class EulerProblems {
    public long euler5(long n) {
        if (n < 1) return -1;
        long result = 1;
        for (int i = 1; i <= n; i++) {
            result = (result * i) / MathFunctions.gcd(result, i);
        }
        return result;
    }

    public int euler26(Integer n) {
        LinkedList<Integer> remainders = new LinkedList<>();
        int result = 1;
        int maxLen = 0;
        for (int i = 1; i <= n; i++) {
            remainders.clear();
            int base = 10;
            int mod;
            int len = 0;
            do {
                mod = base % i;
                base = mod * 10;
                if (remainders.contains(mod)) break;
                remainders.add(mod);
            } while (mod != 0);
            if (mod != 0) len = remainders.size() - remainders.indexOf(mod);
            if (len > maxLen) {
                result = i;
                maxLen = len;
            }
        }
        return result;
    }
}
