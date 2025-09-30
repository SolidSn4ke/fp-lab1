package example.java;

import example.java.math.EulerProblems;

public class Main {
    public static void main(String[] args) {
        EulerProblems ep = new EulerProblems();
        System.out.printf("Euler problem 5 solution: %d\nEuler problem 26 solution: %d\n", ep.euler5(20), ep.euler26(1000));
    }
}
