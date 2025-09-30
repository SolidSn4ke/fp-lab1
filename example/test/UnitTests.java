package example.test;

import example.java.math.EulerProblems;
import org.junit.Test;

import static junit.framework.Assert.assertEquals;

public class UnitTests {
    @Test
    public void testEuler5() {
        EulerProblems ep = new EulerProblems();
        assertEquals(ep.euler5(20), 232792560);
        assertEquals(ep.euler5(10), 2520);
        assertEquals(ep.euler5(1), 1);
        assertEquals(ep.euler5(-20), -1);
    }

    @Test
    public void testEuler26() {
        EulerProblems ep = new EulerProblems();
        assertEquals(ep.euler26(1000), 983);
        assertEquals(ep.euler26(10), 7);
    }
}
