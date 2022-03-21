import org.junit.Test
import org.junit.Assert.assertEquals

import u04lab.code.Complex

class TestComplex :

  @Test def testEquals(): Unit =
    assertEquals(Complex(10, 20), Complex(10, 20))
