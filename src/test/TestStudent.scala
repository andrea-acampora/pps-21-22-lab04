import org.junit.Test
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import u04lab.code.{Course, Student, List}
import u04lab.code.List.*


class TestStudent:

  @Test def testEnrolling(): Unit =
    val cPPS = Course("PPS", "Viroli")
    val s1 = Student("mario", 2015)
    s1.enrolling(cPPS)
    assertEquals(Cons("PPS", Nil()), s1.courses)

  @Test def testHasTeacher(): Unit =
    val cPPS = Course("PPS", "Viroli")
    val s1 = Student("mario", 2015)
    s1.enrolling(cPPS)
    assertTrue(s1.hasTeacher("Viroli"))
