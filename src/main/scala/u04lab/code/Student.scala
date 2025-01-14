package u04lab.code

import List.*

import scala.annotation.tailrec

trait Student:
  def name: String
  def year: Int
  def enrolling(course: Course*): Unit // the student participates to a Course
  def courses: List[String] // names of course the student participates to
  def hasTeacher(teacher: String): Boolean // is the student participating to a course of this teacher?


trait Course:
  def name: String
  def teacher: String

object Student:
  def apply(name: String, year: Int = 2017): Student = StudentImpl(name, year)

  private case class StudentImpl(override val name: String,
                                 override val year: Int) extends Student:

     private var  _courses: List[Course] = Nil()
     override def enrolling(courses: Course*): Unit = courses.map(Cons(_, Nil())).foreach(c => _courses = append(_courses, c))
     override def courses : List[String] = map(_courses)( _.name )
     override def hasTeacher(teacher: String): Boolean = contains(map(_courses)(x => x.teacher), teacher)

object Course:
  def apply(name: String, teacher: String): Course = CourseImpl(name, teacher)
  private case class CourseImpl(override val name: String,
                                override val teacher: String) extends Course

object SameTeacher:

  def unapply(courses: List[Course]): scala.Option[String] = courses match
    case Cons(head, tail) => foldLeft(tail)(scala.Option(head.teacher))((optionTeacher, course) => optionTeacher.filter( _ == course.teacher))
    case _ => scala.Option.empty
