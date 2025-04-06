
import util.Optionals.Optional.{Empty, Just}
import util.Sequences.Sequence
import util.Sequences.Sequence.*

/*  Exercise 2: 
 *  Implement the below trait, and write a meaningful test.
 *  Suggestions:
 *  - reuse Sequences and Optionals as imported above
 *  - For other suggestions look directly to the methods and their description
 */
object SchoolModel:

  trait SchoolModule:
    type School
    type Teacher
    type Course

    /**
     * This a factory method for create a teacher from a name
     * e.g.,
     * teacher("John") // => Teacher("John")
     * Note!! The internal representation of a teacher may vary, decide what is the best for you
     * @param name the name of the teacher
     * @return the teacher created
     */
    def teacher(name: String): Teacher
    /**
     * This a factory method for create a course from a name
     * e.g.,
     * course("Math") // => Course("Math")
     * Note!! The internal representation of a course may vary, decide what is the best for you
     * @param name the name of the course
     * @return the course created
     *  */
    def course(name: String): Course

    /**
     * This method should return an empty school, namely a school without any teacher and course
     * e.g.,
     * emptySchool // => School(courses = Nil(), teachers = Nil(), teacherToCourses = Nil())
     * NOTE!! The above is just an example, the internal representation may vary, decide what is the best for you
     * You can store just the teacherToCourses, or having a case class for the school, or whatever you think is the best
     * @return the empty school
     */
    def emptySchool: School
    extension (school: School)
      /**
       * This method should return the list of courses
       * e.g.,
       * emptySchool.courses // => Nil()
       * emptySchool.setTeacherToCourse(teacher("John"), course("Math")).courses // => Cons("Math", Nil())
       * emptySchool
       *  .setTeacherToCourse(teacher("John"), course("Math"))
       *  .setTeacherToCourse(teacher("John"), course("Italian")).courses // => Cons("Math", Cons("Italian", Nil()))
       * Note!! If there are duplicates, just return them once
       * @return the list of courses
       */
      def courses: Sequence[String]
      /**
       * This method should return the list of teachers
       * e.g.,
       * emptySchool.teachers // => Nil()
       * emptySchool.setTeacherToCourse(teacher("John"), course("Math")).teachers // => Cons("John", Nil())
       * val john = teacher("John")
       * emptySchool
       *  .setTeacherToCourse(john, course("Math"))
       *  .setTeacherToCourse(john, course("Italian")).teachers // => Cons("John", Nil())
       * Note!! If there are duplicates, just return them once
       * @return the list of teachers
       */
      def teachers: Sequence[String]
      /**
       * This method should return a new school with the teacher assigned to the course
       * e.g.,
       * emptySchool
       *   .setTeacherToCourse(teacher("John"), course("Math")) // => School(courses = Cons("Math", Nil()), teachers = Cons("John", Nil()), teacherToCourses = Cons(("John", "Math"), Nil()))
       *  */
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      /**
       * This method should return the list of courses assigned to a teacher
       * e.g.,
       * emptySchool.coursesOfATeacher(teacher("John")) // => Nil()
       * emptySchool
       *   .setTeacherToCourse(teacher("John"), course("Math"))
       *   .coursesOfATeacher(teacher("John")) // => Cons("Math", Nil())
       * emptySchool
       *   .setTeacherToCourse(teacher("John"), course("Math"))
       *   .setTeacherToCourse(teacher("John"), course("Italian"))
       *   .coursesOfATeacher(teacher("John")) // => Cons("Math", Cons("Italian", Nil()))
       * @return the list of courses assigned to a teacher
       */
      def coursesOfATeacher(teacher: Teacher): Sequence[Course]
      /**
       * This method should return true if the teacher is present in the school
       * e.g.,
       * emptySchool.hasTeacher("John") // => false
       * emptySchool
       *  .setTeacherToCourse(teacher("John"), course("Math"))
       *  .hasTeacher("John") // => true
       *
       */
      def hasTeacher(name: String): Boolean
      /**
       * This method should return true if the course is present in the school
       * e.g.,
       * emptySchool.hasCourse("Math") // => false
       * emptySchool
       *  .setTeacherToCourse(teacher("John"), course("Math"))
       *  .hasCourse("Math") // => true
       *
       */
      def hasCourse(name: String): Boolean
  object BasicSchoolModule extends SchoolModule:
    case class SchoolImpl(
                           sequenceCourses: Sequence[Course],
                           sequenceTeachers: Sequence[Teacher],
                           teacherToCourses: Sequence[(Teacher, Sequence[Course])]
                         )

    override type School = SchoolImpl

    override opaque type Teacher = String

    override opaque type Course = String

    def teacher(name: String): Teacher = name

    def course(name: String): Course = name

    def emptySchool: School = SchoolImpl(Nil(), Nil(), Nil())

    extension (school: School)
      def courses: Sequence[String] = school.sequenceCourses.map(c => c)

      def teachers: Sequence[String] = school.sequenceTeachers.map(t => t)

      private def addIfAbsent[A](seq: Sequence[A], elem: A): Sequence[A] =
        if seq.contains(elem) then seq else seq.concat(Cons(elem, Nil()))

      private def updateTeacherCourses(
                                        mappings: Sequence[(Teacher, Sequence[Course])],
                                        teacher: Teacher,
                                        course: Course
                                      ): Sequence[(Teacher, Sequence[Course])] =
        mappings.find(_._1 == teacher) match
          case Just((_, existingCourses)) =>
            mappings.map {
              case (t, cs) if t == teacher =>
                (t, updateCoursesForExistingTeacher(cs, course))
              case other => other
            }
          case Empty() =>
            mappings.concat(Cons((teacher, Cons(course, Nil())), Nil()))

      private def updateCoursesForExistingTeacher(
                                                   courses: Sequence[Course],
                                                   courseToAdd: Course
                                                 ): Sequence[Course] =
        if courses.contains(courseToAdd) then courses
        else courses.concat(Cons(courseToAdd, Nil()))


      def setTeacherToCourse(teacher: Teacher, course: Course): School =
        val newCourses = addIfAbsent(school.sequenceCourses, course)
        val newTeachers = addIfAbsent (school.sequenceTeachers, teacher)
        val newTeacherToCourses = updateTeacherCourses (school.teacherToCourses, teacher, course)
        SchoolImpl (newCourses, newTeachers, newTeacherToCourses)

      def coursesOfATeacher(teacher: Teacher): Sequence[Course] =
        school.teacherToCourses
        .filter { case (t, _) => t == teacher }
        .flatMap { case (_, courses) => courses }

      def hasTeacher(name: String): Boolean = school.sequenceTeachers.contains(teacher(name))

      def hasCourse(name: String): Boolean =  school.sequenceCourses.contains(course(name))

@main def examples(): Unit =
  import SchoolModel.BasicSchoolModule.*
  val school = emptySchool
  println(school.sequenceTeachers) // Nil()
  println(school.sequenceCourses) // Nil()
  println(school.hasTeacher("John")) // false
  println(school.hasCourse("Math")) // false
  val john = teacher("John")
  val math = course("Math")
  val italian = course("Italian")
  val school2 = school.setTeacherToCourse(john, math)
  println(school2.sequenceTeachers) // Cons("John", Nil())
  println(school2.sequenceCourses) // Cons("Math", Nil())
  println(school2.hasTeacher("John")) // true
  println(school2.hasCourse("Math")) // true
  println(school2.hasCourse("Italian")) // false
  val school3 = school2.setTeacherToCourse(john, italian)
  println(school3.sequenceTeachers) // Cons("John", Nil())
  println(school3.sequenceCourses) // Cons("Math", Cons("Italian", Nil()))
  println(school3.hasTeacher("John")) // true
  println(school3.hasCourse("Math")) // true
  println(school3.hasCourse("Italian")) // true
  println(school3.coursesOfATeacher(john)) // Cons("Math", Cons("Italian", Nil()))


