import java.time.LocalDateTime
import java.time.temporal.ChronoUnit
import scala.math.{abs, ceil, pow, sqrt}

object Main {

  // Функция нахождения всех целых делителей числа, в том числе отрицательных
  def getDevider(num: Long): Seq[Long] = {
    Seq
      .range(1, ceil(sqrt(abs(num))).toInt)
      .filter(num % _ == 0)
      .flatMap(x => Seq(x, -1 * x, num / x, -1 * (num / x)))
      .distinct
  }

  // Функция нахождения решения многочлена при заданном корне
  def getPolynomialSolution(coefs: Seq[Long], x: Double): Double =
    if (coefs.isEmpty) 0
    else (pow(x, coefs.length - 1) * coefs.head) + getPolynomialSolution(coefs.tail, x)

  def main(args: Array[String]): Unit = {
    val polynomial: Array[Long] = Array(3445, -2497, -2509, 2134, -92510457) //Array(2, -3, -5, 6) //коэффициенты исследуемого многочлена, где последний элемент - свободный член

    val timeStart = LocalDateTime.now()

    val highDevs = getDevider(polynomial.head)                                  // делители старшего коэффициента
    val freeDevs = getDevider(polynomial(polynomial.length - 1))                // делители свободного коэффициента
    val alldevs  = freeDevs.flatMap(p => highDevs.map(p.toDouble / _)).distinct // предполагаемые корни многочлена
    val roots = alldevs
      .map(v => Tuple2(getPolynomialSolution(polynomial, v), v))
      .filter { case (solution, _) => solution == 0}
      .map(_._2)

    val timeFinish = LocalDateTime.now()

    if (roots.isEmpty) println("Корней нет.")
    else println(s"Корни многочлена: ${roots.mkString(", ")}")

    println(
      s"Время работы алгоритма: ${timeStart.until(timeFinish, ChronoUnit.MILLIS)} milliseconds"
    )

  }
}
