
import scala.math._


def Branin(x: List[Double]): Double = {

    val name = "Branin function"
    val dimInputSPace = 2

    val a = 1.0
    val b = 5.1 / (4 * Pi)
    val c = 5 * Pi
    val d = 6
    val e = 10
    val f = 1 / (8 * Pi)

    val globalOptimaLocations = List(List(-Pi, 12.275), List(Pi, 2.275), List(9.42478, 2.475))

    val suggestedDomain = List(List(-5, 20), List(-5, 20))

    a * pow(x(1) - b * pow(x(0),2) + c * x(0) - d,2) + e * (1 - f) * cos(x(0)) + e

}

println("toto")
val x1 : List[Double] = List(-5, 0, 1, 5, 10)
println(x1)
val x2 : List[Double] = List(-5, 0, 1, 5, 10)

val res = (x1 zip x2)
println(res)
println("tutu")




