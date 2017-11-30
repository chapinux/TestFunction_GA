import scala.math._


object testFunctions extends App{


  def ackley1 (xL: List[Double]): Double = {
    val suggestedDomain = List.fill(xL.size)(List(-35, 35))
    val globalOptimaLocations = List.fill(xL.size)(0)
    val OptimaValues = 0

    val isContinuous = true
    val isDifferentiable = true
    val isSeparable = false
    val isScalable = true
    val isMultiModal = true

    val D = xL.size
    val B = 0.02
    val term1: Double = - 20*exp( pow( (1/(-D)) * (xL.map(x=>pow(x,2)).sum   ), -1/B))
    val term2: Double = - exp((D-1)*xL.map(x=>cos(2*Pi*x)).sum)

    println(term1)

      term1 + term2 + 20 + E

  }

  println(ackley1(List(0,0,0)))


  def Branin(x1:Double, x2: Double): Double = {
    val name = "Branin function"
    val dimInputSPace = 2

    val a = 1.0
    val b = 5.1 / (4 * pow(Pi,2))
    val c = 5 / Pi
    val d = 6
    val e = 10
    val f = 1 / (8 * Pi)

    val globalOptimaLocations = List(List(-Pi, 12.275), List(Pi, 2.275), List(9.42478, 2.475))
    val suggestedDomain = List(List(-5, 20), List(-5, 20))

    a * pow(x2 - b * pow(x1,2) + c * x1 - d,2) + e * (1 - f) * cos(x1) + e
  }


def braninRcos (x1: Double, x2 : Double): Double ={
  val suggestedDomain = List(List(-5, 10),(List(0,15)))
  val globalOptimaLocations= List(List(- Pi, 12.275), List(Pi, 2.275), List(3*Pi, 2.245))
  val OptimaValues = 0.3978873

 val isContinuous = true
  val isDifferentiable = true
  val isSeparable = false
  val isScalable = false
  val isMultiModal = true

  val term1: Double = pow(x2 -  (5.1 * pow(x1,2))/(4* pow(Pi, 2 )) + (5*x1)/Pi -  6, 2 )
    val term2: Double = 10 *(1 - (1/(8*Pi))) * cos(x1) +10


      term1 + term2

}

  //println(braninRcos(-Pi, 12.275))

  def braninRcos2 (x1: Double, x2 : Double): Double = {
    val suggestedDomain = List(List(-5, 15), (List(-5, 15)))
    val globalOptimaLocations = List(-3.2, 12.53)
    val OptimaValues = 5.559037

    val isContinuous = true
    val isDifferentiable = true
    val isSeparable = false
    val isScalable = false
    val isMultiModal = true

    val term1: Double = pow(x2 -  (5.1 * pow(x1,2))/(4* pow(Pi, 2 )) + (5*x1)/Pi -  6, 2 )
    val term2: Double = 10 *(1 - 1/(8*Pi)) * cos(x1)*cos(x2) + log(pow(x1,2)+ pow(x2,2) + 1 ) +10


 (term1 + term2)
  }

  //println(braninRcos2(-3.2, 12.53))






  def deceptiveFunction(xL: List[Double], alphaL: List[Double], beta: Double): Double = {

    assert(alphaL.size == xL.size)

    def ggg(x: Double, alpha: Double): Double = {

      x match {
        case x if ((0 <= x) && (x <= (4.0 / 5.0) * alpha)) => (4.0 / 5.0) - (x / alpha)
        case x if (((4.0 / 5.0) * alpha < x) && (x <= alpha)) => ((5 * x) / alpha) - 4
        case x if ((alpha < x) && (x <= (1 + 4.0 * alpha) / 5.0)) => 1 + 5 * (x - alpha) / (alpha - 1)
        case x if (((1 + 4 * alpha) / 5.0 < x) && (x <= 1)) => (4.0 / 5.0) + (x - 1) / (1 - alpha)
      }
    }

    val elementsG: List[Double] = (xL zip alphaL map { case (x, a) => ggg(x, a) })
    val sumofG: Double = elementsG.sum
    val res: Double = -pow((1 / xL.size.toDouble) * sumofG, beta)

    res
  }
  /*
    var x1  : List[Double]=List( 0.23 , 0.78)
    val alpha: List[Double] = List(0.6, 0.4)
    val  beta: Double = 3.5
    val test = deceptiveFunction(x1,alpha,beta)
    println("test en 2D")
    println("x", x1)
    println("alpha", alpha)
    println("beta", beta)
    println("valeur de la fonction " , test)
  */

  def stochasticMultiplicativeNoise(xL: List[Double] ) : Double ={
    // TODO demander romain pour rng à passer en param pour contrôler

    val suggestedDomain = List.fill(xL.size)(List(-5,5))

    val epsilonL = List.fill(xL.size)(random())


    val funcArgsByIndex  =  (xL zip epsilonL).zipWithIndex
    val res: List[Double] = funcArgsByIndex.map{case ((x,epsilon),index) =>  epsilon * abs( x - 1/(index + 1).toDouble) }

    res.sum
  }

  // valeurs recommandées pour x : entre -5 et 5 , minimum en 2D atteint en (1,0.5)
  /*
    val x : List[Double] = List(1 ,0.5)
    println(stochasticMultiplicativeNoise(x))
  */

  def crossLegTable(x1: Double, x2: Double) : Double ={

    val denom : Double = 1 + abs(exp(abs(100-( sqrt( pow(x1,2) + pow(x2,2) ) / math.Pi))) * sin(x1)*sin(x2) )

    -1 / pow(denom,0.1)
  }

  // optimum global = -1 obtenu en (0,0)
  /*
  val x = 5.0
  val y = 5.0
  println(crossLegTable(x,y))
*/

  def dropWave(x1: Double, x2 : Double):Double = {
    val globalOptimaLocations = List(0.0,0.0)
    val suggestedDomain = List(List(-5.12, 5.12), List(-5.12, 5.12))

    val num : Double = - (1 + cos(12 * sqrt(pow(x1,2) + pow(x2,2) )))
    val denom : Double = 0.5 * (pow(x1,2) + pow(x2,2)) + 2
    num/denom
  }

  // optimum global de -1 obtenu en 0.0
  /*
  val x = -5.12
  val y = -5.12
  println(dropWave(x,y))
  */

  def wavy (xL : List[Double], k:Int) : Double ={

    val globalOptimaLocations = List(List(-Pi, 12.275), List(Pi, 2.275), List(9.42478, 2.475))
    val suggestedDomain = List(List(-5, 20), List(-5, 20))

    val res = xL.map{case x => cos(k*x)*exp(-0.5* pow(x,2))}
    1 - (1/xL.size.toDouble) * res.sum

  }
  // xi taken within [-pi;pi]
  // k usually set to 10
  // global optimum obtained in xi = 0.0
  //  val x: List[Double] = List(Pi, Pi)
  //  println(wavy(x,10))



  def easom(x1: Double, x2: Double): Double ={
    val globalOptimaLocations = List(List(-100,100), List(-100,100))
    val suggestedDomain = List(Pi, Pi)

    val res = -cos(x1)*cos(x2)*exp( -pow(x1-Pi,2) - pow(x2-Pi,2)   )
    res
  }

  // global optima = -1
  // println(easom(Pi,Pi))

  def deJongFifth (x1 : Double, x2: Double): Double={

    val suggestedDomain = List(List(-65.536, 65.536),List(-65.536, 65.536))


    val firstALine = List.fill(5)(List(-32, -16, 0 , 16 , 32)).flatten
    val secondAline = List.fill(5)(-32) ::: List.fill(5)(-16) ::: List.fill(5)(0) ::: List.fill(5)(16) :::List.fill(5)(32)


    // N.B. index i appears as term in the sum from 1 to 25 of terms so => i + 1 in the formula
    def term(i:Int, x1:Double, x2:Double) : Double =  {
      1.0 / ( (i.toDouble + 1 ) + pow(x1 -  firstALine(i).toDouble, 6) + pow(x2 - secondAline(i).toDouble, 6)  )
    }

    var sommeTermes = 0.0
    (0 to 24 by 1).foreach(sommeTermes += term(_,x1,x2))
    pow(( 0.002 + sommeTermes),-1)

  }

  //println(deJongFifth(16.0, 32.0))

  def goldsteinPrice(x1: Double, x2: Double): Double ={

    val suggestedDomain = List(List(-2,2), List(-2,2))
    val globalOptimaLocations = List(0, -1)


    (1 + pow((x1 + x2 + 1 ),2) * (19-14*x1 + 3*pow(x1,2) - 14*x2 + (6 * x1 * x2) + 3* pow(x2,2)) )*(30 + pow((2*x1 - 3*x2),2) * (18- 32*x1 + 12* pow(x1,2) + 48*x2 - (36 * x1 * x2) + (27* pow(x2,2)) ))

  }
  //global optima is 3
  //println(goldsteinPrice(0,-1))

  def griewank(xL: List[Double]): Double ={

    val suggestedDomain = List.fill(xL.size)(List(-40,40))
    val globalOptilmaLocation = List.fill(xL.size)(0.0)


    val sumXsquare:Double = xL.map{ x => pow(x,2)}.sum
    //val indices = (0 to xL.size-1 by 1)
    //val productCos : Double= indices.map{ i => cos(xL(i) / sqrt(i+1))}.product

    val productCos : Seq[Double] = xL.zip (Stream from 1 ) map{
      case(x: Double,i: Int) => cos(x / sqrt(i))
    }
    (1.0/4000.0) *  sumXsquare - productCos.product + 1
  }


  //println(griewank(List(100,100)))

  def langermann(x1: Double, x2: Double) : Double ={

    val suggestedDomain = List(List(0,10),List(0,10))



    val C: List[Double] = List( 5, 2, 1, 4, 9)
    val A: List[ Double] = List(3, 5, 2, 1, 7)


    val res : List[Double] = A zip C  map {
      case (a:Double,c: Double) => c * cos(Pi * (pow(x1-a,2) + pow(x2-a,2)) ) / exp((pow(x1-a,2) + pow(x2-a,2)) / Pi)
    }

    -res.sum


  }


  //println(langermann(2.00299219, 1.006096))

  def michalewicz(x1: Double, x2 : Double) : Double ={

    val suggestedDomain = List(List(0,Pi), List(0,Pi))


    val m : Int = 10
    val xL : List[Double] = List(x1,x2)
    val res = xL zip (Stream from 1) map{
      case(x,i) => sin(x)* pow(  sin(pow(i*x,2)/Pi) , 2*m)
    }

    -res.sum

  }

  //println(michalewicz(0,0))

  def rozenbrockValley(xL : List[Double]):Double= {

    val suggestedDomain = List.fill(xL.size)(List(-5, 10))
    val globalOptilmaLocation = List.fill(xL.size)(1.0)


    val indices  = (0 to xL.size-2 by 1)

    indices.map{
      case(i) => 100 * pow(pow(xL(i),2)-xL(i+1) ,2) + pow(xL(i) -1 , 2)
    }.sum
  }

  //println(rozenbrockValley(List(0,0)))



  def schwefel1(xL : List[Double]) : Double = {

    val suggestedDomain = List.fill(xL.size)(List(-100, 100))
    val globalOptilmaLocation = List.fill(xL.size)(0.0)

    val alpha = Pi

    val res: Double = xL.map{case(x) => pow(x,2)}.sum

    pow(res, sqrt(alpha))

  }



  //println(schwefel1(List(0,0,0)))


  def shubert(x1: Double, x2 : Double) : Double ={

    val suggestedDomain = List(List(-10, 10), List(-10, 10))

    def term(i: Int ,x: Double):Double ={
      i * cos( (i+1)*x + i)
    }

    val term1 : Double = (1 to 5 by 1).map{
      case(i) => term(i,x1)
    }.sum
    val term2 : Double = (1 to 5 by 1).map{
      case(i) => term(i,x2)
    }.sum

    term1 * term2

  }

  //one of the optima
  //println(shubert(-7.0835, 4.8580))

  def sixHumpCamelBack(x1: Double, x2: Double): Double ={

    val suggestedDomain = List(List(-5, 5), List(-5, 5))
    val globalOptilmaLocation = List(List(0.08984201368301331 , -0.7126564032704135), List(-0.08984201368301331, 0.7126564032704135))
    4*pow(x1,2) + x1*x2 -4*pow(x2,2)-2.1*pow(x1,4) + 4*pow(x2,4) + (pow(x1,6)/3.0)
  }

  //println(sixHumpCamelBack(0.08984201368301331 , -0.7126564032704135))


  def schafferF6(x1: Double, x2: Double ): Double={
    val suggestedDomain = List(List(-100, 100), List(-100, 100))
    val globalOptilmaLocation = List(0.0 , 0.0)
    val ssq= pow(x1,2)+ pow(x2,2)
    val numer: Double = pow(sin(sqrt(ssq)),2) - 0.5
    val deno : Double = 1 + 0.001*(ssq)

    0.5 + numer / pow(deno,2)
  }


  //println(schafferF6(0,0))

}