package object Huffman

abstract class ArbolH
case class Nodo(izq:ArbolH, der:ArbolH,
                cars: List[Char], peso:Int) extends ArbolH
case class Hoja(car:Char, peso:Int) extends ArbolH

def peso(arbol: ArbolH):Int = arbol match {
  case Nodo(izq,der,cars,peso_node) => peso_node
  case Hoja(car, peso_hoja) => peso_hoja
}

def cars(arbol: ArbolH):List[Char] = arbol match {
  case Nodo(izq,der,cars_node,peso_node) => cars_node
  case Hoja(car, peso) => List(car)
}

def hacerNodoArbolH(izq: ArbolH, der:ArbolH): ArbolH =
  Nodo(izq, der, cars(izq) ::: cars(der),peso(izq) + peso(der))

def ocurrencias(cars:List[Char]):List[(Char,Int)] ={
  def classify(car:Char, countList: List[(Char,Int)]):List[(Char,Int)] = countList match {
    case Nil => List((car,1))
    case (seenCar, count)::xs =>
      if(seenCar==car) (seenCar,count+1)::xs
      else (seenCar, count)::classify(car, xs)
  }
  if (cars.isEmpty) List()
  else {
    val tailCount = ocurrencias(cars.tail)
    classify(cars.head, tailCount)
  }
}

