package object Huffman {

  abstract class ArbolH
  case class Nodo(izq: ArbolH, der: ArbolH,
                  cars: List[Char], peso: Int) extends ArbolH
  case class Hoja(car: Char, peso: Int) extends ArbolH

  def peso(arbol: ArbolH): Int = arbol match {
    case Nodo(izq, der, cars, peso_node) => peso_node
    case Hoja(car, peso_hoja) => peso_hoja
  }

  def cars(arbol: ArbolH): List[Char] = arbol match {
    case Nodo(izq, der, cars_node, peso_node) => cars_node
    case Hoja(car, peso) => List(car)
  }

  def hacerNodoArbolH(izq: ArbolH, der: ArbolH): ArbolH =
    Nodo(izq, der, cars(izq) ::: cars(der), peso(izq) + peso(der))

  def ocurrencias(cars: List[Char]): List[(Char, Int)] = {
    def classify(car: Char, countList: List[(Char, Int)]): List[(Char, Int)] = countList match {
      case Nil => List((car, 1))
      case (seenCar, count) :: xs =>
        if (seenCar == car) (seenCar, count + 1) :: xs
        else (seenCar, count) :: classify(car, xs)
    }

    if (cars.isEmpty) List()
    else {
      val tailCount = ocurrencias(cars.tail)
      classify(cars.head, tailCount)
    }
  }

  def listaDeHojasOrdenadas(frecs: List[(Char,Int)]): List[Hoja] = {
    def insertLeaf(leaf: Hoja,leafList: List[Hoja]): List[Hoja] = leafList match {
      case Nil => List(leaf)
      case head::xs => 
        if (leaf.peso < head.peso)  leaf::head::xs
        else head::insertLeaf(leaf, xs)
      
    }
    if (frecs.isEmpty) List()
    else {
      val (headChar, headCount) = frecs.head
      val headLeaf = Hoja(headChar, headCount)
      insertLeaf(headLeaf, listaDeHojasOrdenadas(frecs.tail))
    }
  }

  def listaUnitaria(arboles: List[ArbolH]):Boolean = {
    if (arboles.isEmpty) false
    else{
      if (arboles.length == 1) true
      else false
    }
  }

  def getPeso(tree: ArbolH): Int = tree match {
    case Hoja(car, peso) => peso
    case Nodo(izq, der, cars, peso) => peso
    case _ => 0
  }

  def getCars(tree: ArbolH): List[Char] = tree match {
    case Hoja(car, peso) => List(car)
    case Nodo(izq, der, cars, peso) => cars
    case _ => List()
  }
  
  def combinar(arboles : List[ArbolH]):List[ArbolH] = {
    def insertTree(node: Nodo, treeList: List[ArbolH]): List[ArbolH] = treeList match {
      case Nil => List(node)
      case head::xs => 
        val headPeso = getPeso(head)
        if (node.peso < headPeso) node :: head :: xs
        else head :: insertTree(node, xs)
      
      
    }
    val newList = arboles match {
      case Nil => List()
      case arbol::Nil => List(arbol)
      case arbolA::arbolB::xs => 
        val carsA = getCars(arbolA)
        val carsB = getCars(arbolB)
        val pesoA = getPeso(arbolA)
        val pesoB = getPeso(arbolB)
        insertTree(Nodo(arbolA, arbolB, carsA ++ carsB, pesoA + pesoB), xs)
    }
    newList
  }

}