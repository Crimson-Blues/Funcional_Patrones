package object Huffman {

  abstract class ArbolH

  case class Nodo(izq: ArbolH, der: ArbolH,
                  cars: List[Char], peso: Int) extends ArbolH

  case class Hoja(car: Char, peso: Int) extends ArbolH

  // Parte 1: Funciones esenciales y sencillas
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

  // Parte 2: Construyendo arboles de Huffman
  def cadenaALista(cad: String): List[Char] = cad.toList

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

  def listaDeHojasOrdenadas(frecs: List[(Char, Int)]): List[Hoja] = {
    def insertLeaf(leaf: Hoja, leafList: List[Hoja]): List[Hoja] = leafList match {
      case Nil => List(leaf)
      case head :: xs =>
        if (leaf.peso < head.peso) leaf :: head :: xs
        else head :: insertLeaf(leaf, xs)
    }

    if (frecs.isEmpty) List()
    else {
      val (headChar, headCount) = frecs.head
      val headLeaf = Hoja(headChar, headCount)
      insertLeaf(headLeaf, listaDeHojasOrdenadas(frecs.tail))
    }
  }

  def listaUnitaria(arboles: List[ArbolH]): Boolean = {
    if (arboles.isEmpty) false
    else {
      if (arboles.length == 1) true
      else false
    }
  }

  def combinar(arboles: List[ArbolH]): List[ArbolH] = {
    def insertTree(tree: ArbolH, treeList: List[ArbolH]): List[ArbolH] = treeList match {
      case Nil => List(tree)
      case head :: xs =>
        val headPeso = peso(head)
        if (peso(tree) < headPeso) tree :: head :: xs
        else head :: insertTree(tree, xs)

    }

    val newList = arboles match {
      case Nil => List()
      case arbol :: Nil => List(arbol)
      case arbolA :: arbolB :: xs => insertTree(hacerNodoArbolH(arbolA, arbolB), xs)
    }
    newList
  }

  def hastaQue(cond: List[ArbolH] => Boolean, mezclar: List[ArbolH] => List[ArbolH])(listaOrdenadaArboles: List[ArbolH]): List[ArbolH] = {
    if (listaOrdenadaArboles.isEmpty) List()
    else {
      if (cond(listaOrdenadaArboles)) listaOrdenadaArboles
      else hastaQue(cond, mezclar)(mezclar(listaOrdenadaArboles))
    }
  }

  def crearArbolDeHuffman(cars: List[Char]): ArbolH = {
    hastaQue(listaUnitaria, combinar)(listaDeHojasOrdenadas(ocurrencias(cars))).head
  }

  // Parte 3 : Decodificar
  type Bit = Int

  def decodificar(arbol: ArbolH, bits: List[Bit]): List[Char] = {
    def recorrer(actual: ArbolH, bitsRest: List[Bit]): (List[Char]) = actual match {
      case Hoja(car, _) =>
        if (bitsRest.isEmpty) List(car) else car :: recorrer(arbol, bitsRest)
      case Nodo(izq, der, _, _) => bitsRest match {
        case Nil => List() //throw new Error("Bits insuficientes. No se puede decodificar")
        case bit :: tail =>
          if (bit == 0) recorrer(izq, tail)
          else recorrer(der, tail)
      }
    }

    if (bits.isEmpty) List()
    else recorrer(arbol, bits)
  }

  // Parte 4a: Codificando usando arboles de Huffman
  def codificar(arbol: ArbolH)(texto: List[Char]): List[Bit] = {
    def buscar(arbol: ArbolH, c: Char): List[Bit] = arbol match {
      case Hoja(car, _) =>
        if (car == c) List()
        else throw new Error("El carácter no fue encontrado en el árbol")
      case Nodo(izq, der, _, _) =>
        if (cars(izq).contains(c)) 0 :: buscar(izq, c)
        else if (cars(der).contains(c)) 1 :: buscar(der, c)
        else throw new Error("El carácter no fue encontrado en el árbol")
    }

    texto match {
      case Nil => List()
      case c :: tail => buscar(arbol, c) ++ codificar(arbol)(tail)
    }
  }

  // Parte 4b : Codificando usando tablas de codigos
  type TablaCodigos = List[(Char, List[Bit])]

  def codigoEnBits(tabla: TablaCodigos)(car: Char): List[Bit] = {
    val listaBits = tabla match {
      case Nil => List()
      case (c, bits) :: tail =>
        if (c == car) bits
        else codigoEnBits(tail)(car)
    }
    listaBits
  }


  def mezclarTablasDeCodigos(a: TablaCodigos, b: TablaCodigos): TablaCodigos = {
    def agregarBit(tab: TablaCodigos, bit: Bit): TablaCodigos = tab match {
      case Nil => List()
      case (c, bits) :: tail => (c, bit :: bits) :: agregarBit(tail, bit)
    }
    agregarBit(a, 0) ++ agregarBit(b, 1)
  }

  def convertir(arbol: ArbolH): TablaCodigos = {
    val tabla = arbol match {
      case Hoja(c, _) => List((c, List()))
      case Nodo(izq, der, _, _) =>
        val tablaIzq = convertir(izq)
        val tablaDer = convertir(der)
        mezclarTablasDeCodigos(tablaIzq, tablaDer)
    }
    tabla
  }

  def codificarRapido(arbol: ArbolH)(texto: List[Char]): List[Bit] = {
    val tabla = convertir(arbol)
    def recorrer(chars: List[Char]): List[Bit] = chars match {
      case Nil => List()
      case head :: tail => codigoEnBits(tabla)(head) ++ recorrer(tail)
    }
    recorrer(texto)
  }
}