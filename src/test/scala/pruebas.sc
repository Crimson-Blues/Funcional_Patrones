import Huffman.*

val charList = List('A', 'A','C','B','D','A','B')
val countList = ocurrencias(List('A', 'A','C','B','D','A','B'))

val treeList = listaDeHojasOrdenadas(countList)

val newList = combinar(treeList)

val newNewList = combinar(newList)

crearArbolDeHuffman(charList)


// ----------------- Árboles Predefinidos -----------------
val nodo1 = Nodo(Hoja('a', 2), Hoja('b', 5), List('a','b'), 7)
val nodo2 = Nodo(Hoja('a', 1), Hoja('b', 5), List('a', 'b'), 6)
val nodo3 = Nodo(Hoja('c', 6), Hoja('d', 8), List('c', 'd'), 14)
val nodoAnidado = Nodo(nodo1, Hoja('c', 6), List('a','b','c'), 13)
val arbolRepetido = Nodo(Hoja('a', 2), Hoja('a', 3), List('a','a'), 5)

// ----------------- Función peso -----------------
peso(Hoja('a', 1))
peso(Hoja('b', 100))
peso(nodo1)
peso(nodoAnidado)
peso(Nodo(nodo2, nodo3, List('a','b','c', 'd'), peso(nodo2) + peso(nodo3)))

// ----------------- Función cars -----------------
cars(Hoja('a', 1))
cars(nodo1)
cars(nodoAnidado)
cars(arbolRepetido)
cars(Nodo(nodo2, nodo3, cars(nodo2) ++ cars(nodo3), 20))

// ----------------- Función ocurrencias -----------------
ocurrencias(List())
ocurrencias(List('a'))
ocurrencias(List('a', 'a', 'a', 'b', 'b'))
ocurrencias(List('a', 'b', 'c', 'd'))
ocurrencias(cadenaALista("cbacbaac"))

// ----------------- Función listaDeHojasOrdenadas -----------------
listaDeHojasOrdenadas(List())
listaDeHojasOrdenadas(List(('a',1)))
listaDeHojasOrdenadas(List(('a',1),('b',2),('c',3)))
listaDeHojasOrdenadas(List(('c',3),('a',1),('b',2)))
listaDeHojasOrdenadas(ocurrencias(cadenaALista("cbacbaac")))

// ----------------- Función listaUnitaria -----------------
listaUnitaria(List())
listaUnitaria(List(Hoja('a',1)))
listaUnitaria(List(Hoja('a',1), Hoja('b',2)))
listaUnitaria(List(Hoja('a',1), Hoja('b',2), Hoja('c',3), Hoja('d',4)))
listaUnitaria(List(nodo1))
listaUnitaria(List(nodo1, nodo3))

// ----------------- Función combinar -----------------
combinar(List())
combinar(List(Hoja('a',1)))
combinar(List(Hoja('a',1), Hoja('b',2)))
combinar(List(Hoja('c',2), Hoja('a',4), Hoja('b',5)))
combinar(listaDeHojasOrdenadas(ocurrencias(cadenaALista("cbacbaac"))))

// ----------------- Función codificar -----------------
val arbol = crearArbolDeHuffman( List('a','a','a','a','a','a','a','a','a','a','a','a','a','a','a',
  'b','b','b','b','b','b','b','b',
  'c','c','c','c','c',
  'd','d','d',
  'e','e','e','e','e','e','e','e','e','e','e',
  'f','f','f','f',
  'g',
  'h', 'h'
) )
codificar(arbol)(List())
codificar(arbol)(List('a'))
codificar(arbol)(List('b','b','a'))
codificar(arbol)(List('a','b','c','d','e','f','g','h'))
codificar(arbol)(List('i'))

// ----------------- Función decodificar -----------------
decodificar(arbol, List())
decodificar(arbol, List(1,1))
decodificar(arbol, List(1,1,1,0,1,0,0,1,1,0,0,0,0,1,0,0,0,1,0,0,1,0,1,0,0,1,1))
decodificar(arbol, List(1,1,0))
decodificar(arbol, List(0))

// ----------------- Función codigoEnBits -----------------
val tabla = convertir(arbol)
codigoEnBits(tabla)('a')
codigoEnBits(tabla)('i')
codigoEnBits(List())('a')
codigoEnBits(List(('x', List(1,0))))('x')
codigoEnBits(List(('a', List(0)), ('b', List(1, 1)), ('c', List(1, 0))))('c')

// ----------------- Función mezclarTablasDeCodigos -----------------
mezclarTablasDeCodigos(List(), List())
mezclarTablasDeCodigos(List(('a', List(1, 0))), List())
mezclarTablasDeCodigos(List(('a', List(0))), List(('b', List(1))))
mezclarTablasDeCodigos(List(('x', List(0)), ('y', List(1))), List(('z', List(1,0))))
val arbolA = crearArbolDeHuffman(cadenaALista("aaab"))
val arbolB = crearArbolDeHuffman(cadenaALista("cccdd"))
val tablaIzq = convertir(arbolA)
val tablaDer = convertir(arbolB)
mezclarTablasDeCodigos(tablaIzq, tablaDer)

