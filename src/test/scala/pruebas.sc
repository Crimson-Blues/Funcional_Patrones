import Huffman._

val charList = List('A', 'A','C','B','D','A','B')
val countList = ocurrencias(List('A', 'A','C','B','D','A','B'))

val treeList = listaDeHojasOrdenadas(countList)

val hasta =  hastaQue(listaUnitaria,combinar)

val arbolCurry = hasta(treeList)

val arbol = crearArbolDeHuffman(charList)

