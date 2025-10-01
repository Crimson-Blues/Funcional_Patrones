import Huffman._

val charList = List('A', 'A','C','B','D','A','B')
val countList = ocurrencias(List('A', 'A','C','B','D','A','B'))

val treeList = listaDeHojasOrdenadas(countList)

val newList = combinar(treeList)

val newNewList = combinar(newList)

crearArbolDeHuffman(charList)