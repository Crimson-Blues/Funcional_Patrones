import Huffman._

val countList = ocurrencias(List('A', 'A','C','B','D','A','B'))

val treeList = listaDeHojasOrdenadas(countList)

val newList = combinar(treeList)

val newNewList = combinar(newList)