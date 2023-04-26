import Huffman._

def cadenaALista(cad: String): List[Char] = cad.toList

val arbolEjemplo = hacerNodoArbolH(
  hacerNodoArbolH(Hoja('x', 1), Hoja('e', 1)),
  Hoja('t', 2))

val lc = cadenaALista("La_vida_es_dura")
val lho = listaDeHojasOrdenadas(ocurrencias(lc))
listaUnitaria(lho)
crearArbolDeHuffman(lc)

// Pruebas propias

// Listas para jugar
val lista1 = List(hacerNodoArbolH(Hoja('x', 1), Hoja('e', 0)), hacerNodoArbolH(hacerNodoArbolH(Hoja('x', 1), Hoja('e', 0)), Hoja('t', 2)), hacerNodoArbolH(Hoja('x', 1), Hoja('e', 0)))
val lista2 = List(1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1)
val list1 = listaUnitaria(List(hacerNodoArbolH(Hoja('x', 1), Hoja('e', 0))))
val list2 = listaUnitaria(List(hacerNodoArbolH(Hoja('x', 1), Hoja('e', 0)), hacerNodoArbolH(hacerNodoArbolH(Hoja('x', 1), Hoja('e', 0)), Hoja('t', 2))))
val list3 = listaUnitaria(lista1) == true

// combinar
val combinacion1 = combinar(List(hacerNodoArbolH(Hoja('x', 0), Hoja('e', 1)), hacerNodoArbolH(hacerNodoArbolH(Hoja('m', 2), Hoja('s', 3)), Hoja('t', 5)), hacerNodoArbolH(Hoja('y', 6), Hoja('n', 10))))
val combinacion2 = combinar(List(Hoja('x', 1), Hoja('e', 2), Hoja('a', 3)))
val combinacion3 = combinar(lista1)
val combinacion4 = combinar(combinacion3)
combinar(combinacion4)

// Mas listas para jugar
val aux = List('D', 'r', 'a', 'g', 'o', 'n', 'b', 'a', 'l', 'l', 'z')
val arbolPrueba = crearArbolDeHuffman(aux)
val simbolosConjunto = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'ñ', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z')
val arbolPrueba2 = crearArbolDeHuffman(simbolosConjunto)

// decodificar
decodificar(arbolPrueba, List(0, 0, 0))
decodificar(arbolPrueba, List(0, 0, 1))
decodificar(arbolPrueba, List(0, 0, 0)) == List('o')
decodificar(arbolPrueba, List(0, 0, 0)) == List('o')//cambiar
decodificar(arbolPrueba, List(0, 0, 1)) //cambiar

/*
// codificar
codificar(arbolPrueba)(List('H', 'x', 'o', 'x', 'l', 'x','a'))
codificar(arbolPrueba)(List('H', 'o', 'l', 'a'))
val tablaA = List(('H', (codificar(arbolPrueba)(List('H')))), ('o', (codificar(arbolPrueba)(List('o')))), ('l', (codificar(arbolPrueba)(List('l')))), ('a', (codificar(arbolPrueba)(List('a')))))
val tablaB = List()
val tablaC = List(('H', (codificar(arbolPrueba)(List('H')))))

// mezclarTablasDeCodigos
val tablaE = mezclarTablasDeCodigos(tablaC, tablaA)
val tablaF = mezclarTablasDeCodigos(tablaC, tablaB)
val tablaG = mezclarTablasDeCodigos(tablaC, tablaC)
mezclarTablasDeCodigos(tablaA, tablaB)
mezclarTablasDeCodigos(tablaB, tablaA)

// codigoEnBits
codigoEnBits(tablaA)('C')
codigoEnBits(tablaC)('i')
codigoEnBits(tablaB)('n')
codigoEnBits(tablaE)('c')
codigoEnBits(tablaG)('o')

*/
// convertir
convertir(lista1.head)
convertir(combinacion1.head)
convertir(combinacion2.head)
convertir(arbolPrueba)
convertir(arbolPrueba2)

// codificar rapido
codificarRapido(arbolPrueba2)(List('h', 'o', 'l', 'a', 'm', 'u', 'n', 'd', 'o'))
codificarRapido(arbolPrueba2)(List('c','o','d','i','f','i','c','a','m','e'))
codificarRapido(arbolPrueba2)(List('h','a','n','o','i'))
codificarRapido(arbolPrueba2)(List('c','o','n','j','d','i','f','u','s','o'))
//codificarRapido(arbolPrueba2)(List('G','o','k','u','u','u','u'))

// ocurrencias
ocurrencias(List('c','o','n','j','d','i','f','u','s','o'))
ocurrencias(List('c'))
ocurrencias(List('c', 'o', 'n', 'j'))
ocurrencias(List('S', 'x', 'c', 'x', 'a', 'x', 'l', 'x', 'a'))
ocurrencias(List('D', 'r', 'a', 'g', 'o', 'n', 'b', 'a', 'l', 'l', 'z'))
