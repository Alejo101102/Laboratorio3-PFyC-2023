package object Huffman {

  // DEFINICION DE CLASES INICIALES
  abstract class ArbolH

  case class Nodo(izq: ArbolH, der: ArbolH,
                  cars: List[Char], peso: Int) extends ArbolH

  case class Hoja(car: Char, peso: Int) extends ArbolH

  // DESARROLLO DE FUNCIONES
  // Parte 1: Funciones escenciales y sencillas
  def peso(arbol: ArbolH): Int = arbol match {
    case Nodo(izq, der, cars, peso) => peso
    case Hoja(cars, peso) => peso
  }

  def cars(arbol: ArbolH): List[Char] = arbol match {
    case Nodo(izq, der, carsN, peso) => carsN
    case Hoja(cars, peso) => cars :: Nil
  }

  def hacerNodoArbolH(izq: ArbolH, der: ArbolH) = Nodo(izq, der, cars(izq) ::: cars(der), peso(izq) + peso(der))

  // Parte 2: Construyendo arboles de Huffman
  /*
  En este taller estamos trabajando con listas de caracteres
  La funcion cadenaALista crea una lista de caracteres correspondiente a una cadena dada
  * */
  def cadenaALista(cad: String): List[Char] = cad.toList

  def ocurrencias(cars: List[Char]): List[(Char, Int)] = {

    def insertar(numero: Int, cars: List[Char], caracter: Char): Int = cars match {
      case List() => numero
      case y :: ys =>

        if (!cars.isEmpty) {

          if (cars.head == caracter)
            insertar(numero + 1, cars.tail, caracter)
          else
            insertar(numero, cars.tail, caracter)

        }
        else numero
    }

    cars match {
      case List() => List()
      case List(y) => List((y, 1))
      case y :: ys => (y, insertar(1, ys, y)) :: ocurrencias(ys.filterNot(e => e == y))
    }
  }

  def listaDeHojasOrdenadas(frecs: List[(Char, Int)]): List[Hoja] = {
    def organizarArbolesPorEntero(frecs: List[(Char, Int)]): List[(Char, Int)] = {

      def auxiliar(x: (Char, Int), frecs: List[(Char, Int)]): List[(Char, Int)] = frecs match {
        case List() => List((x))
        case y :: ys =>
          if (x._2 < y._2)
            x :: frecs
          else
            y :: auxiliar(x, ys)
      }

      frecs match {
        case List() => List()
        case List(y) => List(y)
        case y :: ys => auxiliar(y, organizarArbolesPorEntero(ys))
      }
    }

    def convertirAHojas(frecs: List[(Char, Int)]): List[Hoja] = frecs match {
      case List() => List()
      case List(y) => List(Hoja(y._1, y._2))
      case y :: ys => Hoja(y._1, y._2) :: convertirAHojas(ys)
    }

    frecs match {
      case List() => List(Hoja(' ', 0))
      case List(y) => List(Hoja(y._1, y._2))
      case y :: ys => convertirAHojas(organizarArbolesPorEntero(frecs))
    }
  }

  def listaUnitaria(arboles: List[ArbolH]): Boolean = {
    arboles match {
      case List() => true
      case List(y) => true
      case y :: ys => false
    }
  }

  def combinar(arboles: List[ArbolH]): List[ArbolH] = {
    def organizarArbolesPorPeso(frecs: List[ArbolH]): List[ArbolH] = {

      def auxiliar(x: ArbolH, frecs: List[ArbolH]): List[ArbolH] = frecs match {
        case List() => List((x))
        case y :: ys =>
          if (peso(x) <= peso(y))
            x :: frecs
          else
            y :: auxiliar(x, ys)
      }

      frecs match {
        case List() => List()
        case List(y) => List(y)
        case y :: ys => auxiliar(y, organizarArbolesPorPeso(ys))
      }
    }

    def unir(y: ArbolH, arboles: List[ArbolH]): List[ArbolH] = {
      arboles match {
        case List() => List(y)
        case List(m) => List(hacerNodoArbolH(y, m))
        case m :: ms => organizarArbolesPorPeso((hacerNodoArbolH(y, m) :: ms))
      }
    }

    arboles match {
      case List() => List()
      case List(m) => arboles
      case y :: ys => unir(y, ys)
    }
  }

  def hastaQue(condicion: List[ArbolH] => Boolean, mezclar: List[ArbolH] => List[ArbolH])(listaOrdenadaA: List[ArbolH]): List[ArbolH] = {
    def mezclarLista(lista: List[ArbolH]): List[ArbolH] = mezclar(lista)

    def condicionCumplida(lista: List[ArbolH]): Boolean = condicion(lista)

    listaOrdenadaA match {
      case List() => List()
      case List(y) => if (condicionCumplida(mezclarLista(y :: Nil))) {
        mezclarLista(y :: Nil)
      } else {
        List()
      }
      case y :: ys =>
        val listaMezclada = mezclarLista(y :: ys)
        if (condicionCumplida(listaMezclada)) {
          hastaQue(condicion, mezclar)(listaMezclada)
        } else {
          listaMezclada
        }
    }
  }

  def crearArbolDeHuffman(cars: List[Char]): ArbolH = {
    val listaOdenadaArbol = listaDeHojasOrdenadas(ocurrencias(cars))
    if (cars.isEmpty) throw new Error("La lista esta vacia")
    else if (cars.length == 1) throw new Error("Se requieren al menos dos caracteres para crear un arbol de Huffman")
    else {
      val resultado = hastaQue(listaOdenadaArbol => (!listaUnitaria(listaOdenadaArbol)), listaOdenadaArbol => combinar(listaOdenadaArbol))(listaOdenadaArbol)
      resultado.head
    }
  }

  // Parte3: Decodificar

  type Bit = Int

  def decodificar(arbol: ArbolH, bits: List[Bit]): List[Char] = {

    def decodificarAux(arbolA: ArbolH, bit: List[Bit]): List[Char] = {
      bit match {
        case List() =>
          arbolA match {
            case Hoja(cars, peso) => cars :: Nil
            // case Nil => Nil
          }
        case List(y) =>
          arbolA match {
            case Nodo(izq, der, carsUno, peso) =>
              if (y == 0)
                decodificarAux(izq, Nil)
              else if (y == 1)
                decodificarAux(der, Nil)
              else throw new Error("El bit es diferente de 0 o 1")
            // case Nil => Nil
          }
        case y :: ys =>
          arbolA match {
            case Hoja(cars, peso) =>
              cars :: decodificarAux(arbol, y :: ys)
            case Nodo(izq, der, carsUno, peso) =>
              if (y == 0)
                decodificarAux(izq, ys)
              else if (y == 1)
                decodificarAux(der, ys)
              else
                throw new Error("El bit es diferente de 0 o 1")
            // case Nil => Nil
          }
      }
    }

    decodificarAux(arbol, bits)
  }

  // Parte 4a: Codificando usando arboles de Huffman
  def codificar(arbol: ArbolH)(texto: List[Char]): List[Bit] = {
    def caracterEstaPresente(chars: List[Char], caracter: Char): Boolean = {
      chars match {
        case Nil => false
        case List(y) =>
          if (y == caracter) {
            true
          }
          else {
            false
          }
        case y :: ys =>
          if (y == caracter) {
            true
          }
          else {
            caracterEstaPresente(ys, caracter)
          }
      }
    }

    def codificarA(arbol: ArbolH, bits: List[Bit], texto: List[Char]): List[Bit] = {

      def codificarAux(arbol: ArbolH, bits: List[Bit], texto: Char): List[Bit] = {
        arbol match {
          case Hoja(car, peso) =>
            bits.reverse
          case Nodo(izq, der, car, peso) =>
            if (caracterEstaPresente(car, texto)) {
              if (caracterEstaPresente(cars(izq), texto))
                codificarAux(izq, bits.::(0), texto)
              else
                codificarAux(der, bits.::(1), texto)
            }
            else throw new Error("El caracter " + texto + " no esta en el arbol")
          // case Nil => Nil
        }
      }

      texto match {
        case Nil => bits
        case List(y) => codificarA(arbol, codificarAux(arbol, bits, y), Nil)
        case y :: ys => codificarA(arbol, codificarAux(arbol, bits, y), Nil) ++ codificarA(arbol, bits, ys)
      }
    }

    codificarA(arbol, Nil, texto)
  }

  // Parte 4b: Codificando usando tablas de codigos
  type TablaCodigos = List[(Char, List[Bit])]

  def codigoEnBits(tabla: TablaCodigos)(car: Char): List[Bit] = {
    tabla match {
      case Nil => Nil
      case List(y) =>
        if (y._1 == car)
          y._2
        else throw new Error("El caracter esta en la tabla")
      case y :: ys =>
        if (y._1 == car) y._2
        else codigoEnBits(ys)(car)
    }
  }

  def mezclarTablasDeCodigos(a: TablaCodigos, b: TablaCodigos): TablaCodigos = {
    a match {
      case Nil => b
      case List(y) => y :: b
      case y :: ys => a ++ b
    }
  }

  def convertir(arbol: ArbolH): TablaCodigos = {

    def auxiliarTabla(arbol: ArbolH, a: List[Char]): TablaCodigos = {
      arbol match {
        case Nodo(izq, der, car, peso) =>
          a match {
            case Nil => Nil
            case List(y) => (y, (codificar(arbol)(y :: Nil))) :: Nil
            case y :: ys => (y, (codificar(arbol)(y :: Nil))) :: auxiliarTabla(arbol, ys)
          }
        // case Nil => Nil
      }
    }

    arbol match {
      case Nodo(izq, der, car, peso) => mezclarTablasDeCodigos(auxiliarTabla(arbol, cars(izq)), auxiliarTabla(arbol, cars(der)))
      // case Nil => Nil
    }
  }

  def codificarRapido(arbol: ArbolH)(texto: List[Char]): List[Bit] = {
    val tabla = convertir(arbol)
    texto match {
      case Nil => Nil
      case List(y) => codigoEnBits(tabla)(y)
      case y :: ys => codigoEnBits(tabla)(y) ++ codificarRapido(arbol)(ys)
    }
  }

}
