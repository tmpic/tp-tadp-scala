
case class Estadistica(fuerza: Int, velocidad: Int, nivel: Int, salud: Int, dinero: Int){
  def subirNivel(): Estadistica = copy(nivel = nivel+1)

  def agregarDinero(monto: Int): Estadistica = copy(dinero = dinero+monto)

  def sufrirDanio(danio: Int): Estadistica = copy(salud = salud - danio)

  def aumentarFuerza(cantidad: Int): Estadistica = copy(fuerza = fuerza + cantidad)
}

case class Heroe(trabajo: Trabajo, estadistica: Estadistica, criterio_heroe: CriterioHeroe, criterio_puerta: CriterioPuerta){

  def fuerza(): Int = trabajo.fuerza(this)
  def velocidad(): Int = estadistica.velocidad
  def nivel(): Int = estadistica.nivel
  def salud(): Int = estadistica.salud
  def dinero(): Int = estadistica.dinero

  def subirNivel(): Heroe = this.copy(estadistica = this.estadistica.subirNivel())

  def agregarDinero(monto: Int): Heroe = this.copy(estadistica = this.estadistica.agregarDinero(monto))

  def sufrirDanio(danio: Int): Heroe = this.copy(estadistica = this.estadistica.sufrirDanio(danio))

  def esDeTipo(tipo: Trabajo): Boolean = trabajo.esDeTipo(tipo)

  def estaVivo(): Boolean = this.salud() > 0

  def imprimirDatosHeroe(): Unit ={
    trabajo match {
      case Guerrero => println("Heroe: Guerrero")
      case Ladron => println("Heroe: Ladron")
      case Mago(_) => println("Heroe: Mago")
    }
    println(s"fuerza: ${fuerza()}")
    println(s"velocidad: ${velocidad()}")
    println(s"nivel: ${nivel()}")
    println(s"salud: ${salud()}")
    println("\n")
  }
}

trait Trabajo {

  def esDeTipo(tipo_buscado: Trabajo): Boolean = this.tipo == tipo_buscado

  def tipo(): Trabajo

  def fuerza(heroe: Heroe) : Int = heroe.estadistica.fuerza
}

case object Guerrero extends Trabajo {
  def tipo(): Guerrero.type = Guerrero

   override def fuerza(heroe: Heroe) : Int = (heroe.estadistica.fuerza + heroe.estadistica.nivel * 0.2).round.asInstanceOf[Int]
}

case class Hechizo(nombre: String)

case class Mago(hechizos_a_aprender: Set[(Hechizo, Int)]) extends Trabajo {
  def tipo(): Mago.type = Mago

   def hechizos_aprendidos(heroe: Heroe): Set[(Hechizo, Int)] = hechizos_a_aprender.filter{ case (hechizo, lvl_requerido) => lvl_requerido <= heroe.nivel()}

   def conoceHechizo(heroe: Heroe, hechizo: Hechizo) = hechizos_aprendidos(heroe).exists(_._1.nombre == hechizo.nombre)
}

case object Ladron extends Trabajo {
  def tipo(): Ladron.type = Ladron

  def habilidadManos(heroe: Heroe): Int = heroe.estadistica.nivel * 3
}

case object Mago extends Trabajo {
  override def tipo(): Trabajo = ???
}

/*
TODO ejemplo juan
class Golondrina{
  def volar(): String = "volar"

  def cantar(): String = "pio"
}

class Gaviota{
  def volar(): String = "otroVolar"

  def cantar(): String = "otroPio"
}
class Pajaro{
  def volar(): String = "otroVolar"

  def cantar(): String = "otroPio"
}
class Ave{

}

case object GolondrinaFuncional extends Ave

case object GaviotaFuncional extends Ave

case object Pajaro extends Ave

def volar(ave: Ave) : String = ave match{
  case GolondrinaFuncional => "volar"
  case GaviotaFuncional => "otroVolar"
  case Pajaro => "otroVolar"
}

def cantar(ave: Ave) : String = ave match {
  case GolondrinaFuncional => "pio"
  case GaviotaFuncional => "otroPio"
  case Pajaro => "otroPio"
}
*/

/*//TODO otro ejemplo de juan, sobre como matchear subtipos
def asd2(a: A) = a match {
  case b: B.type => b.asd()
  case _ => "holis"
}

trait A {}

case object B extends A {
  def asd(): String = "asd"
}

case object C extends A{}
*/

/*//TODO ejemplo juan double dispatch en funcional vs objetos.
(Puerta, Trabajo) ->
    (Escondida, Mago)
    (Escondida, Ladron)

def puedeAbrir(puerta: Puerta, trabajo: Trabajo, grupo: Grupo) = case (puerta, trabajo) match  {
  case (_, Ladron(habilidad)) if habilidad >= 20 => true
  case (PuertaCerrada, Ladron(_)) => grupo.tiene(ganzuas)
  case (PuertaCerrada, mago@Mago(hechizos)) => mago.conoce(alohomora)
}

trait Puerta {
  def puedeSerAbiertaPor(trabajo: Trabajo, grupo: Grupo): Boolean
}

class PuertaCerrada extends Puerta {
  def puedeSerAbiertaPor(trabajo: Trabajo, grupo: Grupo): Boolean = trabajo.puedeAbrirPuertaCerrada(grupo)
}

class PuertaEncantada(hechizo: Hechizo) extends Puerta {
  def puedeSerAbiertaPor(trabajo: Trabajo, grupo: Grupo): Boolean = trabajo.puedeAbrirPuertaEncantada(hechizo, grupo)
}

trait Trabajo {
  def puedeAbrirPuertaCerrada(grupo: Grupo)
  def puedeAbrirPuertaEncantada(hechizo: Hechizo, grupo: Grupo)
}

class Guerrero extends Trabajo {
  def puedeAbrirPuertaCerrada(grupo: Grupo) = false
  def puedeAbrirPuertaEncantada(hechizo: Hechizo, grupo: Grupo) = false
}

class Ladron(habilidad: Int) extends Trabajo {
  def puedeAbrirPuertaCerrada(grupo: Grupo) = grupo.tiene(ganzuas)
  def puedeAbrirPuertaEncantada(hechizo: Hechizo, grupo: Grupo) = habilidad >= 6
}

case class Puerta(tipo_puerta: TipoPuerta, habitacion: Habitacion){

  def puedeAbrir(grupo: Grupo): Boolean = tipo_puerta match {
    case PuertaCerrada => grupo.tieneItem(llave) || (grupo.tieneHeroeDeTipo(Ladron) && grupo.tieneItem(ganzuas))
    case PuertaEscondida => grupo.grupoCumpleCondicion(condicionSegunHeroe(Hechizo("vislumbrar")))
    case PuertaEncantada(hechizo) => grupo.grupoCumpleCondicion(condicionSegunHeroe(hechizo))
  }

  private def condicionSegunHeroe (hechizo: Hechizo)(heroe:Heroe): Boolean = heroe.trabajo match {
    case Ladron(habilidad) if tipo_puerta == PuertaEscondida => habilidad >= 6
    case Mago(hechizos) => hechizos.exists{case (hechizo_del_mago, lvl_requerido) => hechizo_del_mago.nombre.equals(hechizo.nombre)}
    case _ => false
  }

  val llave = Item("llave")
  val ganzuas = Item(nombre = "ganzuas")
}

sealed trait TipoPuerta

case object PuertaCerrada extends TipoPuerta

case object PuertaEscondida extends TipoPuerta

case class PuertaEncantada(hechizo: Hechizo) extends TipoPuerta
 */