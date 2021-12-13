import scala.util.{Failure, Success, Try}

case class Puerta(tipo_puerta: TipoPuerta, habitacion: Habitacion, esDeSalida: Boolean = false){

  def puedeAbrir(grupo: Grupo): Boolean = grupo.integrantes.exists(heroe => (tipo_puerta, heroe.trabajo) match  {
    case (_, Ladron) if Ladron.habilidadManos(heroe) >= 20 => true
    case (PuertaCerrada, Ladron) => grupo.tieneItem(Item("ganzuas"))
    case (PuertaCerrada, _) => grupo.tieneItem(Item("llave"))
    case (PuertaEscondida, Ladron) if Ladron.habilidadManos(heroe) >= 6 => true
    case (PuertaEscondida, mago@Mago(hechizos)) => mago.conoceHechizo(heroe, Hechizo("vislumbrar"))
    case (PuertaEncantada(hechizo), mago@Mago(hechizos)) => mago.conoceHechizo(heroe, hechizo)
    case (_, _) => false
  })
}

sealed trait TipoPuerta

case object PuertaCerrada extends TipoPuerta

case object PuertaEscondida extends TipoPuerta

case class PuertaEncantada(hechizo: Hechizo) extends TipoPuerta




