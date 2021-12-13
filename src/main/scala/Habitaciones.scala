
case class Habitacion(tipoHabitacion: TipoHabitacion, puertas: List[Puerta] = List.empty[Puerta]){
//Las habitaciones hubiesen quedado mejor modeladas con polimorfismo tal vez...
  def aplicarSituacion(grupo: Grupo): Grupo = tipoHabitacion match {
    case HabitacionVacia => grupo
    case HabitacionConItem(item) => grupo.agregarItem(item)
    case HabitacionConDardos => grupo.sufrirDanio(10)
    case HabitacionConLeon => grupo.expulsar(grupo.elMasLento())
    case HabitacionConHeroe(heroe_a_enfrentar) => {
      val grupo_con_desconocido = Grupo(integrantes = grupo.integrantes + heroe_a_enfrentar)

      val lider_del_grupo = grupo.lider()

      if(Pelea.seAgradan(heroe_a_enfrentar, grupo) && Pelea.seAgradan(lider_del_grupo, grupo_con_desconocido) )
        grupo.sumarIntegrante(heroe_a_enfrentar)

      else Pelea.pelear(heroe_a_enfrentar, grupo)
    }
    case HabitacionConMotin if grupo.cantidadIntegrantes().equals(1) => grupo
    case HabitacionConMotin => {
      val heroe_mas_fuerte = grupo.elMasFuerte()
      val grupo_sin_integrante_mas_fuerte = grupo.expulsar(heroe_mas_fuerte)
      Pelea.pelear(heroe_mas_fuerte, grupo_sin_integrante_mas_fuerte)
    }
  }
}

trait TipoHabitacion

case object HabitacionConMotin extends TipoHabitacion

case object HabitacionVacia extends TipoHabitacion

case class HabitacionConItem(item: Item) extends TipoHabitacion

case object HabitacionConDardos extends TipoHabitacion

case object HabitacionConLeon extends TipoHabitacion

case class HabitacionConHeroe(heroe_a_enfrentar: Heroe) extends TipoHabitacion

object Pelea{
  def seAgradan(heroe: Heroe, grupo: Grupo): Boolean = heroe.criterio_heroe match {
    case Introvertido => grupo.integrantes.size <= 3
    case Bigote => !grupo.tieneHeroeDeTipo(Ladron)
    case Interesado(item) => grupo.tieneItem(item)
    case Loquito => true
  }

  def pelear(heroe: Heroe, grupo: Grupo): Grupo = {
    if(grupo.integrantes.map(_.fuerza()).sum > heroe.fuerza()) {
      grupo.copy(integrantes = grupo.integrantes.map(_.subirNivel()))

    } else grupo.copy(integrantes = grupo.integrantes.map(_.sufrirDanio(heroe.fuerza()/grupo.integrantes.size)))
  }
}

trait CriterioHeroe

case object Introvertido extends CriterioHeroe

case object Bigote extends CriterioHeroe

case class Interesado(item: Item) extends CriterioHeroe

case object Loquito extends CriterioHeroe


trait CriterioPuerta

case object Heroico extends CriterioPuerta

case object Ordenado extends CriterioPuerta

case object Vidente extends CriterioPuerta





