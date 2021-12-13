import scala.util.{Failure, Success, Try}

case class Calabozo(puerta_principal: Puerta){
  def recorrer(grupo: Grupo, puerta: Puerta = puerta_principal): Try[Grupo] = grupo.intentarAbrirPuerta(puerta).flatMap(grupito => this.resultadoSobrePuerta(grupito, puerta))

  def resultadoSobrePuerta(grupo: Grupo, puerta: Puerta): Try[Grupo] = grupo.verSiLaPuertaEsDeSalida(this, puerta)

  def elMejorGrupo(grupos: Set[Grupo]): Grupo = {

    val grupos_ganadores = grupos.map(this.recorrer(_)).filter(_.isSuccess)
    if(grupos_ganadores.isEmpty)
      throw new MejorGrupoFallidoException
    else
      grupos_ganadores.map(_.get).maxBy(_.puntaje())

  }

  def nivelesNecesariosParaCompletar(grupo: Grupo, niveles_necesarios_a_subir: Int = 0): Int = {

    val resultado = recorrer(grupo, puerta_principal)
    if(niveles_necesarios_a_subir <= 20){
      resultado match {
        case Success(_) => niveles_necesarios_a_subir
        case Failure(_) => nivelesNecesariosParaCompletar(grupo.subirNivel(), niveles_necesarios_a_subir + 1)
      }
    } else throw new NecesarioSubirMasDe20LevelException
    }
}
class CalabozoFallidoException(private val message: String = "Calabozo fallido",
                                 private val cause: Throwable = None.orNull)
  extends Exception(message, cause)

class NecesarioSubirMasDe20LevelException(private val message: String = "Es necesario que el grupo suba mas de 20 niveles para completar el calabozo",
                               private val cause: Throwable = None.orNull)
  extends Exception(message, cause)

case class MejorGrupoFallidoException(private val message: String = "Ningun grupo sobrevivio al calabozo",
                               private val cause: Throwable = None.orNull)
  extends Exception(message, cause)

