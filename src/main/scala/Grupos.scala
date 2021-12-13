import scala.util.{Failure, Success, Try}

case class Grupo(integrantes: Set[Heroe] = Set.empty[Heroe],
                 cofreComun: Cofre = new Cofre,
                 puertas_encontradas_sin_abrir: Set[Puerta] = Set.empty[Puerta],
                 puertas_encontradas_abiertas: Set[Puerta] = Set.empty[Puerta]) {

  def sumarIntegrante(nuevo_integrante: Heroe): Grupo = this.copy(integrantes = this.integrantes + nuevo_integrante)

  def hayPuertasQueSePuedenAbrir(): Boolean = puertas_encontradas_sin_abrir.exists(_.puedeAbrir(this))

  def intentarSeleccionarUnaSiguientePuerta(calabozo: Calabozo): Try[Grupo] = {

    if(this.hayPuertasQueSePuedenAbrir())
      calabozo.recorrer(this, elegirSiguientePuerta())
    else if(this.hayAlguienVivo())
      Failure(new CalabozoFallidoException(message = "El grupo no puede abrir mas puertas"))
    else
      Failure(new CalabozoFallidoException())

  }

  def verSiLaPuertaEsDeSalida(calabozo: Calabozo, puerta: Puerta): Try[Grupo] = {
    if(puerta.esDeSalida)
      return Success(this)
    else
      this.intentarSeleccionarUnaSiguientePuerta(calabozo)
  }

  def lider(): Heroe = integrantes.head

  def repartirGanancias(monto: Int): Grupo = this.copy(integrantes = integrantes.map(_.agregarDinero(monto / integrantes.size)))

  def estadoGrupo(): Grupo ={
    val nuevos_integrantes = integrantes.filter(_.estaVivo())
    integrantes.foreach(_.imprimirDatosHeroe())
    this.copy(integrantes = nuevos_integrantes)
  }

  def hayAlguienVivo(): Boolean = !integrantes.isEmpty

  def tieneItem(item: Item): Boolean = cofreComun.items.exists(_.nombre == item.nombre)

  def agregarItem(item: Item): Grupo = this.copy(cofreComun = this.cofreComun.copy(items = this.cofreComun.items + item))

  def sufrirDanio(cantidad: Int): Grupo = this.copy(integrantes = this.integrantes.map(_.sufrirDanio(cantidad)))

  def elMasLento(): Heroe = integrantes.minBy(_.velocidad())

  def expulsar(integrante: Heroe): Grupo = this.copy(integrantes = this.integrantes - integrante)

  def tieneHeroeDeTipo(tipo: Trabajo): Boolean = integrantes.exists(_.esDeTipo(tipo))

  def puntaje(): Int = {
    val heroes_vivos = integrantes.filter(_.estaVivo())
    val cantidad_heroes_vivos = heroes_vivos.size
    val cantidad_heroes_muertos = integrantes.size - cantidad_heroes_vivos
    cantidad_heroes_vivos * 10 - cantidad_heroes_muertos * 5 + cofreComun.items.size + integrantes.map(_.nivel()).max
  }

  def puedeAbrirPuerta(puerta: Puerta): Boolean = puerta.puedeAbrir(this)

  def intentarAbrirPuerta(puerta: Puerta): Try[Grupo] = {
    if(this.puedeAbrirPuerta(puerta)) {
      val nuevas_puertas_encontradas_abiertas = puertas_encontradas_abiertas + puerta
      val puertas_encontradas_sin_abrir_sin_la_recien_abierta = puertas_encontradas_sin_abrir - puerta
      val nuevo_grupo = puerta.habitacion.aplicarSituacion(this).estadoGrupo().copy(puertas_encontradas_abiertas = nuevas_puertas_encontradas_abiertas,
        puertas_encontradas_sin_abrir = puertas_encontradas_sin_abrir_sin_la_recien_abierta ++ puerta.habitacion.puertas.toSet)

      if (nuevo_grupo.hayAlguienVivo())
        return Success(nuevo_grupo)
      else
        return Failure(new CalabozoFallidoException)
    } else{
      return Failure(new CalabozoFallidoException)
    }
  }

  def grupoCumpleCondicion(condicion: Heroe => Boolean) : Boolean = this.integrantes.exists(condicion(_))

  def subirNivel(): Grupo = this.copy(integrantes = integrantes.map(_.subirNivel()))

  def elegirSiguientePuerta(): Puerta = {
    val puertas_que_pueden_ser_abiertas = puertasQuePuedenSerAbiertas()

    lider().criterio_puerta match {
    case Heroico => puertas_que_pueden_ser_abiertas.last
    case Ordenado => puertas_que_pueden_ser_abiertas.head
    case Vidente => puertas_que_pueden_ser_abiertas.maxBy(this.intentarAbrirPuerta(_) match {
      case Success(grupo) => grupo.puntaje()
    })
    }
  }

  def puertasQuePuedenSerAbiertas(): Set[Puerta] = puertas_encontradas_sin_abrir.filter(_.puedeAbrir(this))

  def elMasFuerte(): Heroe = integrantes.maxBy(_.fuerza())

  def cantidadIntegrantes(): Int = integrantes.size

}

case class Item(nombre: String){}

case class Cofre(items: Set[Item] = Set.empty[Item])


