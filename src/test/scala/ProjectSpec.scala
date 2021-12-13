import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers._
import org.scalatest.freespec.AnyFreeSpec


class ProjectSpec extends AnyFreeSpec {

  "Este proyecto" - {

    "cuando está correctamente configurado" - {
      "debería resolver las dependencias y pasar este test" in {
        Prueba.materia shouldBe "tadp"
      }
    }
    "heroes" - {
      "si le subo un nivel a un guerrero deberia de aumentar en 1" in {
        val estadistica = Estadistica(10,10,10,10, 0)
        val guerrero = Guerrero
        val heroe = Heroe(guerrero, estadistica, Loquito, null)

        heroe.subirNivel().nivel() shouldBe 11
      }

      "la fuerza de un guerrero es del valor base + un 20% por cada nivel" in {
        val estadistica = Estadistica(10,10,10,10, 0)
        val guerrero = Guerrero
        val heroe = Heroe(guerrero, estadistica, Loquito, null)

        heroe.fuerza() shouldBe 12
      }

      "el Mago aprende hechizos cuando cumple su nivel requerido" in {
        val estadistica = Estadistica(10,10,15,10, 0)

        val hechizos = Set((Hechizo("fireball"), 10), (Hechizo("lightning strike"), 15), (Hechizo("mequedesinnombres"), 20))

        val mago = Mago(hechizos)
        val heroe = Heroe(mago, estadistica, Loquito, null)

        mago.hechizos_aprendidos(heroe).head._1.nombre shouldBe "fireball"
        mago.hechizos_aprendidos(heroe).tail.head._1.nombre shouldBe "lightning strike"
      }

      "el ladron cuando sube de nivel aumenta en 3 su habilidad con las manos" in {
        val estadistica = Estadistica(10,10,3,10, 0)

        val ladron = Ladron
        val heroe = Heroe(ladron, estadistica, Loquito, null)

        val heroaso = heroe.subirNivel()
        heroaso.trabajo shouldBe Ladron
        heroaso.nivel() shouldBe 4
        Ladron.habilidadManos(heroaso) shouldBe 12
      }
      "un heroe te sabe responder si su trabajo es de tal tipo" in {
        val estadistica = Estadistica(10, 10, 10, 10, 10)

        val ladron = Ladron

        val heroe = Heroe(ladron, estadistica, Loquito, null)

        heroe.esDeTipo(Mago) shouldBe false

        heroe.esDeTipo(Ladron) shouldBe true

        heroe.esDeTipo(Guerrero) shouldBe false


      }
      "un grupo te sabe responder si existe un integrante con trabajo de tal tipo" in {
        val estadistica = Estadistica(10, 10, 10, 10, 0)
        val ladron = Ladron
        val heroe = Heroe(ladron, estadistica, Loquito, null)

        val grupo = Grupo(integrantes = Set(heroe))

        grupo.tieneHeroeDeTipo(Ladron) shouldBe true
        grupo.tieneHeroeDeTipo(Mago) shouldBe false
        grupo.tieneHeroeDeTipo(Guerrero) shouldBe false
      }
    }
    "puertas" - {
      "Un ladron con habilidad mayor a 20 puede abrir cualquier puerta" in {

        val estadistica = Estadistica(10, 10, 10, 10, 0)
        val ladron = Ladron
        val heroe = Heroe(ladron, estadistica, Loquito, null)

        val grupo = Grupo(integrantes = Set(heroe))

        val puertaCerrada = Puerta(PuertaCerrada, null, false)
        val puertaEscondida = Puerta(PuertaEscondida, null, false)
        val puertaEncantada = Puerta(PuertaEncantada(Hechizo("hechizaso")), null, false)

        grupo.puedeAbrirPuerta(puertaCerrada) shouldBe true
        grupo.puedeAbrirPuerta(puertaEscondida) shouldBe true
        grupo.puedeAbrirPuerta(puertaEncantada) shouldBe true

      }

      "Un ladron con habilidad mayor o igual a 6 puede abrir una puerta escondida pero no una puerta encantada" in {

        val estadistica = Estadistica(10, 10, 2, 10, 0)
        val ladron = Ladron
        val heroe = Heroe(ladron, estadistica, Loquito, null)

        val grupo = Grupo(integrantes = Set(heroe))

        val puertaEscondida = Puerta(PuertaEscondida, null, false)
        val puertaEncantada = Puerta(PuertaEncantada(Hechizo("hechizoMagico")), null, false)

        grupo.puedeAbrirPuerta(puertaEscondida) shouldBe true
        grupo.puedeAbrirPuerta(puertaEncantada) shouldBe false

      }

      "Si el grupo posee el item ganzuas y existe un Ladron en el grupo puede abrir la PuertaCerrada" in {

        val estadistica = Estadistica(10, 10, 2, 10, 0)

        val heroe = Heroe(Ladron, estadistica, Loquito, null)

        val grupo = Grupo(integrantes = Set(heroe)).agregarItem(Item("ganzuas"))

        val puerta = Puerta(PuertaCerrada, null, false)

        grupo.puedeAbrirPuerta(puerta) shouldBe true

      }

      "Si el grupo posee la llave para abrir la PuertaCerrada entonces puede abrirla" in {

        val estadistica = Estadistica(10, 10, 2, 10, 0)

        val heroe = Heroe(Guerrero, estadistica, Loquito, null)

        val grupo = Grupo(integrantes = Set(heroe)).agregarItem(Item("llave"))

        val puerta = Puerta(PuertaCerrada, null, false)

        grupo.puedeAbrirPuerta(puerta) shouldBe true

      }

      "Un mago que conoce el hechizo vislumbrar puede abrir una puerta escondida" in {

        val estadistica = Estadistica(10, 10, 10, 10, 0)

        val mago = Mago(Set((Hechizo("vislumbrar"), 5)))
        val heroe = Heroe(mago, estadistica, Loquito, null)

        val grupo = Grupo(integrantes = Set(heroe))

        val puerta = Puerta(PuertaEscondida, null, false)

        grupo.puedeAbrirPuerta(puerta) shouldBe true

      }

      "Un mago que conoce el hechizo de la puerta Encantada puede abrirla" in {

        val estadistica = Estadistica(10, 10, 10, 10, 0)

        val mago = Mago(Set((Hechizo("hechizorezarpado"), 5)))
        val heroe = Heroe(mago, estadistica, Loquito, null)

        val grupo = Grupo(integrantes = Set(heroe))

        val puerta = Puerta(PuertaEncantada(Hechizo("hechizorezarpado")), null, false)

        grupo.puedeAbrirPuerta(puerta) shouldBe true

      }

      "Una puerta Encantada no puede ser abierta si el grupo no posee un mago que conozca el hechizo o no posee un ladron con habilidad > 20" in {

        val estadistica = Estadistica(10, 10, 1, 10, 0)

        val unHeroe = Heroe(Ladron, estadistica, Loquito, null)
        val otroHeroe = Heroe(Guerrero, estadistica, Loquito, null)
        val mago = Mago(Set((Hechizo("hechizo_no_tan_zarpado"), 5)))
        val otroHeroeMas = Heroe(mago, estadistica, Loquito, null)

        val grupo = Grupo(integrantes = Set(unHeroe, otroHeroe, otroHeroeMas))

        val puerta = Puerta(PuertaEncantada(Hechizo("hechizorezarpado")), null, false)

        grupo.puedeAbrirPuerta(puerta) shouldBe false

      }
    }
    "calabozos" - {
      "test criterio puerta" in {
        val heroe = Heroe(Ladron, Estadistica(10, 10, 10, 50, 0), Loquito, Vidente)

        val grupo = Grupo(puertas_encontradas_sin_abrir = Set(Puerta(PuertaCerrada, Habitacion(HabitacionConDardos)), Puerta(PuertaCerrada, Habitacion(HabitacionConItem(Item("pancho")))))).sumarIntegrante(heroe)

        grupo.elegirSiguientePuerta() shouldBe Puerta(PuertaCerrada, Habitacion(HabitacionConItem(Item("pancho"))))
      }
    }

    "calabozos" - {
      "test recorrer calabozo" in {
        val heroe1 = Heroe(Ladron, Estadistica(10, 10, 10, 50, 0), Loquito, Heroico)

        val grupo = Grupo()
        val grupo2 = grupo.sumarIntegrante(heroe1)
        val listaConHabitacion = List(Puerta(PuertaEscondida, Habitacion(HabitacionConDardos), true))
        val puerta = Puerta(PuertaEscondida, Habitacion(HabitacionConDardos, List(Puerta(PuertaEscondida, Habitacion(HabitacionConDardos, List(Puerta(PuertaEscondida, Habitacion(HabitacionConDardos, listaConHabitacion), false))), false))), false)
        val calabozo = Calabozo(puerta)

        val resultado_calabozo = calabozo.recorrer(grupo2)
        println(resultado_calabozo)
        resultado_calabozo.isSuccess shouldBe(true)
      }

      "Si los integrantes del grupo mueren, el calabozo resulta fallido" in {
        val heroe1 = Heroe(Ladron, Estadistica(10, 10, 10, 20, 0), Loquito, Heroico)

        val grupo = Grupo()
        val grupo2 = grupo.sumarIntegrante(heroe1)
        val listaConHabitacion = List(Puerta(PuertaEscondida, Habitacion(HabitacionConDardos), true))
        val puerta = Puerta(PuertaEscondida, Habitacion(HabitacionConDardos, List(Puerta(PuertaEscondida, Habitacion(HabitacionConDardos, List(Puerta(PuertaEscondida, Habitacion(HabitacionConDardos, listaConHabitacion), false))), false))), false)
        val calabozo = Calabozo(Puerta(PuertaEscondida, Habitacion(HabitacionConDardos, List()), false))

        val resultado_calabozo = calabozo.recorrer(grupo2, puerta)
        println(resultado_calabozo)
        resultado_calabozo.isSuccess shouldBe(false)
      }

      "Si el grupo no puede abrir mas puertas, el calabozo resulta fallido" in {
        val heroe1 = Heroe(Ladron, Estadistica(10, 10, 3, 50, 0), Loquito, Heroico)

        val grupo = Grupo()
        val grupo2 = grupo.sumarIntegrante(heroe1)
        val listaConHabitacion = List(Puerta(PuertaCerrada, Habitacion(HabitacionConDardos), true))
        val puerta = Puerta(PuertaEscondida, Habitacion(HabitacionConDardos, List(Puerta(PuertaEscondida, Habitacion(HabitacionConDardos, List(Puerta(PuertaEscondida, Habitacion(HabitacionConDardos, listaConHabitacion), false))), false))), false)
        val calabozo = Calabozo(Puerta(PuertaEscondida, Habitacion(HabitacionConDardos, List()), false))

        val resultado_calabozo = calabozo.recorrer(grupo2, puerta)
        println(resultado_calabozo)
        resultado_calabozo.isSuccess shouldBe(false)
      }

      "El mejor grupo para un calabozo es aquel que devuelva mayor puntaje" in {
        val heroe1 = Heroe(Ladron, Estadistica(10, 10, 10, 50, 0), Loquito, Heroico)

        val grupo1 = Grupo().sumarIntegrante(heroe1)

        val listaConHabitacion = List(Puerta(PuertaEscondida, Habitacion(HabitacionConDardos), true))
        val puerta = Puerta(PuertaEscondida, Habitacion(HabitacionConDardos, List(Puerta(PuertaEscondida, Habitacion(HabitacionConDardos, List(Puerta(PuertaEscondida, Habitacion(HabitacionConDardos, listaConHabitacion))))))))
        val calabozo = Calabozo(puerta)

        val heroeGanador = Heroe(Ladron, Estadistica(10, 10, 40, 50, 0), Loquito, Heroico)//este es level mucho mas alto

        val grupo2 = Grupo().sumarIntegrante(heroeGanador)

        val puntaje_grupo1 = calabozo.recorrer(grupo1).get.puntaje()
        val puntaje_grupo2 = calabozo.recorrer(grupo2).get.puntaje()
        val grupoGanador = calabozo.elMejorGrupo(Set(grupo1, grupo2))

        println(s"El puntaje del grupo1 es: ${puntaje_grupo1}")
        println(s"El puntaje del grupo2 es: ${puntaje_grupo2}")

        println(s"El grupo ganador es: ${grupoGanador}")

        (puntaje_grupo2 > puntaje_grupo1) shouldBe(true)
      }

      "Si ningun grupo sobrevive al someterse par ver quien es el mejor grupo se lanza una excepcion" in {
        val heroe1 = Heroe(Ladron, Estadistica(10, 10, 2, 20, 0), Loquito, Heroico)

        val grupo1 = Grupo().sumarIntegrante(heroe1)

        val listaConHabitacion = List(Puerta(PuertaEscondida, Habitacion(HabitacionConDardos), true))
        val puerta = Puerta(PuertaEscondida, Habitacion(HabitacionConDardos, List(Puerta(PuertaEscondida, Habitacion(HabitacionConDardos, List(Puerta(PuertaEscondida, Habitacion(HabitacionConDardos, listaConHabitacion))))))))
        val calabozo = Calabozo(Puerta(PuertaEscondida, Habitacion(HabitacionConDardos, List().empty)))

        val heroeGanador = Heroe(Ladron, Estadistica(10, 10, 2, 20, 0), Loquito, Heroico)

        val grupo2 = Grupo().sumarIntegrante(heroeGanador)

        val thrown = intercept[MejorGrupoFallidoException] {
          calabozo.elMejorGrupo(Set(grupo1, grupo2))
        }
        assert(thrown.getMessage == "Ningun grupo sobrevivio al calabozo")

      }

      "un ladron con habilidad de manos nivel 0 necesitara subir 7 niveles para poder abrir una puerta Encantada el solo" in {
        val heroe1 = Heroe(Ladron, Estadistica(10, 10, 0, 50, 0), Loquito, Heroico)

        val grupo = Grupo()
        val grupo2 = grupo.sumarIntegrante(heroe1)
        val listaConHabitacion = List(Puerta(PuertaEscondida, Habitacion(HabitacionConDardos), true))
        val puerta = Puerta(PuertaEncantada(Hechizo("dislumbrar")), Habitacion(HabitacionConDardos, listaConHabitacion), false)
        val calabozo = Calabozo(puerta)

        val niveles_necesarios = calabozo.nivelesNecesariosParaCompletar(grupo2)
        println(niveles_necesarios)
        niveles_necesarios shouldBe 7
      }
    }
    "tp individual" - {
      "si un grupo tiene 1 integrante y se somete a una HabitacionConMotin no pasa nada" in {
        val heroe = Heroe(Ladron, Estadistica(10, 10, 10, 50, 0), Loquito, Heroico)

        val grupo = Grupo().sumarIntegrante(heroe)

        val puerta = Puerta(PuertaEscondida, Habitacion(HabitacionConMotin), true)

        grupo.intentarAbrirPuerta(puerta).get.integrantes.take(1).shouldBe(grupo.integrantes.take(1))


      }

      "si un grupo se somete a una HabitacionConMotin pelean contra su integrante mas fuerte y pierden, sufren un danio igual a la fuerza del heroe ganador divido la cantidad de integrantes" in {
        val heroe_mas_fuerte = Heroe(Ladron, Estadistica(50, 10, 10, 60, 0), Loquito, Vidente)
        val heroe2 = Heroe(Ladron, Estadistica(10, 10, 10, 100, 0), Loquito, Heroico)

        val grupo = Grupo().sumarIntegrante(heroe_mas_fuerte).sumarIntegrante(heroe2)

        val puerta = Puerta(PuertaEscondida, Habitacion(HabitacionConMotin), true)

        val resultado_grupo = grupo.intentarAbrirPuerta(puerta).get
        resultado_grupo.cantidadIntegrantes().shouldBe(1)
        resultado_grupo.integrantes.head.salud().shouldBe(50)

      }

      "si un grupo se somete a una HabitacionConMotin pelean contra su integrante mas fuerte y ganan, los integrantes suben un nivel" in {
        val heroe_mas_fuerte = Heroe(Ladron, Estadistica(50, 10, 10, 60, 0), Loquito, Vidente)
        val ladron = Heroe(Ladron, Estadistica(20, 10, 10, 100, 0), Loquito, Heroico)
        val guerrero = Heroe(Guerrero, Estadistica(30, 10, 10, 100, 0), Loquito, Heroico)

        val grupo = Grupo().sumarIntegrante(heroe_mas_fuerte).sumarIntegrante(ladron).sumarIntegrante(guerrero)

        val puerta = Puerta(PuertaEscondida, Habitacion(HabitacionConMotin), true)

        val resultado_grupo = grupo.intentarAbrirPuerta(puerta).get
        resultado_grupo.cantidadIntegrantes().shouldBe(2)
        resultado_grupo.integrantes.head.nivel().shouldBe(11)

      }
    }

  }

}
