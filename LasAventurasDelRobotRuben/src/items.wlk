import wollok.game.*
import pjPrincipal.*
import monstruos.*
import inventario.*
import tablero.*
import dealer.*

class Items{
    
    var nivel
    var imagen
    var property position
    
    method image() = imagen
    method nivel() = nivel
    
    method esChocado(personaje) {
    	nivel = 0
        imagen = "assets/items/vacio.png" 
    }
}
class Daga inherits Items{

    override method esChocado(personaje){
    	personaje.ataque(personaje.ataque() + nivel)
		super(personaje)

    }
}

class Escudo inherits Items{

    override method esChocado(personaje){
    	personaje.defensa(personaje.defensa() + nivel)
		super(personaje)

    }
}


class Moneda inherits Items{


    override method esChocado(personaje){
        personaje.monedas(personaje.monedas() + nivel)
    	super(personaje)
    }
}

class Pocion inherits Items{

    override method esChocado(personaje){
    	//personaje.vida(personaje.vida() + nivel)
    	personaje.vida(personaje.subirVida(self.nivel()))
    	super(personaje)
	}
}

class Pinches inherits Items{

    override method esChocado(personaje){
        imagen = "assets/items/pinchosSangre.png"
        personaje.perderVida(self.nivel())
    }

}


// AMARILLO -  +1 de vida, +1 de ataque, +1 de defensa
object powerUp1 {
		
	const imagen = "assets/items/powerUp1.png" 
	const precio = 5
	var property position = game.at(6,15)
	
	method image() = imagen
	method precio() = precio

	method esUsado(personaje)
	{
		personaje.vida(personaje.vida() + 1)
		personaje.defensa(personaje.defensa() + 1)
		personaje.ataque(personaje.ataque() + 1)
		game.removeVisual(self)
		ruben.tienePowerUp(false)
		ruben.powerUpQueTiene(powerUpNulo)
		}
	}	


// VERDE - Aumenta la vida maxima en una unidad. Caso Base de 3 a 4 corazones
object powerUp2 {
		
	const imagen = "assets/items/powerUp2.png" 
	const precio = 12
	var property position = game.at(3,15)

	method image() = imagen
	method precio() = precio

	method esUsado(personaje)
	{
		personaje.vidaMax(personaje.vidaMax() + 1)
		game.removeVisual(self)
		ruben.tienePowerUp(false)
		ruben.powerUpQueTiene(powerUpNulo)
		}
	}	
	

// MATERIA - Todos los monstruos de nivel 2 pasan a ser de nivel 1, a partir del siguiente nivel
object powerUp3 {
		
	const imagen = "assets/items/powerUp3.png" 
	const precio = 15
	var property position = game.at(4,15)
	const nombre = "powerUp3"
	
	method image() = imagen
	method precio() = precio
	method nombre()	= nombre

	method esUsado(personaje)
	{
		 tablero.nivelMonstruo(1) 
		 game.removeVisual(self)
		 ruben.tienePowerUp(false)
		 ruben.powerUpQueTiene(powerUpNulo)
	}
}	

// AZUL - Saltar una casilla
object powerUp4 {
		
	const  imagen = "assets/items/powerUp4.png"
	const precio = 10 
	var property position = game.at(5,15)
	const nombre = "powerUp4"
	
	method image() = imagen
	method precio() = precio
	method nombre()	= nombre

	method esUsado(personaje)
	{
		personaje.cantidadDeMovimiento(2)
		game.removeVisual(self)
		ruben.tienePowerUp(false)
		ruben.powerUpQueTiene(powerUpNulo)
		}
	}	


object powerUpNulo {
		const precio = 0
		//var property position
		method esUsado(personaje) {
			ruben.tienePowerUp(false)
			ruben.powerUpQueTiene(self)
		}
		method precio() = precio
	}
