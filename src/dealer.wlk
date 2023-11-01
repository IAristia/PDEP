import wollok.game.*
import pjPrincipal.*
import items.*
import tablero.*

object dealer {
	
	// Para que se genere exactamente un Power Up de cada tipo - Ver una forma mejor
	var cantPP1 = 0
	var cantPP2 = 0
	var cantPP3 = 0
	var cantPP4 = 0
	var columna
	var fila
	const catalogo = []
	const imagen = "assets/items/pelaAbajo.png" 
	var property position
	
	method image() = imagen
	
	method columna() = columna
	method fila() = fila
	
	method generarPosicion() {
		columna = (0.randomUpTo(8)).roundUp()
		fila = (2.randomUpTo(10)).roundUp()
    	position = game.at(columna,fila)
	}
	
	method mostrarPowerUps(){
		const numero = (0.randomUpTo(4)).roundUp()
		if(numero == 1 and cantPP1 == 0){
			//Crear Power Up
			const powerUp1 = new PowerUp1(imagen = "assets/items/powerUp1.png", precio = 10, position = game.at(3,5))
			game.addVisual(powerUp1)
			catalogo.add(powerUp1)
			cantPP1++	
		}else if(numero == 1 and cantPP1 > 0){
			// Volver a intentar generar un power up random
			self.mostrarPowerUps()
		}
		if(numero == 2 and cantPP2 == 0){
			const powerUp2 = new PowerUp2(imagen = "assets/items/powerUp2.png", precio = 10, position = game.at(5,5))
			game.addVisual(powerUp2)
			catalogo.add(powerUp2)
			cantPP2++		
		}else if(numero == 2 and cantPP2 > 0){
			self.mostrarPowerUps()
		}
		if(numero == 3 and cantPP3 == 0){
			const powerUp3 = new PowerUp3(imagen = "assets/items/powerUp3.png", precio = 10, position = game.at(3,7))
			game.addVisual(powerUp3)
			catalogo.add(powerUp3)	
			cantPP3++		
		}else if(numero == 3 and cantPP3 > 0){
			self.mostrarPowerUps()
		}
		if(numero == 4 and cantPP4 == 0){
			const powerUp4 = new PowerUp4(imagen = "assets/items/powerUp4.png", precio = 10, position = game.at(5,7))
			game.addVisual(powerUp4)
			catalogo.add(powerUp4)	
			cantPP4++		
		}else if(numero == 4 and cantPP4 > 0){
			self.mostrarPowerUps()
		}
	}
	
	method actualizarDealer(){
		game.removeVisual(self)
		self.position(game.at(4,9))
		game.addVisual(self)
		game.say(self, "Que desea \n comprar?")
	}
	
	method realizarIntercambio(personaje,powerUp){
		if(personaje.puedeComprar(powerUp)){		
			personaje.comprar(powerUp)
		}
	}
	method mostrarOfertas(personaje){
		/*
		 * 2. Mostrar todos los Power Ups con sus precios
		 * 2.2 Mostrar Precios + ¿Descripcion?
		 * 3. Realizar Intercambio ()
		 * 3.1 Tocar Tecla para elegir que power Up queremos
		 * 3.2 Restarnos la cantidad de plata que sale el powerUp
		 * 3.3 Añadir powerUp al inventario
		 * 4. Sacar fondoCatalogo
		 * 5. ¿Sacar al dealer?
		 * 6. Hacer que ruben se vuelva a mover 
		 */
		simulacroFondo.ponerFondo()
		personaje.moverse(false)
		self.mostrarPowerUps()
		self.mostrarPowerUps()
		self.mostrarPowerUps()
		self.actualizarDealer()
		keyboard.del().onPressDo({self.removerVisuales(personaje)})
//		keyboard.del().onPressDo({self.removerVisuales()})
//		keyboard.del().onPressDo({personaje.moverse(true)})
//		keyboard.c().onPressDo({self.removerVisuales()}, {personaje.moverse(true)})
		const powerUp = personaje.seleccionaPowerUp()
		self.realizarIntercambio(personaje, powerUp)
		
	}

	method reiniciarCatalogo() {
		cantPP1 = 0
		cantPP2 = 0
		cantPP3 = 0
		cantPP4 = 0
		catalogo.clear()
	}

	method removerVisuales(personaje){
		game.removeVisual(simulacroFondo)
		game.removeVisual(self)
		catalogo.forEach{powerUp_ => tablero.removerVisual(powerUp_)}
		personaje.moverse(true)
		self.reiniciarCatalogo()
	}
	method esChocado(personaje){
		self.mostrarOfertas(personaje)
	}
	
}

object simulacroFondo{
	const imagen = "assets/fondo/fondoCatalogo.png"
	var property position = game.at(0,2)
	
	method image() = imagen
	method ponerFondo(){
		game.addVisual(self)
	}
}