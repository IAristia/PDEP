import wollok.game.*
import pjPrincipal.*
import items.*
import tablero.*
import inventario.*

object dealer {
	
	var columna
	var fila
	var objetoSeleccionado
	var property position
	var property desaparecer = false //Desaparece una vez que interactue con el pjPrincipal y aparece al generar nuevo lvl
	var catalogo
	var posicionesPP
	var stock = []
	const imagen = "assets/items/pelaAbajo.png" 
	
	method image() = imagen	
	method columna() = columna
	method fila() = fila
	
	method generarPosicion() {
			
		columna = (0.randomUpTo(8)).roundUp()
		fila = (2.randomUpTo(10)).roundUp()
		if(posicionesImportantes.esPosicionImportante(columna,fila)) {
			self.generarPosicion()
		}
    	position = game.at(columna,fila)
	}

	method esChocado(personaje){
		if(ruben.tienePowerUp()){
			game.say(self, "Ya tenés \n un Power Up")
			game.schedule(2500, {=>game.removeVisual(self)})
			self.desaparecer(true)
		}else{
			 self.mostrarOfertas(personaje)
		}

	}
	method mostrarOfertas(personaje){
	
		keyboard.del().onPressDo({self.removerVisuales(personaje)})

		fondoPowerUp.ponerFondo()
		
		self.reponePowerUps()
		self.cargarPosicionesPP()
		stock = []
		
		personaje.moverse(false)
		
		self.mostrarPowerUp()
		self.mostrarPowerUp()
		self.mostrarPowerUp()
		
		self.agregarVisualesTienda()
		
		self.actualizarDealer()
		
		personaje.seleccionarPowerUp()
}
	
	method agregarVisualesTienda(){
		game.addVisual(texto_1)
		game.addVisual(texto_2)
		game.addVisual(texto_3)
		game.addVisual(precio_1)
		game.addVisual(precio_2)
		game.addVisual(precio_3)
		game.addVisual(text_precio)
	}
	
	method removerVisualesTienda(){
		game.removeVisual(texto_1)
		game.removeVisual(texto_2)
		game.removeVisual(texto_3)
		game.removeVisual(precio_1)
		game.removeVisual(precio_2)
		game.removeVisual(precio_3)
		game.removeVisual(text_precio)
	}	
	method reponePowerUps(){
		catalogo = [powerUp1,powerUp2,powerUp3,powerUp4]
	}
	method cargarPosicionesPP(){
		posicionesPP = [game.at(3,6),game.at(4,6),game.at(5,6)]
	}
	method mostrarPowerUp(){
		objetoSeleccionado = catalogo.anyOne()
		objetoSeleccionado.position(posicionesPP.first())
		stock.add(objetoSeleccionado)
		game.addVisual(objetoSeleccionado)
		posicionesPP = posicionesPP.copyWithout(posicionesPP.first())
		catalogo = catalogo.copyWithout(objetoSeleccionado)
	}
	
	method obtenerPowerUpEnNPosicion(posicion) = stock.get(posicion) 
	method actualizarDealer(){
		game.removeVisual(self)
		self.position(game.at(4,9))
		game.addVisual(self)
		game.say(self, "Que desea \n comprar?")
	}

	method removerVisuales(personaje){
		if(!personaje.podesMoverte()){
			game.removeVisual(fondoPowerUp)
			self.desaparecer(true)
			game.removeVisual(self)
			stock.forEach{powerUp_ => self.removerVisual(powerUp_)}
			personaje.moverse(true)
			self.removerVisualesTienda()
		}
	}
	method removerVisual(powerUp_){
		game.removeVisual(powerUp_)
	}
}

object fondoPowerUp{
	const imagen = "assets/fondo/fondoCatalogo.png"
	var property position = game.at(0,2)
	
	method image()= imagen
	method ponerFondo(){
		game.addVisual(self)
	}
}