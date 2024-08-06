import wollok.game.*
import monstruos.*
import items.*
import pjPrincipal.*
import inventario.*
import dealer.*

object portal {

    const imagen = "assets/items/portal.png"
    
    var columna
    const fila = 10
    var property position

    method image() = imagen
	method columna() = columna
	method fila() = fila
	
	method generarPosicion() {
		columna = (0.randomUpTo(8)).roundUp()
    	position = game.at(columna,fila)
    	if(posicionesImportantes.esPosicionImportante(columna,fila)){
    		self.generarPosicion()
    	}
	}
	
	method generarNivel() {
		tablero.limpiarse()
		tablero.generarObjetosTablero()
	}

	method esChocado(personaje){
		tablero.pasarNivel()
		self.generarNivel()
	}
}

class Pared {
	var property position
	
	method esChocado(personaje){
		
		personaje.volverAPosicion()
		game.say(personaje, "Reboté \n :-(")
		const boing = game.sound("assets/sonidos/boing.mp3")
		boing.play()
	}
}

object tablero {
	
	// Actualizar el nivel del monstruo - VER
	var nivel = 1
	
	var contador = ruben.vida() - 1 + ruben.ataque() + ruben.defensa()
	
	var nivelMonstruo = 2
	
	const objetosEnTablero = []
	
	method pasarNivel(){
		nivel +=1
	}
	
	method nivelMonstruo(nuevoNivel) {nivelMonstruo = nuevoNivel} 

	method generarObjetosTablero(){
		
		if (nivel == 5) {

			portal.position(game.at(null,null))
			posicionesImportantes.agregrarPosicion(portal.position())			
	 		game.removeVisual(portal)

			const monstruoFinal = new Monstruo(nivel = 10,position = game.at(0,10), imagen = "assets/monstruos/finalBoss.png", esMonstruoFinal = true)
			monstruoFinal.generarPosicion()
			game.addVisual(monstruoFinal)	 
			posicionesImportantes.agregrarPosicion(monstruoFinal.position())
			
			ruben.generarPosicion()
			posicionesImportantes.agregrarPosicion(ruben.position())	

			dealer.generarPosicion()					
			posicionesImportantes.agregrarPosicion(dealer.position())	

		
			new Range(start = 2, end = game.width()+1).forEach{
				fila => new Range(start = 0, end = game.height()).forEach{
					columna => self.generarObjeto(columna,fila)
					}
				}			
			
		}else{
			ruben.generarPosicion()
			posicionesImportantes.agregrarPosicion(ruben.position())	
						
			dealer.generarPosicion()
			posicionesImportantes.agregrarPosicion(dealer.position())	
			
			portal.generarPosicion()
			posicionesImportantes.agregrarPosicion(portal.position())	

			new Range(start = 2, end = game.width()+1).forEach{
				fila => new Range(start = 0, end = game.height()).forEach{
					columna => self.generarObjeto(columna,fila)
					}
				}			
		}
}

	method generarObjeto(columna,fila){
		   
		if(posicionesImportantes.esPosicionImportante(columna,fila)){
			// Se supone que este vacio para que no se generen objetos en las posiciones de esos objetos
		}else{
			
			if (contador <= 0) {
				self.generarObjetoMalo(columna, fila)
				contador =+ 1
			}
			
		else if (contador > 0){
				self.generarObjetoBueno(columna, fila)
				contador =- 1 
			}
		
else if (contador == 0){
			const numero = (0.randomUpTo(2)).roundUp()
			
				if(numero == 1){
				self.generarObjetoBueno(columna,fila)
				contador =+ 1
				}
				if(numero == 2){
				self.generarObjetoBueno(columna,fila)
				contador =- 1	
				
			}
		} 		
	} 		
}

	method generarObjetoMalo(columna, fila) {
		
		const numero = (0.randomUpTo(4)).roundUp()
		
				if(numero == 1){
				const monstruo1 = new Monstruo(nivel = 1,position = game.at(columna,fila), imagen = "assets/monstruos/monstruo1.png")
				game.addVisual(monstruo1)
				objetosEnTablero.add(monstruo1)			
				}
				if(numero == 2){
					const monstruo2 = new Monstruo(nivel = nivelMonstruo,position = game.at(columna,fila), imagen = "assets/monstruos/monstruo2.png")
					game.addVisual(monstruo2)
					objetosEnTablero.add(monstruo2)				
				}
				if(numero == 3){
					const monstruo3 = new Monstruo(nivel = 3,position = game.at(columna,fila), imagen = "assets/monstruos/mago.png")
					game.addVisual(monstruo3)
					objetosEnTablero.add(monstruo3)					
				}
				if(numero == 4){
				const pinches1 = new Pinches(nivel = 1, imagen = "assets/items/pinchos1.png", position = game.at(columna,fila))
				game.addVisual(pinches1)
				objetosEnTablero.add(pinches1)			
				}
	}
	
	method generarObjetoBueno(columna, fila) {
		
		const numero = (0.randomUpTo(9)).roundUp()
	
				if(numero == 1){
					const vida1 = new Pocion(nivel = 1, imagen = "assets/items/pocion1.png", position = game.at(columna,fila))
					game.addVisual(vida1)
					objetosEnTablero.add(vida1)					
				}
				if(numero == 2){
					const vida2 = new Pocion(nivel = 2, imagen = "assets/items/pocion2.png", position = game.at(columna,fila))
					game.addVisual(vida2)
					objetosEnTablero.add(vida2)				
				}
				if(numero == 3){
					const daga1 = new Daga(nivel = 1, imagen = "assets/items/espada1.png", position = game.at(columna,fila))
					game.addVisual(daga1)
					objetosEnTablero.add(daga1)				
				}
				if(numero == 4){
					const daga2 = new Daga(nivel = 2, imagen = "assets/items/espada2.png", position = game.at(columna,fila))
					game.addVisual(daga2)
					objetosEnTablero.add(daga2)				
				}
				if(numero == 5){
					const daga3 = new Daga(nivel = 3, imagen = "assets/items/espada3.png", position = game.at(columna,fila))
					game.addVisual(daga3)
					objetosEnTablero.add(daga3)			
				}
				if(numero == 6){
					const escudo1 = new Escudo(nivel = 1, imagen = "assets/items/escudo1.png", position = game.at(columna,fila))
					game.addVisual(escudo1)
					objetosEnTablero.add(escudo1)			
				}
				if(numero == 7){
					const escudo2 = new Escudo(nivel = 2, imagen = "assets/items/escudo2.png", position = game.at(columna,fila))
					game.addVisual(escudo2)			
					objetosEnTablero.add(escudo2)
				}
				if(numero == 8){
					const escudo3 = new Escudo(nivel = 3, imagen = "assets/items/escudo3.png", position = game.at(columna,fila))
					game.addVisual(escudo3)
					objetosEnTablero.add(escudo3)			
				}
				if(numero == 9){
					const moneda1 = new Moneda(nivel = 1, imagen = "assets/items/moneda1.png", position = game.at(columna,fila))
					game.addVisual(moneda1)
					objetosEnTablero.add(moneda1)			
				}
	}
	
	method generarParedes() {
			
		new Range(start = 0, end = game.width()).forEach{
			columna => new Range(start = 0, end = 1).forEach{
				fila => self.generarPared(columna,fila)
			}
		}
		
		new Range(start = 0, end = game.width()).forEach{
			columna => new Range(start = 11, end = 12).forEach{
				fila => self.generarPared(columna,fila)
			}
		}

		new Range(start = -2, end = -1).forEach{
			columna => new Range(start = 2, end = 10).forEach{
				fila => self.generarPared(columna,fila)
			}
		}

		new Range(start = 9, end = 10).forEach{
			columna => new Range(start = 2, end = 10).forEach{
				fila => self.generarPared(columna,fila)
			}
		}			
}
	method generarPared(columna,fila){
		
		const pared = new Pared(position = game.at(columna, fila))
		game.addVisual(pared)
		
	}
		
	method removerVisual(objeto){
		game.removeVisual(objeto)
	}

// REVISAR
	method limpiarse() {
		
		objetosEnTablero.asSet().asList()
		
		objetosEnTablero.forEach{objeto_ => self.removerVisual(objeto_)}
		objetosEnTablero.clear()
		posicionesImportantes.borrarPosiciones()
		ruben.position(game.at(4,2))
		if(dealer.desaparecer()){			
			game.addVisual(dealer)
			dealer.desaparecer(false)		
		}
	}
					
	method cargarVisualesInicio(){
	    game.boardGround("assets/fondo/fondo1.jpg") 
	    game.addVisual(texto_monedas)
	    game.addVisual(texto_vidas)
	    game.addVisual(texto_ataque)
	    game.addVisual(texto_defensa)
	    game.addVisual(texto_puntuacion)	    
	}
	
	method cargarVisualesObjetosImportantes(){
	    game.addVisual(dealer)
	    game.addVisual(portal)
	    game.addVisual(ruben)
	}
}
object posicionesImportantes{
	var posicionesImportantes = []
	
	method esPosicionImportante(columna,fila) = posicionesImportantes.contains(game.at(columna,fila))

	method agregrarPosicion(posicion) = posicionesImportantes.add(posicion)
	
	method borrarPosiciones() = posicionesImportantes.clear()

}			