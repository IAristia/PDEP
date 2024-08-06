import wollok.game.*
import pjPrincipal.*
import items.*
import inventario.*
import tablero.*

class Monstruo {

    var property position
	var nivel
    var imagen
	var esMonstruoFinal = false
	
    /* Cambiar por property */
    method nivel() = nivel
    method nuevoNivel(nivel_){
        nivel = nivel_
    }
    method image() = imagen
    method boquear(){
        game.say(self, "Ruben, hasta aca llegaste.")
    }

    method morir()
    {
        imagen = "assets/monstruos/cenizas.png"
    }

    method esAtacado(atacante){

        if(atacante.tieneVida() and self.sobrevivio()){
        	atacante.defendete(self)
        	self.resultadoBatalla(atacante)	
        }
    }

    method sobrevivio() = self.nivel() > 0

    method resultadoBatalla(atacante){
    	if (self.esMonstruoFinal()){
	        if(self.sobrevivio()){
	            self.boquear()
	        }else if(atacante.tieneVida()){
	            atacante.boquear()
	            self.morir()
    			fondoYouWin.ponerFondo()
    			game.schedule(3000, {=>game.stop()})
	        }else{
	        	self.morir()
	        }    		
    	} else if (!self.esMonstruoFinal()){
    	
	        if(self.sobrevivio() and !self.esMonstruoFinal()){
	            self.boquear()
	        }else if(atacante.tieneVida()){
	            atacante.boquear()
	            self.morir()
	        }else{
	        	self.morir()
    		}
    	}
    }
	
	method esMonstruoFinal() = esMonstruoFinal
	
		
	method generarPosicion() {
		const columna = (0.randomUpTo(8)).roundUp()
    	position = game.at(columna,10)
    	if(posicionesImportantes.esPosicionImportante(columna,10)){
    		self.generarPosicion()
    	}
	}
	
	method esChocado(personaje){
		if (self.nivel()!=0){
			const fight = game.sound("assets/sonidos/fight.mp3")
			fight.play()
		}
		self.esAtacado(personaje)		
	
	}

}
