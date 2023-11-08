import wollok.game.*
import pjPrincipal.*
import items.*
import inventario.*

class Monstruo {

    var property position
	var nivel
    var imagen

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
    	
        if(self.sobrevivio()){
            self.boquear()
            atacante.morir()
        }else if(atacante.tieneVida()){
            atacante.boquear()
            self.morir()
        }else{
        	atacante.morir()
        	self.morir()
        }
    }

	method esChocado(personaje){
		self.esAtacado(personaje)
	}

}
