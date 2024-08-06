import wollok.game.*
import monstruos.*
import items.*
import pjPrincipal.*
import dealer.*

object texto_vidas {

    method position() = game.at(0,0)

    method text() = "Vidas: " + ruben.vida()
    
    method esChocado(personaje){}
}

object texto_ataque {

    method position() = game.at(2,0)

    method text() = "Ataque: " + ruben.ataque()
    
    method esChocado(personaje){}
}

object texto_defensa {

    method position() = game.at(4,0)

    method text() = "Defensa: " + ruben.defensa()
    
    method esChocado(personaje){}
}


object texto_monedas {

    method position() = game.at(6,0)

    method text() = "Monedas: " + ruben.monedas()
    
    method esChocado(personaje){}
}

object texto_puntuacion {

    method position() = game.at(0,1)

    method text() = "Score: " + ruben.puntuacion()
    
    method esChocado(personaje){}
}

object texto_1 {

    method position() = game.at(3,5)

    method text() = "1"
    
    method esChocado(personaje){}
}

object texto_2 {

    method position() = game.at(4,5)

    method text() = "2"
    
    method esChocado(personaje){}
}

object texto_3 {

    method position() = game.at(5,5)

    method text() = "3"
    
    method esChocado(personaje){}
}

object precio_1 {

    method position() = game.at(3,7)
	
	method obtenerPowerUp() = dealer.obtenerPowerUpEnNPosicion(0)
	method obtenerPrecio() = self.obtenerPowerUp().precio()
    method text() = self.obtenerPrecio().toString()
        
    method esChocado(personaje){}
}

object precio_2 {

    method position() = game.at(4,7)
	
	method obtenerPowerUp() = dealer.obtenerPowerUpEnNPosicion(1)
	method obtenerPrecio() = self.obtenerPowerUp().precio()
    method text() = self.obtenerPrecio().toString()
        
    method esChocado(personaje){}
}

object precio_3 {

    method position() = game.at(5,7)
	
	method obtenerPowerUp() = dealer.obtenerPowerUpEnNPosicion(2)
	method obtenerPrecio() = self.obtenerPowerUp().precio()
    method text() = self.obtenerPrecio().toString()
        
    method esChocado(personaje){}
}

object text_precio {

    method position() = game.at(4,8)

    method text() = "Precios :)"
    method esChocado(personaje){}
}