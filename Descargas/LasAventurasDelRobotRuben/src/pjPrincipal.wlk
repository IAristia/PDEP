import wollok.game.*
import monstruos.*
import items.*
import inventario.*
import dealer.*
import tablero.*

class PjPrincipal{

    var property vida = 3
    var property ataque = 1
    var property defensa = 1
    
	var property vidaMax = 3
    var property monedas = 0
    var property puntuacion = 0
    
    var property cantidadDeMovimiento = 1
    var property moverse = true
    var property position = game.at(4,2)
    var property powerUpSeleccionado = powerUpNulo
    var property tienePowerUp = false
    var property powerUpQueTiene = powerUpNulo
    var image = "assets/rubens/ruben_abajo.png"
    
    var positionAnterior = position
	
    method image() = image
    method ataque() = ataque.min(5)
    method defensa() = defensa.min(5)
   	method subirVida(cantidad) = (vida+cantidad).min(vidaMax)
    method monedas() = monedas
	method poderTotal() = ataque + defensa + vida
	method podesMoverte() = moverse
	method generarPosicion(){
		self.position(game.at(4,2))
	}
	method moverseHacia(direccion){
    	if(self.podesMoverte()){
	    	positionAnterior = position
	        self.position(direccion.proximaPosicion(position))
	        image = "assets/rubens/ruben_" + direccion.nombre() + ".png"
	        self.cantidadDeMovimiento(1)		
    	}  
	}
	
	method moverseHaciaArriba(){
    	self.moverseHacia(arriba)
	}
	
	method moverseHaciaAbajo(){
    	self.moverseHacia(abajo)
	}
	
	method moverseHaciaIzquierda(){
    	self.moverseHacia(izquierda)
	}
	
	method moverseHaciaDerecha(){
    	self.moverseHacia(derecha)
	}
	
	method defendete(enemigo){
		var aux = defensa
        defensa = self.reduccionAlDefenderse(enemigo, self.defensa())
        self.puntuarPorDefensa(enemigo, aux)
	        
        aux= ataque
        ataque = self.reduccionAlDefenderse(enemigo, self.ataque())     
        self.puntuarPorDefensa(enemigo, aux)
	        
        aux = vida
		self.perderVida(enemigo.nivel())
        self.puntuarPorDefensa(enemigo, aux)
	}
	
	method reduccionAlDefenderse(enemigo, valor) = (valor - enemigo.nivel()).max(0)
	
	method puntuarPorDefensa(enemigo, valor) { // el método puntuación también actualiza el nivel del enemigo
        self.puntuacion(puntuacion + enemigo.nivel() * 10)    
		enemigo.nuevoNivel((enemigo.nivel() - valor).max(0))   
	}
	
	
	method agarrar(item){
		item.esChocado(self)
	}
	
	
	method boquear(){
	    game.say(self, "A casa bicho raro")		
	}
    
    method tieneVida() = self.vida() > 0 
    
    method morir(){
    	image = "assets/monstruos/cenizas.png"
    	self.moverse(false)
    	//pasar a pantalla de GAME OVER
    	fondoGameOver.ponerFondo()
    	const gameOver = game.sound("assets/sonidos/gameOver.mp3")
		gameOver.play()
    	game.schedule(3000, {=>game.stop()})

    }
    
    method morirSiNoTieneVida(){
    	if(!self.tieneVida()){
    		self.morir()
    	}
    }
    
    method perderVida(cantidad){
    	vida = (vida - cantidad).max(0)
    	self.morirSiNoTieneVida()
}
    
    method volverAPosicion(){
    	position = positionAnterior
    	game.removeVisual(self)
    	game.addVisual(self)
    }


    method seleccionarPowerUp(){
			self.auxiliar()
}
	
	method auxiliar(){
		
	    keyboard.num1().onPressDo({
			self.powerUpSeleccionado(dealer.obtenerPowerUpEnNPosicion(0))
			self.intentarComprar(powerUpSeleccionado)
	    })
	    keyboard.num2().onPressDo({
			self.powerUpSeleccionado(dealer.obtenerPowerUpEnNPosicion(1))
			self.intentarComprar(powerUpSeleccionado)
	    })
	    keyboard.num3().onPressDo({
			self.powerUpSeleccionado(dealer.obtenerPowerUpEnNPosicion(2))
			self.intentarComprar(powerUpSeleccionado)
	    })
	}
	
	method intentarComprar(powerUp) {
		if(self.puedeComprar(powerUp) and !self.podesMoverte() ) {
			self.comprar(powerUp)
		}else if(!self.podesMoverte()){
			game.say(dealer, "No te alcanza pa")
		}
	}
	
    method puedeComprar(powerUp_) = self.monedas() >= powerUp_.precio()
    method comprar(powerUp) {
		monedas -= powerUp.precio()
		powerUp.position(game.at(8,0))
		self.tienePowerUp(true)
		self.powerUpQueTiene(powerUp)
		dealer.removerVisuales(self)	    		
		game.addVisual(self.powerUpQueTiene())
	}
}
    
object arriba{
	
	method nombre() = "arriba"
	method proximaPosicion(position) {
		return position.up(ruben.cantidadDeMovimiento())
	}
}

object abajo{
	
	method nombre() = "abajo"
	method proximaPosicion(position) {
		return position.down(ruben.cantidadDeMovimiento())
	}
}

object derecha{
	
	method nombre() = "derecha"
	method proximaPosicion(position) {
		return position.right(ruben.cantidadDeMovimiento())
	}
}

object izquierda{
	
	method nombre() = "izquierda"
	method proximaPosicion(position) {
		return position.left(ruben.cantidadDeMovimiento())
	}
}
object fondoGameOver{
	const imagen = "assets/fondo/game_over.png"
	var property position = game.at(0,0)
	
	method image() = imagen
	method ponerFondo(){
		game.addVisual(self)
	}
}
object fondoYouWin{
	const imagen = "assets/fondo/youWin.png"
	var property position = game.at(0,0)
	
	method image() = imagen
	method ponerFondo(){
		game.addVisual(self)
	}
}

const ruben = new PjPrincipal()
