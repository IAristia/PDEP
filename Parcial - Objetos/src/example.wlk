class Plato { // clase abstracta
	method valoracion()
	method precio() {
		if (self.esAptoCeliaco()){
		return self.valoracion() * 300 + 1200
		}else return self.valoracion() * 300
	}
	method esAptoCeliaco()
	method esEspecial()
}


class Provoleta inherits Plato {
	var peso
	var empanado
	
	method peso() = peso
	
	method tieneEmpanado() = empanado
	
	override method esAptoCeliaco() = !self.tieneEmpanado()

	override method esEspecial() = self.peso() > 250 && self.tieneEmpanado()
	
	override method valoracion() {
		if (self.esEspecial())
		return 120
		else return 80
	}
}

class HamburguesaDeCarne inherits Plato {
	
	var pesoMedallon 
	const pan
	
	method pesoMedallon() = pesoMedallon
	
	method peso() = self.pesoMedallon() + pan.peso()
		
	override method esAptoCeliaco() = pan.esAptoCeliaco()
	
	override method valoracion() = self.peso() / 10
	
	override method esEspecial() = self.peso() > 250

}

class Pan {
	const peso
	const esAptoCeliaco
	method peso() = peso
	method esAptoCeliaco() = esAptoCeliaco
}

const panIndustrial = new Pan(peso = 60, esAptoCeliaco = false)	

const panCasero = new Pan(peso = 100, esAptoCeliaco = false)	

const panMaiz = new Pan(peso = 30, esAptoCeliaco = true)


class HamburguesaDoble inherits HamburguesaDeCarne {
	
	override method peso() = 2 * pesoMedallon + pan.peso()
	
	override method esEspecial() = self.peso() > 500
	
	override method valoracion() = self.peso() / 10 // comparte código con su clase superior pero lo sobreescribo en caso de que en un futuro cambie el método de valoración de las hamburguesas dobles

}

class CorteCarne inherits Plato {
	var peso
	var coccion
	override method valoracion() = 100
	override method esAptoCeliaco() = true
	method peso() = peso
	method coccion() = coccion
	method estaAPunto() = self.coccion() == apunto
	override method esEspecial() = self.peso() > 250 && self.estaAPunto()	
}

object jugoso{}

object apunto{}

object cocido{}

class Parrillada inherits Plato {
	var platos = []

	method peso() = platos.map({plato => plato.peso()}).sum()

	override method esEspecial() = self.peso() > 250 && platos.size() >= 3
	
	override method esAptoCeliaco() = platos.all({plato => plato.esAptoCeliaco()})
	
	override method valoracion() = platos.map({plato => plato.valoracion()}).max()
}

class Comensal {
	var property yaCompro = false
	var property dinero
	var property tipoDeComensal // composición para cumplir con el punto 3, herencia no sirve acá
	var comidasOrdenadasPorPrecio = (self.tipoDeComensal().comidasQueLeAgradan().filter({comida =>self.puedeComprar(comida)}).sortBy({plato1, plato2 => plato1.precio() > plato2.precio()}))
	
	method comidasOrdenadasPorPrecio() = comidasOrdenadasPorPrecio
	
	method ganarDinero(cantidad) {self.dinero(self.dinero()+cantidad)}	
		 
	method darseUnGusto(){
		var comidaMasCara = self.comidasOrdenadasPorPrecio().get(0)
		self.intentarComprarComida(comidaMasCara)
	}
	
	method intentarComprarComida(comida) {
		if (self.comidasOrdenadasPorPrecio().size() > 0){
			
			if (comida.precio() <= self.dinero()){
				self.comprar(comida)
			}
			else
			{
				self.comidasOrdenadasPorPrecio().drop(0)
				self.intentarComprarComida(comida)
			}	
		} else throw new Exception (message = "No hay ningún plato que este comensal pueda comprar")
	}
		
	method comprar(comida) {
		self.dinero(self.dinero() - comida.precio())
		parrilla.vender(comida)
		self.yaCompro(true)
	}
	
	method limitadoEconomicamente() {self.tipoDeComensal(todoTerreno)}	
	
	method descubrioQueTieneCeliaquia() {self.tipoDeComensal(celiaco)}

	method viajoAParis(){self.tipoDeComensal(paladarFino)}
	
	method puedeComprar(comida) = comida.precio() <= self.dinero()
}


// los modelo como objetos para cumplir con el punto 3

object celiaco {
	var comidasQueLeAgradan = parrilla.platos().filter({plato => plato.esAptoCeliaco()})
	method comidasQueLeAgradan() = comidasQueLeAgradan
}

object paladarFino {
	var comidasQueLeAgradan = parrilla.platos().filter({plato => plato.esEspecial() || plato.valoracion() > 100})
	method comidasQueLeAgradan() = comidasQueLeAgradan
}

object todoTerreno {
	var comidasQueLeAgradan = parrilla.platos()
	method comidasQueLeAgradan() = comidasQueLeAgradan
}

object parrilla {
	var ingresos = 0 
	var property platos = []
	var comensales = []
	method vender(comida) {ingresos = ingresos + comida.precio()}
	method promocion(cantidad){
		comensales.filter({comensal => comensal.yaCompro()}).forEach({comensal => comensal.ganarDinero(cantidad)})
	}
	
	method descubrieronQueTieneCeliaquia(personas) {personas.forEach{persona=>persona.descubrioQueTieneCeliaquia()}}
	
	method limitadosEconomicamente(personas) {personas.forEach{persona=>persona.limitadoEconomicamente()}}
	
	method viajaronAParis(personas){personas.forEach{persona=>persona.viajoAParis()}}
}

// Punto 3 - cambios de hábitos

// Un comensal viajó a Paris y se volvió extremadamente delicado a la hora de comer en el restaurante, pasa a ser de paladar fino.
