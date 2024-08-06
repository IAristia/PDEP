module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()

correrTests = hspec $ do

  describe "Test de ejemplo" $ do

    it "El pdepreludat se instaló correctamente" $ do

      doble 1 `shouldBe` 2


  describe "Punto 1" $ do 

    it "Prueba del cabezal" $ do

      (1,1) `shouldBe` inicializarCabezal (inicializarMatriz 2 2)


  describe "Punto 2" $ do 

    it "Inicializar Tablero" $ do

      UnTablero {celdas = [UnaCelda {coordenada = (1,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (1,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]}], cabezal = (1,1)}

      `shouldBe` inicializarTablero 2 2



    it "Inicializar Tablero" $ do

      UnTablero {celdas = [UnaCelda {coordenada = (1,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (1,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]}], cabezal = (1,1)} 

      `shouldNotBe` inicializarTablero 2 2


  describe "Punto 3a" $ do 

    it "Prueba de direcciones: Norte" $ do

      UnTablero {celdas = [UnaCelda {coordenada = (1,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (1,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]}], cabezal = (2,1)} 

      `shouldBe` mover norte (inicializarTablero 2 2) 


    it "Prueba de direcciones: Sur" $ do

      UnTablero {celdas = [UnaCelda {coordenada = (1,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (1,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]}], cabezal = (0,1)} 

      `shouldBe` mover sur (inicializarTablero 2 2)


    it "Prueba de direcciones: Este" $ do

      UnTablero {celdas = [UnaCelda {coordenada = (1,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (1,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]}], cabezal = (1,2)} 

      `shouldBe` mover este (inicializarTablero 2 2) 


    it "Prueba de direcciones: Oeste" $ do

       UnTablero {celdas = [UnaCelda {coordenada = (1,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

       UnaCelda {coordenada = (1,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

       UnaCelda {coordenada = (2,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

       UnaCelda {coordenada = (2,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]}], cabezal = (1,0)} 

       `shouldBe` mover oeste (inicializarTablero 2 2)


  describe "Punto 3b" $ do 

    it "Poner bolita" $ do

      UnTablero {celdas = [UnaCelda {coordenada = (1,1), bolitas = [("Rojo",1),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (1,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]}], cabezal = (1,1)} 

      `shouldBe` poner "Rojo" (inicializarTablero 2 2) 



  describe "Punto 3c" $ do 

    it "Saque bolita" $ do

      UnTablero {celdas = [UnaCelda {coordenada = (1,1), bolitas = [("Rojo",2),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (1,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]}], cabezal = (1,1)} 

     `shouldBe` sacar "Rojo" UnTablero {celdas = [UnaCelda {coordenada = (1,1), bolitas = [("Rojo",3),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (1,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]}], cabezal = (1,1)}


  describe "Punto 4" $ do 

    it "Funcion alternativa" $ do

        UnTablero {celdas = [UnaCelda {coordenada = (1,1), bolitas = [("Verde",1),("Rojo",0),("Negro",0),("Azul",0)]},

        UnaCelda {coordenada = (1,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

        UnaCelda {coordenada = (2,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

        UnaCelda {coordenada = (2,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]}], cabezal = (1,1)}
        
        `shouldBe` alternativa condicionEjemplo [sentenciaEjemplo] [sentenciaEjemplo2] tableroEjemplo 


  describe "Punto 4" $ do 

    it "La funcion SI" $ do

       UnTablero {celdas = [UnaCelda {coordenada = (1,1), bolitas = [("Verde",1),("Rojo",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (1,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]}], cabezal = (1,1)} 
      
      `shouldBe` si condicionEjemplo [sentenciaEjemplo] tableroEjemplo

    it "La funcion SI NO" $ do

      UnTablero {celdas = [UnaCelda {coordenada = (1,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (1,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]}], cabezal = (1,1)} `shouldBe` sino condicionEjemplo [sentenciaEjemplo] tableroEjemplo


  describe "Punto 4.b" $ do 

    it "Funcion Repetir" $ do

      UnTablero {celdas = [UnaCelda {coordenada = (1,1), bolitas = [("Verde",3),("Rojo",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (1,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]}], cabezal = (1,1)}

      `shouldBe` repetir 3 [sentenciaEjemplo] tableroEjemplo


  describe "Punto 4.c" $ do 

    it "Funcion mientras" $ do

      UnTablero {celdas = [UnaCelda {coordenada = (1,1), bolitas = [("Verde",4),("Rojo",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (1,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]}], cabezal = (1,1)} 

      `shouldBe` mientras condicionEjemplo [sentenciaEjemplo] tableroEjemplo


  describe "Punto 4.d" $ do 

    it "Funcion ir al borde" $ do

      UnTablero {celdas = [UnaCelda {coordenada = (1,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (1,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]}], cabezal = (2,1)}

      `shouldBe` irAlBorde norte tableroEjemplo


  describe "Punto 5a" $ do 

    it "Puede moverse al Norte" $ do

       True `shouldBe` puedeMoverse norte (inicializarTablero 2 2) 



  describe "Punto 5b" $ do 

    it "Si hay bolita roja en la celda" $ do

      True `shouldBe` hayBolita "Rojo" UnTablero {celdas = [UnaCelda {coordenada = (1,1), bolitas = [("Rojo",1),("Verde",1),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (1,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]}], cabezal = (1,1)} 



  describe "Punto 5c" $ do 

    it "La cantidad de bolitas " $ do

      cantidadBolitas "Rojo" (UnTablero {celdas = [UnaCelda {coordenada = (1,1), bolitas = [("Rojo",9),("Verde",0),("Negro",0),("Azul",0)]},
      
      UnaCelda {coordenada = (1,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]}], cabezal = (1,1)})

     `shouldBe` 9 


  describe "Punto 6" $ do 

    it "Funcion programa" $ do

      UnTablero {celdas = [UnaCelda {coordenada = (1,1), bolitas = [("Verde",1),("Rojo",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (1,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]}], cabezal = (1,1)}

      `shouldBe` programa tableroEjemplo [sentenciaEjemplo]

  
  describe "Punto 7" $ do 

    it "Programa Gobstone" $ do

      UnTablero {celdas = [UnaCelda {coordenada = (1,1), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (1,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (1,3), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (2,1), bolitas = [("Azul",1),("Negro",2),("Rojo",0),("Verde",0)]},

      UnaCelda {coordenada = (2,2), bolitas = [("Azul",1),("Rojo",0),("Verde",0),("Negro",0)]},

      UnaCelda {coordenada = (2,3), bolitas = [("Azul",1),("Verde",10),("Rojo",0),("Negro",0)]},

      UnaCelda {coordenada = (3,1), bolitas = [("Azul",15),("Rojo",15),("Verde",0),("Negro",0)]},

      UnaCelda {coordenada = (3,2), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]},

      UnaCelda {coordenada = (3,3), bolitas = [("Rojo",0),("Verde",0),("Negro",0),("Azul",0)]}], cabezal = (2,3)} 

      `shouldBe` gobstones tableroGobstones
