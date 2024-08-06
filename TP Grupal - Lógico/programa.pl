% https://docs.google.com/document/d/1zCWrMU9x8CQ_Vf0hkx5yw13lsGdU_MBMJQVmAuEW0oI

% Primera parte

elecciones(2017).
elecciones(2019).

estudiantes(sistemas, 2019, juanPerez).
estudiantes(sistemas, 2019, unter).
estudiantes(sistemas, 2019, mathy).
estudiantes(sistemas, 2017, unter).
estudiantes(sistemas, 2017, mathy).


votos(franjaNaranja, 213, sistemas, 2017).
votos(partidoPDEP, 213123123, sistemas, 2017).
votos(franjaNaranja, 21323, sistemas, 2019).
votos(agosto29, 21323, sistemas, 2019).
votos(partidoPDEP, 21231231, sistemas, 2019).


% Punto 1

quienGano(PartidoGanador, Anio):-
    elecciones(Anio),
    findall(VotosPartido, votos(_, VotosPartido,_,Anio), VotosAnio),
    max_list(VotosAnio,CantidadVotosMayor),
    votos(PartidoGanador, CantidadVotosMayor,_,Anio).

% Punto 2

ganaElMismo(Partido):-
    votos(Partido, _, _, _),
    forall(elecciones(Anio), quienGano(Partido, Anio)).


% Punto 3

%huboFraude(Anio).
huboFraude(Anio):-
    elecciones(Anio),
    findall(VotosPorPartido, votos(_,VotosPorPartido,_,Anio), ListaVotosAnio),
    sumlist(ListaVotosAnio, CantidadVotosAnio),

    estudiantes(_,Anio,_),
    findall(RegistradosPorAnio, estudiantes(_,Anio,RegistradosPorAnio), ListaRegistradosAnio),
    length(ListaRegistradosAnio, CantidadRegistradosAnio),

    CantidadVotosAnio > CantidadRegistradosAnio.


% Punto 4. Se puede hacer la consulta con el predicado del punto 3.

% Segunda parte

realizoAccion(franjaNaranja, lucha(salarioDocente)).
realizoAccion(franjaNaranja, gestionIndividual("Excepción de correlativas", juanPerez, 2019)).
realizoAccion(franjaNaranja, obra(2019)).
realizoAccion(agosto29, lucha(salarioDocente)).
realizoAccion(agosto29, lucha(boletoEstudiantil)).
realizoAccion(partidoPDEP, obra(2018)).
realizoAccion(partidoPDEP, gestionIndividual("Cambio de curso", unter, 2017)).
realizoAccion(partidoUltraDemagogico, gestionIndividual("Cambio de curso", unter,2017)).
/*
esDemagogica(Partido):-
    realizoAccion(Partido, _),
    not(esAccion(Partido, obra)),
    not(esAccion(Partido, lucha)).
*/
/*
esBurocrata(Partido):-
    realizoAccion(Partido, _),
    not(esAccion(Partido, lucha)).
*/

esDemagogica(Partido):-
    realizoAccion(Partido,_),
    forall(realizoAccion(Partido,_), not(esAccion(Partido, obra))),
    forall(realizoAccion(Partido,_), not(esAccion(Partido, lucha))).


esBurocrata(Partido):-
    realizoAccion(Partido,_),
    forall(realizoAccion(Partido,_), not(esAccion(Partido,lucha))).


esTransparente(Partido):-
    realizoAccion(Partido, _),
    forall(realizoAccion(Partido, Accion),esGenuina(Accion)).
     

esAccion(Partido, gestionIndividual):-
    realizoAccion(Partido, gestionIndividual(_,_,_)).

esAccion(Partido, obra):-
    realizoAccion(Partido, obra(_)).

esAccion(Partido, lucha):-
    realizoAccion(Partido, lucha(_)).

esGenuina(obra(Anio)):-
    not(elecciones(Anio)).
    
esGenuina(gestionIndividual(_,Nombre,Anio)):-
    estudiantes(_,Anio,Nombre).
    
esGenuina(lucha(_)).
    
caracteristicasSimultaneas(Partido):-
    realizoAccion(Partido,_),
    findall(Caracteristica,caracteristicaDelPartido(Partido,Caracteristica),ListaDeCaracteristicas),
    list_to_set(ListaDeCaracteristicas, ListaDeCaracteristicasReducida),
    length(ListaDeCaracteristicasReducida, CantidadQueCumple),
    CantidadQueCumple >= 2.

caracteristicaDelPartido(Partido,demagogica):-
    esDemagogica(Partido).

caracteristicaDelPartido(Partido,burocrata):-
    esBurocrata(Partido).

caracteristicaDelPartido(Partido,transparente):-
    esTransparente(Partido).

% Tercera Parte

/*
    - Predicado Inversible: caracteristicasSimultaneas(Partido).

        Es inversible ya que reducimos el Universo Conocido en la linea 110, mediante el generador realizoAccion/2, a solo aquellos que sean un Partido 
        y hayan realizado alguna accion.

        Ejemplo de consulta existencial con variable anonima: caracteristicasSimultaneas(_). ¿Existe algun partido que tenga dos o mas caracteristicas 
        distintas al mismo tiempo? Lo que daria true dos veces, ya que encuentra dos caminos distintos. (partidoPDEP es el que cumple en ambas ocasiones).

        Tambien podemos preguntar por el universo de partidos que cumplen con el predicado: caracteristicasSimultaneas(Quienes).
        Quienes = partidoPDEP, Quienes = partidoPDEP.

    - Predicado No Inversible: esGenuina(obra(Anio)). 
        
        No es inversible ya que se utiliza el not sin acotar previamente el universo (no hay un generador). Prolog no sabe cuales son los años que puede unificar.

        Al no ser inversible no se puede preguntar si existe alguna obra que sea genuina ( esGenuina(obra(_)) ), ni cuales son las obras del Universo que
        sean genuinas ( esGenuina(obra(Cuales)) ). Solo le podemos hacer consultas puntuales; esGenuina(obra(2018)) = true, esGenuina(obra(2017)) = false. 
   

*/
