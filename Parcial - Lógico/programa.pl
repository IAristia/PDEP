% Cancion, Compositores,  Reproducciones

cancion(bailanSinCesar, [pabloIlabaca, rodrigoSalinas], 10600177).
cancion(yoOpino, [alvaroDiaz, carlosEspinoza, rodrigoSalinas], 5209110).
cancion(equilibrioEspiritual, [danielCastro, alvaroDiaz, pabloIlabaca, pedroPeirano, rodrigoSalinas], 12052254).
cancion(tangananicaTanganana, [danielCastro, pabloIlabaca, pedroPeirano], 5516191).
cancion(dienteBlanco, [danielCastro, pabloIlabaca, pedroPeirano], 5872927).
cancion(lala, [pabloIlabaca, pedroPeirano, aplaplacRecords], 5100530).
cancion(meCortaronMalElPelo, [danielCastro, alvaroDiaz, pabloIlabaca, rodrigoSalinas], 3428854).

% Mes, Puesto, Cancion

rankingTop3(febrero, 1, lala).
rankingTop3(febrero, 2, tangananicaTanganana).
rankingTop3(febrero, 3, meCortaronMalElPelo).
rankingTop3(marzo, 1, meCortaronMalElPelo).
rankingTop3(marzo, 2, tangananicaTanganana).
rankingTop3(marzo, 3, lala).
rankingTop3(abril, 1, tangananicaTanganana).
rankingTop3(abril, 2, dienteBlanco).
rankingTop3(abril, 3, equilibrioEspiritual).
rankingTop3(mayo, 1, tangananicaTanganana).
% rankingTop3(mayo, 1, meCortaronMalElPelo).
rankingTop3(mayo, 2, dienteBlanco).
rankingTop3(mayo, 3, equilibrioEspiritual).
rankingTop3(junio, 1, dienteBlanco).
rankingTop3(junio, 2, tangananicaTanganana).
rankingTop3(junio, 3, lala).




% 1

apareceEnElRanking(Cancion, Mes):-
    rankingTop3(Mes,_,Cancion).

huboRanking(Mes):-
    rankingTop3(Mes, _, _).

esHit(Cancion):-
    cancion(Cancion,_,_),
    forall(huboRanking(Mes),apareceEnElRanking(Cancion,Mes)).


% 2

noEsReconocidaPorLosCriticos(Cancion):-
    cancion(Cancion,_,_),
    tieneMuchasReproducciones(Cancion),
    not((apareceEnElRanking(Cancion,_))).
    

tieneMuchasReproducciones(Cancion):-
    cancion(Cancion,_,Reproducciones),
    Reproducciones > 7000000.


% 3

colaboradores(Compositor1,Compositor2):-
    cancion(_,Compositores,_),
    member(Compositor1,Compositores),
    member(Compositor2, Compositores),
    Compositor1 \= Compositor2.



% 4

trabajador(tulio, conductor(5)).
trabajador(tulio, reportero(3,20)).
trabajador(bodoque, periodista(2,titulo(licenciatura))).
trabajador(bodoque, reportero(5,300)).
trabajador(marioHugo, periodista(10, titulo(posgrado))).
trabajador(juanin,conductor(0)).

% nuevo trabajador con un trabajo distinto
trabajador(mordekai, chepibe(5, 500)).


% 5

sueldoTotal(Trabajador, SueldoTotal):-
trabajador(Trabajador, _),
findall(Sueldo, sueldoDe(Trabajador, Sueldo), Sueldos),
sumlist(Sueldos, SueldoTotal).
 

sueldoDe(Persona, Sueldo):-
    trabajador(Persona, Trabajo),
    sueldoPorTrabajo(Trabajo, Sueldo).

sueldoPorTrabajo(conductor(Anios),Sueldo):-
    Sueldo is Anios * 10000.

sueldoPorTrabajo(reportero(Anios, Notas),Sueldo):-
    Sueldo is (Anios * 10000 + 100 * Notas).

sueldoPorTrabajo(periodista(Anios, titulo(licenciatura)), Sueldo):-
    Sueldo is (Anios * 5000) + (Anios * 5000) * (20/100).

sueldoPorTrabajo(periodista(Anios, titulo(posgrado)), Sueldo):-
    Sueldo is (Anios * 5000) + (Anios * 5000) * (35/100).

sueldoPorTrabajo(chepibe(Anios, CafesHechos), Sueldo):- % el sueldo de los chepibes es igual a la mitad de los años de antiguedad sumado a la cantidad de cafés que hicieron para sus jefes.
    Sueldo is (Anios / 2 + CafesHechos).

% 6

/*
Agregué al trabajador mordekai que es el chepibe del noticiero.

Esto se puede relacionar al concepto de POLIMORFISMO, en este caso, el predicado sueldoPorTrabajo es POLIMÓRFICO, es capaz de recibir functores con diferente cantidad de argumentos y devolver un valor en todos los casos (que representa el sueldo).
*/
