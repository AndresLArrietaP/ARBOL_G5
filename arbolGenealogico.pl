%Hechos
progenitor(alejandro,nancy).
progenitor(alejandro,carmen).
progenitor(alejandro,angela).
progenitor(alejandro,alicia).
progenitor(alejandro,ady).
progenitor(alejandro,berta).
progenitor(alejandro,ramiro).

progenitor(cerila,nancy).
progenitor(cerila,carmen).
progenitor(cerila,angela).
progenitor(cerila,alicia).
progenitor(cerila,ady).
progenitor(cerila,berta).
progenitor(cerila,ramiro).

progenitor(nancy,jorge).
progenitor(hernan,jorge).

progenitor(carmen,samuel).
progenitor(carmen,fiorella).
progenitor(carmen,henry).
progenitor(fabio,samuel).
progenitor(fabio,fiorella).
progenitor(fabio,henry).

progenitor(angela,milagros).
progenitor(angela,pablo).
progenitor(alejo,milagros).
progenitor(alejo,pablo).

progenitor(alicia,jesus).
progenitor(alicia,cesarin).
progenitor(cesar,jesus).
progenitor(cesar,cesarin).

progenitor(ady,naomi).
progenitor(rolando,naomi).

hombre(alejandro).
hombre(hernan).
hombre(fabio).
hombre(alejo).
hombre(cesar).
hombre(rolando).
hombre(ramiro).
hombre(jorge).
hombre(samuel).
hombre(henry).
hombre(pablo).
hombre(cesarin).
hombre(hernan).

mujer(cerila).
mujer(nancy).
mujer(carmen).
mujer(angela).
mujer(alicia).
mujer(ady).
mujer(berta).
mujer(fiorella).
mujer(milagros).
mujer(naomi).

nacionalidad(alejandro,peruana).
nacionalidad(hernan,peruana).
nacionalidad(fabio,peruana).
nacionalidad(alejo,peruana).
nacionalidad(cesar,peruana).
nacionalidad(rolando,peruana).
nacionalidad(ramiro,peruana).
nacionalidad(jorge,peruana).
nacionalidad(samuel,peruana).
nacionalidad(henry,peruana).
nacionalidad(pablo,peruana).
nacionalidad(cesarin,peruana).
nacionalidad(hernan,peruana).

nacionalidad(cerila,peruana).
nacionalidad(nancy,peruana).
nacionalidad(carmen,peruana).
nacionalidad(angela,peruana).
nacionalidad(alicia,peruana).
nacionalidad(ady,peruana).
nacionalidad(berta,peruana).
nacionalidad(fiorella,peruana).
nacionalidad(milagros,peruana).
nacionalidad(naomi,peruana).

%Reglas

padre(X, Y) :- progenitor(Y, X), hombre(Y).	
madre(X, Y) :- progenitor(Y, X), mujer(Y).

diferente(X, Y) :- not(igual(X, Y)).
igual(X, X).

brothers(X, Y) :- progenitor(Z, X), progenitor(Z, Y), diferente(X, Y).
brothershombre(X, Y) :- brothers(X, Y), hombre(Y).
brothermujer(X, Y) :- brothers(X, Y), mujer(Y).

abuelos(X, Y) :- progenitor(Z, X), progenitor(Y, Z).
abuelosmujer(X, Y) :- abuelos(X, Y), mujer(Y).
abueloshombre(X, Y) :- abuelos(X, Y), hombre(Y).

nietos(X, Y) :- progenitor(X, Z), progenitor(Z, Y).
nietosmujer(X, Y) :- nietos(X, Y), mujer(Y).
nietoshombre(X, Y) :- nietos(X, Y), hombre(Y).

pareja(X, Y) :- progenitor(X, Z), progenitor(Y, Z), diferente(X, Y).

cunadospareja(X, Y) :- pareja(X, Z), brothers(Z, Y).
cunadoshermanos(X, Y) :- pareja(Y, Z), brothers(Z, X).
cunados(X, Y) :- cunadospareja(X, Y).
cunados(X, Y) :- cunadoshermanos(X, Y).

tiosdirectos(X, Y) :- progenitor(Z, X), brothers(Z, Y).	
tiospoliticos(X, Y) :- progenitor(Z, X), brothers(Z, W), pareja(W,Y).
tios(X,Y) :- tiosdirectos(X, Y).
tios(X,Y) :- tiospoliticos(X, Y).

primos(X, Y) :- tiosdirectos(X, Z), progenitor(Z, Y).
primoshombre(X, Y) :- primos(X, Y), hombre(Y).
primosmujer(X, Y) :- primos(X, Y), mujer(Y).

sobrinosdirectos(X, Y) :- brothers(X, Z), progenitor(Z, Y).
sobrinospareja(X, Y) :- pareja(X, Z), sobrinosdirectos(Z,Y).
sobrinos(X, Y) :- sobrinosdirectos(X, Y).
sobrinos(X, Y) :- sobrinospareja(X, Y).

ascendencia(Y, X) :- progenitor(X, Y).
ascendencia(Y, X) :- progenitor(Z, Y), ascendencia(Z, X).

descendencia(X, Y) :- progenitor(X, Y).
descendencia(X, Y) :- progenitor(X, Z), descendencia(Z, Y).

suegros(X, Y) :- pareja(X, Z), progenitor(Y,Z).

yernos(X, Y) :- pareja(Y, Z), progenitor(X, Z), hombre(Y).
nueras(X, Y) :- pareja(Y, Z), progenitor(X, Z), mujer(Y).

familiadirecta(X, Y) :- hermanosdirectos(X, Y).
familiadirecta(X, Y) :- progenitor(Y, X).
familiadirecta(X, Y) :- pareja(X, Y).
familiadirecta(X, Y) :- progenitor(X, Y).

hermanosm(X, Y) :- madre(X, Z), progenitor(Z, Y), diferente(X, Y).
hermanosp(X, Y) :- padre(X, Z), progenitor(Z, Y), diferente(X, Y).

hermanosmadre(X, Y) :- madre(X, Z), progenitor(Z, Y), diferente(X, Y), not(hermanosp(X,Y)).
hermanospadre(X, Y) :- padre(X, Z), progenitor(Z, Y), diferente(X, Y), not(hermanosm(X,Y)).

hermanosdirectos(X, Y) :- hermanosm(X, Y), hermanosp(X, Y).

america(X) :- nacionalidad(X, peruana);nacionalidad(X, argentina);nacionalidad(X, boliviana);nacionalidad(X, chilena).
europa(X) :- nacionalidad(X, espanola);nacionalidad(X, inglesa);nacionalidad(X, francesa).
asia(X) :- nacionalidad(X, china);nacionalidad(X, japonesa);nacionalidad(X, taiwanesa);nacionalidad(X, india);nacionalidad(X, norcoreana);nacionalidad(X, surcoreana).
oceania(X) :- nacionalidad(X, australiano);nacionalidad(X, papuano).
africa(X) :- nacionalidad(X, sudafricano);nacionalidad(X, camerunes);nacionalidad(X, malgache);nacionalidad(X, congoleno).

americano(X) :- america(X),hombre(X).
americana(X) :- america(X),mujer(X).
europeo(X) :- europa(X),hombre(X).
europea(X) :- europa(X),mujer(X).
asiatico(X) :- asia(X),hombre(X).
asiatica(X) :- asia(X),mujer(X).
oceanico(X) :- oceania(X),hombre(X).
oceanica(X) :- oceania(X),mujer(X).
africano(X) :- africa(X),hombre(X).
africana(X) :- africa(X),mujer(X).

% Regla recursiva para determinar si X es americano.
des_americano(X) :- americano(X).
des_americano(X) :- progenitor(Y, X), des_americano(Y).
des_americana(X) :- americana(X).
des_americana(X) :- progenitor(Y, X), des_americana(Y).

des_europeo(X) :- europeo(X).
des_europeo(X) :- progenitor(Y, X), des_europeo(Y).
des_europea(X) :- europea(X).
des_europea(X) :- progenitor(Y, X), des_europea(Y).

des_asiatico(X) :- asiatico(X).
des_asiatico(X) :- progenitor(Y, X), des_asiatico(Y).
des_asiatica(X) :- asiatica(X).
des_asiatica(X) :- progenitor(Y, X), des_asiatica(Y).

des_oceanico(X) :- oceanico(X).
des_oceanico(X) :- progenitor(Y, X), des_oceanico(Y).
des_oceanica(X) :- oceanica(X).
des_oceanica(X) :- progenitor(Y, X), des_oceanica(Y).

des_africano(X) :- africano(X).
des_africano(X) :- progenitor(Y, X), des_africano(Y).
des_africana(X) :- africana(X).
des_africana(X) :- progenitor(Y, X), des_africana(Y).
