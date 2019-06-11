# Prac2_Suicide_rate
Estudi que pretén conèixer quins factors influeixen en l’augment de les taxes de suïcidi.

# Introducció

El dataset s’ha extret de la següent url: (https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016)


# Inspiració

Després de realitzar una recerca exhaustiva entre els diferents repositoris de www.kaggle.com,per a trobar
un dataset que respongués algun tema que considero interessant, finalment he decidit escollir aquest, degut
no únicament a la pregunta que pretén respondre, sinó a l’estructura d’aquest, ja que és un conjunt de
dades amb suficients observacions com per a aplicar diferents mètodes d’anàlisi, i aconseguir una resposta
més acurada sobre la pregunta que pretén respondre. Es pretén saber quins factors influeixen en l’augment
de les taxes de suïcidi, i si aquests es troben en les variables del dataset, és a dir, si aquestes variables són
explicatives del nombre dels suïcidis.


# Conjunt de dades

El dataset inicial té 27820 observacions i 12 variables.

A continuació, farem una breu explicació de les variables del dataset:

•	country: Variable de tipus factor que
descriu el país d’estudi dels suïcidis. Pot prendre 101 valors, com hem vist en aplicar la funció str().

•	year: Variable de tipus integer que indica l’any en el que s’han comès els suïcidis estudiats.

•	age: Variable de tipus factor que indica el rang d’edat dels suïcidis comessos. Hi ha 6 valors diferents.
Traurem la cadena ” years” dels nivells del factor.

•	suicides_no: Variable de tipus integer que indica el nombre de suïcidis

•	population: Variable de tipus integer que indica la població, segons un any, un rang d’edat i el sexe.

•	suicides.100k.pop:* Variable de tipus numeric que indica la taxa de suïcidis en relació a una població, en
percentatge del 100000%. Aquest valor el podem obtenir aplicant un càlcul, pel que aquesta columna seria
candidata a ser descartada. El càlcul seria el següent: (suicides_no/population)100000

•	country.year: Variable de tipus factor que concatena el país amb l’any d’ocurrència del suïcidi. Ja tenim la
variable país i la variable any, llavors aquesta columna és una clara candidata a ser descartada del conjunt
de dades.

•	HDI.for.year: Variable de tipus numeric, que indica l’índex de desenvolupament humà. Un 70% de les
observacions d’aquesta variable són valors buits, pel que aquesta variable és una clara candidata a ser
descartada del dataset.

•	gdp_for_year: Variable de tipus numeric que indica el PIB per any del país. Aquesta variable està
erròniament tipada com a factor, ja que els separadors s’han aplicat amb el caràcter ‘,’. A l’apartat de neteja
de dades aplicarem la solució corresponent.

•	gdp_per_capita: Variable de tipus integer que indica el PIB per capita, segons un país, un any, un rang
d’edat i el sexe.

•	generation: Variable de tipus factor amb 6 nivells, que indica la generació en la qual ha sigut classificada
l’observació corresponent.

