#NOTA IMPORTANT: S'ha decidit eliminar molts dels comentaris del codi, els quals es troben a la versió en PDF,
#per tal d'evitar embrutar massa el codi.

#APARTAT 1: Descripció del dataset. Perquè és important i quina pregunta/problema pretén respondre?
#Després de realitzar una recerca exhaustiva entre els diferents repositoris de www.kaggle.com,
#per a trobar un dataset que respongués algun tema que considero interessant, finalment he decidit
#escollir aquest, degut no únicament a la pregunta que pretén respondre, sinó a l'estructura d'aquest,
#ja que és un conjunt de dades amb suficients observacions per a aplicar diferents mètodes d'anàlisi, i
#aconseguir una resposta més acurada sobre la pregunta que pretén respondre. Es pretén saber quins factors
#influeixen en l'augment de les taxes de suïcidi, i si aquests es troben en les variables del dataset,
#és a dir, si aquestes variables són explicatives del nombre dels suïcidis.

#El dataset s'ha extret de la següent url: https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016



#APARTAT 2: Integració i selecció de les dades d’interès a analitzar.

#Per a descriure les variables del dataset, primer carregarem les dades:
suicideData<-read.csv("C:/PRAC2/suicide_rates.csv", header=T, sep = ",",na.strings = "NA")

#Amb la funció dim(), podrem veure el nombre d'observacions del dataset i el nombre de variables. Aquest dataset
#té 27820 observacions i 12 variables.
dim(suicideData)

#Mitjançant la funció str() mostrarem els tipus de dades de la variable, i alguns valors que prenen, però
#també podrem detectar a priori si hem de canviar el tipus de dades d'alguna variable.
str(suicideData)

#També mitjançant la funció summary() es mostren els valors que prenen les variables, amb una visió més amplia,
#i mostra alguns càculs estadístics de les variables quantitatives. Aquesta funció ens servirà per a comprovar si hi ha valors
#erronis, per exemple, veiem que la variable HDI.for.year conté 19456 NA's o elements buits.
summary(suicideData)


#A l'apartat següent de neteja de dades, haurem de realitzar, entre d'altres, les següents tasques de neteja:
#a) Modificar el nom de les columnes.
#b) Treurem el caràcter ',' a la variable gdp_for_year i canviar el tipus de dades a numeric. També treurem la cadena " years" dels nivells del factor age.
#c) Mirar si no hi ha factors repetits, per exemple, que no hi hagi "Albania" i " Albania ".
#d) Eliminar les variables: HDI for year, country-year, suicides.100k.pop.
#e) Buscarem valors perduts i outliers.



#APARTAT 3: Neteja de les dades.

#a) Començarem canviant el nom de les variables:
colnames(suicideData)[1] <- "country"
colnames(suicideData)[7] <- "suicides_100k"
colnames(suicideData)[8] <- "country_year"
colnames(suicideData)[9] <- "HDI_for_year"
colnames(suicideData)[10] <- "gdp_for_year"
colnames(suicideData)[11] <- "gdp_per_capita"

#b) A continuació, substituirem el caràcter ',' pel caràcter '.' a la variable gdp_for_year i canviarem el tipus de dades a numeric.
#Apliquem la funció options(scipen = 999) per a evitar el format amb la notació científica.
options(scipen = 999)
suicideData$gdp_for_year<-gsub(",","",suicideData$gdp_for_year)
suicideData$gdp_for_year<-as.numeric(suicideData$gdp_for_year)

#I comprovarem mitjançant una taula, que el tipus de les variables és el correcte:
library(knitr)
tipus <- sapply(suicideData,class)
kable(data.frame(Variables=names(tipus),Classe=as.vector(tipus)),
      caption = "Tipus de dades")

#A continuació, treurem la cadena " years" dels nivells del factor age:
suicideData$age<-as.factor(gsub(" years","",suicideData$age))


#c) Ara, mirarem que no hi hagi factors a les variables de tipus factor:
#No mirarem la variable country_year, que era clara candidata a descartar.
#Aplicarem la funció sapply(), concretament sapply(data1,levels), per a obtenir els nivells de les variables factors
#per a poder comprovar que aquestes són correctes, però al codi aplicarem la funció str() perquè en executar el codi
#i presentar-ho en pdf no surtin moltes pàgines generades pel gran volum de dades.
qualitatives<-c(1,3,4,8,12)
data1<-suicideData[,qualitatives]
str(sapply(data1,levels),6)

#d) A continuació, procedim a eliminar les variables: HDI_for_year, country_year, suicides_100k_pop.
#Les columnes a eliminar són la 7, 8 i 9:
suicideData<-suicideData[,-(c(7,8,9))]

#Com a tècnica de reducció de dimensió, pot ser interessant aplicar una Anàlisi de Components Principals (PCA).

#L'objectiu de l'Anàlisi de Components Principals és sintetitzar el nombre
#d'atributs del dataset, és a dir, reduir el nombre d'atributs del dataset, el
#que resulta especialment beneficiós en datasets que tinguin un nombre molt elevat
#de variables, disminuint d'aquesta manera la dimensió de la problemàtica de l'anàlisi.
#El procediment és transformar el conjunt de variables originals en un conjunt de noves
#variables que anomenarem components principals, i que la seva principal característica
#és que existeixi correlació entre alguna d'aquestes variables.

#Per a aquest estudi únicament té sentit seleccionar les variables quantitatives,
#ja que per al càlcul de les components necessitem estimar una matriu de correlació:

#En el cas del nostre dataset, el qual únicament conté 6 variables quantitatives pot resultar
#no tan útil com si s'apliqués a un dataset de major dimensió.

quantitatives<-c(2,5,6,7,8)
df_quant<-suicideData[,quantitatives]

pca.quantitatives <- prcomp(df_quant,scale=T)
pca.quantitatives
summary(pca.quantitatives)

#Podem veure que l'aplicació de la funció prcomp() ens proposa 5 components principals. La primera variable year, explica
#el 44,94% de la variància total, i els 4 primers components expliquen el 95.56%. Podríem prescindir del quint, però en aquest
#cas concret, com no tenim tantes variables, deixarem el dataset com ho tenim.


#e) Buscarem valors perduts i outliers.
#Això ho farem en els apartats 3.1 i 3.2 respectivament.


#APARTAT 3.1: Les dades contenen zeros o elements buits? Com gestionaries aquests casos?
#Els valors perduts o buits poden manifestar-se amb el nom "NA", amb zeros o amb algún caràcter del tipus “”, “ “ o "?".
#A l'apartat 2, concretament al subapartat c), hem comprovat que els nivells de les variables qualitatives eren correctes
#i no contenien valors buits. A continuació, realitzarem la prova a tot el dataset, per analitzar especialment si les
#variables de tipus quantitatiu contenen valors buits.
#Podem aplicar la funció sapply() passant com a paràmetre la funció is.na() que s'aplicarà a tot el dataset:
sapply(suicideData, function(x) sum(is.na(x)))

#Com podem comprovar, no es detecta cap valor perdut. Aplicarem alta prova, indicant NA com a string:
grep("NA",suicideData)>0

#Comprovem que efectivament no es detecten valors perduts.
#Com hem comentat, haguéssim detectat els valors perduts fàcilment amb l'aplicació de la funció summary() al dataset:
summary(suicideData)


#APARTAT 3.2: Identificació i tractament de valors extrems.
#Els valors extrems o outliers, són observacions que semblen inconsistents amb la resta dels valors de la mostra. Amb aquesta definició,
#podem intuir que una bona manera de detectar els outliers o valors extrems és mitjançant diagrames de caixa (boxplot) per a les 
#variables quantitatives i diagrames circulars o diagrames de barres per a les quantitatives.

#Diagrama de barres per a les qualitatives:
par(mfrow = c(1:2))
barplot(table(suicideData$country), xlab="País", ylab="Freqüència", main="Diagrama de barres País",col="lightsteelblue")
barplot(table(suicideData$generation), xlab="Generació", ylab="Freqüència", main="Diagrama de barres Generació",col="lightsteelblue")

#No observem cap valor extrem, les dades estan repartides de manera uniforme, és clar que per exemple, en relació als països, hi haurà més observacions
#per a uns que per a altres, però podem considerar els valors com normals.

par(mfrow = c(1:2))
barplot(table(suicideData$sex), xlab="Sexe", ylab="Freqüència", main="Diagrama de barres Sexe",col="lightsteelblue")
barplot(table(suicideData$age), xlab="Edat", ylab="Freqüència", main="Diagrama de barres Edat",col="lightsteelblue")

#Si observem els diagrames, veiem que el nombre d'observacions per a cada nivell de cada factor és molt similar. Ho comparem amb taules per a confirmar-ho:

table(suicideData$sex)
table(suicideData$age)


#Boxplot per a les quantitatives:

#A continuació, analitzarem si existeixen outliers en les variables quantitatives. En apartats anteriors, mitjançant l'aplicació de la funció summary(),
#vam veure que les variables quantitatives, a excepció de la variable year, prenien valors molt diferents, llavors els diagrames de caixa sortiran esbiaixats,
#pero els resultats són correctes, no hi ha presència d'outliers, simplement hi ha valors molt baixos i molts alts en la mateixa variable, això afecta a totes
#les variables quantitatives, a excepció de la variable year.

#Per a mostrar que els resultats del boxplot no són deguts a presència d'outliers, sinó a la diferència exagerada en els valors d'una mateixa variable,
#possarem d'exemple la variable suicides_no, i primer distribuirem les observacions en rangs per poder representar de millor manera les dades, mitjançant
#l'algorisme d'Sturges, i després ho mostrarem gràficament.

#Ens ajudarem de les següents taules:
options(knitr.kable.NA = '')
qualitatives<-c(1,3,4,9)
discretes<-c(2,5,6,8)
continues<-c(7)
kable(summary(suicideData)[,qualitatives],
      digits=2, align='l', caption="Variables qualitatives")

#Estudi de variables quantitatives discretes
kable(summary(suicideData)[,discretes],
      digits=2, align='l', caption="Variables quantitatives discretes")

#Estudi de variables quantitatives contínues
kable(summary(suicideData)[,continues],
      digits=2, align='l', caption="Variables quantitatives contínues")

boxplot(suicideData$year,main="Box plot: Any", col="lightsteelblue",na.rm=TRUE)

boxplot(suicideData$suicides_no,main="Box plot: Nombre de suïcidis", col="lightsteelblue",na.rm=TRUE)

#Apliquem Sturges:
rang.h<-range(suicideData$suicides_no,na.rm=TRUE)
nclass.Sturges(suicideData$suicides_no)
seq(rang.h[1],rang.h[2],length=nclass.Sturges(suicideData$suicides_no))
intervalshs=cut(suicideData$suicides_no,breaks=seq(rang.h[1],rang.h[2],length=nclass.Sturges(suicideData$suicides_no)),include.lowest=TRUE)
table(intervalshs)

#Efectivamente veiem que no són outliers, sinó que la major part de les observacions es concentra en el primer valor, i la resta estan repartits, però
#són correctes. La mateixa situació s'aplica als gràfics de la resta de variables quantitatives:

barplot(table(intervalshs), xlab="Sexe", ylab="Freqüència", main="Diagrama de barres variable sexe",col="lightsteelblue")
barplot(suicideData$suicides_no)

boxplot(suicideData$population,main="Box plot: Població", col="lightsteelblue",na.rm=TRUE)

boxplot(suicideData$gdp_for_year,main="Box plot: PIB per any", col="lightsteelblue",na.rm=TRUE)

boxplot(suicideData$gdp_per_capita,main="Box plot: PIB per capita", col="lightsteelblue",na.rm=TRUE)

#Guardarem el conjunt de dades net:
ruta <- "C:/PRAC2/suicide_rates_Solution.csv"
write.csv(suicideData,file=ruta,row.names=F)

#APARTAT 4: Anàlisi de les dades.
#APARTAT 4.1: Selecció dels grups de dades que es volen analitzar/comparar (planificació dels anàlisis a aplicar).

#Resolt a la versió en PDF.


#APARTAT 4.2: Comprovació de la normalitat i homogeneïtat de la variància.
#a) Estudi de la normalitat
#A continuació, estudiarem si les variables qualitatives segueixen una distribució normal. Ho farem mitjançant
#la representació gràfica de la corba de la normalitat i mitjançant el test de normalitat Lilliefors de Komolgorov-Smirnov, ja que
#és el test que més s'adequa al nostre dataset. El test de Shapiro Wilk és adequat quan el nombre d'observacions és petit. Es
#recomana per a un nombre d'observacions inferior o igual a 30, encara que es pot aplicar fins a 5000 observacions, com veurem
#en un exemple:
quantit<-colnames(suicideData[,quantitatives])
compt<-0
library(nortest)
for(i in quantitatives){
  compt<-compt+1
  qqnorm(suicideData[,i],main=paste ("Normal Q-Q of ", quantit[compt]))
  qqline(suicideData[,i])
  print(lillie.test(suicideData[,i]))
}


#Si hi apliquem el shapiro test, a les primeres 5000 observacions (és el límit) ens surt el mateix valor del p-value.
for(i in quantitatives){
  print(shapiro.test(suicideData[,i][0:5000]))
}


#b) Estudi de l'homogeneïtat de la variància
#Per a realitzar l'estudi de la homocedasticitat o igualtat de variàncies dels grups a comparar, aplicarem un test de Levene, prova paramètrica que
#s'aplica quan les dades segueixen una distribució normal, com és el cas del dataset en estudi.
#En cas que el p-value sigui superior o igual a 0.05, no rebutjarem la hipòtesi nul·la que les variàncies són iguals.
#En cas contrari, rebutjarem la hipòtesi nul·la en favor de l'alternativa que indica que les variàncies dels nivells del factor analitzat són diferents.

#Realitzarem l'estudi amb totes les variables categòriques, en funció del nombre de suïcidis. Les variables categòriques seran les explicatives de
#la variable explicada suicides_no:
library(car)
leveneTest(suicides_no~country, data = suicideData)
leveneTest(suicides_no~sex, data = suicideData)
leveneTest(suicides_no~age, data = suicideData)
leveneTest(suicides_no~generation, data = suicideData)

#Com podem veure, l'estadístic de contrast F ens surt molt alt, és molt superior a 2 en tots els tests. A més, podem comprovar que totes les variables són significatives,
#totes tenen els tres ***, i el seu p-value és molt inferior a 0.05, proper a 0, pel que rebutjarem la hipòtesi nul·la que les variàncies de les
#variables del grup són iguals.

#També podríem haver aplicat el test de Kruskal Wallis, per a determinar si qualsevol de les diferències entre les mitjanes (no variàncies)
#són estadísticament significatives, i el test pairwise.wilcox.test per a determinar quins són els nivells dels factors amb mitjanes
#que són iguals o diferents. Ho aplicarem d'exemple a la variable generation:
kruskal.test(suicides_no~generation,data=suicideData)
pairwise.wilcox.test(suicideData$suicides_no,suicideData$generation,p.adj='bonferroni',exact=F)

#Efectivament, ens surt que hi ha una diferència significativa entre els diferents nivells del factor (p-value molt petit per a tots els nivells de la matriu).


#APARTAT 4.3: Aplicació de proves estadístiques per comparar els grups de dades. En funció de
#les dades i de l’objectiu de l’estudi, aplicar proves de contrast d’hipòtesis,
#correlacions, regressions, etc. Aplicar almenys tres mètodes d’anàlisi diferents.

#a) Quines variables quantitatives tenen més influència en el nombre de suïcidis?
#En primer lloc procedirem a realitzar una anàlisi de correlació per a tractar d'assolir el nostre objectiu d'estudi,
#on volem saber quines variables tenen més influència en el nombre de suïcidis, concretament, ho aplicarem a les variables
#de tipus quantitatiu. El mètode que aplicarem serà el Coeficient de correlació de Pearson.
#La funció que ens permet calcular la correlació entre dues variables de tipus quantitatiu és cor():
cor(x=suicideData$suicides_no,y=suicideData$year,method="pearson")

#Aplicarem el test, perquè ens indiqui si la correlació entre variables és diferent de zero i també
#l'interval de confiança al 95%.
for(i in quantit[-2]){
  print(cor.test(suicideData$suicides_no,suicideData[,i]))
}


#També podem obtenir directament una matriu de correlacions:
quantitatives_data<-suicideData[,quantitatives]
cor(quantitatives_data)

#Per a respondre a la pregunta feta a aquest apartat, podem determinar que existeix una forta correlació positiva entre la variable suicide_no i la variable
#population.

#D'altra banda, si analitzem la correlació entre la resta de variables podem determinar que existeix una forta correlació
#positiva entre les variables population i gdp_for_year.


#b) El nombre de suïcidis està influït pel PIB per capita, la població, el rang d'edat i la generació?
#En segon lloc, per a poder respondre a la pregunta realitzada, aplicarem un model de regressió lineal múltiple, tenint en
#compte que ho aplicarem a un conjunt de dades amb regressors quantitatius i qualitatius. 

#L’acceptació de la hipòtesi nul·la implicaria que cap de les variables explicatives PIB per capita, poblacio, rang d'edat i generació expliquen la variable
#Y nombre de suïcidis, llavors, la situació ideal seria poder rebutjar la hipòtesi nul·la H0.

#Primer definirem les categories de referència per a les noves variables ageR i generacioR i les incorporarem
#al conjunt de dades. Necessitem fer això per incloure els nivells dels factors a l'estudi, ja que estem treballant amb variables
#de tipus qualitatiu.
#Establirem "25-34" com a categoria de referència per a la variable ageR i "Boomers" per a la variable generation, i afegirem
#les noves variables al conjunt de dades:
ageR<-relevel(suicideData$age,ref="25-34")
generationR<-relevel(suicideData$generation,ref="Boomers")
suicideData_regres<-data.frame(suicideData,ageR,generationR)

#Comprovem que ho hem fet correctament:

#També comprovem si s’han creat correctament els subnivells. Per exemple, el valor “Boomers” de la variable generation
#quedarà incorporat a l’intercepte B0, ja que es donarà quan a totes les Bi el valor per a aquesta variable generation
#sigui 0.
head(model.matrix(suicides_no~gdp_per_capita+ageR+generationR+population,data=suicideData_regres),6)

#Ara apliquem la funció lm() per a obtenir el model de regressió:
model<-lm(suicides_no~gdp_per_capita+ageR+generationR+population,data=suicideData_regres)
summary(model)


#Les variables quantitatives gdp_per_capita i population, són significants pel model, encara que no expliquen massa bé la
#variable suicide_no.

#Podem dir que aquesta variable generation no és significativa pel model.


#Com hem mencionat, la variable generation no és significativa, a continuació provarem de fer el mateix estudi treient-la de
#l'anàlisi:

model<-lm(suicides_no~gdp_per_capita+ageR+population,data=suicideData_regres)
summary(model)


#Podem concloure que la incorporació de la variable explicativa generation no s’ajusta de manera acceptable al
#nostre model de dades, però podem rebutjar la hipòtesi nul·la H0, ja que la resta de variables expliquen bé la variable explicada
#i són significatives, en definitiva, el nombre de suïcidis està influït pel PIB per capita, la població i el rang d'edat.


#c) Existeixen diferències significatives en el nombre de suïcidis dels homes en relació a les dones? 
#Per a trobar una resposta a aquesta pregunta, aplicarem un mètode sobre la diferència de mitjanes en el cas de variàncies
#poblacionals desconegudes, però iguals.

# Per interpretar de millor manera el diagrama de caixa, recuperarem la taula d’informació conjunta per
# intervals de valors del nombre de suicides, variable suicide_no (perquè sigui més llegible).

table(intervalshs,suicideData$sex)

# Perquè encara es vegi millor, calcularem la mitjana de la variable suicide_no per cada categoria de la variable sex:
mean.suicide_noVSsex<-aggregate(suicideData$suicides_no,by=list(suicideData$sex),mean,na.rm=TRUE)
mean.suicide_noVSsex

boxplot(suicides_no~sex, data=suicideData,main="Box plot", col="grey",na.rm=TRUE)

#A continuació expressem les hipòtesis:
#Hipòtesi nul·la: H0: u1-u2 = 0
#Hipòtessis alternativa:H1: u1-u2 != 0 (Bilateral) (on u1 i u2 són dues poblacions diferents que indiquen el valor de la mitjana d’homes i dones
#respectivament, poblacions on constrastarem la hipòtesis nul·la expressada).
# Com ja tenim expressades les hipòtesis, determinarem el nivell de significació: alfa=0.05, ja que el nivell de
# confiança és del 95%.
# Estem en un cas de contrast sobre la diferència de mitjanes en el cas de variàncies poblacionals desconegudes,
# però iguals. Hem d’analitzar si el nivell de satisfacció és igual en homes que en dones. Suposarem que totes
# dues poblacions es distribueixen normalment amb variàncies iguals i desconegudes. Haurem de contrastar
# la diferència de mitjanes per a saber si hi ha una diferència significativa o podem considerar que aquestes
# són iguals.


t.test(suicideData$suicides_no[suicideData$sex=="male"], suicideData$suicides_no[suicideData$sex=="female"], alternative = "two.sided")

#Com podem comprovar, hem obtingut com a resultat un estadístic de contrast prou alt i un p-value molt petit, el qual
#és inferior a 0.05 (nivell de significància fixat 5% amb una confiança del 95%), en definitiva, podem rebutjar la hipòtesi nul·la H0, que
#les mitjanes poblacionals eren iguals pels homes que per a les dones, existeixen diferències significatives en el nombre de suïcidis dels homes
#en relació a les dones.




#APARTAT 5: Representació dels resultats a partir de taules i gràfiques.
#NOTA IMPORTAT: Aquest apartat ha sigut resolt durant l'elaboració de la pràctica, ja que s'han mostrat gràfiques i taules en cada exercici.
#Igualment, per a complementar l'apartat, a continuació mostrarem altres gràfiques amb taules
#amb informació de suport addicional, però cal indicar que el pes més gran de les gràfiques es trova a la resta d'apartats,
#i per no repetir informació, no seran afegits en aquest apartat.

#Realitzarem l’anàlisi visual de la variable suicide_no en funció del sexe i en funció del rang d'edat. El gràfic
#ha de permetre avaluar si hi ha interacció entre factors.

#- 1: Agrupació de dades
library(ggplot2)
library(kableExtra)
library(dplyr)
taula1<-suicideData%>%
  group_by(sex,age)%>%
  summarise(Mitjana=mean(suicides_no))

#- 2: Mostrar conjunt de dades en format taula:
taula1

ggplot(data = taula1, aes(x=sex, y=Mitjana, colour=age,
                          group = age)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  labs(y = 'mean (Nombre de suïcidis)') +
  theme_bw()


#- 3: Conclusions gràfic 1:
#En el gràfic podem veure que l’increment del nombre de suïcidis entre els 6 tipus rangs d'edat, no és
#proporcional pels dos tipus de sexe, sembla que hi ha interacció entre les variables sex i age.
#Podem veure que entre les categories "35-54" i "55-74"per a les dones no hi molta diferència, en canvi sí
#hi ha una diferència molt potent en els homes quan el rant d'edat és igual a "35-54".
#D'altra banda, en relació a la categoria de "5-14", no hi ha molta diferència, en relació
#als altres nivells d'edat.

# Per tot això, tenim que hi ha diferències significatives i hi ha interacció entre variables.
 
# Totes aquestes conclusions, es veuen reforçades amb la taula calculada.


#- 4: Ara construirem el gràfic agrupant per sex:

ggplot(data = taula1, aes(x=age, y=Mitjana, colour=sex,
                            group = sex)) +
stat_summary(fun.y = mean, geom = "point") +
stat_summary(fun.y = mean, geom = "line") +
labs(y = 'mean (Nombre de suïcidis)') +
theme_bw()

#- 5: Conclusions gràfic 2:

# Com es pot observar, es veu una clara interacció, ja que les línies no son rectes, es veu fàcilment que hi
# ha desviació en el sexe, especialment notable al factor "Inc4"5-14"", pel que podem esperar al fet que hi
# hagi una interacció estadísticament significativa.


#APARTAT 6: Resolució del problema. A partir dels resultats obtinguts, quines són les conclusions? Els
#resultats permeten respondre al problema?

#Resolt a la versió en PDF.



#APARTAT 7: Codi: Cal adjuntar el codi, preferiblement en R, amb el que s’ha realitzat la neteja, anàlisi
#i representació de les dades. Si ho preferiu, també podeu treballar en Python.
#Facilitat amb el lliurament de la pràctica.

