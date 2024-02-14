library(MASS)
library(ggplot2)
library(GGally)
library(rio)
library(leaps)
library(car)
library(dplyr)
library(RColorBrewer)


####### 1 #######

Data=import("C:/Users/teobo/Desktop/3_6_Inferenza/ProgettoR_Latex/TEH_World_Happiness_2019_Imputed.xlsx")

Data$Continent=NULL
head(Data)
summary(Data)
str(Data)

Data$Country=NULL # = alla mia y
Data$Year=NULL #irrilevante
Data$Happiness.rank=NULL 

ggpairs(data = Data, title ="Relationships between predictors & response", 
        lower = list(continuous=wrap("points", alpha = 0.5, size=0.1)))

####### 2 #######

g_prova = lm(Happiness.Score ~ ., data=Data)  
summary(g_prova)
shapiro.test(g_prova$residuals)
plot(g_prova, which=1, pch=16)
plot(g_prova, which=2, pch=16)

influencePlot(g_prova, id.method = "identify", main = "influential Plot", 
              sub = "Circle size is proportial to Cook's Distance")

Data=Data[-c(148,149,152),]

g = lm(Happiness.Score ~ ., data=Data)
summary(g)
shapiro.test(g$residuals)
plot(g, which=1, pch=16)
plot(g, which=2, pch=16)

#influencePlot(g, id.method = "identify", main = "influential Plot", 
              #sub = "Circle size is proportial to Cook's Distance")

####### 3 #######

g2 = update(g, . ~ . - Generosity) #Statisticamente irrilevante
summary(g2)
shapiro.test(g2$residuals)

g3 = update(g2, . ~ . - Healthy.life) #Poco rilevante
summary(g3)
shapiro.test(g3$residuals)

g4 = update(g3, . ~ . - Corruption) #Non super rilevante
summary(g4)
shapiro.test(g4$residuals)

g5 = update(g4, . ~ . - Freedom) #L'unico dei 3 che posso levare senza perdere normalità
summary(g5)
shapiro.test(g5$residuals)

####### 4 #######

my_colors = brewer.pal( length( levels( Data2$Continent ) ), 'Set2')

Data2=import("C:/Users/teobo/Desktop/3_6_Inferenza/ProgettoR_Latex/TEH_World_Happiness_2019_Imputed.xlsx")
Data2=na.omit(Data2)
table(Data2$Continent)
Data2=Data2[Data2$Continent != "Oceania",]
Data2$Continent=as.factor(Data2$Continent)
plot(Data2$Continent, Data2$Happiness.Score)
boxplot( Data2$Happiness.Score ~ Data2$Continent, xlab = 'Continent', ylab = 'Happiness.Score',
         col = my_colors )

tapply(Data2$Happiness.Score , Data2$Continent, function( x ) ( shapiro.test( x )$p ) )
Data2=Data2[Data2$Country != "Haiti",]
Data2=Data2[Data2$Country != "Venezuela",]
tapply(Data2$Happiness.Score , Data2$Continent, function( x ) ( shapiro.test( x )$p ) )
plot(Data2$Continent, Data2$Happiness.Score)
boxplot( Data2$Happiness.Score ~ Data2$Continent, xlab = 'Continent', ylab = 'Happiness.Score',
         col = my_colors )

bartlett.test(Data2$Happiness.Score,Data2$Continent)
#leveneTest(Data2$Happiness.Score,Data2$Continent)

# provo boxcox

g = lm(Happiness.Score ~ Continent, data=Data2)
b = boxcox( g, lambda = seq(0.23,1.5,by=0.01) )
best = b$x[ which.max( b$y ) ] 
best

bartlett.test(Data2$Happiness.Score^best,Data2$Continent)
#leveneTest(Data2$Happiness.Score^best,Data2$Continent)

# provo boxcox con america unita

Data3=import("C:/Users/teobo/Desktop/3_6_Inferenza/ProgettoR_Latex/TEH_World_Happiness_2019_Imputed copia.xlsx")
Data3=na.omit(Data3)
table(Data3$Continent)
Data3=Data3[Data3$Continent != "Oceania",]
Data3$Continent=as.factor(Data3$Continent)
tapply(Data3$Happiness.Score , Data3$Continent, function( x ) ( shapiro.test( x )$p ) )
plot(Data3$Continent, Data3$Happiness.Score)
boxplot( Data3$Happiness.Score ~ Data3$Continent, xlab = 'Continent', ylab = 'Happiness.Score',
         col = my_colors )
#Data3[Data3$Country == "Venezuela",3]=4.807 # 4.707
#Data3[Data3$Country == "Haiti",3]=3.997 # 3.597
Data3=Data3[Data3$Country != "Haiti",]
#Data3=Data3[Data3$Country != "Venezuela",]
#Data3=Data3[Data3$Country != "Canada",]
#Data3=Data3[Data3$Country != "Costa Rica",]
tapply(Data3$Happiness.Score , Data3$Continent, function( x ) ( shapiro.test( x )$p ) )



plot(Data3$Continent, Data3$Happiness.Score)

my_colors = brewer.pal( length( levels( Data3$Continent ) ), 'Set2')

####

Data3=import("C:/Users/teobo/Desktop/3_6_Inferenza/ProgettoR_Latex/TEH_World_Happiness_2019_Imputed copia.xlsx")
Data3=na.omit(Data3)
table(Data3$Continent)
Data3=Data3[Data3$Continent != "Oceania",]
Data3=Data3[Data3$Country != "Haiti",]
Data3$Continent=factor(Data3$Continent,levels=c("Africa","Asia","Europe","America"))
plot(Data3$Continent, Data3$Happiness.Score)

boxplot( Data3$Happiness.Score ~ Data3$Continent, xlab = 'Continent', ylab = 'Happiness.Score',
         col = my_colors )
abline( h = mean( Data3$Happiness.Score ) )
###





bartlett.test(Data3$Happiness.Score,Data3$Continent)
#leveneTest(Data3$Happiness.Score,Data3$Continent)


g = lm(Happiness.Score ~ Continent, data=Data3)
b = boxcox( g, lambda = seq(0.3,1.5,by=0.01) )
best = b$x[ which.max( b$y ) ] 
best

bartlett.test(Data3$Happiness.Score^best,Data3$Continent)
tapply(Data3$Happiness.Score^best, Data3$Continent, function( x ) ( shapiro.test( x )$p ) )
#leveneTest(Data3$Happiness.Score^best,Data3$Continent)

#andata al 2% col barlett su Data3 (problema è l'america, ma ho meno dati quindi ci sta)


fit = aov(Data3$Happiness.Score ~ Data3$Continent)
summary(fit)

summary(g)
anova(g)

#vediamo se c'è differenza tra asia america ed europa

Data4=import("C:/Users/teobo/Desktop/3_6_Inferenza/ProgettoR_Latex/TEH_World_Happiness_2019_Imputed copia.xlsx")
Data4=na.omit(Data4)
Data4=Data4[Data4$Continent != "Oceania",]
Data4=Data4[Data4$Continent != "Africa",]
table(Data4$Continent)
Data4$Continent=as.factor(Data4$Continent)
tapply(Data4$Happiness.Score , Data4$Continent, function( x ) ( shapiro.test( x )$p ) )
plot(Data4$Continent, Data4$Happiness.Score)
Data4=Data4[Data4$Country != "Haiti",]
tapply(Data4$Happiness.Score , Data4$Continent, function( x ) ( shapiro.test( x )$p ) )
plot(Data4$Continent, Data4$Happiness.Score)
boxplot( Data4$Happiness.Score ~ Data4$Continent, xlab = 'Continent', ylab = 'Happiness.Score',
         col = my_colors )

bartlett.test(Data4$Happiness.Score,Data4$Continent)

fit = aov(Data4$Happiness.Score ~ Data4$Continent)
summary(fit)

Data4$Continent=relevel(Data4$Continent, ref="Asia")
g = lm(Happiness.Score ~ Continent, data=Data4)
summary(g)
anova(g)

#vediamo se c'è differenza tra america ed europa

Data5=import("C:/Users/teobo/Desktop/3_6_Inferenza/ProgettoR_Latex/TEH_World_Happiness_2019_Imputed copia.xlsx")
Data5=na.omit(Data5)
Data5=Data5[Data5$Continent != "Oceania",]
Data5=Data5[Data5$Continent != "Africa",]
Data5=Data5[Data5$Continent != "Asia",]
table(Data5$Continent)
Data5$Continent=as.factor(Data5$Continent)
tapply(Data5$Happiness.Score , Data5$Continent, function( x ) ( shapiro.test( x )$p ) )
plot(Data5$Continent, Data5$Happiness.Score)
Data5=Data5[Data5$Country != "Haiti",]
tapply(Data5$Happiness.Score , Data5$Continent, function( x ) ( shapiro.test( x )$p ) )
plot(Data5$Continent, Data5$Happiness.Score)
boxplot( Data5$Happiness.Score ~ Data5$Continent, xlab = 'Continent', ylab = 'Happiness.Score',
         col = my_colors )

bartlett.test(Data5$Happiness.Score,Data5$Continent)

fit = aov(Data5$Happiness.Score ~ Data5$Continent)
summary(fit)

g = lm(Happiness.Score ~ Continent, data=Data5)
summary(g)
anova(g)


#NB 
#La crisi in Venezuela, è una congiuntura di diversi problemi economici: 
#la crisi finanziaria, la carenza di prodotti e medicinali di base, 
#l'aumento della disoccupazione dovuto alla chiusura di società private e
# la migrazione di massa verso altri paesi della regione.

#NB
#Le precarie infrastrutture (luce, acqua, telefono), già danneggiate dal terremoto, 
#sono state spazzate via rompendo tutti i tipi di comunicazione. 
#Inoltre, ad agosto 2019, è anche scoppiata la guerra civile
#A dieci anni dal sisma, il sistema sanitario è al collasso mentre 
#il 59% della popolazione vive al di sotto della soglia di povertà.