library(MASS)
library(ggplot2)
library(GGally)
library(rio)
library(leaps)
library(car)
library(dplyr)
library( ellipse )
library(BAS)
library(faraway)
library(rgl)
library(corrplot)
library(ggplot2)


####### 1 #######

Data=import("C:/Users/teobo/Desktop/3_6_Inferenza/ProgettoR_Latex/TEH_World_Happiness_2019_Imputed.xlsx")
Data2=Data
Data2$Year=NULL
Data2$Happiness.rank=NULL
Data2$Happiness.Score=NULL
Data2$Continent=NULL

Data$Continent=NULL
head(Data)
summary(Data)
str(Data)

Data$Country=NULL # = alla mia y
Data$Year=NULL #irrilevante
Data$Happiness.rank=NULL 

summary(Data)
Data2[!complete.cases(Data2), ]
Data[!complete.cases(Data), ]
Data <- na.omit(Data)

x11()
ggpairs(data = Data, title ="Relationships between predictors & response", 
        lower = list(continuous=wrap("points", alpha = 0.5, size=0.1))) +
  theme(plot.background = element_rect(fill = "lightblue")) +
  xlab("Predictors") +
  ylab("Response") + 
  theme(text=element_text(size=16)) 

####### 2 #######

g_prova = lm(Happiness.Score ~ ., data=Data)  
summary(g_prova)

shapiro.test(g_prova$residuals)
plot(g_prova, which=1, pch=16)
plot(g_prova, which=2, pch=16)

x11()
influencePlot(g_prova, id.method = "identify", sub = "Circle size is proportial to Cook's Distance")

Data=Data[-c(148,149,152),]

g_prova2 = lm(Happiness.Score ~ ., data=Data)
shapiro.test(g_prova$residuals)
x11()
influencePlot(g_prova2, id.method = "identify", sub = "Circle size is proportial to Cook's Distance")


g = lm(Happiness.Score ~ ., data=Data)
summary(g)
shapiro.test(g$residuals)
plot(g, which=1, pch=16)
plot(g, which=2, pch=16)

####### 3 #######

summary(g)
AIC(g)
shapiro.test(g$residuals)
step( g, direction = "backward" , trace = T)
step( g, direction = "backward", k = log(dim(Data)[1]), trace = T)

g2 = update(g, . ~ . - Generosity)
summary(g2)
shapiro.test(g2$residuals)
AIC(g2)

g3 = update(g2, . ~ . - GDP.per.capita)
summary(g3)
shapiro.test(g3$residuals)
AIC(g3)

g4 = update(g3, . ~ . - Corruption)
summary(g4)
shapiro.test(g4$residuals)
AIC(g4)

# il modello g2 è il migliore, studiamo però la correlazione

Data2 = Data
Data2$Happiness.Score = NULL
Data2$Generosity = NULL
X = cor(Data2)
corrplot(cor(X), method='number')

g5 = update(g2, . ~ . - Healthy.life)
summary(g5)
shapiro.test(g5$residuals)
AIC(g5)

g6 = update(g5, . ~ . - Corruption)
summary(g6)
shapiro.test(g6$residuals)
AIC(g6)





####### 4 #######

Data2=import("archive/TEH_World_Happiness_2019_Imputed.xlsx")
table(Data2$Continent)
Data2=Data2[Data2$Continent != "Oceania",]
Data2$Continent=as.factor(Data2$Continent)
plot(Data2$Continent, Data2$Happiness.Score)

tapply(Data2$Happiness.Score , Data2$Continent, function( x ) ( shapiro.test( x )$p ) )
Data2=Data2[Data2$Country != "Haiti",]
Data2=Data2[Data2$Country != "Venezuela",]
tapply(Data2$Happiness.Score , Data2$Continent, function( x ) ( shapiro.test( x )$p ) )
plot(Data2$Continent, Data2$Happiness.Score)

bartlett.test(Data2$Happiness.Score,Data2$Continent)
#leveneTest(Data2$Happiness.Score,Data2$Continent)

# provo boxcox

g = lm(Happiness.Score ~ Continent, data=Data2)
b = boxcox( g, lambda = seq(0.35,1.5,by=0.01) )
best = b$x[ which.max( b$y ) ] 
best

bartlett.test(Data2$Happiness.Score^best,Data2$Continent)
#leveneTest(Data2$Happiness.Score^best,Data2$Continent)

# provo boxcox con america unita

Data3=import("archive/TEH_World_Happiness_2019_Imputed copia.xlsx")
table(Data3$Continent)
Data3=Data3[Data3$Continent != "Oceania",]
Data3$Continent=as.factor(Data3$Continent)
tapply(Data3$Happiness.Score , Data3$Continent, function( x ) ( shapiro.test( x )$p ) )
plot(Data3$Continent, Data3$Happiness.Score)
Data3=Data3[Data3$Country != "Haiti",]
#Data3=Data3[Data3$Country != "Venezuela",]
#Data3=Data3[Data3$Country != "Canada",]
#Data3=Data3[Data3$Country != "Costa Rica",]
tapply(Data3$Happiness.Score , Data3$Continent, function( x ) ( shapiro.test( x )$p ) )
plot(Data3$Continent, Data3$Happiness.Score)

bartlett.test(Data3$Happiness.Score,Data3$Continent)
#leveneTest(Data3$Happiness.Score,Data3$Continent)


g = lm(Happiness.Score ~ Continent, data=Data3)
b = boxcox( g, lambda = seq(0.3,1.5,by=0.01) )
best = b$x[ which.max( b$y ) ] 
best

bartlett.test(Data3$Happiness.Score^best,Data3$Continent)
#leveneTest(Data3$Happiness.Score^best,Data3$Continent)

#andata al 2% col barlett su Data3 (problema è l'america, ma ho meno dati quindi ci sta)


fit = aov(Data2$Happiness.Score ~ Data2$Continent)
summary(fit)

summary(g)

#vediamo se c'è differenza tra asia america ed europa

Data4=import("archive/TEH_World_Happiness_2019_Imputed copia.xlsx")
Data4=Data4[Data4$Continent != "Oceania",]
Data4=Data4[Data4$Continent != "Africa",]
table(Data4$Continent)
Data4$Continent=as.factor(Data4$Continent)
tapply(Data4$Happiness.Score , Data4$Continent, function( x ) ( shapiro.test( x )$p ) )
plot(Data4$Continent, Data4$Happiness.Score)
Data4=Data4[Data4$Country != "Haiti",]
tapply(Data4$Happiness.Score , Data4$Continent, function( x ) ( shapiro.test( x )$p ) )
plot(Data4$Continent, Data4$Happiness.Score)

bartlett.test(Data4$Happiness.Score,Data4$Continent)

fit = aov(Data4$Happiness.Score ~ Data4$Continent)
summary(fit)

Data4$Continent=relevel(Data4$Continent, ref="Asia")
g = lm(Happiness.Score ~ Continent, data=Data4)
summary(g)

#vediamo se c'è differenza tra america ed europa

Data5=import("archive/TEH_World_Happiness_2019_Imputed copia.xlsx")
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

bartlett.test(Data5$Happiness.Score,Data5$Continent)

fit = aov(Data5$Happiness.Score ~ Data5$Continent)
summary(fit)

g = lm(Happiness.Score ~ Continent, data=Data5)
summary(g)
