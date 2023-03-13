#install.packages(dplyr)
#install.packages(ggplot2)
#install.packages(corrplot)

library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)

HCI <- read_excel("HCI Covid.xlsx")

colnames(HCI)<- c("Country", "Code", "Region", "NIG", "IG", "PSA5", "EYS", "HTS", "LAYS", "FCU5NS", "ASR", "HCI_L", "HCI", "HCI_U", "CovidD")


View(HCI)
str(HCI)

#Todas as variáveis quantitativas para facilitar análise
Var_num <- HCI[-1:-5]


mean <- sapply(Var_num, mean)
dp <- sapply(Var_num, sd)
min <- sapply(Var_num, min)
max <- sapply(Var_num, max)
med <- sapply(Var_num, median)
table <- rbind (min, mean, med, max, dp)

View(table)

#Como 'Fraction of children under 5 not stunted' tem muitos NA's, dificulta a análise
Var_num <- Var_num[-5]
View(Var_num)

#dotplot de cada variável quant.
par(mfrow = c(2,5))
plot(Var_num$PSA5, xlab = "observação", ylab ="", main = "Probability of survival to age 5")
plot(Var_num$EYS, xlab = "observação",ylab ="", main = "Expected years of school")
plot(Var_num$HTS, xlab = "observação",ylab ="", main = "Harmonized test scores")
plot(Var_num$LAYS, xlab = "observação",ylab ="", main = "Learning - adjusted years of school")
plot(Var_num$ASR, xlab = "observação",ylab ="", main = "Adult survival rate")
plot(Var_num$HCI_L, xlab = "observação",ylab ="", main = "Lower bound Human Capital Index")
plot(Var_num$HCI, xlab = "observação",ylab ="", main = "Human Capital Index")
plot(Var_num$HCI_U, xlab = "observação",ylab ="", main = "Upper bound Human Capital Index")
plot(Var_num$CovidD, xlab = "observação",ylab ="", main = "Covid deaths per 100.000 people")

#boxplot de cada variável quant.
dev.off()
par(mfrow = c(2,5))
boxplot(Var_num$PSA5, xlab = "", main = "Probability of survival to age 5")
boxplot(Var_num$EYS, xlab = "", main = "Expected years of school")
boxplot(Var_num$HTS, xlab = "", main = "Harmonized test scores")
boxplot(Var_num$LAYS, xlab = "", main = "Learning - adjusted years of school")
boxplot(Var_num$ASR, xlab = "", main = "Adult survival rate")
boxplot(Var_num$HCI_L, xlab = "", main = "Lower bound Human Capital Index")
boxplot(Var_num$HCI, xlab = "", main = "Human Capital Index")
boxplot(Var_num$HCI_U, xlab = "", main = "Upper bound Human Capital Index")
boxplot(Var_num$CovidD, xlab = "", main = "Covid deaths per 100.000 people")

#histograma de cada variável quant.
dev.off()
par(mfrow =c(2,5))
hist(Var_num$PSA5, xlab = "", main = "Probability of survival to age 5")
hist(Var_num$EYS, xlab = "", main = "Expected years of school")
hist(Var_num$HTS, xlab = "", main = "Harmonized test scores")
hist(Var_num$LAYS, xlab = "", main = "Learning - adjusted years of school")
hist(Var_num$ASR, xlab = "", main = "Adult survival rate")
hist(Var_num$HCI_L, xlab = "", main = "Lower bound Human Capital Index")
hist(Var_num$HCI, xlab = "", main = "Human Capital Index")
hist(Var_num$HCI_U, xlab = "", main = "Upper bound Human Capital Index")
hist(Var_num$CovidD, xlab = "", main = "Covid deaths per 100.000 people")


#qqplot de cada variável
dev.off()
par(mfrow =c(2,5))
qqnorm(Var_num$PSA5, xlab = "", main = "Probability of survival to age 5")
qqline(Var_num$PSA5, xlab = "", main = "Probability of survival to age 5", col = "red" , lwd = 2)

qqnorm(Var_num$EYS, xlab = "", main = "Expected years of school")
qqline(Var_num$EYS, xlab = "", main = "Expected years of school", col = "green" , lwd = 2)

qqnorm(Var_num$HTS, xlab = "", main = "Harmonized test scores")
qqline(Var_num$HTS, xlab = "", main = "Harmonized test scores", col = "blue" , lwd = 2)

qqnorm(Var_num$LAYS, xlab = "", main = "Learning - adjusted years of school")
qqline(Var_num$LAYS, xlab = "", main = "Learning - adjusted years of school", col = "pink" , lwd = 2)

qqnorm(Var_num$ASR, xlab = "", main = "Adult survival rate")
qqline(Var_num$ASR, xlab = "", main = "Adult survival rate", col = "red" , lwd = 2)

qqnorm(Var_num$HCI_L, xlab = "", main = "Lower bound Human Capital Index")
qqline(Var_num$HCI_L, xlab = "", main = "Lower bound Human Capital Index", col = "orange" , lwd = 2)

qqnorm(Var_num$HCI, xlab = "", main = "Human Capital Index")
qqline(Var_num$HCI, xlab = "", main = "Human Capital Index", col = "gray" , lwd = 2)

qqnorm(Var_num$HCI_U, xlab = "", main = "Upper bound Human Capital Index")
qqline(Var_num$HCI_U, xlab = "", main = "Upper bound Human Capital Index", col = "yellow" , lwd = 2)

qqnorm(Var_num$CovidD, xlab = "", main = "Covid deaths per 100.000 people")
qqline(Var_num$CovidD, xlab = "", main = "Covid deaths per 100.000 people", col = "purple" , lwd = 2)



#Tabela de correlação
par(mfrow = c(1,1))
corrplot(cor(Var_num), method = "number")

#Tabela de correlação (mapa de calor)
corrplot(cor(Var_num), method = "color")




#Scatterplot bivariável

ggplot(HCI, aes(x = PSA5, y = CovidD)) +
    geom_point() + 
    labs(x = "Probabilidade de sobreviver até 5 anos", y = "Covid deaths per 100.000 people")

ggplot(HCI, aes(x = EYS, y = CovidD)) +
  geom_point() + 
  labs(x = "Expected years of school", y = "Covid deaths per 100.000 people")

ggplot(HCI, aes(x = HTS, y = CovidD)) +
  geom_point() + 
  labs(x = "Harmonized test scores", y = "Covid deaths per 100.000 people")

ggplot(HCI, aes(x = LAYS, y = CovidD)) +
  geom_point() + 
  labs(x = "Learning - adjusted years of school", y = "Covid deaths per 100.000 people")

ggplot(HCI, aes(x = ASR, y = CovidD)) +
  geom_point() + 
  labs(x = "Adult survival rate", y = "Covid deaths per 100.000 people")

ggplot(HCI, aes(x = HCI, y = CovidD)) +
  geom_point() + 
  labs(x = "Human Capital Index", y = "Covid deaths per 100.000 people")



##### Verificando a influência da variável renda nas variáveis quantitativas


(ggplot(HCI, aes (x = Code, y = PSA5)) + 
    geom_point() + 
    facet_wrap("~IG") + 
    scale_x_discrete(labels = NULL, breaks = NULL) + 
    labs(x = "", y = "Probability of survival to age 5"))

(ggplot(HCI, aes (x = Code, y = EYS)) + 
    geom_point() + 
    facet_wrap("~IG") + 
    scale_x_discrete(labels = NULL, breaks = NULL) + 
    labs(x = "", y = "Expected years of school"))

(ggplot(HCI, aes (x = Code, y = HTS)) + 
    geom_point() + 
    facet_wrap("~IG") + 
    scale_x_discrete(labels = NULL, breaks = NULL) + 
    labs(x = "", y = "Harmonized test scores"))

(ggplot(HCI, aes (x = Code, y = LAYS)) + 
    geom_point() + 
    facet_wrap("~IG") + 
    scale_x_discrete(labels = NULL, breaks = NULL) + 
    labs(x = "", y = "Learning - adjusted years of school"))

(ggplot(HCI, aes (x = Code, y = ASR)) + 
    geom_point() + 
    facet_wrap("~IG") + 
    scale_x_discrete(labels = NULL, breaks = NULL) + 
    labs(x = "", y = "Adult survival rate"))

(ggplot(HCI, aes (x = Code, y = HCI_L)) + 
    geom_point() + 
    facet_wrap("~IG") + 
    scale_x_discrete(labels = NULL, breaks = NULL) + 
    labs(x = "", y = "Lower bound Human Capital Index"))

(ggplot(HCI, aes (x = Code, y = HCI)) + 
    geom_point() + 
    facet_wrap("~IG") + 
    scale_x_discrete(labels = NULL, breaks = NULL) + 
    labs(x = "", y = "Human Capital Index"))

(ggplot(HCI, aes (x = Code, y = HCI_U)) + 
    geom_point() + 
    facet_wrap("~IG") + 
    scale_x_discrete(labels = NULL, breaks = NULL) + 
    labs(x = "", y = "Upper bound Human Capital Index"))

(ggplot(HCI, aes (x = Code, y = CovidD)) + 
    geom_point() + 
    facet_wrap("~IG") + 
    scale_x_discrete(labels = NULL, breaks = NULL) + 
    labs(x = "", y = "Covid deaths per 100.000 people"))


#boxplot
(ggplot(HCI, aes (x = IG, y = PSA5)) + 
    geom_boxplot() + 
    labs(x = "", y = "Probability of survival to age 5"))

(ggplot(HCI, aes (x = IG, y = EYS)) + 
    geom_boxplot() + 
    labs(x = "", y = "Expected years of school"))

(ggplot(HCI, aes (x = IG, y = HTS)) + 
    geom_boxplot() + 
    labs(x = "", y = "Harmonized test scores"))

(ggplot(HCI, aes (x = IG, y = LAYS)) + 
    geom_boxplot() + 
    labs(x = "", y = "Learning - adjusted years of school"))

(ggplot(HCI, aes (x = IG, y = ASR)) + 
    geom_boxplot() + 
    labs(x = "", y = "Adult survival rate"))

(ggplot(HCI, aes (x = IG, y = HCI_L)) + 
    geom_boxplot() + 
    labs(x = "", y = "Lower bound Human Capital Index"))

(ggplot(HCI, aes (x = IG, y = HCI)) + 
    geom_boxplot() + 
    labs(x = "", y = "Human Capital Index"))

(ggplot(HCI, aes (x = IG, y = HCI_U)) + 
    geom_boxplot() + 
    labs(x = "", y = "Upper bound Human Capital Index"))

 
(ggplot(HCI, aes (x = IG, y = CovidD)) + 
      geom_boxplot() + 
      labs(x = "", y = "Covid deaths per 100.000 people"))


#Correlação
par(mfrow = c(1,2))
HCI %>% 
  filter(IG == "Low income") %>% 
  select(where(is.numeric)) %>% 
  cor() %>% 
  corrplot(method = "number", type = "upper", mar = c(0,0,2,0),
           title = "Correlation - Low Income", bg = "orange")


HCI %>% 
  filter(IG == "High income") %>% 
  select(where(is.numeric)) %>% 
  cor() %>% 
  corrplot(method = "number", type = "upper", mar = c(0,0,2,0),
           title = "Correlation - High Income", bg = "orange")


