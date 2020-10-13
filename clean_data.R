
pobreza <- read.delim("pobreza.txt", sep = "\t")
shapiro.test(pobreza$dato)
library(WDI)
library(tidyverse)
WDIsearch("gini")

gini <- WDI(indicator = "SI.POV.GINI", 
        country = c("ARG","BOL", "BRA", "CHL","COL","CRI", 
                    "ECU", "SLV",	"GTM","HND","NIC","DOM",
                    "MEX",	"PAN","PRY","PER","URY","VEN"),2014, 2018, extra = F)

WDIsearch("adolescent fertility rate")

ferti <- WDI(indicator = "SP.ADO.TFRT", 
             country = c("ARG","BOL", "BRA", "CHL","COL","CRI", 
                         "ECU", "SLV",	"GTM","HND","NIC","DOM",
                         "MEX",	"PAN","PRY","PER","URY","VEN"),2014, 2018, extra = F)

WDIsearch("poverty")

pobreza <- WDI(indicator = "SI.POV.NAHC", 
             country = c("ARG","BOL", "BRA", "CHL","COL","CRI", 
                         "ECU", "SLV",	"GTM","HND","NIC","DOM",
                         "MEX",	"PAN","PRY","PER","URY","VEN"),2014, 2018, extra = F)

WDIsearch("enrollment")
#"School enrollment, secondary (% net)"  
matricula <- WDI(indicator = "SE.SEC.NENR", 
       country = c("ARG","BOL", "BRA", "CHL","COL","CRI", 
                   "ECU", "SLV",	"GTM","HND","NIC","DOM",
                   "MEX",	"PAN","PRY","PER","URY","VEN"),2014, 2018, extra = F)

WDIsearch("investment")
#Foreign direct investment, net inflows (% of GDP)
inv_ext <- WDI(indicator = "BX.KLT.DINV.WD.GD.ZS", 
       country = c("ARG","BOL", "BRA", "CHL","COL","CRI", 
                   "ECU", "SLV",	"GTM","HND","NIC","DOM",
                   "MEX",	"PAN","PRY","PER","URY","VEN"),2014, 2018, extra = F)

WDIsearch("domestic credit")
#Domestic credit provided by financial sector (% of GDP)

credito <- WDI(indicator = "FS.AST.DOMS.GD.ZS", 
               country = c("ARG","BOL", "BRA", "CHL","COL","CRI", 
                           "ECU", "SLV",	"GTM","HND","NIC","DOM",
                           "MEX",	"PAN","PRY","PER","URY","VEN"),2014, 2018, extra = F)



base <- left_join(gini, ferti, by = c("iso2c","country", "year"))
base <- left_join(base, pobreza, by = c("iso2c","country", "year"))
base <- left_join(base, matricula, by = c("iso2c","country", "year"))

base <- base %>% select(c(iso2c, country, year, everything())) %>% rename(gini = SI.POV.GINI, pobreza = SI.POV.NAHC,
                                                                          matricula = SE.SEC.NENR, fertilidad = SP.ADO.TFRT )


base2 <- base %>%
  group_by(country) %>%
  arrange(year) %>% na.omit() %>% 
  summarise(year = last(year), gini=last(gini), fertilidad = last(fertilidad), 
            matricula = last(matricula), pobreza = last(pobreza), pais = last(iso2c) ) %>% ungroup()


base2 %>% ggplot(aes(gini, pobreza, label = pais)) + geom_point() +geom_text(hjust=0.15, vjust=0.1)
base2 %>% ggplot(aes(fertilidad, pobreza, label = pais)) + geom_point() +geom_text(hjust=0.15, vjust=0.1)
base2 %>% ggplot(aes(matricula, pobreza, label = pais)) + geom_point() +geom_text(hjust=0.15, vjust=0.1)



modelo1 <- lm(pobreza ~ gini + fertilidad + matricula, data = base2)
summary(modelo1)
