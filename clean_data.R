
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
#Ratio of female to male labor force participation rate (%) (national estimate)

ratio_mh <- WDI(indicator = "SL.TLF.CACT.FM.NE.ZS", 
                     country = c("ARG","BOL", "BRA", "CHL","COL","CRI", 
                                 "ECU", "SLV",	"GTM","HND","NIC","DOM",
                                 "MEX",	"PAN","PRY","PER","URY","VEN"),2014, 2018, extra = F)

#Children out of school (% of primary school age)
ninos_fc <- WDI(indicator = "SE.PRM.UNER.ZS", 
                country = c("ARG","BOL", "BRA", "CHL","COL","CRI", 
                            "ECU", "SLV",	"GTM","HND","NIC","DOM",
                            "MEX",	"PAN","PRY","PER","URY","VEN"),2014, 2018, extra = F)



  
base <- left_join(gini, ferti, by = c("iso2c","country", "year"))
base <- left_join(base, pobreza, by = c("iso2c","country", "year"))
base <- left_join(base, matricula, by = c("iso2c","country", "year"))
base <- left_join(base, inv_ext, by = c("iso2c","country", "year"))
base <- left_join(base, credito, by = c("iso2c","country", "year"))
base <- left_join(base, ratio_mh, by = c("iso2c","country", "year"))
base <- left_join(base, ninos_fc, by = c("iso2c","country", "year"))

base <- base %>% select(c(iso2c, country, year, everything())) %>% rename(gini = SI.POV.GINI, pobreza = SI.POV.NAHC,
                                                                          matricula = SE.SEC.NENR, fertilidad = SP.ADO.TFRT,
                                                                          inv_ext = BX.KLT.DINV.WD.GD.ZS,
                                                                          credito = FS.AST.DOMS.GD.ZS, 
                                                                          ratio_mh = SL.TLF.CACT.FM.NE.ZS, 
                                                                          ninos_fc = SE.PRM.UNER.ZS)


base2 <- base %>%
  group_by(country) %>%
  arrange(year) %>% na.omit() %>% 
  summarise(year = last(year), gini=last(gini), fertilidad = last(fertilidad), 
            matricula = last(matricula), pobreza = last(pobreza), inv_ext = last(inv_ext), 
            credito = last(credito), ratio_mh = last(ratio_mh), ninos_fc = last(ninos_fc),
            pais = last(iso2c) ) %>% ungroup()


p1 <- base2 %>% ggplot(aes(gini, pobreza, label = pais)) + geom_point() +geom_text(hjust=0.15, vjust=0.2)
p2 <- base2 %>% ggplot(aes(fertilidad, pobreza, label = pais)) + geom_point() +geom_text(hjust=0.15, vjust=0.2)
p3 <- base2 %>% ggplot(aes(matricula, pobreza, label = pais)) + geom_point() +geom_text(hjust=0.15, vjust=0.2)
p4 <- base2 %>% ggplot(aes(inv_ext, pobreza, label = pais)) + geom_point() +geom_text(hjust=0.15, vjust=0.2)
p5 <- base2 %>% ggplot(aes(credito, pobreza, label = pais)) + geom_point() +geom_text(hjust=0.15, vjust=0.2)
p6 <- base2 %>% ggplot(aes(ratio_mh, pobreza, label = pais)) + geom_point() +geom_text(hjust=0.15, vjust=0.2)
p7 <- base2 %>% ggplot(aes(ninos_fc, pobreza, label = pais)) + geom_point() +geom_text(hjust=0.15, vjust=0.2)


modelo1 <- lm(pobreza ~ gini + fertilidad + matricula + inv_ext + credito + ratio_mh + ninos_fc, data = base2)
summary(modelo1)
drop1(modelo1, test = "F")
drop1(update(modelo1, ~ . -gini), test = "F")
drop1(update(modelo1, ~ . -gini -ninos_fc), test = "F")
drop1(update(modelo1, ~ . -gini -ninos_fc -inv_ext), test = "F")
drop1(update(modelo1, ~ . -gini -ninos_fc -inv_ext -ratio_mh), test = "F")
