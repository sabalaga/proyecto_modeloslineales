## ----setup, include = FALSE----------------------------------------------------------------------------------------------------------------------------------

knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)

library(tidyverse)

#base <- read_delim("20200706_Matrícula_Ed_Superior_2019_PUBL_MRUN.csv", 
#                   ";", escape_double = FALSE, trim_ws = TRUE)

#saveRDS(base, "matricula_2019.rds")

#zip(zipfile = 'matricula_2019Zip', files = 'matricula_2019.rds')

base <- readRDS("matricula_2019.rds")



## ------------------------------------------------------------------------------------------------------------------------------------------------------------

# glimpse(base)

base_f <- base %>% 
  group_by(codigo_unico) %>% 
  summarise(tipo_inst_1 = first(tipo_inst_1),
            tipo_inst_2 = first(tipo_inst_2),
            tipo_inst_3 = first(tipo_inst_3),
            cod_inst = first(cod_inst),
            nomb_inst = first(nomb_inst),
            cod_sede = first(cod_sede),
            nomb_sede = first(nomb_sede),
            cod_carrera = first(cod_carrera),
            nomb_carrera = first(nomb_carrera),
            modalidad = first(modalidad),
            jornada = first(jornada),
            version = first(version),
            tipo_plan_carr = first(tipo_plan_carr),
            dur_estudio_carr = first(dur_estudio_carr),
            dur_proceso_tit = first(dur_proceso_tit),
            dur_total_carr = first(dur_total_carr),
            region_sede = first(region_sede),
            provincia_sede = first(provincia_sede),
            comuna_sede = first(comuna_sede),
            nivel_global = first(nivel_global),
            nivel_carrera_1 = first(nivel_carrera_1),
            nivel_carrera_2 = first(nivel_carrera_2),
            requisito_ingreso = first(requisito_ingreso),
            vigencia_carrera = first(vigencia_carrera),
            valor_matricula = first(valor_matricula),
            valor_arancel = first(valor_arancel),
            codigo_demre = first(codigo_demre),
            area_conocimiento = first(area_conocimiento),
            oecd_area = first(oecd_area),
            oecd_subarea = first(oecd_subarea),
            area_carrera_generica = first(area_carrera_generica),
            acreditada_carr = first(acreditada_carr),
            acreditada_inst = first(acreditada_inst),
            acre_inst_desde_hasta = first(acre_inst_desde_hasta),
            acre_inst_anio  = first(acre_inst_anio ),
            costo_proceso_titulacion = first(costo_proceso_titulacion),
            costo_obtencion_titulo_diploma = first(costo_obtencion_titulo_diploma),
            forma_ingreso = first(forma_ingreso),
            n_estudiantes = n())



## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# Estadísticos resumen
summary(base_f$valor_arancel)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# Transformaciones 
base_f <- base_f %>% filter(valor_arancel != 0, # filtrar casos = 0
                            valor_arancel <= 10000000) # filtrar casos > 10MM

base_f <- base_f %>% 
  mutate(region_sede_d = as_factor(if_else(region_sede == "Metropolitana", 1, 0)),
         dur_estudio_carr_m = as_factor(dur_estudio_carr),
         dur_estudio_carr_m = fct_other(dur_estudio_carr_m, 
                                        drop = c(14, 16, 20, 24), 
                                        other_level = "Más de 12 semestres"),
         tipo_inst_1 = factor(tipo_inst_1),
         tipo_inst_2 = factor(tipo_inst_2))



## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# Histograma
ggplot(base_f, aes(x = valor_arancel)) +
  geom_histogram(fill = "#69b3a2", color = "#e9ecef", alpha = 0.9) +
  scale_x_continuous(labels = scales::label_dollar(big.mark = ".", decimal.mark = ",")) + 
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
  labs(title = "Gráfico 1: Distribución variable valor arancel",
       x = "Valor arancel",
       y = "Número de casos")

#ggplot(base_f, aes(x = log(valor_arancel))) +
#  geom_histogram(fill = "#69b3a2", color = "#e9ecef", alpha = 0.9) +
#  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
#  labs(title = "Distribución variable valor arancel - Logaritmo",
#       x = "Valor arancel",
#       y = "Número de casos")



## ------------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot(base_f, aes(x = region_sede_d, y = valor_arancel, fill= region_sede_d)) +
  geom_boxplot() + 
  labs(title = "Gráfico 2: Arancel según región",
       x = "Región",
       y = "Valor arancel") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::label_dollar(big.mark = ".", decimal.mark = ","))
  



## ------------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot(base_f, aes(x = dur_estudio_carr_m, y = valor_arancel, fill= dur_estudio_carr_m)) +
  geom_boxplot() + 
  labs(title = "Gráfico 3: Arancel según duración carrera",
       x = "Semestres",
       y = "Valor arancel") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::label_dollar(big.mark = ".", decimal.mark = ","))
  



## ------------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot(base_f, aes(x = tipo_inst_2, y = valor_arancel, fill= tipo_inst_2)) +
  geom_boxplot() + 
  labs(title = "Gráfico 4: Arancel según tipo de institución",
       x = "Tipo de institución",
       y = "Valor arancel") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::label_dollar(big.mark = ".", decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 40))



## ------------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot(base_f, aes(x = oecd_area, y = valor_arancel, fill= oecd_area)) +
  geom_boxplot() + 
  labs(title = "Gráfico 5: Arancel según área",
       x = "Área",
       y = "Valor arancel") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::label_dollar(big.mark = ".", decimal.mark = ",")) +
  coord_flip()



## ------------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot(base_f, aes(x = as.factor(acre_inst_anio), y = valor_arancel, fill = as.factor(acre_inst_anio))) +
  geom_boxplot() + 
  labs(title = "Gráfico 6: Arancel según años de acreditación",
       x = "Tipo de institución",
       y = "Años de acreditación") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::label_dollar(big.mark = ".", decimal.mark = ","))



## ------------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot(base_f, aes(x = jornada, y = valor_arancel, fill = jornada)) +
  geom_boxplot() + 
  labs(title = "Gráfico 7: Arancel según jornada",
       x = "Jornada",
       y = "Valor arancel") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::label_dollar(big.mark = ".", decimal.mark = ","))





## ------------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot(base_f, aes(x = nivel_carrera_2, y = valor_arancel, fill = nivel_carrera_2)) +
  geom_boxplot() + 
  labs(title = "Gráfico 8: Arancel según nivel carrera",
       x = "Jornada",
       y = "Valor arancel") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::label_dollar(big.mark = ".", decimal.mark = ","))



## ------------------------------------------------------------------------------------------------------------------------------------------------------------

muestra_base_f <- sample_n(base_f, 500) %>% drop_na()

modelo1 <- lm(log(valor_arancel) ~ region_sede_d + dur_estudio_carr_m + oecd_area + acre_inst_anio +  jornada + tipo_inst_2 + nivel_carrera_2, data = muestra_base_f)
drop1(modelo1, test = "F")
drop1(update(modelo1, ~ . -dur_total_carr), test = "F")
drop1(update(modelo1, ~ . -dur_total_carr - jornada), test = "F")
modelo1 <- lm(log(valor_arancel) ~ region_sede_d + dur_estudio_carr_m + tipo_inst_1 + oecd_area + acre_inst_anio  + tipo_inst_2 + nivel_carrera_2, data = muestra_base_f)

summary(modelo1)


