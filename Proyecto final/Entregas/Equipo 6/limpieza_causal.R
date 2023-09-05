# Libraries
library(dplyr)
library(ggplot2)
library(RCT)
library(purrr)
library(tidyr)
library(fastDummies)
library(tidymodels)
library(Matrix)
library(grf)
library(stringr)
library(forcats)
# Directorio
setwd("/Users/adrian_martinez/Dropbox/Maestría/Maestría Clases/Segundo_Semestre/Proyecto_Final_Eco_Compu/Progresa-Targeting")

# Read data
data <- readRDS("./base")

data <- data %>% 
  select(-contains("folio"), -id_trabajo, -ubica_geo, -sinco,
         -scian, -factor, -upm, -pareja_hog, -ubica_geo)

###############################################################################
# Análisis de variables "missing"
# Remplazar valores, etc
missings <- map_dbl(data %>% select_all(), 
                    ~100*sum(is.na(.)/nrow(data)))

(missings <- missings[missings>0])
missings <- data.frame("pct_miss" = missings, 
                          "variable" = names(missings)) 
missings <- missings %>% 
  arrange(-pct_miss)

missings <- missings %>% 
  filter(! variable %in% remove)

aux <- data %>% 
  summarise_at(missings$variable, list(min = min), na.rm = TRUE) %>% 
  pivot_longer(everything(), names_to = "variables", values_to = "valores")
  
names <- colnames(data)[grepl("acc_alim", colnames(data))]

data <- data %>% 
  mutate(across(names, ~ replace_na(., 0)))

data <- data %>% 
  select(-starts_with("anio"))

data <- data %>% 
  rowwise() %>%
  mutate(habito = sum(c_across(starts_with("habito")), na.rm = TRUE)) %>% 
  select(-starts_with("habito_")) %>% 
  ungroup()

data <- data %>% 
  rowwise() %>%
  mutate(segvol = sum(c_across(starts_with("segvol")), na.rm = TRUE)) %>% 
  select(-starts_with("segvol_")) %>% 
  ungroup()

data <- data %>% 
  rowwise() %>%
  mutate(segvol = sum(c_across(starts_with("inscr")), na.rm = TRUE)) %>% 
  select(-starts_with("inscr_")) %>% 
  ungroup()
  
data <- data %>% 
  drop_na(alfabetism)

data <- data %>% 
  select(-contains("licon"), -contains("dicon"))

data <- data %>% 
  mutate(act_pnea2 = replace_na(act_pnea2, 0), 
         tipoact2 = replace_na(tipoact2, 0))

data <- data %>% 
  select(-pagoaten_5, -num_dueno1, -num_due2, -hog_dueno1, -hog_dueno2, -min_7,
         -tipoact2, -lugar, -noatenc_15, -noatenc_16, -starts_with("causa"))

data <- data %>% 
  mutate(across(starts_with("num_"), ~ replace_na(., 0)))

data <- data %>% 
  mutate(across(starts_with("min_"), ~ replace_na(., 0)))

data <- data %>% 
  mutate(across(starts_with("bano_"), ~ replace_na(., 0)))

data <- data %>% 
  mutate(disc1 = if_else(disc1 == "&" | disc1 == "8", 0, 1))

data <- data %>% 
  mutate(across(starts_with("disc"), ~ as.numeric(.))) %>% 
  mutate(across(starts_with("disc"), ~ replace_na(., 0))) %>% 
  mutate(across(starts_with("disc"), ~ ifelse(. != 0, 1, 0)))

data <- data %>% 
  mutate(disc_tot = disc1 + disc2 + disc3 + disc4 + disc5 + disc6 + disc7) %>% 
  select(-starts_with("disc"), disc_tot)

data <- data %>% 
  select(-starts_with("usotiempo"))

data <- data %>% 
  select(-starts_with("causa"), -starts_with("er"))

data <- data %>% 
  mutate(across(starts_with("medtrab"), ~ replace_na(., 0))) %>% 
  mutate(across(starts_with("medtrab"), ~ if_else(. != 0, 1, 0)))
 
data <- data %>% 
  mutate(across(starts_with("hijo"), ~ replace_na(., 0)))

data <- data %>% 
  mutate(act_pnea1 = replace_na(act_pnea1, 0))

data <- data %>% 
  select(-conf_pers)

data <- data %>% 
  mutate(across(starts_with("forma"), ~ replace_na(., 0)))

data <- data %>% 
  mutate(diabetes = case_when(diabetes == 1 ~ 1, diabetes == 2 ~ 0,
         TRUE ~ 3), pres_alta = case_when(pres_alta == 1 ~ 1, 
                                          pres_alta == 2 ~ 0 ,
                                          TRUE ~ 3), 
         peso = case_when(peso == 1 ~ 1, peso == 2 ~ 0, 
                          TRUE ~ 3))

data <- data %>% 
  mutate(across(starts_with("hor_"), ~ replace_na(., 0)))

data <- data %>% 
  select(-starts_with("hog_dueno"))

data <- data %>% 
  mutate(across(starts_with("inst_"), ~ replace_na(., 0)))


data <- data %>% 
  mutate(across(starts_with("noatenc_"), ~ replace_na(., "No aplica")))

data <- data %>% 
  mutate(across(starts_with("norecib_"), ~ replace_na(., "No Aplica")))

data <- data %>% 
  mutate(across(starts_with("pagoaten_"), ~ replace_na(., "No Aplica")))

data <- data %>% 
  mutate(across(starts_with("redsoc_"), ~ replace_na(., 0)))

data <- data %>% 
  select(-starts_with("sermed"))

data <- data %>% 
  select(-starts_with("razon"), -starts_with("tipoact"), -starts_with("servmed"))

# Análisis de variables "missing"
data <- data %>% 
  select(-otro_pago, -hablaesp, -tiene_c, ) %>% 
  mutate(lenguaind = if_else(is.na(lenguaind), 0, 1), 
         pago_viv = if_else(is.na(pago_viv), 0, as.double(pago_viv)))

missings <- map_dbl(data %>% select_all(), 
                    ~100*sum(is.na(.)/nrow(data)))

(missings <- missings[missings>0])
missings <- data.frame("pct_miss" = missings, 
                       "variable" = names(missings)) 
missings <- missings %>% 
  arrange(-pct_miss)

remove <- missings %>% 
   filter(pct_miss >= 98) %>% 
   pull(variable)

data <- data %>% 
   select(-{{remove}})

obs_filter <- missings %>% 
  filter(pct_miss <= 5) %>% 
  pull(variable)

data <- data %>% 
  drop_na({{obs_filter}})
  
missings <- missings %>% 
  filter(!variable %in% {{remove}})

missing_cols <- map_dfc(missings$variable, function(x){
  data.frame(nom_var = ifelse(is.na(data[, x]), 1, 0))
})

colnames(missing_cols) <- paste0(colnames(missing_cols), "_missing")

data <- data %>% 
  bind_cols(missing_cols)

data <- data %>% 
  mutate_all(~replace(., is.na(.), 0))

data <- data %>% 
  select(-numren.x, -numren.y, -numren.y_missing, -parentesco, -madre_hog, 
         -madre_id, -padre_hog, -padre_id, -conyuge_id, -prob_anio, -prob_mes,
  )

data <- data %>% 
  mutate_all(funs(gsub("&", "0", .))) 

######################### 
# Convertir a numeric variables que se guardaron como "char"

a <- lapply(data, class)

data$estim_pago <- as.numeric(data$estim_pago)
data$edad <- as.numeric(data$edad)
data$ss_aa <- as.numeric(data$ss_aa)
data$ss_mm <- as.numeric(data$ss_mm)
data <- data %>% 
  mutate(across(starts_with("hor_"),~as.numeric(.))) %>% 
  mutate(across(starts_with("tsalud1_"),~as.numeric(.))) %>% 
  mutate(across(starts_with("est_"),~as.numeric(.))) %>% 
  mutate(across(starts_with("ing_tri"),~as.numeric(.))) %>% 
  mutate(across(starts_with("htrab_"),~as.numeric(.))) %>% 
  mutate(across(starts_with("hijos_"),~as.numeric(.))) %>% 
  mutate(across(starts_with("propspera"),~as.numeric(.)))
data <- data %>% 
  mutate(across(starts_with("min_"),~as.numeric(.)))
data$mm_lug <- as.numeric(data$mm_lug)
data$hh_lug <- as.numeric(data$hh_lug)
data$mm_esp <- as.numeric(data$mm_esp)
data$hh_esp <- as.numeric(data$hh_esp)
data <- data %>% 
  mutate(across(starts_with("hijos_"),~as.numeric(.))) %>% 
  mutate(across(starts_with("num_"),~as.numeric(.)))
data$antiguedad <- as.numeric(data$antiguedad)
data$num_cuarto <- as.numeric(data$num_cuarto)
data$cuart_dorm <- as.numeric(data$cuart_dorm)
data$num_trabaj <- as.numeric(data$num_trabaj)
data$renta <- as.numeric(data$renta)
data$htrab <- as.numeric(data$htrab)
data$disc_tot <- as.numeric(data$disc_tot)
data$prospera <- as.numeric(data$prospera)
data$informal <- as.numeric(data$informal)
data$factor <- as.numeric(data$factor)
data$pago_viv <- as.numeric(data$pago_viv)
data$id_2 <- as.numeric(data$id_2)
data$preocupacion_comida <- as.numeric(data$preocupacion_comida)
data$menor_secundaria <- as.numeric(data$menor_secundaria)
data$focos_ahor <- as.numeric(data$focos_ahor)


data <- data %>% 
  mutate(across(contains("missing"), ~ as.numeric(.)))

#Dummies
data <- data  %>% 
  select(-contains("pagoaten_")) %>% 
  fastDummies::dummy_cols(remove_selected_columns = TRUE)
###############################################################################

# Dividir la base de datos en entrenamiento y valdiación
data_validacion <- treatment_assign(data = data, share_control = 0.8,
                                    n_t = 1, seed = 1996, key = "id_2",
                                    strata_varlist = "id_2")

data_validacion <- data_validacion$data %>% 
  ungroup() %>% 
  select(treat)

data$tratamiento <- data_validacion$treat

save(data, file = "./data_final.RData")

###############################################################################
# Estimación de los modelos
load("./data_final.RData")

# Creamos la base de entrenamiento
data_entrenamiento <- data %>% 
  filter(tratamiento == 0) %>% 
  select(-id_2)

# Creamos la base de validación 
data_validacion <- data %>% 
  filter(tratamiento == 1) %>% 
  select(-id_2)

# Eliminamos espacios en los nombres
colnames(data_entrenamiento)[grepl(" ", colnames(data_entrenamiento))] <- str_replace(colnames(data)[grepl(" ", colnames(data_entrenamiento))], " ", "")
colnames(data_validacion)[grepl(" ", colnames(data_validacion))] <- str_replace(colnames(data)[grepl(" ", colnames(data_validacion))], " ", "")


# Modelo 1
# Vector con el outcome
Y <-  data_entrenamiento %>% 
  pull(informal)

# Matriz de covariables
X <- data_entrenamiento %>% 
  select(-informal, 
         -preocupacion_comida, -menor_secundaria, 
         -prospera, -tratamiento) %>% 
  as.matrix()

# Vector con los tratamientos
W <- data_entrenamiento %>% 
  pull(prospera)

# Corremos el causal forest
forest <- causal_forest(X = X, Y = Y, W = W, num.trees = 750)

# Guardar el causal forest para no tener que volverlo a correr
saveRDS(forest, "./causal_forest_1.rds")
  
# Modelo 2
# Vector con el outcome
Y <-  data_entrenamiento %>% 
  pull(preocupacion_comida)

# Corremos el causal forest
forest <- causal_forest(X = X, Y = Y, W = W, num.trees = 750)

# Guardar el causal forest para no tener que volverlo a correr
saveRDS(forest, "./causal_forest_2.rds")

# Modelo 3
# Vector con el outcome
Y <-  data_entrenamiento %>% 
  pull(menor_secundaria)

# Corremos el causal forest
forest <- causal_forest(X = X, Y = Y, W = W, num.trees = 750)

# Guardar el causal forest para no tener que volverlo a correr
saveRDS(forest, "./causal_forest_3.rds")

######### Analisis de predicciones #############
# Leer modelos de Causal forest
forest_1 <- readRDS("causal_forest_1.rds")
forest_2 <- readRDS("causal_forest_2.rds")
forest_3 <- readRDS("causal_forest_3.rds")


ate1 <- average_treatment_effect(forest_1,
                                target.sample = "all", method = "AIPW")

ate2 <- average_treatment_effect(forest_2,
                                 target.sample = "all", method = "AIPW")

ate3 <- average_treatment_effect(forest_3,
                                 target.sample = "all", method = "AIPW")

X_val <- data_validacion %>% 
  select(-informal, 
         -preocupacion_comida, -menor_secundaria, 
         -prospera, -tratamiento) %>% 
  as.matrix()

data_validacion$ate1 <- predict(forest_1, X_val)$predictions
data_validacion$ate2 <- predict(forest_2, X_val)$predictions
data_validacion$ate3 <- predict(forest_3, X_val)$predictions

label_ate1 <- paste0("ATE = ", 
                         round(ate1["estimate"], 6))
label_ate2 <- paste0("ATE = ", 
                     round(ate2["estimate"], 6))
label_ate3 <- paste0("ATE = ", 
                     round(ate3["estimate"], 6))

plot_ate_informal <- ggplot(data = data_validacion, aes(x = ate1)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "navyblue") +
  geom_vline(aes(xintercept = mean(data_validacion$ate1), 
                 color = label_ate1), linetype ="dashed") +
  scale_color_manual(name = "", values = c("red")) +
  labs(x = "ATE Informalidad", y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "bottom")

plot_ate_preocupacion <- ggplot(data = data_validacion, aes(x = ate2)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "navyblue") +
  geom_vline(aes(xintercept = mean(data_validacion$ate2), 
                 color = label_ate2), linetype ="dashed") +
  scale_color_manual(name = "", values = c("red")) +
  labs(x = "ATE Alimentación", y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "bottom")

plot_ate_edu <- ggplot(data = data_validacion, aes(x = ate3)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "navyblue") +
  geom_vline(aes(xintercept = mean(data_validacion$ate3), 
                 color = label_ate3), linetype ="dashed") +
  scale_color_manual(name = "", values = c("red")) +
  labs(x = "ATE Educación", y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(plot_ate_informal, filename = "ate_informal.png")
ggsave(plot_ate_edu, filename = "ate_edu.png")
ggsave(plot_ate_preocupacion, filename = "ate_preocupacion.png")

data_validacion <- data_validacion %>% 
  mutate(decil_informal = ntile(ate1, 10), decil_alim = ntile(ate2, 10),
         decil_secu = ntile(ate3, 10))

itt_ate1 <- impact_eval(data_validacion, 
                        endogenous_vars = "informal", 
                        treatment = "prospera", 
                        heterogenous_vars = "decil_informal")$informal_decil_informal

itt_ate1 <- itt_ate1 %>% 
  dplyr::filter(term != "(Intercept)") %>% 
  mutate(decil_informal = factor(decil_informal, 
                                 levels = c("1","2","3","4",
                                                      "5","6","7","8","9","10")))

decils_ate1 <- data_validacion %>% 
  group_by(decil_informal) %>%
  summarise(media_informal = mean(ate1)) %>% 
  mutate(decil_informal = factor(decil_informal, levels = c("1","2","3","4",
                                                    "5","6","7","8","9","10")))

decils_ate1 <- decils_ate1 %>% 
  left_join(itt_ate1 %>% select(decil_informal, estimate), 
            by = "decil_informal")

decils_ate1 <- decils_ate1 %>% 
  pivot_longer(cols = c(media_informal, estimate))
  
decils_1 <- ggplot(decils_ate1, aes(x = decil_informal, y = value, color = name)) +
  geom_line() +
  geom_point(shape = 21, size = 2) +
  theme_bw()
  
## Para ATE 2
itt_ate2 <- impact_eval(data_validacion, 
                        endogenous_vars = "preocupacion_comida", 
                        treatment = "prospera", 
                        heterogenous_vars = "decil_alim")$preocupacion_comida_decil_alim

itt_ate2 <- itt_ate2 %>% 
  dplyr::filter(term != "(Intercept)") %>% 
  mutate(decil_alim = factor(decil_alim, 
                                 levels = c("1","2","3","4",
                                            "5","6","7","8","9","10")))

decils_ate2 <- data_validacion %>% 
  group_by(decil_alim) %>%
  summarise(media_alim = mean(ate2)) %>% 
  mutate(decil_alim = factor(decil_alim, levels = c("1","2","3","4",
                                                            "5","6","7","8","9","10")))

decils_ate2 <- decils_ate2 %>% 
  left_join(itt_ate2 %>% select(decil_alim, estimate), 
            by = "decil_alim")

decils_ate2 <- decils_ate2 %>% 
  pivot_longer(cols = c(media_alim, estimate))

decils_2 <- ggplot(decils_ate2, aes(x = decil_alim, y = value, color = name)) +
  geom_line() +
  geom_point(shape = 21, size = 2) +
  theme_bw()

## Para ATE 3
itt_ate3 <- impact_eval(data_validacion, 
                        endogenous_vars = "menor_secundaria", 
                        treatment = "prospera", 
                        heterogenous_vars = "decil_secu")$menor_secundaria_decil_secu

itt_ate3 <- itt_ate3 %>% 
  dplyr::filter(term != "(Intercept)") %>% 
  mutate(decil_secu = factor(decil_secu, 
                             levels = c("1","2","3","4",
                                        "5","6","7","8","9","10")))

decils_ate3 <- data_validacion %>% 
  group_by(decil_secu) %>%
  summarise(media_secu = mean(ate3)) %>% 
  mutate(decil_secu = factor(decil_secu, levels = c("1","2","3","4",
                                                    "5","6","7","8","9","10")))

decils_ate3 <- decils_ate3 %>% 
  left_join(itt_ate3 %>% select(decil_secu, estimate), 
            by = "decil_secu")

decils_ate3 <- decils_ate3 %>% 
  pivot_longer(cols = c(media_secu, estimate))

decils3 <- ggplot(decils_ate3, aes(x = decil_secu, y = value, color = name)) +
  geom_line() +
  geom_point(shape = 21, size = 2) +
  theme_bw()

ggsave(decils3, filename = "decils3.png")
ggsave(decils_1, filename = "decils1.png")
ggsave(decils_2, filename = "decils2.png")


#### Importancia de variables
ate1_var_importance <- as.data.frame(variable_importance(forest_1)) %>% 
  rename(importancia = V1)
ate1_var_importance$var <- colnames(X)

# ATE 1
ate1_var_importance <- ate_var_importance %>% 
  arrange(-importancia) 

# Informalidad
# Las variables más importantes fueron tipo contrato, ingreso trimestral total y
# horas trabajadas

# ATE 2
ate2_var_importance <- as.data.frame(variable_importance(forest_2)) %>% 
  rename(importancia = V1) %>% 
  mutate(var = colnames(X)) %>% 
  arrange(-importancia)

# Alimentación
# las variables más importante fueron el número de hijos vivos, horas dedicadas
#al quehacer, hijos muertos.

# ATE 3
ate3_var_importance <- as.data.frame(variable_importance(forest_3)) %>% 
  rename(importancia = V1) %>% 
  mutate(var = colnames(X)) %>% 
  arrange(-importancia)
# Las más importantes fueron hijos sobrevivientes, edad y horas dedicadas al 
# quehacer

# Gráficos de importancia
graph1 <- ate1_var_importance %>%
  arrange(desc(importancia)) %>%
  mutate(importancia = importancia / max(importancia)) %>% 
  dplyr::slice(1:10) %>%
  mutate(variables = fct_reorder(var, importancia)) %>%
  ggplot(data = ., aes(x = as.factor(variables), y = importancia)) +
  geom_col(fill = "royalblue4") +
  ylab("Importancia") +
  xlab("Variables") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5))

graph2 <- ate2_var_importance %>%
  arrange(desc(importancia)) %>%
  mutate(importancia = importancia / max(importancia)) %>% 
  dplyr::slice(1:10) %>%
  mutate(variables = fct_reorder(var, importancia)) %>%
  ggplot(data = ., aes(x = as.factor(variables), y = importancia)) +
  geom_col(fill = "royalblue4") +
  ylab("Importancia") +
  xlab("Variables") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5))

graph3 <- ate3_var_importance %>%
  arrange(desc(importancia)) %>%
  mutate(importancia = importancia / max(importancia)) %>% 
  dplyr::slice(1:10) %>%
  mutate(variables = fct_reorder(var, importancia)) %>%
  ggplot(data = ., aes(x = as.factor(variables), y = importancia)) +
  geom_col(fill = "royalblue4") +
  ylab("Importancia") +
  xlab("Variables") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5))

save(graph1, file = "./graphate1.RData")
save(graph2, file = "./graphate2.RData")
save(graph3, file = "./graphate3.RData")

ggsave(graph1, file = "./graphate1.png")
ggsave(graph2, file = "./graphate2.png")
ggsave(graph3, file = "./graphate3.png")

vars_importance_1 <- ate1_var_importance %>% slice(1:10) %>% 
  pull(var)
vars_importance_2 <- ate2_var_importance %>% slice(1:10) %>% 
  pull(var)
vars_importance_3 <- ate3_var_importance %>% slice(1:10) %>% 
  pull(var)

# 1 top, 0 bottom
ate1_balance <- data_validacion %>% filter(decil_informal <= 2 | decil_informal >= 9) %>% 
  select({{vars_importance_1}}, decil_informal) %>% 
  mutate(grupo = if_else(decil_informal <= 2, 1, 0)) %>% 
  select(-decil_informal) %>% 
  balance_table(., treatment = "grupo")

ate2_balance <- data_validacion %>% filter(decil_alim <= 2 | decil_alim >= 9) %>% 
  select({{vars_importance_1}}, decil_alim) %>% 
  mutate(grupo = if_else(decil_alim <= 2, 1, 0)) %>% 
  select(-decil_alim) %>% 
  balance_table(., treatment = "grupo")

ate3_balance <- data_validacion %>% filter(decil_secu <= 2 | decil_secu >= 9) %>% 
  select({{vars_importance_3}}, decil_secu) %>% 
  mutate(grupo = if_else(decil_secu <= 2, 1, 0)) %>% 
  select(-decil_secu) %>% 
  balance_table(., treatment = "grupo")

saveRDS(ate1_balance, "./ate1_balance.RDS")
saveRDS(ate2_balance, "./ate2_balance.RDS")
saveRDS(ate3_balance, "./ate3_balance.RDS")



