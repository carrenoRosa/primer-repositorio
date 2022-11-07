##################################################################################################
#########################COMPARACION BASES DE DATOS ##############################################
##################################################################################################


#Librerias
library(tidyverse)
library(haven)

#MARCHA BLANCA
setwd("C:/Users/rscarrenom/OneDrive - Instituto Nacional de Estadisticas/19.- CAB 2023/3.- Marcha blanca IX EPF/0.- BBDD Marcha Blanca IX EPF/Entrega v2")


#Base Personas 
BP <- read_dta("tabla_personas_mb_ix_epf_ipc.dta")
#-------------------------------------------------------------------------------------------------

#Base Gastos
BG <- read_dta("tabla_gastos_mb_ix_epf_ipc.dta")
BG <- BG %>%
  filter(ccif!="04.2.1.01.01" & ccif!="04.2.2.01.01" & ccif!="04.2.2.01.02" & interview_id!="")
#--------------------------------------------------------------------------------------------------

#Base Quintilizada EPF
BQepf <- read_dta("tabla_q_mb_ix_epf_ipc.dta")

#--------------------------------------------------------------------------------------------------


######################NUMERO DE HOGARES###################################

## a)BASE PERSONAS  : Numero de Hogares encuestados muestra  
NPMBP <-BP[BP$sprincipal==1,] %>% 
  count(BP$sprincipal[BP$sprincipal==1]) %>%
  select(-1) %>% 
  rename(Numero_Hogares_Muestra_BP = n) 


## b)BASE PERSONAS  : Numero de Hogares encuestados estimados
NPEBP <-BP[BP$sprincipal==1,] %>% 
  count(BP$sprincipal[BP$sprincipal==1], wt =BP$fe[BP$sprincipal==1]) %>% 
  select(-1) %>% 
  rename(Numero_Hogares_Estimado_BP=n) 

## c)BASE GASTOS  : Numero de Hogares encuestados muestra  
NPMBG <- BG %>% 
  arrange(interview_id) %>% 
  group_by(interview_id) %>%
  mutate(AUX = row_number()) %>%
  ungroup() %>% 
  count(AUX ==1) %>%
  filter(`AUX == 1`!= FALSE) %>% 
  rename(Numero_Hogares_Estimado_BG=n)

## d)BASE GASTOS  : Numero de Hogares encuestados estimados   

BG0 <-BG %>% 
  arrange(interview_id) %>% 
  group_by(interview_id) %>%
  mutate(AUX = row_number()) %>%
  ungroup()

BG0$AUX2 <- ifelse(BG0$AUX==1,BG0$AUX*BG0$fe,0)
NPEBG <-sum(BG0$AUX2) 

#CHEQUEO NUMERO HOGARES ####

NPMBP$Numero_Hogares_Muestra_BP == NPMBG$Numero_Hogares_Estimado_BG 
NPEBP$Numero_Hogares_Estimado_BP ==NPEBG

rm(NPEBG,NPMBG,NPEBP,NPMBP)

######################GASTOS###################################


## a) BASE PERSONAS : Gasto Hogares Total Muestra
gastoM_BP <- BP %>% 
  filter(sprincipal==1) %>% 
  summarise(suma = sum(gastot_hd))

## b) BASE PERSONAS : Gasto Hogares Total Estimado
gastoE_BP <- BP %>% 
  filter(sprincipal==1) %>% 
  mutate(gasto_expandido = gastot_hd*fe) %>% 
  summarise(suma= sum(gasto_expandido))


## c) BASE GASTOS : Gasto Hogares Total Muestra
gastoM_BG<- BG %>%
  filter(ccif!="04.2.1.01.01" & ccif!="04.2.2.01.01" & ccif!="04.2.2.01.02" & interview_id!="") %>% 
  summarise(gasto_bg = sum(gasto))


## d) BASE GASTOS : Gasto Hogares Total Estimado
gastoE_BG<- BG %>%
  mutate(gasto_expan = gasto*fe) %>% 
  summarise(gasto_bg = sum(gasto_expan))

## e) BASE QUINTILIZADA EPF : Gasto Hogares Total Estimado

gastoE_BQepf <- sum(BQepf$gasto_exp_hog)

## f) BASE QUINTILIZADA PROPIA : Gasto Hogares Total Estimado

load(file = "C:/Users/rscarrenom/OneDrive - Instituto Nacional de Estadisticas/19.- CAB 2023/3.- Marcha blanca IX EPF/3.- Quintilización/Quintilización_R/tabla_q_mb_ix_epf_ipc_Propia.RData")

gastoE_BQpropia <- sum(BG_3$Gasto_expand_hogares)


###CHEQUEO GASTOS #####

gastoM_BG$gasto_bg ==gastoM_BP$suma
gastoE_BG$gasto_bg==gastoE_BP$suma

gastoE_BG$gasto_bg==gastoE_BQepf # SE DETECTAN INCONSISTENCIAS 
gastoE_BG$gasto_bg==gastoE_BQpropia



