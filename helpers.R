library(dplyr)
library(plyr)
library(reshape2)
library(tidyverse)
library(data.table)
library(haven)
library(gt)
library(glue)
library(shiny)
library(DT)

####Prueba nuevo mewtodo
RFC<-data.table::fread("RFC.csv")
Recortada<-data.table::fread("recortada15-21.csv",encoding="UTF-8")
Nomina <- data.table::fread("padronnominas.csv",encoding = "UTF-8")
IMSS <- data.table::fread("Padron_IMSS_Feb_21.csv",encoding = "UTF-8") #Meses pagados

###

Recortada$perimax<-with(Recortada, ifelse(yearp==2021 & monthp>2, 1,0))
Recortada<-filter(Recortada, perimax==0)

Nomina<-merge(Nomina, RFC, by="ctarfc",all.y = T)
Nomina<-dplyr::select(Nomina, ctarfc,nombre_dgti,actividad_dgti,subactividad_dgti )

IMSS<-merge(IMSS, RFC, by="ctarfc",all.y = T)

Recortada<-merge(Recortada, RFC, by="ctarfc",all.y = T)
Recortada<-(Recortada[Recortada$yearp>=2015])

pivotImss <- function(empimss){
  imss_longer <- empimss %>% 
    tidyr::pivot_longer(
      cols = -ctarfc, names_to = c("mesl", "yearp"), 
      names_prefix = "empleados_IMSS_",
      names_sep = "_", 
      values_to = "empleadosIMSS") %>% 
    filter(!is.na(empleadosIMSS))
  
  imss_longer <- mutate(imss_longer,
                        ApareceenIMSS = ifelse(is.na(empleadosIMSS), 0, 1),
                        monthp = case_when(
                          mesl %in% "ene" ~ 1,
                          mesl %in% "feb" ~ 2,
                          mesl %in% "mar" ~ 3,
                          mesl %in% "abr" ~ 4,
                          mesl %in% "may" ~ 5,
                          mesl %in% "jun" ~ 6,
                          mesl %in% "jul" ~ 7,
                          mesl %in% "ago" ~ 8,
                          mesl %in% "sep" ~ 9,
                          mesl %in% "oct" ~ 10,
                          mesl %in% "nov" ~ 11,
                          mesl %in% "dic" ~ 12,
                        ),
                        yearp = as.numeric(yearp)+2000)
}
IMSS<-pivotImss(IMSS)

Recortada2<-left_join((reshape2::dcast(Recortada,ctarfc+yearp+monthp~"Total",value.var = "total",sum,na.rm=T)), (reshape2::dcast(Recortada,ctarfc+yearp+monthp~"Ingreso",value.var = "ingreso",sum,na.rm=T)), by=c("ctarfc","yearp","monthp"))%>%
  left_join((reshape2::dcast(Recortada,ctarfc+yearp+monthp~"Remunera",value.var = "remunera",sum,na.rm=T)), by=c("ctarfc","yearp","monthp"))%>%
  left_join((reshape2::dcast(Recortada,ctarfc+yearp+monthp~"Empleados",value.var = "totemp1",sum,na.rm=T)), by=c("ctarfc","yearp","monthp"))%>%
  mutate(ApareceenISN=1)%>%
  left_join(IMSS, by=c("ctarfc","yearp","monthp"))%>%
  dplyr::select(-mesl)%>%
  left_join((reshape2::dcast(Recortada,ctarfc+yearp~"Total_Total",value.var = "total",sum,na.rm=T)), by=c("ctarfc","yearp"))%>%
  left_join((reshape2::dcast(Recortada,ctarfc+yearp~"Total_Ingreso",value.var = "ingreso",sum,na.rm=T)), by=c("ctarfc","yearp"))%>%
  left_join((reshape2::dcast(Recortada,ctarfc+yearp~"Total_Remunera",value.var = "remunera",sum,na.rm=T)), by=c("ctarfc","yearp"))%>%
  left_join((reshape2::dcast(Recortada,ctarfc+yearp~"Total_Empleados",value.var = "totemp1",sum,na.rm=T)), by=c("ctarfc","yearp"))%>%
  left_join((reshape2::dcast(IMSS,ctarfc+yearp~"Total_Empleados_IMSS",value.var = "empleadosIMSS",sum,na.rm=T)), by=c("ctarfc","yearp"))

Recortada2$periodo_faltante<- with(Recortada2,ifelse(empleadosIMSS>=1 &  is.na(ApareceenISN),1,0))
Recortada2$potencial_por_periodo<- with(Recortada2,ifelse(periodo_faltante==1,  (empleadosIMSS*((Total_Remunera/Total_Empleados)*0.03)),0 ))

Recortada2$empleados_faltantes<- with(Recortada2,ifelse(ApareceenISN>=1  & ApareceenIMSS >= 1 & empleadosIMSS>Empleados, empleadosIMSS - Empleados,0 ))
Recortada2$potencial_por_empleados<- with(Recortada2,ifelse(Empleados>0 & empleados_faltantes>=1,  (((Remunera/ Empleados) * empleados_faltantes) *0.03),0 ))

Recortada3<-reshape2::dcast(Recortada2, ctarfc+yearp~"PeriodosPagados", value.var="ApareceenISN",sum,na.rm=T)%>%
  left_join(reshape2::dcast(Recortada2, ctarfc+yearp~"PagoMensualPromedio", value.var="Ingreso",mean,na.rm=T),by=c("ctarfc","yearp"))%>%
  left_join(reshape2::dcast(Recortada2, ctarfc+yearp~"PagoAnualTotalMDP", value.var="Ingreso",sum,na.rm=T),by=c("ctarfc","yearp"))%>%
  left_join(reshape2::dcast(Recortada2, ctarfc+yearp~"TabajadoresPromedioSISCOR", value.var="Empleados",mean,na.rm=T),by=c("ctarfc","yearp"))%>%
  left_join(reshape2::dcast(Recortada2, ctarfc+yearp~"TotalTabajadoresSISCOR", value.var="Empleados",sum,na.rm=T),by=c("ctarfc","yearp"))%>%
  mutate(Recortada2, SalarioMensualPromedio=(PagoMensualPromedio/0.03)/TabajadoresPromedioSISCOR)%>%
  left_join(reshape2::dcast(Recortada2, ctarfc+yearp~"TrabajadoresPromedioIMSS", value.var="empleadosIMSS",mean,na.rm=T),by=c("ctarfc","yearp"))%>%
  left_join(reshape2::dcast(Recortada2, ctarfc+yearp~"TotalTrabajadoresIMSS", value.var="empleadosIMSS",sum,na.rm=T),by=c("ctarfc","yearp"))%>%
  left_join(reshape2::dcast(Recortada2, ctarfc+yearp~"TotalRemuneracionesMDP", value.var="Remunera",sum,na.rm=T))%>%
  left_join(reshape2::dcast(Recortada2, ctarfc+yearp~"PotencialPorPeriodoMDP", value.var="potencial_por_periodo",sum,na.rm=T),by=c("ctarfc","yearp"))%>%
  left_join(reshape2::dcast(Recortada2, ctarfc+yearp~"PotencialPorEmpleadosMDP", value.var="potencial_por_empleados",sum,na.rm=T),by=c("ctarfc","yearp"))%>%
  mutate(Tipo=ifelse(PotencialPorPeriodoMDP==0 & PotencialPorEmpleadosMDP>0, "C", "SinTipo"))%>%
  mutate(Tipo=ifelse(PotencialPorPeriodoMDP>0 & PotencialPorEmpleadosMDP>0, "B", Tipo))%>%
  mutate(Tipo=ifelse(PotencialPorPeriodoMDP>0 & PotencialPorEmpleadosMDP==0, "A", Tipo))%>%
  ###Redondeo de promedios
  mutate(TabajadoresPromedioSISCOR=round(TabajadoresPromedioSISCOR,0))%>%
  mutate(TotalTabajadoresSISCOR=round(TotalTabajadoresSISCOR,0))%>%
  mutate(TrabajadoresPromedioIMSS=round(TrabajadoresPromedioIMSS,0))%>%
  mutate(TotalTrabajadoresIMSS=round(TotalTrabajadoresIMSS,0))%>%
  ###Redondeo a millones de pesos
  mutate(PagoMensualPromedio=round(PagoMensualPromedio,1))%>%
  mutate(SalarioMensualPromedio=round(SalarioMensualPromedio,1))%>%
  mutate(PagoAnualTotalMDP=round((PagoAnualTotalMDP/1000000),1))%>%
  mutate(TotalRemuneracionesMDP=round((TotalRemuneracionesMDP/1000000),1))%>%
  mutate(PotencialPorPeriodoMDP=round((PotencialPorPeriodoMDP/1000000),1))%>%
  mutate(PotencialPorEmpleadosMDP=round((PotencialPorEmpleadosMDP/1000000),1))