library(httr)
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(ggrepel)
library(viridis)
library(padr)
library(gganimate)
library(wesanderson)
library(readr)
library(forcats)
Sys.setlocale("LC_ALL","English")
`%notin%` <- Negate(`%in%`)
ma <- function(x, n = 7){stats::filter(x, rep(1 / n, n), sides = 1)}

###IMPORTANTE###
today=Sys.Date()-1

options(timeout=12000)
download.file( url = "https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19Casos.zip", file.path("./archivos", "Covid19Casos.zip"), mode = "wb", method = "libcurl")
dataFIS <- read_csv("./archivos/Covid19Casos.zip",guess_max = 250,col_types = cols(
 id_evento_caso = col_integer(),
 sexo = col_skip(),
 edad = col_integer(),
 edad_años_meses = col_factor(),
 residencia_pais_nombre = col_skip(),
 residencia_provincia_nombre = col_factor(),
 residencia_departamento_nombre = col_factor(),
 carga_provincia_nombre = col_skip(),
 fecha_inicio_sintomas = col_date(format = "%Y-%m-%d"),
 fecha_apertura = col_date(format = "%Y-%m-%d"),
 sepi_apertura = col_skip(),
 fecha_internacion = col_date(format = "%Y-%m-%d"),
 cuidado_intensivo = col_skip(),
 fecha_cui_intensivo = col_skip(),
 fallecido = col_factor(),
 fecha_fallecimiento = col_skip(),
 asistencia_respiratoria_mecanica = col_skip(),
 carga_provincia_id = col_skip(),
 origen_financiamiento = col_skip(),
 clasificacion = col_factor(),
 clasificacion_resumen = col_factor(),
 residencia_provincia_id = col_skip(),
 fecha_diagnostico = col_date(format = "%Y-%m-%d"),
 residencia_departamento_id = col_skip(),
 ultima_actualizacion = col_skip()
))

dataFIS<-dataFIS %>% mutate(edad=ifelse(edad_años_meses=="Meses",edad/12,edad)) #arreglo edades en meses
#dataFIS<-dataFIS %>% mutate(fecha_fis= ifelse(is.na(fecha_inicio_sintomas),as.Date(fecha_apertura-3,origin = "1970-01-01"),as.Date(fecha_inicio_sintomas,origin = "1970-01-01")))
dataFIS<-dataFIS %>% mutate(fecha_diagnostico= if_else(is.na(fecha_diagnostico) & !is.na(fecha_inicio_sintomas),fecha_inicio_sintomas+5,fecha_diagnostico))
hosp<-dataFIS %>% filter(clasificacion_resumen=="Confirmado") %>% pad(group = "residencia_provincia_nombre", by="fecha_internacion",start_val = as.Date("2020-03-01"),end_val = today) %>% group_by(residencia_provincia_nombre,fecha_internacion )  %>% tally() %>% arrange(fecha_diagnostico) %>% group_by(residencia_provincia_nombre) %>% mutate(n=ifelse(is.na(n),0,n)) %>% mutate(n7=ma(n,7))
casos<-dataFIS %>% filter(clasificacion_resumen=="Confirmado") %>% pad(group = "residencia_provincia_nombre", by="fecha_diagnostico",start_val = as.Date("2020-03-01"),end_val = today) %>% group_by(residencia_provincia_nombre,fecha_diagnostico )  %>% tally() %>% arrange(fecha_diagnostico) %>% group_by(residencia_provincia_nombre) %>% mutate(n=ifelse(is.na(n),0,n)) %>% mutate(n7=ma(n,7))
diagnosticos<-dataFIS %>% pad(group = "residencia_provincia_nombre", by="fecha_diagnostico",start_val = as.Date("2020-03-01"),end_val = today) %>% group_by(residencia_provincia_nombre,fecha_diagnostico )  %>% tally() %>% arrange(fecha_diagnostico) %>% group_by(residencia_provincia_nombre) %>% mutate(n=ifelse(is.na(n),0,n)) %>% mutate(n7=ma(n,7))

casos_50<-dataFIS %>% filter(clasificacion_resumen=="Confirmado" & edad>=50) %>% pad(group = "residencia_provincia_nombre", by="fecha_diagnostico",start_val = as.Date("2020-03-01"),end_val = today) %>% group_by(residencia_provincia_nombre,fecha_diagnostico )  %>% tally() %>% arrange(fecha_diagnostico) %>% group_by(residencia_provincia_nombre) %>% mutate(n=ifelse(is.na(n),0,n)) %>% mutate(n7=ma(n,7))
diagnosticos_50<-dataFIS %>% filter(edad>=50) %>% pad(group = "residencia_provincia_nombre", by="fecha_diagnostico",start_val = as.Date("2020-03-01"),end_val = today) %>% group_by(residencia_provincia_nombre,fecha_diagnostico )  %>% tally() %>% arrange(fecha_diagnostico) %>% group_by(residencia_provincia_nombre) %>% mutate(n=ifelse(is.na(n),0,n)) %>% mutate(n7=ma(n,7))
tuti<-merge(diagnosticos, casos,by=c("residencia_provincia_nombre","fecha_diagnostico")) %>% mutate(pos=n7.y/n7.x) %>% mutate(pos_7=ma(pos))
tuti50<-merge(diagnosticos_50, casos_50,by=c("residencia_provincia_nombre","fecha_diagnostico")) %>% mutate(pos=n7.y/n7.x) %>% mutate(pos_7=ma(pos))

#casos_ap<-dataFIS %>% pad(group = "residencia_provincia_nombre", by="fecha_ap",start_val = as.Date(today-360),end_val = today) %>% group_by(residencia_provincia_nombre,fecha_ap )  %>% tally() %>% arrange(fecha_ap) %>% group_by(residencia_provincia_nombre) %>% mutate(n=ifelse(is.na(n),0,n)) %>% mutate(n7=ma(n,7))
#departamentos<-dataFIS %>% pad(group = c("residencia_departamento_nombre","residencia_provincia_nombre"), by="fecha_diagnostico",start_val = as.Date(today-360),end_val = today) %>% group_by(residencia_provincia_nombre,residencia_departamento_nombre,fecha_diagnostico )  %>% tally() %>% arrange(fecha_diagnostico) %>% group_by(residencia_provincia_nombre,residencia_departamento_nombre) %>% mutate(n=ifelse(is.na(n),0,n)) %>% mutate(n7=ma(n,7))
pobp<-read.csv("poblacion_provincias.csv", sep=",",header=TRUE)
dep<-read.csv("Población_Proyectada_2020.csv", sep=",",header=TRUE)
names(dep)<-c("residencia_provincia_nombre","residencia_departamento_nombre","poblacion")
names(pobp)<-c("residencia_provincia_nombre","poblacion","ifr")
casos<-merge(casos, pobp, by = "residencia_provincia_nombre") %>% arrange(residencia_provincia_nombre,fecha_diagnostico)
casos<-casos %>% group_by(residencia_provincia_nombre) %>% mutate(casos_hab = n7/poblacion*100000) %>% ungroup()
diagnosticos<-merge(diagnosticos, pobp, by = "residencia_provincia_nombre") %>% arrange(residencia_provincia_nombre,fecha_diagnostico)
diagnosticos<-diagnosticos %>% group_by(residencia_provincia_nombre) %>% mutate(diag_hab = n7/poblacion*100000) %>% ungroup()
casos_50<-merge(casos_50, pobp, by = "residencia_provincia_nombre") %>% arrange(residencia_provincia_nombre,fecha_diagnostico)
casos_50<-casos_50 %>% group_by(residencia_provincia_nombre) %>% mutate(casos_hab = n7/poblacion*100000) %>% ungroup()

#departamentos<-merge(departamentos, dep, by = c("residencia_departamento_nombre","residencia_provincia_nombre") )  %>% arrange(residencia_departamento_nombre,fecha_diagnostico)
#departamentos<-departamentos %>% group_by(residencia_departamento_nombre) %>% mutate(casos_hab = n7/poblacion*100000) %>% ungroup()

p_conc_time <- function(df,fname,colu,desde=today-240,hasta=today,maxv=100,base=11,wrap_size=11,linew=1) {
 pal <- wes_palette("Zissou1", 20, type = "continuous")
 ggplot(df, aes(x = fecha_diagnostico, y = casos_hab*7, group=residencia_provincia_nombre )) +
  #scale_color_viridis(option = "C")+
  #scale_color_viridis(option="C",trans = "log10")+
  scale_color_gradientn(colours = pal, limits = c(0,150),oob=squish) +
  #geom_line(size=1,aes(color=casos_nuevos)) +
  #geom_vline(xintercept = as.Date("2021-01-15"), linetype=3, size=0.7, color="red", show.legend=FALSE)+
  #geom_vline(xintercept = as.Date("2021-03-15"), linetype=3, size=0.7, color="red", show.legend=FALSE)+
  #geom_hline(yintercept = 150/14, linetype=3, size=0.7, color="red", show.legend=FALSE)+
  #geom_hline(yintercept = 5, linetype=3, size=0.7, color="green", show.legend=FALSE)+
  #    geom_hline(yintercept = 50/14, linetype=3, size=0.7, color="orange", show.legend=TRUE)+
  #    geom_hline(yintercept = 20/14, linetype=3, size=0.7, color="yellow", show.legend=TRUE)+
  #    geom_hline(yintercept = 5/14, linetype=3, size=0.7, color="green", show.legend=TRUE)+
   geom_line(size=linew,aes(color=casos_hab*7)) +
#   geom_hline(yintercept = 150, color="red")+
 #  geom_hline(yintercept = 100, color="orange")+
  # geom_hline(yintercept = 50, color="gold")+
  ylab("Casos semanales por cada 100000 habitantes")+
  xlab("Fecha de diagnóstico")+
  theme_light(base_size=base)+
  scale_y_continuous(breaks = seq(0,maxv*7,50),minor_breaks=NULL,expand=c(0,0))+
  coord_cartesian(ylim=c(0,maxv*7))+
  scale_x_date(date_breaks = "5 days", minor_breaks=NULL,labels=date_format("%d-%m-%y"), limits=c(desde,hasta),expand=c(0,1))+
  facet_wrap(~residencia_provincia_nombre,ncol=colu)+
  theme(strip.text.x = element_text(size = wrap_size, color = "black", face = "bold"),plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90,hjust = 0.5,vjust=0.5),axis.title.y = element_text(hjust = 0.5,vjust=0.5),axis.title.x = element_text(hjust = 0.5,vjust=0.5),legend.position ="none", strip.background = element_rect(color="black",fill="white",size=1.5,linetype="solid"))+
  #annotate("label", x = desde, y = maxv*.99, label = "@rquiroga777",colour = "dark red",hjust=0,vjust=1)+

  #geom_vline(xintercept = as.numeric(as.Date("2020-07-01")), linetype=2, size=0.7, color="red", show.legend=TRUE)+
  #geom_vline(xintercept = as.numeric(as.Date("2020-07-31")), linetype=2, size=1,color="orange", show.legend=TRUE)+
  #geom_vline(xintercept = as.numeric(as.Date("2020-08-14")), linetype=2, size=0.7,color="orange", show.legend=TRUE)+
  # scale_colour_manual(name = 'Conferencia',values =c('black'='black','red'='red'), labels = c('c2','c1'))  +
  ggtitle(paste0("Casos semanales cada 100 mil habitantes - ",today))+
  labs(caption="Casos semanales por fecha de diagnóstico o FIS+5, cada 100 mil habitantes. Gráfico por Rodrigo Quiroga @rquiroga777.")
}
p_conc_time_tuti <- function(df,fname,colu,desde=today-240,hasta=today,maxv=100,base=11,wrap_size=11,linew=1,titulo="Titulo") {
  pal <- wes_palette("Zissou1", 20, type = "continuous")
  ggplot(df, aes(x = fecha_diagnostico, y = pos_7*100, group=residencia_provincia_nombre )) +
    #scale_color_viridis(option = "C")+
    #scale_color_viridis(option="C",trans = "log10")+
    scale_color_gradientn(colours = pal, limits = c(0,25),oob=squish) +
    #geom_line(size=1,aes(color=casos_nuevos)) +
    #geom_vline(xintercept = as.Date("2021-01-15"), linetype=3, size=0.7, color="red", show.legend=FALSE)+
    #geom_vline(xintercept = as.Date("2021-03-15"), linetype=3, size=0.7, color="red", show.legend=FALSE)+
    #geom_hline(yintercept = 150/14, linetype=3, size=0.7, color="red", show.legend=FALSE)+
    #geom_hline(yintercept = 5, linetype=3, size=0.7, color="green", show.legend=FALSE)+
    #    geom_hline(yintercept = 50/14, linetype=3, size=0.7, color="orange", show.legend=TRUE)+
    #    geom_hline(yintercept = 20/14, linetype=3, size=0.7, color="yellow", show.legend=TRUE)+
    #    geom_hline(yintercept = 5/14, linetype=3, size=0.7, color="green", show.legend=TRUE)+
    geom_line(size=linew,aes(color=pos_7*100)) +
    #   geom_hline(yintercept = 150, color="red")+
    #  geom_hline(yintercept = 100, color="orange")+
    # geom_hline(yintercept = 50, color="gold")+
    ylab("Positividad")+
    xlab("Fecha de diagnóstico")+
    theme_light(base_size=base)+
    scale_y_continuous(breaks = seq(0,maxv,10),minor_breaks=NULL,expand=c(0,0))+
    coord_cartesian(ylim=c(0,maxv))+
    scale_x_date(date_breaks = "10 days", minor_breaks=NULL,labels=date_format("%d-%m-%y"), limits=c(desde,hasta),expand=c(0,1))+
    facet_wrap(~residencia_provincia_nombre,ncol=colu)+
    theme(strip.text.x = element_text(size = wrap_size, color = "black", face = "bold"),plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90,hjust = 0.5,vjust=0.5),axis.title.y = element_text(hjust = 0.5,vjust=0.5),axis.title.x = element_text(hjust = 0.5,vjust=0.5),legend.position ="none", strip.background = element_rect(color="black",fill="white",size=1.5,linetype="solid"))+
    #annotate("label", x = desde, y = maxv*.99, label = "@rquiroga777",colour = "dark red",hjust=0,vjust=1)+

    #geom_vline(xintercept = as.numeric(as.Date("2020-07-01")), linetype=2, size=0.7, color="red", show.legend=TRUE)+
    #geom_vline(xintercept = as.numeric(as.Date("2020-07-31")), linetype=2, size=1,color="orange", show.legend=TRUE)+
    #geom_vline(xintercept = as.numeric(as.Date("2020-08-14")), linetype=2, size=0.7,color="orange", show.legend=TRUE)+
    # scale_colour_manual(name = 'Conferencia',values =c('black'='black','red'='red'), labels = c('c2','c1'))  +
    ggtitle(titulo)+
    labs(caption="Positividad (media móvil 7 días) por fecha de diagnóstico o FIS+5, cada 100 mil habitantes. Gráfico por Rodrigo Quiroga @rquiroga777.")
}
p_conc_time_diag <- function(df,fname,colu,desde=today-240,hasta=today,maxv=100,base=11,wrap_size=11,linew=1) {
  pal <- wes_palette("Zissou1", 20, type = "continuous")
  ggplot(df, aes(x = fecha_diagnostico, y = diag_hab, group=residencia_provincia_nombre )) +
    #scale_color_viridis(option = "C")+
    #scale_color_viridis(option="C",trans = "log10")+
    scale_color_gradientn(colours = pal, limits = c(0,150),oob=squish) +
    #geom_line(size=1,aes(color=casos_nuevos)) +
    #geom_vline(xintercept = as.Date("2021-01-15"), linetype=3, size=0.7, color="red", show.legend=FALSE)+
    #geom_vline(xintercept = as.Date("2021-03-15"), linetype=3, size=0.7, color="red", show.legend=FALSE)+
    #geom_hline(yintercept = 150/14, linetype=3, size=0.7, color="red", show.legend=FALSE)+
    #geom_hline(yintercept = 5, linetype=3, size=0.7, color="green", show.legend=FALSE)+
    #    geom_hline(yintercept = 50/14, linetype=3, size=0.7, color="orange", show.legend=TRUE)+
    #    geom_hline(yintercept = 20/14, linetype=3, size=0.7, color="yellow", show.legend=TRUE)+
    #    geom_hline(yintercept = 5/14, linetype=3, size=0.7, color="green", show.legend=TRUE)+
    geom_line(size=linew,aes(color=diag_hab)) +
    #   geom_hline(yintercept = 150, color="red")+
    #  geom_hline(yintercept = 100, color="orange")+
    # geom_hline(yintercept = 50, color="gold")+
    ylab("Diag semanales por cada 100000 habitantes")+
    xlab("Fecha de diagnóstico")+
    theme_light(base_size=base)+
    scale_y_continuous(breaks = seq(0,maxv*7,50),minor_breaks=NULL,expand=c(0,0))+
    coord_cartesian(ylim=c(0,maxv*7))+
    scale_x_date(date_breaks = "5 days", minor_breaks=NULL,labels=date_format("%d-%m-%y"), limits=c(desde,hasta),expand=c(0,1))+
    facet_wrap(~residencia_provincia_nombre,ncol=colu)+
    theme(strip.text.x = element_text(size = wrap_size, color = "black", face = "bold"),plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90,hjust = 0.5,vjust=0.5),axis.title.y = element_text(hjust = 0.5,vjust=0.5),axis.title.x = element_text(hjust = 0.5,vjust=0.5),legend.position ="none", strip.background = element_rect(color="black",fill="white",size=1.5,linetype="solid"))+
    #annotate("label", x = desde, y = maxv*.99, label = "@rquiroga777",colour = "dark red",hjust=0,vjust=1)+

    #geom_vline(xintercept = as.numeric(as.Date("2020-07-01")), linetype=2, size=0.7, color="red", show.legend=TRUE)+
    #geom_vline(xintercept = as.numeric(as.Date("2020-07-31")), linetype=2, size=1,color="orange", show.legend=TRUE)+
    #geom_vline(xintercept = as.numeric(as.Date("2020-08-14")), linetype=2, size=0.7,color="orange", show.legend=TRUE)+
    # scale_colour_manual(name = 'Conferencia',values =c('black'='black','red'='red'), labels = c('c2','c1'))  +
    ggtitle(paste0("Diagnosticos semanales cada 100 mil habitantes - ",today))+
    labs(caption="Diagnósticos semanales por fecha de diagnóstico o FIS+5, cada 100 mil habitantes. Gráfico por Rodrigo Quiroga @rquiroga777.")
}

casos$residencia_provincia_nombre<-factor(casos$residencia_provincia_nombre, levels=sort(levels(casos$residencia_provincia_nombre)))
fname<-paste0("provincias_2_wide_sisa",today,".png"); p_conc_time(casos,base=14,wrap_size=17,fname,colu=6,desde=today-47,hasta=today-2,maxv=25); ggsave(fname, dpi = 400,type="cairo-png",width=15,height=10)
fname<-paste0("provincias_2_wide_sisa_positividad",today,".png"); p_conc_time_tuti(tuti,base=14,wrap_size=17,fname,colu=6,desde=today-92,hasta=today-2,maxv=70, titulo=paste0("Positividad - ",today)); ggsave(fname, dpi = 400,type="cairo-png",width=15,height=10)
fname<-paste0("provincias_2_wide_sisa_50_positividad",today,".png"); p_conc_time_tuti(tuti50,base=14,wrap_size=17,fname,colu=6,desde=today-92,hasta=today-2,maxv=70, titulo=paste0("Positividad >50 años - ",today)); ggsave(fname, dpi = 400,type="cairo-png",width=15,height=10)
fname<-paste0("provincias_2_wide_sisa_DIAG",today,".png"); p_conc_time_diag(diagnosticos,base=14,wrap_size=17,fname,colu=6,desde=today-182,hasta=today-2,maxv=75); ggsave(fname, dpi = 400,type="cairo-png",width=15,height=10)
fname<-paste0("provincias_2_wide_sisa_50",today,".png"); p_conc_time(casos_50,base=14,wrap_size=17,fname,colu=6,desde=today-47,hasta=today-2,maxv=25); ggsave(fname, dpi = 400,type="cairo-png",width=15,height=10)
