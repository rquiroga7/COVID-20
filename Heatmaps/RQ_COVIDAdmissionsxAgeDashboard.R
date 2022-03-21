rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(lubridate)
library(ggstream)
library(extrafont)
library(paletteer)
library(RcppRoll)
library(ggrepel)
library(ragg)
library(scales)

theme_custom <- function() {
 theme_classic() %+replace%
  theme(plot.title.position="plot", plot.caption.position="plot",
        strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(1.4), hjust=0,
                                margin=margin(0,0,5.5,0)),
        text=element_text(family="Lato"))
}

today<-Sys.Date()
#Download admissions by age
#Nationally
source1 <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=cumAdmissionsByAge&format=csv"
#Regionally
source2 <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsRegion&metric=cumAdmissionsByAge&format=csv"

temp <- tempfile()

temp <- curl_download(url=source1, destfile=temp, quiet=FALSE, mode="wb")
natdata <- read.csv(temp)
temp <- curl_download(url=source2, destfile=temp, quiet=FALSE, mode="wb")
regdata <- read.csv(temp)

data <- bind_rows(natdata, regdata) %>%
 mutate(date=as.Date(date)) %>%
 group_by(areaName, age) %>%
 arrange(date) %>%
 mutate(admrate=rate-lag(rate, 1),
        admissions=value-lag(value, 1),
        age=gsub("_to_", "-", age),
        age=factor(age, levels=c("0-5", "6-17", "18-64", "65-84", "85+")),
        admrate_roll=roll_mean(admrate, 7, align="center", fill=NA),
        peakrate=max(admrate_roll[date>as.Date("2020-10-01") & date<as.Date("2021-02-01")], na.rm=TRUE)) %>%
 ungroup() %>%
 mutate(peakprop=admrate_roll/peakrate)

#Repeat for Omicron-specific analysis
data2 <- bind_rows(natdata, regdata) %>%
 mutate(date=as.Date(date)) %>%
 group_by(areaName, age) %>%
 arrange(date) %>%
 mutate(admrate=rate-lag(rate, 1),
        admissions=value-lag(value, 1),
        age=gsub("_to_", "-", age),
        age=factor(age, levels=c("0-5", "6-17", "18-64", "65-84", "85+")),
        admrate_roll=roll_mean(admrate, 7, align="center", fill=NA),
        peakrate=max(admrate_roll[date>as.Date("2021-10-01") & date<as.Date("2022-02-01")], na.rm=TRUE)) %>%
 ungroup() %>%
 mutate(peakprop=admrate_roll/peakrate)


##### IMPORTANTE #######
#agg_tiff("Outputs/COVIDAdmissionsxAgeOmi_spanish.tiff", units="in", width=9, height=6, res=300)
ggplot(data2 %>% filter(areaName=="England" & date>as.Date("2021-12-01")),
       aes(x=date, y=peakprop, colour=age))+
 geom_hline(yintercept=1, linetype=2, colour="Grey80")+
 labs(color='Grupo etario') +
 geom_line(size=1.1)+
 geom_vline(xintercept = as.Date("2022-02-22"),linetype=2, colour="Blue")+
 annotate(geom = 'text', label = 'BA.2 mayoritaria', x = as.Date("2022-02-23"), y = 0.98, hjust = 1, vjust = 1,angle=90,, colour="Blue")+
 geom_text_repel(data=data2 %>% filter(date==max(date[!is.na(peakprop)]) & areaName=="England"),
                 aes(x=max(date[!is.na(peakprop)]), y=peakprop, label = age,
                     colour=age),
                 family = "Calibri", direction = "y", xlim = c(as.Date("2022-04-01"), NA),
                 hjust = 0, segment.color = NA, box.padding = .3, show.legend = FALSE)+
 scale_x_date(date_breaks = "1 month",name="", limits=c(NA_Date_, as.Date("2022-03-20")))+
 scale_y_continuous(name="Proporción del pico Ómicron", labels=label_percent(accuracy=1))+
 scale_colour_paletteer_d("awtools::a_palette")+
 theme_custom()+
 theme(axis.text.x = element_text(angle = 90,hjust = 0.5,vjust=0.5))+
 labs(title="INGLATERRA: Aumentan las hospitalizaciones COVID en todos los grupos etarios",
      subtitle="Media móvil 7 días de nuevas hospitalizaciones COVID como proporción del pico Ómicron (BA.1)",
      caption="Datos de coronavirus.data.gov.uk, análisis de prevalencia BA.2 por @AlastairGrant4 \n Gráfico por Rodrigo Quiroga (@rquiroga777), basado en el gráfico diseñado por @VictimOfMaths")
ggsave(paste0("Outputs/COVIDAdmissionsxAgeOmi_spanish",today,".png"), dpi = 600,type="cairo-png",width=9,height=6)
#dev.off()
