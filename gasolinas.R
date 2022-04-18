library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(firatheme)
library(showtext)
library(ggthemes)
library(viridis)
library(hrbrthemes)
library(scico)
library("Cairo")

showtext_auto()


font_add_google(family = "patua-one", "Patua One")
font_add_google(family = "montserrat", "Montserrat")
font_add_google(family = "roboto", "Roboto")

colores <- c("#192440","#D5433D")


precios <- read_xlsx("precios_gas.xlsx")

str(precios)


precios_tidy <- precios %>%
        pivot_longer(!fecha_cal, names_to = "tipo", values_to = "precios_min") 


precios_tidy$fecha_cal <- as.Date(precios_tidy$fecha_cal)


g <- precios_tidy %>%
        filter(tipo != "diesel") %>%
        ggplot(aes(x = fecha_cal, y= precios_min, color = tipo))+
        geom_line(size = 1.5, alpha = 0.7)+
        scale_y_continuous(breaks = seq(13,23,1),labels = scales::dollar)+
        theme_ipsum()+
        scale_x_date(expand = c(0,30),date_breaks = "3 month",
                     limits = as.Date(c('2017-01-01','2022-03-01')),labels=date_format("%b-%Y"))+
        labs(title = "Precios históricos de Gasolinas",
             subtitle = "Precios promedio mensuales en México desde el 01/01/2017 al 28/02/2022",
             x = "Fecha",
             y = "Precio",
             color = "Combustible",
             caption = "Fuente: CRE | Visualización: Miguel HG (@mike_dvz)")+
        theme(axis.title = element_text(), text = element_text(family = "montserrat"))+
        scale_color_manual(values = colores, labels = c("Magna", "Premium"))+
        theme(plot.title = element_text(family = "patua-one", face = "bold", size = 30),
              axis.title.y = element_text(size = 14, face = "bold",family = "montserrat"),
              axis.title.x = element_text(size = 14, face = "bold",family = "montserrat"),
              plot.subtitle = element_text(face = "bold", size = 15),
              axis.text = element_text(face = "bold"),
              plot.caption = element_text(size = 11, face = "bold", family = "patua'one"),
              plot.background = element_rect(fill = "white"),
              legend.position = "bottom")+
        theme(axis.text.x=element_text(angle=45, vjust = 0.5))+
        theme(legend.key.height= unit(0.7, 'cm'),
              legend.key.width= unit(1.5, 'cm'))+
        geom_vline(aes(xintercept = as.Date("2018-01-01")), color = "black", size = 0.9, linetype = "dotted")+
        annotate("text", x =as.Date("2018-06-01"), y = 22.10, family = "montserrat", size = 4.7,
                 lineheight = .9, color = "black", label = "Arranca la 4T")
        
        

g +
geom_curve(data = flechas, aes(x = x1, y = y1,xend = x2, yend = y2),
                arrow = arrow(length = unit(0.08, "inch")), size = 0.9,
                color = "black", curvature = -0.2)


flechas <- tibble(x1 = as.Date("2018-06-01"),
                         y1 = 21.90,
                         x2 = as.Date("2018-01-03"),
                         y2 = 20)     
        

ggsave("gasolinas_final.png",height =1080,width = 1920 , units = c("px"),dpi = 190)



precios_2 <- precios_tidy %>%
        mutate(year = year(fecha_cal),
               month = month(fecha_cal, label=TRUE),
               day = day(fecha_cal))

precios_2 %>%
        filter(tipo != "Diesel") %>%
        ggplot(aes(x = month, y = day, fill = precios_min ))+
        geom_tile(color = "white" ,size = 0.1)+
        facet_grid(tipo~year)+
        scale_fill_viridis(option = "inferno")+
        scale_y_continuous(breaks = seq(1,31,5))+
        theme_fira()+
        theme(legend.position = "bottom")+
        theme(legend.key.height= unit(0.3, 'cm'),
              legend.key.width= unit(1, 'cm'),
              legend.title = element_text(size=12))+
        theme(plot.background = element_rect(fill = "#282828", color = NA),
              axis.text = element_text(color = "white", family = "roboto", size = 8.4),
              legend.text = element_text(color = "white", family = "roboto",face = "bold", size = 12),
              axis.title.y = element_text(color = "white", family = "roboto", size = 13, face = "bold"),
              legend.title = element_text(color = "white", family = "roboto", size = 15, face = "bold"),
              axis.title.x = element_text(color = "white", family = "roboto", size = 13, face = "bold"),
              plot.title = element_text(color = "white", family = "patua-one", size = 30, face = "bold"),
              plot.subtitle = element_text(color = "white", family = "roboto", size = 17),
              plot.caption = element_text(color = "white", family = "patua-one", size = 11, face = "bold"))+
        theme(strip.text = element_text(colour = 'white', family = "patua-one", face = "bold",size = 13))+
        labs(x = "",
             y = "Dia del mes",
             fill = "Precio",
             title = "Evolución de precio en combustibles",
             subtitle = "Precios mínimos diarios en México desde el 01/01/2017 al 28/02/2022",
             caption = "Fuente: CRE | Visualización: Miguel HG (@mike_dvz)")
        #scale_fill_scico(palette = "lajolla", direction = -1)
        

ggsave("gasolinas_heat5.png",height =720,width = 850*2 , units = c("px"),dpi = 100)        

        
        

        
        

