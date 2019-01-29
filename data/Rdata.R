library(WDI)
library(reshape)
library(ggplot2)
library(dplyr)
theme_set(theme_bw())

# Población Somalia

somalia <- list()
etiopia <- list()
kenia <- list()

indicadores <- c("SP.POP.TOTL","SP.DYN.LE00.IN","SP.DYN.TFRT.IN","SP.ADO.TFRT",
                 "SH.DYN.MORT", "SH.DYN.AIDS.ZS","AG.LND.FRST.K2","IT.CEL.SETS.P2",
                 "BX.KLT.DINV.CD.WD","DT.ODA.ALLD.CD")


for (i in seq(1:length(indicadores))){
  somalia[[i]] <- WDI(country = "SO", indicator = indicadores[i], start = 1990,
                  end = 2016, extra = FALSE, cache = NULL)
}

for (i in seq(1:length(indicadores))){
  etiopia[[i]] <- WDI(country = "ETH", indicator = indicadores[i], start = 1990,
                      end = 2016, extra = FALSE, cache = NULL)
}

for (i in seq(1:length(indicadores))){
  kenia[[i]] <- WDI(country = "KEN", indicator = indicadores[i], start = 1990,
                      end = 2016, extra = FALSE, cache = NULL)
}

somalia <- merge_recurse(somalia)
etiopia <- merge_recurse(etiopia)
kenia <- merge_recurse(kenia)

nombres <- c("iso2c","país","ano","poblacion","esperanza_vida",
             "fertilidad","ados_fertilidad","mortalidad","sida",
             "forestal","movil","inversion","ayudas")

names(somalia) <- nombres
names(etiopia) <- nombres
names(kenia) <- nombres

etiopia[25,"forestal"] <- etiopia[25,"forestal"]/1000
etiopia[26,"forestal"] <- etiopia[26,"forestal"]/1000
etiopia[27,"forestal"] <- etiopia[27,"forestal"]/1000

write.csv(somalia,"somalia.csv")
write.csv(etiopia,"etiopia.csv")
write.csv(kenia,"kenia.csv")

data <- rbind(somalia, etiopia)
data <- rbind(data, kenia)

write.csv(data,"data.csv")

# usuarios de internet
#tengo que escraperar esta tablar


##########################################################


data %>% ggplot(aes(año,poblacion/1000000,colour= país))+
  geom_point(size=1.7)+
  geom_line(size=.1)+
  labs(title="Población",
       subtitle="Millones de personas", 
       caption="Fuente: World Bank Data", 
       y="Población")+
  scale_x_continuous(breaks = seq(1990,2016,4))+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 35, vjust=0.5),
        panel.grid.minor = element_blank())


data %>% ggplot(aes(año,esperanza_vida, colour=país))+
  geom_point(size=1.7)+
  geom_line(size=.1)+
  labs(title="Esperanza de vida",
       subtitle="Media de años que es más probable que tendrá un niño al nacer", 
       caption="Fuente: World Bank Data", 
       y="Años")+
  scale_x_continuous(breaks = seq(1990,2016,4))+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 35, vjust=0.5),
        panel.grid.minor = element_blank())


data %>% ggplot(aes(año,fertilidad, colour=país))+
  geom_point(size=1.7)+
  geom_line(size=.1)+
  labs(title="Tasa de fertilidad",
       subtitle="Nacimientos por mil mujeres", 
       caption="Fuente: World Bank Data", 
       y="Número de nacimientos")+
  scale_x_continuous(breaks = seq(1990,2016,4))+
  theme(axis.text.x = element_text(angle = 35, vjust=0.5),
        panel.grid.minor = element_blank())


data %>% ggplot(aes(año,ados_fertilidad, colour=país))+
  geom_point(size=1.7)+
  geom_line(size=.1)+
  labs(title="Tasa de fertilidad en adolescentes",
       subtitle="Número de nacimientos por 1000 mujeres entre 15 y 19 años", 
       caption="Fuente: World Bank Data", 
       y="Nacimientos")+
  scale_x_continuous(breaks = seq(1990,2016,4))+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 35, vjust=0.5),
        panel.grid.minor = element_blank())



data %>% ggplot(aes(año,mortalidad, colour=país))+
  geom_point(size=1.7)+
  geom_line(size=.1)+
  labs(title="Tasa de mortalidad",
       subtitle="Fallecidos menores de cinco años por cada mil nacidos vivos", 
       caption="Fuente: World Bank Data", 
       y="Número de niños fallecidos")+
  scale_x_continuous(breaks = seq(1990,2016,4))+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 35, vjust=0.5),
        panel.grid.minor = element_blank())


data %>% ggplot(aes(año,sida, colour=país))+
  geom_point(size=1.7)+
  geom_line(size=.1)+
  labs(title="Tasa de enfernos por VIH",
       subtitle="Seropositivos como porcentaje de la población entre 15 y 49 años", 
       caption="Fuente: World Bank Data", 
       y="Cantidad de sidosos")+
  scale_x_continuous(breaks = seq(1990,2016,4))+
  theme(axis.text.x = element_text(angle = 35, vjust=0.5),
        panel.grid.minor = element_blank())


data %>% ggplot(aes(año,forestal, colour=país))+
  geom_point(size=1.7)+
  geom_line(size=.1)+
  labs(title="Área forestal",
       subtitle="En kilómetros cuadrados", 
       caption="Fuente: World Bank Data", 
       y="m2 forestales")+
  scale_x_continuous(breaks = seq(1990,2016,4))+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 35, vjust=0.5),
        panel.grid.minor = element_blank())


data %>% ggplot(aes(año,movil, colour=país))+
  geom_point(size=1.7)+
  geom_line(size=.1)+
  labs(title="Uso de móviles",
       subtitle="Abonados a telefonía móvil por cada 100 personas", 
       caption="Fuente: World Bank Data", 
       y="Cantidad de abonados")+
  scale_x_continuous(breaks = seq(1990,2016,4))+
  theme(axis.text.x = element_text(angle = 35, vjust=0.5),
        panel.grid.minor = element_blank())


data %>% ggplot(aes(año,inversion/1000000, colour=país))+
  geom_point(size=1.7)+
  geom_line(size=.1)+
  labs(title="Inversión extranjera directa neta",
       subtitle="Millones de dólares. Dólares corrientes.", 
       caption="Fuente: World Bank Data", 
       y="Dólares")+
  scale_x_continuous(breaks = seq(1990,2016,4))+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 35, vjust=0.5),
        panel.grid.minor = element_blank())


data %>% ggplot(aes(año,ayudas/1000000, colour=país))+
  geom_point(size=1.7)+
  geom_line(size=.1)+
  labs(title="Ayuda externa",
       subtitle="Ayudas al desarrollo. Millones de dólares. Dólares corrientes.", 
       caption="Fuente: World Bank Data", 
       y="Dólares")+
  scale_x_continuous(breaks = seq(1990,2016,4))+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 35, vjust=0.5),
        panel.grid.minor = element_blank())


