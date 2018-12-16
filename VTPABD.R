library(rsdmx)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(eurostat)


# import VTPABD data
vtpabd <- read.csv("VTPABD.csv", stringsAsFactors = FALSE, sep=",")
vtpabd <- vtpabd %>%        
        mutate(time=parse_date_time(time, "y"))

# Average earnings (monthly)
#Sector | Sex | Type
S3R0050_M3060862_3<- readSDMX(providerId = "LSD", resource = "data", flowRef = "S3R0050_M3060862_3", dsd = TRUE)
S3R0050_M3060862_3 <- as.data.frame(S3R0050_M3060862_3 , labels = TRUE)


#HICP (2015 = 100) - annual data (average index and rate of change) 
prc_hicp_aind <- get_eurostat("prc_hicp_aind", stringsAsFactors = FALSE)



df_1 <- S3R0050_M3060862_3 %>%        
        mutate(time=parse_date_time(LAIKOTARPIS, "y")) %>% 
        filter(darboM3060321_label.lt=="Bruto",
               Lytis=="0",
               year(time)>="2006") %>%
        select(Ekon_sektorM2040802_label.en, time, obsValue)%>%
        spread(Ekon_sektorM2040802_label.en, obsValue)

df <- full_join(df_1, vtpabd, by="time") %>%
        mutate(`Privatusis sektorius su iį`=.[,2]/.[1,2]*100,
               `Viešasis sektorius`=.[,3]/.[1,3]*100,
               `Šalies ūkis su iį`=.[,4]/.[1,4]*100,
               `Valstybės tarnautojų pareiginės algos bazinis dydis`=.[,5]/.[1,5]*100) %>% 
        select(1,6:9)%>%
        gather(Grupė,values, 2:5)

jpeg("./figures/VDU_VTD_algų_indeksai.jpeg", width = 9, height = 6, units = 'in', res = 200)
a <- ggplot(df, aes(x=time, y=values, color=Grupė))+
        geom_line(size=1.2)+
        scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")+
        scale_y_continuous(breaks = seq(0,200, by=10))+
        scale_colour_brewer(palette ="Set1")+
        labs(title="VDU ir Valstybės tarnautojų pareiginės algos bazinis dydžio indeksai, 2006=100", 
             subtitle="Šaltinis: VTD, LSD. Skaičiavimai: Lithuanian-Economy.net" , 
             y="Indeksas", 
             x="Metai")+
        theme(plot.title = element_text(hjust = 0.5, face="bold"))+
        theme(legend.position = "bottom")+
        theme(legend.title=element_blank(),plot.title = element_text(hjust = 0))
a
dev.off()


inf <- prc_hicp_aind %>%
        filter(geo=="LT",
               year(time)>="2006",
               coicop=="CP01",
               unit=="INX_A_AVG")%>%
        select(time, values)%>%
        rename(inflation=values) %>%
        mutate(time=as.POSIXct(time))

df_1 <- S3R0050_M3060862_3 %>%        
        mutate(time=parse_date_time(LAIKOTARPIS, "y")) %>% 
        filter(darboM3060321_label.lt=="Bruto",
               Lytis=="0",
               year(time)>="2006") %>%
        select(Ekon_sektorM2040802_label.en, time, obsValue)%>%
        spread(Ekon_sektorM2040802_label.en, obsValue)

df <- full_join(df_1, vtpabd, by="time")
df <- full_join(df, inf, by="time") 
        
df <- df %>%
        mutate(`Privatusis sektorius su iį`=(.[,2]/.[1,2])/(.[,6]/.[1,6])*100,
               `Viešasis sektorius`=(.[,3]/.[1,3])/(.[,6]/.[1,6])*100,
               `Šalies ūkis su iį`=(.[,4]/.[1,4])/(.[,6]/.[1,6])*100,
               `Valstybės tarnautojų pareiginės algos bazinis dydis`=(.[,5]/.[1,5])/(.[,6]/.[1,6])*100)%>% 
        select(1,7:10)%>%
        gather(Grupė,values, 2:5)

jpeg("./figures/Realus_VDU_VTD_algų_indeksai.jpeg", width = 9, height = 6, units = 'in', res = 200)
b <- ggplot(df, aes(x=time, y=values, color=Grupė))+
        geom_line(size=1.2)+
        scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")+
        scale_y_continuous(breaks = seq(0,200, by=10))+
        scale_colour_brewer(palette ="Set1")+
        labs(title="VDU ir Valstybės tarnautojų pareiginės algos bazinis dydžio indeksai įvertinus infliaciją, 2006=100", 
             subtitle="Šaltinis: VTD, LSD, Eurostat. Skaičiavimai: Lithuanian-Economy.net" , 
             y="Indeksas", 
             x="Metai")+
        geom_hline(yintercept=100)+
        theme(plot.title = element_text(hjust = 0.5, face="bold"))+
        theme(legend.position = "bottom")+
        theme(legend.title=element_blank(),plot.title = element_text(hjust = 0))
b
dev.off()

jpeg("./figures/Nominalus_Realus_VDU_VTD_algų_indeksai.jpeg", width = 18, height = 5, units = 'in', res = 200)
grid.arrange(a, b, ncol=2)
dev.off()


jpeg("./figures/VTPABD.jpeg", width = 9, height = 6, units = 'in', res = 200)
ggplot(vtpabd, aes(x=time, y=VTPABD))+
        geom_line(size=1.1)+
        scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")+
        scale_y_continuous(breaks = seq(0,200, by=2))+
        labs(title="Valstybės tarnautojų pareiginės algos bazinis dydis", 
             subtitle="Šaltinis: VTD. Skaičiavimai: Lithuanian-Economy.net" , 
             y="Euro", 
             x="Metai")+
        theme(plot.title = element_text(hjust = 0.5, face="bold"))+
        theme(legend.position = "bottom")+
        theme(legend.title=element_blank(),plot.title = element_text(hjust = 0))
dev.off()
