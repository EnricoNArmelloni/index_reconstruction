library(readxl)
library(tidyverse)
setwd("~/CNR/Solemon/Lavori/2021/Coro/Phase2_assessment")
species='CTC'
species_name='Sepia officinalis'

contribute <- read_excel("~/CNR/Solemon/Lavori/2021/Coro/Phase2_assessment/data/Output contribute CaleXBiomIndex.xlsx", sheet = species_name)%>%
  dplyr::mutate(Station=as.character(Station))
names(contribute)[2]='contribution'

# Import Data
HaulBiomass <- read_excel(paste0("data/trust_indices/",species,"_haul.xlsx")) # Biomass Index by Haul
HaulBiomass$year=as.numeric(
  str_remove(
    str_remove(
      str_remove(
        str_remove(HaulBiomass$Survey, c("SOLEMON")), "_b"),"OTT"), "NOVEMBRE"))
names(HaulBiomass)[c(10:11)]=c( 'lat', 'lon')

# Compute Index
HaulBiomass=HaulBiomass[,c('year', 'Station','Stratum' ,'BiomIndex', 'SweptArea')]
HaulBiomass=HaulBiomass[HaulBiomass$year!=2020,]

HaulBiomass=HaulBiomass%>%dplyr::group_by(Station)%>%
  dplyr::summarise(Biom=mean(BiomIndex))%>%
  dplyr::mutate(contr_biom=Biom/sum(Biom))


comparison_tot=left_join(contribute, HaulBiomass, by='Station')


library(ggrepel)
ggplot(aes(x=contribution, y=contr_biom, label=Station), data=comparison_tot)+
  geom_point()+
  geom_smooth(method = "lm", se = T)+
  geom_label_repel()+
  xlab("Model contribution")+
  ylab("Biomass mean contribution")+
  ggtitle(species_name)

ggsave(file.path('results', paste0(species, "_datacontribution.png")),
       height = 10, width = 20,units='cm')

