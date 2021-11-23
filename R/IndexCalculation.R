library(readxl)
library(tidyverse)

species='CTC'
species_name='Sepia_officinalis'

# Import Data
HaulBiomass <- read_excel(paste0("data/trust_indices/",species,"_haul.xlsx")) # Biomass Index by Haul
HaulBiomass$year=as.numeric(
  str_remove(
    str_remove(
      str_remove(
        str_remove(HaulBiomass$Survey, c("SOLEMON")), "_b"),"OTT"), "NOVEMBRE"))
names(HaulBiomass)[10:11]=c('lat', 'lon')

StrataWeight <- read.csv("data/StrataWeight.csv") # Area of strata


# Compute Index
Index=HaulBiomass[,c('year', 'Station','Stratum' ,'BiomIndex', 'SweptArea')]

calculate_index=function(survey_data, strata, source){
  
  survey_data$BiomRaw=survey_data$BiomIndex*survey_data$SweptArea # return from Biomass Index to raw weight of species in the haul
  
  survey_data=survey_data[!is.na(survey_data$BiomRaw),]
  
  survey_data=aggregate(list(Biomass=survey_data$BiomRaw, SweptArea=survey_data$SweptArea), 
                        by = list(year=survey_data$year, Stratum=survey_data$Stratum), 
                        FUN=sum) # summarize weight and swept area by year and stratum
  
  survey_data$BiomassStratum=survey_data$Biomass/survey_data$SweptArea # Stratum Index 
  
  survey_data=merge(survey_data, StrataWeight, by="Stratum") 
  
  survey_data$BiomassStratumWeighted=survey_data$BiomassStratum*survey_data$StratumWeight # Weight Stratum Index by the relative area of the stratum
  
  survey_data=aggregate(list(survey_data=survey_data$BiomassStratumWeighted), by=list(year=survey_data$year), FUN=sum) # Get Index
  
  survey_data$source=source
  
  return(survey_data)
  
}

Trust_index=calculate_index(Index, StrataWeight, 'Trust')


##### calculate alternative Indices based on new data

# get data
Index$BiomRaw=Index$BiomIndex*Index$SweptArea # return from Biomass Index to raw weight of species in the haul
swas=Index #store swept areas

methods_1=read_csv(paste0("data/results_biomass_model_coro/single_hauls/",species_name,"_reconstructed_hauls.csv"))
methods_1=methods_1[,c('station', 'diva', 'ssa' , 'prediction')]
methods_2=read_csv(paste0("data/results_biomass_model_coro/single_hauls/",tolower(species) ,"_index_BIproportional.csv"))
names(methods_2)[3]='proportional'
methods_all=merge(methods_1, methods_2)

sw_update=swas[swas$year==2019 & swas$Station%in% methods_all$station,c('Station', 'Stratum', 'SweptArea')]
names(sw_update)[1]='station'
methods_all=merge(methods_all, sw_update)
methods_all$year=2020

# create 2020
observed2020=Index[Index$year=='2020',]
observed2020=observed2020[observed2020$Station!='32',]


# function to combine data
new_data=function(observed, predicted, type){
  
  pred=predicted[,c('station', 'Stratum', 'SweptArea', type)]
  pred$year=2020
  names(pred)=c('Station', 'Stratum', 'SweptArea', 'BiomIndex', 'year')
  
  observed=observed%>%dplyr::select(names(pred))
  combined_data=rbind(observed, pred)
  return(combined_data)
  
  
}


# trust
trust=calculate_index(observed2020, StrataWeight, 'trust')

# diva
diva_data=new_data(observed2020, methods_all, 'diva')
diva=calculate_index(diva_data, StrataWeight, 'diva');diva

#ssa
ssa_data=new_data(observed2020, methods_all, 'ssa')
ssa=calculate_index(ssa_data, StrataWeight, 'ssa');ssa

#prediction
pred_data=new_data(observed2020, methods_all, 'prediction')
pred=calculate_index(pred_data, StrataWeight, 'pred');pred

#proportional
prop_data=new_data(observed2020, methods_all, 'proportional')
prop=calculate_index(prop_data, StrataWeight, 'prop');prop


# merge data
fulldat=rbind(Trust_index, diva,ssa,pred,prop)




### Prepare AMSY CPUE data ----
amsy_ID_template <- read_csv("AMSY/EU_Stocks_ID_template.csv")
methods=c('Trust', 'diva','prop','ssa','pred')

# define AMSY priors
resilience_qual=if(species=='CTC'){'Medium'}else if(species=="SJA"){'Medium'}else if(species=="SOL"){'Medium'}else if(species=="MTS"){'Medium'}
resilience_low=if(species=='CTC'){0.37}else if(species=="SJA"){NA}else if(species=="SOL"){0.33}else if(species=="MTS"){0.37}
resilience_high=if(species=='CTC'){0.84}else if(species=="SJA"){NA}else if(species=="SOL"){0.76}else if(species=="MTS"){0.84}
biom_year=if(species=='CTC'){2007}else if(species=="SJA"){2019}else if(species=="SOL"){2018}else if(species=="MTS"){2020}
biom_prior=if(species=='CTC'){'More than half'}else if(species=="SJA"){'Small'}else if(species=="SOL"){'About half'}else if(species=="MTS"){'About half'} # options: 'Very small' 'Small' 'About half' 'More than half' 'Close to unexploited'
creep=if(species=='CTC'){NA}else if(species=="SJA"){NA}else if(species=="SOL"){NA}else if(species=="MTS"){NA}

# cpue file
amsy_dat=NULL
amsy_id=NULL

for(i in 1:length(methods)){
  cat(i)
  
  # cpue
  
  base_dat=fulldat[fulldat$year<2020, c('year','survey_data')]
  
  finalyear_dat=fulldat[fulldat$year==2020 & fulldat$source==methods[i], c('year','survey_data')]
  
  amsy_stock=data.frame(Stock= paste(species, methods[i], sep='_'), 
                      Year=c(base_dat$year, finalyear_dat$year),
                      Catch=NA,
                      CPUE=c(base_dat$survey_data, finalyear_dat$survey_data))
  amsy_dat=rbind(amsy_dat, amsy_stock)
  
  # id
  amsy_id_i=amsy_ID_template
  
  amsy_id_i$CPUE_File=paste(species, 'CPUE.csv', sep='_')
  amsy_id_i$Stock=paste(species, methods[i], sep='_')
  amsy_id_i$Name=species_name
  amsy_id_i$StartYear=min(amsy_dat$Year)
  amsy_id_i$EndYear=max(amsy_dat$Year)
  amsy_id_i$Resilience=resilience_qual
  amsy_id_i$r.low=resilience_low
  amsy_id_i$r.hi=resilience_high
  amsy_id_i$Bk.yr=biom_year
  amsy_id_i$Bk.pr=biom_prior
  amsy_id_i$Bk.pr.low=NA
  amsy_id_i$Bk.pr.hi=NA
  amsy_id_i$e.creep=creep
  
  amsy_id=rbind(amsy_id, amsy_id_i)
    
  
}

write.csv(amsy_dat, file.path('AMSY', paste(species, 'CPUE.csv', sep='_')))

write.csv(amsy_id, file.path('AMSY', paste(species, 'ID.csv', sep='_')))


# some plots
ggplot()+
  geom_point(data=amsy_dat[amsy_dat$Year==2020,], aes(x=Year,y=CPUE, color=Stock))+
  geom_line(data=amsy_dat, aes(x=Year,y=CPUE, color=Stock))+
  ggtitle(species_name)+
  theme_bw()

ggsave(paste0('plots/',species ,'_index.jpeg'), width = 15, height = 10, units='cm')

ggplot()+
  geom_point(data=amsy_dat[amsy_dat$Year==2020,], aes(x=Year,y=CPUE, color=Stock))+
  geom_line(data=amsy_dat, aes(x=Year,y=CPUE, color=Stock))+
  scale_x_continuous(limits = c(2018,2020), breaks=seq(2018,2020,1))+
  ggtitle(species_name)+
  theme_bw()

ggsave(paste0('plots/',species ,'_index_lastyears.jpeg'), width = 15, height = 10, units='cm')


##### compile table with priors ####
xspecies=c("CTC", "MTS", "SJA", "SOL")
mypriors=NA
for(i in 1:length(xspecies)){
  
  species=xspecies[i]
  
  resilience_qual=if(species=='CTC'){'Medium'}else if(species=="SJA"){'Medium'}else if(species=="SOL"){'Medium'}else if(species=="MTS"){'Medium'}
  
  resilience_low=if(species=='CTC'){0.37}else if(species=="SJA"){NA}else if(species=="SOL"){0.33}else if(species=="MTS"){0.37}
  
  resilience_high=if(species=='CTC'){0.84}else if(species=="SJA"){NA}else if(species=="SOL"){0.76}else if(species=="MTS"){0.84}
  
  biom_year=if(species=='CTC'){2007}else if(species=="SJA"){2019}else if(species=="SOL"){2018}else if(species=="MTS"){2020}
  
  biom_prior=if(species=='CTC'){'More than half'}else if(species=="SJA"){'Small'}else if(species=="SOL"){'About half'}else if(species=="MTS"){'About half'} # options: 'Very small' 'Small' 'About half' 'More than half' 'Close to unexploited'
  
  creep=if(species=='CTC'){NA}else if(species=="SJA"){NA}else if(species=="SOL"){NA}else if(species=="MTS"){NA}
  
  mypriors=rbind(mypriors, data.frame(species, resilience_qual, resilience_low, resilience_high, biom_year, biom_prior))
  
  mypriors = mypriors[!is.na(mypriors$species),]
  
  
}

write.csv(mypriors, "AMSY/summary_priors.csv", row.names = F)

### CMSY ####

# catch file
xlanding=read.csv(paste0("data/landings/", species, ".csv" ))

xstocks=unique(amsy_dat$Stock)


cmsy_dat=data.frame(Stock=rep(xstocks, each=nrow(xlanding)),
           Year= rep(xlanding$year, length(xstocks)),
           ct=rep(xlanding$tons, length(xstocks)))

cmsy_dat=cmsy_dat%>%
  left_join(amsy_dat, by=c('Year', 'Stock'))%>%
  dplyr::select(Stock, 'yr'=Year,ct,'bt'=CPUE)


# id file
cmsy_ID_template <- read_csv("CMSY/Stocks_ID_template.csv")
cmsy_ID_template=cmsy_ID_template[1,]

# define CMSY extra priors
stb.low=if(species=='CTC'){0.6}else if(species=="SJA"){0.4}else if(species=="SOL"){'Medium'}else if(species=="MTS"){'Medium'}
stb.high=if(species=='CTC'){0.9}else if(species=="SJA"){0.85}else if(species=="SOL"){0.33}else if(species=="MTS"){0.37}

endb.low=if(species=='CTC'){0.21}else if(species=="SJA"){0.1}else if(species=="SOL"){'Medium'}else if(species=="MTS"){'Medium'}
endb.high=if(species=='CTC'){0.7}else if(species=="SJA"){0.4}else if(species=="SOL"){0.33}else if(species=="MTS"){0.37}

int.yr=if(species=='CTC'){2004}else if(species=="SJA"){2012}else if(species=="SOL"){'Medium'}else if(species=="MTS"){'Medium'}
intb.low=if(species=='CTC'){0.1}else if(species=="SJA"){0.1}else if(species=="SOL"){'Medium'}else if(species=="MTS"){'Medium'}
intb.high=if(species=='CTC'){0.4}else if(species=="SJA"){0.4}else if(species=="SOL"){0.33}else if(species=="MTS"){0.37}


creep=if(species=='CTC'){NA}else if(species=="SJA"){NA}else if(species=="SOL"){NA}else if(species=="MTS"){NA}

cmsy_id=NULL

for(i in 1:length(methods)){
  cat(i)
  
  # id
  cmsy_id_i=cmsy_ID_template
  
  cmsy_id_i$Stock=paste(species, methods[i], sep='_')
  cmsy_id_i$Name=species_name
  cmsy_id_i$StartYear=min(cmsy_dat$yr)
  cmsy_id_i$MinOfYear=min(cmsy_dat$yr)
  cmsy_id_i$EndYear=max(cmsy_dat$yr)
  cmsy_id_i$MaxOfYear=max(cmsy_dat$yr)
  cmsy_id_i$Resilience=resilience_qual
  cmsy_id_i$r.low=resilience_low
  cmsy_id_i$r.hi=resilience_high
  
  cmsy_id_i$stb.low=stb.low
  cmsy_id_i$stb.hi=stb.high
  
  cmsy_id_i$endb.low=endb.low
  cmsy_id_i$endb.hi=endb.high
  
  cmsy_id_i$int.yr=int.yr
  cmsy_id_i$intb.low=intb.low
  cmsy_id_i$intb.hi=intb.high
  
 
  cmsy_id_i$e.creep=creep
  
  cmsy_id=rbind(cmsy_id, cmsy_id_i)
  
  
}

write.csv(cmsy_dat, file.path('CMSY', paste(species, 'CPUE.csv', sep='_')))

write.csv(cmsy_id, file.path('CMSY', paste(species, 'ID.csv', sep='_')))

