library(readxl)
library(readr)
library(ss3diags)
library(dplyr)

out_SOL <- read_csv(paste0("AMSY/Out_man/Out_novembre012021_SOL_ID.csv"))
out_MTS <- read_csv(paste0("AMSY/Out_man/Out_novembre012021_MTS_ID.csv"))
out_CTC <- read_csv(paste0("AMSY/Out_man/Out_ottobre262021_CTC_ID.csv"))
out_SJA <- read_csv(paste0("AMSY/Out_man/Out_ottobre262021_SJA_ID.csv"))

species <- "SJA"  
out <- out_SJA # put the output of species of interest

methods=c('Trust', 'diva','prop','ssa','pred')
amsy_kobe=NULL

for(i in 1:length(methods)){
 # cat(10)
  amsy_kobe2=data.frame(year= "2020", 
                        run= paste(species, methods[i], sep='_'),
                        type="fit",
                        iter=1,
                        stock= out$BBmsy.est[i],
                        harvest= out$FFmsy.est[i],
                        SSB=NA,
                        "F" =NA,
                        Recr =NA,
                        Catch=NA,
                        model= paste(species, methods[i], sep='_'))
                  
  amsy_kobe=rbind(amsy_kobe, amsy_kobe2)
}

##########################
# FINAL Kobe_plot AMSY runs together
##########################
source(paste0("R/plotkobe_fin.R"))
sspar(mfrow=c(1,1),plot.cex = 0.9)
plotKobe_fin(amsy_kobe,fill=T,joint=F,posterior="points",ylab="B/BMSY",xlab="F/FMSY", legendruns = TRUE)
dev.print(jpeg,paste0("Kobe_AMSY_final_" ,species, ".jpg"), width = 12, height = 8, res = 300, units = "in") 
