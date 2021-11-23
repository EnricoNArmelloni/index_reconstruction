library(tidyverse)

species='CTC'

xfile=list.files(path="CMSY", pattern=paste0("*_", species,"_ID.csv"))

xfile=read.csv(file=paste0("CMSY/", xfile))
xfile=xfile[xfile$Group!="Group",]

names(xfile)

# F
F_tr=xfile[,grep("F.Fms", names(xfile))]
F_tr=F_tr[,3:ncol(F_tr)]
F_tr$Stock=xfile$Stock

F_tr=pivot_longer(F_tr, -Stock)%>%
  dplyr::mutate(yr=str_remove(name, "F.Fmsy"))%>%
  dplyr::mutate(yr=as.numeric(ifelse(yr<= 20, paste0(20,yr), paste0(19,yr))))

F_tr=F_tr[F_tr$value!=0,]

# B
last_col_F=max(grep("F.Fms", names(xfile)))
B_tr=xfile[,(last_col_F+1):ncol(xfile)]
B_tr$Stock=xfile$Stock
B_tr$Bmsy=xfile$Bmsy


B_tr=pivot_longer(B_tr, -c(Stock, Bmsy))%>%
  dplyr::mutate(yr=str_remove(name, "B"))%>%
  dplyr::mutate(yr=as.numeric(ifelse(yr<= 20, paste0(20,yr), paste0(19,yr))))%>%
  dplyr::mutate(value=as.numeric(value)/as.numeric(Bmsy))%>%
  dplyr::select(-Bmsy)

B_tr=B_tr[B_tr$yr>= min(F_tr$yr),]

# Plot
library(ggplot2)
  ggsave(plot=ggpubr::ggarrange(
    
    ggplot(data=B_tr)+
      geom_line(aes(x=yr, y=value, color=Stock))+
      ylab(expression('B/B'[MSY]))+
      geom_hline(yintercept = 1, linetype=2)+
      theme_bw()
    ,
    ggplot(data=F_tr)+
      geom_line(aes(x=yr, y=as.numeric(value), color=Stock))+
      ylab(expression('F/F'[MSY]))+
      theme_bw()+
      geom_hline(yintercept = 1, linetype=2),
    common.legend = T
    
    ,nrow=2),
    paste0("CMSY/",species,'_cmsy_trajectories.png'),
    width = 20,
    height = 15,
    units='cm')

