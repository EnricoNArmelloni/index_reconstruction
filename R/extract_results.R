species="spr"
need_raising="T"
raising=function(xb, xkm){
  v=(xb*xkm)/100
  return(v)} # function to rise kg/km2 to absolute tonnes
area=91300 # area of the basin


ress=data.frame(BBmsy.retrospective)
colnames(ress)="BBmsy"
ress$year=rownames(ress)
ress2=data.frame(FFmsy)
ress2$year=rownames(ress2)
merge(ress, ress2, by="year", all.x = T)



write.csv(merge(ress, ress2, by="year", all.x = T),paste0("res_",species, ".csv"))


res_pars=data.frame(r=rv.est,  
                    Fmsy=rv.est/2, 
                    Bbmsy=BBmsy.end ,
                    B=cpuet.median[nyr], 
                    Bmsy=(kqv.est/2), 
                    Fcur = cqt.median[nyr-1]/cpuet.median[nyr-1])

if(need_raising=="T"){
  res_pars$B=raising(res_pars$B, area)
  res_pars$Bmsy=raising(res_pars$Bmsy, area)
}

write.csv(res_pars,paste0("pars_res",species,".csv"))
