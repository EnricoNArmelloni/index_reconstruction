# Define functions


# Trajectories extraction ####
trajectories_fun=function(Bdat, Fdat, Xstock){
  
  Btrajectory=data.frame(Bdat)
  colnames(Btrajectory)="BBmsy"
  Btrajectory$year=rownames(Btrajectory)
  
  Ftrajectory=data.frame(Fdat)
  Ftrajectory$year=rownames(Ftrajectory)
  
  s_trajectories=merge(Btrajectory, Ftrajectory, by="year", all.x = T)
  s_trajectories$species=stock
  
  write.csv(s_trajectories,
            paste0("Stock_trajectories/Stock_trajectories_",Xstock, ".csv"))
  
}  

# Parameters extraction ####
parameters_fun=function(r_dat, b_dat, cpue_dat, k_dat, f_dat, Xstock, xarea){
  
  s_parameters=data.frame(r=r_dat,  
                          Fmsy=r_dat/2, 
                          Bbmsy=b_dat ,
                          B=cpue_dat, 
                          Bmsy=(k_dat/2), 
                          Fcur = f_dat)
  s_parameters$species=Xstock
  
  if(need_raising=="T"){
    s_parameters$B=raising(s_parameters$B, area)
    s_parameters$Bmsy=raising(s_parameters$Bmsy, area)
  }
  
  write.csv(s_parameters,paste0("Stock_parameters/Stock_parameters_",stock,".csv"))
  
}
  

# rise kg/km2 to absolute tonnes ####
raising=function(xb, xkm){
  v=(xb*xkm)/100
  return(v)} 







