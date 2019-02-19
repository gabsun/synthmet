# synthmet_createvars.R
#
# Gab Abramowitz gabsun at gmail dot com 2019
#
create_synthmet = function(index,projectname,tstepsize,SWdown_switch,Rainf_switch,
  LWdown_switch,Tair_switch,Qair_switch,CO2air_switch,Wind_switch,VegType_switch){







  }
create_SWdown = function(SWdown_switch,tstepsize,tsteps){
  # creates synthetic SWdown time series
  tsinday = 24*3600 / tstepsize # time steps in a day
  tssize = (2*pi) / tsinday # time step size, in radians
  # Calculate "full sun" time series
  fullSWdown = 1200 * pmax (sin(c(0:(tsteps-1)) * tssize), 0)
  # initialise SWdown array:
  SWdown = array(NA,dim=c(length(SWdown_switch),tsteps))

  for(s in 1:(length(SWdown_switch))){
    SWdown[s,] = pmax( (fullSWdown - (1200-SWdown_switch[s])), 0)
  }
  return(SWdown)
}
