# synthmet_driver.R
#
# Driver script for synthetic met file creation (e.g. for PLUMBER2)
#
# Gab Abramowitz gabsun at gmail dot com 2019
#
# Script will create collection of files with variables:
# SWdown, LWdown, Tair, Qair, Precip, Wind, CO2air, VegType,

library(parallel)
source('synthmet_createvars.R') # contains variable creation functions
source('synthmet_writefile.R') # contains variable creation functions

tstepsize = 1800 # in seconds; must be < 3 hourly
nyears = 4 # year length of each forcing file (min 4 years)
SWdown_switch = c(200,1200) # low or high daily max value, also determines daylength (max 1200)
Precip_switch = c('regular','drizzle','heavy','drydown')
LWdown_switch = c(1.2,0.8) # high or low multiplier of LWdown from Tair,Qair
Tair_switch = list(values=list(c(260,293),c(303,318),c(260,293,260),c(293,313,293)),
  type=c('sine','sine','step','step'))
Qair_switch = c(5,98) # high or low based on Tair
CO2air_switch = c(350,550)
Wind_switch = list(c(0),c(2),c(20),c(0,2,10,20))
VegType_switch = list(vtype=c('grass','tree','tree'),type=c('sine','step','step'),
  values=list(c(0,3),c(1),c(6))) # determines lai, veg height and reference height

tsteps = nyears*365*(24*3600)/tstepsize # note no leaps years!

SWdown = create_SWdown(SWdown_switch,tstepsize,tsteps)
Tair = create_Tair(Tair_switch,tstepsize,tsteps)
CO2air = create_CO2air(CO2air_switch,tsteps)
Wind = create_Wind(Wind_switch,tsteps)
Precip = create_Precip(Precip_switch,tstepsize,tsteps)
Psurf = create_Psurf(tsteps)

#par(mfcol=c(4,2),mar=c(3,4,3,0.5),oma=c(0,0,0,1),
#  mgp=c(2.5,0.7,0),ps=16,tcl=-0.4)
#plot(SWdown$dat[1,1:1000],type='l')
#plot(Precip$dat[1,1:1000],type='l')
#plot(Tair$dat[1,1:1000],type='l')
#plot(Wind$dat[1,1000],type='l')

# Build index array to send to apply/parApply writing of files:
indexarray = array(NA,dim=c(8,length(SWdown_switch),length(Precip_switch),
  length(LWdown_switch),length(Tair_switch$values),length(Qair_switch),length(CO2air_switch),
  length(Wind_switch),length(VegType_switch)))
for(SW in 1:length(SWdown_switch)){
  for(Pr in 1:length(Precip_switch)){
    for(LW in 1:length(LWdown_switch)){
      for(Ta in 1:length(Tair_switch$values)){
        for(Qa in 1:length(Qair_switch)){
          for(CO2 in 1:length(CO2air_switch)){
            for(W in 1:length(Wind_switch)){
              for(Veg in 1:length(VegType_switch$values)){
                indexarray[1,SW,Pr,LW,Ta,Qa,CO2,W,Veg] = SW # index values for SWdown
                indexarray[2,SW,Pr,LW,Ta,Qa,CO2,W,Veg] = Pr # index values for Prength
                indexarray[3,SW,Pr,LW,Ta,Qa,CO2,W,Veg] = LW # index values for LWdown
                indexarray[4,SW,Pr,LW,Ta,Qa,CO2,W,Veg] = Ta # index values for Tair
                indexarray[5,SW,Pr,LW,Ta,Qa,CO2,W,Veg] = Qa # index value for Qair
                indexarray[6,SW,Pr,LW,Ta,Qa,CO2,W,Veg] = CO2 # index value for CO2air
                indexarray[7,SW,Pr,LW,Ta,Qa,CO2,W,Veg] = W # index value for Wind
                indexarray[8,SW,Pr,LW,Ta,Qa,CO2,W,Veg] = Veg # index value for VegType
              }
            }
          }
        }
      }
    }
  }
}
# Make virtual cluster
cl = makeCluster(getOption('cl.cores', 16))

# Now create and write files through apply/parApply:
err = parApply(cl=cl,indexarray,MARGIN=c(2,3,4,5,6,7,8,9),write_synthmet,tstepsize,tsteps,
  LWdown_switch,Qair_switch,VegType_switch,SWdown,Tair,CO2air,Wind,Precip,Psurf)
#err = apply(indexarray,MARGIN=c(2,3,4,5,6,7,8),write_synthmet,tstepsize,tsteps,
#  LWdown_switch,Qair_switch,VegType_switch,SWdown,Tair,CO2air,Wind,Precip,Psurf)

# stop cluster
stopCluster(cl)

if(!any(err)){
  print('Done.')
}else{
  print('Some failed!')
}
