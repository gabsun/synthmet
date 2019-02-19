# synthmet_driver.R
#
# Driver script for synthetic met file creation (e.g. for PLUMBER2)
#
# Gab Abramowitz gabsun at gmail dot com 2019
#
# Script will create collection of files with variables:
# SWdown, LWdown, Tair, Qair, Precip, Wind, CO2air, VegType,

source(synthmet_createvars.R) # contains variable creation functions
projectname = 'PLUMBER2' # prefix on created filenames

tstepsize = 1800 # in seconds; must be < 3 hourly
nyears = 4 # year length of each forcing file
SWdown_switch = c(200,1200) # low or high daily max value, also determines Prength
Rainf_switch = c('regular','drizzle','heavy','drydown')
LWdown_switch = c(1.2,0.8) # high or low multiplier of LWdown from Tair,Qair
Tair_switch = list(c(260),c(285),c(313),c(260,285),c(285,260),c(285,313))
Qair_switch = c(5,98) # high or low based on Tair
CO2air_switch = c(350,550)
Wind_switch = c(2,20,0)
VegType_switch = c('tree','grass')

tsteps = nyears*365*(24*3600)/tstepsize

SWdown = create_SWdown(SWdown_switch,tstepsize,tsteps)

SPINUP?

# Build index array to send to apply/parApply writing of files:
indexarray = array(NA,dim=c(8,length(SWdown_switch),length(Rainf_switch),
  length(LWdown_switch),length(Tair_switch),length(Qair_switch),length(CO2air_switch),
  length(Wind_switch),length(VegType_switch)))
for(SW in 1:length(SWdown_switch)){
  for(Pr in 1:length(Rainf_switch)){
    for(LW in 1:length(LWdown_switch)){
      for(Ta in 1:length(Tair_switch)){
        for(Qa in 1:length(Qair_switch)){
          for(CO2 in 1:length(CO2air_switch)){
            for(W in 1:length(Wind_switch)){
              for(Veg in 1:length(VegType_switch)){
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
# Now create and write files through apply/parApply:
apply(indexarray,MARGIN=c(2,3,4,5,6,7,8),create_synthmet,projectname,tstepsize,SWdown_switch,
  Rainf_switch,LWdown_switch,Tair_switch,Qair_switch,CO2air_switch,Wind_switch,VegType_switch)
print('Done.')
