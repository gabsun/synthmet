# synthmet_createvars.R
#
# Gab Abramowitz gabsun at gmail dot com 2019
#

Rel2SpecHum = function(relHum,tk,PSurf){
	# Converts relative humidity to specific humidity.
	# tk - T in Kelvin; PSurf in Pa; relHum as %
	tempC = tk - 273.15
	# Sat vapour pressure in Pa
	esat = 610.78*exp( 17.27*tempC / (tempC + 237.3) )
	# Then specific humidity at saturation:
	ws = 0.622*esat/(PSurf - esat)
	# Then specific humidity:
	specHum = (relHum/100) * ws
	return(specHum)
}

create_LWdown = function(LWdown_multiplier,RH_const,Tair){
  # Synthesize LWdown from T and RH:
  satvapres = 611.2*exp(17.67*((Tair-273.15)/(Tair-29.65)))
  vapres = pmax(5,RH_const)/100*satvapres
  LWdown = 2.648*Tair + 0.0346*vapres - 474
  LWdown = LWdown*LWdown_multiplier
  att_text = paste('Linear synthesis following Abramowitz et al (2012) with',
    'multiplier of',LWdown_multiplier,sep=' ')
  label = paste0('L',LWdown_multiplier)
  units = 'W/m^2'
  returnvals = list(dat=LWdown,lab=label,att=att_text,units=units)
  return(returnvals)
}

create_Qair = function(RH_const,Tair,PSurf){
  # Convert RH to specific H, using T and PSurf:
  Qair = Rel2SpecHum(RH_const,Tair,PSurf)
  att_text = paste('Fixed relative humidity of',RH_const,
    'expressed as specific humidity.',sep=' ')
  label = paste0('Q',RH_const)
  units = 'kg/kg'
  returnvals = list(dat=Qair,lab=label,att=att_text,units=units)
  return(returnvals)
}

create_SWdown = function(SWdown_switch,tstepsize,tsteps){
  # Creates synthetic SWdown time series
  tsinday = 24*3600 / tstepsize # time steps in a day
  att_text = c() # attribute text for SWdown
  label = c() # SWdown section of file label
  units = c()
  # Initialise SWdown array:
  SWdown = array(NA,dim=c(length(SWdown_switch),tsteps))
  for(s in 1:(length(SWdown_switch))){ # over each SWdown max limit
    # First calculate number of timesteps of sunshine in a day (min of 4 hrs):
    sunsteps = ceiling( (tsinday / 2) * max((SWdown_switch[s]/1200),1/3) )
    sunhours = (sunsteps/tsinday) * 24
    # And number of night time time steps:
    darksteps = tsinday - sunsteps
    # Now construct sine wave for daytime time steps:
    tssize_rad = (2*pi) / sunsteps # time step size, in radians
    SWdown_onedaytime = SWdown_switch[s] / 2 * (sin((c(1:sunsteps) * tssize_rad - pi/2))+1)
    # Dtermine number of timesteps before dawn, and number after sunset:
    pre_dawn_tsteps = c(1:ceiling(darksteps/2))
    post_sunset_tsteps = darksteps - pre_dawn_tsteps
    # Construct a 24 hour cycle:
    SWdown_24hr = c(pre_dawn_tsteps*0,SWdown_onedaytime,post_sunset_tsteps*0)
    ndays = tsteps / tsinday # number of days
    # Repeat daily cycle for whole timeseries
    SWdown[s,] = rep(SWdown_24hr,times=ndays)
    att_text[s] = paste('Sinusoidally varying for',sunhours,'hours per day,',
      'maximum value',SWdown_switch[s],'W/m^2',sep=' ')
    label[s] = paste0('S',SWdown_switch[s])
    units[s] = 'W/m^2'
  }
  returnvals = list(dat=SWdown,lab=label,att=att_text,units=units)
  return(returnvals)
}

create_Tair = function(Tair_switch,tstepsize,tsteps){
  # Creates synthetic Tair time series
  # Initialise Tair array:
  Tair = array(NA,dim=c(length(Tair_switch$values),tsteps))
  att_text = c() # attribute text for Tair
  label = c() # Tair section of file label
  units = c()
  for (t in 1:length(Tair_switch$values)){
    if(length(Tair_switch$values[[t]]) == 1){ # i.e. constant temperature
      Tair[t,] = Tair_switch$values[[t]][1] # entire time series this temperature
      att_text[t] = paste('Fixed temperature at',Tair_switch$values[[t]][1],'K',sep=' ')
      label[t] = paste0('T',Tair_switch$values[[t]][1])
    }else if((Tair_switch$type[t] == 'sine') & (length(Tair_switch$values[[t]]) == 2)){
      # Sinusoidally varying temp daily with min/max value given:
      tsinday = 24*3600 / tstepsize # time steps in a day
      tssize_rad = (2*pi) / tsinday # time step size, in radians
      sineheight = abs(Tair_switch$values[[t]][2]-Tair_switch$values[[t]][1])
      # multiply [0,2] sine curve by sineheight/2, and add low temp offset:
      Tair[t,] = (sineheight / 2) *
        (sin((c(1:tsteps) * tssize_rad + pi))+1) + min(Tair_switch$values[[t]])
      att_text[t] = paste('Sinusoidally varying between',
        Tair_switch$values[[t]][1],'and',Tair_switch$values[[t]][2],'K',sep=' ')
      label[t] = paste0('TSi',paste(Tair_switch$values[[t]],collapse=''))
    }else if((Tair_switch$type[t] == 'step') & (length(Tair_switch$values[[t]]) == 2)){
      step_length = ceiling(tsteps / length(Tair_switch$values[[t]]))
      Tair[t,1:step_length] = Tair_switch$values[[t]][1] # first temp value
      Tair[t,(step_length+1):tsteps] = Tair_switch$values[[t]][2] # 2nd temp value
      att_text[t] = paste('Step changes between',
        Tair_switch$values[[t]][1],'and',Tair_switch$values[[t]][2],'K',sep=' ')
      label[t] = paste0('TSt',paste(Tair_switch$values[[t]],collapse=''))
    }else if((Tair_switch$type[t] == 'step')&  (length(Tair_switch$values[[t]]) > 2)){
      step_length = ceiling(tsteps / length(Tair_switch$values[[t]]))
      Tair[t,1:step_length] = Tair_switch$values[[t]][1] # first temp value
      for(s in 2:( length(Tair_switch$values[[t]]) - 1 ) ){
        Tair[t,((step_length*(s-1))+1):(step_length*s)] = Tair_switch$values[[t]][s]
      }
      s = length(Tair_switch$values[[t]])
      Tair[t,((step_length*(s-1))+1):tsteps] = Tair_switch$values[[t]][s]
      att_text[t] = paste('Step changes of',paste(Tair_switch$values[[t]],collapse=' '),'K',sep=' ')
      label[t] = paste0('TSt',paste(Tair_switch$values[[t]],collapse=''))
    }else{
      stop('Don\'t know what to do with 3+ dimensional temperature speification.')
    }
    units[t] = 'K'
  }
  returnvals = list(dat=Tair,lab=label,att=att_text,units=units)
  return(returnvals)
}

create_Wind = function(Wind_switch,tsteps){
  # Initialise Wind array:
  Wind = array(NA,dim=c(length(Wind_switch),tsteps))
  att_text = c() # attribute text for Tair
  label = c() # Tair section of file label
  units = c()
  for(w in 1:(length(Wind_switch))){ # over each different windspeed option
    if(length(Wind_switch[[w]]) == 1){ # i.e. constant wind speed
      Wind[w,] = Wind_switch[[w]][1] # entire time series this wind speed
      att_text = paste('Fixed windspeed at',Wind_switch[[w]][1],'m/s',sep=' ')
      label[w] = paste0('W',Wind_switch[[w]][1])
    }else if(length(Wind_switch[[w]]) == 2){ # 2 windspeed steps
      step_length = ceiling(tsteps / length(Wind_switch[[w]]))
      Wind[w,1:step_length] = Wind_switch[[w]][1] # first windsp value
      Wind[w,(step_length+1):tsteps] = Wind_switch[[w]][2] # 2nd windsp value
      att_text[w] = paste('Step changes of',paste(Wind_switch[[w]],collapse=' '),'m/s',sep=' ')
      label[w] = paste0('WSt',paste(Wind_switch[[w]],collapse=''))
    }else if(length(Wind_switch[[w]]) > 2){ # more than 2 windspeed steps
      step_length = ceiling(tsteps / length(Wind_switch[[w]]))
      Wind[w,1:step_length] = Wind_switch[[w]][1] # first windsp value
      for(s in 2:( length(Wind_switch[[w]]) - 1 ) ){
        Wind[w,((step_length*(s-1))+1):(step_length*s)] = Wind_switch[[w]][s]
      }
      s = length(Wind_switch[[w]])
      Wind[w,((step_length*(s-1))+1):tsteps] = Wind_switch[[w]][s]
      att_text[w] = paste('Step changes of',paste(Wind_switch[[w]],collapse=' '),'m/s',sep=' ')
      label[w] = paste0('WSt',paste(Wind_switch[[w]],collapse=''))
    }
    units[w] = 'm/s'
  }
  returnvals = list(dat=Wind,lab=label,att=att_text,units=units)
  return(returnvals)
}

create_Precip = function(Precip_switch,tstepsize,tsteps){
  # Creates synthetic Precip time series
  # Initialise Precip array:
  Precip = array(NA,dim=c(length(Precip_switch),tsteps))
  att_text = c() # attribute text for Tair
  label = c() # Tair section of file label
  units = c()
  tsinday = 24*3600 / tstepsize # time steps in a day
  ndays = tsteps / tsinday # number of days
  for(p in 1:(length(Precip_switch))){
    if(Precip_switch[p]=='regular'){ # 11mm over 6 hours twice a week, 1144mm annual total
      raining_tsteps_on_rain_day = ceiling(1/4 * tsinday)
      dry_tsteps_on_rain_day = tsinday - raining_tsteps_on_rain_day
      half_dry_steps = ceiling(dry_tsteps_on_rain_day/2)
      rain_tstep_amt = 11 / raining_tsteps_on_rain_day / tstepsize # in mm/s
      # fill week with: dry day, early morning rain day, dry day x3, day rain, dry day
      dryday = rep(0,times=tsinday)
      predawnrainday = c( rep(rain_tstep_amt,times=raining_tsteps_on_rain_day),
        rep(0,times=dry_tsteps_on_rain_day) )
      dayrainday = c( rep(0,times=half_dry_steps), rep(rain_tstep_amt,times=raining_tsteps_on_rain_day),
        rep(0,times=(dry_tsteps_on_rain_day-half_dry_steps)))
      week = c(dryday,predawnrainday,dryday,dryday,dryday,dayrainday,dryday)
      weeksinalltime = floor(tsteps / length(week))
      remaining_tsteps = tsteps - (weeksinalltime*7*tsinday)
      Precip[p,1:(weeksinalltime*7*tsinday)] = rep(week,times=weeksinalltime)
      if(remaining_tsteps != 0){ # fill remainder of days with zero rain
        Precip[p,((weeksinalltime*7*tsinday)+1):tsteps] = 0
      }
      att_text[p] = paste('Regular precip regime: 11mm over 6 hours twice a week, 1144mm annual total')
    }else if(Precip_switch[p]=='drizzle'){ # ~3mm daily spread every timestep, 1144mm annual total
      tstep_amt = 3.134 / tsinday / tstepsize # in mm/s
      Precip[p,] = tstep_amt
      att_text[p] = paste('Drizzle precip regime: ~3mm daily spread over every time step, 1144mm annual total')
    }else if(Precip_switch[p]=='heavy'){ # ~95mm in 3 hours, once every 30d, ~1144mm annual total
      raining_tsteps_on_rain_day = ceiling(1/8 * tsinday)
      dry_steps_till_evening = ceiling(tsinday *3/4)
      dry_steps_night = tsinday - dry_steps_till_evening - raining_tsteps_on_rain_day
      rain_tstep_amt = 95.3 / raining_tsteps_on_rain_day / tstepsize # in mm/s
      rainday = c(rep(0,times=dry_steps_till_evening),
        rep(rain_tstep_amt,times=raining_tsteps_on_rain_day), rep(0,times=dry_steps_night))
      dryday = rep(0,times=tsinday)
      month = c(rep(dryday, times=29),rainday)
      monthsinalltime = floor(tsteps / length(month))
      remaining_tsteps = tsteps - (monthsinalltime*30*tsinday)
      Precip[p,1:(monthsinalltime*30*tsinday)] = rep(month,times=monthsinalltime)
      if(remaining_tsteps != 0){ # fill remainder of days with zero rain
        Precip[p,((monthsinalltime*30*tsinday)+1):tsteps] = 0
      }
      att_text[p] = paste('Heavy precip regime: ~95mm in 3 hours, once every 30d, 1144mm annual total')
    }else if(Precip_switch[p]=='drydown'){ # 20mm/hr for 2 weeks, nothing for 3 years, 200mm in 3 hours ~ 1144mm in total
      startrainrate = 312.57/tsinday / tstepsize # in mm/s
      laterainrate = 200/(ceiling(tsinday/8)) / tstepsize # in mm/s
      start = rep(startrainrate,times=(tsinday*14))
      midearly = rep(0,times=(tsinday*365*3))
      midlate = c(rep(0,times=ceiling(tsinday/2)),rep(laterainrate,times=ceiling(tsinday/8)))
      end = rep(0,times=(tsteps-length(start)-length(midearly)-length(midlate)))
      Precip[p,] = c(start,midearly,midlate,end)
      att_text[p] = paste('Drydown precip regime: ~13mm/hr for 2 weeks, nothing for 3 years, 200mm in 3 hours. 1144mm average annual total.' )
    }
    label[p] = paste0('P',Precip_switch[p])
    units[p] = 'mm/s'
  }
  returnvals = list(dat=Precip,lab=label,att=att_text,units=units)
  return(returnvals)
}

create_CO2air = function(CO2air_switch,tsteps){
  # Initialise CO2air array:
  CO2air = array(NA,dim=c(length(CO2air_switch),tsteps))
  att_text = c() # attribute text for Tair
  label = c() # Tair section of file label
  units = c()
  for(c in 1:(length(CO2air_switch))){
    CO2air[c,] = CO2air_switch[c]
    att_text[c] = paste('Fixed CO2 at',CO2air_switch[c],sep=' ')
    label[c] = paste0('C',CO2air_switch[c])
    units[c] = 'ppmv'
  }
  returnvals = list(dat=CO2air,lab=label,att=att_text,units=units)
  return(returnvals)
}

create_PSurf = function(tsteps){

  PSurf = rep(100000,times=tsteps)
  att_text = 'Fixed PSurf at 100kPa'
  label = ''
  units = 'Pa'
  returnvals = list(dat=PSurf,lab=label,att=att_text,units=units)

  return(returnvals)
}
