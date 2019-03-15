# synthmet_writefile.R
#
# Gab Abramowitz gabsun at gmail dot com 2019
#

write_synthmet = function(index,tstepsize,tsteps,LWdown_switch,Qair_switch,
  VegType_switch,SWdown,Tair,CO2air,Wind,Precip,Psurf){
    source('synthmet_createvars.R') # contains variable creation functions
    source('synthmet_writefile.R') # contains variable creation functions
    err=FALSE
    # Synthesize LWdown and Qair based on Tair and relative humidity:
    LWdown = create_LWdown(LWdown_switch[index[3]],Qair_switch[index[5]],
        Tair$dat[index[4],])
    Qair = create_Qair(Qair_switch[index[5]],Tair$dat[index[4],],Psurf$dat)
    # Divide precip between rainfall and snowfall:
    Rainf = list(dat = ifelse(Tair$dat[index[4],]>273.15,Precip$dat[index[2],],0),
      att=Precip$att[index[2]])
    Snowf = list(dat = ifelse(Tair$dat[index[4],]<=273.15,Precip$dat[index[2],],0),
      att = Precip$att[index[2]])


    # Determine properties dependent on vegetation type:
    if(VegType_switch$vtype[index[8]]=='grass'){
      canopyheight = 0.5
      referenceheight = 2
      vegtype='Grassland'
      LAI = create_LAI(VegType_switch$type[index[8]],
        VegType_switch$values[[index[8]]],tstepsize,tsteps)
      veglabel=paste0('Vg',LAI$lab)
    }else if(VegType_switch$vtype[index[8]]=='tree'){
      canopyheight = 20
      referenceheight = 25
      vegtype = 'Evergreen_broadleaf'
      LAI = create_LAI(VegType_switch$type[index[8]],
        VegType_switch$values[[index[8]]],tstepsize,tsteps)
      veglabel=paste0('Vt',LAI$lab)
    }

    filename = paste0('../synth_data/SynthMet',SWdown$lab[index[1]],Precip$lab[index[2]],
      LWdown$lab,Tair$lab[index[4]],Qair$lab,CO2air$lab[index[6]],Wind$lab[index[7]],
      veglabel,'.nc')

    # Define variables:
    var_defs = synthmet_definevars(tsteps,tstepsize)
    # Create file and some global attributes:
    ncid = synthmet_createfile(filename,var_defs,vegtype)
    # Write time independent variables:
    ncvar_put(ncid,'latitude',vals=-31.4099)
  	ncvar_put(ncid,'longitude',vals=152.0357)
    ncvar_put(ncid,'elevation',vals=675)
    ncvar_put(ncid,'reference_height',vals=referenceheight)
    ncvar_put(ncid,'canopy_height',vals=canopyheight)
    ncvar_put(ncid,'utc_offset',vals=10)
    # Write time dependent variables:
    write_synthmet_var(ncid,'SWdown',SWdown$att[index[1]],SWdown$dat[index[1],])
    write_synthmet_var(ncid,'Rainf',Rainf$att,Rainf$dat)
    write_synthmet_var(ncid,'Snowf',Snowf$att,Snowf$dat)
    write_synthmet_var(ncid,'Precip',Precip$att[index[2]],Precip$dat[index[2],])
    write_synthmet_var(ncid,'LWdown',LWdown$att,LWdown$dat)
    write_synthmet_var(ncid,'Tair',Tair$att[index[4]],Tair$dat[index[4],])
    write_synthmet_var(ncid,'Qair',Qair$att,Qair$dat)
    write_synthmet_var(ncid,'Psurf',Psurf$att,Psurf$dat)
    write_synthmet_var(ncid,'CO2air',CO2air$att[index[6]],CO2air$dat[index[6],])
    write_synthmet_var(ncid,'Wind',Wind$att[index[7]],Wind$dat[index[7],])
    write_synthmet_var(ncid,'LAI',LAI$att,LAI$dat)
    # Close netcdf file
    nc_close(ncid)

    return(err)
  }

write_synthmet_var = function(ncid,varname,attr,data){
  ncvar_put(ncid,varname,vals=data)
  ncatt_put(ncid,varname,attname='Synthesis_info',attval=attr)
}

synthmet_createfile = function(filename,var_defs,vegtype){
  # Create netcdf file:
  ncid = nc_create(filename,vars=var_defs)
  # Write global attributes:
  ncatt_put(ncid,varid=0,attname='IGBP_vegetation_type',attval=vegtype)
  ncatt_put(ncid,varid=0,attname='Production_source',
    attval='PLUMBER2_synthetic_forcing_data')
  ncatt_put(ncid,varid=0,attname='Source_code_repository',
    attval='github.com/gabsun/synthmet')
  ncatt_put(ncid,varid=0,attname='Git_revision',
    attval=system('git rev-parse HEAD',intern=TRUE))
  ncatt_put(ncid,varid=0,attname='Contact',
    attval='Gab_Abramwoitz_gabriel@unsw.edu.au')
  ncatt_put(ncid,varid=0,attname='Production_time',
    attval=as.character(Sys.time()))
  ncatt_put(ncid,varid=0,attname='Machine',
    attval=system('hostname',intern=TRUE))
  ncatt_put(ncid,varid=0,attname='OS',
    attval=system('uname',intern=TRUE))
  # Write variable attributes for CF and CMIP compliance:
  ncatt_put(ncid,'SWdown',attname='Standard_name',
    attval='surface_downwelling_shortwave_flux_in_air')
  ncatt_put(ncid,'SWdown',attname='CMIP_short_name',attval='rsds')
  ncatt_put(ncid,'Tair',attname='Standard_name',attval='air_temperature')
  ncatt_put(ncid,'Tair',attname='CMIP_short_name',attval='tas')
  ncatt_put(ncid,'LWdown',attname='Standard_name',
    attval='surface_downwelling_longwave_flux_in_air')
  ncatt_put(ncid,'LWdown',attname='CMIP_short_name',attval='rlds')
  ncatt_put(ncid,'Qair',attname='Standard_name',attval='specific_humidity')
  ncatt_put(ncid,'Qair',attname='CMIP_short_name',attval='huss')
  ncatt_put(ncid,'Psurf',attname='Standard_name',
    attval='surface_air_pressure')
  ncatt_put(ncid,'Psurf',attname='CMIP_short_name',attval='ps')
  ncatt_put(ncid,'Precip',attname='Standard_name',attval='precipitation_flux')
  ncatt_put(ncid,'Precip',attname='CMIP_short_name',attval='pr')
  ncatt_put(ncid,'Rainf',attname='Standard_name',attval='rainfall_flux')
  ncatt_put(ncid,'Rainf',attname='CMIP_short_name',attval='prr')
  ncatt_put(ncid,'Snowf',attname='Standard_name',attval='snowfall_flux')
  ncatt_put(ncid,'Snowf',attname='CMIP_short_name',attval='prsn')
  ncatt_put(ncid,'Wind',attname='Standard_name',attval='wind_speed')
  ncatt_put(ncid,'Wind',attname='CMIP_short_name',attval='ws')
  ncatt_put(ncid,'CO2air',attname='Standard_name',attval='mole_fraction_of_carbon_dioxide_in_air')
  ncatt_put(ncid,'CO2air',attname='CMIP_short_name',attval='co2c')
  ncatt_put(ncid,'LAI',attname='Standard_name',attval='leaf_area_index')
  ncatt_put(ncid,'LAI',attname='CMIP_short_name',attval='lai')
  return(ncid)
}

synthmet_definevars = function(tsteps,tstepsize){
  library(ncdf4) # load netcdf library
  missing_value=-9999 # default missing value for all variables
  # Define x, y and z dimensions
  xd = ncdim_def('x',vals=c(1),units='')
  yd = ncdim_def('y',vals=c(1),units='')
  zd = ncdim_def('z',vals=c(1),units='')
  # Determine data start date and time:
  timeunits = 'seconds since 2000-01-01 00:00:00'
  # Create time dimension variable:
  tt=c(0:(tsteps-1))
  timedata = as.double(tt*tstepsize)
  # Define time dimension:
  td = ncdim_def('time',unlim=TRUE,units=timeunits,vals=timedata)
  # VARIABLE DEFINITIONS ##############################################
  # First, non-time variables:
  # Define latitude:
  lat=ncvar_def('latitude','degrees_north',dim=list(xd,yd),
    missval=missing_value,longname='Latitude')
  # Define longitude:
  lon=ncvar_def('longitude','degrees_east',dim=list(xd,yd),
    missval=missing_value,longname='Longitude')
  # Define elevation:
  elev=ncvar_def('elevation','m',dim=list(xd,yd),
    missval=missing_value,longname='Site elevation above sea level')
  # Define measurement height on tower:
  refheight=ncvar_def('reference_height','m',dim=list(xd,yd),
    missval=missing_value,longname='Measurement height on flux tower')
  # Define canopy height:
  canheight=ncvar_def('canopy_height','m',dim=list(xd,yd),
    missval=missing_value,longname='Maximum height of vegetation')
  # Define site time offset:
  timeoffset=ncvar_def('utc_offset','hours',dim=list(xd,yd),
    missval=missing_value,longname='Local time difference from UTC')
  # Define SWdown variable:
  SWdown=ncvar_def('SWdown','W/m^2', dim=list(xd,yd,td),
    missval=missing_value,longname='Surface incident shortwave radiation')
  # Define Tair variable:
  Tair=ncvar_def('Tair','K', dim=list(xd,yd,td),
    missval=missing_value,longname='Near surface air temperature')
  # Define Precip variable:
  Precip=ncvar_def('Precip','kg/m^2/s', dim=list(xd,yd,td),
    missval=missing_value,longname='Precipitation rate')
  # Define Rainf variable:
  Rainf=ncvar_def('Rainf','kg/m^2/s', dim=list(xd,yd,td),
    missval=missing_value,longname='Rainfall rate')
  # Define Snowf variable:
  Snowf=ncvar_def('Snowf','kg/m^2/s', dim=list(xd,yd,td),
    missval=missing_value,longname='Snowfall rate')
  # Define Qair variable:
  Qair=ncvar_def('Qair','kg/kg', dim=list(xd,yd,td),
    missval=missing_value,longname='Near surface specific humidity')
  # Define Wind variable:
  Wind=ncvar_def('Wind','m/s', dim=list(xd,yd,td),
    missval=missing_value,longname='Scalar windspeed')
  # Define Psurf variable:
  Psurf=ncvar_def('Psurf','Pa', dim=list(xd,yd,td),
    missval=missing_value,longname='Surface air pressure')
  # Define LWdown variable:
  LWdown=ncvar_def('LWdown','W/m^2', dim=list(xd,yd,td),
    missval=missing_value,longname='Surface incident longwave radiation')
  # Define CO2air variable:
  CO2air=ncvar_def('CO2air','ppmv', dim=list(xd,yd,td),
    missval=missing_value,longname='Near surface CO2 concentration')
  # Define CO2air variable:
  LAI=ncvar_def('LAI','-', dim=list(xd,yd,td),
    missval=missing_value,longname='Leaf area index')

  metncvars = list(lat=lat,lon=lon,
    SWdown=SWdown,LWdown=LWdown,Tair=Tair,Rainf=Rainf,Snowf=Snowf,Qair=Qair,
    Wind=Wind,Psurf=Psurf,CO2air=CO2air,Precip=Precip,LAI=LAI,elev=elev,
    refheight=refheight,canheight=canheight,timeoffset=timeoffset)
  return(metncvars)
}
