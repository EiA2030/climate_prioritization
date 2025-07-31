# ------------------------------------------------------------------------------
# Environmental Hazard Analysis Workflow
# 
# Author: p.steward@cgiar.org
# Date: 2024-12-13
# Acknowledgements: Funded under the CGIAR Excellence in Agronomy Initiative (Climate Add-on)
# 
# Description:
# This script performs a comprehensive analysis and visualization of environmental 
# hazards affecting agricultural productivity, focusing on rainfed and irrigated 
# systems. The workflow includes:
# 
# 1. Data Acquisition:
#    - Automatically downloads spatial datasets (crop calendars, rainfall variability, 
#      flood hazards, drought risks, heat stress, water stress, and growing season reduction) 
#      from public repositories and S3 buckets.
# 
# 2. Data Preprocessing:
#    - Aligns and processes datasets for compatibility using resampling and classification.
#    - Converts raw hazard indicators into meaningful categories for analysis.
# 
# 3. Hazard Combination:
#    - Integrates multiple hazards into composite hazard maps for rainfed and irrigated systems.
#    - Calculates frequency and intensity of hazards for each scenario.
# 
# 4. Visualization and Output:
#    - Generates classified raster maps with color-coded hazard scores.
#    - Saves hazard maps and summary tables for further analysis and reporting.
# 
# Outputs:
#    - Composite hazard maps for rainfed and irrigated systems.
#    - Frequency and severity tables summarizing hazards.
#    - Enhanced visualizations for interpretability.
# 
# Dependencies:
#    - R packages: data.table, terra, sf, ecmwfr, pbapply, viridis, MetBrewer, wesanderson
# 
# Usage:
#    - Ensure all dependencies are installed, and set up access to the required S3 bucket.
#    - Run the script sequentially; outputs are saved in specified directories.
# ------------------------------------------------------------------------------

# 0) Install and load packages ####
if(!require("pacman", character.only = TRUE)){install.packages("pacman",dependencies = T)}

pacman::p_load(data.table,terra,sf,ecmwfr,pbapply,viridis,MetBrewer,wesanderson,install = T)

Viridis<-data.table(Source="viridis",Palette=c("magma","inferno","plasma","viridis","cividis","rocket","mako","turbo"))
Met<-data.table(Source="MetBrewer",Palette=names(MetBrewer::MetPalettes))
Wes<-data.table(Source="wesanderson",Palette=names(wesanderson::wes_palettes))

Palettes<-rbind(Viridis,Met,Wes)

## 0.1) Create helper functions #####
# Helper function to generate palettes
PalFun<-function(PalName,N,Names) {
  Viridis<-data.table(Source="viridis",Palette=c("magma","inferno","plasma","viridis","cividis","rocket","mako","turbo"))
  Met<-data.table(Source="MetBrewer",Palette=names(MetBrewer::MetPalettes))
  Palettes<-rbind(Viridis,Met)
  
  if(Palettes[Palette==PalName,Source]=="viridis"){
    PAL<-viridis::viridis(N,option=PalName)
  }
  
  if(Palettes[Palette==PalName,Source]=="MetBrewer"){
    PAL<-MetBrewer::met.brewer(name=PalName, n=N, type="continuous")
  }
  
  if(Palettes[Palette==PalName,Source]=="Wes"){
    PAL<-wesanderson::wes_palette(name=PalName, n=N, type="continuous")
  }
  names(PAL)<-Names
  
  return(PAL)
}

# Helper function to download files if they don't exist
download_if_missing <- function(file_path, url) {
  if (!file.exists(file_path)) {
    dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)
    download.file(url, file_path, mode = "wb")
  }
}

# Helper function to convert Julian day to month
convert_to_month <- function(julian_day,year) {
  julian_day<-as.numeric(julian_day)
  date <- as.Date(julian_day - 1, origin = paste0(year, "-01-01"))
  as.numeric(format(date, "%m"))  # Return numeric month
}

## 0.2) Set s3 bucket #####
bucket_name_s3<-"s3://digital-atlas"
s3<-s3fs::S3FileSystem$new(anonymous = T)

# 1) Download & process datasets ####
  ## 1.1) GCCMI crop calendars  #####
    ### 1.1.1) Download ####
  # File paths and URLs
  file_maize_rf <- "raw_data/gccmi/mai_rf_ggcmi_crop_calendar_phase3_v1.01.nc4"
  url_maize_rf <- "https://zenodo.org/records/5062513/files/mai_rf_ggcmi_crop_calendar_phase3_v1.01.nc4"
  
  file_maize_ir <- "raw_data/gccmi/mai_ir_ggcmi_crop_calendar_phase3_v1.01.nc4"
  url_maize_ir <- "https://zenodo.org/records/5062513/files/mai_ir_ggcmi_crop_calendar_phase3_v1.01.nc4"
  
  # Download files if missing
  download_if_missing(file_maize_rf, url_maize_rf)
  download_if_missing(file_maize_ir, url_maize_ir)
  
    ### 1.1.2) Rainfed  ######
    cc_maize_rf<-terra::rast(file_maize_rf)
    # Convert days to month
    cc_maize_rf$planting_month<- convert_to_month(as.numeric(cc_maize_rf$planting_day[]),year=1980)
    cc_maize_rf$maturity_month<- convert_to_month(as.numeric(cc_maize_rf$maturity_day[]),year=1980)
    # Optional: Add names for clarity
    names(cc_maize_rf$planting_month) <- "month"
    names(cc_maize_rf$maturity_month) <- "month"
    
    ### 1.1.2) Irrigated ######
    
    cc_maize_ir<-terra::rast(file_maize_ir)
    # Convert days to month
    cc_maize_ir$planting_month<-convert_to_month(as.numeric(cc_maize_ir$planting_day[]),year=1980)
    cc_maize_ir$maturity_month<-convert_to_month(as.numeric(cc_maize_ir$maturity_day[]),year=1980)
    # Optional: Add names for clarity
    names(cc_maize_ir$planting_month) <- "month"
    names(cc_maize_ir$maturity_month) <- "month"

  ## 1.2) CHIRPS rainfall CV ####
    ### 1.2.1) Download & load data ######
    local_dir<-"raw_data/chirps_cv"
    s3_bucket <-file.path("s3://digital-atlas/hazards/chirps_cv_global")
    
    # List files in the specified S3 bucket and prefix
    files_s3<-s3$dir_ls(s3_bucket)
    files_local<-file.path(local_dir,basename(files_s3))
    
    # If data does not exist locally download from S3 bucket
    for(i in 1:length(files_local)){
      file<-files_local[i]
      if(!file.exists(file)){
        s3$file_download(files_s3[i],file)
      }
    }
    
  # 2010 - 2023
  rain_cv<-terra::rast(file.path(local_dir,"cv_2010-2023_annual.tif"))
  
    ### 1.2.2) Classify #####
  # Create a classification matrix for terra::classify
  # Columns: from, to, becomes
  classification_matrix <- matrix(c(
    0, 20, 1,   # Low Variability
    20, 50, 2,  # Moderate Variability
    50, 80, 3,  # High Variability
    80, 200, 4  # Extreme Variability
  ), ncol = 3, byrow = TRUE)
  
  # Use classify with the classification matrix
  rain_cv_class <- classify(rain_cv, classification_matrix)
  
  # Assign levels (optional, to add labels)
  cv_levels <-data.frame(value =1:4, label =  c("Low Variability", "Moderate Variability", "High Variability", "Extreme Variability"))
  levels(rain_cv_class)<-cv_levels
  
  ## 1.3) Aqueduct water stress #####
    ### 1.3.1) Download #####
  # File paths and URLs
  file_aqueduct <- "raw_data/aqueduct/aqueduct-4-0-water-risk-data.zip"
  url_aqueduct <- "https://files.wri.org/aqueduct/aqueduct-4-0-water-risk-data.zip"
  
  # Download and unzip dataset
  download_if_missing(file_aqueduct, url_aqueduct)
  
  aqueduct_dir<-"raw_data/aqueduct/data"
  if(!dir.exists(aqueduct_dir)){
    dir.create(aqueduct_dir,recursive=T)
    unzip(file_aqueduct,"raw_data/aqueduct/data",junkpaths=T)
  }
  
    ### 1.3.2) Process data ######
  bws_file<-"raw_data/aqueduct/bws_baseline.tif"
  rfr_file<-"raw_data/aqueduct/rfr_baseline.tif"
  bwd_file<-"raw_data/aqueduct/bwd_baseline.tif"
  cfr_file<-"raw_data/aqueduct/cfr_baseline.tif"
  sev_file<-"raw_data/aqueduct/sev_baseline.tif"
  
  if(!file.exists(bws_file)){
  aqueduct_gdb<-"raw_data/aqueduct/data/GDB/Aq40_Y2023D07M05.gdb"
    
  sf::st_layers(aqueduct_gdb)
  aq_base_annual<-sf::st_read(aqueduct_gdb,layer="baseline_annual")
  aq_base_annual<-terra::vect(aq_base_annual)
  # https://github.com/wri/Aqueduct40/blob/master/data_dictionary_country-rankings.md#country-baseline
  
  bws<-terra::rasterize(aq_base_annual,rain_cv,field="bws_cat")
  rast_levels<-unique(data.table(data.frame(aq_base_annual))[,.(bws_cat,bws_label)])
  levels(bws) <- data.frame(value = rast_levels$bws_cat, label = rast_levels$bws_label)
  terra::writeRaster(bws,filename=bws_file,overwrite=T)
  
  bwd<-terra::rasterize(aq_base_annual,rain_cv,field="bwd_cat")
  rast_levels<-unique(data.table(data.frame(aq_base_annual))[,.(bwd_cat,bwd_label)])
  levels(bwd) <- data.frame(value = rast_levels$bwd_cat, label = rast_levels$bwd_label)
  terra::writeRaster(bwd,filename=bwd_file,overwrite=T)
  
  rfr<-terra::rasterize(aq_base_annual,rain_cv,field="rfr_cat")
  rast_levels<-unique(data.table(data.frame(aq_base_annual))[,.(rfr_cat,rfr_label)])[order(rfr_cat)]
  levels(rfr) <- data.frame(value = rast_levels$rfr_cat, label = rast_levels$rfr_label)
  terra::writeRaster(rfr,filename=rfr_file,overwrite=T)
  
  cfr<-terra::rasterize(aq_base_annual,rain_cv,field="cfr_cat")
  rast_levels<-unique(data.table(data.frame(aq_base_annual))[,.(cfr_cat,cfr_label)])
  levels(cfr) <- data.frame(value = rast_levels$cfr_cat, label = rast_levels$cfr_label)
  terra::writeRaster(cfr,filename=cfr_file,overwrite=T)
  
  sev<-terra::rasterize(aq_base_annual,rain_cv,field="sev_cat")
  rast_levels<-unique(data.table(data.frame(aq_base_annual))[,.(sev_cat,sev_label)])
  levels(sev) <- data.frame(value = rast_levels$sev_cat, label = rast_levels$sev_label)
  terra::writeRaster(sev,filename=sev_file,overwrite=T)
  
  
  }else{
    bws<-rast(bws_file)
    bwd<-rast(bwd_file)
    rfr<-rast(rfr_file)
    cfr<-rast(cfr_file)
    sev<-rast(sev_file)
  }
  
  ## 1.4) JRC flood hazard map #####
    ### 1.4.1) Download & process #####
  # https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/CEMS-GLOFAS/flood_hazard/

  # File paths and URLs
  # 100 yr return period
  file_100y <- "raw_data/jrc_floods/floodMapGL_rp100y.tif"
  url_100y <- "https://data.jrc.ec.europa.eu/dataset/jrc-floods-floodmapgl_rp100y-tif"
  
  # 20 yr return period
  file_20y <- "raw_data/jrc_floods/floodMapGL_rp20y.tif"
  url_20y <- "https://data.jrc.ec.europa.eu/dataset/jrc-floods-floodmapgl_rp20y-tif"
  
  # Download files if missing
  download_if_missing(file_100y, url_100y)
  download_if_missing(file_20y, url_20y)
  
  # Load and process rasters
  jrc_floodrisk_100y <- terra::rast(file_100y)
  jrc_floodrisk_100y <- terra::resample(jrc_floodrisk_100y, rain_cv)
  
  jrc_floodrisk_20y <- terra::rast(file_20y)
  jrc_floodrisk_20y <- terra::resample(jrc_floodrisk_20y, rain_cv)
  
  ## 1.5) Global drought observatory ####
    ### 1.5.1) Download ######
  
    # Data were downloaded manually from the GDO website and then uploaded to the digital atlas s3
    # https://drought.emergency.copernicus.eu/tumbo/gdo/download/ 
    # Years 2010-2024
    # It may be possible to update to automatically download from http://data.europa.eu/89h/b4727c91-955f-4e2f-b0cf-dd64145b57e7
    local_dir<-"raw_data/drought_observatory"
      if(!dir.exists(local_dir)){
        dir.create(local_dir)
      }
    
    s3_bucket <-file.path("s3://digital-atlas/hazards/global_drought_observatory")
    
    # List files in the specified S3 bucket and prefix
    files_s3<-s3$dir_ls(s3_bucket)
    files_local<-file.path(local_dir,basename(files_s3))
    
    # If data does not exist locally download from S3 bucket
    for(i in 1:length(files_local)){
      file<-files_local[i]
      if(!file.exists(file)){
        s3$file_download(files_s3[i],file)
      }
    }
      
    ### 1.5.2) Load & process #####
  files<-list.files(local_dir,"tif$",full.names = T,recursive = T)
  layer_names<-gsub("_t.tif","",gsub("rdria_m_wld_","",basename(files)))
  layer_names<-data.table(year=substr(layer_names,1,4),
                          month=substr(layer_names,5,6),
                          day=substr(layer_names,7,8),
                          file=files)
  years<-layer_names[,unique(year)]
  
  layer_names<-layer_names[order(year,month,day)]
  drought<-terra::rast(layer_names$file)
  drought<-drought+0
  
    ### 1.5.3) Rainfed ######
  plant<-terra::resample(cc_maize_rf$planting_month,drought,method="near")
  harvest<-terra::resample(cc_maize_rf$maturity_month,drought,method="near")
  
  plant<-(plant$planting_month*3)-2
  harvest<-harvest$maturity_month*3
  # Where plant>harvest (e.g. plant = 11 harvest = 3) add 12 to harvest (e.g. plant = 11 harvest = 15)
  harvest[plant[]>harvest[]]<-harvest[plant[]>harvest[]]+36
  
  plant_min<-min(plant[],na.rm=T)
  harvest_max<-max(harvest[],na.rm=T)
  
  years<-layer_names[,unique(year)]
  
  drought_yrs_rf<-terra::rast(pblapply(1:(length(years)-1),FUN=function(m){
  
    # Subset drought to increase efficiency
    x = drought[[(plant_min+36*(m-1)):(harvest_max+36*(m-1))]]
    
    drought1<-terra::rapp(x,
                           first=plant,
                           last=harvest,
                           fun="mean",
                           na.rm=T)
    
    names(drought1)<-years[m]
    drought1
  }))
  
  # Calculate proportion of seasons where mean drought risk is >1 (1 = low)
  d_fun<-function(x,threshold){
    x<-x[!is.na(x)]
    x<-sum(x>threshold)/length(x)
    return(x)
  }
  
  drought_rf<-app(drought_yrs_rf,d_fun,threshold=1)
  
    ### 1.5.4) Irrigated ######
  plant<-terra::resample(cc_maize_ir$planting_month,drought,method="near")
  harvest<-terra::resample(cc_maize_ir$maturity_month,drought,method="near")
  
  plant<-(plant$planting_month*3)-2
  harvest<-harvest$maturity_month*3
  # Where plant>harvest (e.g. plant = 11 harvest = 3) add 12 to harvest (e.g. plant = 11 harvest = 15)
  harvest[plant[]>harvest[]]<-harvest[plant[]>harvest[]]+36
  
  plant_min<-min(plant[],na.rm=T)
  harvest_max<-max(harvest[],na.rm=T)
  
  years<-layer_names[,unique(year)]
  
  drought_yrs_ir<-terra::rast(pblapply(1:(length(years)-1),FUN=function(m){
    
    # Subset drought to increase efficiency
    x = drought[[(plant_min+36*(m-1)):(harvest_max+36*(m-1))]]
    
    drought1<-terra::rapp(x,
                          first=plant,
                          last=harvest,
                          fun="mean",
                          na.rm=T)
    
    names(drought1)<-years[m]
    drought1
  }))
  drought_ir<-app(drought_yrs_ir,d_fun,threshold=1)
  
  ## 1.6) AgERA5 - NTx ####
    ### 1.6.1) Download & load data ######
    local_dir<-"raw_data/agera5"
    s3_bucket <-file.path("s3://digital-atlas/hazards/agera5_ntx_global")
    
    # List files in the specified S3 bucket and prefix
    files_s3<-s3$dir_ls(s3_bucket)
    # Subset to ntx35
    files_s3<-grep("x35",files_s3,value=T)
    
    files_local<-file.path(local_dir,basename(files_s3))
    
    # If data does not exist locally download from S3 bucket
    for(i in 1:length(files_local)){
      file<-files_local[i]
      if(!file.exists(file)){
        s3$file_download(files_s3[i],file)
      }
    }
    
    ntx35<-terra::rast(file.path(local_dir,"NTx35.tif"))
    years<-unique(format(as.Date(names(ntx35)),"%Y"))
    ### 1.6.2) Irrigated #####
    plant<- cc_maize_ir$planting_month
    harvest <- cc_maize_ir$maturity_month
    # Resample to wsdi layer
    plant<-terra::resample(plant,ntx35,method="near")
    harvest<-terra::resample(harvest,ntx35,method="near")
    
    # Where plant>harvest (e.g. plant = 11 harvest = 3) add 12 to harvest (e.g. plant = 11 harvest = 15)
    harvest[plant[]>harvest[]]<-harvest[plant[]>harvest[]]+12
      
    plant_min<-min(plant[],na.rm=T)
    harvest_max<-max(harvest[],na.rm=T)
    
    ntx35_yrs_ir<-terra::rast(pblapply(1:(length(years)-1),FUN=function(m){
      
      # Subset drought to increase efficiency
      x = ntx35[[(plant_min+12*(m-1)):(harvest_max+12*(m-1))]]
      
      y<-terra::rapp(x,
                    first=plant,
                    last=harvest,
                    fun="mean",
                    na.rm=T)
      
      names(y)<-years[m]
      y
    }))
  
    # Classify each year
    ntx35_yrs_ir_class<-classify(ntx35_yrs_ir,data.frame(from=c(0,10,20,25),to=c(10,20,25,999),becomes=c(0,1,2,3)))
    ntx35_levels<-data.frame(value=0:3,hazard=c("No Stress","Moderate Stress","Severe Stress","Extreme Stress"))
    
    for(i in 1:nlyr(ntx35_yrs_ir_class)){
    levels(ntx35_yrs_ir_class[[i]])<-ntx35_levels
    }
    names(ntx35_yrs_ir_class)<-names(ntx35_yrs_ir)
    
    # Get frequency of moderate hazard or higher
    ntx35_ir_freq<-ntx35_yrs_ir_class
    ntx35_ir_freq<-classify(ntx35_ir_freq,data.frame(from=c(0:3),to=c(0,1,1,1)))
    ntx35_ir_freq<-sum(ntx35_ir_freq,na.rm=T)/nlyr(ntx35_ir_freq)
    names(ntx35_ir_freq)<-"ntx35_mod-haz-freq_ir-cc"
    plot(ntx35_ir_freq)
    
    # Resample
    ntx35_ir_freq<-terra::resample(ntx35_ir_freq,rain_cv)
    
    ### 1.6.3) Rainfed #####
    plant<- cc_maize_rf$planting_month
    harvest <- cc_maize_rf$maturity_month
    # Resample to wsdi layer
    plant<-terra::resample(plant,ntx35,method="near")
    harvest<-terra::resample(harvest,ntx35,method="near")
    
    # Where plant>harvest (e.g. plant = 11 harvest = 3) add 12 to harvest (e.g. plant = 11 harvest = 15)
    harvest[plant[]>harvest[]]<-harvest[plant[]>harvest[]]+12
    
    plant_min<-min(plant[],na.rm=T)
    harvest_max<-max(harvest[],na.rm=T)
    
    ntx35_yrs_rf<-terra::rast(pblapply(1:(length(years)-1),FUN=function(m){
      
      # Subset drought to increase efficiency
      x = ntx35[[(plant_min+12*(m-1)):(harvest_max+12*(m-1))]]
      
      y<-terra::rapp(x,
                     first=plant,
                     last=harvest,
                     fun="mean",
                     na.rm=T)
      
      names(y)<-years[m]
      y
    }))
    
    # Classify each year
    ntx35_yrs_rf_class<-classify(ntx35_yrs_rf,data.frame(from=c(0,10,20,25),to=c(10,20,25,999),becomes=c(0,1,2,3)))
    ntx35_levels<-data.frame(value=0:3,hazard=c("No Stress","Moderate Stress","Severe Stress","Extreme Stress"))
    
    for(i in 1:nlyr(ntx35_yrs_rf_class)){
      levels(ntx35_yrs_rf_class[[i]])<-ntx35_levels
    }
    names(ntx35_yrs_rf_class)<-names(ntx35_yrs_rf)
    
    # Get frequency of moderate hazard or higher
    ntx35_rf_freq<-ntx35_yrs_rf_class
    ntx35_rf_freq<-classify(ntx35_rf_freq,data.frame(from=c(0:3),to=c(0,1,1,1)))
    ntx35_rf_freq<-sum(ntx35_rf_freq,na.rm=T)/nlyr(ntx35_rf_freq)
    names(ntx35_rf_freq)<-"ntx35_mod-haz-freq_rf-cc"
    plot(ntx35_rf_freq)
    
  ## 1.7) GAEZ - LGP ####
    ### 1.7.1) Download ######
    local_dir<-"raw_data/gaez"
    if(!dir.exists(local_dir)){
      dir.create(local_dir)
    }
    
    s3_bucket <-file.path("s3://digital-atlas/hazards/gaez_lgp")
    
    # List files in the specified S3 bucket and prefix
    files_s3<-s3$dir_ls(s3_bucket)
    files_local<-file.path(local_dir,basename(files_s3))
    
    # If data does not exist locally download from S3 bucket
    for(i in 1:length(files_local)){
      file<-files_local[i]
      if(!file.exists(file)){
        s3$file_download(files_s3[i],file)
      }
    }
    
    ### 1.7.2) Load & process #####
    file<-list.files("raw_data/gaez/len-longest-component-LGP","rcp2p6_2050s",full.names = T)
    lgp_fut<-terra::rast(file)
    file<-list.files("raw_data/gaez/len-longest-component-LGP","Hist",full.names = T)
    lgp_base<-terra::rast(file)
    lgp_change<-(lgp_fut-lgp_base)
    lgp_change[lgp_change>=0]<-NA  
    lgp_change<-100*(-lgp_change/lgp_base)
# 2) Prepare (classify) Hazards ####
  ## 2.1) Rain cv #####
  var<-rain_cv_class
  var[var[]==1]<-0
  var[var[]>1]<-1
  levels(var)<-data.frame(value=1,label="v")
  names(var)<-"v = rainfall_cv>20"
  
  ## 2.2) Flood #####
  flood<-jrc_floodrisk_20y
  flood[flood[]>0]<-10
  levels(flood)<-data.frame(value=10,label="f")
  names(flood)<-"f = flood_risk_20y"
  
  ## 2.3) Drought #####
  cutoff<-0.2
  dry<-drought_rf
  dry<-resample(dry,rain_cv)
  dry[dry[]<cutoff]<-0
  dry[dry[]>=cutoff]<-100
  levels(dry)<-data.frame(value=100,label="d")
  names(dry)<-paste0("d = drought risk>",cutoff)
  plot(dry)
  
  ## 2.4) Heat #####
  cutoff<-0.2
  heat_rf<-ntx35_rf_freq
  heat_rf<-resample(heat_rf,rain_cv)
  heat_rf[heat_rf[]<cutoff]<-0
  heat_rf[heat_rf[]>=cutoff]<-1000
  levels(heat_rf)<-data.frame(value=1000,label="h")
  names(heat_rf)<-paste0("d = heat risk>",cutoff)
  plot(heat_rf)
  
  cutoff<-0.2
  heat_ir<-ntx35_ir_freq
  heat_ir<-resample(heat_ir,rain_cv)
  heat_ir[heat_ir[]<cutoff]<-0
  heat_ir[heat_ir[]>=cutoff]<-1000
  levels(heat_ir)<-data.frame(value=1000,label="h")
  names(heat_ir)<-paste0("d = heat risk>",cutoff)
  plot(heat_ir)
  
  ## 2.5) Water Stress #####
  water_stress<-bwd
  water_stress<-resample(water_stress,rain_cv,method="near")
  levels(water_stress)
  water_stress<-terra::classify(water_stress,data.table(from=c(-9999,-1,0,1,2,3,4),to=c(0,0,0,0,100,100,100)))
  levels(water_stress)<-data.frame(value=100,label="ws")
  plot(water_stress)
  ## 2.6) Growing Season Reduction ####
  gsr<-lgp_change
  gsr<-resample(gsr,rain_cv)
  gsr[gsr<10]<-0
  gsr[gsr>=10]<-10000
  levels(gsr)<-data.frame(value=10000,label="r")
  plot(gsr)
  
# 3) Combine hazards #####
  ## 3.1) Rainfed ######
   haz_comb<-sum(c(var,flood,dry,heat_rf,gsr),na.rm=T)
   freq(haz_comb)
  # Add labels
  haz_levels<-data.frame(
    data.table(expand.grid(var=c(0,1),flood=c(0,10),dry=c(0,100),heat=c(0,1000),gsr=c(0,10000)))[,.(value=var+dry+flood+heat+gsr)],
    data.table(expand.grid(var=c(NA,"v"),
                           flood=c(NA,"f"),
                           dry=c(NA,"d"),
                           heat=c(NA,"h"),
                           gsr=c(NA,"r")))[,.(label=paste(na.omit(c(var,flood,dry,heat,gsr)),collapse = "+")),by=.(var,flood,dry,heat,gsr)][,"label"]
  )
  
  haz_levels$new_val<-0:(nrow(haz_levels)-1)
  haz_levels$label[haz_levels$value==0]<-"none"
  
  haz_comb<-classify(haz_comb,data.frame(from=haz_levels$value,to=haz_levels$new_val))
  haz_levels$value<-haz_levels$new_val
  haz_levels$new_val<-NULL
  
  levels(haz_comb)<-haz_levels
  names(haz_comb)<-"combined hazards"
  
  # Improve palette
  color_map<-PalFun(PalName="turbo",N=nrow(haz_levels),Names = haz_levels$value)
  color_map<-data.frame(value=names(color_map),color=color_map)
  color_map$color[1]<-NA
  
  # Assign the colormap to the raster
  coltab(haz_comb) <- as.matrix(color_map)
  
  plot(haz_comb)  
  
  # Calc freq
  haz_comb_f<-data.table(freq(haz_comb))[order(count,decreasing = T)]
  haz_comb_f[,perc:=round(100*(count/sum(count)),1)]
  haz_comb_f
  
  HazTab<-data.table(terra::levels(haz_comb)[[1]])
  colnames(HazTab)<-c("Code","ShortName")
  HazTab<-HazTab[,Hazard:=ShortName
  ][ShortName=="v",Hazard:="Rain variability (v)"
  ][ShortName=="f",Hazard:="Flood (f)"
  ][ShortName=="d",Hazard:="Drought (d)"
  ][ShortName=="h",Hazard:="Heat (h)"
  ][ShortName=="r",Hazard:="Reduced season (r)"]

  
    ### 3.1.1) Save results ####
    terra::writeRaster(haz_comb,filename = "raw_data/haz_comb/haz_full_rf.tif",overwrite=T)
    fwrite(HazTab,"raw_data/haz_comb/haz_full_rf.csv")
    
  ## 3.2) Irrigated ######
  haz_comb<-sum(c(flood,water_stress,heat_ir,gsr),na.rm=T)
  # Add labels
  haz_levels<-data.frame(
    data.table(expand.grid(flood=c(0,10),
                           waterstress=c(0,100),
                           heat=c(0,1000),
                           gsr=c(0,10000)))[,.(value=waterstress+flood+heat+gsr)],
    data.table(expand.grid(flood=c(NA,"f"),
                           waterstress=c(NA,"ws"),
                           heat=c(NA,"h"),
                           gsr=c(NA,"r")))[,.(label=paste(na.omit(c(flood,waterstress,heat,gsr)),collapse = "+")),by=.(flood,waterstress,heat,gsr)][,"label"]
  )
  
  haz_levels$new_val<-0:(nrow(haz_levels)-1)
  haz_levels$label[haz_levels$value==0]<-"none"
  
  haz_comb<-classify(haz_comb,data.frame(from=haz_levels$value,to=haz_levels$new_val))
  haz_levels$value<-haz_levels$new_val
  haz_levels$new_val<-NULL
  levels(haz_comb)<-haz_levels
  names(haz_comb)<-"combined hazards"
  freq(haz_comb)
  
  # Improve palette
  color_map<-PalFun(PalName="turbo",N=nrow(haz_levels),Names = haz_levels$value)
  color_map<-data.frame(value=names(color_map),color=color_map)
  color_map$color[1]<-NA
  
  # Assign the colormap to the raster
  coltab(haz_comb) <- as.matrix(color_map)
  plot(haz_comb)  
  
  # Calc freq
  haz_comb_f<-data.table(freq(haz_comb))[order(count,decreasing = T)]
  haz_comb_f[,perc:=round(100*(count/sum(count)),1)]
  haz_comb_f
  
  # Create table for legend
  HazTab<-data.table(terra::levels(haz_comb)[[1]])     
  colnames(HazTab)<-c("Code","ShortName")
  HazTab<-HazTab[,Hazard:=ShortName
  ][ShortName=="r",Hazard:="Reduced season (r)"
  ][ShortName=="f",Hazard:="Flood (f)"
  ][ShortName=="ws",Hazard:="Water stress (ws)"
  ][ShortName=="h",Hazard:="Heat (h)"]
  
    ### 3.2.1) Save results ####
  save_file<-"raw_data/haz_comb/haz_full_ir.tif"
  terra::writeRaster(haz_comb,filename =save_file,overwrite=T)
  fwrite(HazTab,"raw_data/haz_comb/haz_full_ir.csv")
  check<-rast(save_file)
  