# a) Load R functions & packages ####
pacman::p_load(terra,data.table,httr,countrycode,wbstats,arrow,geoarrow,ggplot2,dplyr,tidyr,dataverse)
Sys.setenv(DATAVERSE_SERVER = "dataverse.harvard.edu")

# Load functions & wrappers
source(url("https://raw.githubusercontent.com/AdaptationAtlas/hazards_prototype/main/R/haz_functions.R"))
options(scipen=999)

# b) Set up workspace ####
spam_dir<-file.path("raw_data/SPAM/spam2020v1r2_ssa")
geo_dir<-"raw_data/boundaries"
fao_dir<-"raw_data/fao"

if(!dir.exists(spam_dir)){
  dir.create(spam_dir)
}

#   Create an S3FileSystem object for anonymous read access ####
s3 <- s3fs::S3FileSystem$new(anonymous = TRUE)

# Mapspam code tables
ms_codes_url <- "https://raw.githubusercontent.com/AdaptationAtlas/hazards_prototype/main/metadata/SpamCodes.csv"
spam2fao_url <- "https://raw.githubusercontent.com/AdaptationAtlas/hazards_prototype/main/metadata/SPAM2010_FAO_crops.csv"

# 1) Load geographies ####


s3_bucket <-"s3://digital-atlas/exposure/mapspam/eia_climate_prioritization"

s3_bucket <- file.path(bucket_name_s3,"boundaries/eia_climate_prioritization")
s3_files<-s3$dir_ls(s3_bucket)
local_files<-file.path(geo_dir,basename(s3_files))

lapply(1:length(local_files),FUN=function(i){
  file<-local_files[i]
  if(!file.exists(file)|update==T){
    s3$file_download(s3_files[i],file)
  }
})

file<-grep("countries",local_files,value=T)

geoboundaries<-terra::vect(file)
names(geoboundaries)[2]<-"iso3"

geoboundaries[geoboundaries$ADMIN=="South Sudan"]$iso3<-"SSD"

# Subset to african countries
af_iso3<-sort(names(african_neighbors))
geoboundaries<-geoboundaries[geoboundaries$iso3 %in% af_iso3,]

# 2) Download and Load mapspam data ####
## 2.1) Download ####

# Construct the S3 folder path
bucket_name_s3<-"s3://digital-atlas"
folder_path <- "domain=exposure/type=crop/source=spam2020v1r2_ssa/region=ssa/processing=atlas-harmonized/"

# List files from the specified S3 bucket location
files_s3 <- s3$dir_ls(file.path(bucket_name_s3, folder_path), recurse = TRUE)
files_s3<-grep("prod_t|harv-area",files_s3,value=T)
files_local <-file.path(spam_dir,basename(files_s3))

# Download files if missing or if update=TRUE
for (i in seq_along(files_local)) {
  cat("3.2.1) Downloading mapspam processed files",i,"/",length(files_local),"     \r")
  file <- files_local[i]
  save_dir<-dirname(file)
  if(!dir.exists(save_dir)){
    dir.create(save_dir,recursive=T)
  }
  if (!file.exists(file) | update == TRUE) {
    s3$file_download(files_s3[i], file, overwrite = TRUE)
  }
}
## 2.2) Prepare and load data #####

# Set base raster
base_rast<-rast(list.files(spam_dir,".tif",full.names = T)[1])
base_rast<-crop(base_rast,geoboundaries)

# Rasterize geoboundaries
admin_rast<-terra::rasterize(geoboundaries, base_rast, field = "iso3")
ms_codes<-data.table::fread(ms_codes_url)[,Code:=toupper(Code)][,Code_ifpri_2020:=toupper(Code_ifpri_2020)]
ms_codes<-ms_codes[compound=="no" & !is.na(Code_ifpri_2020) & !is.na(Code)]

# List spam files
files_raw<-list.files(spam_dir,".tif",full.names = T)
variables<-c("_prod_","_harv-area_")

for(variable in variables){
  cat("Running variable",variable,"\n")
  # Create save paths
  save_file_a<-file.path(spam_dir,paste0("ssa_crop",variable,"a.tif"))
  save_file_i<-file.path(spam_dir,paste0("ssa_crop",variable,"i.tif"))
  save_file_r<-file.path(spam_dir,paste0("ssa_crop",variable,"r.tif"))
  
  if(!file.exists(save_file_a)){
    # Load data
    files<-grep(variable,files_raw,value=T)
    
    files_a<-grep("_all.tif",files,value=T)
    spam_prod_raw<-terra::rast(files_a)
    
    files_r<-grep("_rf-all.tif",files,value=T)
    spam_prod_raw_r<-terra::rast(files_r)
    
    files_i<-grep("_irr.tif",files,value=T)
    spam_prod_raw_i<-terra::rast(files_i)
    
    # Mask to geoboundaries 
    spam_prod_raw<-terra::crop(spam_prod_raw,geoboundaries)
    spam_prod_raw_r<-terra::crop(spam_prod_raw_r,geoboundaries)
    spam_prod_raw_i<-terra::crop(spam_prod_raw_i,geoboundaries)
    
    # update ifpri spam names to match ssa mapspam
    spam_prod<-spam_prod_raw[[names(spam_prod_raw) %in%  ms_codes[!is.na(Fullname),Fullname]]]
    spam_prod_i<-spam_prod_raw_i[[names(spam_prod_raw_i) %in%  ms_codes[!is.na(Fullname),Fullname]]]
    spam_prod_r<-spam_prod_raw_r[[names(spam_prod_raw_r) %in%  ms_codes[!is.na(Fullname),Fullname]]]
    spam_prod_a<-spam_prod
    
    # Check data
    if(F){
      a<-spam_prod$SOYB
      i<-spam_prod_i$SOYB
      r<-spam_prod_r$SOYB
      plot(c(a,sum(c(i,r),na.rm=T)))
    }
    
    # Save data
    writeRaster(spam_prod,save_file_a)
    writeRaster(spam_prod_i,save_file_i)
    writeRaster(spam_prod_r,save_file_r)
  }
}

### 2.2.1) Load production data ######
spam_prod<-rast(save_file_a)
spam_prod_i<-rast(save_file_i)
spam_prod_r<-rast(save_file_r)

# Aggregate crops to match FAOstat
spam_prod$coffee<-spam_prod$`arabica coffee`+spam_prod$`robusta coffee`
spam_prod$`arabica coffee`<-NULL
spam_prod$`robusta coffee`<-NULL

## 2.3) Mask to geoboundaries #####
# mask to focal countries
spam_prod<-terra::crop(spam_prod,geoboundaries)
spam_prod_i<-terra::crop(spam_prod_i,geoboundaries)
spam_prod_r<-terra::crop(spam_prod_r,geoboundaries)

## 2.4) Extract spam totals by geoboundaries #####
# Use terra::zonal to sum production values by administrative unit
spam_prod_admin0_ex <- terra::zonal(spam_prod, admin_rast, fun = "sum", na.rm = TRUE)

# Optionally, add administrative codes back to the result
# Assuming `admin_rast` uses codes (e.g., iso3) as its values
spam_prod_admin0_ex <- data.table::as.data.table(spam_prod_admin0_ex)
spam_prod_admin0<-melt(data.table(spam_prod_admin0_ex),id.vars="iso3",variable.name = "Code",value.name="prod")

## 2.5) map spam codes values to atlas #####
spam2fao<-fread(spam2fao_url)[,short_spam2010:=toupper(short_spam2010)][long_spam2010 %in% names(spam_prod)]

## 2.6) Create proportions ####

# Step 1: Extract levels
iso3_levels<-levels(admin_rast)[[1]]
spam_prod_admin0_ex<-merge(spam_prod_admin0_ex,iso3_levels,by="iso3",all.x=T)

# Step 2: Create a raster stack directly from spam_prod_admin0_ex
spam_tot <- terra::rast(lapply(names(spam_prod), function(colname) {
  # Extract the column for the current variable and its corresponding ISO3 codes
  temp_data <- spam_prod_admin0_ex[, c("ID", colname), with = FALSE]
  
  # Remove rows with NA in the column being processed
  #temp_data <- temp_data[!is.na(temp_data[[colname]])]
  
  # Create a reclassification matrix directly
  rcl <- as.matrix(temp_data)  # Columns: iso3, value
  # Reclassify admin_rast based on the rcl matrix
  raster_layer <- terra::classify(admin_rast, rcl = rcl, include.lowest = TRUE)
  
  return(raster_layer)
}))

# Step 3: Assign meaningful names to the layers
names(spam_tot) <- names(spam_prod)

# Step 4: Create proportions (divide pixel production by country total)
spam_prop<-spam_prod/spam_tot
names(spam_prop)<-names(spam_prod)

# Replace infinite values with NA
spam_prop[is.infinite(spam_prop)] <- NA

# 3) Download and process FAOstat data ####

## 3.1) Download data #####  
vop_file_world<-file.path(fao_dir,"Value_of_Production_E_All_Data.csv")
if(!file.exists(vop_file_world)){
  # Define the URL and set the save path
  url <- "https://fenixservices.fao.org/faostat/static/bulkdownloads/Value_of_Production_E_All_Data.zip"
  zip_file_path <- file.path(fao_dir, basename(url))
  
  # Download the file
  download.file(url, zip_file_path, mode = "wb")
  
  # Unzip the file
  unzip(zip_file_path, exdir = fao_dir)
  
  # Delete the ZIP file
  unlink(zip_file_path)
}  

remove_countries<- c("Ethiopia PDR","Sudan (former)")
atlas_iso3<-geoboundaries$ADM0_A3
target_year<-2019:2023

## 3.2) Load data #####
prod_value_i<-fread(vop_file_world,encoding = "Latin-1")

# Choose element
prod_value_i[,unique(Element)]
element<-"Gross Production Value (constant 2014-2016 thousand I$)"
# Note current thousand US$ has only 35 values whereas constant 12-16 has 157

cols<-c("Item","Element","Area","Area Code (M49)",paste0("Y",target_year))
prod_value_i<-prod_value_i[Element %in% element,..cols]

# Convert Area Code (M49) to ISO3 codes and filter by atlas_iso3 countries
prod_value_i[, M49 := as.numeric(gsub("[']", "", `Area Code (M49)`))]
prod_value_i[, iso3 := countrycode(sourcevar = M49, origin = "un", destination = "iso3c")]
prod_value_i<-prod_value_i[!is.na(iso3)]

# Any countries missing?
unique(spam_prod_admin0$iso3[!spam_prod_admin0$iso3 %in% prod_value_i$iso3 ])

# Combine similar products
prod_value_i[grep("Maize",Item),Item:="Maize (corn)"]

# Define columns to sum
y_cols <- grep("^Y\\d{4}$", names(prod_value_i), value = TRUE) # Select columns starting with "Y"

# Group by iso3 and atlas_name and sum the Y columns
prod_value_i <- prod_value_i[, lapply(.SD, sum, na.rm = TRUE), by = .(iso3, Item), .SDcols = y_cols]


val_cols<-paste0("Y",target_year)
prod_value_i[,value:=rowMeans(.SD,na.rm=T),.SDcols = val_cols[2:4]
][is.na(value),value:=rowMeans(.SD,na.rm=T),.SDcols = val_cols]

### 3.1.3) Update names ####
prod_value_i<-merge(prod_value_i,spam2fao[,.(long_spam2010,name_fao_val)],by.x="Item",by.y="name_fao_val",all.x=T)
prod_value_i<-prod_value_i[!is.na(long_spam2010)]
setnames(prod_value_i,"long_spam2010","Code")

names(spam_prod)[!names(spam_prod) %in% prod_value_i$Code]

# 4) Distribute fao vop to spam production ####
final_vop_i<-prod_value_i[,list(iso3,Code,value)]

final_vop_i[is.na(value)]

# Distribute $I data we do have to production raster
final_vop_i_cast<-dcast(final_vop_i,iso3~Code)

# Convert value to vector then raster
final_vop_i_vect<-geoboundaries
final_vop_i_vect<-merge(final_vop_i_vect,final_vop_i_cast,by="iso3",all.x=T)

iso3_levels<-levels(admin_rast)[[1]]
final_vop_i<-as.data.table(merge(final_vop_i_vect,iso3_levels,by="iso3",all.x=T))

# Create a raster stack directly from final_vop_i_vect
final_vop_i_rast <- terra::rast(lapply(names(spam_prod), function(colname) {
  # Extract the column for the current variable and its corresponding ISO3 codes
  temp_data <- final_vop_i[, c("ID", colname), with = FALSE]
  
  # Remove rows with NA in the column being processed
  #temp_data <- temp_data[!is.na(temp_data[[colname]])]
  
  # Create a reclassification matrix directly
  rcl <- as.matrix(temp_data)  # Columns: iso3, value
  # Reclassify admin_rast based on the rcl matrix
  raster_layer <- terra::classify(admin_rast, rcl = rcl, include.lowest = TRUE)
  
  return(raster_layer)
}))

names(final_vop_i_rast)<-names(spam_prod)

# Multiply national VoP by cell proportion
spam_vop_intd<-spam_prop*final_vop_i_rast

# Split COFF in ACOF and RCOF
coff<-spam_vop_intd$coffee
acof<-spam_prod_raw$`arabica coffee`
rcof<-spam_prod_raw$`robusta coffee`
arcof<-acof+rcof
acof<-coff * acof/arcof
names(acof)<-"arabica coffee"
rcof<-coff * rcof/arcof
names(rcof)<-"robusta coffee"

spam_vop_intd$`robusta coffee`<-rcof
spam_vop_intd$`arabica coffee`<-acof
spam_vop_intd<-spam_vop_intd[[order(names(spam_vop_intd))]]
spam_vop_intd$coffee<-NULL

terra::writeRaster(round(spam_vop_intd*1000,0),file.path(spam_dir,"ssa_crop_vop2020_int2015-2021_a.tif"),overwrite=T)

# 5) Split between rainfed and irrigated ####
spam_prod_i<-spam_prod_i[[order(names(spam_prod_i))]]
spam_prod_a<-spam_prod_a[[order(names(spam_prod_a))]]

spam_prod_i_p<-pbapply::pblapply(1:nlyr(spam_prod_i),FUN=function(i){
  a<-spam_prod_i[[i]]
  b<-spam_prod_a[[i]]
  a[!is.na(values(a))]<-a[!is.na(values(a))]/b[!is.na(values(a))]
  return(a)
})
spam_prod_i_p<-rast(spam_prod_i_p)


# Irrigated = irrigated_prod/total_prod * vop
all(names(spam_vop_intd)==names(spam_prod_i_p))
spam_vop_intd_i<-spam_vop_intd*spam_prod_i_p
# Rainfed is all - irrigated
spam_vop_intd_r<-spam_vop_intd- spam_vop_intd_i

terra::writeRaster(round(spam_vop_intd_i*1000,1),file.path(spam_dir,"ssa_crop_vop2020_int2015-2021_i.tif"),overwrite=T)
terra::writeRaster(round(spam_vop_intd_r*1000,1),file.path(spam_dir,"ssa_crop_vop2020_int2015-2021_r.tif"),overwrite=T)

# Check data
a<-spam_vop_intd$SOYB
i<-spam_vop_intd_i$SOYB
r<-spam_vop_intd_r$SOYB

# i + r should be virtually the same as a
plot(a-sum(c(i,r),na.rm=T))

