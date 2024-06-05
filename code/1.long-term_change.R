#### Process ISIMIP data across sectors
#### Coding: Aurore A. Maureaud, June 2024

# load libraries
library(raster)
library(ncdf4)
library(here)
library(tidyverse)
library(ggplot2)
library(sf)
sf_use_s2(FALSE)
library(exactextractr)
library(abind)
source("functions/times.R")
source("functions/get_areas.R")

# regions data
regions <- st_read("data/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp") %>% 
  filter(POL_TYPE != "Joint regime (EEZ)") %>% 
  dplyr::select(UNION)

# output files: pct = percent; diff = difference; ts = time-series; 
# avg = average (end of century); g = global; r = region; cs = cross-sector
pct_diff_ts_g_cs <- data.frame()
pct_diff_avg_g_cs <- data.frame()
pct_diff_ts_r_cs <- data.frame()
pct_diff_avg_r_cs <- data.frame()


################################################################################
#### 1. AGRICULTURE
################################################################################
# pre-processed data provided by Jonas Jägermeyr from 2021 Nature Food paper
# initial set up
sector <- "agriculture"
eco_model <- c("CROVER","CYGMA1p74","DSSAT-Pythia","EPIC-IIASA","ISAM","LDNDC",
               "LPJmL","PDSSAT","PEPIC","PROMET","SIMPLACE-LINTUL5")
crops <- c("maize","wheat","soybean","rice")
create_list_files <- FALSE

# load observational yield map
obs_yield <- nc_open("data/observational_yield/GGCMI_observational_reference_yield_map.nc4")
# only keep the total rainfed + irrigated crop yields (4th dimension)
obs_yield <- ncvar_get(obs_yield, obs_yield$var$yield)[,,,3]

# path to data source
path <- paste0("~/Documents/Rutgers University/Minerva/Data/ISIMIP3b/OutputData/",sector,"/pre-processed")

#### A. gather & create information about simulations ----
# load models and gather outputs
if(create_list_files == TRUE){
  models <- list.files(path = path, pattern = "*.nc") %>% 
    data.frame() %>% 
    dplyr::rename("file.name" = ".") %>% 
    mutate(eco_model = str_split(file.name, pattern = "_", simplify = TRUE)[,6],
           eco_model = str_split(eco_model, pattern = ".nc4", simplify = TRUE)[,1],
           climate_model = str_split(file.name, pattern = "_", simplify = TRUE)[,5],
           bias_adjustment = "w5e5",
           experiment_climate = str_split(file.name, pattern = "_", simplify = TRUE)[,4],
           experiment_human_forcing = "2015soc",
           experiment_sensitivity = "default",
           output_variable = "crop_yield",
           spatial_res = 0.5,
           time_res = "annual-gs",
           start_year = 1983,
           end_year = 2099,
           #end_year = str_split(end_year, pattern = ".nc", simplify = TRUE)[,1],
           output_crop = NA_character_,
           output_irr = NA_character_,
           #output_crop = str_split(output_variable, "-", simplify = TRUE)[,2],
           #output_irr = str_split(output_variable, "-", simplify = TRUE)[,3],
           #output_variable = str_split(output_variable, "-", simplify = TRUE)[,1])
    )
  
  write.csv(data.frame(models), 
            file = paste0(path,"/models.csv"),
            row.names = FALSE)
}

#### B. global aggregation ----
# list of models and parameters
name.models <- read.csv(paste0(path,"/models.csv")) %>% 
  mutate(variable = paste(eco_model, climate_model, experiment_climate, experiment_human_forcing, 
                          experiment_sensitivity, output_variable, 
                          start_year, end_year, sep = "_")) %>% 
  filter(eco_model != "acea")

# create output file
global <- rbind(name.models %>% mutate(output_crop = "maize"), 
                name.models %>% mutate(output_crop = "wheat"), 
                name.models %>% mutate(output_crop = "soybean"),
                name.models %>% mutate(output_crop = "rice")) %>% 
  arrange(climate_model, experiment_climate, eco_model) %>% 
  mutate(percent_diff = NA,
         log_ratio = NA)
yearly_percent_diff <- data.frame()

# create time variables
years <- sort(seq(from=1983, to=2099, by=1))
time <- c(1:length(years))
time_f <- data.frame(cbind(time, years)) %>% 
  mutate(time = as.character(time))
yearly_percent_diff <- data.frame()

# get global aggregation
for(e in 1:nrow(name.models)){
  
  # file selected
  print(name.models$variable[e])
  
  # read file
  output_i <- nc_open(here(paste0(path, "/", name.models$file.name[e])))
  var_array <- ncvar_get(output_i, output_i$var$`yield change`)
  
  # re-calculate the absolute yields with the observational yields for the ref. period
  var_array_abs <- array(dim = dim(var_array))
  for(z in 1:4){
    for(i in 1:117){
      var_array_abs[,,i,z] <- obs_yield[,,z]+obs_yield[,,z]*var_array[,,i,z]/100
    }
  }
  
  # standardize by grid cell size
  area_i <- t(as.matrix(grid_cells_areas(var_array_abs)))
  for(a in 1:dim(var_array_abs)[3]){
    for(b in 1:dim(var_array_abs)[4]){
      # area_i in km2, so multiple by 1e6 to get m2
      var_array_abs[,,a,b] <- var_array_abs[,,a,b]*area_i*1e6
    }
  }
  
  var_array_abs <- apply(var_array_abs, c(3,4), sum, na.rm=T)
  
  # average yield around 2015
  agg_2010_2019 <- apply(var_array_abs[28:37,], 2, mean, na.rm=TRUE)
  
  # get relative values compared to decadal reference period
  var_array <- var_array_log <- array(dim = dim(var_array_abs))
  for(z in 1:4){
    var_array[,z] <- 100*(var_array_abs[,z]-agg_2010_2019[z])/agg_2010_2019[z]
  }
  
  # % difference per decade
  percent_diff_10y <- data.frame(cbind(time_f, var_array)) %>% 
    rename(maize = X1,
           wheat = X2,
           soybean = X3,
           rice = X4) %>%
    mutate(eco_model = name.models$eco_model[e],
           climate_model = name.models$climate_model[e],
           experiment_climate = name.models$experiment_climate[e])
  
  if(nrow(yearly_percent_diff)==0){yearly_percent_diff <- percent_diff_10y
  } else {yearly_percent_diff <- rbind(yearly_percent_diff, percent_diff_10y)}
  
  # % difference per decade
  percent_diff_10y <- apply(var_array[108:117,], 2, mean, na.rm=TRUE)
  
  i <- which(global$variable == name.models$variable[e])
  for(z in 1:4){
    # % diff aggregate
    global$percent_diff[i[z]] <- percent_diff_10y[z]
  }
  
}

# global temporal trend data
yearly_percent_diff <- yearly_percent_diff %>% 
  mutate(spatial_scale = "global",
         sector = "agriculture") %>% 
  pivot_longer(3:6, names_to = "output_crop", values_to = "percent_diff") %>% 
  select(sector, years, spatial_scale, eco_model, climate_model, experiment_climate, output_crop, percent_diff) %>% 
  dplyr::rename(output_variable = output_crop)
pct_diff_ts_g_cs <- rbind(pct_diff_ts_g_cs, yearly_percent_diff)
rm(yearly_percent_diff)

# end of the century average data
global <- global %>% 
  mutate(spatial_scale = "global",
         sector = "agriculture") %>% 
  select(sector, spatial_scale, eco_model, climate_model, experiment_climate, output_crop, percent_diff) %>% 
  dplyr::rename(output_variable = output_crop)
pct_diff_avg_g_cs <- rbind(pct_diff_avg_g_cs, global)
rm(global)


#### C. aggregation for countries ----
# output data file
regions_agg <- sort(unique(regions$UNION))
global <- rbind(name.models %>% mutate(output_crop = "maize"), 
                name.models %>% mutate(output_crop = "wheat"), 
                name.models %>% mutate(output_crop = "soybean"),
                name.models %>% mutate(output_crop = "rice")) %>% 
  arrange(climate_model, experiment_climate, eco_model)
for(r in 1:length(regions_agg)){
  global[,ncol(global)+1] <- NA
  names(global)[ncol(global)] <- regions_agg[r]
}
yearly_percent_diff <- data.frame()

# year data
years <- sort(seq(from=1983, to=2099, by=1))
time <- c(1:length(years))
time_f <- data.frame(cbind(time, years)) %>% 
  mutate(time = as.character(time))
yearly_percent_diff <- data.frame()

# aggregation of crop data
for(e in 1:nrow(name.models)){
  
  # file selected
  print(name.models$variable[e])
  
  # read file
  output_i <- nc_open(here(paste0(path, "/", name.models$file.name[e])))
  var_array <- ncvar_get(output_i, output_i$var$`yield change`)
  
  # re-calculate the absolute yields with the observational yields for the ref. period
  var_array_abs <- array(dim = dim(var_array))
  for(z in 1:4){
    for(i in 1:117){
      var_array_abs[,,i,z] <- obs_yield[,,z]+obs_yield[,,z]*var_array[,,i,z]/100
    }
  }
  
  # standardize by grid cell size
  area_i <- t(as.matrix(grid_cells_areas(var_array_abs)))
  for(a in 1:dim(var_array_abs)[3]){
    for(b in 1:dim(var_array_abs)[4]){
      # area_i in km2, so multiple by 1e6 to get m2
      var_array_abs[,,a,b] <- var_array_abs[,,a,b]*area_i*1e6
    }
  }
  
  # extract crop data per region and get sum by geographical unit
  var_region <- array(data=NA, dim=c(length(regions_agg),dim(var_array_abs)[3],4))
  for(z in 1:4){
    var_array_abs_r <- raster::brick(aperm(var_array_abs[,,,z], c(2,1,3)), xmn=-180, xmx = 180, ymn=-90, ymx=90)
    var_regions <- as.matrix(exact_extract(var_array_abs_r, regions, "sum"))
    for(i in 1:length(regions_agg)){
      k <- which(regions$UNION == regions_agg[i])
      if(length(k)==1){var_region[i,,z] <- var_regions[k,]
      } else {var_region[i,,z] <- apply(var_regions[k,], 2, sum, na.rm=T)}
    }
    rm(var_array_abs_r, var_regions, k)
  }
  var_region <- aperm(var_region,c(2,1,3))
  
  # average yield around 2015 (2010-2019)
  agg_2010_2019 <- apply(var_region[28:37,,], c(2,3), mean, na.rm=TRUE)
  
  # get relative values compared to reference period
  var_array <- var_array_log <- array(dim = dim(var_region))
  for(z in 1:4){
    for(i in 1:117){
      var_array[i,,z] <- 100*(var_region[i,,z]-agg_2010_2019[,z])/agg_2010_2019[,z]
    }
  }
  
  # % difference per decade
  yearly_percent_diff_e <- data.frame()
  for(z in 1:4){
    xx <- data.frame(cbind(var_array[,,z], time_f$years)) %>% 
      mutate(output_crop = crops[z])
    names(xx)[1:length(regions_agg)] <- regions_agg
    names(xx)[length(regions_agg)+1] <- "years"
    xx <- xx %>% 
      pivot_longer(1:length(regions_agg), names_to = "regions", values_to = "percent_diff") %>% 
      mutate(eco_model = name.models$eco_model[e],
             climate_model = name.models$climate_model[e],
             experiment_climate = name.models$experiment_climate[e])
    if(z == 1){yearly_percent_diff_e <- xx} else {yearly_percent_diff_e <- rbind(yearly_percent_diff_e, xx)}
    rm(xx)
  }
  
  if(nrow(yearly_percent_diff)==0){yearly_percent_diff <- yearly_percent_diff_e
  } else {yearly_percent_diff <- rbind(yearly_percent_diff, yearly_percent_diff_e)}
  
  ## % difference per decade
  percent_diff_10y <- apply(var_array[108:117,,], c(2,3), mean, na.rm=TRUE)
  i <- which(global$variable == name.models$variable[e])
  for(z in 1:4){
    global[i[z],16:ncol(global)] <- percent_diff_10y[,z]
  }
  
  rm(yearly_percent_diff_e, percent_diff_10y, var_array, agg_2010_2019, var_region,
     var_array_abs, output_i) 
}


# save the temporal trend data
yearly_percent_diff <- yearly_percent_diff %>% 
  mutate(spatial_scale = "regions",
         sector = "agriculture") %>% 
  dplyr::select(sector, years, spatial_scale, eco_model, climate_model, experiment_climate, output_crop, regions, percent_diff) %>% 
  dplyr::rename(output_variable = output_crop)
pct_diff_ts_r_cs <- rbind(pct_diff_ts_r_cs, yearly_percent_diff)
rm(yearly_percent_diff)

# save the end of the century average data
global <- global %>% 
  mutate(spatial_scale = "regions",
         sector = "agriculture") %>% 
  pivot_longer(16:ncol(global), values_to = "percent_diff", names_to = "regions") %>% 
  dplyr::select(sector, spatial_scale, eco_model, climate_model, experiment_climate, output_crop, regions, percent_diff) %>% 
  dplyr::rename(output_variable = output_crop)
pct_diff_avg_r_cs <- rbind(pct_diff_avg_r_cs, global)
rm(global)


################################################################################
#### 2. FISHERIES
################################################################################

# code options
create_list_files <- TRUE
sector <- "marine-fishery_global"
eco_model <- c("APECOSM","BOATS","DBEM","DBPM","EcoOCean","EcoTroph","FEISTY",
               "MACROECOLOGICAL","ZooMSS")

#### A. gather & create information about simulations ----
for(e in 1:length(eco_model)){
  # path to data source
  path <- paste0("~/Documents/Rutgers University/Minerva/Data/ISIMIP3b/OutputData/",sector,"/",eco_model[e])
  
  ### Load models and gather outputs
  # method on how to read netcdf files: https://pjbartlein.github.io/REarthSysSci/netCDF.html
  
  # list files
  if(create_list_files == TRUE){
    models <- list.files(path = path, pattern = "*.nc") %>% 
      data.frame() %>% 
      dplyr::rename("file.name" = ".") %>% 
      mutate(eco_model = str_split(file.name, pattern = "_", simplify = TRUE)[,1],
             climate_model = str_split(file.name, pattern = "_", simplify = TRUE)[,2],
             bias_adjustment = str_split(file.name, pattern = "_", simplify = TRUE)[,3],
             experiment_climate = str_split(file.name, pattern = "_", simplify = TRUE)[,4],
             experiment_human_forcing = str_split(file.name, pattern = "_", simplify = TRUE)[,5],
             experiment_sensitivity = str_split(file.name, pattern = "_", simplify = TRUE)[,6],
             output_variable = str_split(file.name, pattern = "_", simplify = TRUE)[,7],
             spatial_res = str_split(file.name, pattern = "_", simplify = TRUE)[,8],
             time_res = str_split(file.name, pattern = "_", simplify = TRUE)[,9],
             start_year = str_split(file.name, pattern = "_", simplify = TRUE)[,10],
             end_year = str_split(file.name, pattern = "_", simplify = TRUE)[,11],
             end_year = str_split(end_year, pattern = ".nc", simplify = TRUE)[,1],
             output_crop = NA_character_,
             output_irr = NA_character_)
    
    write.csv(data.frame(models), 
              file = paste0(path,"/models.csv"),
              row.names = FALSE)
  }
}


#### B. global aggregation ----
years <- sort(seq(from=1983, to=2100, by=1))
time <- c(1:length(years))
time_f <- data.frame(cbind(time, years)) %>% 
  mutate(time = as.character(time))
yearly_percent_diff <- data.frame()

global <- data.frame() %>% 
  mutate(eco_model = NA_character_,
         climate_model = NA_character_,
         experiment_climate = NA_character_,
         percent_diff = NA)

for(e in 1:length(eco_model)){
  
  print(eco_model[e])
  
  # path to data source
  path <- paste0("~/Documents/Rutgers University/Minerva/Data/ISIMIP3b/OutputData/",sector,"/",eco_model[e])
  
  name.models <- read.csv(paste0(path,"/models.csv")) %>% 
    mutate(variable = paste(eco_model, climate_model, experiment_climate, experiment_human_forcing, 
                            experiment_sensitivity, output_variable,
                            start_year, end_year, sep = "_")) %>% 
    filter(experiment_human_forcing == "nat",
           experiment_climate %in% c("ssp126", "ssp585", "historical"),
           output_variable %in% c("tcb","tcblog10"))
  
  for(i in 1:nrow(name.models)){
    
    print(i)
    
    if(name.models$experiment_climate[i]!="historical"){
      # read file
      output_i <- nc_open(here(paste0(path, "/", name.models$file.name[i])))
      var_array <- ncvar_get(output_i, names(output_i$var))
      if(length(dim(var_array))==4){
        if(dim(var_array)[3]==6){var_array2 <- apply(var_array[,,2:6,], c(1,2,4), sum, na.rm=T)}
        if(dim(var_array)[3]==5){var_array2 <- apply(var_array[,,2:5,], c(1,2,4), sum, na.rm=T)}
        var_array <- var_array2
      }
      time_i <- get_times(name.models, i)
      area_i <- t(as.matrix(grid_cells_areas(var_array)))
      # add historical simulations 
      h <- which(name.models$climate_model==name.models$climate_model[i] & name.models$experiment_climate == "historical" & name.models$output_variable == name.models$output_variable[i])
      time_h <- get_times(name.models, h)
      output_i_hist <- nc_open(here(paste0(path, "/", name.models$file.name[h])))
      var_hist <- ncvar_get(output_i_hist, names(output_i_hist$var))
      if(length(dim(var_hist))==4){
        if(dim(var_hist)[3]==6){var_hist2 <- apply(var_hist[,,2:6,], c(1,2,4), sum, na.rm=T)}
        if(dim(var_hist)[3]==5){var_hist2 <- apply(var_hist[,,2:5,], c(1,2,4), sum, na.rm=T)}
        var_hist <- var_hist2
      }
      # select years after 1982
      y <- which(time_h$years>(time_f$years[1]-1))
      var_hist <- var_hist[,,y[1]:dim(var_hist)[3]]
      # combine historical and future simulations
      var_array <- abind(var_hist, var_array, along=3)
      time_c <- get_times_combined(name.models, i, start_year = time_f$years[1])
      rm(y, h, time_h, time_i, var_hist, output_i, output_i_hist)
      
      # standardize by grid cell size
      for(a in 1:dim(var_array)[3]){
        # area_i in km2, so multiple by 1e6 to get m2
        var_array[,,a] <- var_array[,,a]*area_i*1e6
      }
      
      # get global sum or mean per year
      var_array <- apply(var_array, 3, sum, na.rm=T)
      
      # sum up months if model is ran monthly
      if(name.models$time_res[i]=="monthly"){
        var_m <- cbind(time_c, var_array) %>% 
          group_by(years) %>% 
          summarize(var_array = mean(var_array, na.rm=T))
        var_array <- var_m$var_array
      }      
      
      # average biomass 2010-2019
      t_2010 <- min(which(time_f$years==2010))
      t_2019 <- max(which(time_f$years==2019))
      agg_2010_2019 <- mean(var_array[t_2010:t_2019], na.rm=TRUE)
      t_2090 <- min(which(time_f$years==2090))
      t_2099 <- max(which(time_f$years==2099))
      
      ## get relative values compared to decadal reference period
      var_array_10y <- 100*((var_array-agg_2010_2019)/agg_2010_2019)
      
      # % difference for end decade
      vec <- data.frame(name.models$eco_model[i], name.models$experiment_climate[i], 
                        name.models$climate_model[i], name.models$output_variable[i],
                        mean(var_array_10y[t_2090:t_2099], na.rm=T))
      global <- rbind(global, vec)
      
      ## % difference per decade
      percent_diff_10y <- data.frame(cbind(time_f, var_array_10y)) %>% 
        rename(var_pd = var_array_10y) %>% 
        mutate(eco_model = name.models$eco_model[i],
               climate_model = name.models$climate_model[i],
               experiment_climate = name.models$experiment_climate[i],
               output_variable = name.models$output_variable[i])
      
      if(nrow(yearly_percent_diff)==0){yearly_percent_diff <- percent_diff_10y
      } else {yearly_percent_diff <- rbind(yearly_percent_diff, percent_diff_10y)}
      
      rm(percent_diff_10y, var_array_10y, var_array, t_2010, t_2019, agg_2010_2019, 
         vec, t_2090, t_2099)
    }
  }
  
}

# save the temporal trend data
yearly_percent_diff <- yearly_percent_diff %>% 
  mutate(spatial_scale = "global",
         sector = sector) %>%
  rename(percent_diff = var_pd) %>% 
  dplyr::select(sector, years, spatial_scale, eco_model, climate_model, experiment_climate, output_variable, percent_diff)
pct_diff_ts_g_cs <- rbind(pct_diff_ts_g_cs, yearly_percent_diff)
rm(yearly_percent_diff)

# save the end of the century average data
names(global)[1:5] <- c("eco_model", "experiment_climate", "climate_model", "output_variable", "percent_diff")
global <- global %>% 
  mutate(spatial_scale = "global",
         sector = sector) %>% 
  dplyr::select(sector, spatial_scale, eco_model, climate_model, experiment_climate, output_variable, percent_diff)
pct_diff_avg_g_cs <- rbind(pct_diff_avg_g_cs, global)
rm(global)


#### C. country aggregation ----
# output data
global <- data.frame()
yearly_percent_diff <- data.frame()

# year data
years <- sort(seq(from=1983, to=2100, by=1))
time <- c(1:length(years))
time_f <- data.frame(cbind(time, years)) %>% 
  mutate(time = as.character(time))

# aggregation of crop data
for(e in 1:length(eco_model)){
  
  print(eco_model[e])
  
  # path to data source
  path <- paste0("~/Documents/Rutgers University/Minerva/Data/ISIMIP3b/OutputData/",sector,"/",eco_model[e])
  
  name.models <- read.csv(paste0(path,"/models.csv")) %>% 
    mutate(variable = paste(eco_model, climate_model, experiment_climate, experiment_human_forcing, 
                            experiment_sensitivity, output_variable,
                            start_year, end_year, sep = "_")) %>% 
    filter(experiment_human_forcing == "nat",
           experiment_climate %in% c("ssp126", "ssp585", "historical"),
           output_variable %in% c("tcb","tcblog10"))
  
  for(i in 1:nrow(name.models)){
    
    if(name.models$experiment_climate[i]!="historical"){
      # read file
      output_i <- nc_open(here(paste0(path, "/", name.models$file.name[i])))
      var_array <- ncvar_get(output_i, names(output_i$var))
      # sum up biomass sizes when it's the tcb log10 variable for fish from 10g-100kg
      if(length(dim(var_array))==4){
        if(dim(var_array)[3]==6){var_array2 <- apply(var_array[,,2:6,], c(1,2,4), sum, na.rm=T)}
        if(dim(var_array)[3]==5){var_array2 <- apply(var_array[,,2:5,], c(1,2,4), sum, na.rm=T)}
        var_array <- var_array2
      }
      time_i <- get_times(name.models, i)
      area_i <- t(as.matrix(grid_cells_areas(var_array)))
      # add historical simulations 
      h <- which(name.models$climate_model==name.models$climate_model[i] & name.models$experiment_climate == "historical" & name.models$output_variable == name.models$output_variable[i])
      time_h <- get_times(name.models, h)
      output_i_hist <- nc_open(here(paste0(path, "/", name.models$file.name[h])))
      var_hist <- ncvar_get(output_i_hist, names(output_i_hist$var))
      if(length(dim(var_hist))==4){
        if(dim(var_hist)[3]==6){var_hist2 <- apply(var_hist[,,2:6,], c(1,2,4), sum, na.rm=T)}
        if(dim(var_hist)[3]==5){var_hist2 <- apply(var_hist[,,2:5,], c(1,2,4), sum, na.rm=T)}
        var_hist <- var_hist2
      }
      # select years after 1982
      y <- which(time_h$years>1982)
      var_hist <- var_hist[,,y[1]:dim(var_hist)[3]]
      # combine historical and future simulations
      var_array <- abind(var_hist, var_array, along=3)
      time_c <- get_times_combined(name.models, i, start_year = 1983)
      rm(y, h, time_h, time_i, var_hist, output_i, output_i_hist)
      
      # standardize by grid cell size
      for(a in 1:dim(var_array)[3]){
        # area_i in km2, so multiple by 1e6 to get m2
        var_array[,,a] <- var_array[,,a]*area_i*1e6
      }
      
      # extract ocean data per region and get sum by geographical unit
      var_region <- array(data=NA, dim=c(length(regions_agg),dim(var_array)[3]))
      var_array_r <- raster::brick(aperm(var_array, c(2,1,3)), xmn=-180, xmx = 180, ymn=-90, ymx=90)
      var_regions <- as.matrix(exact_extract(var_array_r, regions, "sum"))
      for(r in 1:length(regions_agg)){
        k <- which(regions$UNION == regions_agg[r])
        if(length(k)==1){var_region[r,] <- var_regions[k,]
        } else {var_region[r,] <- apply(var_regions[k,], 2, sum, na.rm=T)}
      }
      rm(k, var_regions, var_array_r)
      var_region <- aperm(var_region,c(2,1))
      
      # aggregate per year if model ran monthly
      if(name.models$time_res[i]=="monthly"){
        var_m <- cbind(time_c, var_region)
        names(var_m)[4:ncol(var_m)] <- regions_agg
        var_m <- var_m %>% 
          pivot_longer(regions_agg, names_to = "regions", values_to = "biomass") %>% 
          group_by(years, regions) %>% 
          summarize(biomass = mean(biomass, na.rm=T)) %>% 
          pivot_wider(names_from = regions, values_from = biomass)
        var_region <- as.matrix(var_m[,2:ncol(var_m)])
      }      
      
      # get time values
      t_2010 <- min(which(time_f$years==2010))
      t_2019 <- max(which(time_f$years==2019))
      t_2090 <- min(which(time_f$years==2090))
      t_2099 <- max(which(time_f$years==2099))
      
      # average biomass 1990-1999
      agg_2010_2019 <- apply(var_region[t_2010:t_2019,], 2, mean, na.rm=TRUE)
      
      ## get relative values compared to reference period
      var_array <- array(dim = dim(var_region))
      for(y in 1:nrow(time_f)){
        var_array[y,] <- 100*(var_region[y,]-agg_2010_2019)/agg_2010_2019
      }
      
      ## % difference per decade
      percent_diff_10y <- data.frame(cbind(var_array, time_f$years))
      names(percent_diff_10y)[1:length(regions_agg)] <- regions_agg
      names(percent_diff_10y)[length(regions_agg)+1] <- "years"
      percent_diff_10y <- percent_diff_10y %>% 
        pivot_longer(1:length(regions_agg), names_to = "regions", values_to = "percent_diff") %>% 
        mutate(eco_model = name.models$eco_model[i],
               climate_model = name.models$climate_model[i],
               experiment_climate = name.models$experiment_climate[i],
               output_variable = name.models$output_variable[i])
      
      if(nrow(yearly_percent_diff)==0){yearly_percent_diff <- percent_diff_10y
      } else {yearly_percent_diff <- rbind(yearly_percent_diff, percent_diff_10y)}
      
      # % difference for end decade
      vec <- data.frame(name.models$eco_model[i], name.models$experiment_climate[i], 
                        name.models$climate_model[i], name.models$output_variable[i],
                        apply(var_array[t_2090:t_2099,], 2, mean, na.rm=T), regions_agg)
      if(nrow(global)==0){global <- vec} else {global <- rbind(global, vec)}
      
      rm(percent_diff_10y, var_array, t_2010, t_2019, agg_2010_2019, t_2090, t_2099, vec)
    }
  }
  save.image(file = "data/ag_wa_revised_fi.RData")
}

## save data
# save the temporal trend data
yearly_percent_diff <- yearly_percent_diff %>% 
  mutate(spatial_scale = "regions",
         sector = sector) %>% 
  dplyr::select(sector, years, spatial_scale, eco_model, climate_model, experiment_climate, output_variable, regions, percent_diff)
pct_diff_ts_r_cs <- rbind(pct_diff_ts_r_cs, yearly_percent_diff)
rm(yearly_percent_diff)

# save the end of the century average data
names(global)[1:6] <- c("eco_model", "experiment_climate", "climate_model", "output_variable", "percent_diff", "regions")
global <- global %>% 
  mutate(spatial_scale = "regions",
         sector = sector) %>% 
  dplyr::select(sector, spatial_scale, eco_model, climate_model, experiment_climate, output_variable, regions, percent_diff)
pct_diff_avg_r_cs <- rbind(pct_diff_avg_r_cs, global)
rm(global)


################################################################################
#### 3. WATER
################################################################################

# code options
create_list_files <- TRUE
sector <- "water_global"
eco_model <- c("CWatM","H08","JULES-W2","MIROC-INTEG-LAND","WaterGAP2-2e","WEB-DHM-SG")


#### A. gather & create information about simulations ----

for(e in 1:length(eco_model)){
  # path to data source
  path <- paste0("~/Documents/Rutgers University/Minerva/Data/ISIMIP3b/OutputData/",sector,"/",eco_model[e])

  # list files
  if(create_list_files == TRUE){
    models <- list.files(path = path, pattern = "*.nc") %>% 
      data.frame() %>% 
      dplyr::rename("file.name" = ".") %>% 
      mutate(eco_model = str_split(file.name, pattern = "_", simplify = TRUE)[,1],
             climate_model = str_split(file.name, pattern = "_", simplify = TRUE)[,2],
             bias_adjustment = str_split(file.name, pattern = "_", simplify = TRUE)[,3],
             experiment_climate = str_split(file.name, pattern = "_", simplify = TRUE)[,4],
             experiment_human_forcing = str_split(file.name, pattern = "_", simplify = TRUE)[,5],
             experiment_sensitivity = str_split(file.name, pattern = "_", simplify = TRUE)[,6],
             output_variable = str_split(file.name, pattern = "_", simplify = TRUE)[,7],
             spatial_res = str_split(file.name, pattern = "_", simplify = TRUE)[,8],
             time_res = str_split(file.name, pattern = "_", simplify = TRUE)[,9],
             start_year = str_split(file.name, pattern = "_", simplify = TRUE)[,10],
             end_year = str_split(file.name, pattern = "_", simplify = TRUE)[,11],
             end_year = str_split(end_year, pattern = ".nc", simplify = TRUE)[,1],
             output_crop = NA_character_,
             output_irr = NA_character_)
    
    write.csv(data.frame(models), file = paste0(path,"/models.csv"), row.names = FALSE)
  }
}


#### B. global aggregation ----
years <- sort(seq(from=1983, to=2100, by=1))
time <- c(1:length(years))
time_f <- data.frame(cbind(time, years)) %>% 
  mutate(time = as.character(time))
yearly_percent_diff <- data.frame()

global <- data.frame() %>% 
  mutate(output_variable = NA_character_,
         eco_model = NA_character_,
         climate_model = NA_character_,
         experiment_climate = NA_character_,
         experiment_human_forcing = NA_character_,
         percent_diff = NA)

for(e in 1:length(eco_model)){
  
  print(eco_model[e])
  
  # path to data source
  path <- paste0("~/Documents/Rutgers University/Minerva/Data/ISIMIP3b/OutputData/",sector,"/",eco_model[e])
  
  name.models <- read.csv(paste0(path,"/models.csv")) %>% 
    mutate(variable = paste(eco_model, climate_model, experiment_climate, experiment_human_forcing, 
                            experiment_sensitivity, output_variable,
                            start_year, end_year, sep = "_")) %>% 
    filter(experiment_human_forcing %in% c("2015soc"),
           experiment_climate %in% c("ssp126", "ssp585", "historical"),
           output_variable %in% c("tws","ptotww","qtot"),
           experiment_sensitivity == "default")
  
  if(nrow(name.models)>0){
    for(i in 1:nrow(name.models)){
      
      if(name.models$experiment_climate[i]!="historical"){
        
        print(i)
        
        # read file
        output_i <- nc_open(here(paste0(path, "/", name.models$file.name[i])))
        var_array <- ncvar_get(output_i, names(output_i$var))
        time_i <- get_times(name.models, i)
        var_array_i <- apply(var_array, 3, sum, na.rm=T)
        # add historical simulations 
        h <- which(name.models$climate_model==name.models$climate_model[i] & name.models$experiment_climate == "historical" & name.models$output_variable==name.models$output_variable[i])
        time_h <- get_times(name.models, h)
        output_i_hist <- nc_open(here(paste0(path, "/", name.models$file.name[h])))
        var_hist <- ncvar_get(output_i_hist, names(output_i_hist$var))
        var_hist_h <- apply(var_hist, 3, sum, na.rm=T)
        # select years after 1982
        y <- which(time_h$years>1982)
        var_hist <- var_hist[,,y[1]:dim(var_hist)[3]]
        # combine historical and future simulations
        var_array <- abind(var_hist, var_array, along=3)
        time_c <- get_times_combined(name.models, i, start_year = 1983)
        var_array_c <- apply(var_array, 3, sum, na.rm=T)
        rm(y, h, time_h, time_i, var_hist, output_i, output_i_hist)
        rm(var_hist_h, var_array_i, var_array_c)
        
        # standardize by grid cell size
        for(a in 1:dim(var_array)[3]){
          # area_i in km2, so multiple by 1e6 to get m2
          var_array[,,a] <- var_array[,,a]*area_i*1e6
        }
        
        # get global sum or mean per year
        var_array <- apply(var_array, 3, sum, na.rm=T)
        
        # sum up months if model is ran monthly
        if(name.models$time_res[i]=="monthly"){
          var_m <- cbind(time_c, var_array) %>% 
            group_by(years) %>% 
            summarize(var_array = mean(var_array, na.rm=T))
          var_array <- var_m$var_array
        }      
        
        # average biomass 2010-2019
        t_2010 <- min(which(time_f$years==2010))
        t_2019 <- max(which(time_f$years==2019))
        agg_2010_2019 <- mean(var_array[t_2010:t_2019], na.rm=TRUE)
        t_2090 <- min(which(time_f$years==2090))
        t_2099 <- max(which(time_f$years==2099))
        
        ## get relative values compared to decadal reference period
        var_array_10y <- 100*((var_array-agg_2010_2019)/agg_2010_2019)
        
        # % difference for end decade
        vec <- data.frame(name.models$output_variable[i], name.models$eco_model[i], name.models$experiment_climate[i], 
                          name.models$climate_model[i], name.models$experiment_human_forcing[i],
                          mean(var_array_10y[t_2090:t_2099], na.rm=T))
        global <- rbind(global, vec)
        
        ## % difference per decade
        percent_diff_10y <- data.frame(cbind(time_f, var_array_10y)) %>% 
          rename(var_pd = var_array_10y) %>% 
          mutate(eco_model = name.models$eco_model[i],
                 climate_model = name.models$climate_model[i],
                 experiment_climate = name.models$experiment_climate[i],
                 experiment_human_forcing = name.models$experiment_human_forcing[i],
                 output_variable = name.models$output_variable[i])
        
        if(nrow(yearly_percent_diff)==0){yearly_percent_diff <- percent_diff_10y
        } else {yearly_percent_diff <- rbind(yearly_percent_diff, percent_diff_10y)}
        
        rm(percent_diff_10y, var_array_10y, var_array, t_2010, t_2019, agg_2010_2019, vec, t_2090, t_2099)
      }
    }
  }
}

# save the temporal trend data
yearly_percent_diff <- yearly_percent_diff %>% 
  mutate(spatial_scale = "global",
         sector = "water_global") %>%
  rename(percent_diff = var_pd) %>% 
  dplyr::select(sector, years, spatial_scale, eco_model, climate_model, experiment_climate, output_variable, percent_diff)
pct_diff_ts_g_cs <- rbind(pct_diff_ts_g_cs, yearly_percent_diff)
rm(yearly_percent_diff)

# save the end of the century average data
names(global)[1:6] <- c("output_variable", "eco_model", "experiment_climate", "climate_model", "experiment_human_forcing", "percent_diff")
global <- global %>% 
  mutate(spatial_scale = "global",
         sector = "water_global") %>% 
  dplyr::select(sector, spatial_scale, eco_model, climate_model, experiment_climate, output_variable, percent_diff)
pct_diff_avg_g_cs <- rbind(pct_diff_avg_g_cs, global)
rm(global)


#### C. country aggregation ----
# output data
global <- data.frame()
yearly_percent_diff <- data.frame()

# year data
years <- sort(seq(from=1983, to=2100, by=1))
time <- c(1:length(years))
time_f <- data.frame(cbind(time, years)) %>% 
  mutate(time = as.character(time))

for(e in 1:length(eco_model)){
  
  print(eco_model[e])
  
  # path to data source
  path <- paste0("~/Documents/Rutgers University/Minerva/Data/ISIMIP3b/OutputData/",sector,"/",eco_model[e])
  
  name.models <- read.csv(paste0(path,"/models.csv")) %>% 
    mutate(variable = paste(eco_model, climate_model, experiment_climate, experiment_human_forcing, 
                            experiment_sensitivity, output_variable,
                            start_year, end_year, sep = "_")) %>% 
    filter(experiment_human_forcing %in% c("2015soc"),
           experiment_climate %in% c("ssp126", "ssp585", "historical"),
           output_variable %in% c("tws","ptotww","qtot"),
           experiment_sensitivity == "default")
  
  if(nrow(name.models)>0){
    for(i in 1:nrow(name.models)){
      
      if(name.models$experiment_climate[i]!="historical"){
        
        # read file
        output_i <- nc_open(here(paste0(path, "/", name.models$file.name[i])))
        var_array <- ncvar_get(output_i, names(output_i$var))
        time_i <- get_times(name.models, i)
        # add historical simulations 
        h <- which(name.models$climate_model==name.models$climate_model[i] & name.models$experiment_climate == "historical" & name.models$output_variable==name.models$output_variable[i])
        time_h <- get_times(name.models, h)
        output_i_hist <- nc_open(here(paste0(path, "/", name.models$file.name[h])))
        var_hist <- ncvar_get(output_i_hist, names(output_i_hist$var))
        # select years after 1982
        y <- which(time_h$years>1982)
        var_hist <- var_hist[,,y[1]:dim(var_hist)[3]]
        # combine historical and future simulations
        var_array <- abind(var_hist, var_array, along=3)
        time_c <- get_times_combined(name.models, i, start_year = 1983)
        rm(y, h, time_h, time_i, var_hist, output_i, output_i_hist)
        
        # standardize by grid cell size
        for(a in 1:dim(var_array)[3]){
          # area_i in km2, so multiple by 1e6 to get m2
          var_array[,,a] <- var_array[,,a]*area_i*1e6
        }
        
        # extract crop data per region and get sum by geographical unit
        var_region <- array(data=NA, dim=c(length(regions_agg),dim(var_array)[3]))
        var_array_r <- raster::brick(aperm(var_array, c(2,1,3)), xmn=-180, xmx = 180, ymn=-90, ymx=90)
        var_regions <- as.matrix(exact_extract(var_array_r, regions, "sum"))
        for(r in 1:length(regions_agg)){
          k <- which(regions$UNION == regions_agg[r])
          if(length(k)==1){var_region[r,] <- var_regions[k,]
          } else {var_region[r,] <- apply(var_regions[k,], 2, sum, na.rm=T)}
        }
        rm(k, var_regions, var_array_r)
        var_region <- aperm(var_region,c(2,1))
        
        # aggregate per year if model ran monthly
        if(name.models$time_res[i]=="monthly"){
          var_m <- cbind(time_c, var_region)
          names(var_m)[4:ncol(var_m)] <- regions_agg
          var_m <- var_m %>% 
            pivot_longer(regions_agg, names_to = "regions", values_to = "biomass") %>% 
            group_by(years, regions) %>% 
            summarize(biomass = mean(biomass, na.rm=T)) %>% 
            pivot_wider(names_from = regions, values_from = biomass)
          var_region <- as.matrix(var_m[,2:ncol(var_m)])
        }      
        
        # get time values
        t_2010 <- min(which(time_f$years==2010))
        t_2019 <- max(which(time_f$years==2019))
        t_2090 <- min(which(time_f$years==2090))
        t_2099 <- max(which(time_f$years==2099))
        
        # average biomass 1990-1999
        agg_2010_2019 <- apply(var_region[t_2010:t_2019,], 2, mean, na.rm=TRUE)
        
        ## get relative values compared to reference period
        var_array <- array(dim = dim(var_region))
        for(y in 1:nrow(time_f)){
          var_array[y,] <- 100*(var_region[y,]-agg_2010_2019)/agg_2010_2019
        }
        
        ## % difference per decade
        percent_diff_10y <- data.frame(cbind(var_array, time_f$years))
        names(percent_diff_10y)[1:length(regions_agg)] <- regions_agg
        names(percent_diff_10y)[length(regions_agg)+1] <- "years"
        percent_diff_10y <- percent_diff_10y %>% 
          pivot_longer(1:length(regions_agg), names_to = "regions", values_to = "percent_diff") %>% 
          mutate(eco_model = name.models$eco_model[i],
                 climate_model = name.models$climate_model[i],
                 experiment_climate = name.models$experiment_climate[i],
                 experiment_human_forcing = name.models$experiment_human_forcing[i],
                 output_variable = name.models$output_variable[i])
        
        if(nrow(yearly_percent_diff)==0){yearly_percent_diff <- percent_diff_10y
        } else {yearly_percent_diff <- rbind(yearly_percent_diff, percent_diff_10y)}
        
        # % difference for end decade
        vec <- data.frame(name.models$eco_model[i], name.models$experiment_climate[i], name.models$climate_model[i], 
                          name.models$experiment_human_forcing[i], name.models$output_variable[i],
                          apply(var_array[t_2090:t_2099,], 2, mean, na.rm=T), regions_agg)
        if(nrow(global)==0){global <- vec} else {global <- rbind(global, vec)}
        
        rm(percent_diff_10y, var_array, t_2010, t_2019, agg_2010_2019, t_2090, t_2099, vec)
      }
    }
  }
}

## save data
# save the temporal trend data
yearly_percent_diff <- yearly_percent_diff %>% 
  mutate(spatial_scale = "regions",
         sector = "water_global") %>%
  dplyr::select(sector, years, spatial_scale, eco_model, climate_model, experiment_climate, output_variable, regions, percent_diff)
pct_diff_ts_r_cs <- rbind(pct_diff_ts_r_cs, yearly_percent_diff)
rm(yearly_percent_diff)

# save the end of the century average data
names(global)[1:6] <- c("eco_model", "experiment_climate", "climate_model", "experiment_human_forcing", "output_variable", "percent_diff")
global <- global %>% 
  mutate(spatial_scale = "regions",
         sector = "water_global") %>%
  dplyr::rename(regions = regions_agg) %>% 
  dplyr::select(sector, spatial_scale, eco_model, climate_model, experiment_climate, output_variable, regions, percent_diff)
pct_diff_avg_r_cs <- rbind(pct_diff_avg_r_cs, global)
rm(global)


################################################################################
#### 4. SAVING CROSS-SECTOR OUTPUTS
################################################################################

write.csv(pct_diff_ts_g_cs, file = "data/long-term_change/global_time_series_2010-2019_2090-2099_revised.csv", row.names = F)
write.csv(pct_diff_avg_g_cs, file = "data/long-term_change/global_average_2010-2019_2090-2099_revised.csv", row.names = F)
write.csv(pct_diff_ts_r_cs, file = "data/long-term_change/countries_time_series_2010-2019_2090-2099_revised.csv", row.names = F)
write.csv(pct_diff_avg_r_cs, file = "data/long-term_change/countries_average_2010-2019_2090-2099_revised.csv", row.names = F)

# in case running is long, save environment
save.image(file = "data/ag_wa_revised.RData")
