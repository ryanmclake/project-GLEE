### This is a script that pulls individual .tif bathymetry files from https://www.nature.com/articles/s41597-022-01132-9
### and calculates the area of the lake with depths "LESS THAN" 4 meters. A proxy for littoral area. 
s = Sys.time()

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sf, stars, units, utils, doParallel)

lakes_str <- read.csv("./data/hylak_ids/hylak_hybas_names.csv") %>%
  select(hylak_id)

high_elevation_lake_names = list(paste0(high_lakes_str$hylak_id,"_bathymetry.tif"))
bathy_file <- list.files(path = "/Volumes/SeagateBackupPlusDrive/GLOBAL_BATHYMETRY")

#bathy_file <- sapply(high_elevation_lake_names, function(x){list.files(path = "/Volumes/SeagateBackupPlusDrive/GLOBAL_BATHYMETRY", pattern = x)})

extract_littoral <- function(bathy_file){

    stars::read_stars(paste0("/Volumes/SeagateBackupPlusDrive/GLOBAL_BATHYMETRY/",bathy_file,""), recursive = T) %>%
      sf::st_as_sf(.) %>% 
      dplyr::rename(depth_m = paste0(bathy_file)) %>%
      dplyr::mutate(max_depth_m = max(depth_m, na.rm = T)) %>%
      dplyr::mutate(mean_depth_m = mean(depth_m, na.rm = T)) %>%
      dplyr::filter(depth_m <= 4) %>%
      dplyr::rename(mean_littoral_depth_m = depth_m) %>%
      dplyr::mutate(littoral_area_m2 = st_area(.)) %>% 
      sf::st_drop_geometry(.) %>% 
      units::drop_units(.)%>%
      dplyr::summarise_all(funs(mean), na.rm = T) %>%
      dplyr::mutate(hylak_id = sub("_.*", "", bathy_file)) %>%
      utils::write.table(., file = paste0("./data/global_bathymetry/global_littoral_area.csv"),
                  append = T,
                  row.names = F,
                  col.names = !file.exists("./data/global_bathymetry/global_littoral_area.csv"))
    
    return(unique(bathy_file))
  }
  
  
no_cores <- detectCores()-1
cl <- makeCluster(no_cores, type="FORK")
registerDoParallel(cl)
foreach(i=bathy_file) %dopar% extract_littoral(i)

e <- Sys.time()
t=e-s
print(t)





