### This is a script that pulls individual .tif bathymetry files from https://www.nature.com/articles/s41597-022-01132-9
### and calculates the area of the lake with depths "LESS THAN" 4 meters. A proxy for littoral area. 
s = Sys.time()

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sf, stars, units, utils, doParallel)

lakes_str <- read.csv("./data/hylak_ids/hylak_hybas_names.csv") %>%
  select(hylak_id)

lake_names = list(paste0(lakes_str$hylak_id,"_bathymetry.tif"))
bathy_file <- list.files(path = "/Volumes/SeagateBackupPlusDrive/GLOBAL_BATHYMETRY")

extract_littoral <- function(bathy_file){

  whole <- stars::read_stars(paste0("/Volumes/SeagateBackupPlusDrive/GLOBAL_BATHYMETRY/",bathy_file[7],""), recursive = T) %>%
    sf::st_as_sf(.) %>% 
    dplyr::rename(depth_m = paste0(bathy_file[7])) %>%
    dplyr::group_by(depth_m) %>%
    dplyr::mutate(lake_area_m2 = st_area(geometry)) %>%
    sf::st_drop_geometry(.) %>% 
    units::drop_units(.) %>%
    dplyr::ungroup(.) %>%
    dplyr::summarise(lake_area_km2 = sum(lake_area_m2)/1000) %>%
    dplyr::mutate(hylak_id = sub("_.*", "", bathy_file[7]))
  
  lit <- stars::read_stars(paste0("/Volumes/SeagateBackupPlusDrive/GLOBAL_BATHYMETRY/",bathy_file[7],""), recursive = T) %>%
      sf::st_as_sf(.) %>% 
      dplyr::rename(depth_m = paste0(bathy_file[7])) %>%
      dplyr::mutate(max_depth_m = max(depth_m, na.rm = T)) %>%
      dplyr::mutate(mean_depth_m = mean(depth_m, na.rm = T)) %>%
      dplyr::filter(depth_m <= 3) %>%
      dplyr::group_by(depth_m) %>%
      dplyr::mutate(littoral_area_m2 = st_area(geometry)) %>% 
      sf::st_drop_geometry(.) %>% 
      units::drop_units(.) %>%
      dplyr::ungroup(.) %>%
      dplyr::summarise(mean_littoral_depth_m = mean(depth_m),
                       max_depth_m = mean(max_depth_m),
                       mean_depth_m = mean(mean_depth_m),
                       littoral_area_km2 = sum(littoral_area_m2)/1000) %>%
      dplyr::mutate(hylak_id = sub("_.*", "", bathy_file[7])) %>%
      dplyr::left_join(., whole, by = "hylak_id") %>%
      dplyr::mutate(percent_littoral = (littoral_area_km2/lake_area_km2) * 100)
      utils::write.table(., file = paste0("./data/global_littoral_area.csv"),
                  append = T,
                  row.names = F,
                  col.names = !file.exists("./data/global_littoral_area.csv"))
    
    return(unique(bathy_file))
  }
  
  
no_cores <- detectCores()-2
cl <- makeCluster(no_cores, type="FORK")
registerDoParallel(cl)
foreach(i=bathy_file) %dopar% extract_littoral(i)

e <- Sys.time()
t=e-s
print(t)





