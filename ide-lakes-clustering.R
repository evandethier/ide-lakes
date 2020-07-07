#### LIBRARY IMPORTS ####
library(dataRetrieval)
library(tidyhydat)

library(readr)
library(readxl)

library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(ggpubr)
library(gstat)
library(ggspatial)
library(svglite)
library(plotly)

library(data.table)

library(dplyr) # hoping to move away from dplyr
library(tidyverse)
library(tidyquant)
library(tidyr)
library(broom)
library(modelr)

library(scales)
library(kdensity)
library(NbClust)
library(zoo)
library(segmented)
library(lubridate)
library(reshape2)
library(matrixStats)
library(smoother)


library(glmnet)
library(boot)
library(kernelboot)
library(np)

library(automap)
library(sp)
library(USAboundaries)
library(sf)
library(rgeos)
library(raster)
library(rgdal)
library(maptools)
library(PBSmapping)

#### THEMES ####
theme_evan <- theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linetype = 'dashed',color = 'grey70'),
    panel.grid.major.x = element_blank(),
    # panel.grid = element_blank(),
    legend.position = 'none',
    panel.border = element_rect(size = 0.5),
    text = element_text(size=8),
    axis.text = element_text(size = 8), 
    plot.title = element_text(size = 9)
  )

theme_evan_facet <- theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid = element_blank(),
    # legend.position = 'none',
    panel.border = element_rect(size = 0.5),
    strip.background = element_rect(fill = 'white'),
    text = element_text(size=12),
    axis.text = element_text(size = 12), 
    plot.title = element_text(size = 13)
  )
season_facet <- theme_evan_facet + theme(
  legend.position = 'none', 
  strip.background = element_blank(),
  strip.text = element_text(hjust = 0, margin = margin(0,0,0,0, unit = 'pt'))
)
park_palettes <- list(
  SmokyMountains = c("#D58A60", "#40663F", "#497381", "#548F01", "#CFA3EE", "#4E5462"),
  RockyMountains = c("#EBECF0", "#DDC70F", "#4B4E55", "#62589F", "#2B313F"),
  Yellowstone = c("#8CBEB1", "#FAFAF2","#EEEAA0", "#999275", "#E8C533", "#3A5836"),
  Arches = c("#A8CDEC", "#F6955E", "#682C37", "#9B6981", "#7887A4", "#A89F8E"),
  ArcticGates = c("#F4E7C5", "#678096", "#ACC2CF", "#979461", "#CD5733", "#A12A19"),
  MtMckinley = c("#D5AE63", "#6E6C81", "#F7ECD8", "#3F3939", "#93AD90", "#C9B793"),
  GeneralGrant = c("#FBE697", "#F3AE6D", "#516888", "#C9DACA", "#14232A", "#557780", "#1F304A", "#802729"),
  Hawaii = c("#D67B44", "#34273B", "#D95B42", "#150718", "#F2E191"),
  CraterLake = c("#7DCCD3", "#4E7147", "#BE9C9D", "#F7ECD8", "#376597", "#9888A5", "#DBA662"),
  Saguaro = c("#847CA3", "#E45A5A", "#F4A65E", "#80792B", "#F2D56F", "#1A1237"),
  GrandTeton = c("#F0EEE2", "#5B6C88", "#48594E", "#A8D0CF", "#BABBB1"),
  BryceCanyon = c("#E39B38", "#C7D8C8", "#B6BDCC", "#BFC4C5", "#9B593F"),
  MtRainier = c("#466D53", "#83CDC0", "#D3A3A1", "#A79CA5", "#FBEAD6"),
  Badlands = c("#5495CF", "#F5AF4D", "#DB4743", "#7C873E", "#FEF4D5"),
  Redwoods = c("#769370", "#BDB2A7", "#F1C646", "#6E687E", "#F17236"),
  Everglades = c("#91D5DE", "#2E8289", "#B4674E", "#EAAE37", "#565F41"),
  Voyageurs = c("#8FC0CE", "#F6F18F", "#FDFCDE", "#238451", "#359F8B"),
  BlueRidgePkwy = c("#EC8FA3", "#FCBA65", "#FAECCF", "#8D7F99", "#8C9D57", "#163343"),
  Denali = c("#73979D", "#DADCD7", "#43200E", "#E16509", "#747669"),
  GreatBasin = c("#6BBAE5", "#E3EEF4", "#454B68", "#F9F5EA", "#81974C", "#553F31"),
  ChannelIslands = c("#F5D2E6", "#554C6C", "#EB8D43", "#70646E", "#7397CB", "#CEA347"),
  Yosemite = c("#9FC2B2", "#DFDED3", "#A49A69", "#3F5B66", "#869144"),
  Acadia = c("#FED789", "#023743", "#72874E", "#476F84", "#A4BED5", "#453947"),
  DeathValley = c("#B23539", "#FAB57C", "#F7E790", "#73652D", "#E79498", "#514289"),
  Zion = c("#469BEC", "#C9FAFF", "#F1E3B6", "#C4878C", "#6D882B")
)
# Converts log10 axis values to format 10^x
fancy_scientific <- function(l) { 
  # turn in to character string in scientific notation 
  l <- log10(l)
  # return(parse(text=paste("'Discharge [m'", "^3* s", "^-1 ", "*']'", sep="")))
  return(parse(text = paste("10^",as.character(l),sep = "")))
} 

#### SET DIRECTORIES ####
# Set root directory
wd_root <- '/Users/evandethier/ide-lakes'

# Imports folder (store all import files here)
wd_imports <- paste0(wd_root,"/ide-imports/")
# Exports folder (save all figures, tables here)
wd_exports <- paste0(wd_root,"/ide-exports/")

wd_figures <- paste0(wd_exports, "ide-figures/")
wd_station_standalone <- paste0(wd_exports, "ide-station-vs-standalone-models/")
wd_standalone_models <- paste0(wd_exports, "ide-standalone-models/")
wd_standalone_figures <- paste0(wd_standalone_models, "ide-standalone-figures/")

# Create folders within root directory to organize outputs if those folders do not exist
export_folder_paths <- c(wd_imports, wd_exports, wd_figures,wd_station_standalone, 
                         wd_standalone_models, wd_standalone_figures)
for(i in 1:length(export_folder_paths)){
  path_sel <- export_folder_paths[i]
  if(!dir.exists(path_sel)){
    dir.create(path_sel)}
}


#### IMPORT DATA AND DEFINE COLUMNS ####
setwd(wd_imports)
# Import in situ data
ls_insitu_raw <- fread(file = 'ide_lakes_secchi.csv')
# Import sites that aren't close to lake edges
cleaned_sites <- fread(file = 'DataforEvan7.7_w_sites-removed.csv')

# Select only in situ data from sites that aren't close to lake edges
ls_insitu_raw <- ls_insitu_raw[loc_rowid %in% cleaned_sites$site_no]

# Import and merge all individual lake files
ls_all <- rbindlist(lapply(list.files()[grepl('ide_lakes_training', list.files())], fread), fill = T, use.names = T)[
  loc_rowid %in% cleaned_sites$site_no]

# Clean raw landsat data
ls_clean <- na.omit(ls_all[,':='(
                                       # Rename columns for simplicity
                                       B1 = B1_median,
                                       B2 = B2_median,
                                       B3 = B3_median,
                                       B4 = B4_median,
                                       B5 = B5_median,
                                       B6 = B6_median,
                                       B7 = B7_median,
                                       nd52 = nd_median,
                                       num_pix = B2_count,
                                       sample_dt = ymd(date),
                                       landsat_dt = ymd(date)
)], cols = c('B1','B2','B3','B4','B5','B7'))[
  B1 > 0 & B2 > 0 & B3 > 0 & B4 > 0 & B5 > 0 & B7 > 0][
    ,':='( 
      # add new columns with band ratios
      site_no = loc_rowid,
      station_nm = dissPermID,
      B2.B1 = B2/B1,
      B3.B1 = B3/B1,
      B4.B1 = B4/B1,
      B5.B1 = B5/B1,
      B7.B1 = B7/B1,
      B3.B2 = B3/B2,
      B4.B2 = B4/B2,
      B5.B2 = B5/B2,
      B7.B2 = B7/B2,
      B4.B3 = B4/B3,
      B5.B3 = B5/B3,
      B7.B3 = B7/B3,
      B5.B4 = B5/B4,
      B7.B4 = B7/B4,
      B7.B5 = B7/B5,
      Latitude = lat,
      Longitude = lon,
      sensor = ifelse(grepl('LT',`system:index`),'Landsat 5','Landsat 7'))][ 
        # select only columns of interest
        ,.(station_nm, sensor, site_no, Latitude,Longitude,sample_dt, num_pix, landsat_dt,
           loc_rowid, dissPermID, included_RF,
           B1,B2,B3,B4,B5,B6,B7,B2.B1,B3.B1,B4.B1,B5.B1,B7.B1,B3.B2,B4.B2,B5.B2,
           B7.B2,B4.B3,B5.B3,B7.B3,B5.B4,B7.B4,B7.B5,nd52,cloud_cover,cloud_qa_count,
           solar_az, solar_zen,sr_atmos_opacity_median,sr_cloud_qa_median
        )]

#### ADD CLUSTER TO EACH STATION WITH 1-7 CLUSTERS ####
# Uses data.table package to structure data
# Landsat data is called ls_clean
# In situ data is called ls_insitu_raw
# Bands are named B1:B7;
# Band ratios are band names separated by a period
set.seed(1)
# Get all the band medians per site
site_band_quantiles_all <- ls_clean[
  # n_insitu_samples_bySite][N_insitu_samples > 12
  ,.(N_samples = .N,
     B1 = median(B1),
     B2 = median(B2),
     B3 = median(B3),
     B4 = median(B4),
     # B5 = median(B5),
     # B7 = median(B7),
     B2.B1 = median(B2.B1),
     B3.B1 = median(B3.B1),
     B4.B1 = median(B4.B1),
     B3.B2 = median(B3.B2),
     B4.B2 = median(B4.B2),
     B4.B3 = median(B4.B3),
     B4.B3.B1 = median(B4.B3/B1), 
     Latitude = median(Latitude),
     Longitude = median(Longitude)),
  by = .(station_nm,site_no)]

# Select only the visible and near-ir bands and band ratios
vis_nir_bands <- c('B1','B2','B3','B4','B2.B1','B3.B1','B4.B1','B3.B2','B4.B2','B4.B3','B4.B3.B1')

# Scale each band and band ratio for k-means (this also saves the scaling parameters for each band)
site_band_scaling_all <- scale(site_band_quantiles_all[,..vis_nir_bands])

# Get K-means clustering for each possible band and band combination
# Make a list of every possible band and band combination to be used in k-means
cluster_var_combinations <- Map(as.data.frame, sapply(seq_along(vis_nir_bands), function(k) t(combn(vis_nir_bands,k))))
for(i in 4:length(vis_nir_bands)){ # loop through between 4 and 12 bands/band combination inputs
  cluster_var_k_sel <- cluster_var_combinations[[i]] 
  for(k in 1:nrow(cluster_var_k_sel)){ # loop through all possible band combinations
    print(paste0(i, " ", k)) # print each selected combination 
    # Convert selected band median columns to matrix for k-means clustering
    cluster_var_sel <- c(as.matrix(cluster_var_k_sel[k,])) 
    
    # Generate cluster label
    cluster_var_label <- paste(cluster_var_sel, collapse = "_")
    # For each number of clusters between 4 and 10, get k-means clusters
    # Also calculate cubic clustering criterion (CCC) (measure of within cluster variance)
    ccc_result <- data.table(cbind(cluster_var_label, i, c(4:10), NbClust(site_band_scaling_all[,cluster_var_sel],
                                                                          min.nc=4, max.nc=10, index="ccc", method="kmeans")$All.index))
    
    # Save which variables were used, the number of variables, the number of clusters, and the CCC
    colnames(ccc_result) <- c('variables','nvars','nclusters','ccc')
    if(k == 1 & i == 4){
      ccc_master <- ccc_result
    }else{
      ccc_master <- rbind(ccc_master, ccc_result)
    }
    if(k%%100 == 0){
      print(ccc_result) # print result every 100 tries just to see it's working
    }
  }}

# data.table of CCC for all possible combinations
ccc_analysis <- ccc_master[,':='(nvars = as.numeric(nvars),
                                 nclusters = as.numeric(nclusters),
                                 ccc = as.numeric(ccc))]

# Select optimal clustering variables; best combinations use five or six variables (see ccc plot)
ccc_best <- ccc_analysis[nclusters > 2 & nvars < 6][, .(mean_ccc = mean(ccc, na.rm = T)), by = variables][order(-mean_ccc)]

# Plot CCC for each variable combination (many combinations!!)
ccc_plot <- ggplot(ccc_analysis, aes(x = factor(nclusters), y = ccc, color = factor(nvars))) + 
  geom_boxplot() +
  # geom_point() + 
  season_facet + 
  theme(legend.position = 'right') + 
  labs(
    x = 'Number of clusters',
    y = 'Cubic clustering criterion',
    color = 'Number of\nvariables'
  )

# Optional save plot. Need to update directory (wd_exports) if you use it.
ggsave(ccc_plot, filename = paste0(wd_exports,'ccc_optimize_plot.pdf'), width = 7, height = 7)

# Calculate k-means cluster based on all regressors at all sites
# # Using raw band and band ratio values
# Select colors for plotting
cl_colors <- brewer.pal(name = 'Paired',n=12)

# Select variables to use for clustering
clustering_vars <- unlist(strsplit(as.character(ccc_best[1,'variables']),'_')) # based on optimal cluster vars from ccc analysis

# Optional write csv with band medians. Need to change directory (wd_exports) if you use it.
write_csv(site_band_quantiles_all, paste0(wd_exports,'all_sts_band_medians.csv'))

## Prepare data for cluster analysis
clusters_calculated_list <- rep(list(NA), 10)
ssc_cluster_color_plot_list <- rep(list(NA), 10)
ssc_cluster_false_color_plot_list <- rep(list(NA), 10)

for(i in c(1:10)){ # test different cluster numbers
  
  # Select number of k-means cluster groups
  n_centers <- c(1:10)[i]
  cluster_col_name <- paste0('cluster_n',n_centers)
  
  # Calculate k-means cluster based on all variables at all sites
  # # Using scaled band and band ratio values
  site_band_scaling <- scale(site_band_quantiles_all[,..clustering_vars])
  clusters_calculated <- kmeans(site_band_scaling, centers = n_centers,
                                nstart = 20, iter.max = 50)
  
  # Save cluster centers, etc. to master list
  clusters_calculated_list[[i]] <- clusters_calculated
  
  # Compute cluster centers
  cluster_centers <- clusters_calculated$centers
  
  # Assign cluster to each site in the data.table
  site_band_quantiles_all$cluster <- clusters_calculated$cluster
  
  fwrite(site_band_quantiles_all, paste0(wd_exports, 'site_band_median_cl', n_centers,'.csv'))
  # select just the site number and cluster columns
  clustered_sites <- site_band_quantiles_all[,.(site_no,cluster)]
  
  # TYPICAL WATERBODY COLOR AT DIFFERENT CLUSTERS
  # This could be adapted from SSC for other variables: Secchi disk depth, chl-a, etc.
  
  # Select SSC categories for plotting
  # ssc_categories <- c(0,25,50,100,250,500,750,1000,1500, 1e6)
  # Replace "ssc_categories" with "secchi_categories", "ssc_category" with "secchi_category", 
  # ssc_category_labels" with "secchi_category_labels", "log10_SSC_mgL" with "secchi_depth_m"
  secchi_categories <- c(0,1,2,3,4,5,100) 
  # ssc_categories <- c(0,50,100,200,500,1e6)
  # ssc_categories <- c(0,10,25,50,75,100,150,200,250,300,350, 400, 450, 500,600, 700, 800,900,1000,1100,1500, 1e6)
  
  # Generate SSC labels as 'low value' - 'high value'
  secchi_category_labels <- paste0(secchi_categories[-length(secchi_categories)],'-',c(secchi_categories[-1]))
  # Make highest SSC category "> highest value"
  secchi_category_labels[length(secchi_category_labels)] <- paste0('> ', secchi_categories[length(secchi_category_labels)])
  
  ## Add cluster group as column to ls-insitu matched data.table
  # setkey(ls_insitu_raw,dissPermID) # for Lake matching
  setkey(ls_insitu_raw,loc_rowid) # for individual site matching
  setkey(clustered_sites,site_no)
  ls_insitu_cl <- ls_insitu_raw[clustered_sites][
    ,':='(cluster_sel = cluster,
          # # Categorize SSC value as one of selected categories
          secchi_category = cut(secchi_depth_m, 
                             breaks = secchi_categories,
                             labels = secchi_category_labels))][]
  # Select cluster for analysis
  
  # # Generate median B,G,R, near-infrared for each Secchi category and each cluster or site
  secchi_category_color <- na.omit(ls_insitu_cl[
                              # abs(lag_days) < 8
                              , 
                                     keyby = .(cluster_sel, secchi_category),
                                     lapply(.SD, median,na.rm = T),
                                     .SDcols = c('B1','B2','B3', 'B4')], cols = c('B1','B2','B3','B4'))
  
  # Create true-color and false-color plots of 'typical' river color for each SSC category at each cluster group
  for(j in 1:2){
    color_sel <- c('true_color','false_color')[j]
    raster_color_types <- c(geom_raster(aes(fill = rgb(B3/2000,B2/2000,B1/2000))), # true color
                            geom_raster(aes(fill = rgb(B4/2000,B3/2000,B2/2000))) # false color)
    )
    cluster_secchi_category_color_plot <- 
      ggplot(secchi_category_color, aes(x = cluster_sel, y = secchi_category)) +
      raster_color_types[j] +
      scale_fill_identity() +
      season_facet + 
      theme(axis.text.x = element_text(angle = 90)) + 
      labs(
        y = 'Secchi depth (m)',
        x = 'Cluster grouping'
      )
    
    ggsave(cluster_secchi_category_color_plot, filename = paste0(wd_figures, cluster_col_name,'_secchi_category_color_plot_',color_sel,'.pdf'),
           width = 3, height = 3.4, useDingbats = F)
    ggsave(cluster_secchi_category_color_plot, filename = paste0(wd_figures, cluster_col_name,'_secchi_category_color_plot_',color_sel,'.png'),
           width = 3, height = 3.4)
    if(j == 1){
      ssc_cluster_color_plot_list[[i]] <- cluster_secchi_category_color_plot
    }else{
      ssc_cluster_false_color_plot_list[[i]] <- cluster_secchi_category_color_plot
    }
  }
}

# Select 5-cluster grouping
cluster_n_best <- 5
# Import site table with clusters assigned, merge with by-site band median
cluster_groups <- fread(paste0(wd_exports,'site_band_median_cl5.csv'))[
  site_band_quantiles_all[,.(station_nm, site_no, Latitude, Longitude)], 
  on = c('station_nm','site_no')
]

#### APPLY CLUSTERS TO DIFFERENT DATA ####

# Generate function to apply clustering to different data
getCluster <- function(dt,clustering_vars,n_centers, kmeans_object){
  # Compute band median at each site for clustering variables
  site_band_quantiles_all <- dt[
    # n_insitu_samples_bySite][N_insitu_samples > 12
    ,.(N_samples = .N,
       B1 = median(B1),
       B2 = median(B2),
       B3 = median(B3),
       B4 = median(B4),
       # B5 = median(B5),
       # B7 = median(B7),
       B2.B1 = median(B2.B1),
       B3.B1 = median(B3.B1),
       B4.B1 = median(B4.B1),
       B3.B2 = median(B3.B2),
       B4.B2 = median(B4.B2),
       B4.B3 = median(B4.B3),
       B4.B3.B1 = median(B4.B3/B1)), 
    by = .(station_nm,site_no)]
  
  # Scale band medians using pre-determined scale from initial scaling of band median data
  site_band_quantile_scaled <- scale(site_band_quantiles_all[,..clustering_vars], 
                                     center = attributes(site_band_scaling)$`scaled:center`, 
                                     scale = attributes(site_band_scaling)$`scaled:scale`)
  
  # Make a function that assigns cluster to a new site, based on scaled band medians
  # K-Means works by minimizing distance to cluster center
  closest.cluster <- function(x) {
    cluster.dist <- apply(kmeans_object$centers, 1, function(y) sqrt(sum((x-y)^2)))
    return(which.min(cluster.dist)[1])
  }
  site_band_quantiles_all$cluster <- apply(site_band_quantile_scaled, 1, closest.cluster)
  
  dt_cluster <- merge(dt,site_band_quantiles_all[,c('site_no','cluster')], by = 'site_no')
  dt_cluster$cluster_sel <- dt_cluster$cluster
  # Return new data 
  return(dt_cluster)
  
}

# Apply cluster algorithm to lake data (rather than individual site data)
# Aggregate band medians for each lake from all the raw data from each sampling site at that lake,
# *not* all pixels from that lake (haven't downloaded those full-lake data).
lake_band_quantiles_all <- site_band_quantiles_all[,':='(site_no = station_nm,
                                         cluster_sel=NA)][
  # n_insitu_samples_bySite][N_insitu_samples > 12
  ,.(N_samples = .N,
     B1 = median(B1),
     B2 = median(B2),
     B3 = median(B3),
     B4 = median(B4),
     # B5 = median(B5),
     # B7 = median(B7),
     B2.B1 = median(B2.B1),
     B3.B1 = median(B3.B1),
     B4.B1 = median(B4.B1),
     B3.B2 = median(B3.B2),
     B4.B2 = median(B4.B2),
     B4.B3 = median(B4.B3),
     B4.B3.B1 = median(B4.B3/B1),
     Latitude = mean(Latitude),
     Longitude = mean(Longitude)), 
  by = .(station_nm,site_no)]

# Apply cluster algorithm to lake data
clustered_lakes <- getCluster(lake_band_quantiles_all[,':='(site_no = station_nm,
                                                            cluster_sel=NA)], 
  clustering_vars,cluster_n_best, 
  clusters_calculated_list[[cluster_n_best]])

fwrite(clustered_lakes, file = paste0(wd_exports, 'clustered_lakes_ncl5.csv'))
# Import individual site cluster data
clustered_indiv_sites <- fread(paste0(wd_exports,'site_band_median_cl5.csv'))

# Join by-site and by-lake data
clustered_lake_vs_site <- clustered_indiv_sites[,cluster_indiv:=cluster][
                  ,.(site_no, station_nm, cluster_indiv)][
                    clustered_lakes, on = 'station_nm']

# Compare by-site and by-lake clustering to check for site-lake agreement
cluster_lake_site_correspond <- clustered_lake_vs_site[,
                                 ':='(N_sites = .N), by = station_nm][
                                   N_sites > 1][,
                                 ':='(correspond_specific = ifelse(cluster_indiv == cluster, 'Agree','Differ'),
                                      correspond_general = ifelse(cluster_indiv %in% c(3,4) & cluster %in% c(3,4), 
                                                               'eutr. same', 
                                                        ifelse(cluster_indiv %in% c(3,4) & cluster %in% c(1,2,5), 
                                                               'eutr. differ',
                                      ifelse(cluster_indiv %in% c(1,2,5) & cluster %in% c(1,2,5),
                                                               'het. same', 'het. differ'))))][,
                                      ':='(clusters = ifelse(grepl('eutr', correspond_general), 'Clusters 3, 4', 'Clusters 1, 2, 5'),
                                           same_differ = ifelse(grepl('same', correspond_general), 'Agree','Differ'))]

# Summarize cluster agreement (self-similar clusters)
cluster_lake_site_correspond_summary <- cluster_lake_site_correspond[,.(N_sites = .N), by = .(same_differ, clusters)]
# Summarize cluster agreement by each cluster
cluster_lake_site_specific_correspond_summary <- cluster_lake_site_correspond[,.(N_sites = .N), 
                                                          by = .(correspond_specific, cluster)]
# Which sites don't agree with the whole-lake assignemnt different?
eutr_differ <- cluster_lake_site_correspond[correspond_general == 'eutr. differ']

# Make barplot of site-lake correspondence (general predictable vs. non-predictable clusters)
cluster_lake_general_correspond_plot <- ggplot(cluster_lake_site_correspond, aes(x = same_differ, fill = clusters)) + 
  facet_wrap(.~clusters, scales = 'free_x') +
  geom_text(data = cluster_lake_site_correspond_summary, aes(label = paste0('N = ', N_sites), 
                                                             x = same_differ, y = N_sites + 20)) +
  season_facet +
  scale_fill_manual(values = c('#025159','#F28705')) +
  geom_bar() +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  labs(
    x = 'Site-Lake correspondence',
    y = 'N Sites'
  )

# Make barplot of site-lake correspondence (for each cluster)
cluster_lake_specific_correspond_plot <- ggplot(cluster_lake_site_correspond, aes(x = correspond_specific, fill = factor(cluster))) + 
  facet_wrap(.~cluster, scales = 'free_x') +
  geom_text(data = cluster_lake_site_specific_correspond_summary, aes(label = paste0('N = ', N_sites), 
                                                             x = correspond_specific, y = N_sites + 20)) +
  season_facet +
  scale_fill_manual(values = c('#00A1F5','#03A696','#F28705','#F25D27','#025159')) +
  geom_bar() +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  labs(
    x = 'Site-Lake correspondence',
    y = 'N Sites'
  )
 
# Save cluster correspondence maps
ggsave(cluster_lake_general_correspond_plot, filename = paste0(wd_figures, 'cluster_lake_general_correspond_plot.pdf'),
       useDingbats = F, width = 5, height = 5) 

ggsave(cluster_lake_specific_correspond_plot, filename = paste0(wd_figures, 'cluster_lake_specific_correspond_plot.pdf'),
       useDingbats = F, width = 5, height = 5) 

# Import map data for New England states
new_england_states <- map_data('state', region = c('maine','new york','massachusetts','vermont', 'new hampshire', 'connecticut','rhode island'))
# Create map of sites colored by cluster
cluster_map <- ggplot(cluster_groups[order(factor(cluster, levels = c(1,2,5,3,4)))], 
                      aes(x = Longitude, y = Latitude)) +
  geom_path(data = new_england_states, aes(x = long, y = lat, group = group), lwd = 0.25) +
  geom_point(aes(fill = factor(cluster), shape = factor(cluster)), size = 2.5, color = 'black', stroke = 0.25) + 
  scale_fill_manual(values = c('#00A1F5','#03A696','#F28705','#F25D27','#025159')) +
  scale_shape_manual(values = c(21:25)) +
  # guides(fill = guide_legend())
  season_facet + 
  theme(legend.position = c(0.2, 0.8)) +
  labs(
    fill = 'Cluster group',
    shape = 'Cluster group'
  )
# Save cluster map
ggsave(cluster_map, filename = paste0(wd_figures, 'cluster_map_ncl5.pdf'), width = 7, height = 6, useDingbats = F)

# Create map of sites colored by whole-lake cluster
cluster_lakes_map <- ggplot(clustered_lakes[order(factor(cluster, levels = c(1,2,5,3,4)))], 
                            aes(x = Longitude, y = Latitude)) +
  geom_path(data = new_england_states, aes(x = long, y = lat, group = group), lwd = 0.25) +
  geom_point(aes(fill = factor(cluster), shape = factor(cluster)), size = 2.5, color = 'black', stroke = 0.25) + 
  scale_fill_manual(values = c('#00A1F5','#03A696','#F28705','#F25D27','#025159')) +
  scale_shape_manual(values = c(21:25)) +
  # guides(fill = guide_legend())
  season_facet + 
  theme(legend.position = c(0.2, 0.8)) +
  labs(
    fill = 'Cluster group',
    shape = 'Cluster group'
  )

# Save cluster by-lake map
ggsave(cluster_lakes_map, filename = paste0(wd_figures, 'cluster_lakes_map_ncl5.pdf'), width = 7, height = 6, useDingbats = F)

# Combine by-site and by-lake maps into one figure, save it.
ggsave(ggarrange(cluster_lakes_map, cluster_map, ncol = 2, common.legend = T, labels = c('by Lake','by Site'), 
                 label.x = 0.05, label.y = 0.98), 
       filename = paste0(wd_figures,'cluster_lakes_combined_map_ncl5.pdf'), useDingbats = F, width = 11, height = 6)
