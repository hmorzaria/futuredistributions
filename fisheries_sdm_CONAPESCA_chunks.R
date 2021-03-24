#' Functions to generate SDMs for catch species
#' @author Hem Nalini Morzaria Luna
#' @date July 2019
#' Uses species from catch data 2014-2106

make_map <- function(shape.file, file.name,scale.factor, bar.position, min.long, max.long, min.lat,max.lat) {
  
  model.shape <- readOGR(shape.file)
  
  # Reformat shape for mapping purposes
  model.shape.df <- broom::tidy(model.shape)
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  class(world)
  
  world_points<- st_centroid(world)
  world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))
  
  model.map <- ggplot(data = world) +
    geom_sf() +
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                           style = north_arrow_fancy_orienteering) +
    #geom_text(data= world_points,aes(x=X, y=Y, label=name),
    #          color = "darkgrey", check_overlap = FALSE) +
    coord_sf(xlim = c(min.long, max.long), ylim = c(min.lat, max.lat), expand = FALSE)+
    geom_path(data = model.shape.df, aes(x = long, y = lat, group=group),
              colour = "lightgrey") +
    annotation_scale(location = "tl", width_hint = 0.2) +
    xlab("Lon")+
    ylab("Lat")+
    theme_bw()+
    annotate(geom="text", x=-102, y=24, label="Mexico",
             color="black")
  
  ggsave(file.name, model.map, width = 16,height = 12, units="cm", dpi = 400)
  
  return(model.map)
  
}

get_range <- function(thissamplearea){
  
  environpath <- paste("~/conapescaspecies/rasters/",thissamplearea,sep="")
  
  ## Load predictor rasters
  # make raster "stack" with raster for each predictor
  
  variables <- c("alk","clo","fos","nit","oxi","ph","pp","sal","sil","sst","sst_modis")
   
  substract_raster <- function(thisvariable) {
    
    print(thisvariable)
   
    raster.1 <- list.files(environpath, full.names=T, paste(thisvariable,"_max.asc",sep="")) %>% 
      raster
    
    raster.2 <- list.files(environpath, full.names=T, paste(thisvariable,"_min.asc",sep="")) %>% 
      raster 
    
    projection(raster.1) <-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84
    projection(raster.2) <-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84
    
    range.raster <- overlay(raster.1,
                          raster.2,
                          fun=function(r1, r2){return(r1-r2)})
    
    plot(range.raster)
    setwd(environpath)
    writeRaster(range.raster,paste(thisvariable,"_range.asc",sep=""),overwrite=TRUE)
  }

  lapply(variables,substract_raster)

}


colinear_test <- function(thissamplearea, parameter.names, list.rasters, raster.name) {
  
  raster.stack <- raster::stack(list.rasters)
  
  projection(raster.stack) <-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84
  
  var.names <- read_csv(parameter.names)
  
  collin.rast <- removeCollinearity(raster.stack, multicollinearity.cutoff = 0.75,
                                    select.variables = TRUE, sample.points = FALSE, plot = FALSE)
  
  rasters.selected <- subset(raster.stack, collin.rast)
  
  write.csv(names(rasters.selected), paste(thissamplearea,"_",raster.name,"_rasters_selected.csv",sep=""))
  
  #get correlation matrix for environmental layers from ENMtools
  #cor.matrix <- raster.cor.matrix(rasters.env)
  
  #plot environmental variables
  #cor.plot <- raster.cor.plot(rasters.env)
  
  #correlation plot
  #mds.plot <- cor.plot$cor.mds.plot
  
  #heatmap
  #cor.heatmap <- cor.plot$cor.heatmap
  
  #save correlation and heatmap 
  #ggsave(paste(thissamplearea,"correlation_plot.png",sep="_"), mds.plot, width = 10, height = 10, dpi = 150, device='png')
  
  #ggsave(paste(thissamplearea,"heatmap_plot.png",sep="_"), cor.heatmap, width = 10, height = 10, dpi = 150, device='png')
  
  raster.list <- list()
  
  for(eachraster in 1:length(names(rasters.selected))){
    
    this.raster <- rasters.selected[[eachraster]]
    
    this.raster.df <- as.data.frame(this.raster, xy = TRUE)
    
    this.variable <- names(rasters.selected)[eachraster] %>% ggplot2::sym()
    
    if(parameter.names=="observed_layers.csv") {
      
    if(grepl("ph",this.variable)){
      
      this.name <- var.names %>% 
        filter(var_name==this.variable) %>% 
        {
          .$stat_name %>% 
            capitalize() %>% 
          paste("pH", .) 
        }
    } else {

        this.line <- var.names %>% 
          filter(var_name==this.variable)
            
        this.name <- paste(this.line$variable, this.line$stat_name) %>% toTitleCase
      
    }
      } else if(parameter.names=="biooracle_layers.csv")  {
      
      layer.code <- this.variable %>% 
        str_split("_") %>% 
        unlist 
      
      if(length(layer.code)==5){
        
        layer.full.code <- paste(layer.code[1],layer.code[2],layer.code[3],sep="_")
      }
      
      if(length(layer.code)==4){
        
        layer.full.code <- paste(layer.code[1],layer.code[2],sep="_")
      }
      
      
      this.name <- var.names %>% 
        filter(layer_code==layer.full.code) %>%
        pull(name)
      
    }
    
    print(this.variable)
    print(this.name)
    
    ggplot.raster <-  ggplot() +
      geom_raster(data = this.raster.df , aes(x = x, y = y, fill =!!this.variable)) + 
      coord_quickmap()+
      ggtitle(this.name) +
      xlab("Lon") + ylab("Lat")+
      labs(fill = "")
    
    raster.list[[eachraster]] <- ggplot.raster
  }

  
  raster.grob <- marrangeGrob(grobs = raster.list, nrow=4, ncol = 3, top="")
  
  OutFileName <- paste(thissamplearea,raster.name,"raster_plot.pdf",sep="_")
  
  ggsave(file = OutFileName, raster.grob, width = 16, height = 12, dpi = 150, device='pdf')  ## save plot
  
}

cut_raster <- function(analysis.layer,reference.raster,crs.geo.wgs,study.area) {
    
    analysis.raster <- raster(analysis.layer)
    
    analysis.cropped <-crop(analysis.raster,reference.raster)
    
    plot(analysis.cropped)
    plot(reference.raster,add=TRUE)
    
  # introduce na in rst1 for all locations that are non-na in rst2
    analysis.region <- overlay(analysis.cropped, reference.raster, fun = function(x, y) {
      x[is.na(y[])] <- NA
      return(x)
    })
    
    analysis.resam <- raster::resample(analysis.region,reference.raster,method='bilinear')
    plot(analysis.resam)
    
    new.name <- analysis.layer %>% 
      str_split("[.]") %>% 
      unlist %>% 
      .[1] %>% 
      str_split("/") %>% 
      unlist %>% 
      .[6] %>%
      paste(save_dir,"/",.,"_",study.area,".asc",sep="")
    
    analysis.resam
    
   writeRaster(analysis.resam,file.path(new.name),format="ascii",overwrite=TRUE)
   
   return(new.name)
  }



  



  project_future <- function(raster.stack){
    
    SDM_projection <- ssdm::project(esdm.model,ipcc.stack)
    
  }
  
  file_names <- list.files(path="/home/atlantis/conapescaspecies/occurrence_maps",pattern = "atlantic _*.*png")
  
  rename_files <- function(thisfile) {
  
    sp.name <- thisfile %>% 
      str_split("_") %>% 
      unlist %>% 
      .[2]
    
    new.name <- sp.name %>% 
      gsub(" ","_", .) %>% 
      paste(.,"_map.png", sep="")
    
    file.rename(paste("/home/atlantis/conapescaspecies/occurrence_maps/",thisfile,sep=""),
                paste("/home/atlantis/conapescaspecies/new_occurrence_maps/",new.name,sep=""))
    
  }
  
  lapply(file_names, rename_files)



get_smd <- function(this.species, this.area){
  
  #use mget() to search for objects given a character vector.
 
  
  sp.folder <- this.species %>% 
    str_split("full_log_file.txt") %>% 
    unlist %>% 
    .[1] %>% 
    gsub("_thinned","",.)
  
  sp.folder.path <- sp.folder %>% 
    gsub(" ","_",.)

  sp.thin.list.files <- list.files(path=paste("~/conapescaspecies/thindata/",sp.folder, "full",sep=""), pattern = "*.*csv$")
  
  max.file <- regmatches(sp.thin.list.files, regexpr( "\\d+", sp.thin.list.files)) %>% 
    max()
  
  thin.file <- sp.folder %>% 
    gsub("_","",.) %>% 
    paste(.,"_thinned_thin",max.file,".csv", sep="")
  
  dir.create(paste("~/conapescaspecies/smd/",sp.folder.path,"smd",sep=""))
  
  file.copy(from=paste("~/conapescaspecies/thindata/",sp.folder,"full/",thin.file,sep=""),to=paste("~/conapescaspecies/smd/",sp.folder.path,"smd/",paste(sp.folder.path,"joint.csv",sep=""),sep=""),overwrite=TRUE)
  
  sp.points <- paste("~/conapescaspecies/smd/",sp.folder.path,"smd/",paste(sp.folder.path,"joint.csv",sep=""),sep="") %>% read_csv()
  sp.points.area <- sp.points %>% 
    filter(lat <= 40 & lat >= -10) %>% 
    filter(lon <= -50 & lon >= -130) %>% 
    dplyr::select(lon,lat)
  
  sp.points.area <- sp.points %>% 
    filter(lat <= 40 & lat >= -10) %>% 
    filter(lon <= -50 & lon >= -130) %>% 
    dplyr::select(lat,lon)
  
  p <- SpatialPoints(coords = sp.points.area,
                             proj4string = crs(biooracle.stack))
  
  coordinates(sp.points.area) <- ~lon+lat
  projection(sp.points.area) <-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # 
  
  file = paste(sp.folder.path,"joint.csv",sep="")
  path = paste("~/conapescaspecies/smd/",sp.folder.path,"smd",sep="")
  file <- paste0(path, "/", file)
  
  Occurrences <- read.csv2(file = file, ...)  # Occ = occurrences
  
  Occurrences[, which(names(Occurrences) == Spcol)] <- as.factor(Occurrences[, which(names(Occurrences) == Spcol)])
  
  Occurrences$validity <- raster::extract(biooracle.stack[[1]], Occurrences[, c(which(names(Occurrences) == Xcol), which(names(Occurrences) == Ycol))])
  
  Occurrences$validity <- raster::extract(biooracle.stack[[1]], sp.points.area[, c(which(names(sp.points.area) == Xcol), which(names(sp.points.area) == Ycol))])
  
  Occ <- SSDM::load_occ(path = paste("~/conapescaspecies/smd/",sp.folder.path,"smd",sep=""), biooracle.stack,
                  Xcol = 'lon', Ycol = 'lat', Spcol = 'species',
                  file = paste(sp.folder.path,"joint.csv",sep=""), sep = ',', verbose = FALSE)
  
  Occ <- SSDM::load_occ(path = paste("~/conapescaspecies/smd/",sp.folder.path,"smd",sep=""), biooracle.stack,
                        Xcol = 'lon', Ycol = 'lat', Spcol = 'species',
                        file = paste(sp.folder.path,"joint.csv",sep="", GeoRes = TRUE, reso = 10), sep = ',', verbose = FALSE)
  
  
  
  SDM <- modelling('GLM', subset(Occ, Occ$species == 'zapteryx exasperata'), 
                   biooracle.stack,Xcol = 'lon', Ycol = 'lat', verbose = TRUE)
  
  plot(SDM@projection, main = 'SDM\nfor zapteryx exasperata\nwith GLM algorithm')
  
  #Ensemble species distribution models (ESDMs)
  
  ESDM <- ensemble_modelling(c('CTA', 'MARS'), subset(Occ, Occ$species == 'zapteryx exasperata'),
                             biooracle.stack, Xcol = 'lon', Ycol = 'lat',
                             ensemble.thresh = 0, verbose = TRUE)
  
  plot(ESDM@projection, main = 'ESDM\nfor Cryptocarya elliptica\nwith CTA and MARS algorithms')
  
  
  fold <- kfold(sp.points, k=5) # add an index that makes five random groups of observations
  spoints.test <- sp.points[fold == 1, ] # hold out one fifth as test data
  spoints.train <- sp.points[fold != 1, ] # the other four fifths are training data
  
  write_csv(spoints.train,paste("~/conapescaspecies/smd/",sp.folder.path,"smd/",paste(sp.folder.path,"train.csv",sep=""),sep=""))
  write_csv(spoints.test, paste("~/conapescaspecies/smd/",sp.folder.path,"smd/",paste(sp.folder.path,"test.csv",sep=""),sep=""))
  
  setwd("~/conapescaspecies/smd/")
  
  
  
    # Variables with information to be used as arguments. Change "YOUR/DIRECTORY" by your actual directory.
  occ_joint <- paste(sp.folder.path,"joint.csv",sep="")
  occ_tra <- paste(sp.folder.path,"train.csv",sep="")
  M_var_dir <- paste("M_variable",this.area,sep="_")
  batch_cal <- paste("Candidate_models",sp.folder.path,sep="_")
  out_dir <- paste("Candidate_models",sp.folder.path,sep="_")
  #reg_mult <- c(seq(0.1, 1, 0.1), seq(2, 6, 1), 8, 10)
  reg_mult <- c(2)
  
  f_clas <- "all"
  args <- NULL # e.g., "maximumbackground=20000" for increasing the number of pixels in the bacground or
  # note that some arguments are fixed in the function and should not be changed
  maxent_path <- "./maxent"
  wait <- FALSE
  run <- TRUE
  
  kuenm_cal(occ.joint = occ_joint, occ.tra = occ_tra, M.var.dir = M_var_dir, batch = batch_cal,
            out.dir = out_dir, reg.mult = reg_mult, f.clas = f_clas, args = args,
            maxent.path = maxent_path, wait = wait, run = run)
  
  system(paste("sudo chmod + ",batch_cal,".sh",sep=""), wait = TRUE)
  system(paste("sudo sh ./",batch_cal,".sh",sep=""), wait = TRUE)
  
  
  ###
  
  raster.sel.list <- fread("pacific_rasters_selected.csv") %>% 
    filter(!is.na(V1)) %>%
    pull(V2) %>% 
    paste(.,".asc",sep="")
  
  setwd("~/conapescaspecies/rasters/pacific")
  
  rasters.selected <- stack(raster.sel.list)
  
#  rasters.selected <- stack(raster.list)
  
  projection(rasters.selected) <-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84
  
#  lapply(raster.sel.list,function (x) file.copy(from=paste0("~/conapescaspecies/rasters/pacific/",x),to=paste0("~/conapescaspecies/smd/M_variable_pacific/Set 1/",x)))
  
  setwd(thinpath)
  setwd(sp.dirs[eachdir])
  sp.file <- list.files() %>% grepl("*5.csv",.)
  last.file <- list.files() %>% .[sp.file]
  spoints <- fread(last.file) %>% dplyr::select(lon,lat) %>% rename(Longitude="lon",Latitude="lat")
  this.species <- fread(last.file) %>% distinct(species) %>% .$species %>% as.character()
  spoints %>% write_csv("all_points.csv")
  
  
  fold <- kfold(spoints, k=5) # add an index that makes five random groups of observations
  spointstest <- spoints[fold == 1, ] # hold out one fifth as test data
  spointstrain <- spoints[fold != 1, ] # the other four fifths are training data
  
  write_csv(spointstrain, "points_train.csv")
  write_csv(spointstest, "points_test.csv")
  
  occ_joint <- "all_points.csv"
  occ_tra <- "points_train.csv"
  M_var_dir <- "/M_variable"
  batch_cal <- "Candidate_models"
  out_dir <- "Candidate_mdels"
  reg_mult <- c(seq(0.1, 1, 0.1), seq(2, 6, 1), 8, 10)
  f_clas <- "all"
  args <- NULL
  maxent_path <- "~/maxent"
  wait <- TRUE
  run <- TRUE
  
  kuenm_cal(occ.joint = occ_joint, occ.tra = occ_tra, M.var.dir = M_var_dir, batch = batch_cal,
            out.dir = out_dir, reg.mult = reg_mult, f.clas = f_clas, args = args,
            maxent.path = maxent_path, wait = wait, run = run)
  
  
  occ_test <- "points_test.csv"
  out_eval <- "Calibration_results"
  threshold <- 5
  rand_percent <- 50
  iterations <- 100
  kept <- TRUE
  selection <- "OR_AICc"
  paral_proc <- TRUE # make this true to perform pROC calculations in parallel, recommended
  # only if a powerfull computer is used (see function's help)
  
  # Note, some of the variables used here as arguments were already created for previous function
  cal_eval <- kuenm_ceval(path = out_dir, occ.joint = occ_joint, occ.tra = occ_tra, occ.test = occ_test, batch = batch_cal,
                          out.eval = out_eval, threshold = threshold, rand.percent = rand_percent, iterations = iterations,
                          kept = kept, selection = selection, parallel.proc = paral_proc)
  
  setwd(thinpath)
  
  sp.dirs <- list.dirs(path=".",recursive=FALSE)
  eachdir <- 1
  
  auc.values <- list()
  
  for(eachdir in 1:length(sp.dirs)){
    
    #Continue here adding ENMTools object
    # see https://github.com/danlwarren/ENMTools
    setwd(thinpath)
    setwd(sp.dirs[eachdir])
    sp.file <- list.files() %>% grepl("*5.csv",.)
    last.file <- list.files() %>% .[sp.file]
    spoints <- fread(last.file) %>% dplyr::select(lon,lat) %>% rename(Longitude="lon",Latitude="lat")
    this.species <- fread(last.file) %>% distinct(species) %>% .$species %>% as.character()
    spoints %>% write_csv("all_points.csv")
    
    fold <- kfold(spoints, k=5) # add an index that makes five random groups of observations
    spointstest <- spoints[fold == 1, ] # hold out one fifth as test data
    spointstrain <- spoints[fold != 1, ] # the other four fifths are training data
    
    write.csv(spoints,file=paste(this.species,"_obspoints.csv",sep=""))
    

    if(nrow(spoints)>20){
      
      
      this.sp.data <- enmtools.species()
      
      #we will withold 20% of our data as test data to test the accuracy of the model later
      #using the other 80% of the data to make the model
      
      fold <- kfold(spoints, k=5) # add an index that makes five random groups of observations
      spointstest <- spoints[fold == 1, ] # hold out one fifth as test data
      spointstrain <- spoints[fold != 1, ] # the other four fifths are training data
      
      background.points <- background.points.buffer(points = spoints, radius = 90000, n = 1000, mask = rasters.selected[[1]])
      background.range <- background.raster.buffer(spoints, 50000, mask = rasters.selected[[1]])
      
      this.sp.data$species.name <- this.species
      
      this.sp.data$presence.points <- spoints
      
      this.sp.data$range <- background.range
      
      this.sp.data$background.points <- background.points
      
      monticola.mx <- enmtools.maxent(this.sp.data, rasters.selected, test.prop = 0.2)
      
      
      enmtools.maxent(this.sp.data, rasters.selected, test.prop = 0.2, nback = 1000, env.nback = 10000, report = NULL, overwrite = FALSE, rts.reps = 0,  bg.source = "default")
        
      all.points <- bind_rows(back.points,spointstrain) 
      
      all.presence <- c(rep(0,nrow(back.points)),rep(1,nrow(spointstrain))) %>% 
        as.data.frame() %>% setNames(c("presence"))
      
      
      ## Extracting values from rasters
      presvals <- raster::extract(rasters.selected, all.points) %>% as.data.frame()
      
      sdm.model <-  maxent(presvals, all.presence)
      
      sdm.pred <- dismo::predict(sdm.model, rasters.selected)
      
      plot(sdm.pred, main="Predicted Suitability")
      map('worldHires', fill=FALSE, add=TRUE)
      points(spoints$lon, spoints$lat, pch="+", cex=0.5, col="black")
      
      e1 <- evaluate(sdm.model, p=spointstest, a=back.points, x=rasters.selected)
      
      plot(e1, 'ROC')
      
      auc.values[[eachdir]] <- e1@auc
      
      #Projecting responses to climate change
      
      #sdm.future <- predict(sdm.model, future.rasters.selected)
      
      #plot(sdm.future, main="Predicted Suitability")
      #map('worldHires', fill=FALSE, add=TRUE)
      #points(spointstrain$lon, spointstrain$lat, pch="+", cex=0.5, col="black")
      
      
    }
  
}

}


# resultado seria el mejor modelo para cada especie
# predicciones para capas futuras (modelos deben empatar capas con observadas)
# mapas de cambio distribucion futura
# comparar cambios en los pixeles del presente y el futuro como una diferencia entre presente - futuro
# obtener todos los registros de especies en la region (comerciales y no comerciales)
# aplicar indice de riqueza para obtener modelo de biodiversidad para todo el area de estudio
# en los sdms seleccionar valor de probabilidad a partir del cual se considera presente
# se normaliza (todo menor a ese valor es 0) y se suman para obtener un indice de riqueza basado en el SDM
# comparar riqueza a partir de los SDMS de especies comerciales con el indice por interpolacion tomando todas las especies
# correlacionar proyecciones a futuro en SDMS con indice de riqueza
# [esto se podria usar en un momento para correlacionar con abundancia]

# este mismo proceso se debe de hacer para el Atlantico



