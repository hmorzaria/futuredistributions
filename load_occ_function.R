load_occ <- function(path = getwd(), Env, file = NULL, ..., Xcol = "Longitude",
                     Ycol = "Latitude", Spcol = NULL, GeoRes = TRUE, reso = max(res(Env@layers[[1]])),
                     verbose = TRUE, GUI = FALSE) {
  # # Check arguments
  # .checkargs(path = path, file = file, Xcol = Xcol, Ycol = Ycol, Spcol = Spcol,
  #            GeoRes = GeoRes, reso = reso, verbose = verbose, GUI = GUI)
  # 
  # pdir = getwd()
  if (verbose) {
    cat("Occurrences loading \n")
  }
  # setwd(path)
  if (is.null(file)) {
    file <- as.character(list.files(path = path, pattern = ".csv")[[1]])
  }
  if (!is.null(path)) {
    file <- paste0(path, "/", file)
  }
  Occurrences <- read.csv2(file = file, ...)  # Occ = occurrences
  
  # Checking columns format
  if (!is.null(Spcol)) {
    if (!inherits(Occurrences[, which(names(Occurrences) == Spcol)],
                  "factor")) {
      Occurrences[, which(names(Occurrences) == Spcol)] <- as.factor(Occurrences[,
                                                                                 which(names(Occurrences) == Spcol)])
    }
  }
  if (!inherits(Occurrences[, which(names(Occurrences) == Xcol)], "numeric")) {
    if (inherits(Occurrences[, which(names(Occurrences) == Xcol)],
                 "factor")) {
      Occurrences[, which(names(Occurrences) == Xcol)] <- as.numeric(as.character(Occurrences[,
                                                                                              which(names(Occurrences) == Xcol)]))
      Occurrences[, which(names(Occurrences) == Ycol)] <- as.numeric(as.character(Occurrences[,
                                                                                              which(names(Occurrences) == Ycol)]))
    }
  }
  
  # Checking points validity
  Occurrences$validity <- raster::extract(Env[[1]], Occurrences[, c(which(names(Occurrences) ==
                                                                            Xcol), which(names(Occurrences) == Ycol))])
  if (length(which(is.na(Occurrences$validity))) > 0) {
    warning("You have occurrences that aren't in the extent of your environmental variables, they will be automatically removed ! \n")
    Occurrences <- Occurrences[-which(is.na(Occurrences$validity)),
                               ]
  }
  Occurrences <- Occurrences[-which(names(Occurrences) == "validity")]
  Occurrences <- droplevels(Occurrences)
  
  # Geographical resampling
  if (is.null(Spcol)) {
    Occurrences$SpNULL <- 1
    Spcol <- "SpNULL"
    Occurrences$SpNULL <- as.factor(Occurrences$SpNULL)
  }
  for (i in seq_len(length(levels(Occurrences[, which(names(Occurrences) ==
                                                      Spcol)])))) {
    if (GeoRes) {
      if (verbose) {
        cat(levels(as.factor(Occurrences[, which(names(Occurrences) ==
                                                   Spcol)]))[i], "geographical resampling \n")
      }
      SpOccurrences <- subset(Occurrences, Occurrences[which(names(Occurrences) ==
                                                               Spcol)] == levels(as.factor(Occurrences[, which(names(Occurrences) ==
                                                                                                                 Spcol)]))[i])
      thin.result <- thin(SpOccurrences, long.col = Xcol, lat.col = Ycol,
                          spec.col = Spcol, thin.par = reso, reps = 1, locs.thinned.list.return = TRUE,
                          write.files = FALSE, write.log.file = FALSE, verbose = FALSE)
      if (GUI) {
        incProgress(1/length(levels(as.factor(Occurrences[, which(names(Occurrences) ==
                                                                    Spcol)]))), detail = paste(levels(as.factor(Occurrences[,
                                                                                                                            which(names(Occurrences) == Spcol)]))[i], "thinned"))
      }
      deleted <- {
      }
      occ.indices <- c(seq_len(length(row.names(SpOccurrences))))
      res.indices <- as.numeric(row.names(thin.result[[1]]))
      for (i in seq_len(length(occ.indices))) {
        if (!(occ.indices[i] %in% res.indices)) {
          deleted <- c(deleted, occ.indices[i])
        }
      }
      deleted <- row.names(SpOccurrences[deleted, ])
      deleted <- which(row.names(Occurrences) %in% deleted)
      if (length(deleted) > 0) {
        Occurrences <- Occurrences[-deleted, ]
      }
    }
  }
  Occurrences <- droplevels(Occurrences)
  
  # Test species occurrences > 3
  for (i in seq_len(length(levels(Occurrences[, which(names(Occurrences) ==
                                                      Spcol)])))) {
    sp <- levels(as.factor(Occurrences[, which(names(Occurrences) ==
                                                 Spcol)]))[i]
    spocc <- subset(Occurrences, Occurrences[, which(names(Occurrences) ==
                                                       Spcol)] == sp)
    l <- length(spocc[, 1])
    if (l < 4) {
      warning(paste(sp, "have 3 or less occurrences after spatial thinning, it's not enough for modelling, this species will be automatically removed ! \n"))
      Occurrences <- Occurrences[-which(Occurrences[, which(names(Occurrences) ==
                                                              Spcol)] == sp), ]
    }
  }
  if (Spcol == "SpNULL") {
    Occurrences <- Occurrences[-which(names(Occurrences) == "SpNULL")]
  }
  Occurrences <- droplevels(Occurrences)
  
  # setwd(pdir)
  return(Occurrences)
}