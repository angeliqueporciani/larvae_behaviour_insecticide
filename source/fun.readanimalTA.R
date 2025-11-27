readAnimalTAmod <- function (animalTAPath, flipY = FALSE, imgHeight = NULL, rawDat = FALSE) 
{
  error <- .errorCheck(imgHeight = imgHeight)
  if (flipY == TRUE & !is.null(error)) {
    stop(error)
  }
  if (inherits(try(read.delim(animalTAPath, sep = ";"), silent = TRUE), 
               "try-error")) {
    stop("No such file or directory : undefined or wrong path supplied")
  }
  else {
    trackDat <- read.delim(animalTAPath, sep = ";")
  }
  FT <- which(names(trackDat) %in% c("Frame", "Time"))
  Data <- lapply(c("Y_", "X_"), function(x) {
    temp <- cbind(trackDat[, FT], trackDat[grep(x, names(trackDat))])
    Res <- cbind(temp[FT], utils::stack(lapply(temp[-FT], 
                                               as.numeric)))
    names(Res)[which(names(Res) == "values")] <- gsub("_", 
                                                      "", x)
    splittedId <- do.call(rbind, strsplit(as.character(Res$ind), 
                                          "_"))[, 2:3]
    Id <- stats::setNames(data.frame(apply(splittedId, 2, 
                                           function(x) gsub("[^0-9;^A-Z]", "", as.character(x)))), 
                          c("Arena", "Ind"))
    Res <- cbind(Res, Id)
    return(Res)
  })
  trackDatLong <- do.call(cbind, Data)
  trackDatLong <- trackDatLong[!duplicated(names(trackDatLong))]
  if (flipY == TRUE) {
    trackDatLong[["Y"]] <- MoveR::flipYCoords(trackDatLong[["Y"]], 
                                              imgHeight = imgHeight)
  }
  if (rawDat == FALSE) {
    AnimTA_all <- list(maj.ax = rep(NA, nrow(trackDatLong)), 
                       angle = rep(NA, nrow(trackDatLong)), min.ax = rep(NA, 
                                                                         nrow(trackDatLong)), x.pos = trackDatLong[["X"]], 
                       y.pos = trackDatLong[["Y"]], identity = trackDatLong[["Ind"]], 
                       frame = trackDatLong[["Frame"]])
  }
  else if (rawDat == TRUE) {
    AnimTA_all <- list(maj.ax = rep(NA, nrow(trackDatLong)), 
                       angle = rep(NA, nrow(trackDatLong)), min.ax = rep(NA, 
                                                                         nrow(trackDatLong)), x.pos = trackDatLong[["X"]], 
                       y.pos = trackDatLong[["Y"]], identity = trackDatLong[["Ind"]], 
                       frame = trackDatLong[["Frame"]], arena = trackDatLong[["Arena"]])
  }
  frameR <- max(AnimTA_all[["frame"]], na.rm = T)/max(trackDatLong[["Time"]], 
                                                      na.rm = T)
  AnimTA_all <- MoveR::convert2Tracklets(AnimTA_all)
  AnimTA_all <- MoveR::setInfo(AnimTA_all, frameR = frameR)
  return(AnimTA_all)
}
