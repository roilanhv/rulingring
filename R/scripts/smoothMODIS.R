smooth.modis <- function (x, w = NULL, t = NULL, groupYears = TRUE, timeInfo = orgTime(x), 
          df = 6, outDirPath = "./", ...) 
{
    opt <- combineOptions(...)
    dir.create(outDirPath, showWarnings = FALSE)
    outDirPath <- normalizePath(outDirPath, winslash = "/", 
                                mustWork = TRUE)
    outDirPath <- MODIS:::setPath(outDirPath)
    bitShift <- opts$bitShift
    bitMask <- opts$bitMask
    threshold <- opts$threshold
    dataFormat <- opts$dataFormat
    rasterOut <- toupper(writeFormats())
    if (dataFormat %in% rasterOut[, "name"]) {
        dataFormat <- MODIS:::getExtension(dataFormat)
    }else {
        stop("Argument dataFormat='", dataFormat, "' in 'smooth.spline.raster()' is unknown/not supported. Please run 'writeFormats()' (column 'name') so list available dataFormat's")
    }
    if (!inherits(x, "Raster")) {
        x <- stack(x, quick = TRUE)
    }
    if (!inherits(w, "Raster") & !is.null(w)) {
        w <- stack(w, quick = TRUE)
    }
    if (!inherits(t, "Raster") & !is.null(t)) {
        t <- stack(t, quick = TRUE)
    }
    tsLength <- as.numeric(max(timeInfo$inputLayerDates) - (min(timeInfo$inputLayerDates) - 
                                                                1))
    tsLayers <- length(unique(timeInfo$inputLayerDates))
    indf <- df
    if (is.character(df)) {
        cat("Using fixed 'df':", df, "\n")
        nameDf <- "FixedDf"
    }else {
        df <- df * (tsLength/365)
        cat("Yearly 'df' is:", indf, "\nNow changed with df*('length of input data period in days'/365) to:", 
            df, "\n")
        nameDf <- "YearlyDf"
    }
    df <- as.numeric(df)
    b <- list()
    if (groupYears) {
        for (a in seq_along(unique(format(timeInfo$outputLayerDates, 
                                          "%Y")))) {
            y <- unique(format(timeInfo$outputLayerDates, "%Y"))[a]
            b[[a]] <- brick(raster(x), nl = as.integer(sum(format(timeInfo$outputLayerDates, 
                                                                  "%Y") == y)), values = FALSE)
            b[[a]] <- writeStart(b[[a]], filename = paste(opt$outDirPath, 
                                                          "/NDVI_", nameDf, indf, "_year", y, dataFormat, 
                                                          sep = ""),overwrite=T)
        }
    }else {
        b[[1]] <- brick(raster(x), nl = as.integer(length(timeInfo$outSeq)), 
                        values = FALSE)
        b[[1]] <- writeStart(b[[1]], filename = paste(outDirPath, 
                                                      "/NDVI_", nameDf, indf, "_fullPeriod", dataFormat, 
                                                      sep = ""), overwrite=T)
    }
    tr <- blockSize(x)
    cluster <- raster:::.doCluster()
    if (cluster) {
        cl <- getCluster()
        on.exit(endCluster())
        nodes <- getOption("rasterClusterCores")
        clF <- function(i) {
            require(MODIS)
        }
        for (i in 1:nodes) {
            sendCall(cl[[i]], clF, i, tag = i)
            recvOneData(cl)
        }
        clusterEvalQ(cl, require(bitops))
        clusterEvalQ(cl, require(rgdal))
        tr <- blockSizeCluster(x)
    }
    cat("Data is in, start processing!\n")
    clFun <- function(l) {
        minval <- -2000
        val <- getValues(x, row = tr$row[l], nrows = tr$nrows[l])
        mtrdim <- dim(val)
        set0 <- val <= minval
        set0[is.na(val)] <- TRUE
        set0[rowSums(val, na.rm = TRUE) == 0] <- TRUE
        if (!is.null(w)) {
            wtu <- getValues(w, row = tr$row[l], nrows = tr$nrows[l])
            if (max(wtu) > 1) {
                bits <- detectBitInfo(vi, "VI usefulness", warn = FALSE)
                if (is.null(bits)) {
                    stop("Could not extract 'bits' for weighting from this product. Use 'makeWeights' function to generate weightings manualy!")
                }
                wtu <- makeWeights(wtu, bitShift = bits$bitShift, 
                                   bitMask = bits$bitMask, decodeOnly = TRUE)
            }
            set0[wtu == 0] <- TRUE
        }else {
            wtu <- matrix(1, nrow = mtrdim[1], ncol = mtrdim[2])
        }
        if (inherits(t, "Raster")) {
            inTu <- getValues(t, row = tr$row[l], nrows = tr$nrows[l])
            inTu <- repDoy(inTu, timeInfo, bias = timeInfo$inSeq[1] - 
                               1)
            set0[is.na(inTu)] <- TRUE
            inTu[set0] <- 0
        }else {
            inTu <- matrix(timeInfo$inSeq, nrow = mtrdim[1], 
                           ncol = mtrdim[2], byrow = TRUE)
        }
        wtu[set0] <- 0
        val[set0] <- 0
        r <- MODIS:::smooth.splineMtr(vali = val, wti = wtu, inTi = inTu, 
                              timeInfo = timeInfo, df = df)
        r[rowSums(abs(r)) == 0, ] <- NA
        return(r)
    }
    if (!cluster) {
        for (i in seq_along(tr$row)) {
            res <- clFun(i)
            res <- round(res)
            if (groupYears) {
                for (a in seq_along(unique(format(timeInfo$outputLayerDates, 
                                                  "%Y")))) {
                    y <- unique(format(timeInfo$outputLayerDates, 
                                       "%Y"))[a]
                    b[[a]] <- writeValues(b[[a]], res[, format(timeInfo$outputLayerDates, 
                                                               "%Y") == y], tr$row[i])
                }
            }else {
                b[[1]] <- writeValues(b[[1]], res, tr$row[i])
            }
        }
    }else {
        for (i in 1:nodes) {
            sendCall(cl[[i]], clFun, i, tag = i)
        }
        for (i in 1:tr$n) {
            d <- recvOneData(cl)
            if (!d$value$success) {
                stop("Cluster error in Row: ", tr$row[d$value$tag], 
                     "\n")
            }
            ind <- d$value$tag
            d$value$value <- round(d$value$value)
            if (groupYears) {
                for (a in seq_along(unique(format(timeInfo$outputLayerDates, 
                                                  "%Y")))) {
                    y <- unique(format(timeInfo$outputLayerDates, 
                                       "%Y"))[a]
                    b[[a]] <- writeValues(b[[a]], d$value$value[, 
                                                                format(timeInfo$outputLayerDates, "%Y") == 
                                                                    y], tr$row[ind])
                }
            }else {
                b[[1]] <- writeValues(b[[1]], d$value$value, 
                                      tr$row[ind])
            }
            ni <- nodes + i
            if (ni <= tr$n) {
                sendCall(cl[[d$node]], clFun, ni, tag = ni)
            }
        }
    }
    for (a in seq_along(b)) {
        b[[a]] <- writeStop(b[[a]])
        if (groupYears) {
            y <- unique(format(timeInfo$outputLayerDates, "%Y"))[a]
            write.table(x = timeInfo$outputLayerDates[format(timeInfo$outputLayerDates, 
                                                             "%Y") == y], file = paste(outDirPath, "/LayerDates_NDVI_", 
                                                                                       nameDf, indf, "_year", y, sep = ""), row.names = FALSE, 
                        col.names = FALSE)
        }
        else {
            write.table(x = timeInfo$outputLayerDates, file = paste(opt$outDirPath, 
                                                                    "/LayerDates_NDVI_", nameDf, indf, "fullPeriod", 
                                                                    sep = ""), col.names = FALSE, row.names = FALSE)
        }
    }
    return(NULL)
}
