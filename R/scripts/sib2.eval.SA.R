sib2.eval.SA <- function (j, Thetas, nparamsets, N, X.Boundaries, write2disk = FALSE, 
    REPORT, verbose, digits, model.FUN, model.FUN.args, parallel, 
    ncores, part.dirs,obs) 
{   #  j = 12
    if (parallel != "none"){
      model.FUN.args <- modifyList(model.FUN.args, 
                                   list(dir_out = part.dirs[j],
                                        id = ifelse(j<10,
                                                    paste0("00",j),
                                                    ifelse(j<100,
                                                           paste0("0",j),
                                                           as.character(j)) ))
      )
    }
    nelements <- 2
    out <- vector("list", nelements)
    gof.is.numeric <- FALSE
    while (!gof.is.numeric) {
        params <- Thetas[j, ]
        suppressWarnings(param.values <- as.numeric(formatC(params, 
            format = "E", digits = digits)))
        model.FUN.args <- modifyList(model.FUN.args, as.list( params))
        hydromod.out <- do.call(model.FUN, as.list(model.FUN.args))
        hydromod.win <- selectByDate(hydromod.out,
                                     start = obs$gof.Ini,
                                     end = obs$gof.Fin)
        
        out[[1]] <- do.call(gof.FUN,
                            args = list(sim = hydromod.win[,model.FUN.args$vars_out],
                                        obs = obs$gof.obs[,model.FUN.args$vars_out])
        )
        out[[2]] <- hydromod.out[, model.FUN.args$vars_out ]
        ifelse(is.finite(out[[1]]), gof.is.numeric <- TRUE, gof.is.numeric <- FALSE)
        if (!gof.is.numeric) {
            warning(" parameter set ", j, ": not numeric GoF ! => it was replaced")
            tmp <- rLHS(n = N, ranges = X.Boundaries)
            Thetas[j, ] <- tmp[j, ]
        }
    }
    names(out)[1:nelements] <- c("GoF", "model.out")
    if (j/REPORT == floor(j/REPORT)) {
        if (verbose) 
            message("[ Parameter set ", format(j, width = 4, 
                justify = "left"), "/", nparamsets, ". Finished.   GoF: ", 
                format(out[["GoF"]], scientific = TRUE, 
                  digits = digits), "]")
    }
    return(out)
}