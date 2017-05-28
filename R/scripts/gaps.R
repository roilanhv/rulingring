## Funcao gaps (hydrosanity) com a opcao reverse 
## default: reverse = F
## se reverse = T, retorna a posicao e o numero de elementos a partir daquela posicao com dados validos
gaps <- 
function (x, max.length = Inf, internal.only = F, reverse = F) 
{
   
    if(reverse) seriesNA <- !is.na(x) else  seriesNA <- is.na(x)

    diffNA <- c(0, diff(seriesNA))
    preDataGap <- match(F, seriesNA) - 1
    postDataGap <- match(F, rev(seriesNA)) - 1
    diffNA[preDataGap + 1] <- 0
    gapEnd <- which(diffNA == -1) - 1
    nGaps <- length(gapEnd)
    naCumSum <- cumsum(seriesNA)
    gapLength <- naCumSum[gapEnd] - naCumSum[c(preDataGap + 1, 
        gapEnd[-nGaps])]
    if (internal.only == FALSE) {
        gapLength <- c(if (preDataGap > 0) {
            preDataGap
        }, gapLength, if (postDataGap > 0) {
            postDataGap
        })
        gapEnd <- c(if (preDataGap > 0) {
            preDataGap
        }, gapEnd, if (postDataGap > 0) {
            length(x)
        })
    }
    ok <- (gapLength <= max.length)
    gapLength <- gapLength[ok]
    gapEnd <- gapEnd[ok]
    gapStart <- gapEnd - gapLength + 1
    gapInfo <- data.frame(length = gapLength, start = gapStart)
    if (internal.only) {
        attr(gapInfo, "pre.data") <- preDataGap
        attr(gapInfo, "post.data") <- postDataGap
    }
    return(gapInfo)
}
