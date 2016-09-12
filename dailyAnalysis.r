library("functional")

readNYTFrame <- function(i) {
  dataf <- read.csv(
    url(
      paste(
        "http://stat.columbia.edu/~rachel/datasets/nyt",
        i,
        ".csv",
        sep="",
        collapse=""
      )
    )
  )
  
  dataf$day <- rep.int(i, nrow(dataf))
  dataf
}

fetchDataSets <- function(start, end) {
  if (exists("NYTDataSets")) {
    lapply(NYTDataSets[c(start:end)], FUN = summarizeNYTFrame)
  } else {
    NYTDataSets <<- lapply(1:31, readNYTFrame)
    fetchDataSets(start, end)
  }
}

summarizeNYTDataSets <- function(start, end) {
  if (start >= end) {
    stop("Start index must be less than end index.")
  }
  
  dfsummaries <- fetchDatasets(start, end)
  dfsummaries <- do.call(rbind, lapply(dfsummaries, function(row) do.call(c, row)))
  dfsummaries <- as.data.frame(dfsummaries)
  dfsummaries <- transform(dfsummaries, ctr = Clicks.mean / Impressions.mean)
  dfsummaries$day <- c(1:nrow(dfsummaries)) 
  dfsummaries
}

summarizeColumn <- function(column) {
  lapply(
    list(
      min=min,
      qu1=function(col) as.numeric(quantile(col, probs=c(0.25))),
      median=median,
      mean=mean,
      qu3=function(col) as.numeric(quantile(col, probs=c(0.75))),
      max=max
    ),
    function(fnc) do.call(fnc, list(column))
  )
}

summarizeNYTFrame <- function(dataf) {
  unlist(lapply(dataf, summarizeColumn), recursive = FALSE)
}

NYTctrByAge <- function(dataf) {
  cutf <- dataf
  cutf$ageBracket <- cut(dataf$Age, c(-Inf, 18, 24, 34, 44, 54, 64, Inf))
  cutf <- transform(cutf, ctr = Clicks / Impressions)
  cutf <- cutf[!(abs(cutf$ctr) %in% c(Inf, NaN)),]
  cutf <- cutf[c("ageBracket", "ctr", "day")]
  cutf
}

NYTctrOverTimeByAge <- function(start, end) {
  ctrVsTime <- lapply(start:end, Compose(readNYTFrame, NYTctrByAge))
  ctrVsTime <- do.call(rbind, ctrVsTime)
  aggregate(ctr ~ ageBracket:day, ctrVsTime, mean)
}

library("ggplot2")
plotNYTctrOverTimeByAge <- function(nytdf) {
  config <- aes(x = nytdf$day, y = nytdf$ctr, color = nytdf$ageBracket)
  ggplot(nytdf, config) + geom_line()
}