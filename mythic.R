x <- read.csv("mythic.csv", stringsAsFactors=FALSE)

x <- subset(x, !is.na(Scenario) & Scenario != "")
x <- x[, !names(x) %in% c("X", "X.1", "X.2", "X.3", "X.4", "X.5", "X.6")]
nm <- names(x)

y <- as.matrix(x[,4:length(nm)])
rownames(y) <- x$Scenario

# Turn into 0 or 1 groups
y[] <- vapply(y, function(z) if(is.na(z)) 0 else 1, numeric(1))

# Get rid of recruit scenarios
y <- y[y[,"Recruit"]==0, 2:(dim(y)[2]) ]

# Now Find Minimums!

count <- 1
doit <- function(y)
{
  while(length(rownames(y)) > 1)
  {
    min_paint <- min(rowSums(y))
    possibilities <- rownames(y)[rowSums(y) == min_paint]
    previous <- -1
    chosen   <- NULL
    eliminate <- c()
    for(i in possibilities)
    {
      paints <- y[rownames(y) == i,]
      paints <- paints[paints > 0]
  
      touches <- if(length(names(paints)) == 1)
      {
        sum(y[,names(paints)] > 0)
      } else
      { 
        sum(rowSums(y[,names(paints)]) > 0)
      }
      if(touches > previous)
      {
        previous <- touches
        chosen <- i
        eliminate <- paints
      }
    }
  
    cat(count, " '", chosen, "' by painting: ", paste(names(eliminate), collapse=", "), "\n", sep="")
    count <- count + 1
    thelast <- rownames(y)
    y <- y[!rownames(y) %in% chosen,!colnames(y) %in% names(eliminate), drop=FALSE ]
    rows <- !rownames(y) %in% chosen
    cols <- !colnames(y) %in% names(eliminate)
    if(sum(rows) > 0 && sum(cols) > 0) y <- y[rows, cols, drop=FALSE]
    if(length(rownames(y)) == 0) cat(paste0("'", thelast, "'"))
    if(length(rownames(y)) == 1) cat(count, " '", rownames(y), "' by painting: ", paste(colnames(y), collapse=", "), "\n", sep="")
  }
}

doit(y)