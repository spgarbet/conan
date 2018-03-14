x <- read.csv("~/Projects/ordering/conan-scenarios.csv", stringsAsFactors=FALSE)

x <- subset(x, !is.na(Scenario) & Scenario != "")
x <- x[, !names(x) %in% c("X", "X.1", "X.2", "X.3", "X.4", "X.5", "X.6")]
nm <- names(x)

y <- as.matrix(x[,4:length(nm)])
rownames(y) <- x$Scenario

# Turn into 0 or 1 groups
y[] <- vapply(y, function(z) if(is.na(z)) 0 else 1, numeric(1))

# Now Find Minimums!

doit <- function(y)
{
  while(length(rownames(y) > 0))
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
  
    cat("'", chosen, "' by painting: ", paste(names(eliminate), collapse=", "), "\n", sep="")
    thelast <- rownames(y)
    y <- y[!rownames(y) %in% chosen,!colnames(y) %in% names(eliminate) ]
    if(length(rownames(y)) == 0) cat(paste("'", thelast, "'"))
  }
}
#doit(y)


# 2nd Run with what I randomly painted already eliminated
y <- as.matrix(x[,4:length(nm)])
rownames(y) <- x$Scenario

# Turn into 0 or 1 groups
y[] <- vapply(y, function(z) if(is.na(z)) 0 else 1, numeric(1))
# Eliminate painted figures
y <- y[, !colnames(y) %in% c(
  "Belit", "Conan", "Shevatas", "Hadrathus",
  "Valeria", "Thalis..Princess.", "Captain",
  "Skulthus", "Zaporavo", "Zogar.Sag",
  "Bossonian.Archers", "Hyenas", "Pict.Hunters", 
  "Pirates", "Dark.Demon", "Giant.Snake", "Thaug",
  "Tentacles")]#,
  #"Bossonian.Guards", "Pict.Warriors", "Pict.Archers",
  # "Thak", "Pallantides", "Thag", "Yogah.of.Yag","Kushite.Witch.Hunters",
  # "Belit.s.Guards", "Outer.Dark.Demon", "Khosatral.Khel",
  # "Conan..Thief.", "Crossbowmen", "Zelata", "Giant.Wolves","N.Gora","Pelias",
  # "Belit..Savage.", "Conan..Amra.", "Gitara","Kerim.Shah", "Giant.Spider",
  # "Skeletons", "Mummies", "Constantius", "Bone.Golem", "Giant.Scorpion", "Ageera",
  # "Hyperborean.Primitive", "Conan..General.", "Akivasha", "Thog", "Natohk")]

cat("\n\nFrom What I have painted!\n")
doit(y)




