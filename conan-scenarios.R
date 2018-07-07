x <- read.csv("conan-scenarios.csv", stringsAsFactors=FALSE)

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
    if(length(rownames(y)) == 0) cat(paste0("'", thelast, "'"))
  }
}
#doit(y)


# 2nd Run with what I randomly painted already eliminated
y <- as.matrix(x[,4:length(nm)])
rownames(y) <- x$Scenario

# Turn into 0 or 1 groups
y[] <- vapply(y, function(z) if(is.na(z)) 0 else 1, numeric(1))
# Eliminate painted figures -- This is my ordering-- yours will inevitably vary
y <- y[, !colnames(y) %in% c(
  # "Pict.Hunters"
  
  "Belit", "Conan", "Shevatas", "Hadrathus",
  "Valeria", "Thalis..Princess.", "Captain",
  "Skulthus", "Zaporavo", "Zogar.Sag",
  "Bossonian.Archers", "Hyenas", "Pict.Hunters",
  "Pirates", "Dark.Demon", "Giant.Snake", "Thaug", "Tentacles",
  "Thak", "Pallantides", "Olgerd.Vladislav", "Yogah.of.Yag",
  
  # 3 scenarios opened, fully painted.
  
  # # Next group
  "Bossonian.Guards", "Thog",
  
  # 11 scenarios opened, fully painted.
  
  # # Next Plans -- all beefcake
  "Pict.Warriors", "Pict.Archers",
  "Conan..Warlord.", "Taurus", "Conan..Thief.", "Conan..Wanderer.",
  "Conan..Amra.", "Constantius", "Conan..Mercenary.", "Conan..General.",
  "Khosatral.Khel",
  
  # 19 Scenarios opened, fully painted
  
  #  
  # # All the dark flesh
  "Kushite.Witch.Hunters", "N.Gora", "Belit.s.Guards",
  "Amboola", "Ageera", "Baal.Pteor",
  
  # 22 Scenarios opened
  
  #  
  # # Ladies & Gentlemen
  "Valkyrie", "Balthus.Slasher", "Zelata",
  "Belit..Savage.", "Khemsa", "Akivasha", "Gitara", "Atali",
  
  # 25 Scenarios opened

  # Critters
  "Camel", "Giant.Wolves", "Giant.Scorpion", "Grey.Man.Ape",
  "Giant.Spider", "Crows", "Sabertooth.Tiger",
  
  # 39 Scenarios opened
  
  # Undead Hordes
  "Mummies", "Pelias", "Warlock", "Natohk", "Skeletons",
  # 
  # All the Demons
  "Outer.Dark.Demon", "Forest.Demon", "Swamp.Demon",
  "Bone.Golem"
  # 
  # # Armoured
  # "Black.Dragons", "Hyperborean.Primitive", "Kerim.Shah", "Kothian.Archer",
  # "Crossbowmen",
  # 
  # # Nordheim
  # "Giants", "Niord", "Aesir.Warriors", "Vanir.Warriors"

)]


cat("\n\nFrom What I have painted!\n")
doit(y)




