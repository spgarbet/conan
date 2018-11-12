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
  current <- 1
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
  
    cat(current, " '", chosen, "' by painting: ", paste(names(eliminate), collapse=", "), "\n", sep="")
    current <- current + 1
    
    thelast <- rownames(y)
    y <- y[!rownames(y) %in% chosen,!colnames(y) %in% names(eliminate) ]
    if(length(rownames(y)) == 0)
    {
      cat("And ", length(thelast), "more: ")
      cat(paste0("'", thelast, "'"))
    }
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
  "Thog",
  
  # # Next group
  "Bossonian.Guards",

  "Outer.Dark.Demon",
  "Camel",
  # 
  # # Next Plans -- all beefcake
  "Pict.Warriors",
  "Pict.Archers",
  "Taurus", 
  "Conan..Amra.",
  "Conan..Warlord.",  "Khosatral.Khel",
  "Conan..Thief.", "Conan..Wanderer.",
  "Constantius", "Conan..Mercenary.", "Conan..General."
  # 22 Scenarios opened, fully painted
  
    
  # All the dark flesh
  # "Kushite.Witch.Hunters", "N.Gora", "Belit.s.Guards",
  # "Amboola", "Ageera", "Baal.Pteor",
  # 28 Scenarios opened

  # Ladies & Gentlemen
  # "Valkyrie", "Balthus.Slasher", "Zelata",
  # "Belit..Savage.", "Khemsa", "Akivasha", "Gitara", "Atali",
  # 34 Scenarios opened
  
  # Critters
  # "Giant.Wolves", "Giant.Scorpion", "Grey.Man.Ape",
  # "Giant.Spider", "Crows", "Sabertooth.Tiger",
  # 51 Scenarios opened

  # Undead Hordes
  # "Mummies", "Pelias", "Warlock", "Natohk", "Skeletons",
  # 64 Scenarios opened
 
  # All the Demons
  # "Forest.Demon", "Swamp.Demon","Bone.Golem",
  # 76 Scenarios opened  
 
  # Armoured
  # "Black.Dragons", "Hyperborean.Primitive", "Kerim.Shah", "Kothian.Archer",
  # "Crossbowmen",
  # 90 Scenarios opened
  
  # Nordheim
  # "Giants", "Niord", "Aesir.Warriors", "Vanir.Warriors"
  # 93 Scenarios opened

)]


cat("\n\nFrom What I have painted!\n")
doit(y)




