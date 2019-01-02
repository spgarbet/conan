x <- read.csv("conan-scenarios.csv", stringsAsFactors=FALSE)

x <- subset(x, !is.na(Scenario) & Scenario != "")
x <- x[, !names(x) %in% c("X", "X.1", "X.2", "X.3", "X.4", "X.5", "X.6")]
nm <- names(x)

y <- as.matrix(x[,4:length(nm)])
rownames(y) <- x$Scenario

# Turn into 0 or 1 groups
y[] <- vapply(y, function(z) if(is.na(z)) 0 else 1, numeric(1))

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
    rows <- !rownames(y) %in% chosen
    cols <- !colnames(y) %in% names(eliminate)
    if(sum(rows) > 0 && sum(cols) > 0) y <- y[rows, cols, drop=FALSE]
    if(length(rownames(y)) == 0) cat(paste0("'", thelast, "'"))
    if(length(rownames(y)) == 1) cat(count, " '", rownames(y), "' by painting: ", paste(colnames(y), collapse=", "), "\n", sep="")
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

  "Belit", "Conan", "Shevatas", "Hadrathus",
  "Valeria", "Thalis..Princess.", "Captain",
  "Skulthus", "Zaporavo", "Zogar.Sag",
  "Bossonian.Archers", "Hyenas", "Pict.Hunters",
  "Pirates", "Dark.Demon", "Giant.Snake", "Thaug", "Tentacles",
  "Thak", "Pallantides", "Olgerd.Vladislav", "Yogah.of.Yag",
  "Thog",
  "Bossonian.Guards",
  "Outer.Dark.Demon",
  "Pict.Warriors",
  "Taurus", "Camel", "Khosatral.Khel",
  "Pict.Archers",
  "Conan..Thief.", "Conan..Amra.","Constantius"
  # 20 Scenarios opened

  ,"Conan..Warlord."
  # 22 Scenarios opened, fully painted
  
  #, "Conan..Wanderer.", "Conan..Mercenary.", "Conan..General.", "Conan..King."
  
  # All the dark flesh
  ,"N.Gora", "Belit.s.Guards","Amboola", "Baal.Pteor"
  # 25 Scenarios opened (+3)

  #,"Giant.Wolves"
  # 26 Scenarios opened (+1)
  
  # Ladies & Gentlemen Part 1
  #,"Zelata", "Belit..Savage.", "Balthus.Slasher"
  # 34 Scenarios Opened (+8)
  
  #,"Mummies"
  # 36 Scenarios Opened (+2)
  
  #,"Skeletons"
  # 39 Scenarios Opened (+3)
  
  #,"Giant.Scorpion","Scorpion.Broodmother"
  # 40 Scenarios Opened (+1)
  
  #,"Grey.Man.Ape", "Forest.Demon"
  # 44 Scenarios Opened (+4)
  
  #,"Hyperborean.Primitive", "Crows"
  # 48 Scenarios Opened (+4)
  
  #,"Giant.Spider", "Swamp.Demon",  "Bone.Golem"
  # 58 Scenarios Opened (+10)

  # Wizards
  #,"Pelias", "Warlock", "Natohk"
  # 65 Scenarios Opened (+7)
  
  # Ladies and Gents Part 2 (Full campaign unlocked)
  #,"Akivasha", "Gitara", "Valkyrie", "Khemsa", "Atali"
  # 79 Scenarios opened (+14)

  #,"Kushite.Witch.Hunters", "Ageera"  
  # 80 Scenarios opened (+1)

  # Armoured
  #,"Kerim.Shah", "Black.Dragons", "Kothian.Archer"
  # 87 Scenarios opened (+7)

  #,"Crossbowmen"
  # 88 Scenarios opened (+1)
  
  # Remaining critters
  #,"Sabertooth.Tiger"
  # 89 Scenarios opened (+1)
  #
  #,"Dragon" # Use Proxy...
  # 90 Scenarios opened (+1)
  # 
  # # Nordheim
  #,"Giants", "Niord", "Aesir.Warriors", "Vanir.Warriors"
  # 93 Scenarios opened

  #,"Ikhmet", "Shentu", "Thot.Amon",
  #,"Assassins",
  #,"Eternal.Guard"
  #,"Shuang.Mian","Javelin.Throwers"
  #,"Honor.Guards", "Khitan.Guards"
  
  # 103 Scenarios!!!
)]


cat("\n\nFrom What I have painted!\n")
doit(y)




