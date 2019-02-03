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
  ,"Conan..Warlord."
  ,"N.Gora", "Baal.Pteor"
  # 22 Scenarios opened, fully painted
  
  ,"Belit.s.Guards","Amboola"
  # 26 Scenarios opened (+4)

  #,"Conan..Wanderer.", "Conan..Mercenary.", "Conan..General.", "Conan..King."
  
  #,"Giant.Wolves"
  # 27 Scenarios opened (+1)
  
  # Ladies & Gentlemen Part 1
  #,"Zelata", "Belit..Savage.", "Balthus.Slasher"
  # 35 Scenarios Opened (+8)
  
  #,"Mummies"
  # 37 Scenarios Opened (+2)
  
  #,"Skeletons"
  # 40 Scenarios Opened (+3)
  
  #,"Giant.Scorpion","Scorpion.Broodmother","Grey.Man.Ape", "Forest.Demon"
  # 45 Scenarios Opened (+5)
  
  #,"Hyperborean.Primitive", "Crows"
  # 50 Scenarios Opened (+5)
  
  #,"Giant.Spider", "Swamp.Demon",  "Bone.Golem"
  # 58 Scenarios Opened (+8)

  # Wizards & and a Warrior
  #,"Pelias", "Warlock", "Natohk", "Niord"
  # 67 Scenarios Opened (+9)
  
  # Ladies and Gents Part 2 (Full campaign unlocked)
  #,"Akivasha", "Gitara", "Valkyrie", "Khemsa", "Atali"
  # 79 Scenarios opened (+12)

  # Armoured
  #,"Kerim.Shah", "Black.Dragons", "Kothian.Archer"
  # 85 Scenarios opened (+6)
  
  #,"Crossbowmen"
  # 86 Scenarios opened (+1)
  
  #,"Kushite.Witch.Hunters", "Ageera"  
  # 91 Scenarios opened (+5)

  # Remaining critters, start Nordheim
  #,"Sabertooth.Tiger","Giants"
  # 94 Scenarios opened (+3)
  
  # Finish Nordheim
  #,"Aesir.Warriors", "Vanir.Warriors"
  # 98 Scenarios opened (+4)
  
  ,"Dragon" # Use Proxy...
  # 99 Scenarios opened (+1)
 
  ,"Black.Ones"
  # 100 Scenarios opened (+1)

  # Stygia
  ,"Ikhmet", "Shentu", "Thot.Amon"
  ,"Assassins"
  ,"Eternal.Guard"
  # 107 Scenarios opened (+7)
  
  # Khitan
  #,"Shuang.Mian","Javelin.Throwers"
  #,"Honor.Guards", "Khitan.Guards"
  #,"Foo.Dogs"
  # 115 Scenarios!!! (+8)
)]


cat("\n\nFrom What I have painted!\n")
doit(y)




