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
  
  ,"Belit.s.Guards" ,"Amboola"
  # 27 Scenarios opened (+5)

  ,"Conan..Wanderer.", "Conan..Mercenary.", "Conan..General.", "Conan..King."
  ,"Giant.Wolves"
  # 28 Scenarios opened (+1)
  
  # Ladies & Gentlemen Part 1 (6 models)
  ,"Zelata", "Belit..Savage.", "Balthus.Slasher", "Ageera"
  # 36 Scenarios Opened (+8)
  
  # Undead palooza
  #,"Mummies" 
  # 38 Scenarios Opened (+2)  (15 models)
  
  # Minions from King complete!
  #,"Skeletons"
  # 41 Scenarios Opened (+3)  (10 models)
  
  # Stingers
  #,"Giant.Scorpion","Scorpion.Broodmother"
  # 42 Scenarios Opened (+1) (11 models)
  
  # Critter festival
  #,"Grey.Man.Ape", "Forest.Demon","Hyperborean.Primitive", "Crows"
  # 50 Scenarios Opened (+8)  (8 models)
  
  # Most critters complete
  #,"Giant.Spider", "Swamp.Demon",  "Bone.Golem"
  # 60 Scenarios Opened (+10)  (3 models)

  # Wizards & and a Warrior (4 models)
  #,"Pelias", "Warlock", "Natohk", "Niord"
  # 75 Scenarios Opened (+15)
  
  # Ladies and Gents Part 2 (Full campaign unlocked)
  #,"Akivasha", "Gitara", "Valkyrie", "Khemsa", "Atali"
  # 89 Scenarios opened (+14)  (5 models)

  # Armoured 
  #,"Kerim.Shah", "Black.Dragons", "Kothian.Archer"
  # 99 Scenarios opened (+10)  (7 Models)
  
  #,"Crossbowmen" 
  # 100 Scenarios opened (+1) (10 models)
  
  # Remaining critters, start Nordheim (3 models)
  #,"Giants","Sabertooth.Tiger"
  # 101 Scenarios opened (+1)
  
  #,"Kushite.Witch.Hunters"  
  # 105 Scenarios opened (+4)  (5 models)

  # Finish Nordheim (15 models)
  #,"Aesir.Warriors" ,"Vanir.Warriors"
  # 108 Scenarios opened (+3)
  
  #,"Black.Ones"
  # 109 Scenarios opened (+1)  (10 models)

  # Stygia Leaders
  #,"Ikhmet" ,"Shentu" ,"Thoth.Amon"
  # 111 Scenarios opened (+2) (3 models)
  
  # Stygian Minions (10 models)
  #,"Eternal.Guard","Assassins"
  # 106 Scenarios opened (+5)
  
  # Khitai (22 models)
  #,"Shuang.Mian" ,"Javelin.Throwers"
  #,"Honor.Guards" ,"Khitan.Guards"
  #,"Foo.Dogs" ,"Earth.Demon"

  #, "Earth.Demon"
  #,"Dragon" # Use Proxy... (1 model)

)]


cat("\n\nFrom What I have painted!\n")
doit(y)




