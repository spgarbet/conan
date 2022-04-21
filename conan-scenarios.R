
x <- read.csv("conan-scenarios.csv", stringsAsFactors=FALSE, skip=1)


# x <- data.frame(
#   Scenario = c("Zero", "Zero.1", "One", "Two", "Three", "Four"),
#   Players  = NA,
#   Book     = NA,
#   Mini0    = c(NA, NA, NA, NA, NA, NA), # Not used
#   Mini0.1  = c(NA, NA, NA, NA, NA, NA),
#   Mini1    = c(NA, NA,  1,  1,  1,  1),
#   Mini2    = c(NA, NA, NA,  1,  1,  1),
#   Mini2.1  = c(NA, NA, NA,  1,  1,  1),
#   Mini3    = c(NA, NA, NA, NA,  1,  1)
# )

# x <- data.frame(
#   Scenario = c("Single", "Double.1", "Double.2"),
#   Players  = NA,
#   Book     = NA,
#   Mini1    = c( 1, NA, NA),
#   Mini2    = c( 1, NA, NA),
#   Mini3    = c(NA,  1,  1),
#   Mini4    = c(NA,  1,  1)
# )


x <- subset(x, !is.na(Scenario) & Scenario != "")
x <- x[, !names(x) %in% c("X", "X.1", "X.2", "X.3", "X.4", "X.5", "X.6")]
nm <- names(x)

y <- as.matrix(x[,4:length(nm)])
rownames(y) <- x$Scenario

# Turn into 0 or 1 groups
y[] <- vapply(y, function(z) if(is.na(z)) 0 else 1, numeric(1))





# Now Find Minimums!

paint_order <- function(y)
{
  paint <- list()
  pos   <- 1
  
  while(length(rownames(y)) > 0)
  {
    min_paint     <- min(rowSums(y)) # Minimum groups to paint
    possibilities <- rownames(y)[rowSums(y) == min_paint] # Possible scenarios
    
    if(sum(y) == 0) # Remaining Unused
    {
      paint[[pos]] = list(colnames(y), NA)
      y <- NULL
    } else 
    if(min_paint == 0) # Nothing to paint
    {
      paint[[pos]] = list(NA, possibilities)
      
      y <- y[!rownames(y) %in% possibilities, ,drop=FALSE]
    } else # find the best set to paint
    {
      previous  <- -1   # Number required to paint
      chosen    <- NULL # Chosen at this number
      eliminate <- c()  # Set to Paint
      
      for (i in possibilities) # Consider each scenario
      {
        paints  <- y[i, ,drop=FALSE]
        opens   <- apply(y,1,function(x) all(x==paints[1,]))
        touches <- sum(y[, paints[1,]==1, drop=FALSE])
      
        if (touches > previous)
        {
          previous  <- touches
          chosen    <- names(opens[opens])
          eliminate <- colnames(paints)[paints[1,] > 0]
        }
      }

      paint[[pos]] = list(eliminate, chosen)
      
      rows <- rownames(y) %in% chosen
      cols <- colnames(y) %in% eliminate
      y <- y[!rows, !cols, drop=FALSE]
    }
    pos <- pos + 1

  }
  # Any minis that have no scenarios
  if(length(colnames(y)) > 0) paint[[pos]] <- list(colnames(y), NA)
  
  paint
}

print_order <- function(z)
{
  count     <- 0
  for(i in 1:length(z))
  {
    minis     <- z[[i]][[1]]
    scenarios <- z[[i]][[2]]

    
    if(length(minis) == 1     && is.na(minis))     minis     <- "Paint Nothing"
    if(length(scenarios) == 1 && is.na(scenarios)) scenarios <- "Not Used In Any Scenario"
    
    cat("-", paste(minis, collapse=", "), "\n")
    cat(paste("    ", (1:length(scenarios))+count, rep(". ", length(scenarios)), scenarios, sep="", collapse="\n"), "\n")
    
    count <- count + length(scenarios)
  }
}

print_order(paint_order(y))


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
  "Hyenas", "Pict.Hunters",
  "Bossonian.Archers",
  "Thaug", "Tentacles",
  "Pirates",
  "Dark.Demon", "Giant.Snake",
  "Thak",
  "Pallantides", "Olgerd.Vladislav", "Yogah.of.Yag",
  "Thog",
  "Bossonian.Guards",
  "Outer.Dark.Demon",
  "Pict.Warriors",
  "Taurus", "Camel", "Khosatral.Khel",
  "Pict.Archers",
  "Conan..Thief.", "Conan..Amra.","Constantius"
  ,"Conan..Warlord."
  ,"N.Gora", "Baal.Pteor"
  ,"Belit.s.Guards" ,"Amboola"
  ,"Conan..Wanderer.", "Conan..Mercenary.", "Conan..General.", "Conan..King."
  ,"Giant.Wolves"
  ,"Giant.Spider" ,"Giant.Scorpion","Hyperborean.Primitive"
  ,"Zelata" ,"Belit..Savage." ,"Balthus.Slasher" ,"Ageera" ,"Belit..Brom." ,"Belit..Xavier.","Akivasha"
  ,"Mummies"
  ,"Skeletons", "Crows", "Bone.Golem", "Swamp.Demon", "Earth.Demon", "Forest.Demon", "Grey.Man.Ape"
  ,"Pelias", "Warlock", "Natohk", "Khemsa"
  ,"Scorpion.Broodmother"
  ,"Atali", "Niord", "Kerim.Shah", "Kothian.Archer"
  ,"Gitara", "Valkyrie", "Winged.One"
  # 104 Scenarios opened

  #,"Crossbowmen"
  # 106 Scenarios opened (+1) (10 models)

  # Remaining critters, into Nordheim (3 models)
  #,"Giants" ,"Sabertooth.Tiger"
  # 109 Scenarios opened (+3)

  #,"Kushite.Witch.Hunters"
  # 111 Scenarios opened (+2)  (4 models)

  # Armoured (King Complete)
  #,"Black.Dragons"
  # 112 Scenarios opened (+1)  (5 Models)

  #, "Dragon"
  # 113 (+1) (1 Model)

  #,"Black.Ones"
  # 114 Scenarios opened (+1)  (10 models)

  # Stygia leaders
  #,"Ikhmet" ,"Shentu" ,"Thoth.Amon"
  # 115 Scenarios opened (+1) (3 models)

  # Stygian complete (10 models)
  #,"Eternal.Guard","Assassins"
  # 121 Scenarios opened (+5)

  # Finish Nordheim (15 models)
  #,"Aesir.Warriors" ,"Vanir.Warriors"
  # 126 Scenarios opened (+5)

  # Khitai (22 models)
  #,"Shuang.Mian" ,"Javelin.Throwers"
  #,"Honor.Guards" ,"Khitan.Guards"
  #,"Foo.Dogs"

)]

cat("\n\nFrom What I have painted!\n")

print_order(paint_order(y))
