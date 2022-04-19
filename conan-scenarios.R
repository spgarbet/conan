x <- read.csv("conan-scenarios.csv", stringsAsFactors=FALSE, skip=1)

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
  
  ,"Crossbowmen" 
  # 106 Scenarios opened (+1) (10 models)
  
  # Remaining critters, into Nordheim (3 models)
  ,"Giants" ,"Sabertooth.Tiger"
  # 109 Scenarios opened (+3)
  
    
  ,"Kushite.Witch.Hunters"  
  # 111 Scenarios opened (+2)  (4 models)

  # Armoured (King Complete)
  ,"Black.Dragons"
  # 112 Scenarios opened (+1)  (5 Models)
  
  , "Dragon"
  # 113 (+1) (1 Model)
  
  ,"Black.Ones"
  # 114 Scenarios opened (+1)  (10 models)

  # Stygia leaders
  ,"Ikhmet" ,"Shentu" ,"Thoth.Amon"
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
doit(y)

  