# ST558-RProj3

Packages required for this app:
  * `shiny`: (obviously)
  * `shinyjs`: to toggle hidden on/of
  * `tidyverse`: used for literally everything
  * `caret`: for training models
  * `plotly`: for cool, interactive plots
  * `DT`: for cool, interactive data tables
  * `ggfortify`: for a cool biplot
  * `MASS`: to help with 3D plots
  * `rpart.plot`: to upgrade the tree plot
  * `rpart`: to fit classification trees
  * `class`: to fit knn models
  
The code for installing all the packages:
```
pkgs <- c("shiny", "shinyjs", "tidyverse", 
          "tools", "caret", "plotly", "DT", 
          "ggfortify", "MASS", "rpart.plot", 
          "rpart", "class")
install.packages(pkgs)
lapply(x, library, character.only = TRUE)
```

The code for running the app:
```
shiny::runGitHub("ST558-RProj3", "manaaziz", ref = "master", subdir = "master/finalApp/")
```