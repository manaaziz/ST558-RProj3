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
  
The code for installing and loading all the packages and running the app:
```
# Install/Load the packages
pkgs <- c("shiny", "shinyjs", "tidyverse", 
          "caret", "plotly", "DT", 
          "ggfortify", "MASS", "rpart.plot", 
          "rpart", "class")
install.packages(pkgs)
lapply(pkgs, library, character.only = TRUE)

# Run the app from GitHub
shiny::runGitHub("ST558-RProj3", "manaaziz", ref = "master", subdir = "master/finalApp/")
```

The code for running the app:
```

```