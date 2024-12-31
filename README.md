The live and interactive version of this document is avaliable at https://africa-adaptation-atlas.shinyapps.io/eia_climate_prioritization/

This interactive Markdown document is designed to support the prioritization of climate adaptation investments at the regional level for CGIAR science programs. It accompanies a publication and provides an interactive interface for exploring the intersection of climate hazards and agricultural exposure. The primary goal is to aid decision-makers in identifying and prioritizing regions and crops most vulnerable to climate risks.

In addition to its specific application for CGIAR programs, this codebase is built to be fully repeatable and customizable, enabling adaptation for other purposes or regions. By incorporating flexible input datasets and scalable hazard combinations, the tool is suited to a wide range of agricultural and environmental risk assessments.

The tool empowers users to dynamically adjust parameters, such as the selection of crops, regions, or hazard thresholds, providing a customizable framework for climate risk analysis.

If you wish to build this tool from the source code, rather than using the hosted version, you will need to follow these steps:

1.  Clone this repository to your local machine.
2.  Install [Quarto](https://quarto.org/) if not already installed.
3.  Install the R dependenies for the .qmd file. This can be done manually, or by using a tool such as pacman. 
```r
if(!require("pacman", character.only = TRUE)){install.packages("pacman",dependencies = T)}

required.packages <- c("arrow", "colourpicker", "data.table", "DT", "dplyr", "ggplot2", "ggpubr", "MetBrewer", "reshape", "shiny", "viridis", "wesanderson")

pacman::p_load(char=required.packages,install = T,character.only = T)

if(require("waffle")==F){ devtools::install_github("hrbrmstr/waffle") require("waffle") } 
```

 4. The scripts in the `R` folder do not need to be run for the tool to work, as all necessary data is included. However, they can be modified or used as a reference for further customization.
 5. Serve the .qmd file using Quarto. This can be done by running the following command in the terminal:
```shell
quarto serve eia_climate_prioritization.qmd
```

![abc](https://github.com/user-attachments/assets/f2960fe9-715e-480f-945a-f7548988ad8f)

![eia](https://github.com/user-attachments/assets/8f714333-1506-4d4a-8179-7b374b8b6f39)

![cgiar](https://github.com/user-attachments/assets/c5dc44e4-7cd1-4d16-9904-e959fb044d1f)