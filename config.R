all_packages <- c("data.table",
                  'lubridate',
                  'forecast',
                  'tseries',
                  'magrittr',
                  "ggplot2",
                  "gridExtra",
                  "plotly")


new_packages <-  setdiff(all_packages,rownames(installed.packages()))

if(length(new_packages)) install.packages(new_packages)


lapply(all_packages,require,character.only=TRUE)