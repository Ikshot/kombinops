## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----example, echo=TRUE--------------------------------------------------
## Solution with kombinops
library(kombinops)
res <- (Ank_(4,6)-
       Cnk(4,3)*Ank_(3,6)+
       Cnk(4,2)*Ank_(2,6)-
       Cnk(4,1)*Ank_(1,6))/
       Ank_(4,6)
print(res)

