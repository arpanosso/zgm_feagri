library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)

# Dados
data("selfesteem", package = "datarium")
head(selfesteem, 3)


# 1. Compute group differences
grp.diff <- selfesteem %>%
   transmute(
      `t1-t2` = t1 - t2,
      `t1-t3` = t1 - t3,
      `t2-t3` = t2 - t3
   )
head(grp.diff, 3)
grp.diff  %>% map(var)

selfesteem <- selfesteem %>%
   gather(key = "time", value = "score", t1, t2, t3) %>%
   convert_as_factor(id, time)
head(selfesteem, 3)

res <- anova_test(data = selfesteem, dv = score, wid = id, within = time)
res

res$`Sphericity Corrections`

# correction = "auto"
get_anova_table(res)

# correction = "GG"
get_anova_table(res, correction = "GG")


