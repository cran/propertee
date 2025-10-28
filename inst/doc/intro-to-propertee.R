## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
#devtools::load_all("~/repositories/_r/propertee/")
library(propertee)

## -----------------------------------------------------------------------------
data("STARplus")
table(STARplus$cond_at_entry)

## -----------------------------------------------------------------------------
STARplus$cond_small <- STARplus$cond_at_entry == "small"
table(STARplus$cond_small)

## -----------------------------------------------------------------------------
summary(STARplus$g1treadss)

## -----------------------------------------------------------------------------
length(unique(STARplus$school_at_entry))
head(table(STARplus$school_at_entry))

## -----------------------------------------------------------------------------
length(unique(STARplus$stdntid))
head(STARplus$stdntid)

## -----------------------------------------------------------------------------
spec <- obs_spec(cond_small ~ unit_of_assignment(stdntid) + block(school_at_entry),
                  data = STARplus, na.fail = FALSE)
summary(spec)

## -----------------------------------------------------------------------------
te <- lmitt(g1treadss ~ 1, data = STARplus, specification = spec)
summary(te)

## -----------------------------------------------------------------------------
te_s <- lmitt(g1treadss ~ race, data = STARplus, specification = spec)
summary(te_s)

## -----------------------------------------------------------------------------
lmitt(g1treadss ~ 1, data = STARplus, specification = spec, weights = "ate")
lmitt(g1treadss ~ 1, data = STARplus, specification = spec, weights = "ett")

## -----------------------------------------------------------------------------
head(ate(spec, data = STARplus))

## -----------------------------------------------------------------------------
camod <- lm(g1treadss ~ gender * dob + race, data = STARplus)

## -----------------------------------------------------------------------------
lmitt(g1treadss ~ 1, data = STARplus, specification = spec,
      weights = "ate", offset = cov_adj(camod))

## -----------------------------------------------------------------------------
head(cov_adj(camod, newdata = STARplus))

## -----------------------------------------------------------------------------
lm(g1treadss ~ cond_small, data = STARplus, weights = ate(spec),
   offset = cov_adj(camod))

## -----------------------------------------------------------------------------
lmitt(g1treadss ~ 1, data = STARplus, specification = spec, absorb = TRUE)

