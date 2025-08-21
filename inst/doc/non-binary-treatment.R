## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(propertee)

## ----eval = FALSE-------------------------------------------------------------
# dose > 200 ~ dose <= 200

## ----eval = FALSE-------------------------------------------------------------
# dose > 200 ~ .
# . ~ dose <= 200

## ----eval = FALSE-------------------------------------------------------------
# dose >= 300 ~ dose <= 100

## -----------------------------------------------------------------------------
data(simdata)
table(simdata$dose)
spec1 <- rct_spec(dose ~ uoa(uoa1, uoa2), data = simdata)
summary(spec1)

## -----------------------------------------------------------------------------
head(ate(spec1, data = simdata, dichotomy = dose >= 300 ~ dose <= 100))

## -----------------------------------------------------------------------------
head(assigned(spec1, data = simdata, dichotomy = dose >= 300 ~ dose <= 100))

