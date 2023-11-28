---
title: 'Measuring Rural Stress: Code Appendix'
author: "Jean Velez"
date: "2023-11-28"
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
	echo = TRUE,
	message = FALSE,
	warning = FALSE
)
```

# Import Lavaan
```{r}
library(lavaan)
library(httr)
library(readxl)
```

# Import Dataset
```{r}
httr::GET("https://query.data.world/s/fif6xttgj4kvir4ixvnlnxdmlqhiuw?dws=00000", write_disk(tf <- tempfile(fileext = ".xlsx")))
data <- readxl::read_excel(tf)
```

# Factorize Likert-type items
```{r}
for (i in 2:5){
data[,i] = factor(dplyr::pull(data[,i]),ordered = TRUE)
}
```


# Build structual and measurement equations
```{r}
m <- '
  # latent variable - measurement model
  overallstress =~ stress + stress_finance + stress_home + stress_employment
  
  # structural path - structural model
  overallstress ~ rural + hispanic + income_higher + income_lower
  
  #correlated error
  stress~~stress_home
'
```


# Create SEM object
```{r}
m.fit <- lavaan::sem(m, data = data)
```


# Output summary of SEM
```{r}
lavaan::summary(m.fit, standardized = T, fit.measures = T)
```
