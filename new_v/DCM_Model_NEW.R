---
title: "Discrete Choice Experiment - Resistant Grape Varieties"
output: html_document
date: "`r Sys.Date()`"
---

```{r packages, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(ExpertChoice)
library(AlgDesign)
library(DoE.base)
library(DoE.MIParray)
library(readxl)
library(idefix)

```

```{r dev model, include=FALSE}

#Step 0
attri4321 <- list(
  price = c("1", "2", "3"),
  grape = c("1", "2", "3"),
  seal = c("1", "2"),
  label = c("1", "2", "3"))

# Step 1
ff_piwi <- full_factorial(attri4321)

# Step 2
aff_piwi <- augment_levels(ff_piwi)

nlevels <- unlist(purrr::map(ff_piwi, function(x){length(levels(x))}))
write.csv(ff_piwi, "dcm_piwi.csv")

# Step 3
nlevels <- unlist(purrr::map(ff_piwi, function(x){length(levels(x))}))
oa_feasible(24, nlevels, strength = 2)
fractional_factorial_4321_18 <- oa.design(nlevels = nlevels, columns = "min34")

# Step 4
# Confirming that this is an efficient design.
colnames(fractional_factorial_4321_18) <- colnames(ff_piwi)
fractional_factorial_4321_18 <- search_design(ff_piwi, fractional_factorial_4321_18)

# Step 5
# This table is reported as Table
# Confirm that this design supports all interactions.
row1_main_effects <- fractional_factorial_efficiency(~ price + grape + seal + label, fractional_factorial_4321_18)

# Step 6
piwi_choice_set <- modulo_method(
  fractional_factorial_4321_18, list(c(2, 1, 3, 2), c(1, 2, 1, 3))) 
class(piwi_choice_set) <- c(class(piwi_choice_set), "piwi_choice_set")

# Step 7
# This experiment uses categorical data (not ordinal) hence there can be no pareto dominate solution.
# Each category is merely a choice.
checking_overshadow <- check_overshadow(piwi_choice_set)

# Step 8
dce_efficency_menu_piwi <- dce_efficiency(aff_piwi, piwi_choice_set)

# Step 9
# Construct the question table
piwi_question_table <- construct_question_frame(aff_piwi, piwi_choice_set)

# Finally augment the question table
levels(piwi_question_table$price) <- c("3,99€", "6,99€", "9,99€")
levels(piwi_question_table$grape) <- c("Cabernet Blanc", "Chardonnay", "Cuvée")
levels(piwi_question_table$seal) <- c("Yes", "No")
levels(piwi_question_table$label) <- c("LowBudget", "MedBudget", "PremBudget")

# Convert question table to matrix

# Spalte "levels" aufsplitten in 4 Spalten
# Spalten entsprechend der Variablen benennen
piwi_question_table$price <- substr(piwi_question_table$levels,1,1)
piwi_question_table$grape <- substr(piwi_question_table$levels,2,2)
piwi_question_table$seal <- substr(piwi_question_table$levels,3,3)
piwi_question_table$label <- substr(piwi_question_table$levels,4,4)

# dummy 1: price
w <- model.matrix( ~price-1, piwi_question_table) # dummies in separater Matrix erstellen
piwi_question_table <- data.frame(piwi_question_table, w) # Matrix mit Datensatz verbinden

# dummy 2: grape
w <- model.matrix( ~grape-1, piwi_question_table)
piwi_question_table <- data.frame(piwi_question_table, w)

# dummy 3: seal
w <- model.matrix( ~seal-1, piwi_question_table)
piwi_question_table <- data.frame(piwi_question_table, w)

# dummy 4: label
w <- model.matrix( ~label-1, piwi_question_table)
piwi_question_table <- data.frame(piwi_question_table, w)

# optional: einzelne spalten price, grape, seal und info wieder löschen
piwi_question_table$price  <- NULL
piwi_question_table$grape   <- NULL
piwi_question_table$seal <- NULL
piwi_question_table$label  <- NULL

#Tabelle anpassen

#Schritt 1 - question und choice in eine spalte
row.names(piwi_question_table) <- paste(piwi_question_table$question, piwi_question_table$choice, sep=".")

# #Schritt 3 - no choice Option einfügen
# row_no_choice <- model.matrix( ~nochoice, piwi_question_table)
# piwi_question_table_no_choice <- data.frame(piwi_question_table_less_rows, row_no_choice)

#Schritt 4 - deleting rows
piwi_question_table_less_rows <- piwi_question_table
piwi_question_table_less_rows [, c('question', 'choice', 'levels')] <- list(NULL)

# #Schritt 5 - From csv to matrix
# Matrix_NoChoice_mod <- read_excel("Matrix_NoChoice_mod.xlsx",
# col_types = c("text", "numeric", "numeric",
# "numeric", "numeric", "numeric",
# "numeric", "numeric", "numeric",
# "numeric", "numeric", "numeric",
# "numeric"))
# Matrix_NoChoice_mod = as.matrix(Matrix_NoChoice_mod)
# vectordata=as.vector(Matrix_NoChoice_mod)
```

```{python dev stimuli, eval=FALSE, include=FALSE}

```

# Darstellung 

```{r model, echo=FALSE}
piwi_question_table
```

```{r stimuli design, echo=FALSE}

```

```{r optional: questionnaire, eval=FALSE, include=FALSE}
# Modelling questionnaire

Matrix_NoChoice_mod <- piwi_question_table_less_rows

xdes <- Matrix_NoChoice_mod
xdes

n.sets <- 18
alternatives <- c("Produkt A", "Produkt B", "Produkt C", "No")
attributes <- c("price", "grape", "seal", "label")
labels <- vector(mode = "list", length(attributes))
labels[[1]] <- c("3,99€", "6,99€", "9,99€")
labels[[2]] <- c("Cabernet Blanc", "Chardonnay", "Cuvée")
labels[[3]] <- c("Yes", "No")
labels[[4]] <- c("LowBudget", "MidBudget", "HighBudget")
at.lvls <- c(3, 3, 2, 3)
code <- c("D", "D", "D", "D")
b.text <- "Bitte wählen Sie eine der Ihnen hier gezeigten Optionen aus."
i.text <- "Liebe:r Teilnehmer:in, im Rahmen des Projekts VitiFIT untersuchen wir verschiedene Einflussfaktoren auf das Weinkonsumverhalten.
            Im Folgenden werden Ihnen dazu diverse Fragestellungen gezeigt, die Sie bitte so gut und spontan wie möglich beantworten sollen. Die Befragung sollte möglichst an einem Bildschirm von mindestens 13 Zoll durchgeführt werden. Ansonsten bitte an die Produktbilder heran zoomen.
            Im ersten Schritt werden Ihnen verschiedene Produktkombinationen gezeigt. Wir würden Sie gerne darum bitten, zu jeder Auswahlmöglichkeit eine Kaufentscheidung zu treffen.
            Anschließend bitten wir Sie noch weitere Hintergrund- sowie personenspezifische Fragen zu beantworten.
            Die Umfrage und damit Ihre Angaben werden natürlich anonym behandelt. Die Dauer der Umfrage bemisst sich auf ca. 15 Minuten. 
            Wir freuen uns auf Ihre Antworten.
            Vielen Dank im Voraus.
            Ihr Geisenheimer Marktforschungsteam"
e.text <- "Vielen Dank für Ihre Teilnahme! Wir möchten uns ganz herzlich für Ihre Mithilfe bedanken.
            Ihre Antworten wurden gespeichert, Sie können das Browser-Fenster nun schließen."
if (interactive()) {
  SurveyApp (des = xdes, n.total = n.sets, alts = alternatives,
             atts = attributes, lvl.names = labels, coding = code, alt.cte = c(0,0,0,1), no.choice = 4,
             buttons.text = b.text, intro.text = i.text, end.text = e.text,
             data.dir = '/Users/kieferch/Nextcloud/4_Paper No.3_Neuro/4_DCM/1_Versuchsaufbau/1_Modell/Data')
}
```

# Ergebnisse

```{r Auswertung, eval=FALSE, include=FALSE}
#Auswertung

#piwi.data <- aggregate_design 
# des <- as.matrix(piwi.data[, 3:8], ncol = 6)
# des

# y <- piwi.data[, 9]
# y

# Datatrans(pkg = "bayesm", des = des, y = y, 
# n.alts = 2, n.sets = 11, n.resp = 7, bin = TRUE)
## Only the first participant is shown in order to reduce the space the output would take 

# Datatrans(pkg = "mlogit", des = des, y = y,
# n.alts = 2, n.sets = 11, n.resp = 7, bin = TRUE)
## Some rows are exluded in order to reduce the space the output would take 
```
