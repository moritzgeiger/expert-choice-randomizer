library(ExpertChoice)
library(AlgDesign)
library(DoE.base)
library(DoE.MIParray)

#Step 0
attri3321 <- list(
  price = c("1", "2", "3"),
  grape = c("1", "2", "3"),
  siegel = c("1", "2"),
  information = c("1", "2", "3")
)
# Step 1
ff_piwi <- full_factorial(attri3321)

# Step 2
aff_piwi <- augment_levels(ff_piwi)

#Applying B mat
write.csv(ff_piwi, "dcm_piwi.csv")

# Step 3
nlevels <- unlist(purrr::map(ff_piwi, function(x){length(levels(x))}))
oa_feasible(36, nlevels, strength = 2)
fractional_factorial_3321_18 <- oa.design(nlevels = nlevels, columns = "min34")

# Step 4
# Confirming that this is an efficient design.
colnames(fractional_factorial_3321_18) <- colnames(ff_piwi)
fractional_factorial_3321_18 <- search_design(ff_piwi, fractional_factorial_3321_18)

# Step 5.
# This table is reported as Table
# Confirm that this design supports all interactions.
row1_main_effects <- fractional_factorial_efficiency(~ price + grape + siegel + information, fractional_factorial_3321_18)

# Step 6 & 7.
# From fractional factorial design to DCM
  #> ÄNDERN 3321
piwi_choice_set <- modulo_method(
fractional_factorial_3321_18, list(c(2, 1, 1, 3), c(1, 2, 3, 1)))
class(piwi_choice_set) <- c(class(piwi_choice_set), "piwi_choice_set")

# Step 8
dce_efficency_piwi <- dce_efficiency(aff_piwi, piwi_choice_set)

# Step 9
# Construct the question table
piwi_question_table <- construct_question_frame(aff_piwi, piwi_choice_set)

# Finally augment the question table. See Table 1 in the Theoretical Vignette.
levels(piwi_question_table$price) <- c("3,99€", "6,99€", "9,99€")
levels(piwi_question_table$grape) <- c("Cabernet Blanc", "Chardonnay", "Cuvée")
levels(piwi_question_table$siegel) <- c("Ja", "Nein")
levels(piwi_question_table$information) <- c("Emotional", "Technisch", "Non")
View(piwi_question_table)