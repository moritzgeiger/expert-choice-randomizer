library("idefix")

at.lvls <- c(3, 3, 2, 3)
c.type <- c("D", "D", "D", "D")
con.lvls <- list(c(4, 6, 8, 10))
Profiles(lvls = at.lvls, coding = c.type)

code <- c("D", "D", "D")
cs <- Profiles(lvls = c(3, 3, 3), coding = code)
mu <- c(0.8, 0.2, -0.3, -0.2, 0.7, 0.4)
v <- diag(length(mu)) # Prior variance.
sd <- list(example_design)
set.seed(123)
ps <- MASS::mvrnorm(n = 10, mu = mu, Sigma = v) # 10 draws.
CEA(lvls = c(3, 3, 3), coding = c("D", "D", "D"), par.draws = ps,
    n.alts = 2, n.sets = 8, parallel = FALSE, start.des = sd)


lvls <- list(
  c("3,99€", "6,99€", "9.99€"), 
  c("Cabernet Blanc", "Chardonnay", "Cuvée"), 
  c("Technisch", "Emotional", "Non") 
)
DD <- Decode(des = D$design, n.alts = 2, lvl.names = lvls, coding = code)
DD

library("truncnorm")
set.seed(123)
N <- 500
N <- 250
U <- rnorm(n = N, mean = 0, sd = 1)
S <- rtruncnorm(n = N, a = -Inf, b = 0, mean = 0, sd = 1)
I <- rnorm(n = N, mean = -0.33, sd = 0.1)
I2 <- runif(n = N, min= -1.3, max = -0.16)

I <- cbind(I, -1)
lev_time <- c(30, 36, 42, 48, 54)
lev_price <- c(1, 4, 7, 10, 13)
D_I <- CEA(lvls = c(3, 3, 3), coding = c("D", "D", "D"), 
           c.lvls = list(lev_time, lev_price),
           n.sets = 20, n.alts = 2, parallel = TRUE, 
           par.draws = I, best = TRUE)
des <- D_I$design
range <- cbind(seq(-1.667, 0, 0.08333), -1)
I_robust <- DBerr(par.draws = range, des = des, n.alts = 2, 
                  mean = FALSE)
I_robust

set.seed(123)
cs <- Profiles(lvls = c(4, 3, 2), coding = c("D", "D", "D"))
m <- c(0.25, 0.5, 1, -0.5, -1, 0.5)
v <- diag(length(m))
ps <- MASS::mvrnorm(n = 500, mu = m, Sigma = v)
init.des <- Modfed(cand.set = cs, n.sets = 8, n.alts = 2, alt.cte = c(0, 0), par.draws = ps)$design
init.des

truePREF <- c(0.5, 1, 2, -1, -1, 1.5)
set.seed(123)
y.sim <- RespondMNL(par = truePREF, des = init.des, n.alts = 2)
y.sim

set.seed(123)
draws <- ImpsampMNL(n.draws = 200, prior.mean =  m, prior.covar = v, des = init.des, n.alts = 2, y = y.sim)
draws

set <- SeqMOD(des = init.des, cand.set = cs, n.alts = 2, par.draws = draws$sample, prior.covar = v, weights =  draws$weights)
set

data("example_design", package = "idefix")
xdes <- example_design
xdes

n.sets <- 8
alternatives <- c("Produkt Y", "Produkt X")
attributes <- c("Preis", "Rebsorte", "Informationen")
labels <- vector(mode = "list", length(attributes))
labels[[1]] <- c("3,99€", "6,99€", "9,99€")
labels[[2]] <- c("Cabernet Blanc", "Chardonnay", "Cuvée")
labels[[3]] <- c("Emotonal", "Technisch", "Koine")
code <- c("D", "D", "D")
b.text <- "Bitte wählen Sie die von Ihnen bevorzugte Alternative"
i.text <- "Hallo Hier sit der Fragebogen, Danke."
e.text <- "merci"
if (interactive()) {
  SurveyApp (des = xdes, n.total = n.sets, alts = alternatives,
             atts = attributes, lvl.names = labels, coding = code,
             buttons.text = b.text, intro.text = i.text, end.text = e.text,
             data.dir = getwd())
}
## screenshot taken to show application 
## The two files shown are the ones that are stored when the survey application in previous example is completed. 

data("nochoice_design", package = "idefix") 
ncdes <- nochoice_design
ncdes

alternatives <- c("Alternative A", "Alternative B", "None")
if (interactive()) {
  SurveyApp(des = ncdes, n.total = n.sets, alts = alternatives,
            atts = attributes, lvl.names = labels, coding = code,
            alt.cte = c(0, 0, 1), no.choice = 3,
            buttons.text = b.text, intro.text = i.text, end.text = e.text,
            data.dir = NULL)
}
## screenshot of application is shown. 

n.sets <- 12
p.mean <- c(0.3, 0.7, 0.3, 0.7, 0.3, 0.7)
p.var <- diag(length(p.mean))
levels <- c(3, 3, 3)
cand <- Profiles(lvls = levels, coding = code)
alternatives <- c("Alternative A", "Alternative B")
if (interactive()) {
  SurveyApp(des = xdes, n.total = n.sets, alts = alternatives,
            atts = attributes, lvl.names = labels, coding = code,
            buttons.text = b.text, intro.text = i.text,
            end.text = e.text, data.dir = getwd(),
            prior.mean = p.mean, prior.covar = p.var,
            cand.set = cand, n.draws = 100)
}
## textfile such as adapdata.txt will be stored in working directory

## total_num.txt
## Loaddata (not reproducable without generating files in directory first)
## dataDir <- getwd()
## data <- LoadData(data.dir = dataDir, type = "num")
## data
idefix.data <- aggregate_design 
des <- as.matrix(idefix.data[, 3:8], ncol = 6)
des

y <- idefix.data[, 9]
y

Datatrans(pkg = "bayesm", des = des, y = y, 
          n.alts = 2, n.sets = 8, n.resp = 7, bin = TRUE)
## Only the first participant is shown in order to reduce the space the output would take 

Datatrans(pkg = "mlogit", des = des, y = y,
          n.alts = 2, n.sets = 8, n.resp = 7, bin = TRUE)
## Some rows are exluded in order to reduce the space the output would take 
