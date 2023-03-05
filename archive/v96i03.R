### replication script for all Figures except 1 & 2
### Generating Efficient Designs for Discrete Choice Experiments in R: The idefix Package 

## install.packages('idefix')
library("idefix")

at.lvls <- c(3, 3, 2, 3)
c.type <- c("D", "D", "D", "D")
Profiles(lvls = at.lvls, coding = c.type)

code <- c("D", "D", "D", "D")
cs <- Profiles(lvls = c(3, 3, 2, 3), coding = code)
mu <- c(-0,4, -1, 2, 3)
sigma <- diag(length(mu))
set.seed(123)
M <- MASS::mvrnorm(n = 30, mu = mu, Sigma = sigma)
D <- Modfed(cand.set = cs, n.sets = 8, n.alts = 2, alt.cte = c(1, 0), par.draws = M)
D

lvls <- list(
  c("3,99€", "6,99€", "9,99€"), 
  c("Cabernet Blanc", "Chardonnay", "Cuvée"), 
  c("Emotional", "Technisch", "Non") 
)
DD <- Decode(des = D$design, n.alts = 2, lvl.names = lvls, coding = code)
DD

set.seed(123)
code <- c("E", "E", "E")
cs <- Profiles(lvls = c(4, 2, 3), coding = code)
alt.cte <- c(1, 0, 1)
m <- c(0.1, 1.5, 1.2, 0.8, -0.5, 1, -1.5, 0.6)
v <- diag(length(m))
ps <- MASS::mvrnorm(n = 500, mu = m, Sigma = v)
ps <- list(ps[, 1:2], ps[, 3:8])
D.nc <- Modfed(cand.set = cs, n.sets = 10, n.alts = 3, 
               alt.cte = alt.cte, par.draws = ps, no.choice = TRUE, best = FALSE)
for (i in 1:length(D.nc)) print(D.nc[[i]]$error)

test <- Decode(des = D.nc[[1]]$design, n.alts = 3, lvl.names = lvls,
               alt.cte = alt.cte, coding = code, no.choice = 3)
cbind(test$design, probs = as.vector(t(D.nc[[1]]$probs)))

set.seed(123)
lvls <- c(4, 2, 3)
coding <- c("E", "E", "E")
alt.cte <- c(1, 0, 1)
m <- c(0.1, 1.5, 1.2, 0.8, -0.5, 1, -1.5, 0.6)
v <- diag(length(m))
ps <- MASS::mvrnorm(n = 500, mu = m, Sigma = v)
ps <- list(ps[, 1:2], ps[, 3:8])
D.nc_cea <- CEA(lvls = lvls, coding = coding, n.alts = 3, n.sets = 10,
                alt.cte = alt.cte, par.draws = ps, no.choice = TRUE, 
                best = TRUE)
D.nc_cea

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
D_I <- CEA(lvls = c(5, 5), coding = c("C", "C"), 
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
alternatives <- c("Alt A", "Alt B")
attributes <- c("Price", "Time", "Comfort")
labels <- vector(mode = "list", length(attributes))
labels[[1]] <- c("$10", "$5", "$1")
labels[[2]] <- c("20 min", "12 min", "3 min")
labels[[3]] <- c("bad", "average", "good")
code <- c("D", "D", "D")
b.text <- "Please choose the alternative you prefer"
i.text <- "Welcome, here are some instructions ... good luck!"
e.text <- "Thanks for taking the survey"
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
