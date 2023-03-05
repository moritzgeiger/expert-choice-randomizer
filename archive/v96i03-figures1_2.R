### replication script for Figures 1 and 2
### Generating Efficient Designs for Discrete Choice Experiments in R: The idefix Package 

set.seed(123)

library("truncnorm")
library("idefix")
library("grDevices")

sets <- 20
nlev <- 5
N <- 250

# levels of the continuous attributes
levels_time <- c(30, 36, 42, 48, 54)
levels_price <- c(1, 4, 7, 10, 13)

#Uninformative; B_time = 0; VOT = $0/h
pd <-  cbind(rnorm(n = N, mean = 0, sd = 1), -1)

# sem-informative; B_time = 0; VOT = $0/h (but positive)
pd2 <- cbind(rtruncnorm(n = N, a = -Inf, b = 0, mean = 0, sd = 1), -1)

# Informative; B_time = -0.33; VOT = $20/h 
pd3 <- cbind(rnorm(n = N, mean = -0.33, sd = 0.1), -1)

# Informative; B_time = [-1.3; -0.16] ; VOT = $9.6/h - $78/h 
pd4 <- cbind(runif(n = N, min = -1.3, max = -0.16),-1)

### generate designs
design.object <- CEA(lvls = c(nlev, nlev), coding = c("C", "C"), 
                     c.lvls = list(levels_time, levels_price),
                     n.sets = sets, n.alts = 2, parallel = TRUE, 
                     par.draws = pd, best = TRUE)
des <- design.object$design


design.object2 <- CEA(lvls = c(nlev, nlev), coding = c("C", "C"), 
                      c.lvls = list(levels_time, levels_price),
                      n.sets = sets, n.alts = 2, parallel = TRUE, par.draws = pd2, best = TRUE)
des2 <- design.object2$design


design.object3 <- CEA( lvls = c(nlev, nlev), coding = c("C", "C"), 
                       c.lvls = list(levels_time, levels_price),
                       n.sets = sets, n.alts = 2, parallel = TRUE, par.draws = pd3, best = TRUE)
des3 <- design.object3$design


design.object4 <- CEA( lvls = c(nlev, nlev), coding = c("C", "C"), 
                       c.lvls = list(levels_time, levels_price),
                       n.sets = sets, n.alts = 2, parallel = TRUE, par.draws = pd4, best = TRUE)
des4 <- design.object4$design


fun <- function (x, des){DBerr(par.draws = t(x), des = des, n.alts = 2)}

# range of possible true VOT's
mat <- cbind(seq(-1.667, 0, 0.08333333), -1)

dberr11 <- rev(apply(mat, 1, fun, des))
dberr12 <- rev(apply(mat, 1, fun, des2))
dberr13 <- rev(apply(mat, 1, fun, des3))
dberr14 <- rev(apply(mat, 1, fun, des4))



### plot 1 = Figure 1 ################################
plot(1:nrow(mat), dberr11[1:nrow(mat)], ylim =c(0,0.3), xaxt="n",
     ylab ="D-error", xlab = "True VOT ($/hr)")
lines(1:nrow(mat), dberr12[1:nrow(mat)], lty = 2) #semi
lines(1:nrow(mat), dberr13[1:nrow(mat)], lty = 1) #inf1


xlab = rev(as.character(round(-mat[, 1]* 60), digits = 2))
axis(side = 1, at = 1:nrow(mat), labels = xlab)

mycol <- adjustcolor("grey", alpha.f = 0.2)
panel.first <- rect(c(0, 7), -1e6, c(3, 22), 1e6, col = mycol)
abline(v = 3)
abline(v = 7)

legend <- c("naive prior", "semi-informative prior", "informative prior")
legend(13.5, 0.3, legend = legend, lty = c(0, 2, 1), pch = c(1, NA, NA))


### plot 2 = Figure 2 #######################################
plot(1: nrow(mat), dberr11[1:nrow(mat)], ylim = c(0, 0.3), xaxt = "n",
     ylab = "D-error", xlab = "True VOT ($/hr)")
lines(1:nrow(mat), dberr12[1:nrow(mat)], lty = 2) # semi
lines(1:nrow(mat), dberr14[1:nrow(mat)], lty = 1) # inf2

axis(side = 1, at = 1:nrow(mat), labels = xlab)

legend <- c("naive prior", "semi-informative prior", "informative prior")
legend(8, 0.3, legend = legend, lty = c(0, 2, 1), pch = c(1, NA, NA))

panel.first <- rect(c(0, 17), -1e6, c(3, 22), 1e6, col = mycol)
abline(v = 3)
abline(v = 17)
