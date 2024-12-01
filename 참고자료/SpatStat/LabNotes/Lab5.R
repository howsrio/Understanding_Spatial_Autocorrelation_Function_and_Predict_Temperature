#' ---
#' title: "Lab 5. Modeling and Analysis for Point Pattern"
#' author: "Spatial Big Data Analysis with GIS (jaehongjeong@hanyang.ac.kr)"
#' date: "Korean Statistical Society, Winter School, February 24, 2023"
#' 
#' output:
#'   html_document:
#'     number_sections: yes
#'     toc: yes
#'   pdf_document:
#'     toc: yes
#'   fonttheme: professionalfonts
#' ---
#' 
#' This document is based on "Applied Spatial Data Analysis with R" by Bivand, Pebesma, and Gomez-Rubio.
#' 
## ----setup, include=FALSE----------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, out.width = "75%", fig.align = "center", error = FALSE, warning = FALSE)
library(spatstat)
library(spatstat.explore)
library(maptools)

#' 
## ----eval = FALSE------------------------------------------------------------------------------------------------
## # Required packages
## library(spatstat)
## library(spatstat.explore)
## library(maptools)

#' 
#' # Modeling and Analysis for Point Pattern using ${\tt spatstat}$
#' 
#' ## Inspecting data
#' 
#' We will use one of the standard pattern datasets that is installed with the package. The `Swedish Pines' dataset represent the positions of 71 trees in a forest plot 9.6 by 10.0 meters.
#' 
## ----out.width="90%"---------------------------------------------------------------------------------------------
#library(spatstat)
#library(spatstat.explore)
#library(maptools)
data(swedishpines) 
X <- swedishpines
plot(X)

# Study the intensity (density of points) in this point pattern.
summary(X)

plot(density(X, sigma=10)) # 10 is the chosen value for the standard deviation of the Gaussian smoothing kernel.
contour(density(X, 10), axes=FALSE)

#' 
#' The contours are labeled in density units of ``trees per square decimeter".
#' 
#' ## Exploratory data analysis
#' 
#' The ${\tt spatstat}$ is designed to support all the standard types of EDA for point patterns. One example is ${\it quadrat counting}$. The study region is divided into rectangles of equal size, and the number of points in each rectangle is counted.
#' 
## ----------------------------------------------------------------------------------------------------------------
Q <- quadratcount(X, nx=4, ny=3)
Q
plot(X)
plot(Q, add=TRUE, cex=2)
K <- Kest(X)
plot(K)

#' 
#' Another example is ${\it Ripley's\ K\ function}$.
#' 
#' ## Multitype point patterns
#' 
#' A marked point pattern in which the marks are a categorical variable is usually called a ${\it multitype}$ point pattern. The `types' are the different values or levels of the mark variable.
#' Here is the famous Lansing Woods dataset recording the positions of 2251 trees of 6 different
#' species (hickories, maples, red oaks, white oaks, black oaks and miscellaneous trees).
#' 
## ----------------------------------------------------------------------------------------------------------------
data(lansing)
lansing
summary(lansing)
plot(lansing)

#' 
#' In this plot, each type of point (i.e,. each species of tree) is represented by a different plot
#' symbol. The last line of output above explains the encoding: black oak is coded as symbol 1
#' (open circle) and so on.
#' 
## ----------------------------------------------------------------------------------------------------------------
plot(split(lansing))
hick <- split(lansing)$hickory
plot(hick)

#' 
#' ## Classes in ${\tt spatstat}$
#' 
#' To handle point pattern datasets and related data, the ${\tt spatstat}$ package defines the following classes of objects:
#' 
#' - ${\tt ppp}$: planar point pattern
#' 
#' - ${\tt owin}$: spatial region (observation window)
#' 
#' - ${\tt im}$: pixel image
#' 
#' - ${\tt psp}$: pattern of line segments
#' 
## ----------------------------------------------------------------------------------------------------------------
#methods(plot)
methods(class = "ppp")

#' 
#' # Manipulating point pattern - example
#' 
#' We will use one of the standard point pattern datasets that is installed with the package. The
#' NZ trees dataset represent the positions of 86 trees in a forest plot 153 by 95 feet.
#' 
## ----------------------------------------------------------------------------------------------------------------
data(nztrees)
nztrees
plot(nztrees)
contour(density(nztrees, 10), axes=FALSE)

#' 
#' The density surface has a steep slope at the top right-hand corner of the study region.
#' Looking at the plot of the point pattern itself, we can see a cluster of trees at the top right.
#' 
#' You may also notice a line of trees at the right-hand edge of the study region. It looks
#' as though the study region may have included some trees that were planted as a boundary or
#' avenue. This sticks out like a sore thumb if we plot the x coordinates of the trees:
#' 
## ----------------------------------------------------------------------------------------------------------------
hist(nztrees$x, nclass=25)

#' 
#' We might want to exclude the right-hand boundary from the study region, to focus on the
#' pattern of the remaining trees. Let’s say we decide to trim a 5-foot margin off the right-hand
#' side.
#' 
#' First we create the new, trimmed study region:
#' 
## ----------------------------------------------------------------------------------------------------------------
chopped <- owin(c(0, 148), c(0, 95))
#or
win <- nztrees$window
chopped <- trim.rectangle(win, xmargin=c(0, 5), ymargin=0)
chopped
nzchop <- nztrees[chopped]

#' 
#' We can now study the `chopped' point pattern.
#' 
## ----------------------------------------------------------------------------------------------------------------
summary(nzchop)
plot(density(nzchop, 10))
plot(nzchop, add=TRUE)

#' 
#' # Tests of complete spatial randomness
#' 
#' In classical literature, the homogeneous Poisson process (Complete Spatial Randomness, CSR) is usually taken as the appropriate `null' model for a point pattern. Our basic task in analyzing a point pattern is to find evidence against CSR.
#' 
#' ## Quadrat counting tests for CSR
#' 
## ----------------------------------------------------------------------------------------------------------------
#quadrat.test(nzchop, nx=3, ny=2)
M <- quadrat.test(nzchop, nx=3, ny=2)
M

plot(nzchop)
plot(M, add=TRUE, cex=2)

# The null hypothesis: data pattern is a realization of CSR
M$p.value

#' 
#' Typically a more powerful test of CSR is the Kolmogorov-Smirnov test in which we compare the observed and expected distributions of the values of some function $T$.
#' 
#' # Maximum likelihood for Poisson process
#' 
#' The ${\it inhomogeneous}$ Poisson process can be simulated using ${\tt rpoispp}$.
#' 
## ----------------------------------------------------------------------------------------------------------------
lambda <- function(x, y) {
 100 * (x + y)
}
plot(rpoispp(lambda))

#' 
#' ## Model-fitting function
#' 
## ----------------------------------------------------------------------------------------------------------------
data(bei)
plot(bei)

# fit the homogeneous Poisson model
ppm(bei, ~1) 

# fit the inhomogeneous Poisson model with an intensity that is log-linear in the cartesian coordinates
ppm(bei, ~x + y) 

# fit the inhomogeneous Poisson model with an intensity that is log-quadratic in cartesian coordinates
ppm(bei, ~polynom(x, y, 2)) 

#' 
#' It is possible to fit an inhomogeneous Poisson process model with an intensity function that depends on an observed covariate.
#' 
## ----------------------------------------------------------------------------------------------------------------
grad <- bei.extra$grad
plot(grad)

#' 
#' To fit the inhomogeneous Poisson model with intensity which is a loglinear function of slope, i.e., $\lambda(u)=\exp(\beta_{0}+\beta_{1}Z(u))$ where $\beta_{0},\beta_{1}$ are parameters and $Z(u)$ is the slope at location $u$.
#' 
## ----------------------------------------------------------------------------------------------------------------
ppm(bei, ~slope, covariates=list(slope=grad))

#' 
#' Also, we can fit the inhomogeneous Poisson model with intensity that is ${\it proportional}$ to slope, $\lambda(u)=\beta Z(u)$.
#' 
## ----------------------------------------------------------------------------------------------------------------
ppm(bei, ~offset(log(slope)), covariates=list(slope=grad))

#' 
#' ## Fitted models
#' 
## ----------------------------------------------------------------------------------------------------------------
fit <- ppm(bei, ~x + y)
fit

plot(fit, how="image")

predict(fit, type="trend")

predict(fit, type="cif", ngrid=256)

coef(fit)
vcov(fit)
sqrt(diag(vcov(fit)))
round(vcov(fit, what="corr"), 2)

#' 
#' ## Model selection
#' 
## ----------------------------------------------------------------------------------------------------------------
fit <- ppm(bei, ~slope, covariates=list(slope=grad))
fitnull <- update(fit, ~1)
anova(fitnull, fit, test = "Chi")

fitprop <- ppm(bei, ~offset(log(slope)), covariates=list(slope=grad))
fitnull <- ppm(bei, ~1)
c(AIC(fitprop),BIC(fitprop))
c(AIC(fitnull),BIC(fitnull))

#' 
#' ## Simulating the fitted model
#' 
#' A fitted Poisson model can be simulated automatically using the function ${\tt rmh}$.
#' 
## ----------------------------------------------------------------------------------------------------------------
X <- rmh(fitprop)
plot(X, main = "realisation of fitted model")

#' 
#' # Distance methods for point patterns
#' 
#' Suppose that a point pattern appears to have constant intensity, and we wish to assess whether
#' the pattern is Poisson. The alternative is that the points are dependent (they exhibit interaction).
#' Classical writers suggested a simple trichotomy between independence (the Poisson process), regularity (where points tend to avoid each other), and clustering (where points tend to be
#' close together). 
#' 
#' ## Empty space function $F$
#' 
## ----------------------------------------------------------------------------------------------------------------
data(cells)
emp <- distmap(cells)
plot(emp, main="Empty space distances")
plot(cells, add=TRUE)

#' 
## ----------------------------------------------------------------------------------------------------------------
data(cells)
plot(cells)
Fc <- Fest(cells)
Fc

par(pty = "s")
plot(Fest(cells))
plot(Fest(cells), hazard ~ r, main="Hazard rate of F")

#' 
#' ## Nearest neighbour distances and $G$ function
## ----------------------------------------------------------------------------------------------------------------
Gc <- Gest(cells)
Gc

par(pty = "s")
plot(Gest(cells))

fisher <- function(x) {
 asin(sqrt(x))
}
plot(Gest(cells), fisher(.) ~ fisher(theo))

#' 
#' ## Pairwise distances and the $K$ function
#' 
## ----------------------------------------------------------------------------------------------------------------
Gc <- Kest(cells)
Gc

par(pty="s")
plot(Kest(cells))

#' 
## ----------------------------------------------------------------------------------------------------------------
L <- Lest(cells)
plot(L, main="L function")


#' 
#' ## $J$ function
#' 
#' A useful combination of $F$ and $G$ is the $J$ function, $J(r)=\frac{1-G(r)}{1-F(r)}$. For a homogeneous Poisson process, $F=G$, so that $J_{pois}(r)=1$. Value $J(r)>1$ suggest regularity and $J(r)<1$ suggest clustering.
#' 
## ----------------------------------------------------------------------------------------------------------------
plot(allstats(cells))

