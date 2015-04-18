#### Header ####
# File: twitter_boost.R
# Author: Ian Johnston
# Modified: 19APR2015
# Purpose: Fit my proposed mixture model to the 
#          Twitter data using an EM algorithm
#### Numerically precise logit related functions ####
logLowL = -20
logUppL = 20
logepsilon = log(5^(-304))
l = function(l1,l2)
{
  mi = min(l1, l2); ma = max(l1, l2)
  if (mi == -Inf || mi - ma < logepsilon) return(ma)
  return(ma + log(1 + exp(mi - ma)))
}
lse = function(x) Reduce(l, x)
invlogit = function(x)
{
  y = x
  ind = which(x < logUppL)
  if(length(ind)==0)
  {
    y = rep(1,length(x))
  }else
  {
    y[ind] = exp(x[ind])/(1+exp(x[ind]))
    y[-ind] = 1
  }
  return(y)
}
logit <- function(x)
{
  y <- x
  ind0 <- which(y==0)
  ind1 <- which(y==1)
  indx <- intersect(which(y > 0), which(y < 1))
  if(length(ind0) > 0) {
    y[ind0] <- logLowL
  }
  if(length(ind1) > 0) {
    y[ind1] <- logUppL
  }  
  if(length(indx) > 0) {
    y[indx] <- log(y[indx]) - log(1-y[indx])
  }
  return(y)
}
#### EM algorithm for the model ####
EStep <- function(X, MU, TAU2, PI) {
  K <- length(TAU2)
  ALPHA <- matrix(0, nrow = nrow(X), ncol = K)
  for(j in 1:K) {
    ALPHA[, j] <- -0.5 * log(2 * pi * TAU2[j]) - 
      0.5 * rowSums(sweep(X, 
                          2, 
                          MU[j, ], 
                          "-") ^ 2) / TAU2[j] + 
      log(PI[j])
  }
  Z <- apply(ALPHA, 1, lse)
  ALPHA <- sweep(ALPHA, 1, Z, "-")
  return(exp(ALPHA))
}
MStep <- function(X,
                  ALPHA, 
                  MU.prior, 
                  TAU2,
                  TAU2.prior) {
  K <- ncol(ALPHA)
  MU <- matrix(0, nrow = K, ncol = 2)
  for(j in 1:K) {
    NUM <- MU.prior[j, ] / TAU2.prior[j] + 
      colSums(ALPHA[,j] * X) / TAU2[j]
    DENOM <- 1 / TAU2.prior[j] + sum(ALPHA[,j]) / TAU2[j]
    MU[j, ] <- NUM / DENOM
  }
  return(MU)
}
EMAlgorithm <- function(X,
                        TAU2,
                        TAU2.prior,
                        MU,
                        MU.prior,
                        PI,
                        MAXITER = 20,
                        TOL = 1e-4,
                        print = FALSE) {
  EMITER <- 1
  EMNORM <- 1e4
  ALPHA <- NULL
  while((EMITER < MAXITER) & (EMNORM > TOL)) {
    CMU <- MU
    ALPHA <- EStep(X, CMU, TAU2, PI)
    MU <- MStep(X, 
                ALPHA, 
                MU.prior, 
                TAU2,
                TAU2.prior)
    EMNORM <- sqrt(sum((MU - CMU) ^ 2))
    if(print) {
      print(paste0("Iteration #", 
                   EMITER, 
                   ": NORM = ",
                   round(EMNORM, 4)))
    }
    EMITER <- EMITER + 1
  }
  return(list(MU = MU,
              ALPHA = ALPHA))
}
#### Analysis ####
DATA <- read.csv("full_dataset_24MAR2015.csv",
                 sep = ",",
                 header = TRUE)
SIMLIST <- c("happy", "halloween", "beer", "music", "lunch")
# a little clean up
TCOUNTS <- table(DATA$token)
SCOUNTS <- sort(TCOUNTS, decreasing = TRUE)
BADTKNS <- c("", "http", "rt", "i", "is", "are", "i'm",
             "was", "it's", "don't", "https", "via", "u",
             "can't", "de", "2", "why", "y", "10", "5",
             "4", "1", "3", "2", "6", "7", "8", "9")
DATA <- DATA[-which(DATA$token %in% BADTKNS), ]
TCOUNTS <- table(DATA$token)
SCOUNTS <- sort(TCOUNTS, decreasing = TRUE)
# Define Bounding Box for Cities
NNGPS <- which(!is.na(DATA[,5]))
SCOORDS <- apply(DATA[NNGPS, 5:6], 2, as.numeric)
CITIES <- vector("list", 3) # change if adding more cities
CITIES[[1]]$NAME <- "boston"
CITIES[[1]]$BOX <- c(-71.2, -71.0, 42.2, 42.5)
CITIES[[2]]$NAME <- "nyc"
CITIES[[2]]$BOX <- c(-74.1, -73.8, 40.6, 40.9)
CITIES[[3]]$NAME <- "philly"
CITIES[[3]]$BOX <- c(-75.3, -74.0, 39.8, 40.1)
tweetInBoundingBox <- function(LONLAT, BOX) {
  if((LONLAT[1] > BOX[1]) & (LONLAT[1] < BOX[2]) & 
       (LONLAT[2] > BOX[3]) & (LONLAT[2] < BOX[4])) {
    return(TRUE)
  } else {
    return(FALSE)
  }  
}
MakePI <- function(TOKENS,
                   W,
                   REL = NULL,
                   K = 1,
                   XI0 = -8,
                   XI1 = 0) {  
  if(is.null(REL)) {
    REL <- rep(1, length(W))
  }
  BOOST <- sum((W %in% TOKENS) * REL)
  MBOOST <- max(BOOST)
  if(MBOOST > 0) {
    BOOST <- BOOST / MBOOST
  }
  PI.all <- invlogit(XI0 + XI1 * BOOST)
  PI <- rep(0, K + 1)
  PI[1] <- 1 - PI.all
  PI[2:(K + 1)] <- PI.all / K
  return(PI)
}
fast.bincombinations <- function(p)
  vapply(X = seq_len(p),
         FUN = function(i)rep(rep(0:1, each = 2^(p-i)), times = 2^(i-1)),
         FUN.VALUE = integer(2^(p)))

BCOMBS <- fast.bincombinations(5)
#### city specific files
FSUFFIX <- "_tweet_final.csv"
for(CITY in 1:3) {
  print(paste0("Putting together files for ", CITIES[[CITY]]$NAME, "..."))
  CITYIND <- apply(SCOORDS,
                   1,
                   tweetInBoundingBox,
                   BOX = CITIES[[CITY]]$BOX)
  TOKENS <- apply(DATA[NNGPS[CITYIND], 1:2], 2, as.character)
  UID <- unique(TOKENS[,1])
  GIND <- which(!duplicated(TOKENS[,1]))
  SCOORDS2 <- apply(DATA[NNGPS[CITYIND][GIND], 5:6], 2, as.numeric)
  BOSTWEETS <- SCOORDS2[apply(SCOORDS2,
                              1,
                              tweetInBoundingBox,
                              BOX = CITIES[[CITY]]$BOX),]
  NTWEETS <- nrow(BOSTWEETS)
  KEYWORD_LIST <- vector("list", NTWEETS)
  X <- matrix(0, nrow = NTWEETS, ncol = length(SIMLIST))  
  for(i in 1:NTWEETS) {
    KEYWORD_LIST[[i]] <- TOKENS[TOKENS[,1] == UID[i], 2]
    X[i ,] <- 1*(SIMLIST %in% KEYWORD_LIST[[i]])
  }
  colnames(X) <- SIMLIST
  FDATA <- cbind(BOSTWEETS, X)
  # For each possible number of components, for all word choices
  # generate the prior probabilities of each component, PI, 
  # and generate the initial locations of the centroids, MU
  tally <- 1
  for(K in 2:20) {
    for(WC in 1:32) {
      print(paste0("[", round(tally / 608 * 100, 2), "%]..."))
      MU.prior <- matrix(0, nrow = K, ncol = 2)
      MU.prior[1, ] <- colMeans(BOSTWEETS)
      PI <- matrix(0, nrow = nrow(BOSTWEETS), ncol = K)
      PI[, 1] <- 1 - invlogit(-3)
      if(WC > 1) {
        PI[, 1] <- 1 - invlogit(-3 + 1.5 * rowMeans(X[, which(BCOMBS[WC,] > 0), drop = FALSE]))
        PI[, -1] <- (1 - PI[, 1, drop = FALSE]) / K
      } else {
        PI[, 1] <- 1 - invlogit(-3)
        PI[, -1] <- (1 - PI[, 1, drop = FALSE]) / K
      }
      THRESHOLD <- 1 - invlogit(-3)
      PIND <- which(PI[, 1] < THRESHOLD)
      if(length(PIND) <= K) {
        PIND <- 1:nrow(BOSTWEETS)
      }
      FIT <- kmeans(BOSTWEETS[PIND, ], K - 1) # K cluster solution
      CENTERS <- aggregate(BOSTWEETS[PIND, ],
                           by = list(FIT$cluster),
                           FUN = mean)
      MU.prior[-1, ] <- as.matrix(CENTERS[, -1])          
      MVAR <- max(diag(cov(BOSTWEETS)))
      TAU2 <- c(MVAR, rep(MVAR / 100, K - 1))
      TAU2.prior <- c(MVAR / 9, rep(MVAR / 9, K - 1))
      MU <- cbind(runif(K, CITIES[[CITY]]$BOX[1], CITIES[[CITY]]$BOX[2]),
                  runif(K, CITIES[[CITY]]$BOX[3], CITIES[[CITY]]$BOX[4]))
      TEMP <- EMAlgorithm(BOSTWEETS,
                          TAU2,
                          TAU2.prior,
                          MU,
                          MU.prior,
                          PI,
                          MAXITER = 20,
                          TOL = 1e-4,
                          print = FALSE)
      FSUFFIX <- paste0("_k_",K,"_wc_",WC - 1, "_fitted.csv")
      write.csv(x = TEMP$MU,
                file = paste0("results/",CITIES[[CITY]]$NAME, FSUFFIX),
                row.names = FALSE)
      tally <- tally + 1
    }
  }  
}