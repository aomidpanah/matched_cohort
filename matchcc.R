## calculate the coverage of the 90% CI for the beta-1 term (OR of response for unit diff in X)
cover <- function(fit) {
  ci <- confint.default(fit, 0.9)[2, ]
  b1 > ci[1] & b1 < ci[2] 
}

set.seed(123)
n <- 10000
w <- rep(c(0,0,0,1), times=n/4)  ## it helps for w to be rare to justify matching
x <- seq(-3, 3, length.out = n) + w

b0 <- -2
b1 <- 0.3
b2 <- -1

i.mcohort <- c(sample(which(w==1), 1000), sample(which(w==0), 1000)) ## subset for matched cohort 

do.one <- function() {
  
  ## generate outcome 
  y <- rbinom(n, 1, plogis(b0 + b1 * x +b2*w))
  
  ## do outcome-dependent matching
  ncc <- min(table(y))
  i.cc <- c(sample(which(y==0), ncc), sample(which(y==1), ncc))
  
  wcc <- w[i.cc]
  xcc <- x[i.cc]
  ycc <- y[i.cc]
  
  nmcc <- min(table(wcc))
  i.mcc <- c(sample(which(wcc==0), nmcc), sample(which(wcc==1, nmcc)))
  
  ## do models
  mod <- list(fall.crude = glm(y ~ x, family=binomial),
              fall.adj = glm(y ~ x + w, family=binomial),
              
              fmcoh.crude = glm(y ~ x, family=binomial, subset=i.mcohort),
              fmcoh.adj = glm(y ~ x+w, family=binomial, subset=i.mcohort),
              
              fcc.crude = glm(ycc ~ xcc, family=binomial),
              fcc.adj = glm(ycc ~ xcc + wcc, family=binomial),
              
              fmcc.crude = glm(ycc ~ xcc, family=binomial, subset=i.mcc),
              fmcc.adj = glm(ycc ~ xcc + wcc, family=binomial, subset=i.mcc)
  )
  
  c('nall' = n, 'nmcoh' = 200, 'ncc' = length(i.cc), 'nmcc' = length(i.mcc), 
    se = sapply(sapply(mod, vcov), `[`, i=2, j=2)^.5,
    cv = sapply(mod, cover)
    )
}
  
  
out <- replicate(n = 1000, do.one())
out <- rowMeans(out)

cat(sprintf('Average Sample Size for:
        
        Cohort: %d 
        Matched Cohort: %d 
        Case Control: %d 
        Matched Case Control %d ', 
        as.integer(out[1]), as.integer(out[2]), as.integer(out[3]), as.integer(out[4])))

cat(sprintf('Average standard error:
        
        Crude Cohort: %1.3f
        Adjusted Cohort: %1.3f
        Matched Crude Cohort: %1.3f
        Matched Adjusted Cohort: %1.3f
        Crude Case Control: %1.3f
        Adjusted Case Control: %1.3f
        Matched Crude Case Control %1.3f
        Matched Adjusted Case Control %1.3f ', 
            out[5], out[6], out[7], out[8],
            out[9], out[10], out[11], out[12]
            ))

cat(sprintf('Average 90 percent CI coverage:
        
        Crude Cohort: %1.3f
        Adjusted Cohort: %1.3f
        Matched Crude Cohort: %1.3f
        Matched Adjusted Cohort: %1.3f
        Crude Case Control: %1.3f
        Adjusted Case Control: %1.3f
        Matched Crude Case Control %1.3f
        Matched Adjusted Case Control %1.3f ', 
            out[13], out[14], out[15], out[16],
            out[17], out[18], out[19], out[20]
))
