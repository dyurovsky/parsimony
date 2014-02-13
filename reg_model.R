###############################################################################
############################## SET UP PARAMETERS ##############################
###############################################################################
source('~/Projects/Other/Ranalysis/useful.R')

P.DIMS = 10;
NUM.EXPS = 2;

CP.CORR <- .7
YP.CORR <- .7


NREP <- 100



seen.ps <- NULL
for(i in 1:NREP){
  p.betas <- runif(P.DIMS,0,1)
  
  ps <- rnorm(NUM.EXPS * P.DIMS, 
              mean=rnorm(NUM.EXPS * P.DIMS, 0,10), sd = .05)
  c.noise <- rnorm(NUM.EXPS * P.DIMS, mean=0, sd = .05)
  y.noise <- rnorm(NUM.EXPS * P.DIMS, mean=0, sd = .05)
  cs <- CP.CORR*ps + sqrt(1-CP.CORR^2)*c.noise + 1
  ys <- YP.CORR*ps + sqrt(1-YP.CORR^2)*y.noise +1
  ps <- ps + 1
  
  d <- data.frame(c.value=cs, p.value=ps, 
                  p.dim=rep(seq(1,P.DIMS), each=NUM.EXPS),y=ys)
    
  exps <- dcast(d,formula = c.value+y~p.dim,value.var="p.value",fill=0)
  
  names(exps) <- c("C","y",paste("P",names(exps)[c(-1,-2)],sep=""))
  
  eqstr <- paste(" y ~ ", paste(names(exps[-c(1,2)]),"+",collapse=" "), 
                 " C", sep="")
  
  l <- lm( as.formula(eqstr), data=exps)
  
  seen.ps <- c(seen.ps, coef(summary(l))[P.DIMS+2,4])
}
hist(seen.ps)



form <- as.formula(paste("outcomes ~ 0 + ", form, "C"))


c.sub.preds <- glm(form, data = c.sub.exps)
r.sub.preds <- glm(form, data = r.sub.exps)
