#' Title
#'
#' @param ydata 
#' @param lags 
#' @param xdata 
#' @param const 
#' @param breaks 
#'
#' @return
#' @export
#'
#' @examples
rfvar <-  function(ydata=NA, lags=2, xdata=NULL, const=TRUE, breaks=NULL){
  if(is.null(dim(ydata))) dim(ydata) <- c(length(ydata),1)
  T <- dim(ydata)[1]
  nvar <- dim(ydata)[2]
  if(const){
    xdata <- cbind(xdata,matrix(1,T,1))
  }
  nox <- identical(xdata,NULL)
  if(!nox){
    T2 <- dim(xdata)[1]
    nx <- dim(xdata)[2]
  }
  else{
    T2 <- T
    nx <- 0
    xdata <- matrix(0,T2,0)
  }
  #
  if(!identical(T2,T)){
    print('xdata and ydata must be same length.')
    return()
  }
  if(identical(breaks,NULL))
    nbreaks <- 0
  else
    nbreaks<-length(breaks)
  breaks <- c(0,breaks,T)
  if(any(breaks[2:length(breaks)] <= breaks[1:(length(breaks)-1)]))
    stop("list of breaks must be in strictly increasing order\n")
  if(breaks[2]>lags)
    smpl <- (lags+1):breaks[2]
  else
    smpl <- NULL
  if(nbreaks>0){
    for (nb in 2:(nbreaks+1))
      smpl <- c(smpl,(breaks[nb]+lags+1):breaks[nb+1])
  }
  Tsmpl <- length(smpl)
  X <- array(0,dim=c(Tsmpl,nvar,lags))
  for(ix in seq(along=smpl))
    X[ix,,] <- t(ydata[smpl[ix]-(1:lags),,drop=FALSE])
  dim(X) <- c(Tsmpl,nvar*lags)
  X <- cbind(X, xdata[smpl,,drop=FALSE]) # rhs data for model
  y <- ydata[smpl,,drop=FALSE] # lhs data for model
  vldvr <- svd(X)
  di <- 1./vldvr$d
  dfx <- sum(vldvr$d > 100*.Machine$double.eps)
  di <- di[1:dfx]
  vldvr$u <- vldvr$u[, 1:dfx]
  vldvr$v <- vldvr$v[, 1:dfx]
  snglty <- dim(X)[2] - dfx
  B <- vldvr$v %*% (di * (t(vldvr$u) %*% y))
  u <-  y-X %*% B
  if (!is.null(tsp(ydata))) u <- ts(u, start=start(ydata)+c(0,lags),frequency=frequency(ydata))
  nX <- dim(X)[2]
  xx <-  di * t(vldvr$v)
  xx <-  crossprod(xx)
  By <-  t(B) # fix this only plus 1 for const
  # dim(By) <-  c(nvar,lags,nvar)       # variables, lags, equations
  # By <-  aperm(By,c(3,1,2)) #equations, variables, lags to match impulsdt.m
  if(!is.null(dimnames(ydata)[2]))
  {
    ynames <- dimnames(ydata)[[2]]
  }else
  {
    ynames <- rep("",times=nvar)
  }
  if(!nox)
  {
    if(!is.null(dimnames(xdata)[2]))
    {
      xnames <- dimnames(xdata)[[2]]
      xxnames <- c(paste(rep(ynames,each=lags),1:lags,sep=""), xnames)
      # dimnames(xx) <- list(xxnames,xxnames)
      dimnames(By) <- list(ynames, c(paste(ynames,rep(1:lags, each=length(ynames)), sep="")))
    }else
    {
      xnames <- rep("",times=nx)
      xxnames <- c(paste(rep(ynames,each=lags),1:lags,sep=""))
      # dimnames(xx) <- list(xxnames,xxnames)
      dimnames(By) <- list(ynames, c(paste(ynames,rep(1:lags, each=length(ynames)), sep=""),"const"))
    }
  }
  if (nox)
    Bx <-  NULL
  else
  {
    Bx <-  matrix(B[nvar*lags+(1:nx),],dim(B)[2],nx)
    dimnames(Bx) <- list(ynames,xnames)
  }
  return(list(By=By, Bx=Bx, u=u, xx = xx, singular=snglty, X=X))
}

#' Title
#'
#' @param Bh 
#' @param swish 
#' @param nn 
#'
#' @return
#' @export
#'
#' @examples
fn.impulse <- function(Bh,swish,nn){
  nvar <- nn[1]
  lags <- nn[2]
  imstep <- nn[3]
  #
  ll <- lags + 1
  n1 <- nvar + 1
  nl <- nvar*lags
  #
  Ah <- t(Bh)
  #
  imf <- matrix(nrow=imstep, ncol=nvar*nvar)
  #
  M <- matrix(nrow=nvar*imstep, ncol=nvar)
  #
  M[1:nvar,] <- t(swish)
  Mtem <- M[1:nvar,]
  #
  imf[1,] <- t(as.vector(Mtem))
  #
  ims2 <- imstep - 1
  ims1 <- min(c(ims2, lags))
  t <- 1
  while(t <=ims1){
    nt <- nvar*t
    ntt <- nvar*(t+1)
    tt <- t+1
    Mtem <- Ah[,1:nt] %*% M[1:nt,]
    M[n1:ntt,] <- M[1:nt,]
    M[1:nvar,] <- Mtem
    imf[tt,] <- t(as.vector(Mtem))
    t <- t+1
  }
  #
  for(t in ll:ims2){
    nt <- nvar*t
    ntt <- nvar*(t+1)
    tt <- t+1
    Mtem <- Ah[,1:nl] %*% M[1:nl,]
    M[n1:ntt,] <- M[1:nt,]
    M[1:nvar,] <- Mtem
    imf[tt,] <- t(as.vector(Mtem))
  }
  return(imf)
}

#' Generates a random vector from a uniform distribution in a sphere centered at
#' the origin
#'
#' @param n Dimension of the random vector to be generated
#' @param d Radius of the sphere
#'
#' @return
#' @export
#'
#' @examples
rballunif <- function(n, d) {
  ## generate a point uniformly in the n-dimensional ball
  ## centered at the origin and having radius `d'
  # cat(n, d, "\n")
  # n=8
  # d=1
  x <- rnorm(n)
  d * runif(1)^(1/n) * x / c(sqrt(crossprod(x)))
}

#' Identifies stuctural shocks using Uhlig's (2005) rejection method and
#' estimates a Bayesian vector autoregression model with a flat Normal
#' inverted-Wishart prior.
#'
#' @param Y A ts object containing the data series used for estimation; this
#'   should be of size T x nvar.
#' @param nlags The number of lags to include of each variable. The default
#'   value is 4.
#' @param draws An integer value for the number of Markov Chain Monte Carlo
#'   (MCMC) sampling replications. The default value is 200.
#' @param subdraws An integer value for the number of sub-draws over the
#'   rejection routine. The default value is 200.
#' @param nkeep An integer value for the number of desired MCMC draws that meet
#'   the imposed sign restrictions. The default value is 1000.
#' @param KMIN An integer value for the first period of the impulse responses to
#'   which the sign restrictions apply. The default value is 1.
#' @param KMAX An integer value for the last period of the impulse responses to
#'   which the sign restrictions apply. The default value is 4.
#' @param constrained A list of sign restrictions of length <= nvar. The first
#'   entry of the list characterises the shock of interest. You MUST specify a
#'   sign restriction for the shock of interest.
#' @param constant A logical statement on whether to include an intercept in the
#'   model. The default is 'TRUE'.
#' @param steps An integer value for the horizon of the impulse response
#'   calculations. The default value is 24.
#'
#' @return
#' @export
#'
#' @examples
uhlig.reject <- function(Y=NULL,  nlags=4, draws=200, subdraws=200, nkeep=1000, KMIN=1, KMAX=4, constrained=NULL, constant=TRUE, steps=24){
  #
  #---SANITY CHECK ---#
  sanity.check.reject(Y=Y, nlags=nlags, draws=draws, subdraws=subdraws, nkeep=nkeep, KMIN=KMIN, KMAX=KMAX, constrained=constrained, constant=constant, steps=steps)
  #
  #--- SET UP PARAS ---#
  varnames <- colnames(Y)
  n1 <- draws
  n2 <- subdraws
  nstep <- steps
  nlags <- nlags
  nvar <- ncol(Y)
  nobs <- nrow(Y)
  nnobs0 <- nlags + 1
  nnobs <- nobs - nlags
  nnvar0 <- nvar + 1
  ntot <- n1*n2
  #
  if(constant == FALSE){
    CONS <- "F"
    ncoef <- nvar * nlags
    nncoef <- nvar * nlags
    nnvar1 <- nvar * (nlags + 1)
  }else{
    CONS <- "T"
    ncoef <- nvar * (nlags+1)
    nncoef <- nvar * nlags + 1
    nnvar1 <- nvar * (nlags + 1) + 1
  }
  #
  #---REDUCED FORM VAR MODEL ---#
  model <- rfvar(ydata=Y, lags=nlags, const=CONS)
  bcoef <- model$By # same order as above but w/const and nvar x nvar x lags
  resid <- model$u # same as above
  data <- model$X
  xx <- model$xx
  #
  #--- SIGMA and SXX ---#
  uu <- crossprod(resid)
  # sigma <- (1/(nnobs-nncoef))*uu
  sigma <- (1/nnobs)*uu
  #
  #--- SET UP MCMC OF VAR ---#
  sxx <-  chol(xx)
  sv <- solve(uu)
  svt <-  chol(sv)
  betaols <- t(bcoef)
  best <- betaols
  wishdof <- nnobs-nncoef
  #
  #--- MATRICES FOR DRAWS ---#
  goodresp <- array(NA, c(nkeep, nstep, nvar))
  BDraws <- array(NA, c(n1, nncoef, nvar))
  SDraws <- array(NA, c(n1, nvar, nvar))
  imp <- matrix(NA, nrow=nstep, ncol=nvar)
  fevd <- matrix(NA, nrow=nstep, ncol=nvar)
  goodfevd <- array(NA, c(nkeep, nstep, nvar))
  goodshock <- array(NA, c(nkeep, nnobs))
  uhatt <- matrix(NA, nnobs, 1)
  #
  #--- Monte CARLO INTEGRATION ---#
  accept <- 0
  message('Starting MCMC, ', date(),'.', sep="")
  pb0 <- txtProgressBar(min = 0, max = n1, style = 3)
  for(draws in 1:n1){
    setTxtProgressBar(pb0, draws)
    #
    #--- sigma draws ---#
    sigmad  <- solve(matrix(rWishart(1, wishdof, sv), nrow=nvar, ncol=nvar))
    swish   <- chol(sigmad)
    #
    #--- beta draws ---#
    swsxx <-   sigmad  %x% xx
    bd <- rep(0, nrow(swsxx))
    #betau <- matrix(mvrnormR(1,0,swsxx), nrow=nncoef, ncol=nvar)
    betau <- matrix(mvnfast::rmvn(1, bd, swsxx), nrow=nncoef, ncol=nvar)
    betadraw <- betaols + betau
    bhat <- betadraw
    #
    #--- irfs ---#
    imfhat <- fn.impulse(bhat, swish, c(nvar, nlags, nstep))
    impulses <-  array(imfhat, dim=c(nstep,nvar,nvar))
    imp2 <- impulses^2
    imp2sum <- apply(imp2, c(2,3), cumsum)
    mse <-  apply(imp2sum, c(1,2), sum)
    fevd0 <- array(apply(imp2sum, 3, "/",  mse), dim=c(nstep, nvar, nvar))
    #
    for(subdraws in 1:n2){
      a <- matrix(rballunif(nvar,1), nvar, 1)
      UAR <- UhligAccept(a,KMIN,KMAX,constrained, impulses)
      UA <- UAR$acc
      q <- UAR$Q
      if(UA==1){
        for(j in 1:nstep){ # this can be done via apply
          imp[j,] <- t(impulses[j,,]%*%q)
          fevd[j,] <- t(fevd0[j,,]%*%(q^2))
        }
        accept <- accept+1
        goodresp[accept, ,] <-  imp
        goodfevd[accept, ,] <- fevd * 100
        BDraws[draws, , ] <- betadraw
        SDraws[draws, , ] <- sigmad
        uhat <-   Y[nnobs0:nobs ,] - data %*% bhat
        for(i in 1:nnobs){
          uhatt[i,] <-   uhat[i, ] %*%  (  solve(swish) %*% q)
        }
        goodshock[accept, ] <-  t(uhatt)
      }else{
        next
      }
      #
      if(accept>=nkeep){
        break
      }
      #
    } # end subdraws
    if(accept>=nkeep){
      break
    }
    ldraw <- draws
  }#end draws
  close(pb0)
  #
  #--- FIX PARA MATRICES ---#
  if(ldraw<n1){
    BDraws <- BDraws[1:ldraw, , ]
    SDraws <- SDraws[1:ldraw, , ]
    dimnames(SDraws) <- list(1:ldraw, varnames, varnames)
  }
  #
  #--- WARNING MESSAGE IN CASE OF TOO FEW DRAWS ---#
  if(accept<nkeep){
    if(accept==0){
      stop("\n Not enough accepted draws to proceed!")
    }else{
      goodresp <- goodresp[1:accept, , ]
      goodfevd <- goodfevd[1:accept, , ]
      goodshock <- goodshock[1:accept, ]
      message('\n Warning! Had only ', accept,' accepted draw(s) out of ',ntot,'.', sep="")
    }
  }
  nn1 <- accept
  dimnames(goodresp) <- list(1:nn1, 1:nstep, varnames)
  dimnames(goodfevd) <- list(1:nn1, 1:nstep, varnames)
  #
  if(constant == FALSE){
    dimnames(BDraws) <-  list(1:ldraw, c(paste(varnames,rep(1:nlags, each=length(varnames)), sep="")) , varnames)}else{
      dimnames(BDraws) <- list(1:ldraw, c(paste(varnames,rep(1:nlags, each=length(varnames)), sep=""),"const"), varnames)
    }
  #
  message('\n MCMC finished, ', date(),'.', sep="")
  return(list(IRFS=goodresp, FEVDS = goodfevd,  SHOCKS = goodshock, BDraws=BDraws, SDraws=SDraws))
}

#' Title
#'
#' @param Y 
#' @param nlags 
#' @param draws 
#' @param subdraws 
#' @param nkeep 
#' @param KMIN 
#' @param KMAX 
#' @param constrained 
#' @param constant 
#' @param steps 
#'
#' @return
#' @export
#'
#' @examples
sanity.check.reject <- function(Y=Y, nlags=nlags, draws=draws, subdraws=subdraws, nkeep=nkeep, KMIN=KMIN, KMAX=KMAX, constrained=constrained, constant=constant, steps=steps){
    
    Yts <-is.ts(Y)
    Ydf <- is.data.frame(Y)
    
    if(Yts==FALSE & Ydf==FALSE){
      
      message(" ")
      stop(" Data has to be a data.frame() or ts() object.\n", call. = FALSE)
    }
    
    if(ncol(Y)<2){
      
      message(" ")
      stop(" Need more than 1 variable.\n", call. = FALSE)
    }
    
    Yna <- any(is.na(Y)==TRUE)
    Ynan <- any(is.nan(Y)==TRUE)
    Ynum <-  is.numeric(Y)
    
    if(Yna==TRUE | Ynan==TRUE){
      
      message(" ")
      stop(" Data must not contain missing values.\n", call. = FALSE)
    }
    
    if(Ynum!=TRUE){
      
      message(" ")
      stop(" Data must not contain strings.\n", call. = FALSE)
    }
    
    nlagsint <- nlags%%1==0
    nlagsobs <- nrow(Y)-nlags
    
    if(nlagsint==FALSE){
      
      message(" ")
      stop("Number of lags must be integer.\n", call. = FALSE)
    }
    
    if(nlagsobs<=0){
      
      message(" ")
      stop(" Number of lags cannot be larger than the number of observations.\n", call. = FALSE)
    }
    
    if(nlags<1){
      
      message(" ")
      stop(" Need at least 1 lag.\n")
    }
    if(nlags>nrow(Y)){
      
      message(" ")
      stop(" Number of lags have to be smaller than number of observations.\n", call. = FALSE)
    }
    
    drawsint <- draws%%1==0
    
    if(drawsint!=TRUE){
      
      message(" ")
      stop(" Number of draws must be integer.\n", call. = FALSE)
    }
    
    if(draws<=0){
      
      message(" ")
      stop(" Number of draws must geater than zero.\n", call. = FALSE)
    }
    
    stepsint <- steps%%1==0
    
    if(stepsint!=TRUE){
      
      message(" ")
      stop(" Number of steps must be integer.\n", call. = FALSE)
    }
    
    if(steps<=0){
      
      message(" ")
      stop(" Number of steps must geater than zero.\n", call. = FALSE)
    }
    
    
    subdrawsint <- subdraws%%1==0
    
    if(subdrawsint!=TRUE){
      
      message(" ")
      stop(" Number of subdraws must be integer.\n", call. = FALSE)
    }
    
    if(subdraws<=0){
      
      message(" ")
      stop(" Number of subdraws must geater than zero.\n", call. = FALSE)
    }
    
    nkeepint <- nkeep%%1==0
    
    if(nkeepint!=TRUE){
      
      message(" ")
      stop(" Number of nkeep must be integer.\n", call. = FALSE)
    }
    
    if(nkeep<=0){
      
      message(" ")
      stop(" Number of nkeep must geater than zero.\n", call. = FALSE)
    }
    
    
    KMINint <- KMIN%%1==0
    
    if(KMINint!=TRUE){
      
      message(" ")
      stop("KMIN must be integer.\n", call. = FALSE)
    }
    
    if(KMIN<=0 | KMIN>KMAX){
      
      message(" ")
      stop("KMIN must be greater than zero and smaller than KMAX.\n", call. = FALSE)
    }
    
    
    KMAXint <- KMAX%%1==0
    
    if(KMAXint!=TRUE){
      
      message(" ")
      stop("KMAX must be integer.\n", call. = FALSE)
    }
    
    if(KMIN<=0 | KMIN>KMAX){
      
      message(" ")
      stop("KMAX must be greater than zero and greater than KMIN.\n", call. = FALSE)
    }
    
    
    cnsl <- length(constrained)
    
    if(cnsl<=0){
      
      message(" ")
      stop("Number of constraints must at least 1.\n", call. = FALSE)
    }
    
    if(cnsl>ncol(Y)){
      
      message(" ")
      stop("Number of constraints cannot be greater than number of variables.\n", call. = FALSE)
    }
    
    if(max(abs(constrained))>ncol(Y) | min(abs(constrained))==0){
      
      message(" ")
      stop("Constraints must be between 1 and the number of variables.\n", call. = FALSE)
    }
    
    cnscns <- all(constrained%%1==0)
    
    if(cnscns==FALSE){
      
      message(" ")
      stop("All constraints must be integers.\n", call. = FALSE)
    }
    
    cnsdup <- anyDuplicated(abs(constrained))
    if(cnsdup>0){
      
      message(" ")
      stop("Cannot provide multiple constraints for the same variable.\n", call. = FALSE)
    }
    
    return()
    
  }


#' Title
#'
#' @param Q 
#' @param first 
#' @param last 
#' @param constrained 
#' @param impulses 
#'
#' @return
#' @export
#'
#' @examples
UhligAccept <- function(Q, first, last, constrained, impulses){#ok
    for(k in first:last){#ok
      ik <- impulses[k, , ]%*%Q#ok
      for(i in 1:length(constrained)){#ok
        if(constrained[i]<0){#ok
          value <- ik[-1.0 * constrained[i]]#ok
        }else{#ok
          value <- -1.0 * ik[constrained[i]]#ok
        }#ok
        if(value>0.0){#ok
          if(k==first & i==1){#ok
            Q <- -1.0 * Q#ok
            ik <- -1.0 * ik#ok
          }else{#ok
            acc <- 0
            uar <- list(Q=Q, acc=acc, ika=ik)
            return(uar)
          }#ok
        }#ok
      }#end i #ok
    }#end k #ok
    acc <- 1
    uar <- list(Q=Q, acc=acc, ika=ik)
    return(uar)
  }


#' Generates plots of impulse responses with error bands from VAR posterior
#' draws
#'
#' @param irfdraws A draws x steps x nvar array of posterior impulse responses.
#' @param type A string, either "median" or "mean" response, specifying the type
#'   of response to be plotted. The default is "median".
#' @param labels A list of variable labels for impulse response plots. The
#'   default are the variable names of the model.
#' @param save A logical statement to save the graph in the current working
#'   directory. The default is 'FALSE'.
#' @param bands A list of length 2 containing the error bands of the impulse
#'   responses. If bands='NULL', only IRFs are plotted. The default is c(0.16,
#'   0.84).
#' @param grid A logical statement on whether or not to generate grid lines in
#'   the plots. The default is 'TRUE'.
#' @param bw A logical statement on whether or not to generate black and white
#'   or colour graphs. The default is 'FALSE'.
#'
#' @return
#' @export
#'
#' @examples
#' irfplot(irfdraws=irfs0, type="mean", labels=vl, save=FALSE, bands=c(0.16, 0.84),grid=TRUE, bw=FALSE)
irfplot <- function(irfdraws=NULL,type="median", labels=unlist(dimnames(irfdraws)[3]), save=FALSE, bands=c(0.16, 0.84), grid=TRUE, bw=FALSE, fileName = "irf.eps"){
    #
    
    #--- SANITY CHECK ---#
    sanity.check.irfplot(irfdraws=irfdraws,type=type, labels=labels,save=save, bands=bands, grid=grid, bw=bw)
    
    graphics.off()
    par.def <- par(no.readonly = T)
    #graph parameter
    goodresp <- irfdraws
    irftype <- type #  0== median, 1== mean response
    gridgrph <- grid # grid in irf plots 0== none, 1== adds grid to plots
    bndtest <- is.null(bands)
    if(bndtest!=TRUE){
      ebupp <- bands[2]# error bands for irf plots
      eblow <- bands[1]
    }else{
      ebupp <- 0.84# error bands for irf plots
      eblow <- 0.16
    }
    varlbl <- labels
    nstep <- dim(irfdraws)[2]
    nvar <- dim(irfdraws)[3]
    
    if(irftype=="mean"){
      imp_responses <- array(NA, dim=c(3, nstep, nvar))
      irfbands <- apply(goodresp,c(2,3),quantile,probs=c(eblow, ebupp))
      irfmean <-  array(apply(goodresp,c(2,3),mean), dim=c(1,nstep, nvar))
      dimnames(imp_responses) <- list(c("irf", "lower", "upper"),1:nstep, varlbl)
      imp_responses[1,,] <- irfmean
      imp_responses[2:3,,] <- irfbands
      dimnames(imp_responses) <- list(c("irf", "lower", "upper"),1:nstep, varlbl)
    }else{
      imp_responses <- apply(goodresp,c(2,3),quantile,probs=c(0.5, eblow, ebupp))
      dimnames(imp_responses) <- list(c("irf", "lower", "upper"),1:nstep, varlbl)
    }
    impt <- imp_responses
    impt <- aperm(impt,c(3,2,1))
    
    #--- DETERMINE COLS AND ROWS OF PLOT ---#
    rowsize <-  ceiling(sqrt(nvar))
    colsize <- ceiling(nvar / rowsize)
    
    #-- GENERATE PLOTS ---#
    # dev.off()
    par(bty="o", mfcol=c(rowsize, colsize), mar=c(rep(2.5,4)))
    if(bw==FALSE){
      for(i in 1:nvar){
        ulim <- max(impt[i,,1:3])
        llim <- min(impt[i,,1:3])
        plot(x=1:nstep, y=impt[i,,1], type="l", col="red", lwd=2, xlab="", ylab="", main= paste(varlbl[i]), ylim=c(llim, ulim))
        if(bndtest!=TRUE){
          lines(1:nstep, impt[i,,2], col="blue", lwd=2)
          lines(1:nstep, impt[i,,3], col="blue", lwd=2)
        }
        abline(h=0, col="black")
        if(gridgrph==1){
          grid(nx = NULL, ny = NULL, col = "lightgrey", lty = "dotted",   lwd = par("lwd"))
        }
      }
      if(save==TRUE){
        dev.copy(postscript, fileName)
        dev.off()
      }
    }else{
      for(i in 1:nvar){
        ulim <- max(impt[i,,1:3])
        llim <- min(impt[i,,1:3])
        plot(x=1:nstep, y=impt[i,,1], type="l", col="black", lwd=2, xlab="", ylab="", main= paste(varlbl[i]), ylim=c(llim, ulim))
        if(bndtest!=TRUE){
          lines(1:nstep, impt[i,,2], lty=2, col="black", lwd=2)
          lines(1:nstep, impt[i,,3], lty=2, col="black", lwd=2)
        }
        abline(h=0, col="black")
        if(gridgrph==1){
          grid(nx = NULL, ny = NULL, col = "lightgrey", lty = "dotted",   lwd = par("lwd"))
        }
        
      }
      if(save==TRUE){
        dev.copy(postscript, fileName)
        dev.off()
      }
      
    }
    par(par.def)
}



#' Title
#'
#' @param irfdraws 
#' @param type 
#' @param labels 
#' @param save 
#' @param bands 
#' @param grid 
#' @param bw 
#'
#' @return
#' @export
#'
#' @examples
sanity.check.irfplot <- function(irfdraws=irfdraws,type=type, labels=labels,save=save, bands=bands, grid=grid, bw=bw){
    #irfdraws
    Idim <- is.null(dim(irfdraws))
    
    if(Idim==TRUE){
      
      message(" ")
      stop(" Irfdraws must be of dimensions (draws x steps x nvar) .\n", call. = FALSE)
    }
    
    Idiml <- length(dim(irfdraws))
    
    if(Idiml!=3){
      
      message(" ")
      stop(" Irfdraws must be of dimensions (draws x steps x nvar) .\n", call. = FALSE)
    }
    
    Ina <- any(is.na(irfdraws)==TRUE)
    Inan <- any(is.nan(irfdraws)==TRUE)
    Inum <-  is.numeric(irfdraws)
    
    if(Ina==TRUE | Inan==TRUE){
      
      message(" ")
      stop(" Irfdraws must not contain missing values.\n", call. = FALSE)
    }
    
    #type
    if(type!="median" & type!="mean"){
      
      message(" ")
      stop("IRF type must be mean or median.\n", call. = FALSE)
    }
    #labels
    lbll <- length(labels)
    
    if(lbll < dim(irfdraws)[3]){
      
      message(" ")
      stop("Number of labels must be equal number of variables in the model.\n", call. = FALSE)
    }
    bndtest <- is.null(bands)
    if(bndtest!=TRUE){
      bndsl <- length(bands)
      
      if(bndsl !=2){
        
        message(" ")
        stop("Error bands must contain only two values c(lower, upper) or 'NULL'.\n", call. = FALSE)
      }
      
      
      if(max(bands)>=1 | min(bands)<=0){
        
        message(" ")
        stop("Error bands must be between 0 and 1.\n", call. = FALSE)
      }
    }
    return()
}
