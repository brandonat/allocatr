## Efficient Frontier

## Adapted from 
## http://www.rinfinance.com/RinFinance2009/presentations/yollin_slides.pdf

effFrontier = function (averet, rcov, nports = 20, shorts=T, wmax=1)
{
  mxret = max(abs(averet))
  mnret = -mxret
  n.assets = ncol(averet)
  reshigh = rep(wmax,n.assets)
  if( shorts )
  {
    reslow = rep(-wmax,n.assets)
  } else {
    reslow = rep(0,n.assets)
  }
  min.rets = seq(mnret, mxret, len = nports)
  vol = rep(NA, nports)
  ret = rep(NA, nports)
  for (k in 1:nports)
  {
    port.sol = NULL
    try(port.sol <- portfolio.optim(x=averet, pm=min.rets[k], covmat=rcov,
                                    reshigh=reshigh, reslow=reslow,shorts=shorts),silent=T)
    if ( !is.null(port.sol) )
    {
      vol[k] = sqrt(as.vector(port.sol$pw %*% rcov %*% port.sol$pw))
      ret[k] = averet %*% port.sol$pw
    }
  }
  return(list(vol = vol, ret = ret))
}


maxSharpe = function (averet, rcov, shorts=T, wmax = 1)
{
  optim.callback = function(param,averet,rcov,reshigh,reslow,shorts)
  {
    port.sol = NULL
    try(port.sol <- portfolio.optim(x=averet, pm=param, covmat=rcov,
                                    reshigh=reshigh, reslow=reslow, shorts=shorts), silent = T)
    if (is.null(port.sol)) {
      ratio = 10^9
    } else {
      m.return = averet %*% port.sol$pw
      m.risk = sqrt(as.vector(port.sol$pw %*% rcov %*% port.sol$pw))
      ratio = -m.return/m.risk
      assign("w",port.sol$pw,inherits=T)
    }
    return(ratio)
  }
  ef = effFrontier(averet=averet, rcov=rcov, shorts=shorts, wmax=wmax, nports = 100)
  n = ncol(averet)
  reshigh = rep(wmax,n)
  if( shorts ) {
    reslow = -reshigh
  } else {
    reslow = rep(0,n)
  }
  max.sh = which.max(ef$ret/ef$vol)
  w = rep(0,ncol(averet))
  xmin = optimize(f=optim.callback, interval=c(ef$ret[max.sh-1], upper=ef$ret[max.sh+1]),
                  averet=averet,rcov=rcov,reshigh=reshigh,reslow=reslow,shorts=shorts)
  return(w)
}