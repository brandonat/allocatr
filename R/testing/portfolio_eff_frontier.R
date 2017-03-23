
#http://blog.streeteye.com/blog/2012/01/portfolio-optimization-and-efficient-frontiers-in-r/

library(quadprog)
library(ggplot2)
library(dplyr)

## Paramaters
equity  <- c("SPY", "MDY", "IJR", "EFA", "EEM")
bond    <- c("SHV", "IEI", "TLT", "LQD", "BNDX", "TIP")
other   <- c("IYR")
symbols <- sort(c(equity, bond, other))

## Load stock prices
prices <- get_prices(symbols, from = "2000-01-01")

## Calculate daily returns
rets <- daily_returns(prices)

## Plot cumulative returns
plot_returns(rets)








optimal_portofio(rets)


## Minimum Variance Portfolio

efficient_frontier <- function (returns, short_selling = FALSE, max_allocation = NULL,
                                risk_premium_up = 0.5, risk_increment = 0.005){
  
  # return argument should be a m x n matrix with one column per security
  # short argument is whether short-selling is allowed; default is no (short
  # selling prohibited) max_allocation is the maximum % allowed for any one
  # security (reduces concentration) risk.premium.up is the upper limit of the
  # risk premium modeled (see for loop below) and risk.increment is the
  # increment (by) value used in the for loop

  cov_mat = cov(returns)
  n_assets <- ncol(returns)
  
  # Create initial Amat and bvec assuming only equality constraint
  # (short-selling is allowed, no allocation constraints)
  Amat <- matrix (1, nrow = n_assets)
  bvec <- 1
  meq <- 1

  # Then modify the Amat and bvec if short-selling is prohibited
  if(!short_selling){
    Amat <- cbind(1, diag(n_assets))
    bvec <- c(bvec, rep(0, n_assets))
  }

  # And modify Amat and bvec if a max allocation (concentration) is specified
  if(!is.null(max_allocation)){
    if(max_allocation > 1 | max_allocation <0){
      stop("max_allocation must be greater than 0 and less than 1")
    }
    if(max_allocation * n_assets < 1){
      stop("Need to set max_allocation higher; not enough assets to add to 1")
    }
    Amat <- cbind(Amat, -diag(n_assets))
    bvec <- c(bvec, rep(-max_allocation, n_assets))
  }

  # Calculate the number of loops
  loops <- risk_premium_up / risk_increment + 1
  loop <- 1

  # Initialize a matrix to contain allocation and statistics
  # This is not necessary, but speeds up processing and uses less memory
  ef <- matrix(nrow = loops, ncol = n_assets + 3)
  
  # Now I need to give the matrix column names
  colnames(ef) <- c(colnames(returns), "sd", "ret", "sharpe")

  # Loop through the quadratic program solver
  for (i in seq(0, risk_premium_up, risk_increment)) {
    dvec <- colMeans(returns) * i # This moves the solution along the EF
    sol <- solve.QP(cov_mat, dvec = dvec, Amat = Amat, bvec = bvec, meq = meq)
    ef[loop, 1:n_assets] <- sol$solution
    ef[loop, "sd"] <- sqrt(sum(sol$solution * colSums((cov_mat * sol$solution))))
    ef[loop, "ret"] <- as.numeric(sol$solution %*% colMeans(returns))
    loop <- loop+1
  }
  ef[, "sharpe"] <- ef[, "ret"] / ef[, "sd"]
  
  as.data.frame(ef)
}

ef <- efficient_frontier(rets, max_allocation = 0.2)

## annualize
ef$ret <- round((1 + ef$ret) ^ 250 - 1, 4)
ef$sd <- round(ef$sd * sqrt(250), 4)  

optimal <- ef[which.max(ef$sharpe), ]

ggplot(ef) + 
  geom_line(aes(x = sd, y = ret)) +
  geom_point(aes(x = optimal$sd, y = optimal$ret)) +
  theme_minimal()




# minimize variance:  w %*% covmatrix %*% t(w)
# subject to sum of ws = 1
# subject to each ws > 0

# solution.minvol <- solve.QP(covmatrix, zeros, t(opt.constraints), opt.rhs, meq = opt.meq)
# first 2 parameters covmatrix, zeros define function to be minimized
# if zeros is all 0s, the function minimized ends up equal to port variance / 2
# opt.constraints is the left hand side of the constraints, ie the cs in
# c1 w1 + c2 w2 ... + cn wn = K
# opt.rhs is the Ks in the above equation
# meq means the first meq rows are 'equals' constraints, remainder are >= constraints
# if you want to do a <= constraint, multply by -1 to make it a >= constraint
# does not appear to accept 0 RHS, so we make it a tiny number> 0

# compute covariance matrix


# 1 x numassets array of 1s
constraints <- matrix (c(1, 1, 1, 1,   # sum of weights =1
             						 1, 0, 0, 0,   # w1 >= 0
                         0, 1, 0, 0,   # w1 >= 0
                         0, 0, 1, 0,   # w1 >= 0
                         0, 0, 0, 1)   # w2 >= 0
                         , nrow=5, byrow=TRUE)

opt.rhs <- matrix(c(1, 0.000001, 0.000001, 0.000001, 0.000001))
opt.meq <- 1  # first constraint is '=', rest are '>='

# numassets x 1 array of 0s
zeros <- array(0, dim = c(n_assets,1))

res <- solve.QP(cov_mat, zeros, t(opt.constraints), opt.rhs, meq = opt.meq)

minvol_wts <= res$solution
minvol_var <- res$value
minvol_ret <- asset_returns %*% minvol_wts
  




  
#   > # generate a sequence of 50 evenly spaced returns between min var return and max return
#   > lowreturn=ret.minvol
# > highreturn=ret.maxret
# > minreturns=seq(lowreturn, highreturn, length.out=50)
# >
#   > # add a return constraint: sum of weight * return >= x
#   > retconst= rbind(opt.constraints, realreturns)
# > retrhs=rbind(opt.rhs, ret.minvol)
# >
#   > # create vectors for the returns, vols, and weights along the frontier,
#   > # starting with the minvol portfolio
#   > out.ret=c(ret.minvol)
# > out.vol=c(vol.minvol)
# > out.stocks=c(wts.minvol[1])
# > out.bills=c(wts.minvol[2])
# > out.bonds=c(wts.minvol[3])
# > out.gold=c(wts.minvol[4])
# >
#   > # loop and run a minimum volatility optimization for each return level from 2-49
#   > for(i in 2:(length(minreturns) - 1)) {
#     +	 print(i)
#     +	 # start with existing constraints, no return constraint
#       +	 tmp.constraints = retconst
#       +	 tmp.rhs=retrhs
#       +	 # set return constraint
#         +	 tmp.rhs[6] = minreturns[i]
#         +
#           +	 tmpsol <- solve.QP(covmatrix, zeros, t(tmp.constraints), tmp.rhs, meq = opt.meq)
#           +
#             +	 tmp.wts = tmpsol$solution
#             +	 tmp.var = tmpsol$value *2
#             +	 out.ret[i] = realreturns %*% tmp.wts
#             +	 out.vol[i] = sqrt(tmp.var)
#             +	 out.stocks[i]=tmp.wts[1]
#             +	 out.bills[i]=tmp.wts[2]
#             +	 out.bonds[i]=tmp.wts[3]
#             +	 out.gold[i]=tmp.wts[4]
#             + }
# >
#   > # put maxreturn portfolio in return series for max return, index =50
#   > out.ret[50]=c(ret.maxret)
# > out.vol[50]=c(vol.maxret)
# > out.stocks[50]=c(wts.maxret[1])
# > out.bills[50]=c(wts.maxret[2])
# > out.bonds[50]=c(wts.maxret[3])
# > out.gold[50]=c(wts.maxret[4])
# >
#   > # organize in a data frame
#   > efrontier=data.frame(out.ret*100)
# > efrontier$vol=out.vol*100
# > efrontier$stocks=out.stocks*100
# > efrontier$bills=out.bills*100
# > efrontier$bonds=out.bonds*100
# > efrontier$gold=out.gold*100
# > names(efrontier) = c("Return", "Risk", "%Stocks", "%Bills", "%Bonds", "%Gold")
# > efrontier
# 
# 
# apoints=data.frame(realsdspct)
# apoints$realreturns = realreturnspct
# ggplot(data=efrontier, aes(x=Risk, y=Return)) +
#   opts(title="Efficient Frontier") +
#   theme_bw() +
#   geom_line(size=1.4) +
#   geom_point(aes(x=apoints$realsdspct, y=apoints$realreturns)) +
#   scale_x_continuous(limits=c(1,24)) +
#   annotate("text", apoints[1,1], apoints[1,2],label=" stocks", hjust=0) +
#   annotate("text", apoints[2,1], apoints[2,2],label=" bills", hjust=0) +
#   annotate("text", apoints[3,1], apoints[3,2],label=" bonds", hjust=0) +
#   annotate("text", apoints[4,1], apoints[4,2],label=" gold", hjust=0) +
#   annotate("text", 19,0.3,label="streeteye.com", hjust=0, alpha=0.5)
#   
#   
# 
# 
# > keep=c("Risk", "%Stocks","%Bills","%Bonds","%Gold")
# > efrontier.tmp = efrontier[keep]
# > efrontier.m = melt(efrontier.tmp, id ='Risk')
# >
#   > ggplot(data=efrontier.m, aes(x=Risk, y=value, colour=variable, fill=variable)) +
#   +	 theme_bw() +
#   +	 opts(title="Transition Map", legend.position="top", legend.direction="horizontal") +
#   +	 ylab('% Portfolio') +
#   +	 geom_area() +
#   +	 scale_colour_manual("", breaks=c("%Stocks", "%Bills", "%Bonds","%Gold"), values = c(dvblue,dvgreen,dvred,dvyellow), labels=c('%Stocks', '%Bills','%Bonds','%Gold')) +
#   +	 scale_fill_manual("", breaks=c("%Stocks", "%Bills", "%Bonds","%Gold"), values = c(dvblue,dvgreen,dvred,dvyellow), labels=c('%Stocks', '%Bills','%Bonds','%Gold')) +
#   +	 annotate("text", 16,-2.5,label="streeteye.com", hjust=0, alpha=0.5)
# >
# 
# 
# 
# # Economist at Large
# # Modern Portfolio Theory
# # Use solve.QP to solve for efficient frontier
# # Last Edited 5/3/13
# 
# # This file uses the solve.QP function in the quadprog package to solve for the
# # efficient frontier.
# # Since the efficient frontier is a parabolic function, we can find the solution
# # that minimizes portfolio variance and then vary the risk premium to find
# # points along the efficient frontier. Then simply find the portfolio with the
# # largest Sharpe ratio (expected return / sd) to identify the most
# # efficient portfolio
# 
# library(stockPortfolio) # Base package for retrieving returns
# library(ggplot2) # Used to graph efficient frontier
# library(reshape2) # Used to melt the data
# library(quadprog) #Needed for solve.QP
# 
# # Create the portfolio using ETFs, incl. hypothetical non-efficient allocation
# stocks <- c(
#   "VTSMX" = .0,
#   "SPY" = .20,
#   "EFA" = .10,
#   "IWM" = .10,
#   "VWO" = .30,
#   "LQD" = .20,
#   "HYG" = .10)
# 
# # Retrieve returns, from earliest start date possible (where all stocks have
# # data) through most recent date
# returns <- getReturns(names(stocks[-1]), freq="week") #Currently, drop index
# 
# #### Efficient Frontier function ####
# eff.frontier <- function (returns, short="no", max_allocation=NULL,
#                           risk.premium.up=.5, risk.increment=.005){
#   # return argument should be a m x n matrix with one column per security
#   # short argument is whether short-selling is allowed; default is no (short
#   # selling prohibited)max_allocation is the maximum % allowed for any one
#   # security (reduces concentration) risk.premium.up is the upper limit of the
#   # risk premium modeled (see for loop below) and risk.increment is the
#   # increment (by) value used in the for loop
#   
#   covariance <- cov(returns)
#   print(covariance)
#   n <- ncol(covariance)
#   
#   # Create initial Amat and bvec assuming only equality constraint
#   # (short-selling is allowed, no allocation constraints)
#   Amat <- matrix (1, nrow=n)
#   bvec <- 1
#   meq <- 1
#   
#   # Then modify the Amat and bvec if short-selling is prohibited
#   if(short=="no"){
#     Amat <- cbind(1, diag(n))
#     bvec <- c(bvec, rep(0, n))
#   }
#   
#   # And modify Amat and bvec if a max allocation (concentration) is specified
#   if(!is.null(max_allocation)){
#     if(max_allocation > 1 | max_allocation <0){
#       stop("max_allocation must be greater than 0 and less than 1")
#     }
#     if(max_allocation * n < 1){
#       stop("Need to set max_allocation higher; not enough assets to add to 1")
#     }
#     Amat <- cbind(Amat, -diag(n))
#     bvec <- c(bvec, rep(-max_allocation, n))
#   }
#   
#   # Calculate the number of loops
#   loops <- risk.premium.up / risk.increment + 1
#   loop <- 1
#   
#   # Initialize a matrix to contain allocation and statistics
#   # This is not necessary, but speeds up processing and uses less memory
#   eff <- matrix(nrow=loops, ncol=n+3)
#   # Now I need to give the matrix column names
#   colnames(eff) <- c(colnames(returns), "Std.Dev", "Exp.Return", "sharpe")
#   
#   # Loop through the quadratic program solver
#   for (i in seq(from=0, to=risk.premium.up, by=risk.increment)){
#     dvec <- colMeans(returns) * i # This moves the solution along the EF
#     sol <- solve.QP(covariance, dvec=dvec, Amat=Amat, bvec=bvec, meq=meq)
#     eff[loop,"Std.Dev"] <- sqrt(sum(sol$solution*colSums((covariance*sol$solution))))
#     eff[loop,"Exp.Return"] <- as.numeric(sol$solution %*% colMeans(returns))
#     eff[loop,"sharpe"] <- eff[loop,"Exp.Return"] / eff[loop,"Std.Dev"]
#     eff[loop,1:n] <- sol$solution
#     loop <- loop+1
#   }
#   
#   return(as.data.frame(eff))
# }
# 
# # Run the eff.frontier function based on no short and 50% alloc. restrictions
# eff <- eff.frontier(returns=returns$R, short="no", max_allocation=.50,
#                     risk.premium.up=1, risk.increment=.001)
# 
# # Find the optimal portfolio
# eff.optimal.point <- eff[eff$sharpe==max(eff$sharpe),]
# 
# # graph efficient frontier
# # Start with color scheme
# ealred <- "#7D110C"
# ealtan <- "#CDC4B6"
# eallighttan <- "#F7F6F0"
# ealdark <- "#423C30"
# 
# ggplot(eff, aes(x=Std.Dev, y=Exp.Return)) + geom_point(alpha=.1, color=ealdark) +
#   geom_point(data=eff.optimal.point, aes(x=Std.Dev, y=Exp.Return, label=sharpe),
#              color=ealred, size=5) +
#   annotate(geom="text", x=eff.optimal.point$Std.Dev,
#            y=eff.optimal.point$Exp.Return,
#            label=paste("Risk: ",
#                        round(eff.optimal.point$Std.Dev*100, digits=3),"\nReturn: ",
#                        round(eff.optimal.point$Exp.Return*100, digits=4),"%\nSharpe: ",
#                        round(eff.optimal.point$sharpe*100, digits=2), "%", sep=""),
#            hjust=0, vjust=1.2) +
#   ggtitle("Efficient Frontier\nand Optimal Portfolio") +
#   labs(x="Risk (standard deviation of portfolio)", y="Return") +
#   theme(panel.background=element_rect(fill=eallighttan),
#         text=element_text(color=ealdark),
#         plot.title=element_text(size=24, color=ealred))
# ggsave("Efficient Frontier.png")
