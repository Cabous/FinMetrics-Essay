getPortfolio <-
    function(er, cov.mat, weights)
    {
        # contruct portfolio object
        #
        # inputs:
        # er				   N x 1 vector of expected returns
        # cov.mat  		 N x N covariance matrix of returns
        # weights			 N x 1 vector of portfolio weights
        #
        # output is portfolio object with the following elements
        # call				original function call
        # er				  portfolio expected return
        # sd				  portfolio standard deviation
        # weights			N x 1 vector of portfolio weights
        #
        call <- match.call()

        #
        # check for valid inputs
        #
        asset.names <- names(er)
        weights <- as.vector(weights)
        names(weights) = names(er)
        er <- as.vector(er)					# assign names if none exist
        if(length(er) != length(weights))
            stop("dimensions of er and weights do not match")
        cov.mat <- as.matrix(cov.mat)
        if(length(er) != nrow(cov.mat))
            stop("dimensions of er and cov.mat do not match")
        if(any(diag(chol(cov.mat)) <= 0))
            stop("Covariance matrix not positive definite")

        #
        # create portfolio
        #
        er.port <- crossprod(er,weights)
        sd.port <- sqrt(weights %*% cov.mat %*% weights)
        ans <- list("call" = call,
                    "er" = as.vector(er.port),
                    "sd" = as.vector(sd.port),
                    "weights" = weights)
        class(ans) <- "portfolio"
        ans
    }

efficient.portfolio <-
    function(er, cov.mat, target.return)
    {
        # compute minimum variance portfolio subject to target return
        #
        # inputs:
        # er					    N x 1 vector of expected returns
        # cov.mat  			  N x N covariance matrix of returns
        # target.return	  scalar, target expected return
        #
        # output is portfolio object with the following elements
        # call				    original function call
        # er					    portfolio expected return
        # sd					    portfolio standard deviation
        # weights			    N x 1 vector of portfolio weights
        #
        call <- match.call()

        #
        # check for valid inputs
        #
        asset.names <- names(er)
        er <- as.vector(er)					# assign names if none exist
        cov.mat <- as.matrix(cov.mat)
        if(length(er) != nrow(cov.mat))
            stop("invalid inputs")
        if(any(diag(chol(cov.mat)) <= 0))
            stop("Covariance matrix not positive definite")
        # remark: could use generalized inverse if cov.mat is positive semidefinite

        #
        # compute efficient portfolio
        #
        ones <- rep(1, length(er))
        top <- cbind(2*cov.mat, er, ones)
        bot <- cbind(rbind(er, ones), matrix(0,2,2))
        A <- rbind(top, bot)
        b.target <- as.matrix(c(rep(0, length(er)), target.return, 1))
        x <- solve(A, b.target)
        w <- x[1:length(er)]
        names(w) <- asset.names

        #
        # compute portfolio expected returns and variance
        #
        er.port <- crossprod(er,w)
        sd.port <- sqrt(w %*% cov.mat %*% w)
        ans <- list("call" = call,
                    "er" = as.vector(er.port),
                    "sd" = as.vector(sd.port),
                    "weights" = w)
        class(ans) <- "portfolio"
        ans
    }

globalMin.portfolio <-
    function(er, cov.mat)
    {
        # Compute global minimum variance portfolio
        #
        # inputs:
        # er				N x 1 vector of expected returns
        # cov.mat		N x N return covariance matrix
        #
        # output is portfolio object with the following elements
        # call			original function call
        # er				portfolio expected return
        # sd				portfolio standard deviation
        # weights		N x 1 vector of portfolio weights
        call <- match.call()

        #
        # check for valid inputs
        #
        asset.names <- names(er)
        er <- as.vector(er)					# assign names if none exist
        cov.mat <- as.matrix(cov.mat)
        if(length(er) != nrow(cov.mat))
            stop("invalid inputs")
        if(any(diag(chol(cov.mat)) <= 0))
            stop("Covariance matrix not positive definite")
        # remark: could use generalized inverse if cov.mat is positive semi-definite

        #
        # compute global minimum portfolio
        #
        cov.mat.inv <- solve(cov.mat)
        one.vec <- rep(1,length(er))
        #  w.gmin <- cov.mat.inv %*% one.vec/as.vector(one.vec %*% cov.mat.inv %*% one.vec)
        w.gmin <- rowSums(cov.mat.inv) / sum(cov.mat.inv)
        w.gmin <- as.vector(w.gmin)
        names(w.gmin) <- asset.names
        er.gmin <- crossprod(w.gmin,er)
        sd.gmin <- sqrt(t(w.gmin) %*% cov.mat %*% w.gmin)
        gmin.port <- list("call" = call,
                          "er" = as.vector(er.gmin),
                          "sd" = as.vector(sd.gmin),
                          "weights" = w.gmin)
        class(gmin.port) <- "portfolio"
        gmin.port
    }


tangency.portfolio <-
    function(er,cov.mat,risk.free)
    {
        # compute tangency portfolio
        #
        # inputs:
        # er				   N x 1 vector of expected returns
        # cov.mat		   N x N return covariance matrix
        # risk.free		 scalar, risk-free rate
        #
        # output is portfolio object with the following elements
        # call			  captures function call
        # er				  portfolio expected return
        # sd				  portfolio standard deviation
        # weights		 N x 1 vector of portfolio weights
        call <- match.call()

        #
        # check for valid inputs
        #
        asset.names <- names(er)
        if(risk.free < 0)
            stop("Risk-free rate must be positive")
        er <- as.vector(er)
        cov.mat <- as.matrix(cov.mat)
        if(length(er) != nrow(cov.mat))
            stop("invalid inputs")
        if(any(diag(chol(cov.mat)) <= 0))
            stop("Covariance matrix not positive definite")
        # remark: could use generalized inverse if cov.mat is positive semi-definite

        #
        # compute global minimum variance portfolio
        #
        gmin.port <- globalMin.portfolio(er,cov.mat)
        if(gmin.port$er < risk.free)
            stop("Risk-free rate greater than avg return on global minimum variance portfolio")

        #
        # compute tangency portfolio
        #
        cov.mat.inv <- solve(cov.mat)
        w.t <- cov.mat.inv %*% (er - risk.free) # tangency portfolio
        w.t <- as.vector(w.t/sum(w.t))	# normalize weights
        names(w.t) <- asset.names
        er.t <- crossprod(w.t,er)
        sd.t <- sqrt(t(w.t) %*% cov.mat %*% w.t)
        tan.port <- list("call" = call,
                         "er" = as.vector(er.t),
                         "sd" = as.vector(sd.t),
                         "weights" = w.t)
        class(tan.port) <- "portfolio"
        tan.port
    }
