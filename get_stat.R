# This file contains useful functions for general numerical and statistical analysis.
# Last update: Jan 2013, Amos P. K. Tai (pkamostai@gmail.com)



# Area under curve:
trapz = function(x, y) {
	# This function calculates the area under a curve y = f(x), i.e. integral of f(x)dx, using linear approximation.
  	A.i = (y[-1]+y[-length(y)])*(x[-1]-x[-length(x)])/2
  	A = sum(A.i)
  	return(A)
 	}



# Roots of quadratic equation:
quadroot = function(a, b, c) {
	# This function finds the roots of the quadratic equation a*x^2 + b*x + c = 0.
	D = b^2 - 4*a*c
	if (D >= 0) {	# Roots are real.
		x1 = (-b+sqrt(D))/(2*a)
		x2 = (-b-sqrt(D))/(2*a)
		}
	else {	# Roots are complex.
		x1 = complex(real=(-b/2/a), imaginary=(sqrt(-D)/2/a))
		x2 = complex(real=(-b/2/a), imaginary=(-sqrt(-D)/2/a))
		}
	return(c(x1, x2))
	}
		


# Cross product of two vectors:
vec.cross = function(a, b) {
	# This function calculates the cross product of two vectors a x b.
	n = NULL
	n[1] = a[2]*b[3]-b[2]*a[3]
	n[2] = b[1]*a[3]-a[1]*b[3]
	n[3] = a[1]*b[2]-b[1]*a[2]
	return(n)
	}



# Calculating centered values for a multidimensional array:
c.val = function(X) {
	# This function calculates the centered values (X - mean(X)) for data along the third dimension of a 3-dimensional array X.
	X.c = array(0, dim=dim(X))
	for (i in 1:dim(X)[1]) {
		for (j in 1:dim(X)[2]) {
			X.mean = mean(X[i,j,], na.rm=TRUE)
			X.c[i,j,] = X[i,j,]-X.mean
			}
		}
	return(X.c)
	}			



# Calculating the corresponding threshold R-value for a given p-value:
find.Rlim = function(pval, n) {
	# "pval" is the desired p-value; "n" is the sample size.
	t0 = qt(p=(1-pval/2), df=(n-1))	# The corresponding t-statistics
	R = sqrt(t0^2/(n-2+t0^2))
	# R is the corresponding R-value (correlation coefficient).
	return(R)
	}



# Test for multicollinearity using Variance Inflation Factor:
find.VIF = function(X) {
	# This function tests for the severity of the problem of multicollinearity based on VIF. VIF should not be > 5 for multicollinearity to be considered inconsequential.
	# "X" is a matrix with each column being an explanatory (predictor) variable.
	VIF = rep(0, times=ncol(X))
	for (i in 1:ncol(X)) {
		model = lm(X[,i] ~ X[,-i], na.action=na.exclude)
		adj.R2 = summary.lm(model)$adj.r.squared
		VIF[i] = 1/(1-adj.R2)
		}
	return(VIF)
	}



# Multiple linear regression:
ml.reg = function(X, Y, plim=0.05, normalized=FALSE) {
	# This function performs multiple linear regression of Y on X, obtaining only statistically significant regression coefficients (p-value < "plim").
	# Y is a vector of response (dependent) variable and X is a matrix with columns of explanatory (independent) variables without the constant term.
	# "ml.reg" returns a matrix, first column being the regression coefficient (with the constant term as the first entry), second column being the standard error, and third column being the associated p-value.
	# If "normalized" is TRUE, then the inputs X and Y are normalized to their standard deviations.
	reg.coef = rep(0, times=(ncol(as.matrix(X))+1))
	se = rep(0, times=(ncol(as.matrix(X))+1))
	pval = rep(0, times=(ncol(as.matrix(X))+1))
	MAT = na.omit(cbind(Y, X))
	Y = MAT[,1]
	X = MAT[,-1]
	if (nrow(MAT) <= ncol(MAT)) {
		reg.coef = NaN
		se = NaN
		pval = NaN
	}			
	else {
		if (normalized) {
			Y = (Y - mean(Y))/sd(Y)
			if (ncol(as.matrix(X)) == 1) X = (X - mean(X))/sd(X)
			else {
				for (j in 1:ncol(as.matrix(X))) X[,j] = (X[,j] - mean(X[,j]))/sd(X[,j])
			}
		}
		reg.model = lm(Y ~ X, na.action=na.exclude)
		reg.coef = summary(reg.model)$coefficients[,1]
		se = summary(reg.model)$coefficients[,2]
		pval = summary(reg.model)$coefficients[,4]
		pval[which(pval == "NaN")] = 1
		reg.coef[which(pval > plim)] = NaN
		se[which(pval > plim)] = NaN
	}
	if (length(reg.coef) != (ncol(as.matrix(X))+1)) {	# Singularities may have occured.
		reg.coef = rep(NaN, times=(ncol(as.matrix(X))+1))
		se = rep(NaN, times=(ncol(as.matrix(X))+1))
		pval = rep(NaN, times=(ncol(as.matrix(X))+1))
	}
	if (normalized) {
		reg.coef = reg.coef[-1]
		se = se[-1]
		pval = pval[-1]
	}
	return(cbind(reg.coef, se, pval))
}



# Orthogonal regression:
ortho.reg = function(X, Y, alpha=0.05) {
	# This function performs orthogonal regression of Y on X, i.e., minimizes the perpendicular distances between observations and regression line Y = b0 + b2*X.
	# X and Y are vectors of the explanatory (independent) and response (dependent) variables, respectively.
	# This function returns a vector c(b0, b1, b1.low, b1.up, R^2) for the linear model Y = b0 + b1*X, where b1.low and b1.up are the 100*(1-alpha)% confidence interval for b1; R^2 is the coefficient of determination.
	MAT = na.omit(cbind(X, Y))
	X = MAT[,1]; Y = MAT[,2]
	n = nrow(MAT)
	Sxx = sum((X-mean(X))^2); Syy = sum((Y-mean(Y))^2); Sxy = sum((X-mean(X))*(Y-mean(Y)));
	b1 = (Syy-Sxx+sqrt((Syy-Sxx)^2+4*Sxy^2))/2/Sxy
	b0 = mean(Y) - b1*mean(X)
	R = sum((X-mean(X))*(Y-mean(Y)))/sqrt(sum((X-mean(X))^2))/sqrt(sum((Y-mean(Y))^2))
	S = cbind(c(Sxx, Sxy), c(Sxy, Syy))
	LAMBDA = eigen(S)$values
	l1 = sort(LAMBDA)[2]; l2 = sort(LAMBDA)[1];
	theta = atan(b1)
	phi = asin((qchisq(1-alpha,1)/((n-1)*(l1/l2 + l2/l1 - 2)))^0.5)
	ci = c(tan(theta-phi), tan(theta+phi))
	return(c(b0, b1, ci, R^2))
}



# Multiple roots-finding:
multiroot = function(f, interval, tol=1e-9) {
	# This function searches for and calculates all possible roots of an arbitrary function within a given search interval.
	# "f" is the function to solve, e.g. f = function(x) x^2 - 3*x
	# "interval" is a vector of length 2, specifying the search interal.
	# "tol" is the tolerance level used in the function "uniroot".
	x = seq(interval[1], interval[2], length.out=1e3)
	y = rep(NaN, times=length(x))
	for (i in 1:length(y)) y[i] = f(x[i])
	count = 0
	root.int = NULL
	for (i in 1:(length(y)-1)) {
		# Case 1: no sign change
		if (sign(y[i+1]) == sign(y[i])) { } # Do nothing.
		# Case 2: sign change
		else {
			if (sign(y[i]) == 0) { } # Do nothing, as info must have been obtained during step i-1.
			else {
				count = count + 1
				if (sign(y[i+1]) == 0)
					root.int[(count*2-1):(count*2)] = c(x[i], x[i+2])
				else
					root.int[(count*2-1):(count*2)] = c(x[i], x[i+1])
				}
			}
		}
	roots = NULL
	for (i in 1:count) {
		roots[i] = uniroot(f, root.int[(i*2-1):(i*2)], tol=tol)$root
		}
	return(roots)	
	}
	
	
	
# Error functions:
# These are the Gauss error function, complementary error function, and the inverses.
# Error function (see Abramowitz and Stegun 29.2.29):
erf = function(x) 2*pnorm(x*sqrt(2)) - 1
# Complementary error function:
erfc = function(x) 2*pnorm(x*sqrt(2), lower=FALSE)
# The inverses of "erf" and "erfc":
erfinv = function(x) qnorm((1 + x)/2)/sqrt(2)
erfcinv = function(x) qnorm(x/2, lower=FALSE)/sqrt(2)



# Dicrete Fourier Series:
discrete.fourier = function(y, t) {
	# This function computes the discrete Fourier transformation for a time series y as a function of time domain t, assuming t is equally spaced and complete (i.e. no missing data). The function also returns the corresponding Fourier line spectrum in terms of R^2 = (n/2)*Ck^2/(n-1)/sy^2, where sy^2 is the variance of y.
	n = length(t)
	omega1 = 2*pi/n
	K = floor(n/2)
	A = rep(NaN, times=K)
	B = rep(NaN, times=K)
	C = rep(NaN, times=K)
	phi = rep(NaN, times=K)
	R.sq = rep(NaN, times=K)
	COS.MAT = matrix(NaN, nrow=n, ncol=K)
	SIN.MAT = matrix(NaN, nrow=n, ncol=K)
	for (k in 1:K) {
		if ((round(n/2)*2 == n) & k == K) {
			A[k] = 1/n*sum(y*cos(k*omega1*t))
			B[k] = 0/n*sum(y*sin(k*omega1*t))
		}
		else {
			A[k] = 2/n*sum(y*cos(k*omega1*t))
			B[k] = 2/n*sum(y*sin(k*omega1*t))
		}
		C[k] = sqrt(A[k]^2 + B[k]^2)
		if (A[k] > 0) phi[k] = atan(B[k]/A[k])
		if (A[k] < 0) phi[k] = atan(B[k]/A[k]) + pi
		if (A[k] == 0) phi[k] = pi/2
		R.sq[k] = n/2*C[k]^2/(n - 1)/var(y)
		COS.MAT[,k] = cos(k*omega1*t)
		SIN.MAT[,k] = sin(k*omega1*t)
	}
	y.fit = mean(y) + COS.MAT %*% A + SIN.MAT %*% B
	k = seq(1, K)
	omega = k*omega1
	fourier.fit = as.data.frame(cbind(k, omega, A, B, C, phi, R.sq))
	OUTPUT = list(n=n, y=y, var.y=var(y, na.rm=TRUE), y.fit=y.fit, cos.terms=COS.MAT, sin.terms=SIN.MAT, parameters=fourier.fit, R.sq=R.sq, C.sq=C^2)
	return(OUTPUT)
}



# Quantile of probability function y = f(x):
quantile.fx = function(x, y, p) {
	# This function finds the p-th quantile value of x for a given probability distribution vector y = f(x).
	# E.g. quantile.fx(x, y, 0.5) gives the median value of x given probability distribution y = f(x).
	cum.fx = cumsum(y)/tail(cumsum(y), 1)
	quant.x = spline(x=cum.fx, y=x, xout=p)$y
	return(quant.x)
}



# Mode of probability function y = f(x):
mode.fx = function(x, y) {
	# This function finds the mode of x given a probability distribution vector y = f(x), using the Golden Section method. In case of multi-modal distribution, the global maximum will be found.
	if (which(y == max(y)) == 1 | which(y == max(y)) == length(y)) {
		# Maximum is on the boundary.
		a = x[which(y == max(y))]
		print('Max is on boundary.')
	} else {
		tol = 1e-10
		tau = (sqrt(5) - 1)/2
		a = x[which(y == max(y))-1]
		b = x[which(y == max(y))+1]
		x1 = a + (1 - tau)*(b - a)
		y1 = spline(x, y, xout=x1)$y
		x2 = a + tau*(b - a)
		y2 = spline(x, y, xout=x2)$y
		while (abs(b - a) > tol) {
			if (y1 > y2) {
				a = x1
				x1 = x2
				y1 = y2
				x2 = a + tau*(b - a)
				y2 = spline(x, y, xout=x2)$y
			} else {
				b = x2
				x2 = x1
				y2 = y1
				x1 = a + (1 - tau)*(b - a)
				y1 = spline(x, y, xout=x1)$y
			}
		}
	}
	return(a)
}



# Find cumulative probability for where a specific value lies in a given sample distribution:
quantile.inv = function(x, x.spec) {
	# This function finds the cumulative probability for where a specific sample value "x.spec" lies within a given sample distribution "x", where "x" is a vector of sample values.
	# "x.spec" cannot be the exact median of "x".
	tol = 1e-9
	x = na.omit(x)
	if (x.spec > max(x) | x.spec < min(x)) {
		print('Requested sample value lies outside of the range of sample.')
		c = NaN
	} else {
		if (x.spec > median(x)) {
			a = 0.5
			b = 1
		} else {
			a = 0
			b = 0.5
		}
		c = a + (b - a)/2
		x1 = quantile(x, c)
		while (abs(x.spec - x1) > tol) {
			if (x.spec > x1) {
				a = c
				b = b
			} else {
				a = a
				b = c
			}
			c = a + (b - a)/2
			x1 = quantile(x, c)
		}
	}
	return(c)
}
			


# Specify plot region:
# This function overrides default plot parameter.

quartz.par = function(title='', width=5.5, height=4, mfrow=c(1,1), mai=c(0.5,0.5,0.2,0.2), mgp=c(1.4,0.4,0), tcl=-0.25, ps=12) {
	quartz(title=title, width=width, height=height)
	par(mfrow=mfrow, mai=mai, mgp=mgp, tcl=tcl, ps=ps)
}

X11.par = function(title='', width=5.5, height=4, mfrow=c(1,1), mai=c(0.5,0.5,0.2,0.2), mgp=c(1.4,0.4,0), tcl=-0.25, ps=12) {
	X11(title=title, width=width, height=height)
	par(mfrow=mfrow, mai=mai, mgp=mgp, tcl=tcl, ps=ps)
}



# Standard (reduced) major-axis regression:
sma.reg = function(X, Y, alpha=0.05) {
	# This function calculates the standard (reduced) major-axis regression results for model Y = b0 + b1*X, giving also (1-alpha)% confidence intervals and r squared.
	MAT = na.omit(cbind(X, Y))
	X = MAT[,1]; Y = MAT[,2]
	n = length(X)
	r = cor(X, Y)
	b1 = sign(r)*sd(Y)/sd(X)
	b0 = mean(Y) - b1*mean(X)
	B = qt((1 - alpha/2), n-2)^2*(1 - r^2)/(n - 2)
	if (b1 > 0) {
		b1.low = b1*(sqrt(B + 1) - sqrt(B))
		b1.up = b1*(sqrt(B + 1) + sqrt(B))
	} else {
		b1.low = b1*(sqrt(B + 1) + sqrt(B))
		b1.up = b1*(sqrt(B + 1) - sqrt(B))
	}
	if (mean(X) >= 0) {
		b0.low = mean(Y) - b1.up*mean(X)
		b0.up = mean(Y) - b1.low*mean(X)
	} else {
		b0.low = mean(Y) - b1.low*mean(X)
		b0.up = mean(Y) - b1.up*mean(X)
	}
	out = list(reg.coef=c(b0, b1), conf.int=rbind(c(b0.low, b0.up), c(b1.low, b1.up)), r.squared=r^2)
	return(out)
}
		


# Model biases and errors:
model.bias.error = function(obs, mod) {
	# This function calculates various statistical measures used in model performance evaluation, given observed ('obs') and modeled ('mod') results.
	MAT = na.omit(cbind(obs, mod))
	obs = MAT[,1]; mod = MAT[,2]
	n = length(obs)
	# Mean bias:
	MB = sum(mod - obs)/n
	# Normalized mean bias:
	NMB = sum(mod - obs)/sum(obs)*100
	# Normalized mean error:
	NME = sum(abs(mod - obs))/sum(obs)*100
	# Root mean square error:
	RMSE = sqrt(sum((mod - obs)^2)/n)
	out = list(MB=MB, NMB=NMB, NME=NME, RMSE=RMSE)
	return(out)
}
	
	
