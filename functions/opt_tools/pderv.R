pderv = function(iVar, fcn, x, f0, ...){

# Calculates partial derivative
# Pulled out of numgrad to allow an easier multi-core implementation

delta = 1e-5 #can be adjusted
x[iVar] = x[iVar] + delta

g0 = (fcn(x, ...) - f0) / (delta)

#checking for very large gradient value
if (max(abs(g0) < 1e50)) {
	derv = as.vector(g0)
} else {
	c(derv) = NaN #all values set to NaN
#	dervbad = TRUE
	cat("bad gradient ------------------------\n")
}

return(derv)
}