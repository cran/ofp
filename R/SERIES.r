#regular evaluation only
#regular assumed, if minor irregularity
SERIES = function (x, y, regular=TRUE, f=series1)
{	n = length (x)
	if (n < 2)
		stop ("SERIES must have at least two points")
	if (missing (y) )
	{	y = x
		x = 1:n
	}
	else
	{	if (n != length (y) )
			stop ("SERIES x and y values must be same length")
		if (regular)
		{	dx = sum (diff (x) < 0.000001)
			if (length (dx) > 1)
				stop ("SERIES x values irregularly spaced")
		}
		else f = function (...) NA
	}
	extend (FUNCTION (f), "SERIES", n, x, y)
}

#if u provided, assume irregular
SMOOTH = function (x, y, n=10, degree=2, wf=.wf.std, smoothness, u)
{	regular = TRUE
	if (missing (u) )
		u = seq (min (x), max (x), length=n)
	else
	{	regular = FALSE
		n = length (u)
	}

	m = NULL
	for (j in 0:degree)
		m = cbind (m, x ^ j)
	v = rep (as.numeric (NA), n)

	if (missing (smoothness) )
		smoothness = 0.67 * diff (range (x) )
	hs = smoothness / 2

	for (i in 1:n)
	{	p = u [i]
		k = x >= p - hs & x <= p + hs

		xsub = x [k]
		ysub = y [k]
		msub = m [k,]
		w = wf (p, smoothness, xsub)

		v [i] = sum (lm.wfit (msub, ysub, w)$coefficients * p ^ (0:degree) )
	}
	
	f = SERIES (u, v)
	extend (f, "SMOOTH", regular)
}

print.SERIES = function (f, ...) cat ("SERIES\n")
print.SMOOTH = function (f, ...) cat ("SMOOTH\n")

plot.SERIES = function (f, ...) plot (f$x, f$y, type="l", ...)
lines.SERIES = function (f, ...) lines (f$x, f$y, ...)
points.SERIES = function (f, ...) points (f$x, f$y, ...)

#maybe return NAs, for u values outside domain
series1 = function (u)
{	r = (n - 1) * (u - x [1]) / (x [n] - x [1]) + 1
	r [r < 1] = 1
	r [r >= n] = n

	rf = floor (r)
	k = (r != rf)
	a = b = y [rf]
	b [k] = y [rf + 1][k]
	p = r - rf

	(1 - p) * a + p * b
}

.wf.std = function (m, w, x)
{	if (m != 0) x = x - m
	z = 2 * x / w
	1 - z * z
}


