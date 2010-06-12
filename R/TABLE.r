TABLE = function (..., call=sys.call () )
	d = extend (LIST (..., call=call), "TABLE")

is.TABLE = function (d) inherits (d, "TABLE")

#change to d (requires mask)
dim.TABLE = function (x) c (length (x [[1]]), length (x) )

#issues... (e.g. maybe i or j only)
"[.TABLE" = function (obj, i, j, ...) obj [[j]][i]

print.TABLE = function (d, ...)
{	dst = as.data.frame (d)
	print (dst)
}

#todo: rewrite
summary.TABLE = function (d, ...)
{	d = as.data.frame (d)
	n = nrow (d)
	s = length (d)
	ifactor = inumeric  = iother = numeric ()
	for (i in 1:s)
	{	if (inherits (d [[i]], "factor") ) ifactor = c (ifactor, i)
		else if (inherits (d [[i]], "integer") || inherits (d [[i]], "numeric") )
			inumeric = c (inumeric, i)
		else iother = c (iother, i)
	}
	if (ifst (ifactor) )
	{	cat ("factors:  ")
		for (i in ifactor)
		{	cat (names (d) [i], " in {", sep="")
			levs = levels (d [[i]])
			levs = if (length (levs) > 6)
				paste (paste (levs [1:6], collapse=", "), ", ...", sep="")
			else paste (levs, collapse=", ")
			cat (levs, "}\n", sep="")
			if (i != ifactor [length (ifactor)]) cat ("          ")
		}
	}
	if (ifst (inumeric) )
	{	cat ("numerics: ")
		for (i in inumeric)
		{	cat (names (d) [i], " in (", sep="")
			x = d [[i]]
			cat (min (x, na.rm=TRUE), ", ", max (x, na.rm=TRUE), ")\n", sep="")
			if (i != inumeric [length (inumeric)]) cat ("          ")
		}
	}
	if (ifst (iother) )
	{	cat ("others:   ")
		cat (paste (names (d) [iother], collapse=", "), "\n")
	}
	v = numeric ()
	valid = rep (TRUE, n)
	for (i in 1:s)
	{	validi = is.finite (d [[i]])
		v [i] = sum (validi)
		valid = valid & validi
	}
	nv = sum (valid)
	flag = if (n == nv) "(clean) " else "(missing/corrupt) "
	cat ("\ntable ", flag, n, ", ", s, "\n", sep="")
	if (n < 11) print (d)
	else print (d [c (1:3, (n - 2):n),])
	
	if (n > nv)
	{	v = data.frame (matrix (v, nc=s), nv, row.names="nv")
		names (v) = c (names (d), "overall")
		cat ("\n")
		print (v)
	}

}


