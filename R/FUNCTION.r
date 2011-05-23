FUNCTION = function (f, ..., call)
{	if (missing (f) ) f = function () NULL
	if (missing (call) ) call = sys.call () [-2]
	if (!is.cleancall (call) ) stop ("couldn't create function")
	e = ENVIRONMENT ()
	environment (f) = e
	e$. = e
	extend (f, "FUNCTION", attributes=LIST (..., call=call) )
}

is.FUNCTION = function (f) inherits (f, "FUNCTION")

definef = function (f, def, ...)
{	b = NULL
	n = length (def)
	if (n == 1) b = parse (text=def)
	else if (n > 1)
	{	b = list ()
		b [[1]] = as.name ("{")
		for (i in 1:n) b [[i + 1]] = parse (text=def [i]) [[1]]
		b = as.call (b)
	}
	body (f) = b
	f
}

extendf = function (f, subclass, g, ..., attributes)
{	if (missing (attributes) )
	{	if (is.cleancall () ) attributes = LIST (..., call=sys.call () [-(2:4)])
		else stop ("extendf attribute list can't include dots")
	}
	e = environment (f)
	z = attributes (f)
	body (f) = body (g)
	environment (f) = e
	attributes (f) = z
	extend (f, subclass, attributes=attributes)
}


#could cause infinite recursion
#cloning regular functions with environments?
clone.FUNCTION = function (f, ...)
{	g = f
	environment (g) = clone (environment (f) )
	g
}

"$.FUNCTION" = function (f, name) get (name, environment (f) )
"$<-.FUNCTION" = function (f, name, value)
{	assign (name, value, environment (f) )
	f
}

print.FUNCTION = function (f, ...)
{	g = format (args (f) )
	n = length (g)
	cat ("FUNCTION")
	cat (substring (g [1], 9), "\n")
	if (n > 2) cat (g [-c (1, n) ], sep="\n")
	print (body (f) )
	x = ls (environment (f) )
	if (length (x) > 0)
	{	cat ("attributes:\n")
		cat (x)
		cat ("\n")
	}
}


