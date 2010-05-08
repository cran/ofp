#simplify these constructors...
.VECTOR = function (x, field, fieldst, dimension)
{	if (missing (x) )
	{	x = vector (fieldst, prod (dimension) )
		if (length (dimension) > 1) dim (x) = dimension
	}
	extend (x, c (field, "VECTOR") )
}

#to many spaces
print.VECTOR = function (x, ...)
{	cat (class (x) [1], "\n")
	y = as.vector (x)
	dim (y) = dim (x)
	print (y)
	z = attributes (x)
	z$class = NULL
	z$dim = NULL
	if (ifst (z) )
	{	cat ("\n")
		print (z)
	}
}

LOGICAL = function (x, ..., dimension=0)
{	attributes = if (is.cleancall () ) LIST (..., call=sys.call () [-2])
	else LIST (..., resolve=FALSE)
	implant (.VECTOR (x, "LOGICIAL", "logical", dimension), attributes=attributes)
}

TEXT = function (x, ..., dimension=0)
{	attributes = if (is.cleancall () ) LIST (..., call=sys.call () [-2])
	else LIST (..., resolve=FALSE)
	implant (.VECTOR (x, "TEXT", "character", dimension), attributes=attributes)
}

INTEGER = function (x, ..., dimension=0)
{	attributes = if (is.cleancall () ) LIST (..., call=sys.call () [-2])
	else LIST (..., resolve=FALSE)
	implant (.VECTOR (x, "INTEGER", "integer", dimension), attributes=attributes)
}

REAL = function (x, ..., dimension=0)
{	attributes = if (is.cleancall () ) LIST (..., call=sys.call () [-2])
	else LIST (..., resolve=FALSE)
	implant (.VECTOR (x, "REAL", "numeric", dimension), attributes=attributes)
}

"$.VECTOR" = function (x, name) attr (x, name)
"$<-.VECTOR" = function (x, name, value)
{	attr (x, name) = value
	x
}

#warning: this may be inefficient
"+.TEXT" = function (a, b) paste (a, b, sep="")






