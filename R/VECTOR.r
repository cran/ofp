#some problems...
MATRIX = function (x, ..., call, nr=length (x), nc=1)
{	if (missing (call) ) call = sys.call () [-2]
	x = as.vector (x)
	d = c (nr, nc)
	if (is.character (x) )
		.VECTOR (x, "TEXT", "character", call, dimension=d, ...)
	else if (is.numeric (x) )
		.VECTOR (x, "REAL", "numeric", call, dimension=d, ...)
	else if (is.complex (x) )
		.VECTOR (x, "COMPLEX", "complex", call, dimension=d, ...)
	else if (is.integer (x) )
		.VECTOR (x, "INTEGER", "integer", call, dimension=d, ...)
	else if (is.logical (x) )
		.VECTOR (x, "LOGICIAL", "logical", call, dimension=d, ...)
	else stop ("couldn't create matrix")
}

#note, can't have attribute called x
TEXT = function (x, ..., call, dimension)
{	if (missing (call) ) call = sys.call () [-2]
	if (!missing (x) && !is.character (x) ) x = as.character (x)
	.VECTOR (x, "TEXT", "character", call, dimension, ...)
}

REAL = function (x, ..., call, dimension)
{	if (missing (call) ) call = sys.call () [-2]
	if (!missing (x) && !is.numeric (x) ) x = as.numeric (x)
	.VECTOR (x, "REAL", "numeric", call, dimension, ...)
}

COMPLEX = function (x, ..., call, dimension)
{	if (missing (call) ) call = sys.call () [-2]
	if (!missing (x) && !is.complex (x) ) x = as.complex (x)
	.VECTOR (x, "COMPLEX", "complex", call, dimension, ...)
}

INTEGER = function (x, ..., call, dimension)
{	if (missing (call) ) call = sys.call () [-2]
	if (!missing (x) && !is.integer (x) ) x = as.integer (x)
	.VECTOR (x, "INTEGER", "integer", call, dimension, ...)
}

LOGICAL = function (x, ..., call, dimension)
{	if (missing (call) ) call = sys.call () [-2]
	if (!missing (x) && !is.logical (x) ) x = as.logical (x)
	.VECTOR (x, "LOGICIAL", "logical", call, dimension, ...)
}

.VECTOR = function (x, field, fieldst, call, dimension, ...)
{	if (missing (call) ) call = sys.call () [-(2:6)]
	if (!is.cleancall (call) ) stop ("couldn't create vector")
	if (missing (dimension) ) dimension = 0
	nd = length (dimension)
	if (missing (x) )
	{	x = vector (fieldst, prod (dimension) )
		if (nd > 1) dim (x) = dimension
	}
	inter = "VECTOR"
	if (nd == 2) inter = c ("MATRIX", inter, "matrix")
	extend (x, c (field, inter), attributes=LIST (..., call=call) )
}

is.VECTOR = function (x) inherits (x, "VECTOR")
is.MATRIX = function (x) inherits (x, "MATRIX")
is.TEXT = function (x) inherits (x, "TEXT")
is.REAL = function (x) inherits (x, "REAL")
is.COMPLEX = function (x) inherits (x, "COMPLEX")
is.INTEGER = function (x) inherits (x, "INTEGER")
is.LOGICAL = function (x) inherits (x, "LOGICAL")

"[.VECTOR" = function (x, ...)
{	y = unclass (x) [...]
	d = dim (y)
	attributes (y) = attributes (x)
	dim (y) = d
	y
}

"$.VECTOR" = function (x, name) attr (x, name)
"$<-.VECTOR" = function (x, name, value)
{	attr (x, name) = value
	x
}

print.VECTOR = function (x, ...)
{	cat (class (x) [1], "\n")
	if (is.MATRIX (x) ) print (as.matrix (x) )
	else print (as.vector (x) )
	z = attributes (x)
	z$class = NULL
	z$dim = NULL
	if (length (z) > 0)
	{	z = names (z)
		cat ("attributes:\n")
		cat (z)
		cat ("\n")
	}
}

#"+.TEXT" = function (a, b) paste (a, b, sep="")



