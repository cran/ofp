preview.TABLE = function (x, n=5, ...) .preview2d (x, n)
preview.MATRIX = function (x, n=5, ...) .preview2d (as.matrix (x), n)
preview.data.frame = function (x, n=5, ...) .preview2d (x, n)
preview.matrix = function (x, n=5, ...) .preview2d (x, n)

preview.VECTOR = function (x, n=5, ...)
{	x = as.vector (x)
	nx = length (x)
	if (nx <= 2 * n) print (x)
	else
	{	cat ("first\n")
		print (x [1:n])
		cat ("last\n")
		print (x [(nx - n + 1):nx])
	}
}

.preview2d = function (x, n=5)
{	nx = nrow (x)
	if (nx <= 2 * n) print (x)
	else
	{	cat ("first\n")
		print (x [1:n,])
		cat ("last\n")
		print (x [(nx - n + 1):nx,])
	}
}


