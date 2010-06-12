evalt = function (text)
	for (k in text) eval (parse (text=k), sys.frame (-1) )

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

