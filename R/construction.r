extend = function (obj, subclass, ..., attributes)
{	if (missing (attributes) )
	{	if (is.cleancall () ) attributes = LIST (..., call=sys.call () [-(2:3)])
		else stop ("extend attribute list can't include dots")
	}
	if (!inherits (obj, subclass) ) class (obj) = c (subclass, class (obj) )
	if (ifst (attributes) ) implant (obj, attributes=attributes) else obj
}

implant = function (obj, ..., attributes)
{	if (missing (attributes) )
	{	if (is.cleancall () ) attributes = LIST (..., call=sys.call () [-2])
		else stop ("implant attribute list can't include dots")
	}
	if (ifst (attributes) )
	{	strs = names (attributes)
		if (is.list (obj) )
			for (i in itobj (attributes) )
			{	n = length (obj) + 1
				obj [[n]] = attributes [i]
				names (obj) [n] = strs [i]
			}
		else if (is.environment (obj) )
			for (i in itobj (attributes) )
				assign (strs [i], attributes [i], obj)
		else if (inherits (obj, "FUNCTION") )
			for (i in itobj (attributes) )
				assign (strs [i], attributes [i], environment (obj) )
		else for (i in itobj (attributes) )
				attr (obj, strs [i]) = attributes [i]
	}
	obj
}

is.cleancall = function ()
{	k = sys.call (-1)
	clean = TRUE
	n = length (k)
	for (i in iter (n, 2) ) if (as.character (k [i]) == "...") clean = FALSE
	clean
}








