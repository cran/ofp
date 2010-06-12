extend = function (object, subclass, ..., attributes)
{	if (missing (attributes) )
	{	if (is.cleancall () ) attributes = LIST (..., call=sys.call () [-(2:3)])
		else stop ("extend attribute list can't include dots")
	}
	if (!inherits (object, subclass) ) class (object) = c (subclass, class (object) )
	if (ifst (attributes) ) implant (object, attributes=attributes) else object
}

implant = function (object, ..., attributes)
{	if (missing (attributes) )
	{	if (is.cleancall () ) attributes = LIST (..., call=sys.call () [-2])
		else stop ("implant attribute list can't include dots")
	}
	if (ifst (attributes) )
	{	names = names (attributes)
		for (i in 1:length (attributes) )
			evalt (paste ("object$", names [i], "=attributes[[i]]") )
	}
	object
}

is.cleancall = function (call=sys.call (-1) )
{	clean = TRUE
	n = length (call)
	for (i in iter (n, 2) ) if (as.character (call [i]) == "...") clean = FALSE
	clean
}

