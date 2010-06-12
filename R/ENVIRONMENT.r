ENVIRONMENT = function (..., call=sys.call (), hash=FALSE)
{	if (!is.cleancall (call) ) stop ("couldn't create environment")
	extend (new.env (hash), "ENVIRONMENT", attributes=LIST (..., call=call) )
}

is.ENVIRONMENT = function (e) inherits (e, "ENVIRONMENT")
is.hashed = function (e) attr (e, "hash")

"==.ENVIRONMENT" = function (e1, e2) (format (e1) == format (e2) )

#test further...
clone.ENVIRONMENT = function (e, ...)
{	f = ENVIRONMENT (hash=is.hashed (e) )
	.environment.clone (e, f, ...)
	f
}

#test further...
clone.environment = function (e, ...)
{	f = ENVIRONMENT ()
	.environment.clone (e, f, ...)
	unclass (f)
}

#note, functions assumed to have own environments
.environment.clone = function (e, f, flags=objref (list () ) )
{	flags [[length (flags) + 1]] = list (e, f)
	if (length (e) > 0)
	{	strs = ls (e)
		for (str in strs)
		{	x = get (str, envir=e)
			if (inherits (x, "environment") )
			{	flagged = NULL
				for (flag in flags () ) if (`==.ENVIRONMENT` (x, flag [[1]]) ) flagged = flag [[2]]
				if (is.null (flagged) ) assign (str, clone (x, flags), envir=f)
				else assign (str, flagged, envir=f)
			}
			else assign (str, clone (x), envir=f)
		}
	}
	f
}

print.ENVIRONMENT = function (e, ...)
{	obj = as.list (e)
	if (if0 (obj) ) cat ("empty ENVIRONMENT\n")
	else for (i in itobj (obj) )
	{	cat ("$", names (obj) [i], "\n", sep="")
		if (is.environment (obj [[i]]) ) cat (format (obj [[i]]), "\n" )
		else print (obj [[i]])
	}
}



