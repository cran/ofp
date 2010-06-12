LIST = function (..., call=sys.call () )
{	objs = list (...)
	if (!is.cleancall (call) ) stop ("couldn't create list")
	n = length (objs)
	names = names (objs)
	if (is.null (names) ) names = rep ("", n)
	args = match.call (call=call) [-1]
	for (i in iter (n) )
	{	if (names [i] == "" && is.name (args [[i]]) )
			names [i] = as.character (args [[i]])
	}
	names (objs) = names
	#note, can't use extend here, causes infinite recursion
	structure (objs, class=c ("LIST", "list") )
}

is.LIST = function (object) inherits (object, "LIST")

clone.list = function (object, ...)
{	for (i in 1:length (object) )
		object [[i]] = clone (object [[i]])
	object
}

#return LIST instead of list?
#"[.LIST" = function (obj, ...) obj [...]



