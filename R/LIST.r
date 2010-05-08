LIST = function (..., resolve=TRUE, call=sys.call () )
{	#robust option?
	objs = list (...)
	if (resolve)
	{	n = length (objs)
		names = names (objs)
		if (is.null (names) ) names = rep ("", n)
		args = match.call (call=call) [-1]
		for (i in iter (n) )
		{	if (names [i] == "" && is.name (args [[i]]) )
				names [i] = as.character (args [[i]])
		}
		names (objs) = names
	}
	#note: can't use extend here
	#will cause infinite recursion
	structure (objs, class=c ("LIST", "list") )
}

#use double brackets instead?
#would need to return LIST instead of list
is.LIST = function (obj) inherits (obj, "LIST")
"[.LIST" = function (obj, ...) obj [[...]]
"[<-.LIST" = function (obj, ..., value) {obj [[...]] = value; obj}



