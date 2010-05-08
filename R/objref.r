objref = function (obj)
{	f = function () .objref.image ()
	extend (FUNCTION (f, obj), "objref")
}

print.objref = function (ref, ...) cat ("objref:", class (ref () ) [[1]], "\n", sep="")
mutate.objref = function (ref, obj, ...) environment (ref)$obj = obj
length.objref = function (x) length (environment (x)$obj)
.objref.image = function () environment (sys.function (-1) )$obj
"[.objref" = function (ref, ...) environment (ref)$obj [...]
"[[.objref" = function (ref, ...) environment (ref)$obj [[...]]

"[<-.objref" = function (ref, ..., value)
{	e = environment (ref)
	e$obj [...] = value
	ref
}

"[[<-.objref" = function (ref, ..., value)
{	e = environment (ref)
	e$obj [[...]] = value
	ref
}


