collection = function (..., resolve=TRUE, call=sys.call () )
{	objs = list (...)
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
	as.collection (objs)
}

is.collection = function (obj) inherits (obj, "collection")
as.collection = function (obj) extend (as.component (obj), "collection")
"[.collection" = function (obj, ...) obj [[...]]
"[<-.collection" = function (obj, ..., value) {obj [[...]] = value; obj}

compenv = function (..., hash=FALSE)
{	objs = if (is.cleancall () ) collection (..., call=sys.call () )
	else collection (..., resolve=FALSE)
	if (ifst (objs) ) list.to.compenv (objs, hash)
	else structure (extend (as.component (new.env (hash) ), "compenv"), hash=hash)
}

is.compenv = function (e) inherits (e, "compenv")

as.compenv = function (obj)
{	if (is.compenv (obj) ) obj
	else if (inherits (obj, "environment") ) extend (as.component (obj), "compenv")
	else if (inherits (obj, "list") ) list.to.compenv (obj)
	else stop ("as.compenv not applicable")
}

"==.compenv" = function (e1, e2) (format (e1) == format (e2) )

print.compenv = function (e, ...)
{	obj = as.list (e)
	if (if0 (obj) ) cat ("empty compenv\n")
	else for (i in itobj (obj) )
	{	cat ("$", names (obj) [i], "\n", sep="")
		if (is.compenv (obj [[i]]) ) cat (format (obj [[i]]), "\n" )
		else print (obj [[i]])
	}
}

list.to.compenv = function (obj, hash=FALSE)
{	names = names (obj)
	if (is.null (names) || any (names == "") )
		stop ("compenv args must be named (or nameable)")
	e = compenv (hash=hash)
	for (i in itobj (obj) ) assign (names [i], obj [[i]], envir=e)
	e
}

clone.compenv = function (e, ...)
	structure (as.compenv (compenv.clone (e, ...) ), hash=is.hashed (e) )

compenv.clone = function (e, flags=objref (list () ) )
{	f = new.env ()
	flags [[length (flags) + 1]] = list (e, f)
	if (length (e) > 0)
	{	strs = ls (e)
		for (str in strs)
		{	x = get (str, envir=e)
			if (inherits (x, "environment") )
			{	flagged = NULL
				for (flag in flags () ) if (`==.compenv` (x, flag [[1]]) ) flagged = flag [[2]]
				if (is.null (flagged) ) assign (str, clone (x, flags), envir=f)
				else assign (str, flagged, envir=f)
			}
			else assign (str, x, envir=f)
		}
	}
	f
}

mclass = function (super, ..., hybrid=TRUE, abstract=FALSE)
{	cc = sys.call (-1)
	cid = as.character (cc [[1]])
	generate = TRUE
	if (cid == ".mclass.init")
	{	cid = as.character (cc [[2]])
		generate = FALSE
	}
	if (missing (super) ) super = "MObject"
	else
	{	sid = as.character (sys.call () [[2]])
		if (!.mclass.exists (sid) )
		{	.mclass.init = function (x) NULL
			body (.mclass.init) = body (super)
			eval (as.call (parse (text=paste (".mclass.init(", sid, ")") ) [[1]]) )
		}
		super = sid
	}
	cs = cobj = NULL
	if (.mclass.exists (cid) ) cobj = .mclass.get (cid)
	else
	{	cd = sys.function (-1)
		cs = .mclass.parse (cid, cd)
		cobj = .mclass.generate (cid, super, abstract, cs$constructor, cs$mid, cs$methods)
	}
	if (generate)
	{	if (cobj$abstract) stop (paste ("class", cid, "is abstract") )
		oobj = .mobject.generate (cid, hybrid)
		if (ifst (cobj$constructor) )
		{	mc = match.call (cobj$constructor, cc)
			mc [[1]] = as.name (".oosp.constructor")
			.oosp.constructor <<- cobj$constructor
			cobj$context = oobj
			eval (mc, sys.frame (-2) )
		}
		else
		{	mc = match.call (sys.function (-1), cc)[-1]
			ss = names (mc)
			for (i in itobj (mc) )
				if (ss [i] != "") assign (ss [i], mc [[i]], envir=oobj)
		}
		oobj
	}
}

.mclass.parse = function (cid, cd)
{	constructor = NULL
	mid = character ()
	methods = list ()
	b = as.list (body (cd) )
	for (expr in b)
	{	if (class (expr) == "=")
		{	right = expr [[3]]
			if (class (right) == "call")
			{	id = as.character (expr [[2]])
				f = eval (right)
				if (cid == id) constructor = f
				else
				{	mid = c (mid, id)
					methods [[length (methods) + 1]] = f
				} 
			}
		}
	}
	collection (constructor, mid, methods)
}

.mclass.generate = function (cid, super=NULL, abstract=FALSE,
	constructor=NULL, mid=character (), methods=list () )
{	if (ifst (super) ) super = .mclass.get (super)
	context = NA
	attributes = compenv ()
	methodenv = compenv ()
	cobj = extend (compenv (cid, super, abstract, context, attributes,
		mid, methods=methodenv), "mclass")
	if (ifst (constructor) )
	{	constructor = .mclass.constructor (constructor)
		environment (constructor) = cobj
	}
	cobj$constructor = constructor
	for (i in itobj (mid) )
	{	f = .mclass.method (methods [[i]])
		environment (f) = cobj
		assign (mid [i], f, envir=methodenv) 
	}
	.mclass.add (cobj)
	invisible (cobj)
}

.mclass.exists = function (cid) any (cid == .oosp.cid)
.mclass.get = function (cid) .oosp.image [[which (cid ==.oosp.cid)]]
.mclass.add = function (cobj)
{	n = length (.oosp.cid) + 1
	.oosp.cid [n] <<- cobj$cid
	.oosp.image [[n]] <<- cobj
}

mdelete = function (cid)
{	i = (cid == .oosp.cid)
	.oosp.cid <<- .oosp.cid [!i]
	.oosp.image <<- .oosp.image [!i]
}

.mobject.generate = function (cid, hybrid=TRUE)
{	cobj = .mclass.get (cid)
	oobj = compenv (.class=cobj)
	if (hybrid)
	{	classes = cid
		while (ifst (cobj$super) )
		{	cobj = cobj$super
			classes = c (classes, cobj$cid)
		}
		class (oobj) = classes
	}
	else class (oobj) = "MObject"
	oobj
}

is.MObject = function (obj) inherits (obj, "MObject")

print.MObject = function (obj, ...) cat ("MObject:", obj$.class$cid, "\n")

#duplication
#todo: fix
.mclass.method = function (f)
{	g = list ()
	g [[1]] = as.name ("{")
	g [[2]] = parse (text=".=environment(sys.function())$context") [[1]]
	g [[3]] = parse (text="..=environment(sys.function())$attributes") [[1]]
	b = body (f)
	if (class (b) == "{")
	{	b = as.list (b) [-1]
		for (i in itobj (b) ) g [[i + 3]] = b [[i]]
	}
	else g [[4]] = b
	body (f) = as.call (g)
	structure (f, class="Method")
}

.mclass.constructor = function (f)
{	g = list ()
	g [[1]] = as.name ("{")
	g [[2]] = parse (text="super=.mobject.super") [[1]]
	g [[3]] = parse (text=".=environment(sys.function())$context") [[1]]
	g [[4]] = parse (text="..=environment(sys.function())$attributes") [[1]]
	b = body (f)
	if (class (b) == "{")
	{	b = as.list (b) [-1]
		for (i in itobj (b) ) g [[i + 4]] = b [[i]]
	}
	else g [[5]] = b
	body (f) = as.call (g)
	structure (f, class="Method")
}

.mobject.super = function (...)
{	cobj = environment (sys.function (-1) )
	cobj$super$context = cobj$context
	cobj$super$constructor (...)
}

.mclass.getmethod = function (cobj, mid)
{	i = which (mid == cobj$mid)
	while (length (i) == 0)
	{	super = cobj$super
		if (is.null (super) ) stop (paste ("method", mid, "not found") )
		cobj = super
		i = which (mid == cobj$mid)
	}
	get (mid, envir=cobj$methods)
}

#this function is intended to be called by the invocation operator only
.mobject.invoke = function (obj, mid, invocation)
{	invocation [[1]] = as.name (".oosp.method")
	.oosp.method <<- .mclass.getmethod (obj$.class, mid)
	obj$.class$context = obj
	eval (invocation, sys.frame (-3) )
}

#invocation.on = function () assign ("~", .invocation.operator, envir=.GlobalEnv)
#invocation.off = function () assign ("~", base::`~`, envir=.GlobalEnv)

#UseMethod fails
#need the double-up, not sure why
"~" = function (obj, invocation)
{	if (missing (invocation) ) `~.default` (obj, invocation)
	else if (is.MObject (obj) )
	{	invocation = deparse (substitute (invocation) )
		`~.MObject` (obj, invocation)
	}
	else `~.default` (obj, invocation)
}

"~.MObject" = function (obj, invocation)
{	obj = eval (obj)
	invocation = as.call (parse (text=invocation) [[1]])
	mid = as.character (invocation [[1]])
	.mobject.invoke (obj, mid, invocation)
}

"~.default" = function (obj, ...)
{	call = sys.call (-1)
	call [[1]] = as.name ("~")
	e = sys.frame (-2)
	f = new.env ()
	environment (f) = e
	assign ("~", base::`~`, envir=f)
	obj = eval (call, envir=f)
	environment (obj) = e
	obj
}

freemethod = function (f, ...)
{	objs = if (is.cleancall () ) collection (..., call=sys.call () [-2])
	else collection (..., resolve=FALSE)
	g = list ()
	g [[1]] = as.name ("{")
	g [[2]] = parse (text=".=environment(sys.function())") [[1]]
	b = body (f)
	if (class (b) == "{")
	{	b = as.list (b) [-1]
		for (i in itobj (b) ) g [[i + 2]] = b [[i]]
	}
	else g [[3]] = b
	body (f) = as.call (g)
	environment (f) = as.compenv (objs)
	structure (as.component (f), class="freemethod")
}

hypermethod = function (obj, mid)
{	m = .mclass.getmethod (obj$.class, mid)
	f = function (...)
	{	.$obj$.class$context = .$obj
		.$m (...)
	}
	extend (freemethod (f, obj, m), "hypermethod")
}

print.freemethod = function (f, ...)
{	environment (f) = .GlobalEnv
	attributes (f) = NULL
	print.default (f)
}







