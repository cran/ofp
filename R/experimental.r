#todo: redesign
ifst = function (obj) (!is.null (obj) && length (obj) > 0)
if0 = function (obj) (!is.null (obj) && length (obj) == 0)
if1 = function (obj) (ifst (obj) && length (obj) == 1)
iter = function (n, min=1) if (n < min) numeric () else min:n
itobj = function (obj) if (ifst (obj) ) 1:length (obj) else numeric ()

#todo: redesign
datafile = function (pkg, file, global=FALSE, preview=FALSE, ...)
{	path = if (missing (pkg) ) "" else paste (.find.package (pkg), "/data/", sep="")
	path = paste (path, file, sep="")
	if (!file.exists (path) ) path = paste (path, ".csv", sep="")
	if (!file.exists (path) ) stop ("file does not exist")
	d = read.csv (path, ...)
	if (global) for (i in itobj (d) ) assign (names (d) [i], d [[i]], envir=.GlobalEnv)
	if (preview) preview (d)
	if (global || preview) invisible (d) else d
}

