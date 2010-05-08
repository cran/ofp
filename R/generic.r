#mask functions
#create a better system (maybe only create mask where needed)...
#not sure about length
print = function (...) base::print (...)
summary = function (...) base::summary (...)
format = function (...) base::format (...)
plot = function (...) graphics::plot (...)
lines = function (...) graphics::lines (...)
points = function (...) graphics::points (...)
mean = function (...) base::mean (...)
fitted = function (...) stats::fitted (...)
residuals = function (...) stats::residuals (...)

#get rid of...?
as.list = function (...) base::as.list (...)
as.data.frame = function (...) base::as.data.frame (...)

#new
clone = function (...) UseMethod ("clone")
clone.default = function (obj, ...) obj
mutate = function (...) UseMethod ("mutate")


