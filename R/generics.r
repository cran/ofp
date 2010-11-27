print = function (...) base::print (...)
summary = function (...) base::summary (...)
format = function (...) base::format (...)
plot = function (...) graphics::plot (...)
lines = function (...) graphics::lines (...)
points = function (...) graphics::points (...)

fit = function (...) UseMethod ("fit")
clone = function (...) UseMethod ("clone")
preview = function (...) UseMethod ("preview")

clone.default = function (x, ...) x
preview.default = function (x, ...) x

