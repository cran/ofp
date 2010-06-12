#mask functions
#not sure about length
print = function (...) base::print (...)
summary = function (...) base::summary (...)
format = function (...) base::format (...)
plot = function (...) graphics::plot (...)
lines = function (...) graphics::lines (...)
points = function (...) graphics::points (...)

#new
clone = function (...) UseMethod ("clone")
preview = function (...) UseMethod ("prevew")
clone.default = function (object, ...) object
preview.default = function (x, ...) x


