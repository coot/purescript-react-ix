"use strict"

exports.unsafeListPropsImpl = function(o) {
  return Object.entries(o).map(function(x) {return {key: x[0], value: x[1]}})
}
