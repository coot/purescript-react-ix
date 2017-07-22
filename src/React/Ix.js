"use strict"

exports.unsafeSetImpl = function(l, v, o) {
  return o[l] = v, o
}

exports.unsafeGetImpl = function(l, o) {
  return o[l]
}

exports.unsafeInsertImpl = function(l, v, o) {
  return o[l] = v, o
}

exports.unsafeNullifyImpl = function(l, o) {
  return o[l] = null, o
}
