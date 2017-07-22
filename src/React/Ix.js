"use strict"

exports.unsafeGetImpl = function(l, o) {
  return o[l]
}

exports.unsafeSetImpl = function(l, v, o) {
  return o[l] = v, o
}

exports.unsafeModifyImpl = function(l, f, o) {
  return o[l] = f(o[l]), o
}

exports.unsafeInsertImpl = function(l, v, o) {
  return o[l] = v, o
}

exports.unsafeNullifyImpl = function(l, o) {
  return o[l] = null, o
}
