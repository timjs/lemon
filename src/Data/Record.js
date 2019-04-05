"use strict";

exports.unsafeIntersectionFn = function(r1, r2) {
  var copy = {};
  for (var k in r2) {
    if ({}.hasOwnProperty.call(r1, k)) {
      copy[k] = r1[k];
    }
  }
  return copy;
};
