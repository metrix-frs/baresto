// module Lib.Validation

"use strict";

exports.fastPatch = function (patch) {
  return function (current) {
    var m = {};
    for (var k in current) {
      if (current.hasOwnProperty(k)) {
        if (current[k].length > 0) {
          m[k] = current[k];
        }
      }
    }
    for (var k in patch) {
      if (patch.hasOwnProperty(k)) {
        if (patch[k].length > 0) {
          m[k] = patch[k];
        } else {
          delete m[k];
        }
      }
    }
    return m;
  };
};
