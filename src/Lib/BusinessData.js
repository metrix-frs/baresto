"use strict";

// module Lib.BusinessData

exports.stripDecimals = function(val) {
  return function(places) {
    var num = parseFloat(val);
    if (isNaN(num)) {
      return val;
    } else {
      return num.toFixed(places).toString();
    }
  };
};
