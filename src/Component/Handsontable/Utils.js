"use strict";

// module Component.Handsontable.Utils

exports.attachClickHandler = function(selector) {
  return function(callback) {
    var nodeList = document.querySelectorAll(selector);
    for (var el in nodeList) {
      el.addEventListener("click", callback);
    }
  };
};

exports.forceString = function(val) {
  return val.toString();
};
