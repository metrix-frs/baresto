"use strict";

// module Component.Handsontable.Utils

exports.attachClickHandler = function(selector) {
  return function(callback) {
    var attach = function(callback) {
      var nodeList = document.querySelectorAll(selector);
      for (var i = 0; i < nodeList.length; i++) {
        nodeList[i].addEventListener("click", callback);
      }
    };
    return function() {
      attach(callback);
      setTimeout(function() {
        attach(callback);
      }, 1000);
    };
  };
};

exports.forceString = function(val) {
  return val.toString();
};
