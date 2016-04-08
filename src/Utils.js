// module Utils

"use strict";

var Clipboard = require('clipboard');

exports.initClipboard = function (selector) {
  return function () {
    new Clipboard(selector);
  };
};

exports.createEventImpl = function (type) {
  return new Event(type);
};

exports.createErrorEventImpl = function (type) {
  return function (msg) {
    return new CustomEvent(type, {"detail": msg});
  };
};

exports.errorEventDetailImpl = function (e) {
  return e.detail;
};

exports.getInputFileListImpl = function(id) {
  return function() {
    var inp = document.getElementById(id);
    if ("files" in inp) {
      if (inp.files.length > 0) {
        return inp.files;
      } else {
        return null;
      }
    } else {
      return null;
    }
  };
};
