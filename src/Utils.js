// module Utils

"use strict";

exports.createEventImpl = function (type) {
  return new Event(type);
};

exports.createCustomEventImpl = function (type) {
  return function (msg) {
    return new CustomEvent(type, {"detail": msg});
  };
};

exports.customEventDetailImpl = function (e) {
  return e.detail;
};
