// module Types

"use strict";

exports.showDate = function (date) {
  return date.toLocaleString();
}

exports.showDayImpl = function (date) {
  return date.toLocaleDateString();
}
