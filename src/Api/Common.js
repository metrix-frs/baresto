"use strict";

// module Api.Common

exports.filesToFormData = function(files) {
  var formData = new FormData();
  for (var i = 0; i < files.length; i++) {
    formData.append("file", files[i], files[i].name);
  }
  return formData;
};
