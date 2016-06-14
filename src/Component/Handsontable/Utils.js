'use strict'

// module Component.Handsontable.Utils

exports.attachClickHandler = function (hot) {
  return function (selector) {
    return function (callback) {
      var attach = function (callback) {
        var nodeList = document.querySelectorAll(selector)
        if (nodeList[1]) {
          nodeList[1].addEventListener('click', callback)
        }
      }
      return function () {
        hot.addHook('afterRender', function (forced) {
          setTimeout(function () {
            attach(callback)
          }, 200)
        })
      }
    }
  }
}

exports.forceString = function (val) {
  return val.toString()
}
