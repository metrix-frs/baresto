'use strict'

// module Component.Handsontable.Options

var Handsontable = require('handsontable')

exports.renderSetClass = function (cls) {
  return function (instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments)
    td.className = cls
  }
}

exports.renderer = function (name) {
  return name
}

exports.renderHtml = function (cls) {
  return function (instance, td, row, col, prop, value, cellProperties) {
    var html = Handsontable.helper.stringify(value)
    td.innerHTML = html
    td.className = cls
    return td
  }
}

exports.borderImpl = function (r1, c1, r2, c2, top, right, bot, left) {
  var obj = {
    range: {
      from: {
        row: r1,
        col: c1
      },
      to: {
        row: r2,
        col: c2
      }
    }
  }
  if (top != null) {
    obj.top = top
  }
  if (right != null) {
    obj.right = right
  }
  if (bot != null) {
    obj.bottom = bot
  }
  if (left != null) {
    obj.left = left
  }
  return obj
}

exports.colPropEmpty = {}

exports.colPropWidth = function (w) {
  return {
    width: w
  }
}
