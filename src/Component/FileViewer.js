// module Component.FileViewer

"use strict";

var Queue = function () {
  this.queue = [];
  this.callback = null;
  this.running = false;
};

Queue.prototype.push = function (a) {
  this.queue.push(a);
  this.trigger();
};

Queue.prototype.trigger = function () {
  if (this.callback) {
    var elem = this.queue.shift();
    if (elem) {
      this.callback(elem)();
    }
  }
};

Queue.prototype.register = function (cb) {
  this.callback = cb;
};

exports.newQueue = function () {
  return new Queue();
};

exports.registerQueue = function (queue) {
  return function (cb) {
    return function () {
      return queue.register(cb);
    };
  };
};

exports.pushQueue = function (queue) {
  return function (elem) {
    return function () {
      return queue.push(elem);
    };
  };
};

exports.triggerQueue = function (queue) {
  return function () {
    return queue.trigger();
  };
};
