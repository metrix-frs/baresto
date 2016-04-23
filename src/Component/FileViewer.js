// module Component.FileViewer

"use strict";

var Queue = function () {
  this.queue = [];
  this.callback = null;
  this.running = false;
};

Queue.prototype.push = function (a) {
  this.queue.push(a);
  if (!this.running) {
    this.nextElem();
  }
};

Queue.prototype.nextElem = function () {
  this.running = false;
  if (this.callback) {
    var elem = this.queue.shift();
    if (elem) {
      this.running = true;
      this.callback(elem)();
    }
  }
  return this.running;
};

Queue.prototype.register = function (cb) {
  this.callback = cb;
};

Queue.prototype.unregister = function () {
  this.callback = null;
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

exports.unregisterQueue = function (queue) {
  return function () {
    return queue.unregister();
  };
};

exports.pushQueue = function (queue) {
  return function (elem) {
    return function () {
      return queue.push(elem);
    };
  };
};

exports.nextElemQueue = function (queue) {
  return function () {
    return queue.nextElem();
  };
};
