/* global exports, console */
"use strict";

// module Control.Monad.Eff.Console2

exports.info = function (s) {
  return function () {
    console.info(s);
    return {};
  };
};

exports.warn = function (s) {
  return function () {
    console.warn(s);
    return {};
  };
};
