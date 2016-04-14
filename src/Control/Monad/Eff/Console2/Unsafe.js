/* global exports, console */
"use strict";

// module Control.Monad.Eff.Console2.Unsafe

exports.infoAny = function (s) {
  return function () {
    console.info(s);
    return {};
  };
};

exports.warnAny = function (s) {
  return function () {
    console.warn(s);
    return {};
  };
};
