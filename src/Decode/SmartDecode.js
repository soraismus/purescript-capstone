"use strict";

exports.unsafeCreate = function (label) {
  return function (value) {
    var record = {};
    record[label] = value;
    return record;
  };
};
