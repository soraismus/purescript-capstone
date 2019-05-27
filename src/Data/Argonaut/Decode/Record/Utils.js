"use strict";

exports.unsafeGetSubRecord = function (record0) {
  return function (record1) {
    var result = {};
    var hasProp = function (record, key) {
      return {}.hasOwnProperty.call(record, key);
    };
    for (var key in record0) {
      if (hasProp(record0, key) && hasProp(record1, key)) {
        result[key] = record1[key];
      }
    }
    return result;
  };
};

exports.unsafeSingleton = function (label) {
  return function (value) {
    var record = {};
    record[label] = value;
    return record;
  };
};
