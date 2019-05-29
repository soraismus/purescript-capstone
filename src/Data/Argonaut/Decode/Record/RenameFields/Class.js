"use strict";

exports["renameFields'"] = function (tuples, record) {
  var result = {};
  for (var key in record) {
    if ({}.hasOwnProperty.call(record, key)) {
      result[key] = record[key];
    }
  }
  var count = tuples.length;
  for (var i = 0; i < count; i++) {
    var tuple = tuples[i];
    var oldKey = tuple.value0;
    if ({}.hasOwnProperty.call(result, oldKey)) {
      var value = result[oldKey];
      delete result[value];
      var newKey = tuple.value1;
      result[newKey] = value;
    }
  }
  return result;
};
