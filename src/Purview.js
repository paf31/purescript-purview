'use strict';

exports.objDelete = function (key, obj) {
  delete obj.value[key];
}

exports.objUpsert = function (key, value, obj) {
  obj.value[key] = value;
};

exports.objUpdate = function (key, f, obj) {
  obj.value[key] = f(obj.value[key]);
};
