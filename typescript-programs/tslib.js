"use strict";
// simple linear algebra
Object.defineProperty(exports, "__esModule", { value: true });
exports.addm = exports.addv = void 0;
function elementWisev(fn) {
    return function (v, v2) { return v.map(function (x, i) { return fn(x, v2[i]); }); };
}
function elementWisem(fn) {
    return function (m, m2) { return m.map(function (x, i) { return fn(x, m2[i]); }); };
}
var add = function (e, e2) {
    return e + e2;
};
exports.addv = elementWisev(add);
exports.addm = elementWisem(exports.addv);
