//// [tests/cases/compiler/es6ImportDefaultBindingFollowedWithNamedImportDts.ts] ////

//// [server.ts]

export class a { }
export class x { }
export class m { }
export class a11 { }
export class a12 { }
export class x11 { }

//// [client.ts]
import defaultBinding1, { } from "server";
import defaultBinding2, { a } from "server";
export var x1 = new a();
import defaultBinding3, { a11 as b } from "server";
export var x2 = new b();
import defaultBinding4, { x, a12 as y } from "server";
export var x4 = new x();
export var x5 = new y();
import defaultBinding5, { x11 as z,  } from "server";
export var x3 = new z();
import defaultBinding6, { m,  } from "server";
export var x6 = new m();


//// [server.js]
var a = (function () {
    function a() {
    }
    return a;
})();
exports.a = a;
var x = (function () {
    function x() {
    }
    return x;
})();
exports.x = x;
var m = (function () {
    function m() {
    }
    return m;
})();
exports.m = m;
var a11 = (function () {
    function a11() {
    }
    return a11;
})();
exports.a11 = a11;
var a12 = (function () {
    function a12() {
    }
    return a12;
})();
exports.a12 = a12;
var x11 = (function () {
    function x11() {
    }
    return x11;
})();
exports.x11 = x11;
//// [client.js]
var _server_1 = require("server");
exports.x1 = new _server_1.a();
var _server_2 = require("server");
exports.x2 = new _server_2.a11();
var _server_3 = require("server");
exports.x4 = new _server_3.x();
exports.x5 = new _server_3.a12();
var _server_4 = require("server");
exports.x3 = new _server_4.x11();
var _server_5 = require("server");
exports.x6 = new _server_5.m();


//// [server.d.ts]
export declare class a {
}
export declare class x {
}
export declare class m {
}
export declare class a11 {
}
export declare class a12 {
}
export declare class x11 {
}
//// [client.d.ts]
import { a } from "server";
export declare var x1: a;
import { a11 as b } from "server";
export declare var x2: b;
import { x, a12 as y } from "server";
export declare var x4: x;
export declare var x5: y;
import { x11 as z } from "server";
export declare var x3: z;
import { m } from "server";
export declare var x6: m;
