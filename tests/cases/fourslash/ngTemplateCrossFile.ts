/// <reference path='fourslash.ts'/>

//@no-default-lib: false
//@module: commonjs

//@Filename: domtypes.d.ts
//// declare interface Event {}
//// declare interface HTMLElement {
//// }
//// declare interface HTMLDivElement extends HTMLElement {
//// }
//// declare var HTMLDivElement: {
////     prototype: HTMLDivElement;
////     new(): HTMLDivElement;
//// }

//@Filename: myexports.ts
//// export class Foo{
////     name/*name*/: string;
////     getName(){ return [/*c1*/'bill'].join(/*c2*/);}
////     private setName(){}
//// }

//@Filename: mycode.ts
//// <reference path="domtypes.d.ts"/>
////
//// import {Foo} from "./myexports"
////
//// @Component({
//// 	template: `<div [title]='foo./*memb*/'></div>`
//// })
//// class Comp{
//// 	foo: Foo;
//// }

goTo.marker('memb');
verify.memberListContains('name');
verify.memberListContains('getName');
verify.not.memberListContains('setName');
verify.not.memberListContains('foo');

goTo.marker('name');
edit.backspace(4);
edit.insert('firstName');

goTo.marker('memb');
verify.memberListContains('firstName');
verify.not.memberListContains('name');
