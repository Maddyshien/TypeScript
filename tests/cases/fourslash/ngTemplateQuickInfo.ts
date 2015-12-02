/// <reference path='fourslash.ts'/>

// @experimentalDecorators: true

////declare var Component: any;
////declare class HTMLDivElement {};
////
////@Component({
////	template: `<div (click)='cal/*1*/lMe()'></div>`
////})
////class Foo{
////	name: string;
////    /** 
////      * Call this function
////      * @param val - A string input value 
////      */
////    callMe(val: string){
////    }
////}

goTo.marker("1");
verify.quickInfoIs("(method) Foo.callMe(val: string): void");
