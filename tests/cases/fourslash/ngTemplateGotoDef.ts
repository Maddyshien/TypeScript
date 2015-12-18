/// <reference path='fourslash.ts'/>

// @experimentalDecorators: true

////declare var Component: any;
////declare class HTMLDivElement {};
////
////@Component({
////	template: `<div #mydiv (click)='callMe()'><div>{{/*1*/mydiv.innerHTML}}</div></div>`
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
goTo.definition();
