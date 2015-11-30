/// <reference path='fourslash.ts'/>

// @experimentalDecorators: true

////declare var Component: any;
////declare class HTMLDivElement {};
////
////@Component({
////	template: `<div (click)='callMe(/*1*/)'></div>`
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
verify.signatureHelpCountIs(1);
verify.currentSignatureParameterCountIs(1);
verify.currentSignatureHelpIs("callMe(val: string): void")
verify.currentSignatureHelpDocCommentIs("Call this function");
