/// <reference path='fourslash.ts'/>

// @experimentalDecorators: true

////declare var Component: any;
////
////@Component({
////	template: `/*1*/<div>/*2*/`
////})
////class Foo{
////	name: string;
////}

//verify.errorExistsBetweenMarkers("1", "2");
verify.getSyntacticDiagnostics("html");
debug.printErrorList();
