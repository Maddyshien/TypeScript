/// <reference path='fourslash.ts'/>

// @experimentalDecorators: true

////declare var Component: any;
////declare class HTMLDivElement {}
////
////@Component({
////	template: `/*1*/<div>/*2*/`
////})
////class Foo{
////	name: string;
////}

verify.numberOfErrorsInCurrentFile(1);
verify.errorExistsBetweenMarkers("1", "2");
verify.getSyntacticDiagnostics(`[
  {
    "message": "Unmatched opening tag",
    "start": 86,
    "length": 5,
    "category": "warning",
    "code": 1
  }
]`);
