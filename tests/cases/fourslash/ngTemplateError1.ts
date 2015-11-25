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

verify.numberOfErrorsInCurrentFile(1);
verify.errorExistsBetweenMarkers("1", "2");
verify.getSyntacticDiagnostics(`[
  {
    "message": "Unmatched opening tag",
    "start": 54,
    "length": 5,
    "category": "warning",
    "code": 1
  }
]`);
