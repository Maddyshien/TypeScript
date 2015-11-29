/// <reference path='fourslash.ts'/>

// @experimentalDecorators: true

////declare var Component: any;
////declare class HTMLDivElement {};
////
////@Component({
////	template: `<div>Greetings, {{/*1*/nombre/*2*/}}</div>`
////})
////class Foo{
////	name: string;
////}

verify.numberOfErrorsInCurrentFile(1);
verify.errorExistsBetweenMarkers("1", "2");
verify.getSemanticDiagnostics(`[
  {
    "message": "Property 'nombre' does not exist on type 'Foo'.",
    "start": 105,
    "length": 6,
    "category": "error",
    "code": 2339
  }
]`);
