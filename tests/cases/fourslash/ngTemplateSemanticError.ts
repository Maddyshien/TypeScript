/// <reference path='fourslash.ts'/>

// @experimentalDecorators: true

////declare var Component: any;
////declare class HTMLElement {};
////
////@Component({
////	template: `<div>Greetings, {{nombre}}</div>`
////})
////class Foo{
////	name: string;
////}

verify.numberOfErrorsInCurrentFile(1);
verify.getSemanticDiagnostics(`[
  {
    "message": "Property 'nombre' does not exist on type 'Foo'.",
    "start": 84,
    "length": 32,
    "category": "error",
    "code": 2339
  }
]`);
