/// <reference path='fourslash.ts'/>

////@Component({
////	template: `<div */**/`
////})
////class Foo{
////	name: string;
////}

goTo.marker();
verify.memberListContains('ngFor');
verify.memberListContains('ngIf');
verify.not.memberListContains('p');
verify.not.memberListContains('div');
