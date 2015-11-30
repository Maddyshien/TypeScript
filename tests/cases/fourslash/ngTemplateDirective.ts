/// <reference path='fourslash.ts'/>

////@Component({
////	template: `<div */**/`
////})
////class Foo{
////	name: string;
////}

goTo.marker();
verify.memberListContains('ng-for');
verify.memberListContains('ng-if');
verify.not.memberListContains('p');
verify.not.memberListContains('div');
