/// <reference path='fourslash.ts'/>

////@Component({
////	template: `</**/`
////})
////class Foo{
////	name: string;
////}

goTo.marker();
verify.memberListContains('div');
verify.memberListContains('span');
verify.memberListContains('p');
verify.not.memberListContains('ng-repeat');
