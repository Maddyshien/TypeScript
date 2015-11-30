/// <reference path='fourslash.ts'/>

////@Component({
////	template: `<div [/*prop*/]='/*memb*/' (/*event*/)='getName()'></div>`
////})
////class Foo{
////	name: string;
////    getName(){ return [/*c1*/'bill'].join(/*c2*/);}
////}

goTo.marker('c1');
verify.completionListIsEmpty();
goTo.marker('c2');
verify.completionListIsEmpty();

goTo.marker('prop');
verify.memberListContains('text');
verify.memberListContains('class');
verify.not.memberListContains('click');
verify.not.memberListContains('div');

goTo.marker('memb');
verify.memberListContains('name');
verify.memberListContains('getName');
verify.not.memberListContains('text');
verify.not.memberListContains('div');

goTo.marker('event');
verify.memberListContains('click');
verify.memberListContains('change');
verify.not.memberListContains('text');
verify.not.memberListContains('getName');
