/// <reference path='fourslash.ts'/>

////@Component({
////	template: `<div>{{name./*1*/}}</div><div>{{/*2*/</div>`
////})
////class Foo{
////	name: string;
////}

goTo.marker('1');
verify.memberListContains('charAt');
verify.not.memberListContains('Date');
verify.not.memberListContains('name');
verify.not.memberListContains('Foo');

goTo.marker('2');
verify.memberListContains('name');
verify.not.memberListContains('Date');
verify.not.memberListContains('chatAt');
verify.not.memberListContains('Foo');
