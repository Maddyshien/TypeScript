/// <reference path='fourslash.ts'/>

//@no-default-lib: false

//@Filename: domtypes.d.ts
////declare interface Event {}
////declare interface HTMLElement {
////    children: any;
////    contentEditable: string;
////    lang: string;
////    offsetHeight: number;
////    offsetLeft: number;
////    offsetParent: any;
////    offsetTop: number;
////    offsetWidth: number;
////    onclick: (ev: Event) => any;
////    onerror: (ev: Event) => any;
////    onfocus: (ev: Event) => any;
////}
////declare interface HTMLDivElement extends HTMLElement {
////    tagName: string;
////    id: string;
////    className: string;
////    onblur: (ev: Event) => any;
////    oncanplay: (ev: Event) => any;
////    oncanplaythrough: (ev: Event) => any;
////    onchange: (ev: Event) => any;
////    oninput: (ev: Event) => any;
////    onkeydown: (ev: Event) => any;
////    onkeypress: (ev: Event) => any;
////    onkeyup: (ev: Event) => any;
////    onload: (ev: Event) => any;
////    onloadeddata: (ev: Event) => any;
////    onloadedmetadata: (ev: Event) => any;
////    onloadstart: (ev: Event) => any;
////    onmousedown: (ev: Event) => any;
////    onmouseenter: (ev: Event) => any;
////    onmouseleave: (ev: Event) => any;
////    onmousemove: (ev: Event) => any;
////    onmouseout: (ev: Event) => any;
////    onmouseover: (ev: Event) => any;
////    onmouseup: (ev: Event) => any;
////    outerHTML: string;
////    outerText: string;
////    spellcheck: boolean;
////    tabIndex: number;
////    title: string;
////    blur(): void;
////    click(): void;
////    contains(child: HTMLDivElement): boolean;
////    dragDrop(): boolean;
////    focus(): void;
////    getAttribute(name?: string): string;
////    setAttribute(name?: string, value?: string): void;
////    setPointerCapture(pointerId: number): void;
////    addEventListener(type: "webkitfullscreenerror", listener: (ev: Event) => any, useCapture?: boolean): void;
////    addEventListener(type: "wheel", listener: (ev: Event) => any, useCapture?: boolean): void;
////    addEventListener(type: string, listener: (ev: Event) => any, useCapture?: boolean): void;
////}
////declare var HTMLDivElement: {
////    prototype: HTMLDivElement;
////    new(): HTMLDivElement;
////}

//@Filename: mycode.ts
////<reference path="domtypes.d.ts"/>
////@Component({
////	template: `</*elem*/div [/*prop*/]='/*memb*/' (/*event*/)='getName()'><//*close*/div>
//// <div [/*open*/] (/*halfopen*/></div>`
////})
////class Foo{
////	name: string;
////    getName(){ return [/*c1*/'bill'].join(/*c2*/);}
////}

goTo.marker('open');
verify.memberListContains('children');
goTo.marker('halfopen');
verify.memberListContains('mouseenter');
verify.not.memberListContains('onclick');

goTo.marker('c1');
verify.completionListIsEmpty();
goTo.marker('c2');
verify.completionListIsEmpty();

goTo.marker('elem');
verify.memberListContains('div');
verify.memberListContains('h1');
verify.not.memberListContains('children');
verify.not.memberListContains('getName');

goTo.marker('close');
verify.memberListContains('div');
verify.not.memberListContains('onclick');
verify.not.memberListContains('h1');
verify.not.memberListContains('getName');

goTo.marker('prop');
verify.memberListContains('children');
verify.memberListContains('className');
verify.not.memberListContains('div');
verify.not.memberListContains('getName');

goTo.marker('memb');
verify.memberListContains('name');
verify.memberListContains('getName');
verify.not.memberListContains('children');
verify.not.memberListContains('div');

goTo.marker('event');
verify.memberListContains('click');
verify.not.memberListContains('text');
verify.not.memberListContains('getName');
