/// <reference path='services.ts'/>

namespace ngml {
    ts.pluginFactories.push( program => {

        function getValidSourceFile(fileName: string): ts.SourceFile {
            fileName = ts.normalizeSlashes(fileName);
            let getCanonicalFileName = ts.createGetCanonicalFileName(/* useCaseSensitivefileNames */ false);
            let sourceFile = program.getSourceFile(getCanonicalFileName(fileName));
            if (!sourceFile) {
                throw new Error("Could not find file: '" + fileName + "'.");
            }
            return sourceFile;
        }

        function getNgTemplateCompletionsAtPosition(fileName: string, position: number): ts.CompletionInfo {
            // TODO: getCompletionEntryDetails needs to be wired up also
            let typeChecker = program.getTypeChecker();
            let sourceFile = getValidSourceFile(fileName);
            let isJavaScriptFile = ts.isJavaScript(fileName);
            let currentToken = ts.getTokenAtPosition(sourceFile, position);

            // Only execute if in a template string
            // TODO: Probably needs to handle multi-part tokens (e.g. contains expressions)
            if(currentToken.kind !== ts.SyntaxKind.FirstTemplateToken){
                return undefined;
            }

            // Ensure this template string is set up as per an Angular template
            let classDecl = getNgTemplateClassDecl(currentToken);
            if(!classDecl) {
                return undefined;
            }

            let classSymbol = typeChecker.getTypeAtLocation(classDecl) as ts.InterfaceType;
            let classProps = classSymbol.thisType.getProperties();

            var members: ts.CompletionEntry[] = classProps.map(prop => ({
                name: prop.getName(),
                kind: ts.ScriptElementKind.memberVariableElement,
                kindModifiers: "",
                sortText: "0"
            }));

            var token = ts.getTokenAtPosition(sourceFile, position);
            // getText gets the full string from the `
            // getStart gets the position of the `
            var templateText = token.getText();
            var priorPos = templateText.charAt(position - 1 - token.getStart());

            var elements: ts.CompletionEntry[];

            switch(priorPos){
                case '<':
                    elements = getElementCompletions();
                    break
                case '*':
                    elements = getDirectiveCompletions();
                    break;
                case '[':
                    elements = getPropertyCompletions();
                    break;
                case '(':
                    elements = getEventCompletions();
                    break;
                case "'":
                default:
                    elements = members;
                    break;
            }

            var result: ts.CompletionInfo = {
                isMemberCompletion: false,
                isNewIdentifierLocation: false,
                entries: elements
            }
            return result;
        }

        return {
            version: "0.1.0",
            getCompletionsAtPosition: getNgTemplateCompletionsAtPosition
        };
    });

    function getNgTemplateClassDecl(currentToken: ts.Node){
        // Verify we are in a 'template' property assignment, in an object literal, which is an call arg, in a decorator
        let parentNode = currentToken.parent;  // PropertyAssignment
        if(!parentNode){
            return undefined;
        }
        if(parentNode.kind !== ts.SyntaxKind.PropertyAssignment){
            return undefined;
        } else {
            // TODO: Is this different for a literal, i.e. a quoted name like "template"
            if((parentNode as any).name.text !== 'template'){
                return undefined;
            }
        }
        parentNode = parentNode.parent; // ObjectLiteralExpression
        if(!parentNode || parentNode.kind !== ts.SyntaxKind.ObjectLiteralExpression){
            return undefined;
        }

        parentNode = parentNode.parent; // CallExpression
        if(!parentNode || parentNode.kind !== ts.SyntaxKind.CallExpression){
            return undefined;
        }

        let decorator = parentNode.parent; // Decorator
        if(!decorator || decorator.kind !== ts.SyntaxKind.Decorator){
            return undefined;
        }

        let classDecl = decorator.parent; // ClassDeclaration
        if(!classDecl || classDecl.kind !== ts.SyntaxKind.ClassDeclaration){
            return undefined;
        }
        return classDecl;
    }

    // TODO: The below are to return a collection of mock completions for now...
    function getElementCompletions(): ts.CompletionEntry[]{
        return ["div", "span", "p", "h1", "h2", "img"].map( name => ({
                name,
                kind: ts.ScriptElementKind.classElement,
                kindModifiers: "",
                sortText: "0"
        }));
    }

    function getDirectiveCompletions(): ts.CompletionEntry[]{
        return ["ng-for", "ng-repeat", "ng-if", "ng-switch"].map( name => ({
                name,
                kind: ts.ScriptElementKind.keyword,
                kindModifiers: "",
                sortText: "0"
        }));
    }

    function getPropertyCompletions(): ts.CompletionEntry[] {
        return ["text", "class", "style"].map( name => ({
                name,
                kind: ts.ScriptElementKind.memberVariableElement,
                kindModifiers: "",
                sortText: "0"
        }));
    }

    function getEventCompletions(): ts.CompletionEntry[] {
        return ["click", "change", "mouseover"].map( name => ({
                name,
                kind: ts.ScriptElementKind.memberFunctionElement,
                kindModifiers: "",
                sortText: "0"
        }));
    }

    // A rudimentary parser for Angular templates
    // It expects the HTML to be well formed, i.e. self closing or matched tags, not unmatched like <br>.
    // It has a startPos, which is the first significant character, and a fullStartPos, which is immediately after the proceeding token.
    // Every char is part of a token. Any trailing space would be a 'Text' token.

    export enum ngNodeKind {
        Root,         // Enclosing document. No name or attributes, just children.
        SelfClosingTag,
        StartTag,
        Attribute,
        EndTag,
        Text,
        Comment,      // Includes enclosing <!-- & -->
        Interpolation // Includes enclosing {{ & }}
    }

    export interface NgNode {
        kind: ngNodeKind,
        fullStartPos: number; // Includes leading whitespace
        startPos: number;
        endPos: number; // Position after final char. Text length = endPos - startPos
    }

    export interface NgNamedNode extends NgNode {
        name: string; // Used directly for closing tags
    }

    export interface NgTag extends NgNamedNode {
        attributes: NgAttrib[];
        children: NgNode[]; // Final child will be closing tag for non-self-closing tags
    }

    // TODO: Have a flag to indicate if the value is an expression, and maybe an AST property if it is.
    export interface NgAttrib extends NgNamedNode {
        value?: string;
    }

    export class NgTemplateParser {
        // TODO: Make ascii case-insensitive for element & attribute names
        currentPos: number;
        ast: NgTag;
        errors: string[];
        stats = {
          openTags: 0,
          closeTags: 0,
          selfClosingTags: 0,
          attributes: 0,
          comments: 0,
          interpolations: 0,
          textNodes: 0
        };

        // Creating a new scanner will automatically populate the AST and error list.
        constructor(public text: string){
            this.currentPos = 0;
            this.errors = [];
            this.ast = this.scan();
        }

        private getChar(offset: number = 0){
            if(this.currentPos + offset >= this.text.length){
                return '\x00';
            } else {
                let result = this.text[this.currentPos + offset];
                this.currentPos += (offset + 1);
                return result;
            }
        }

        private peekChar(offset: number = 0){
            if(this.currentPos + offset >= this.text.length){
                return '\x00';
            } else {
                return this.text[this.currentPos + offset];
            }
        }

        scan(): NgTag{
            if(!this.text) {
                return null;
            }

            var root: NgTag = {
                kind: ngNodeKind.Root,
                fullStartPos: 0,
                startPos: 0,
                endPos: this.text.length,
                name: null,
                attributes: null,
                children: []
            }

            /*
            Effectively we start by pushing the root node on the stack, and scanning for children (parseTagChildren).
            findNextChild iteratively looks for a comment, text, interpolation, or tag, until it reaches an close tag (the parent's) or EOF.
            findNextChild pushes each child it finds onto the 'childen' array of the current top of stack.

            Parsing of text simply runs to the next "<", ">" or "{{"
            Parsing of an interpolation runs until the next "}}"
            Parsing of a comment runs to the closing "-->"

            When an tag start is found, parseTag is called.
            ParseTag pushes itself on the stack, and calls parseTagAttributes until it encounters "/>" or ">".
            If it encounters "/>", it completes the tag and returns.
            If it encounters ">", it calls parseTagChildren.
            */


            var stack: NgTag[] = [root];
            let nextChild: NgNode;
            while(nextChild = this.findNextChild()){
                // For open or close tags, move up or down the stack
                switch(nextChild.kind){
                    case ngNodeKind.StartTag:
                        stack[stack.length - 1].children.push(nextChild);
                        // Start of child tag found, make the top of the stack.
                        stack.push(nextChild as NgTag);
                        break;
                    case ngNodeKind.EndTag:
                        if((nextChild as NgNamedNode).name === (stack[stack.length - 1].name)){
                            // Close tag for current top of stack. Pop from stack, add as final child, and continue
                            stack.pop().children.push(nextChild);
                        } else {
                            this.errors.push(`html:${nextChild.startPos}:${this.currentPos - 1}: Unexpected closing tag`);
                        }
                        break;
                    default:
                        // Add the child node to the current tag on top of stack
                        stack[stack.length - 1].children.push(nextChild);
                        break;
                }
            }
            return root;
        }

        findNextChild(): NgNode {
            let fullStartPos = this.currentPos;
            this.skipWhitespace();
            let ch = this.getChar();
            switch(ch){
                case '\x00':
                    // Did we have trailing text or not?
                    if(this.currentPos === fullStartPos){
                        return null;
                    } else {
                        this.stats.textNodes++;
                        return {
                            kind: ngNodeKind.Text,
                            fullStartPos: fullStartPos,
                            startPos: this.currentPos,
                            endPos: this.currentPos
                        }
                    }
                    break;
                case '<':
                    if(this.peekChar(0) === '!' && this.peekChar(1) === '-' && this.peekChar(2) === '-'){
                        return this.parseComment(fullStartPos);
                    } else if(this.isTagStartChar(this.peekChar(0))) {
                        return this.parseTag(fullStartPos);
                    } else if(this.peekChar(0) === '/') {
                        return this.parseCloseTag(fullStartPos);
                    } else {
                        let result = this.parseText(fullStartPos, /* stopOnGreaterThan */ true);
                        this.errors.push(`html:${result.startPos}:${this.currentPos - 1}: Invalid tag`);
                        return result;
                    }
                    break;
                case '{':
                    // Check for "{{"
                    if(this.peekChar(0) === '{'){
                        return this.parseInterpolation(fullStartPos);
                    } else {
                        return this.parseText(fullStartPos);
                    }
                    break;
                default:
                    return this.parseText(fullStartPos);
            }
        }

        parseComment(fullStartPos: number): NgNode {
            let result: NgNode = {
                kind: ngNodeKind.Comment,
                fullStartPos: fullStartPos,
                startPos: this.currentPos - 1,
                endPos: 0
            }

            // Skip the '!--', then scan to closing '-->'
            this.currentPos += 3;

            let ch: string;
            while((ch = this.getChar()) !== '\x00'){
                if(ch === '-' && this.peekChar(0) === '-' && this.peekChar(1) === '>') {
                    this.currentPos += 2;
                    break;
                }
            }
            result.endPos = this.currentPos;
            this.stats.comments++;
            return result;
        }

        parseTag(fullStartPos: number): NgTag {
            // Assuming it's an opening to tag to begin, and fix later if wrong.
            let result: NgTag = {
                kind: ngNodeKind.StartTag,
                fullStartPos: fullStartPos,
                startPos: this.currentPos - 1,
                endPos: 0,
                name: null,
                attributes: [],
                children: []
            }

            while(this.isTagPartChar(this.peekChar())) this.getChar();
            result.name = this.text.substring(result.startPos + 1, this.currentPos);

            this.parseAttributes(result);
            this.skipWhitespace();

            if(this.peekChar() === '/' && this.peekChar(1) === '>'){
                this.currentPos += 2;
                result.kind = ngNodeKind.SelfClosingTag
            } else if(this.peekChar() === '>'){
                this.currentPos++;
            } else {
                this.errors.push(`html:${result.startPos}:${this.currentPos - 1}: Invalid tag end`);
            }
            // TODO: Log error if not a well formed closing tag (i.e. doesn't close, whitespace, invalid chars...)

            result.kind === ngNodeKind.SelfClosingTag ? this.stats.selfClosingTags++ : this.stats.openTags++;

            result.endPos = this.currentPos;
            return result;
        }

        parseAttributes(parent: NgTag){
            while(true){
                let attrib = this.parseAttribute();
                if(attrib){
                    parent.attributes.push(attrib);
                } else {
                    break;
                }
            }
        }

        parseAttribute(): NgAttrib {
            // Note: May be spaces around the '=' sign. Require quoted values for now, and allow any non-quote chars inside.
            // TODO: Make more compliant with the spec: https://html.spec.whatwg.org/multipage/syntax.html#attributes-2
            let result: NgAttrib = {
                kind: ngNodeKind.Attribute,
                fullStartPos: this.currentPos,
                startPos: 0,
                endPos: 0,
                name: null,
                value: null
            }

            this.skipWhitespace();
            if(!this.isAttribChar(this.peekChar())){
                return null;
            }

            result.startPos = this.currentPos;

            // Consume the name
            while(this.isAttribChar(this.peekChar())) this.getChar();
            result.name = this.text.substring(result.startPos, this.currentPos);
            this.stats.attributes++;

            this.skipWhitespace();
            // No value given
            if(this.peekChar() !== '='){
                return result;
            }

            // Parse the value, after skipping the = and any whitespace
            this.getChar();
            this.skipWhitespace();

            let valuePos = this.currentPos;
            let valueChar = this.peekChar();
            if(valueChar === "'"){
                this.getChar(); // Skip over the quote
                while((valueChar = this.getChar()) !== '\x00' ){
                    if(valueChar === "'"){
                        result.endPos = this.currentPos;
                        result.value = this.text.substring(valuePos + 1, result.endPos - 1);
                        return result;
                    }
                }
                // End of stream
                result.endPos = this.currentPos;
                this.errors.push(`html:${result.startPos}:${result.endPos}: Incomplete attribute value`);
            } else {
                // TODO: Allow other forms, such as double quotes or naked. But error for now.
                this.errors.push(`html:${result.startPos}:${this.currentPos}: Unrecognized attribute value`);
            }

            return result;
        }

        parseCloseTag(fullStartPos: number): NgNamedNode {
            let result: NgNamedNode = {
                kind: ngNodeKind.EndTag,
                fullStartPos: fullStartPos,
                startPos: this.currentPos - 1,
                endPos: 0,
                name: null,
            }
            this.stats.closeTags++;

            let ch = this.getChar(); // Consume the '/', then scan to closing '>'
            while((ch = this.getChar()) !== '\x00'){
                if(ch === '>') {
                    // TODO: Log error if not a well formed closing tag (i.e. whitespace, invalid chars...)
                    result.name = this.text.substring(result.startPos + 2, this.currentPos - 1);
                    result.endPos = this.currentPos;
                    return result;
                };
            }

            // Hit the end of the stream before the closing '>'
            result.endPos = this.currentPos;
            this.errors.push(`html:${result.startPos}:${result.endPos}: Incomplete closing tag`);

            return result;
        }

        parseInterpolation(fullStartPos: number): NgNode {
            let result: NgNode = {
                kind: ngNodeKind.Interpolation,
                fullStartPos: fullStartPos,
                startPos: this.currentPos - 1,
                endPos: 0
            }
            this.stats.interpolations++;

            let ch = this.getChar(); // Consume the second '{', then scan to closing '}}'
            while((ch = this.getChar()) !== '\x00'){
                if(ch === '}' && this.peekChar() === '}'){
                    this.currentPos += 1;
                    result.endPos = this.currentPos;
                    return result;
                }
            }

            // Hit the end of the stream before the closing '}}'
            result.endPos = this.currentPos;
            this.errors.push(`html:${result.startPos}:${result.endPos}: Unclosed interpolation`);
            return result;
        }

        parseText(fullStartPos: number, stopOnGreaterThan: boolean = false): NgNode {
            let result: NgNode = {
                kind: ngNodeKind.Text,
                fullStartPos: fullStartPos,
                startPos: this.currentPos - 1,
                endPos: 0
            }
            this.stats.textNodes++;

            let ch: string;
            while(true){
                // Go up to the next char of interest, but don't consume it
                ch = this.peekChar();
                if(ch === '\x00' || ch === '<') break;
                if(ch === '{' && this.peekChar(1) === '{') break;
                if(stopOnGreaterThan && ch === '>') {
                    // Consume this one, and finish
                    this.getChar();
                    break;
                }
                this.getChar();
            }

            result.endPos = this.currentPos;
            return result;
        }

        skipWhitespace() {
            while(this.currentPos < this.text.length){
                if(this.isWhiteSpace(this.text[this.currentPos])){
                    this.currentPos++;
                } else {
                    return;
                }
            }
        }

        isWhiteSpace(char: string){
            return [' ', '\t', '\x0D', '\x0A', '\x0C'].some( ch => ch === char);
        }

        isTagStartChar(char: string){
            return (char >= 'a' && char <= 'z' || char >= 'A' && char <= 'Z')
        }

        isTagPartChar(char: string){
            // See if it's one of the disallowed chars. See https://html.spec.whatwg.org/multipage/syntax.html#tag-name-state
            // Note: Disallowing all control codes here. Some seem to be allowed for tag names, but that seems like a bad idea.
            if(char.charCodeAt(0) < 0x20 || [' ', '/', '>'].some(ch => ch === char)){
                return false;
            }
            return true;
        }

        isAttribChar(char: string){
            // See if it's one of the disallowed chars. See https://html.spec.whatwg.org/multipage/syntax.html#tag-name-state
            if(char.charCodeAt(0) < 0x20 || [' ', '/', '>', '=', '"', "'"].some(ch => ch === char)){
                return false;
            }
            return true;
        }
    }

    export function testParser(){
        let assert = (condition: boolean, msg: string) => {
            if(!condition) {
                throw msg;
            }
        };

        var tmp = new NgTemplateParser(`<!-- A comment --><div name='foo'>Hi {{this.name}}</div><br/>`);
        assert(tmp.stats.comments == 1, "Expected 1 comment");
        assert(tmp.stats.openTags == 1, "Expected 1 open tag");
        assert(tmp.stats.attributes == 1, "Expected 1 attribute");
        assert(tmp.stats.textNodes == 1, "Expected 1 text node");
        assert(tmp.stats.interpolations == 1, "Expected 1 interpolation");
        assert(tmp.stats.closeTags == 2, "Expected 1 close tag");
        assert(tmp.stats.selfClosingTags == 1, "Expected 1 self closing tag");
        assert(tmp.ast.children.length == 3, "Expected fragment to have 3 children");
        assert(tmp.ast.children[1].kind == ngNodeKind.StartTag, "Expected second child to be a start tag");
        assert((tmp.ast.children[1] as NgNamedNode).name == "div", "Expected second child to be named 'div");
    }
}
