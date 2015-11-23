/// <reference path='services.ts'/>

namespace ts {
    pluginFactories.push( program => {

        function getValidSourceFile(fileName: string): SourceFile {
            fileName = normalizeSlashes(fileName);
            let getCanonicalFileName = createGetCanonicalFileName(/* useCaseSensitivefileNames */ false);
            let sourceFile = program.getSourceFile(getCanonicalFileName(fileName));
            if (!sourceFile) {
                throw new Error("Could not find file: '" + fileName + "'.");
            }
            return sourceFile;
        }

        function getNgTemplateCompletionsAtPosition(fileName: string, position: number): CompletionInfo {
            // TODO: getCompletionEntryDetails needs to be wired up also
            let typeChecker = program.getTypeChecker();
            let sourceFile = getValidSourceFile(fileName);
            let isJavaScriptFile = isJavaScript(fileName);
            let currentToken = getTokenAtPosition(sourceFile, position);

            // TODO: Probably needs to handle multi-part tokens (e.g. contains expressions)
            if(currentToken.kind !== SyntaxKind.FirstTemplateToken){
                return undefined;
            }

            // Verify we are in a 'template' property assignment in a decorator
            let parentNode = currentToken.parent;  // PropertyAssignment
            if(!parentNode){
                return undefined;
            }
            if(parentNode.kind !== SyntaxKind.PropertyAssignment){
                return undefined;
            } else {
                // TODO: Is this different for a literal, i.e. 'template'
                if((parentNode as any).name.text !== 'template'){
                    return undefined;
                }
            }
            parentNode = parentNode.parent; // ObjectLiteralExpression
            if(!parentNode || parentNode.kind !== SyntaxKind.ObjectLiteralExpression){
                return undefined;
            }

            parentNode = parentNode.parent; // CallExpression
            if(!parentNode || parentNode.kind !== SyntaxKind.CallExpression){
                return undefined;
            }

            let decorator = parentNode.parent; // Decorator
            if(!decorator || decorator.kind !== SyntaxKind.Decorator){
                return undefined;
            }

            let classDecl = decorator.parent; // ClassDeclaration
            if(!classDecl || classDecl.kind !== SyntaxKind.ClassDeclaration){
                return undefined;
            }

            let classSymbol = typeChecker.getTypeAtLocation(classDecl) as InterfaceType;
            let classProps = classSymbol.thisType.getProperties();

            var members: CompletionEntry[] = classProps.map(prop => ({
                name: prop.getName(),
                kind: ScriptElementKind.memberVariableElement,
                kindModifiers: "",
                sortText: "0"
            }));

            var elements: CompletionEntry[] = ["div", "span", "p", "h1", "h2", "img"].map( name => ({
                name,
                kind: ScriptElementKind.classElement,
                kindModifiers: "",
                sortText: "0"
            }));

            var directives: CompletionEntry[] = ["ng-for", "ng-repeat", "ng-if", "ng-switch"].map( name => ({
                name,
                kind: ScriptElementKind.keyword,
                kindModifiers: "",
                sortText: "0"
            }));

            var properties: CompletionEntry[] = ["text", "class", "style"].map( name => ({
                name,
                kind: ScriptElementKind.memberVariableElement,
                kindModifiers: "",
                sortText: "0"
            }));

            var events: CompletionEntry[] = ["click", "change", "mouseover"].map( name => ({
                name,
                kind: ScriptElementKind.memberFunctionElement,
                kindModifiers: "",
                sortText: "0"
            }));

            var token = getTokenAtPosition(sourceFile, position);
            // getText gets the full string from the `
            // getStart gets the position of the `
            // So to get the char before the current position...
            var templateText = token.getText();
            var priorPos = templateText.charAt(position - 1 - token.getStart());
            if(priorPos === '*'){
                elements = directives;
            } else if(priorPos === "'"){
                elements = members;
            } else if(priorPos === '['){
                elements = properties;
            } else if(priorPos === "("){
                elements = events;
            }

            var result: CompletionInfo = {
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
}
