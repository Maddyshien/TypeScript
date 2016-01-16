/// <reference path='services.ts'/>

namespace ngml {
	if(typeof ts !== 'undefined' && ts.pluginFactories){
		// Set to true when creating the extra language service, to avoid it creating a plugin, which creates a language service, etc...
		let creatingLanguageServiceFlag = false;

		ts.pluginFactories.push( (cancellationToken, host, docRegistry) => {
			if(creatingLanguageServiceFlag) return undefined;

			// The below are used for answering questions from generated code via a separate program & language service.
			creatingLanguageServiceFlag = true;
			let hostProxy = createHostProxy();
			let ngmlLanguageService = ts.createLanguageService(hostProxy, docRegistry);
			let currentProgram: ts.Program = null;
			creatingLanguageServiceFlag = false;

			interface MappingFuncs {
				isPosInTemplate(pos: number): boolean;
				isPosInGeneratedCode(pos: number): boolean;
				mapPosFromTemplateToGeneratedCode(pos: number): number;
				mapPosFromGeneratedCodeToTemplate(pos: number): number;
			}

			let filePositionMappings: ts.Map<MappingFuncs> = {};

			// The host proxy is used to serve up synthesized source files containing generated code for any source file
			// that contains an Angular template. It expects all the original source files are already attached to the
			// original program (as the language service that just called into the plugin should have ensured). Rather than
			// the default logic of getting the snapshot from the host, checking the document registry, etc., it just
			// checks if the already created source file contains any template strings. If not, return as is. If it does,
			// create a new source file from generated code and return that.
			function createHostProxy() : ts.LanguageServiceHost {
				// Just delegate most of the functions to the original host.
				let result: ts.LanguageServiceHost = {
					getCompilationSettings: () => host.getCompilationSettings(),
					getScriptFileNames: () => host.getScriptFileNames(),
					getScriptVersion: (fileName: string) => host.getScriptVersion(fileName),
					getScriptSnapshot: (fileName: string) => host.getScriptSnapshot(fileName),
					getCurrentDirectory: () => host.getCurrentDirectory(),
					getDefaultLibFileName: (options: ts.CompilerOptions) => host.getDefaultLibFileName(options)
				};

				// Optional properties. Only add if the original host has them.
				if(host.getNewLine) result.getNewLine = () => host.getNewLine();
				if(host.getProjectVersion) result.getProjectVersion = () => host.getProjectVersion();
		        if(host.getLocalizedDiagnosticMessages) result.getLocalizedDiagnosticMessages = () => host.getLocalizedDiagnosticMessages();
		        if(host.getCancellationToken) result.getCancellationToken = () => host.getCancellationToken();
		        if(host.useCaseSensitiveFileNames) result.useCaseSensitiveFileNames = () => host.useCaseSensitiveFileNames();

				// The below method is not used by the TypeScript language service, only by the plugin
				// It short circuits a lot of the default code that uses the host, Document Registry, etc. to provide a SourceFile.
				result.getScriptSourceFile = (fileName: string) => {
					// The original language service should already have this up to date.
					let originalFile = getValidSourceFile(fileName);
					let templateStrings = getNgTemplateStringsInSourceFile(originalFile);
					if(!templateStrings.length){
						// No template strings in the file, just use the original
						return originalFile;
					} else {
						// Generate the synthesized source file and return that
						let {generatedFile, mappingFuncs} = getGeneratedFile(originalFile, templateStrings);
						filePositionMappings[fileName] = mappingFuncs;
						return generatedFile;
					}
				}

				return result;
			}

			interface ngTemplateNode {
				templateString: ts.Node;
				classDecl: ts.Node;
			}

			function getNgTemplateStringsInSourceFile(sourceFile: ts.SourceFile) : ngTemplateNode[] {
				let result: ngTemplateNode[] = [];

				// Find each template string in the file
				ts.forEachChild(sourceFile, visit);
				function visit(child: ts.Node){
					if(child.kind === ts.SyntaxKind.FirstTemplateToken){
						// Ensure it is a Angular template string
						let classDecl = getNgTemplateClassDecl(child);
						if(classDecl){
							result.push({templateString: child, classDecl});
						}
					} else {
						ts.forEachChild(child, visit);
					}
				}

				return result;
			}

			// Given a template string node, see if it is an Angular template string, and if so return the containing class.
			function getNgTemplateClassDecl(currentToken: ts.Node){
				// Verify we are in a 'template' property assignment, in an object literal, which is an call arg, in a decorator
				let parentNode = currentToken.parent;  // PropertyAssignment
				if(!parentNode){
					return undefined;
				}
				if(parentNode.kind !== ts.SyntaxKind.PropertyAssignment){
					return undefined;
				} else {
					// TODO: Is this different for a literal, i.e. a quoted property name like "template"?
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

			// Given an array of templates from a file, location the one containing the given position
			function getTemplateAtPosition(templates: ngTemplateNode[], position: number) : ngTemplateNode {
				let ngTemplate: ngTemplateNode = null;
				templates.some( elem => {
					if(elem.templateString.getStart() < position && elem.templateString.getEnd() > position){
						ngTemplate = elem;
						return true;
					}
					return false;
				});
				return ngTemplate;
			}

			function getCompletionsAtPosition(fileName: string, position: number) : ts.CompletionInfo {
				let sourceFile = getValidSourceFile(fileName);

				// Does it contain an Angular template at the position requested? If not, exit.
				let templatesInFile = getNgTemplateStringsInSourceFile(sourceFile);
                if(!getTemplateAtPosition(templatesInFile, position)){
					return undefined;
                }

                let queryGeneratedCode = (fileName: string, pos: number) => {
                    if(filePositionMappings[fileName]){
                        pos = filePositionMappings[fileName].mapPosFromTemplateToGeneratedCode(pos);
                    }
                    return ngmlLanguageService.getCompletionsAtPosition(fileName, pos);
                }

                return getNgTemplateCompletionsAtPosition(fileName, position, queryGeneratedCode);
			}

			function getCompletionEntryDetails(fileName: string, position: number, entryName: string): ts.CompletionEntryDetails {
				let sourceFile = getValidSourceFile(fileName);

				// Does it contain an Angular template at the position requested? If not, exit.
				let templatesInFile = getNgTemplateStringsInSourceFile(sourceFile);
				if(!getTemplateAtPosition(templatesInFile, position)){
					return undefined;
				}

                if(filePositionMappings[fileName]){
                    position = filePositionMappings[fileName].mapPosFromTemplateToGeneratedCode(position);
                }
                // TODO: See above getCompletionsAtPosition to see handling details for items not in the generated code (e.g. elements)
                return ngmlLanguageService.getCompletionEntryDetails(fileName, position, entryName);
			}

			function getDefinitionAtPosition(fileName: string, position: number): ts.DefinitionInfo[] {
				let sourceFile = getValidSourceFile(fileName);

				// Does it contain an Angular template at the position requested? If not, exit.
				let templatesInFile = getNgTemplateStringsInSourceFile(sourceFile);
				if(!getTemplateAtPosition(templatesInFile, position)){
					return undefined;
				} else {
					let mappingInfo = filePositionMappings[fileName];
					position = mappingInfo.mapPosFromTemplateToGeneratedCode(position);
					let result = ngmlLanguageService.getDefinitionAtPosition(fileName, position);

					// If the result is in the generated code, map back to the template.
					if(mappingInfo && result){
						result.forEach(mapping => {
							let startPos = mapping.textSpan.start
							if(mappingInfo.isPosInGeneratedCode(startPos)){
								mapping.textSpan.start = mappingInfo.mapPosFromGeneratedCodeToTemplate(startPos);
							}
						})
					}
                    return result;
				}
			}

			function getGeneratedFile(originalFile: ts.SourceFile, templatesInFile: ngTemplateNode[]) {
				// TODO: Just does the first template for now. Update to handle multiple per file
				if(!templatesInFile || templatesInFile.length === 0) return undefined;

				// Find the first (if any) template string for this position
				let ngTemplate = templatesInFile[0];

				// Generate a source file with the generated template code, and map to the position in that
				let text = ngTemplate.templateString.getText();
				text = text.substring(1, text.length - 1); // Strip the surrounding back-ticks
				let htmlParser = new NgTemplateParser(text);

				// Get the name of the class and generate the stub function
				let className = ngTemplate.classDecl.symbol.name;
				let generatedFunc = generateFunction(htmlParser.ast, className);

				// Generate a source file with the injected content and get errors on it
				let insertionPoint = ngTemplate.classDecl.getEnd();
				let oldText = originalFile.getText();
				let newText = `${oldText.substring(0, insertionPoint)}\n${generatedFunc}\n${oldText.substring(insertionPoint)}`;
				let endNewText = insertionPoint + generatedFunc.length + 2;
				let newSourceFile = ts.createSourceFile(originalFile.fileName, newText, originalFile.languageVersion, true);

				let mappingFuncs: MappingFuncs = {
					isPosInTemplate: (position) => position > ngTemplate.templateString.getStart() && position < ngTemplate.templateString.getEnd(),
					isPosInGeneratedCode: (position) => position > insertionPoint && position < endNewText,
					mapPosFromGeneratedCodeToTemplate: (position) => {
						let posOffsetInCodeGen = position - insertionPoint - 1;
						let mappedPos = mapPosViaMarkers(generatedFunc, posOffsetInCodeGen, true);
						if(mappedPos.pointInTemplate === -1){
							// Didn't find it. Just return the start of the template string.
							return ngTemplate.templateString.getStart() + 1;
						} else {
							return mappedPos.pointInTemplate += ngTemplate.templateString.getStart() + 1;
						}
					},
					mapPosFromTemplateToGeneratedCode: (position) => {
						let posOffsetInTemplate = position - (ngTemplate.templateString.getStart() + 1);

						// See if this maps to a location in the generated code
						let mappedPos = mapPosViaMarkers(generatedFunc, posOffsetInTemplate);
						if(mappedPos.pointInGenCode === -1) {
							if(position >= endNewText){
								position += generatedFunc.length + 2;
							}
							return position;
						} else {
							mappedPos.pointInGenCode += insertionPoint + 1;
							return mappedPos.pointInGenCode;
						}
					}
				};

				return {generatedFile: newSourceFile, mappingFuncs};
			}

			function getValidSourceFile(fileName: string): ts.SourceFile {
				fileName = ts.normalizeSlashes(fileName);
				let getCanonicalFileName = ts.createGetCanonicalFileName(/* useCaseSensitivefileNames */ false);
				let sourceFile = currentProgram.getSourceFile(getCanonicalFileName(fileName));
				if (!sourceFile) {
					throw new Error("Could not find file: '" + fileName + "'.");
				}
				return sourceFile;
			}

			// Return the global symbols for the template 'context'. This is basically the instance members
			// on the component, and any introduced locals.
			function getContextSymbols(sourceFile: ts.SourceFile, position: number) : ts.Symbol[]{
				// Walk up from the position looking for block scoped variables (those introduced by #names),
				// excluding the __<name> variables used for tag types. Stop once the generated IIFE is encountered,
				// which introduces the __comp parameter, and add the instance members from this type also.
				let result: ts.Symbol[] = [];

				// Walk up the nodes from the position, stopping on nodes with locals, erroring if we get to the file level.
				let currNode = ts.getTokenAtPosition(sourceFile, position);
				while(true){
					if(!currNode || currNode.kind === ts.SyntaxKind.SourceFile){
						throw new Error("invalid start position for template locals");
					}
					if(currNode.locals){
						for(let symName in currNode.locals){
							let sym = currNode.locals[symName];
							if(sym.flags & ts.SymbolFlags.Variable){
								// Skip over locals used purely for HTML tag elements
								if(sym.getName().substring(0,2) === '__') {
									continue;
								}
								result.push(sym);
							}
						}
					}
					// Did we reach the outer IIFE?
					if(currNode.kind === ts.SyntaxKind.FunctionExpression){
						let currFunc = currNode as ts.FunctionExpression;
						// Should have just the one param, __comp
						if(currFunc.parameters.length == 1 && currFunc.parameters[0].name.getText() === '__comp'){
							let checker = currentProgram.getTypeChecker();
							let type = checker.getTypeAtLocation(currFunc.parameters[0])
							let props = type.getProperties();
							result = result.concat(props);
							break;
						}
					}
					currNode = currNode.parent;
				}

				return result;
			}

			function getNgTemplateCompletionsAtPosition(fileName: string, position: number,
                            queryGeneratedCode: (fileName: string, pos: number) => ts.CompletionInfo): ts.CompletionInfo {
				// This function should:
				// - Check it is in a template string
				// - If so, see if it is in a start tag name, attribute name, expression, or close tag.
				// - For an open tag, return the list of tags available (+ next expected close tag)
				// - For a close tag, return the next expected close tag
				// - For an attribute name, return the list of properties for the tag type
				// - For an expression, return the completions for the mapped expression location in the generated code
				let typeChecker = currentProgram.getTypeChecker();
				let sourceFile = getValidSourceFile(fileName);

                let templatesInFile = getNgTemplateStringsInSourceFile(sourceFile);
                let templateNode = getTemplateAtPosition(templatesInFile, position);
				if(!templateNode){
					return undefined;
				}

				// Parse the template string into an AST and see what position we're in
				let text = templateNode.templateString.getText();
				text = text.substring(1, text.length - 1); // Strip the surrounding back-ticks
				let htmlParser = new NgTemplateParser(text);
				let posInTemplate = position - (templateNode.templateString.getStart() + 1)
				let currNode = htmlParser.getNodeAtPosition(posInTemplate);

				let elements: ts.CompletionEntry[];

                function getExpressionCompletions(text: string, pos: number){
                    if(isAfterDot(text, pos)){
						return queryGeneratedCode(fileName, position).entries;
					} else {
                              // TODO: Should include locally introduced names also (e.g. #player)
						return getClassMembers(typeChecker, templateNode.classDecl);
					}
                }

				switch(currNode.kind){
					case ngNodeKind.StartTag:
					case ngNodeKind.SelfClosingTag:
						elements = getElementCompletions();
						if(currNode.parent.name !== '__root__'){
							elements.unshift({
							name: '/' + currNode.parent.name,
							kind: ts.ScriptElementKind.classElement,
							kindModifiers: "",
							sortText: "0"
						});
						}
						break;
					case ngNodeKind.EndTag:
						elements = [{
							name: currNode.parent.name,
							kind: ts.ScriptElementKind.classElement,
							kindModifiers: "",
							sortText: "0"
						}];
						break;
					case ngNodeKind.Attribute:
						if(posInTemplate >= (currNode as NgAttrib).valuePos){
							// If we're after a '.', delegate to getCompletions for the location in the generated code
							let attribValue = (currNode as NgAttrib).value;
							let attribPos = posInTemplate - (currNode as NgAttrib).valuePos;
                            elements = getExpressionCompletions(attribValue, attribPos);
						} else if((currNode as NgAttrib).name[0] === '*'){
							elements = getDirectiveCompletions();
						} else {
							// Get the parent tag name, the type for that, and the members of that type
							let parentTagType = tagToType[currNode.parent.name];
							if(!parentTagType){
								elements = getDummyCompletions();
								break;
							}

							// HTMLDivElement etc. are declared as variables
							// TODO: Handle custom elements, which will be classes
							let inScopeSymbols = typeChecker.getSymbolsInScope(templateNode.templateString, ts.SymbolFlags.Interface);
							if(!inScopeSymbols || inScopeSymbols.length == 0){
								elements = getDummyCompletions();
								break;
							}
							if(!inScopeSymbols.some( sym => {
								if(sym.name === parentTagType){
									let declType = typeChecker.getDeclaredTypeOfSymbol(sym);
									let members = declType.getProperties();
									elements = members.map(member => ({
										name: member.name,
										kind: member.flags & ts.SymbolFlags.Method ? ts.ScriptElementKind.memberFunctionElement : ts.ScriptElementKind.memberVariableElement,
										kindModifiers: "",
										sortText: "0"
									}));

									// Filter from event bindings to 'on*' members and remove the on.
									if((currNode as NgAttrib).name[0] === '('){
										elements = elements.filter( elem => {
											if(elem.name.length > 2 && elem.name.substring(0,2) === 'on'){
												elem.name = elem.name.substring(2);
												return true;
											}
											return false;
										});
									}
                                    // Filter data bindings to only be data properties, not methods/event handlers
                                    if((currNode as NgAttrib).name[0] === '['){
										elements = elements.filter( elem => (elem.kind === ts.ScriptElementKind.memberVariableElement));
									}
                                    return true;
								}
							})){
								elements = getDummyCompletions();
							}
						}
						break;
					case ngNodeKind.Interpolation:
						let interpText = (currNode as NgNode).getText().substring(2);
						let pos = posInTemplate - currNode.startPos - 2;
                        elements = getExpressionCompletions(interpText, pos);
						break;
				}

				var result: ts.CompletionInfo = {
					isMemberCompletion: false,
					isNewIdentifierLocation: false,
					entries: elements
				}
				return result;
			}

			function getSignatureHelpItems(fileName: string, position: number): ts.SignatureHelpItems{
				let sourceFile = getValidSourceFile(fileName);

				let templatesInFile = getNgTemplateStringsInSourceFile(sourceFile);
				if(!getTemplateAtPosition(templatesInFile, position)){
					return undefined;
                }

                let mappingInfo = filePositionMappings[fileName];
                position = mappingInfo.mapPosFromTemplateToGeneratedCode(position);
                let result = ngmlLanguageService.getSignatureHelpItems(fileName, position);
                if(mappingInfo && result){
                    let startPos = result.applicableSpan.start;
                    if(mappingInfo.isPosInGeneratedCode(startPos)){
						result.applicableSpan.start = mappingInfo.mapPosFromGeneratedCodeToTemplate(startPos);
					}
                }

                return result;
			}

			function getQuickInfoAtPosition(fileName: string, position: number): ts.QuickInfo {
				let sourceFile = getValidSourceFile(fileName);

				let templatesInFile = getNgTemplateStringsInSourceFile(sourceFile);
				if(!getTemplateAtPosition(templatesInFile, position)){
					return undefined;
                }

                let mappingInfo = filePositionMappings[fileName];
                position = mappingInfo.mapPosFromTemplateToGeneratedCode(position);
                let result = ngmlLanguageService.getQuickInfoAtPosition(fileName, position);
                if(mappingInfo && result){
                    let startPos = result.textSpan.start;
                    if(mappingInfo.isPosInGeneratedCode(startPos)){
						result.textSpan.start = mappingInfo.mapPosFromGeneratedCodeToTemplate(startPos);
					}
                }

                return result;
			}

            // Probably can't use the generated code here, as there is so much Angular2 specific syntax even in expressions.
            // The parser will need to detect and add such errors directly.
			function getSyntacticDiagnostics(sourceFile: ts.SourceFile): ts.Diagnostic[]{
				let result: ts.Diagnostic[] = [];

				getNgTemplateStringsInSourceFile(sourceFile).forEach( elem => {
					let text = elem.templateString.getText();
					text = text.substring(1, text.length - 1);
					addTemplateErrors(text, elem.templateString.getStart() + 1);
				});

				function addTemplateErrors(text: string, offset: number){
					let parser = new NgTemplateParser(text);
					parser.errors.forEach( err => {
						let parts = err.split(':');
						// TODO: Change the errors in the parser to match expected Diagnostic fields
						let diag: ts.Diagnostic = {
							file: sourceFile,
							start: parseInt(parts[1]) + offset,
							length: parseInt(parts[2]) - parseInt(parts[1]),
							messageText: parts[3].trim(),
							category: ts.DiagnosticCategory.Warning,
							code: 1
						}
						result.push(diag);
					});
				}

				return result;
			}

			function getSemanticDiagnostics(sourceFile: ts.SourceFile): ts.Diagnostic[]{
				let templatesInFile = getNgTemplateStringsInSourceFile(sourceFile);
                if(!templatesInFile.length) {
                    return [];
                }

                let fileName = sourceFile.fileName;
                let mappingInfo = filePositionMappings[fileName];

                let result = ngmlLanguageService.getSemanticDiagnostics(fileName);
                if(result && mappingInfo){
                    result.forEach( elem => {
                        let startPos = elem.start;
                        if(mappingInfo.isPosInGeneratedCode(startPos)){
					       	elem.start = mappingInfo.mapPosFromGeneratedCodeToTemplate(startPos);
					    }
                    });
                }
                return result;
			}

			return {
				version: "0.1.0",
				setProgram: (program) => {
					currentProgram = program;
					ngmlLanguageService.getProgram(); // Trigger a recalculation to update mapping funcs
				},
				getCompletionsAtPosition,
				getCompletionEntryDetails,
				getQuickInfoAtPosition,
				getDefinitionAtPosition,
				getSignatureHelpItems,
				getSyntacticDiagnostics,
				getSemanticDiagnostics
			};
		});
	}

    // TODO: The below are to return a collection of mock completions for now...
    function getElementCompletions(): ts.CompletionEntry[]{
		return Object.keys(tagToType).map( name => ({
                name,
                kind: ts.ScriptElementKind.classElement,
                kindModifiers: "",
                sortText: "0"
        }));
    }

    function getDirectiveCompletions(): ts.CompletionEntry[]{
        return ["ngFor", "ngIf", "ngSwitch"].map( name => ({
                name,
                kind: ts.ScriptElementKind.keyword,
                kindModifiers: "",
                sortText: "0"
        }));
    }

	function getDummyCompletions(): ts.CompletionEntry[]{
        return ["sausage", "bacon", "eggs"].map( name => ({
                name,
                kind: ts.ScriptElementKind.label,
                kindModifiers: "",
                sortText: "0"
        }));
    }

	// Detect if we're in a member completion position, rather than an identifier
	// TODO: Crude logic.
	function isAfterDot(expr: string, pos: number): boolean {
		while(--pos > 0){
			if(expr[pos] === '.') return true;
			if(!ts.isIdentifierPart(expr.charCodeAt(pos), ts.ScriptTarget.ES2015)) {
				return false;
			}
		}
		return false;
	}

	function getClassMembers(typeChecker: ts.TypeChecker, classDecl: ts.Node): ts.CompletionEntry[]{
		let classSymbol = typeChecker.getTypeAtLocation(classDecl) as ts.InterfaceType;
		let classProps = classSymbol.thisType.getProperties();

		var members: ts.CompletionEntry[] = classProps.map(prop => ({
			name: prop.getName(),
			kind: ts.ScriptElementKind.memberVariableElement,
			kindModifiers: "",
			sortText: "0"
		}));
		return members;
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
		parent: NgTag;
		getText?: () => string;
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
		valuePos?: number;
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
			textNodes: 0,
			totalNodes: function(){ return this.openTags + this.closeTags + this.selfClosingTags +
				this.attributes + this.comments + this.interpolations + this.textNodes;}
		};

		// Creating a new scanner will automatically populate the AST and error list.
		constructor(public text: string){
			this.currentPos = 0;
			this.errors = [];
			this.ast = this.scan();
		}

		getNodeText(node: NgNode){
			return this.text.substring(node.startPos, node.endPos);
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
				endPos: 0,
				name: "__root__",
				attributes: [],
				children: [],
				parent: null
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
				nextChild.parent = stack[stack.length - 1];
				stack[stack.length - 1].children.push(nextChild);
				// For open or close tags, move up or down the stack
				switch(nextChild.kind){
					case ngNodeKind.StartTag:
						// Start of child tag found, make the top of the stack.
						stack.push(nextChild as NgTag);
						break;
					case ngNodeKind.EndTag:
						if((nextChild as NgNamedNode).name === (stack[stack.length - 1].name)){
							// Close tag for current top of stack. Pop from stack, add as final child, and continue
							stack.pop();
						} else {
							let msg = (stack.length > 1) ?
									`Expected closing tag named "${stack[stack.length - 1].name}"` :
									`Unexpected closing tag`;

							this.errors.push(`html:${nextChild.startPos}:${this.currentPos}: ${msg}`);
						}
						break;
					default:
						// Add the child node to the current tag on top of stack
						break;
				}
			}

			// Check for unmatched tags
			while(true){
				let unmatched = stack.pop();
				if(!unmatched || unmatched === root) break;
				this.errors.push(`html:${unmatched.startPos}:${unmatched.endPos}: Unmatched opening tag`)
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
							endPos: this.currentPos,
							parent: null
						}
					}
					break;
				case '<':
					if(this.peekChar(0) === '!' && this.peekChar(1) === '-' && this.peekChar(2) === '-'){
						return this.parseComment(fullStartPos);
					} else if(this.peekChar(0) === '/') {
						return this.parseCloseTag(fullStartPos);
					} else return this.parseTag(fullStartPos);
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
				endPos: 0,
				parent: null
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
				parent: null,
				name: "",
				attributes: [],
				children: []
			}

			if(!this.isTagStartChar(this.getChar())){
				result.endPos = this.currentPos;
				this.errors.push(`html:${result.startPos}:${this.currentPos}: Invalid tag name`);
				this.stats.openTags++;
				return result;
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
					attrib.parent = parent;
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
				parent: null,
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
				result.endPos = this.currentPos;
				return result;
			} else {
			this.getChar();
			}

			this.skipWhitespace();

			let valuePos = this.currentPos;
			let valueChar = this.peekChar();
			if(valueChar === "'" || valueChar === '"' || this.isAttribChar(valueChar)){
				result.valuePos = this.currentPos;
				let endQuote: string = undefined;
				if(!this.isAttribChar(valueChar)){
					// Is a quoted string. Store it and skip over it.
					endQuote = valueChar;
					result.valuePos++;
					this.getChar();
				}

				while((valueChar = this.getChar()) !== '\x00' ){
					if(endQuote && valueChar === endQuote){
						// End of quoted string
						result.endPos = this.currentPos;
						result.value = this.text.substring(valuePos + 1, result.endPos - 1);
						return result;
					} else if(!endQuote && !this.isAttribChar(valueChar)){
						// End of unquoted value. Put back whatever extra char was consumed
						this.currentPos--;
						result.endPos = this.currentPos;
						result.value = this.text.substring(valuePos, result.endPos);
						return result;
					}
				}
				// End of stream
				result.endPos = this.currentPos;
				this.errors.push(`html:${result.startPos}:${result.endPos}: Incomplete attribute value`);
			} else {
				// TODO: Allow other forms, such as double quotes or naked. But error for now.
				result.endPos = this.currentPos;
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
				parent: null,
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
				endPos: 0,
				parent: null,
				getText: () => this.getNodeText(result)
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
				endPos: 0,
				parent: null
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

		getNodeAtPosition(pos: number): NgNode {
			if(!this.ast) return null;

			// The AST is a tree of nodes, where every non-leave Node is an Open tag (incl. Root).
			// Locate the first Node where endPos > pos (or the very last node)
			// Just keep drilling down until there are no more children.
			let lastNode: NgNode = this.ast;
			while(true){
				if(lastNode.kind !== ngNodeKind.StartTag && lastNode.kind !== ngNodeKind.Root) {
					// Doesn't have any children, so this is the Node
					break;
				} else {
					let openTag = (lastNode as NgTag);
					if(openTag.endPos > pos || !openTag.children.length) {
						let lastNode = openTag;
						break;
					} else {
						// Move through the children updating lastNode, stopping if one ends after pos.
						(lastNode as NgTag).children.some(elem => {
							lastNode = elem;
							return this.getFullEndPos(elem) > pos;
						});
					}
				}
			}
			if(lastNode.kind === ngNodeKind.StartTag || lastNode.kind === ngNodeKind.SelfClosingTag){
				let attrib = this.getAttribAtPosition(lastNode as NgTag, pos);
				if(attrib){
					lastNode = attrib;
				}
			}
			return lastNode;
		}

		// Utility to work with open tags. Finds the endPos including children (and close tags).
		getFullEndPos(tag: NgNode){
			if(tag.kind !== ngNodeKind.StartTag && tag.kind !== ngNodeKind.Root){
				return tag.endPos;
			} else {
				let openTag = tag as NgTag;
				if(!openTag.children.length){
					return openTag.endPos;
				} else {
					return openTag.children[openTag.children.length - 1].endPos;
				}
			}
		}

		getAttribAtPosition(node: NgTag, pos: number): NgAttrib{
			let result: NgAttrib = null;
			node.attributes.forEach( attrib => {
				if(pos >= attrib.startPos && pos <= attrib.endPos) {
					result = attrib;
				}
			});
			return result;
		}
	}

	function generateFunction(ast: NgNode, componentType: string): string {
		type nameTable = string[]; // Maps name to type
		var availableGlobals: nameTable = []; // None, I believe.
		var nameScopes: nameTable[] = [availableGlobals];
		let body = "";

		if(ast){
			let indent = '  ';
			body = processNode(ast as NgTag);
			function processNode(node: NgTag): string {
				let map: {[index: string]: string} = {};
				let locals: {[index: string]: string} = {};
				let blocks: string[] = [];
				let statements: string[] = [];
				let names: nameTable = [];
				nameScopes.push(names);
				node.children.forEach( child => {
					if(child.kind == ngNodeKind.StartTag || child.kind == ngNodeKind.SelfClosingTag){
						// Declare a local of each tag type needed.
						let tagNode = child as NgTag;
						let tagType = tagToType[tagNode.name];
						if(!tagType) tagType = "HTMLElement";
						map[tagNode.name] = `let __${tagNode.name} = new ${tagType}();\n`;

						// Add any local names introduced
						tagNode.attributes.forEach( attrib => {
							if(attrib.name[0] == '#' && !attrib.value){
								// TODO: This removes/hides duplicate identifier errors if a local is used twice
								let attribName = fixupName(attrib.name.substring(1));
								let nameText = `/*{start:${attrib.startPos}}*/${attribName}/*{end:${attrib.startPos + attrib.name.length}}*/`
								locals[attribName] = `let ${nameText} = __${tagNode.name};\n`
								names.push(attribName);
							}
						});

						// Declare a statement block for each element also.
						let block = indent + '{\n';
						let oldIndent = indent;
						indent += '  ';
						// Skip empty blocks
						let blockText = processNode(tagNode);
						block += blockText;
						indent = oldIndent;
						block += indent + '}\n';
						if(blockText.trim()) {
							blocks.push(block);
						}
					} else if(child.kind == ngNodeKind.Interpolation){
						let expr = child.getText();
						expr = expr.substring(2, expr.length - 2); // Trim the {{-}}
						expr = indent + `(${bindNames(expr, child.startPos + 2, child.endPos - 2)});\n`;
						statements.push(expr);
					}
				});
				node.attributes.forEach( attrib => {
					if((attrib.name[0] == '[' || attrib.name[0] == '(') && attrib.value){
						// Data or event binding
						let isEvent = attrib.name[0] == '(';
						let name = attrib.name.substring(1, attrib.name.length - 1);
						name = fixupName(name, isEvent);
						let value = bindNames(attrib.value, attrib.valuePos, attrib.valuePos + attrib.value.length);
						let tagName = attrib.parent.name;
						let markedName = `/*{start:${attrib.startPos + 1}}*/${name}/*{end:${attrib.startPos + attrib.name.length - 1}}*/`;
						if(isEvent){
							statements.push(indent + `__${tagName}.${markedName} = $event => ${value};\n`);
						} else {
							statements.push(indent + `__${tagName}.${markedName} = ${value};\n`);
						}
					} else {
						// TODO: Handle interpolation inside attributes here, or add a specific child node?
					}
				});
				nameScopes.pop();

				let result = "";
				for(let key in map){
					result += indent + map[key];
				}
				for(let key in locals){
					result += indent + locals[key];
				}
				statements.forEach(line => result += line);
				blocks.forEach(block => result += block);
				return result;
			}

			function fixupName(name: string, isEvent: boolean = false) : string {
				let result = name;
				if(isEvent){
					result = 'on' + result;
				}
				return result;
			}

			function bindNames(expr: string, start: number, end: number): string {
				// TODO: Need to break this apart to find each identifier. Just handles a raw name to first separator for now.
				let name = expr;
				[' ', '.', '('].forEach(char => {
					let trimAt = name.indexOf(char);
					if(trimAt !== -1){
						name = name.substring(0, trimAt);
					}
				});

				for(let i = nameScopes.length - 1; i > 0; i--){
					let scope = nameScopes[i];
					if(scope.indexOf(name) !== -1){
						// It's declared in an outscope scope. Use it directly
						return `/*{start:${start}}*/${expr}/*{end:${end}}*/`;
					}
				}
				// Bind it to the component instance
				return expr.replace(name, "__comp." + `/*{start:${start}}*/${name}`) + `/*{end:${end}}*/`;
			}
		}

		return `(function(__comp: ${componentType}){
${body}})(null);`;
	}

	// TODO: Should create one of these per component/parser, as each may add custom tags
	let tagToType: {[index: string]: string} = {
		// Copied from the createElement overloads in lib.d.ts
		"a": "HTMLAnchorElement",
		"abbr": "HTMLPhraseElement",
		"acronym": "HTMLPhraseElement",
		"address": "HTMLBlockElement",
		"applet": "HTMLAppletElement",
		"area": "HTMLAreaElement",
		"audio": "HTMLAudioElement",
		"b": "HTMLPhraseElement",
		"base": "HTMLBaseElement",
		"basefont": "HTMLBaseFontElement",
		"bdo": "HTMLPhraseElement",
		"big": "HTMLPhraseElement",
		"blockquote": "HTMLBlockElement",
		"body": "HTMLBodyElement",
		"br": "HTMLBRElement",
		"button": "HTMLButtonElement",
		"canvas": "HTMLCanvasElement",
		"caption": "HTMLTableCaptionElement",
		"center": "HTMLBlockElement",
		"cite": "HTMLPhraseElement",
		"code": "HTMLPhraseElement",
		"col": "HTMLTableColElement",
		"colgroup": "HTMLTableColElement",
		"datalist": "HTMLDataListElement",
		"dd": "HTMLDDElement",
		"del": "HTMLModElement",
		"dfn": "HTMLPhraseElement",
		"dir": "HTMLDirectoryElement",
		"div": "HTMLDivElement",
		"dl": "HTMLDListElement",
		"dt": "HTMLDTElement",
		"em": "HTMLPhraseElement",
		"embed": "HTMLEmbedElement",
		"fieldset": "HTMLFieldSetElement",
		"font": "HTMLFontElement",
		"form": "HTMLFormElement",
		"frame": "HTMLFrameElement",
		"frameset": "HTMLFrameSetElement",
		"h1": "HTMLHeadingElement",
		"h2": "HTMLHeadingElement",
		"h3": "HTMLHeadingElement",
		"h4": "HTMLHeadingElement",
		"h5": "HTMLHeadingElement",
		"h6": "HTMLHeadingElement",
		"head": "HTMLHeadElement",
		"hr": "HTMLHRElement",
		"html": "HTMLHtmlElement",
		"i": "HTMLPhraseElement",
		"iframe": "HTMLIFrameElement",
		"img": "HTMLImageElement",
		"input": "HTMLInputElement",
		"ins": "HTMLModElement",
		"isindex": "HTMLIsIndexElement",
		"kbd": "HTMLPhraseElement",
		"keygen": "HTMLBlockElement",
		"label": "HTMLLabelElement",
		"legend": "HTMLLegendElement",
		"li": "HTMLLIElement",
		"link": "HTMLLinkElement",
		"listing": "HTMLBlockElement",
		"map": "HTMLMapElement",
		"marquee": "HTMLMarqueeElement",
		"menu": "HTMLMenuElement",
		"meta": "HTMLMetaElement",
		"nextid": "HTMLNextIdElement",
		"nobr": "HTMLPhraseElement",
		"object": "HTMLObjectElement",
		"ol": "HTMLOListElement",
		"optgroup": "HTMLOptGroupElement",
		"option": "HTMLOptionElement",
		"p": "HTMLParagraphElement",
		"param": "HTMLParamElement",
		"plaintext": "HTMLBlockElement",
		"pre": "HTMLPreElement",
		"progress": "HTMLProgressElement",
		"q": "HTMLQuoteElement",
		"rt": "HTMLPhraseElement",
		"ruby": "HTMLPhraseElement",
		"s": "HTMLPhraseElement",
		"samp": "HTMLPhraseElement",
		"script": "HTMLScriptElement",
		"select": "HTMLSelectElement",
		"small": "HTMLPhraseElement",
		"source": "HTMLSourceElement",
		"span": "HTMLSpanElement",
		"strike": "HTMLPhraseElement",
		"strong": "HTMLPhraseElement",
		"style": "HTMLStyleElement",
		"sub": "HTMLPhraseElement",
		"sup": "HTMLPhraseElement",
		"table": "HTMLTableElement",
		"tbody": "HTMLTableSectionElement",
		"td": "HTMLTableDataCellElement",
		"textarea": "HTMLTextAreaElement",
		"tfoot": "HTMLTableSectionElement",
		"th": "HTMLTableHeaderCellElement",
		"thead": "HTMLTableSectionElement",
		"title": "HTMLTitleElement",
		"tr": "HTMLTableRowElement",
		"track": "HTMLTrackElement",
		"tt": "HTMLPhraseElement",
		"u": "HTMLPhraseElement",
		"ul": "HTMLUListElement",
		"var": "HTMLPhraseElement",
		"video": "HTMLVideoElement",
		"x-ms-webview": "MSHTMLWebViewElement",
		"xmp": "HTMLBlockElement"
	};

	function stripMarkers(input: string){
		return input.replace(/\/\*\{(start|end):\d+}\*\//g, "");
	}

	interface CodeMapping {
		pointInTemplate: number;
		startRangeInTemplate: number;
		endRangeInTemplate: number;
		pointInGenCode: number;
		startRangeInGenCode: number;
		endRangeInGenCode: number;
	}

	// This function is given the generated code with the markers, and a position from the template to try
	// and location within it. It searches the markers to see if the position from the template maps to a
	// location in the generated code.
	// To do the reverse (map a point in the generated code to the template), is also iterates through the
	// markers in the generated code, and sees if the point is within a start/end marker span in the generated code.
	function mapPosViaMarkers(input: string, pos: number, codeToTemplate: boolean = false): CodeMapping {
		let result: CodeMapping = {
			pointInTemplate: codeToTemplate ? -1 : pos,
			startRangeInTemplate: -1,
			endRangeInTemplate: -1,
			pointInGenCode: codeToTemplate ? pos : -1,
			startRangeInGenCode: -1,
			endRangeInGenCode: -1
		};

		// We want to find the start/end markers separately to easily get lastIndexOf as the first position after the start marker
		let startMarker = /\/\*\{start:(\d+)}\*\//g;
		let endMarker = /\/\*\{end:(\d+)}\*\//g;

		let startResult: RegExpExecArray = null;
		let endResult: RegExpExecArray = null;
		while(startResult = startMarker.exec(input)){
			// Always advance in pairs
			endResult = endMarker.exec(input);
			let startPos = parseInt(startResult[1]);
			let endPos = parseInt(endResult[1]);
			if(codeToTemplate){
				// Is the pos within the marker locations?
				if(pos >= startMarker.lastIndex && pos < endMarker.lastIndex){
					result.startRangeInTemplate = startPos;
					result.endRangeInTemplate = endPos;
					result.startRangeInGenCode = startMarker.lastIndex;
					result.endRangeInGenCode = endMarker.lastIndex - (endResult[0].length);
					result.pointInTemplate = startPos + (pos - startMarker.lastIndex);
					// Ensure we don't overrun the mapping due to renames
					if(result.pointInTemplate > endPos) {
						result.pointInTemplate = endPos;
					}
					break;
				}
			} else {
				// Is the pos within the marker values?
				if(pos >= startPos && pos <= endPos){
					// Found a range that matches.
					result.pointInGenCode = startMarker.lastIndex + (pos - startPos);
					result.startRangeInTemplate = startPos;
					result.endRangeInTemplate = endPos;
					result.startRangeInGenCode = startMarker.lastIndex;
					result.endRangeInGenCode = endMarker.lastIndex - (endResult[0].length);
					// Ensure we don't overrun the mapping due to renames
					if(result.pointInGenCode > result.endRangeInGenCode) {
						result.pointInGenCode = result.endRangeInGenCode;
					}
					break;
				}
			}
		}

		return result;
	}

	// This function is used for mapping an error in the generated code, to a range in the template code.
	// It basically takes the range of the error, and finds the first range in the template with some overlap.
	function findFirstOverlap(input: string, startPos: number, endPos: number){
		// Loop though the input finding range marker pairs.
		let markerPairRegex = /\/\*\{start:(\d+)}\*\/.+?\/\*\{end:(\d+)}\*\//g;
		let markerPairResult: RegExpExecArray = null;

		while(markerPairResult = markerPairRegex.exec(input)){
			// When found, see if the span of that range intersects with the range given.
			let endMatch = markerPairRegex.lastIndex;
			let startMatch = endMatch - markerPairResult[0].length;
			if(endMatch >= startPos && startMatch <= endPos){
				// If so, return the range as specified by the markers.
				let startRange = parseInt(markerPairResult[1]);
				let endRange = parseInt(markerPairResult[2]);
				return {startRange, endRange};
			}
		}
		return null;
	}

	export function testParser(){
		let assert = (condition: boolean, msg?: string) => {
			if(!condition) {
				throw new Error(msg);
			}
		};

		let getPos = (input: string) => ({
			cursor: input.indexOf('~'),
			result: input.split('~').join('')
		});

		var tmp = new NgTemplateParser(`<!-- A comment --><div name='foo'>Hi {{this.name}}</div><br/>`);
		assert(tmp.stats.totalNodes() === 7, "Expected 7 nodes");
		assert(tmp.stats.comments == 1, "Expected 1 comment");
		assert(tmp.stats.openTags == 1, "Expected 1 open tag");
		assert(tmp.stats.attributes == 1, "Expected 1 attribute");
		assert(tmp.stats.textNodes == 1, "Expected 1 text node");
		assert(tmp.stats.interpolations == 1, "Expected 1 interpolation");
		assert(tmp.stats.closeTags == 1, "Expected 1 close tag");
		assert(tmp.stats.selfClosingTags == 1, "Expected 1 self closing tag");
		assert(tmp.ast.children.length == 3, "Expected fragment to have 3 children");
		assert(tmp.ast.children[0].kind === ngNodeKind.Comment, "Expected child 0 to be a comment");
		assert(tmp.ast.children[1].kind == ngNodeKind.StartTag, "Expected second child to be a start tag");
		let elem1 = tmp.ast.children[1] as NgTag;
		assert(elem1.name == "div", "Expected second child to be named 'div");
		assert(elem1.startPos === 18, "Expected div tag to start at pos 18");
		assert(elem1.endPos === 34, "Expected div tag to end at pos 18");

		tmp = new NgTemplateParser(`<div>`);
		assert(tmp.stats.openTags == 1, "Expected 1 open tag");
		assert(tmp.stats.totalNodes() === 1, "Expected 1 node total");
		assert(tmp.errors.length == 1, "Expected 1 error");
		assert(tmp.errors[0] == "html:0:5: Unmatched opening tag", "Error does not match expected text");

		tmp = new NgTemplateParser(`<p foo=bar/>`);
		assert(tmp.errors.length === 0, "Expected 0 attribute parse error");
		assert(tmp.stats.totalNodes() === 2, "Expected 2 nodes total");
		assert(tmp.stats.selfClosingTags === 1, "Expected 1 self-closing tags");
		assert(tmp.ast.children[0].endPos === 12, "Expected first child to end as pos 12");
		assert((tmp.ast.children[0] as NgNamedNode).name === 'p', "Expected tag named 'p");
		assert(tmp.stats.attributes === 1, "Expected 1 attribute");
		let attrib = (tmp.ast.children[0] as NgTag).attributes[0];
		assert(attrib.name === 'foo', "Expected first attribute to be named 'foo'");
		assert(attrib.value === 'bar', "Expected attribute value to be 'bar");

		tmp = new NgTemplateParser("");
		assert(tmp.errors.length === 0, "Empty string should parse fine");
		assert(tmp.stats.totalNodes() === 0, "empty string should have no nodes");
		let nodeAtPos = tmp.getNodeAtPosition(0);
		assert(nodeAtPos === null, "Expected null for node at 0 pos in empty string")

		let input = getPos("<~");
		tmp = new NgTemplateParser(input.result);
		assert(tmp.stats.totalNodes() === 1, "Expected 1 node");
		assert(tmp.errors.length === 2, "Expected 2 errors");
		assert(tmp.errors[0] === "html:0:1: Invalid tag name", "Error does not match expected text");
		assert(tmp.errors[1] === "html:0:1: Unmatched opening tag", "Error does not match expected text");
		nodeAtPos = tmp.getNodeAtPosition(input.cursor);
		assert(nodeAtPos.kind === ngNodeKind.StartTag, "Expected start tag at position");
		assert((nodeAtPos as NgTag).name === "", "Expected node at position to have empty");
		assert(nodeAtPos.parent.name === "__root__", "Expected node at position to have root parent");

		input = getPos("<div></~");
		tmp = new NgTemplateParser(input.result);
		assert(tmp.stats.totalNodes() === 2, "Expected 2 nodes");
		assert(tmp.errors.length === 3, "Expected 2 errors");
		assert(tmp.errors[0] === "html:5:7: Incomplete closing tag", "Error does not match expected text");
		assert(tmp.errors[1] === 'html:5:7: Expected closing tag named "div"', "Error does not match expected text");
		assert(tmp.errors[2] === "html:0:5: Unmatched opening tag", "Error does not match expected text");
		nodeAtPos = tmp.getNodeAtPosition(input.cursor);
		assert(nodeAtPos.kind === ngNodeKind.EndTag, "Expected end tag at position");
		assert(nodeAtPos.parent.name === "div", "Expected node at position to have 'div' parent");

		input = getPos("<div name = 'te~st'/>");
		tmp = new NgTemplateParser(input.result);
		nodeAtPos = tmp.getNodeAtPosition(input.cursor);
		assert(nodeAtPos.kind === ngNodeKind.Attribute);
		attrib = nodeAtPos as NgAttrib;
		assert(attrib.name === "name");
		assert(attrib.startPos === 5);
		assert(attrib.valuePos === input.cursor - 2);
		assert(attrib.value === "test");
		assert(nodeAtPos.parent.kind === ngNodeKind.SelfClosingTag);
		assert((nodeAtPos.parent as NgTag).name === "div");

		// GenerateFunction tests
		let result = generateFunction(null, 'MyComp');
		let expected = `(function(__comp: MyComp){
})(null);`;
		assert(result == expected);


		tmp = new NgTemplateParser("<div></div>");
		result = generateFunction(tmp.ast, 'MyComp');
		expected = `(function(__comp: MyComp){
  let __div = new HTMLDivElement();
})(null);`;
		assert(result == expected);


		tmp = new NgTemplateParser("<div><p>Hello</p></div>");
		result = generateFunction(tmp.ast, 'MyComp');
		expected = `(function(__comp: MyComp){
  let __div = new HTMLDivElement();
  {
    let __p = new HTMLParagraphElement();
  }
})(null);`;
		assert(result == expected);

		tmp = new NgTemplateParser("<div>{{greeting}}</div>");
		result = generateFunction(tmp.ast, 'MyComp');
		expected = `(function(__comp: MyComp){
  let __div = new HTMLDivElement();
  {
    (__comp./*{start:7}*/greeting/*{end:15}*/);
  }
})(null);`;
		assert(result == expected);

		tmp = new NgTemplateParser("<div [data]='myProp'>Test</div>");
		result = generateFunction(tmp.ast, 'MyComp');
		expected = `(function(__comp: MyComp){
  let __div = new HTMLDivElement();
  {
    __div./*{start:6}*/data/*{end:10}*/ = __comp./*{start:13}*/myProp/*{end:19}*/;
  }
})(null);`;
		assert(result == expected);

		tmp = new NgTemplateParser("<div (click)='handleClick()'>Test</div>");
		result = generateFunction(tmp.ast, 'MyComp');
		result = stripMarkers(result);
		expected = `(function(__comp: MyComp){
  let __div = new HTMLDivElement();
  {
    __div.onclick = $event => __comp.handleClick();
  }
})(null);`;
		assert(result == expected);

		tmp = new NgTemplateParser("<div (clickHandler)='handleClick($event)' [textContent]='data'>Test</div>");
		result = generateFunction(tmp.ast, 'MyComp');
		expected = `(function(__comp: MyComp){
  let __div = new HTMLDivElement();
  {
    __div./*{start:6}*/onclickHandler/*{end:18}*/ = $event => __comp./*{start:21}*/handleClick($event)/*{end:40}*/;
    __div./*{start:43}*/textContent/*{end:54}*/ = __comp./*{start:57}*/data/*{end:61}*/;
  }
})(null);`;
		assert(result == expected);

		tmp = new NgTemplateParser(`
<player #myPlayer/>
<button (click)='myPlayer.play()'>Play</button>`);
		result = generateFunction(tmp.ast, 'MyComp');
		result = stripMarkers(result);
		expected = `(function(__comp: MyComp){
  let __player = new HTMLElement();
  let __button = new HTMLButtonElement();
  let myPlayer = __player;
  {
    __button.onclick = $event => myPlayer.play();
  }
})(null);`;
		assert(result == expected);


		tmp = new NgTemplateParser(`<div [style]='divStyle'>
  <h1 #myHeader>{{greeting}}</h1>
  <p (hover)='hide(myHeader)' [textContent]='myHeader.value'></p>
</div>`);
		result = generateFunction(tmp.ast, 'MyComp');
		result = stripMarkers(result);
		expected = `(function(__comp: MyComp){
  let __div = new HTMLDivElement();
  {
    let __h1 = new HTMLHeadingElement();
    let __p = new HTMLParagraphElement();
    let myHeader = __h1;
    __div.style = __comp.divStyle;
    {
      (__comp.greeting);
    }
    {
      __p.onhover = $event => __comp.hide(myHeader);
      __p.textContent = myHeader.value;
    }
  }
})(null);`;
		assert(result == expected);


		let ngml = "<div [style.]='items.' (click)='foo(10, '>{{foo.}}</div>";
		tmp = new NgTemplateParser(ngml);
		result = generateFunction(tmp.ast, 'MyComp');
		expected = `(function(__comp: MyComp){
  let __div = new HTMLDivElement();
  {
    (__comp./*{start:44}*/foo./*{end:48}*/);
    __div./*{start:6}*/style./*{end:12}*/ = __comp./*{start:15}*/items./*{end:21}*/;
    __div./*{start:24}*/onclick/*{end:29}*/ = $event => __comp./*{start:32}*/foo(10, /*{end:40}*/;
  }
})(null);`;
		assert(result == expected);
		nodeAtPos = tmp.getNodeAtPosition(48);
		assert(nodeAtPos.kind === ngNodeKind.Interpolation);

		nodeAtPos = tmp.getNodeAtPosition(32);
		assert(nodeAtPos.kind === ngNodeKind.Attribute);
		assert((nodeAtPos as NgAttrib).name === '(click)');

		let mappedPos = mapPosViaMarkers(result, 5);
		assert(mappedPos.pointInGenCode === -1);

		// Check cursor in input at [s|tyle] maps to generated at __div.s|tyle
		mappedPos = mapPosViaMarkers(result, 7);
		assert(mappedPos.pointInGenCode === 136);

		// Check an error range in the generated text maps to the template
		let interpStart = result.indexOf("(__comp.") + 1;
		let errRange = findFirstOverlap(result, interpStart, interpStart + 37);
		let interpExprStart = ngml.indexOf("{{foo.}}") + 2;
		assert(errRange.startRange === interpExprStart);
		assert(errRange.endRange === errRange.startRange + 4);

		/*
		TODO
		 - Create the right element type for the tag name.
		 - Include custom elements/directives in above list.
		 - Get the event and property names from above types.
		 - Break apart and bind the template expressions more accurately.
		 - Implement mapping to map the generated code to the template.
		 - Micro-syntax and bindings for directives, e.g.: *ng-for="#hero of heroes", #i=index"
		 - Better understanding of template syntax, such as pipes and elvis'.

		Template syntax notes
		=====================
		 - There is no global scope available. All expressions are instance members.
		 - To model this, bind any non-local identifiers to an instance of the class.
		 - Other names in scope are pipes, but these (I believe) are always preceded by "|".
		 - Standard attributes are HTML attributes, not the DOM properties as specified by typings and bound by Angular.
		 - Do we want to provide intellisense for standard attributes? Where would these be specified?
		 - Need to disallow 'this' keyword (should work if just not recognized and prefixed with "this.", i.e. "this.this")
		 - For simple examples, see the test cases.

		Name scoping notes
		==================
		Every sibling element needs to be in the same block, to see each others #name.
		Any "#item in items" directive needs 'item' to not be visible to siblings, but visible to its own expressions.
		e.g. <div *ng-for="#hero of heroes", #i=index">{{i+1}} - {{hero.fullName}}</div>
		Data and event names also need to bind to these introduced locals
		e.g. <little-hero *ng-for="#hero of heroes" [hero]="hero"></little-hero>
		So every sibling element generates a sibling element to represent itself in the current block...
		... and a block for all its additional names, attributes, and children.

		Here is the psuedo-code
		=======================
		Generate the surrounding IIFE. Now insert inside it...
		For each node with children or attributes, starting at __root__
		##ProcessNode
		 - Pre-pass: Push a new nameTable on nameScopes.
		     For each direct child tag, add "let __<tag> = new HTML<tagName>Element();" to a unique dictionary.
		     For each with a #<name>, also emit "let <name> = __<tag>;" to a dictionary, and add <name> to nameTable.
		     Emit statements from __<tag> dictionary, then <name> dictionary (this avoids duplicates).
		 - For each direct tag child again:
		     Generate a new statment block and push a new nameTable.
		     Pre-pass: Go over the directive attributes and find each that may introduce a new name, e.g. *ng-for='#item in items'
		       For "#<name> in <id>" types, call 'bindExpr' on <id>, and emit = "let <name> = <boundExpr>;".
		       Add <name> to nameTable.
		     For every non-directive attribute:
		       Call 'bindExpr' on <value> to rebind expressions, and 'fixName' on <name> to fixup names (snake-case to mixed-case).
		         Note: This handles regular attributes differently, and only extracts/binds interpolations and embeds in "(...);".
		         Note: Needs to embed interpol expressions in "(..);" as they may be object literals, and will be expression statements.
		       If it's a data binding, emit as: __<tag>.<fixedname> = <boundExpr>;
		       If it's an event, emit as: __<tag>.<fixedname> = ($event) => <boundExpr>;
		       If it's a regular attrib, emit: <boundExpr>
		       TODO: Two-way syntax, i.e. [(foo)] = 'expr'
		     Call ##ProcessNode on the child
		     Pop the nameTable.
		 - For every child interpolation node:
		     Call 'bindExpr' on the expressions, emit as: <boundExpr>
		*/
	}
}
