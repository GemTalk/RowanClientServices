Class {
	#name : 'RowanSpecificationService',
	#superclass : 'RowanInspectorService',
	#instVars : [
		'loadSpecOop',
		'loadSpecPath'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'command support' }
RowanSpecificationService >> addInstVars: anObject [
	"coerce the oops into strings for display in the browsers"

	super addInstVars: anObject.
	1 to: objects size do: [ :index | 
		objects
			at: index
			put:
				(objects at: index) key
					-> (Object _objectForOop: (objects at: index) value) printString ]
]

{ #category : 'client commands' }
RowanSpecificationService >> editInWindow: handle [ 
	loadSpecOop := (RwSpecification fromFile: loadSpecPath) asOop.
	handle
		ifNotNil: [ RowanBrowserService new saveRootObject: loadSpecOop windowHandle: handle ].
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanSpecificationService >> exportLoadSpecification [
	self halt
]

{ #category : 'constants' }
RowanSpecificationService >> hiddenAttributes [
	^ #('customConditionalAttributes' 'comment' 'componentNames' 'platformProperties' 'mercurialUrl' 'svnUrl' 'projectsHome' 'repositoryResolutionPolicy')
]

{ #category : 'initialization' }
RowanSpecificationService >> initialize: anOop [
	"intention revealing - not really an inspect"
	isOop := true. 
	maxIndexedVars := 0. 
	super inspect: anOop
]

{ #category : 'constants' }
RowanSpecificationService >> instVarPrefix [
	^ String new
]

{ #category : 'accessing' }
RowanSpecificationService >> removeHiddenAttributes [
	self hiddenAttributes
		do: [ :attribute | objects remove: (objects detect: [ :assoc | assoc key = attribute ]) ]
]

{ #category : 'client commands' }
RowanSpecificationService >> setConditionalAttributes: array [
	self definition customConditionalAttributes: array
]

{ #category : 'client commands' }
RowanSpecificationService >> setLoadSpecProperty: property to: value [
	self definition loadSpecification
		perform: (property , ':') asSymbol
		with: value
]

{ #category : 'client commands' }
RowanSpecificationService >> setPlatformProperty: property to: value [
	property = #'defaultMethodEnv'
		ifTrue: [ self definition loadSpecification gemstoneSetDefaultMethodEnvTo: value ].
	property = #'defaultSymbolDict'
		ifTrue: [ self definition loadSpecification gemstoneSetDefaultSymbolDictNameTo: value ].

	property = #'useSessionMethods'
		ifTrue: [ 
			self definition loadSpecification
				gemstoneSetDefaultUseSessionMethodsForExtensionsTo: value ]
]
