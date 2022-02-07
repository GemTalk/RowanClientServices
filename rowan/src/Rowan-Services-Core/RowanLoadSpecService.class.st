Class {
	#name : 'RowanLoadSpecService',
	#superclass : 'RowanDefinitionService',
	#instVars : [
		'loadSpecOop',
		'loadSpecPath'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'client commands' }
RowanLoadSpecService >> editInWindow: handle [ 
	loadSpecOop := (RwSpecification fromFile: loadSpecPath) asOop.
	handle
		ifNotNil: [ RowanBrowserService new saveRootObject: loadSpecOop windowHandle: handle ].
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanLoadSpecService >> exportLoadSpecification [
	self halt
]

{ #category : 'client commands' }
RowanLoadSpecService >> setConditionalAttributes: array [
	self definition customConditionalAttributes: array
]

{ #category : 'client commands' }
RowanLoadSpecService >> setLoadSpecProperty: property to: value [
	self definition loadSpecification
		perform: (property , ':') asSymbol
		with: value
]

{ #category : 'client commands' }
RowanLoadSpecService >> setPlatformProperty: property to: value [
	property = #'defaultMethodEnv'
		ifTrue: [ self definition loadSpecification gemstoneSetDefaultMethodEnvTo: value ].
	property = #'defaultSymbolDict'
		ifTrue: [ self definition loadSpecification gemstoneSetDefaultSymbolDictNameTo: value ].

	property = #'useSessionMethods'
		ifTrue: [ 
			self definition loadSpecification
				gemstoneSetDefaultUseSessionMethodsForExtensionsTo: value ]
]
