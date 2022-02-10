Class {
	#name : 'RowanLoadSpecService',
	#superclass : 'RowanInspectorService',
	#instVars : [
		'loadSpecOop',
		'loadSpecPath',
		'comment',
		'conditionalAttributes',
		'platformProperties',
		'loadSpec'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'command support' }
RowanLoadSpecService >> addInstVars: anObject [
	"coerce the oops into strings for display in the browsers"

	objects := Array new.
	super addInstVars: anObject.
	1 to: objects size do: [ :index | 
		objects
			at: index
			put:
				(objects at: index) key
					->
						((Object _objectForOop: (objects at: index) value) ifNil: [ String new ])
							asString ]
]

{ #category : 'updating' }
RowanLoadSpecService >> allUsersName [

	^self loadSpec _gemstoneAllUsersName
]

{ #category : 'client commands' }
RowanLoadSpecService >> editInWindow: handle [ 
	loadSpecOop := (RwSpecification fromFile: loadSpecPath) asOop.
	handle
		ifNotNil: [ RowanBrowserService new saveRootObject: loadSpecOop windowHandle: handle ].
	self update. 
	RowanCommandResult addResult: self
]

{ #category : 'ston' }
RowanLoadSpecService >> excludedInstVars [

	^super excludedInstVars, #( #loadSpec)
]

{ #category : 'client commands' }
RowanLoadSpecService >> export [
	| path directory |
	path := Path from: loadSpecPath.
	directory := path parent fullName. 
	self loadSpec exportTo:  (FileReference / directory)
]

{ #category : 'constants' }
RowanLoadSpecService >> hiddenAttributes [
	^ #('customConditionalAttributes' 'comment' 'componentNames' 'platformProperties' 'mercurialUrl' 'svnUrl' 'projectsHome' 'repositoryResolutionPolicy')
]

{ #category : 'initialization' }
RowanLoadSpecService >> initialize [
	super initialize.
	isOop := true.
	maxIndexedVars := 0.
	conditionalAttributes := Array new.
	comment := String new.
	platformProperties := Dictionary new.
]

{ #category : 'initialization' }
RowanLoadSpecService >> initialize: anOop [
	"needed?"
	isOop := true. 
	maxIndexedVars := 0.
	conditionalAttributes := Array new. 
	comment := String new. 
	super inspect: anOop
]

{ #category : 'constants' }
RowanLoadSpecService >> instVarPrefix [
	^ String new
]

{ #category : 'client commands' }
RowanLoadSpecService >> load [
	self loadSpec load
]

{ #category : 'accessing' }
RowanLoadSpecService >> loadSpec [

	^loadSpec ifNil: [loadSpec := Object _objectForOop: loadSpecOop]
]

{ #category : 'accessing' }
RowanLoadSpecService >> removeHiddenAttributes [
	self hiddenAttributes
		do: [ :attribute | objects remove: (objects detect: [ :assoc | assoc key = attribute ]) ]
]

{ #category : 'client commands' }
RowanLoadSpecService >> setComment: aString [
	self loadSpec comment: aString
]

{ #category : 'client commands' }
RowanLoadSpecService >> setConditionalAttributes: anArray [
	self loadSpec customConditionalAttributes: anArray
]

{ #category : 'client commands' }
RowanLoadSpecService >> setLoadSpecProperty: property to: value [
	self loadSpec
		perform: (property , ':') asSymbol
		with: value
]

{ #category : 'client commands' }
RowanLoadSpecService >> setPlatformProperty: property to: value [
	property = #'defaultMethodEnv'
		ifTrue: [ self loadSpec gemstoneSetDefaultMethodEnvTo: value ].
	property = #'defaultSymbolDict'
		ifTrue: [ self loadSpec gemstoneSetDefaultSymbolDictNameTo: value ].
	property = #'useSessionMethods'
		ifTrue: [ self loadSpec gemstoneSetDefaultUseSessionMethodsForExtensionsTo: value ]
]

{ #category : 'updating' }
RowanLoadSpecService >> update [
	self updateSettableAttributes.
	comment := self loadSpec comment.
	conditionalAttributes := self loadSpec customConditionalAttributes asOrderedCollection. 
	self updatePlatformProperties.
]

{ #category : 'updating' }
RowanLoadSpecService >> updatePlatformProperties [
	platformProperties := Dictionary new.
	platformProperties
		at: #'defaultMethodEnv'
		put: (self loadSpec gemstoneDefaultMethodEnvForUser: self allUsersName).
	platformProperties
		at: #'defaultSymbolDict'
		put: self loadSpec gemstoneDefaultSymbolDictName.
	platformProperties
		at: #'useSessionMethods'
		put:
			(self loadSpec
				gemstoneDefaultUseSessionMethodsForExtensionsForUser: self allUsersName)
]

{ #category : 'updating' }
RowanLoadSpecService >> updateSettableAttributes [
	self initialize: loadSpecOop.
	self removeHiddenAttributes
]
