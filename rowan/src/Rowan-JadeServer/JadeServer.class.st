Class {
	#name : 'JadeServer',
	#superclass : 'Object',
	#instVars : [
		'classList',
		'classOrganizers',
		'readStream',
		'writeStream',
		'selectedClass',
		'methodFilterType',
		'methodFilters',
		'selections',
		'methodCommandResult'
	],
	#classVars : [
		'ExternalInteger',
		'GciError',
		'GsObject',
		'OopType32',
		'OopType64'
	],
	#category : 'Rowan-JadeServer'
}

{ #category : 'jadeite server' }
JadeServer class >> theJadeiteServer [
	^ SessionTemps current
		at: #'jadeiteServer'
		ifAbsentPut: [ 
			(Rowan jadeServerClassNamed: #'JadeServer64bit35') new
				initialize;
				yourself ]
]

{ #category : 'category' }
JadeServer >> _describeMCAddition: anMCAddition on: aStream [
	aStream
		nextPut: $A;
		tab;
		nextPutAll: (self oopOf: anMCAddition) printString;
		tab;
		yourself.
	self _describeMCDefinition: anMCAddition definition on: aStream
]

{ #category : 'category' }
JadeServer >> _describeMCClassDefinition: anMCClassDefinition on: aStream [
	| string |
	string := anMCClassDefinition definitionString
		collect: [ :char | 
			char = Character lf
				ifTrue: [ Character cr ]
				ifFalse: [ char ] ].
	aStream
		nextPut: $C;
		tab;
		nextPutAll: string;
		lf;
		yourself
]

{ #category : 'category' }
JadeServer >> _describeMCDefinition: anMCDefinition on: aStream [
	anMCDefinition isMethodDefinition
		ifTrue: [ 
			self _describeMCMethodDefinition: anMCDefinition on: aStream.
			^ self ].
	anMCDefinition isOrganizationDefinition
		ifTrue: [ 
			self _describeMCOrganizationDefinition: anMCDefinition on: aStream.
			^ self ].
	anMCDefinition isClassDefinition
		ifTrue: [ 
			self _describeMCClassDefinition: anMCDefinition on: aStream.
			^ self ].
	self halt
]

{ #category : 'category' }
JadeServer >> _describeMCMethodDefinition: anMCMethodDefinition on: aStream [
	| unicodeFreeSource |
	unicodeFreeSource := RowanMethodService
		removeUnicodeFromSource: anMCMethodDefinition source.
	aStream
		nextPut: $M;
		tab;
		nextPutAll: anMCMethodDefinition timeStamp;
		tab;
		nextPutAll: anMCMethodDefinition className;
		tab;
		nextPutAll: anMCMethodDefinition classIsMeta printString;
		tab;
		nextPutAll: anMCMethodDefinition category;
		tab;
		nextPutAll: anMCMethodDefinition selector;
		tab;
		nextPutAll: unicodeFreeSource size printString;
		tab;
		nextPutAll: unicodeFreeSource;
		lf
]

{ #category : 'category' }
JadeServer >> _describeMCModification: anMCModification on: aStream [
	aStream
		nextPut: $M;
		tab;
		nextPutAll: (self oopOf: anMCModification) printString;
		tab;
		yourself.
	self _describeMCDefinition: anMCModification obsoletion on: aStream.
	self _describeMCDefinition: anMCModification modification on: aStream
]

{ #category : 'category' }
JadeServer >> _describeMCOrganizationDefinition: anMCOrganizationDefinition on: aStream [
	aStream
		nextPut: $O;
		tab;
		yourself.
	anMCOrganizationDefinition categories
		do: [ :each | 
			aStream
				nextPutAll: each;
				tab ].
	aStream lf
]

{ #category : 'category' }
JadeServer >> _describeMCRemoval: anMCRemoval on: aStream [
	aStream
		nextPut: $R;
		tab;
		nextPutAll: (self oopOf: anMCRemoval) printString;
		tab;
		yourself.
	self _describeMCDefinition: anMCRemoval definition on: aStream
]

{ #category : 'category' }
JadeServer >> _mcDescriptionOfPatch: aPatch baseName: aString1 alternateName: aString2 [ 

	| stream |
	stream := WriteStream on: String new.
	(self oopOf: aPatch) printOn: stream.
	stream 
		tab; nextPutAll: (aString1 isNil ifTrue: ['loaded'] ifFalse: [aString1]);
		nextPutAll: ' vs. ';
		nextPutAll: (aString2 isNil ifTrue: ['loaded'] ifFalse: [aString2]);
		lf.
	aPatch operations do: [:each | 
		each isAddition 		ifTrue: [self _describeMCAddition: 		each on: stream].
		each isModification 	ifTrue: [self _describeMCModification: 	each on: stream].
		each isRemoval 		ifTrue: [self _describeMCRemoval: 		each on: stream].
	].
	^stream contents.
]

{ #category : 'jadeite' }
JadeServer >> autoCommitIfRequired [
	| commitResult |
	Rowan serviceClass autoCommit == true ifTrue:[
		commitResult := System commitTransaction.
		RowanAutoCommitService new autoCommit:  
			(commitResult 
				ifTrue:[true] 
				ifFalse:[#failed])].
]

{ #category : 'category' }
JadeServer >> commit [
	classOrganizers := Array new: 4.
	^ System commitTransaction
]

{ #category : 'category' }
JadeServer >> contents [ 
	"WriteStream method to identify things that have not yet been flushed to the output. We have flushed everything!"

	^''.
]

{ #category : 'category' }
JadeServer >> debugString: aString fromContext: anObject environment: anInteger [
	anInteger == 0
		ifFalse: [ self error: 'Only environment 0 is supported in this version!' ].
	^ (RowanDebuggerService new debugStringFrom: aString)
		evaluateInContext: anObject
		symbolList: GsSession currentSession symbolList
]

{ #category : 'category' }
JadeServer >> errorListFor: aCollection [ 

	| stream |
	aCollection class name == #'ErrorDescription' ifTrue: [^''].
	stream := WriteStream on: String new.
	aCollection do: [:each | 
		stream
			nextPutAll: (each at: 1) printString; tab;
			nextPutAll: (each at: 2) printString; tab;
			nextPutAll: ((2 < each size and: [(each at: 3) notNil]) ifTrue: [(each at: 3)] ifFalse: [(GemStoneError at: #English) at: (each at: 1)]); tab;
			lf.
	].
	^stream contents.
]

{ #category : 'category' }
JadeServer >> gemLogPath [ 

	^''
]

{ #category : 'jadeite' }
JadeServer >> gsInteractionInformFailureHandler [
  self interactionHandlerActive
    ifFalse: [ 
      ^ GsInteractionHandler new
        defaultBlock: [ :ignored | self assert: false description: 'expected a confirmation' ];
        confirmBlock: [ :interaction | interaction ok ];
        informBlock: [ :interaction |  ];
        inspectBlock: [ :interaction |  ];
        yourself ].
  ^ GsInteractionHandler new
    confirmBlock: [ :interaction | 
          | exception answer |
          exception := ClientForwarderSend new
            receiver: self
            clientObj: 1
            selector: #'confirmMessageBox:'
            args: (Array with: interaction prompt).
          answer := exception defaultAction.	"expect printString of answer back. Jadeite has limited ability to convert client objects to oops"
          answer evaluate ];
    informBlock: [ :interaction | 
          | exception |
          exception := ClientForwarderSend new
            receiver: self
            clientObj: 1
            selector: #'informMessageBox:'
            args: (Array with: interaction message).
          exception defaultAction.
          nil ];
    inspectBlock: [ :interaction | 
          | exception |
          exception := ClientForwarderSend new
            receiver: self
            clientObj: 1
            selector: #'inspectServerObject:'
            args: (Array with: interaction theObject asOop).
          exception defaultAction.
          interaction theObject ]
]

{ #category : 'jadeite' }
JadeServer >> interactionHandlerActive [
  ^ SessionTemps current at: #'rowanServiceInteractionActive' ifAbsent: [ true ]
]

{ #category : 'category' }
JadeServer >> mySessionInfo [ 

	| dict stream |
	stream := WriteStream on: String new.
	stream nextPutAll: self gemLogPath; cr.
	dict := System gemVersionReport.
	dict keys asSortedCollection do: [:each | 
		stream nextPutAll: each; tab; nextPutAll: (dict at: each) asString; cr.
	].
	stream nextPut: $%; cr.
	dict := System gemConfigurationReport.
	dict keys asSortedCollection do: [:each | 
		stream nextPutAll: each; tab; nextPutAll: (dict at: each) asString; cr.
	].
	stream nextPut: $%; cr.
	^stream contents
]

{ #category : 'category' }
JadeServer >> nextPut: aCharacter [ 

	self nextPutAll: aCharacter asString.
]

{ #category : 'category' }
JadeServer >> nextPutAll: anObject [ 

	| string args |
	string := self asString: anObject.
	args := Array
		with: self
		with: 1
		with: #'nextPutAll:'
		with: (Array with: string).
	System
		signal: 2336
		args: args
		signalDictionary: GemStoneError.
]

{ #category : 'category' }
JadeServer >> oopOf: anObject [
	^ anObject asOop
]

{ #category : 'category' }
JadeServer >> reset [ 
	"WriteStream protocol"
]

{ #category : 'category' }
JadeServer >> stackForProcess: aGsProcess [
	| array stream |
	Exception category: nil number: nil do: [ :ex :cat :num :args | nil ].
	array := aGsProcess _reportOfSize: 5000.
	stream := WriteStream on: String new.
	array
		do: [ :each | 
			stream
				nextPutAll: each;
				lf ].
	^ stream contents
]

{ #category : 'category' }
JadeServer >> step: aGsProcess inFrame: anInteger [
	aGsProcess _stepOverInFrame: anInteger
]

{ #category : 'category' }
JadeServer >> stoneInfo [ 

	| dict stream |
	stream := (WriteStream on: String new)
		nextPutAll: self streamType; tab;
		nextPutAll: self stringType; tab;
		cr;
		yourself.
	dict := System stoneVersionReport.
	dict keys asSortedCollection do: [:each | 
		stream nextPutAll: each; tab; nextPutAll: (dict at: each) asString; cr.
	].
	stream nextPut: $%; cr.
	dict := System stoneConfigurationReport.
	dict keys asSortedCollection do: [:each | 
		stream nextPutAll: each; tab; nextPutAll: (dict at: each) asString; cr.
	].
	stream nextPut: $%; cr.
	^stream contents
]

{ #category : 'jadeite' }
JadeServer >> updateFromSton: stonString [
	| services organizer resultString |
	[ 
	Rowan commandResultClass initializeResults.
	services := ((STON reader allowComplexMapKeys: true) on: stonString readStream)
		next.
	organizer := ClassOrganizer new.
	[ 
	services
		do: [ :service | 
			service organizer: organizer.
			service updateType: nil.	"Update type is only for returned commands"
			service command ifNil: [ service command: #'update' ].
			service servicePerform: service command withArguments: service commandArgs ] ]
		on: GsInteractionRequest
		do: [ :ex | 
			ex
				response:
					(ex interaction interactWith: self gsInteractionInformFailureHandler) ].
	self autoCommitIfRequired.
	Rowan loggingServiceClass current logSentServices.
	resultString := STON toString: Rowan commandResultClass results.
	^ resultString ]
		on: Error
		do: [ :ex | 
			RowanDebuggerService new saveProcessOop: GsProcess _current asOop.
			ex pass ]
]
