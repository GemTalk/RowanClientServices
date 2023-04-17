Class {
	#name : 'RowanAnsweringServiceServer',
	#superclass : 'RowanAnsweringService',
	#instVars : [
		'process'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'client commands' }
RowanAnsweringServiceServer >> basicExecReturningPrintString: aString context: oop shouldDebug: shouldDebug [
	| object symbolList tempMethod result executionComplete |
	object := Object _objectForOop: oop.
	symbolList := Rowan image symbolList.
	[ tempMethod := aString _compileInContext: object symbolList: symbolList ]
		on: CompileError
		do: [ :ex | 
			answer := Array with: false with: ex errorDetails first last.
			^ answer ].
	shouldDebug
		ifTrue: [ 
			tempMethod setBreakAtStepPoint: 1.
			RowanDebuggerService new saveProcessOop: GsProcess _current asOop ].
	executionComplete := Semaphore new.
	[ 
	process := [ 
	[ 
	[ 
	result := tempMethod
		_executeInContext: object
		ensure: [ executionComplete signal ] ]
		on: RowanClientForwarderSend
		do: [ :ex | 
			command := #'transcriptWrite'.
			answer := ex args first.
			executionComplete signal.
			Processor activeProcess suspend.
			ex resume  ] ] ] fork.
	executionComplete wait.
	answer := Array with: true with: result asOop with: result printString ]
		ensure: [ 
			shouldDebug
				ifTrue: [ tempMethod clearBreakAtStepPoint: 1 ] ].
	RowanService autoCommit == true
		ifTrue: [ System commitTransaction ].
	^ answer
]
