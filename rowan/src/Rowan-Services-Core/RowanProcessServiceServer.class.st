Class {
	#name : 'RowanProcessServiceServer',
	#superclass : 'RowanProcessService',
	#category : 'Rowan-Services-Core'
}

{ #category : 'instance creation' }
RowanProcessServiceServer class >> existingProcessServiceFor: suspendedProcess [
	" for use with rsr only"

	^self processServices at: suspendedProcess ifAbsent: [  ]
]

{ #category : 'instance creation' }
RowanProcessServiceServer class >> openDebuggerOn: suspendedProcess exception: ex connection: connection [
	" for use with rsr only"

	| processService |
	
	processService := self saveOnSuspendedProcess: suspendedProcess exception: ex connection: connection.
	^processService openDebugger
]

{ #category : 'instance creation' }
RowanProcessServiceServer class >> processServices [
	" for use with rsr only"

	^ SessionTemps current
		at: #'jadeiteProcessServices'
		ifAbsentPut: [ Dictionary new ]
]

{ #category : 'instance creation' }
RowanProcessServiceServer class >> removeProcessServiceFor: suspendedProcess [
	" for use with rsr only"

	| processService |
	processService := self processServices at: suspendedProcess ifAbsent: [  ].
	processService ifNotNil: [ processService cleanUpClient ].
	self processServices removeKey: suspendedProcess ifAbsent: [  ]
]

{ #category : 'instance creation' }
RowanProcessServiceServer class >> saveOnSuspendedProcess: suspendedProcess exception: ex connection: connection [
	" for use with rsr only"

	| processService |
	processService := RowanProcessServiceServer
		onSuspendedProcess: suspendedProcess.
	processService
		registerWith: connection;
		errorMessage: ex messageText;
		updates: Array new;
		postCommandExecution.
	self processServices at: suspendedProcess put: processService.
	^ processService
]

{ #category : 'debugger' }
RowanProcessServiceServer >> cleanUpClient [
	| promise |
	self postCommandExecution. 
	promise := remoteSelf cleanUpClient.
	^ promise wait
]

{ #category : 'debugger' }
RowanProcessServiceServer >> openDebugger [
	| promise |
	promise := remoteSelf openDebugger.
	^ promise wait
]

{ #category : 'debugger' }
RowanProcessServiceServer >> updateClient [
	| promise |
	self update. 
	self postCommandExecution. 
	promise := remoteSelf updateClient.
	^ promise wait
]
