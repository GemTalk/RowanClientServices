"
No class-specific documentation for RowanProcessService, hierarchy is:
Object
  RowanService( definition updates command commandArgs updateType organizer)
    RowanProcessService( frames oop status)

"
Class {
	#name : 'RowanProcessService',
	#superclass : 'RowanService',
	#instVars : [
		'frames',
		'oop',
		'status',
		'name',
		'errorMessage',
		'priority',
		'serverPrintString'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'instance creation' }
RowanProcessService class >> new [

	"in general use one of the other constructor methods.
	Needed for STON replication"
	^super new
]

{ #category : 'instance creation' }
RowanProcessService class >> onActiveProcess: aGsProcess [

	^self basicNew
		initialize;
		initialize: aGsProcess status: 'active';
		name: aGsProcess name;
		yourself
]

{ #category : 'instance creation' }
RowanProcessService class >> onReadyProcess: aGsProcess [
	^ self basicNew
		initialize: aGsProcess status: 'ready';
		name: aGsProcess name;
		yourself
]

{ #category : 'instance creation' }
RowanProcessService class >> onSuspendedProcess: aGsProcess [
	^ self basicNew
		initialize: aGsProcess status: 'suspended';
		name: aGsProcess name;
		yourself
]

{ #category : 'instance creation' }
RowanProcessService class >> onWaitingProcess: aGsProcess [
	^ self basicNew
		initialize: aGsProcess status: 'waiting';
		name: aGsProcess name;
		yourself
]

{ #category : 'rsr' }
RowanProcessService class >> templateClassName [

	^#RowanProcessService
]

{ #category : 'initialize' }
RowanProcessService >> clearOrganizers [
	self organizer: nil. 
	updates := nil.
	frames
		ifNotNil: [ 
			frames
				do: [ :frameService | 
					frameService organizer: nil.  
					frameService method organizer: nil.
					frameService vars
						ifNotNil: [ :vars | vars do: [ :variableService | variableService organizer: nil ] ] ] ]
]

{ #category : 'actions' }
RowanProcessService >> debug [
	| suspendedProcess debuggerResult |
	suspendedProcess := Object _objectForOop: oop.
	RowanDebuggerService new saveProcessOop: oop.
	debuggerResult := (RowanProcessServiceServer
		existingProcessServiceFor: suspendedProcess)
		ifNil: [ self openDebugger ]
		ifNotNil: [ self updateClient ].
	debuggerResult = #'terminate'
		ifTrue: [ 
			RowanProcessServiceServer removeProcessServiceFor: suspendedProcess.
			^ suspendedProcess terminate ]
		ifFalse: [ self resume: suspendedProcess orStep: debuggerResult ]
]

{ #category : 'accessing' }
RowanProcessService >> errorMessage [
	^errorMessage
]

{ #category : 'accessing' }
RowanProcessService >> errorMessage: object [
	errorMessage := object
]

{ #category : 'initialize' }
RowanProcessService >> frames [

	^frames
]

{ #category : 'accessing' }
RowanProcessService >> gsProcess [
	^ Object _objectForOop: oop
]

{ #category : 'initialize' }
RowanProcessService >> initialize [

	frames := Array new
]

{ #category : 'initialize' }
RowanProcessService >> initialize: aGsProcess status: aString [
	| theOrganizer |
	theOrganizer := ClassOrganizer new.
	frames := Array new: aGsProcess stackDepth.
	1 to: aGsProcess stackDepth do: [ :i | 
		frames
			at: i
			put:
				([ RowanFrameServiceServer process: aGsProcess level: i organizer: theOrganizer ]
					on: Error , Halt
					do: [ :ex | 
						GsFile gciLogServer: 'error building Jadeite debugger - ' , ex printString.
						RowanFrameServiceServer basicNew
							label: 'unprintable frame - error: ' , ex printString;
							method:
									(RowanMethodServiceServer new
											stepPoints: Array new;
											breakPoints: Array new) ]) ].
	oop := aGsProcess asOop.
	priority := aGsProcess priority. 
	[serverPrintString := aGsProcess printString] on: Error do: [:ex | 'process printString got an error - ', ex asString]. 
	status := aString
]

{ #category : 'accessing' }
RowanProcessService >> name [
	^name
]

{ #category : 'accessing' }
RowanProcessService >> name: object [

	name := object
]

{ #category : 'accessing' }
RowanProcessService >> oop [
	^oop
]

{ #category : 'accessing' }
RowanProcessService >> oop: object [
	oop := object
]

{ #category : 'actions' }
RowanProcessService >> priority: anInteger [

	self gsProcess priority: anInteger
]

{ #category : 'actions' }
RowanProcessService >> resume: suspendedProcess orStep: debuggerResult [
	debuggerResult = #'resume'
		ifTrue: [ 
			"open a new debugger if necessary"
			RowanProcessServiceServer removeProcessServiceFor: suspendedProcess.
			^ suspendedProcess resume ].
	suspendedProcess
		perform: debuggerResult first
		withArguments: (debuggerResult copyFrom: 2 to: debuggerResult size).
	debuggerResult first = #'trimStackToLevel:'
		ifTrue: [ 
			| result processService |
			processService := RowanProcessServiceServer
				existingProcessServiceFor: suspendedProcess.
			result := processService updateClient.
			^ self resume: suspendedProcess orStep: result ].
	(Delay forMilliseconds: 100) wait.
	suspendedProcess resume
]

{ #category : 'accessing' }
RowanProcessService >> status [
	^status
]

{ #category : 'accessing' }
RowanProcessService >> status: object [
	status := object
]

{ #category : 'actions' }
RowanProcessService >> terminate [
	[ (Object _objectForOop: oop) terminate ] fork
]

{ #category : 'updating' }
RowanProcessService >> update [
	super update. 
	self initialize: (Object _objectForOop: oop) status: 'suspended'.
	RowanCommandResult addResult: self.
]
