"
Rowan Service is the abstract service class for classes that represent
first class entities in Rowan. They are transported to the client via 
ston. 

On the client, set the command & commandArgs inst vars, then tell
the browser to #issueCommand: with an array of services. A service
received without the the command inst var set will send #update to
the service. #issueCommand: should call JadeServer>>updateFromSton:
which will run the command and return a result. 

Any service that sends updates back to the client willl propogates to 
registered windows. Add a service for return to the client with:
RowanCommandResult addResult: <self>
"
Class {
	#name : 'RowanService',
	#superclass : 'RsrService',
	#instVars : [
		'command',
		'commandArgs',
		'updateType',
		'organizer',
		'updates',
		'wasUpdated',
		'shouldUpdate'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'autocommit' }
RowanService class >> autoCommit [

	^SessionTemps current at: #'Jadeite_AutoCommit' ifAbsentPut: [false]
]

{ #category : 'autocommit' }
RowanService class >> breakPointsAreEnabled [

	^SessionTemps current at: #'Jadeite_BreakPointsAreEnabled' ifAbsentPut: [true]
]

{ #category : 'autocommit' }
RowanService class >> flipAutoCommit [
	| newValue |
	newValue := self autoCommit == #failed ifTrue:[false] ifFalse:[self autoCommit not].
	^self setAutoCommit: newValue
]

{ #category : 'testing' }
RowanService class >> isRowanClientServicesVersionSupported: lowerLimit [
	^ self
		isRowanClientServicesVersionSupported: RowanService version
		lowerLimit: lowerLimit
]

{ #category : 'testing' }
RowanService class >> isRowanClientServicesVersionSupported: versionString lowerLimit: lowerLimit [
	"return a two element arg showing if the version is supported and the version"

	| low high version |
	low := RwSemanticVersionNumber fromString: lowerLimit.
	high := low copy incrementMinorVersion.
	version := RwSemanticVersionNumber fromString: versionString.
	^ low <= version and: [ version < high ]
]

{ #category : 'instance creation' }
RowanService class >> new [

	^super new initialize

]

{ #category : 'other' }
RowanService class >> rowanFixMe [

	"send this message to see everywhere that GS_Jade should be fixed"

]

{ #category : 'examples' }
RowanService class >> sampleService [

	^self new sampleService

]

{ #category : 'autocommit' }
RowanService class >> setAutoCommit: object [

	^SessionTemps current at: #'Jadeite_AutoCommit' put: object
]

{ #category : 'autocommit' }
RowanService class >> setBreakPointsAreEnabled: boolean [
  ^ SessionTemps current at: #'Jadeite_BreakPointsAreEnabled' put: boolean
]

{ #category : 'rsr' }
RowanService class >> templateClassName [

	^#RowanService
]

{ #category : 'accessing' }
RowanService class >> version [
	^ '3.0.8'
]

{ #category : 'accessing' }
RowanService class >> versionRangeHigh: lowRange [
	| low high |
	low := RwSemanticVersionNumber fromString: lowRange.
	high := low incrementMinorVersion.
	^high printString
]

{ #category : 'commands support' }
RowanService >> addAllSubclassHierarchiesOf: theClass to: hierarchies [
	(organizer subclassesOf: theClass)
		do: [ :subclass | 
			hierarchies
				add:
					(organizer allSuperclassesOf: theClass) , (Array with: theClass with: subclass).
			self addAllSubclassHierarchiesOf: subclass to: hierarchies ]
]

{ #category : 'other' }
RowanService >> answer: anObject [

	| answeringService |
	answeringService := RowanAnsweringService new. 
	answeringService answer: anObject. 
	RowanCommandResult addResult: answeringService.
]

{ #category : 'commands support' }
RowanService >> autoCommitIfRequired [
	| commitResult |
	self class autoCommit == true
		ifTrue: [ 
			commitResult := [ System commitTransaction ]
				on: Error
				do: [ :e | 
					RowanAutoCommitService new autoCommit: #'failed'.
					^ self ].
			RowanAutoCommitService new
				autoCommit:
					(commitResult
						ifTrue: [ true ]
						ifFalse: [ #'failed' ]) ]
]

{ #category : 'rowan' }
RowanService >> browserTool [

	^self projectTools browser

]

{ #category : 'rsr' }
RowanService >> checkForDeadProcesses [
	"Rowan Client Services holds onto processes while Jadeite is debugging them. 
	Sometimes Jadeite won't know when a process is terminated so we check on
	every round trip for extinguished processes"

	RowanProcessServiceServer processServices copy
		keysAndValuesDo: [ :process :processService | 
			process _isTerminated
				ifTrue: [ RowanProcessServiceServer removeProcessServiceFor: process ] ]
]

{ #category : 'commands support' }
RowanService >> classHierarchy: theClasses [
	| superclassChains levels services hierarchies toExpand hierarchyServices |
	superclassChains := self superclassChainsFor: theClasses.
	hierarchies := self extendHierarchies: superclassChains forClasses: theClasses.
	levels := self hierarchiesByLevel: hierarchies.
	services := Dictionary new.
	toExpand := Set new.
	self
		services: services
		from: levels
		expand: toExpand
		classes: theClasses.
	hierarchyServices := services reject: [ :array | array isEmpty ].
	hierarchyServices copy
		keysAndValuesDo: [ :key :value | 
			hierarchyServices
				at: key
				put: (value asSet asSortedCollection: [ :x :y | x name < y name ]) asArray ].
	^ hierarchyServices
]

{ #category : 'rsr' }
RowanService >> clearOrganizers [
	organizer := nil.
	updates do: [ :update | update organizer: nil ]
]

{ #category : 'accessing' }
RowanService >> command [

	^command

]

{ #category : 'accessing' }
RowanService >> command: aSymbol [

	command := aSymbol

]

{ #category : 'accessing' }
RowanService >> commandArgs [

	^commandArgs ifNil:[commandArgs := Array new]

]

{ #category : 'accessing' }
RowanService >> commandArgs: anArray [

	"for tests" 

	commandArgs := anArray
]

{ #category : 'commands support' }
RowanService >> computePackageNameFor: theClass in: packageNames [
	"Similar to Behavior>>rowanPackageNames but pass in a cached list of packageNames for performance"

	| loadedClass packageName |
	loadedClass := Rowan image
		loadedClassForClass: theClass thisClass
		ifAbsent: [ ^ Rowan unpackagedName ].
	packageName := loadedClass loadedPackage name.
	^ (packageNames includes: packageName)
		ifTrue: [ packageName ]
		ifFalse: [ Rowan unpackagedName ]
]

{ #category : 'symbol dictionaries' }
RowanService >> createDefaultSymbolDictionary [

	^self createSymbolDictionaryNamed: self defaultSymbolDictionaryName

]

{ #category : 'samples' }
RowanService >> createSampleSymbolDictionary [

	self removeSymbolDictionaryNamed: self sampleSymbolDictionaryName.
	self createSymbolDictionaryNamed: self sampleSymbolDictionaryName

]

{ #category : 'commands support' }
RowanService >> createServiceFor: aClass unlessExistingIn: newServices expand: toExpand packageNames: packageNames [
	| service |
	service := newServices
		detect: [ :classService | classService name asString = aClass name asString ]
		ifNone: [ 
			RowanClassService new
				classServiceFromOop: aClass asOop
				packageNames: packageNames ].
	(toExpand includes: service theClass)
		ifTrue: [ service expand: true ]
		ifFalse: [ service expand: false ].

	(organizer subclassesOf: aClass) size > 0
		ifTrue: [ service hasSubclasses: true ].

	^ service
]

{ #category : 'symbol dictionaries' }
RowanService >> createSymbolDictionaryNamed: aName [

	| dictionary size |
	dictionary := SymbolDictionary new.
	dictionary at: aName asSymbol put: dictionary.
	size := Rowan image symbolList size.
	System myUserProfile insertDictionary: dictionary at: size + 1.
	^ dictionary
]

{ #category : 'symbol dictionaries' }
RowanService >> defaultSymbolDictionary [
	"used?"

	^self symbolDictionaryNamed: self defaultSymbolDictionaryName
]

{ #category : 'symbol dictionaries' }
RowanService >> defaultSymbolDictionaryName [

	^'RowanProjects'

]

{ #category : 'rowan' }
RowanService >> definitionClass [

	^self subclassResponsibility

]

{ #category : 'rowan' }
RowanService >> definitionClassName [

	^self definitionClass name

]

{ #category : 'replication' }
RowanService >> excludedInstVars [

	^#( #organizer)

]

{ #category : 'rsr' }
RowanService >> executeCommand [
	"RSR -> RowanServices primary api."
	self checkForDeadProcesses.
	self setDebugActionBlock.
	[ 
	Rowan commandResultClass initializeResults.
	organizer := ClassOrganizer new.
	[ 
	updateType := nil.	"Update type is only for returned commands"
	self servicePerform: command withArguments: commandArgs ]
		on: GsInteractionRequest
		do: [ :ex | 
			ex
				response:
					(ex interaction interactWith: self gsInteractionInformFailureHandler) ].
	updates := Rowan commandResultClass results.

	self postCommandExecution ]
		on: Exception
		do: [ :ex | 
			RowanDebuggerService new saveProcessOop: GsProcess _current asOop.
			ex pass ].
	^ self
]

{ #category : 'commands support' }
RowanService >> extendHierarchies: hierarchies forClasses: theClasses [
	"extend the hierarchies by one level
	of subclasses if it is a selected class"

	hierarchies
		do: [ :hierarchy | 
			| theClass |
			theClass := hierarchy last.
			(theClasses includes: theClass)
				ifTrue: [ self addAllSubclassHierarchiesOf: theClass to: hierarchies ] ].
	^ hierarchies
]

{ #category : 'commands support' }
RowanService >> fileOut: ws on: path [
	| file | 
	file := path asFileReference.
	file exists
		ifTrue: [ 
			(self confirm: 'File exists. File out anyway?')
				ifTrue: [ file delete ]
				ifFalse: [ ^ self ] ].
	file := file writeStreamDo: [ :stream | stream nextPutAll: ws contents ]
]

{ #category : 'rsr' }
RowanService >> gsInteractionInformFailureHandler [
  self interactionHandlerActive
   ifFalse: [ 
      ^ GsInteractionHandler new
        defaultBlock: [ :ignored | Error signal: 'expected a confirmation' ];
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

{ #category : 'perform' }
RowanService >> handleDeletedService [
  self updateType: #'removed:'.
  RowanCommandResult addResult: self
]

{ #category : 'commands support' }
RowanService >> hierarchiesByLevel: hierarchies [

	"Return dictionary of classes by level. 
	Example: 
		hierarchies - #(#(Object Collection Array) #(Object AbstractException Exception))
	Return: 
		#(#nil->#(Object) Object->#(Collection AbstractException) Collection->#(Array) AbstractException->#(Exception)
	"
	| levels |
	levels := hierarchies inject: Dictionary new into:[:dict :chain | 
		1 to: chain size do: [:index | 
			| cls theSuper classSet |
			cls := chain at: index.
			classSet := dict at: cls ifAbsentPut: [Array new].
			index = 1 
		ifTrue:[
			classSet := dict at: #'nil' ifAbsentPut: [Array new]. 
			((dict at: #'nil') includes: cls) ifFalse:[(dict at: #'nil') add: cls].
		]
		ifFalse:[
				theSuper := chain at: index - 1.
				((dict at: theSuper) includes: cls) ifFalse:[(dict at: theSuper) add: cls]
				]].
			dict].
	^levels
]

{ #category : 'initialization' }
RowanService >> initialize [
	updates := Array new.
	wasUpdated := false
]

{ #category : 'rsr' }
RowanService >> interactionHandlerActive [
  ^ SessionTemps current at: #'rowanServiceInteractionActive' ifAbsent: [ true ]
]

{ #category : 'testing' }
RowanService >> isClassService [

	^false
]

{ #category : 'testing' }
RowanService >> isDefinedProject [

	^false
]

{ #category : 'testing' }
RowanService >> isDictionaryService [

	^false
]

{ #category : 'testing' }
RowanService >> isMethodService [

	^false
]

{ #category : 'testing' }
RowanService >> isPackageService [

	^false
]

{ #category : 'testing' }
RowanService >> isProjectService [

	^false
]

{ #category : 'testing' }
RowanService >> isUpdating [

	^command == #update
]

{ #category : 'perform' }
RowanService >> isUpdatingButFoundToBeDeleted [
  ^ self command == #'update' and: [ self wasDeleted ]
]

{ #category : 'accessing' }
RowanService >> jadeiteServer [

	^(Rowan jadeServerClassNamed: #JadeServer) theJadeiteServer
]

{ #category : 'printing' }
RowanService >> logOn: aStream [
	| instVarNames |
	super printOn: aStream.
	aStream lf.
	instVarNames := self class allInstVarNames.
	1 to: instVarNames size do: [ :index | 
		| instVarValue |
		instVarValue := self instVarAt: index.
		instVarValue
			ifNotNil: [ 
				aStream
					nextPutAll: (instVarNames at: index);
					nextPut: $=;
					nextPutAll: instVarValue printString;
					tab ] ].
	aStream
		lf
]

{ #category : 'printing' }
RowanService >> logString [
	| ws |
	ws := WriteStream on: String new.
	self logOn: ws.
	^ ws contents
]

{ #category : 'rsr' }
RowanService >> massage: theValue inNonIndexableCollection: collection visitLog: visitLog [
	theValue isSpecial
		ifTrue: [ ^ self ].
	theValue class = LargeInteger 
		ifTrue:[^self]. 
	(theValue isKindOf: ClassOrganizer)
		ifTrue: [ ^ collection remove: theValue ].
	(theValue isKindOf: Association)
		ifTrue: [ 
			| replacement |
			replacement := Array with: theValue key with: theValue value.
			collection remove: theValue.
			collection add: replacement ].
	(theValue isKindOf: RowanService)
		ifTrue: [ ^ theValue massageServiceForRsrTransportWithVisitLog: visitLog ].
	theValue class isVariable
		ifTrue: [ ^ self massageCollection: theValue visitLog: visitLog ]
]

{ #category : 'rsr' }
RowanService >> massageCollection: collection visitLog: visitLog [
	(visitLog includes: collection asOop)
		ifTrue: [ ^ self ].
	visitLog add: collection asOop.
	collection class isIndexable
		ifTrue: [ self massageIndexableCollection: collection visitLog: visitLog ]
		ifFalse: [ self massageNonIndexableCollection: collection visitLog: visitLog ]
]

{ #category : 'rsr' }
RowanService >> massageIndexableCollection: collection visitLog: visitLog [
	1 to: collection size do: [ :index | self massageObject: collection atIndex: index visitLog: visitLog ]
]

{ #category : 'rsr' }
RowanService >> massageNonIndexableCollection: collection visitLog: visitLog [
	collection
		ifNotNil: [  
			collection
				do: [ :theValue | self massage: theValue inNonIndexableCollection: collection visitLog: visitLog ] ]
]

{ #category : 'rsr' }
RowanService >> massageObject: object atIndex: index visitLog: visitLog [
	| theValue |
	(object isKindOf: Dictionary) ifTrue:[^self "for now"].
	theValue := object at: index.
	theValue isSpecial
		ifTrue: [ ^ self ].
	(theValue isKindOf: ClassOrganizer)
		ifTrue: [ ^ object at: index put: nil ].
	(theValue isKindOf: Association)
		ifTrue: [ ^ object at: index put: (Array with: theValue key with: theValue value) ].
	(theValue isKindOf: RowanService)
		ifTrue: [ ^ theValue massageServiceForRsrTransportWithVisitLog: visitLog ].
	theValue class isVariable
		ifTrue: [ ^self massageCollection: theValue visitLog: visitLog ]
]

{ #category : 'rsr' }
RowanService >> massageObjectAtInstVarIndex: index visitLog: visitLog [
	| instVarValue |
	instVarValue := self instVarAt: index.
	instVarValue isSpecial
		ifTrue: [ ^ self ].
	instVarValue class = LargeInteger 
		ifTrue:[^self]. 
	(instVarValue isKindOf: ClassOrganizer)
		ifTrue: [ ^ self instVarAt: index put: nil ].
	(instVarValue isKindOf: Association)
		ifTrue: [ 
			^ self
				instVarAt: index
				put: (Array with: instVarValue key with: instVarValue value) ].
	(instVarValue isKindOf: RowanService)
		ifTrue: [ ^ instVarValue massageServiceForRsrTransportWithVisitLog: visitLog ].
	instVarValue class isVariable
		ifTrue: [ self massageCollection: instVarValue visitLog: visitLog ]
]

{ #category : 'rsr' }
RowanService >> massageServiceForRsrTransportWithVisitLog: visitLog [
	"rsr won't handle associations"

	self isSpecial
		ifTrue: [ ^ self ].
	(visitLog includes: self asOop)
		ifTrue: [ ^ self ].
	visitLog add: self asOop.
	1 to: self class allInstVarNames size do: [ :index |
		self
			massageObjectAtInstVarIndex: index
			visitLog: visitLog ].
]

{ #category : 'rsr' }
RowanService >> nilRsrVariables [

	_id := nil. 
	_connection := nil. 
	remoteSelf := nil
]

{ #category : 'accessing' }
RowanService >> organizer: anOrganizer [

	organizer := anOrganizer.
]

{ #category : 'commands support' }
RowanService >> peerHierarchies: theClasses [
	" create hierarchies for each peer of the 
	classes of interest."

	| peerHierarchies |
	peerHierarchies := Array new.
	theClasses
		do: [ :theClass | 
			| superclass |
			superclass := theClass superclass.
			superclass
				ifNil: [ peerHierarchies add: (Array with: theClass) ]
				ifNotNil: [ 
					superclass subclasses
						do: [ :subclass | 
							peerHierarchies
								add:
									((organizer allSuperclassesOf: subclass)
										add: subclass;
										yourself) ] ] ].
	^ peerHierarchies
]

{ #category : 'other' }
RowanService >> postCommandExecution [
	| visitLog |
	self clearOrganizers.
	self removeDuplicateServices.
	visitLog := Set new.
	self massageServiceForRsrTransportWithVisitLog: visitLog.
	self autoCommitIfRequired.
]

{ #category : 'rowan' }
RowanService >> projectTools [

	^Rowan projectTools

]

{ #category : 'release' }
RowanService >> releaseProcessOop: oop [
  "not really releasing it. The client should have registered
	the process with the debugger window it opened before
	this is run"

  | jadeiteProcesses process |
  ((process := Object _objectForOop: oop) isKindOf: GsProcess)
    ifTrue: [ 
      jadeiteProcesses := SessionTemps current
        at: #'jadeiteProcesses'
        ifAbsentPut: [ Array new ].
      jadeiteProcesses remove: process ifAbsent: [  ] ]
]

{ #category : 'rsr' }
RowanService >> removeDuplicateServices [
	updates copy ifNotNil: [:copy |
		copy do: [ :update | 
			update = self
				ifTrue: [ 
					self updateInternalService: update.
					updates remove: update ] ]]
]

{ #category : 'samples' }
RowanService >> removeSampleSymbolDictionary [

	self removeSymbolDictionaryNamed: self sampleSymbolDictionaryName.

]

{ #category : 'symbol dictionaries' }
RowanService >> removeSymbolDictionaryNamed: aName [

	| index |
	index := Rowan image symbolList names indexOf: aName asSymbol.
	index ~= 0 ifTrue:[
		System myUserProfile removeDictionaryAt: index]
]

{ #category : 'rsr' }
RowanService >> resume: suspendedProcess orStep: debuggerResult [
	debuggerResult = #'resume'
		ifTrue: [ 
			"open a new debugger if necessary"
			RowanProcessServiceServer removeProcessServiceFor: suspendedProcess.
			^ suspendedProcess resume ].
	suspendedProcess
		perform: debuggerResult first
		withArguments: (debuggerResult copyFrom: 2 to: debuggerResult size).
	debuggerResult first = #trimStackToLevel: ifTrue:[
		^(RowanProcessServiceServer existingProcessServiceFor: suspendedProcess) updateClient.
			].
	(Delay forMilliseconds: 100) wait.
	suspendedProcess resume
]

{ #category : 'other' }
RowanService >> rowanFixMe [
		
	"marker for all things broken in Rowan"

]

{ #category : 'accessing' }
RowanService >> rowanProjectName [

	"all services should be able to return a project name
	even if they are not truly packaged" 

	^nil
]

{ #category : 'samples' }
RowanService >> sampleSymbolDictionaryName [

	^'SampleSymbolDictionaryName'

]

{ #category : 'perform' }
RowanService >> servicePerform: symbol withArguments: collection [
	"subclasses may not want to update after performing the command"

	self servicePerform: symbol withArguments: collection shouldUpdate: true
]

{ #category : 'perform' }
RowanService >> servicePerform: commandSymbol withArguments: collection shouldUpdate: possiblyUpdate [
	"each service updates itself after performing a command.
	Therefore, if the command is #update, don't run it here"

	shouldUpdate := possiblyUpdate.	"let the command decide if an update is actually needed"
	super perform: commandSymbol withArguments: collection.
	(shouldUpdate and: [organizer notNil])
		ifTrue: [ "a nil organizer implies that this process is in the process of terminating" self update ]
]

{ #category : 'commands support' }
RowanService >> services: services from: levels expand: toExpand classes: theClasses [
	"In order to avoid the expense of creating duplicate services, we cache
them in the newServices temporary for look up"

	| newServices allPackageNames |
	allPackageNames := Rowan image packageNames.
	newServices := Array new.
	theClasses
		do: [ :aClass | toExpand addAll: (organizer allSuperclassesOf: aClass) ].
	levels
		keysAndValuesDo: [ :key :value | 
			| newKey |
			newKey := key = #'nil'
				ifTrue: [ #'nil' ]
				ifFalse: [ 
					self
						createServiceFor: key
						unlessExistingIn: newServices
						expand: toExpand
						packageNames: allPackageNames ].
			services
				at: newKey
				put:
					(value
						collect: [ :cls | 
							self
								createServiceFor: cls
								unlessExistingIn: newServices
								expand: toExpand
								packageNames: allPackageNames ]) ]
]

{ #category : 'rsr' }
RowanService >> setDebugActionBlock [
	Processor activeProcess
		breakpointLevel: 1;
		debugActionBlock: [ :ex | 
					| debuggerResult suspendedProcess |
					_connection isOpen
						ifFalse: [ 
							GsFile gciLogServer: ex printString.
							GsFile gciLogServer: 'connection not open'.	"can't return"
							Processor activeProcess terminate ].
					suspendedProcess := Processor activeProcess.
					ex class = CompileError
						ifTrue: [ 
							| compileErrorService |
							self postCommandExecution.
							compileErrorService := RowanCompileErrorServiceServer new.
							compileErrorService gsArguments: ex errorDetails.
							updates := Array with: compileErrorService.
							^ ex return ].
					[ 
					RowanDebuggerService new saveProcessOop: suspendedProcess asOop.
					debuggerResult := (RowanProcessServiceServer
						existingProcessServiceFor: suspendedProcess)
						ifNil: [ 
							RowanProcessServiceServer
								openDebuggerOn: suspendedProcess
								exception: ex
								connection: _connection ]
						ifNotNil: [ :processService |   
							GsFile gciLogServer: 'updateClient'.
							processService updateClient ].
					ex isResumable
						ifFalse: [ debuggerResult := #'terminate' ].
					debuggerResult = #'terminate'
						ifTrue: [ 
							RowanProcessServiceServer removeProcessServiceFor: suspendedProcess.
							suspendedProcess resume	"let the exception handler block return nil" ]
						ifFalse: [ self resume: suspendedProcess orStep: debuggerResult ] ] fork.
					suspendedProcess ifNotNil: [:proc | proc suspend].
					debuggerResult = #'terminate'
						ifTrue: [ 
							self postCommandExecution.
							ex tag: #rsrProcessTerminated. 
							RsrUnhandledException signal: ex  "stop processing the exception but let rsr return" ].
					ex resume ]
]

{ #category : 'replication' }
RowanService >> stonOn: stonWriter [
    | instanceVariableNames |
    instanceVariableNames := self class allInstVarNames reject: [:iv | self excludedInstVars includes: iv].
    stonWriter writeObject: self
        streamMap: 
            [:dictionary |
            instanceVariableNames do: 
                    [:each |
                    (self instVarAt: (self class allInstVarNames indexOf: each asSymbol))
                        ifNotNil: [:value | dictionary at: each asSymbol put: value]
                        ifNil: [self stonShouldWriteNilInstVars ifTrue: [dictionary at: each asSymbol put: nil]]]]
]

{ #category : 'replication' }
RowanService >> stonStringFor: anObject [

	"return a string representing a complete object structure
	suitable for replicating on the client."

	^STON toString: anObject

]

{ #category : 'private' }
RowanService >> stripOutUnicode: string [
  | asciiString |
  asciiString := string
    collect: [ :char | 
      ((self validLowRangeCharacters includes: char) not
        and: [ char asciiValue < 32 or: [ char asciiValue > 255 ] ])
        ifTrue: [ $? ]
        ifFalse: [ char ] ].
  ^ asciiString
]

{ #category : 'commands support' }
RowanService >> superclassChainsFor: behaviors [
	organizer := ClassOrganizer new. 
	^behaviors collect:[:behavior | | supers |
			supers := organizer allSuperclassesOf: behavior. 
			supers add: behavior. 
			supers].

]

{ #category : 'symbol dictionaries' }
RowanService >> symbolDictionaryNamed: aName [

	| symbolList  index |
	symbolList := Rowan image symbolList.
	index :=symbolList names indexOf: aName asSymbol.
	^index ~= 0
		ifTrue:[
			symbolList at: index]
		ifFalse:[
			self createSymbolDictionaryNamed: aName].

]

{ #category : 'rsr' }
RowanService >> unregisteredCopy [
	"copy of service without any reference to the rsr connection or organizer"

	| copy |
	copy := self copy. 
	copy nilRsrVariables.
	copy organizer: nil. 
	^copy
]

{ #category : 'update' }
RowanService >> update [
	wasUpdated := true
]

{ #category : 'update' }
RowanService >> updateInternalService: updatedService [

	"no internally held services to update"
]

{ #category : 'update' }
RowanService >> updateLatest [
  "subclasses may want to special behavior to update themselves
	to their loaded version"

  self update
]

{ #category : 'other' }
RowanService >> updates: aCollection [

	updates := aCollection
]

{ #category : 'accessing' }
RowanService >> updateType: aSymbol [

	updateType := aSymbol
]

{ #category : 'accessing' }
RowanService >> userGlobals [
	^ Rowan image symbolList objectNamed: #'UserGlobals'
]

{ #category : 'private' }
RowanService >> validLowRangeCharacters [
  ^ Array with: Character lf with: Character tab
]

{ #category : 'testing' }
RowanService >> wasDeleted [

	^false
]

{ #category : 'fileout' }
RowanService >> writeFileOutHeaderOn: stream [
	"This method will write a fileout header onto the given file.
	Adapted from GBS - GbxBrowser>>writeFileOutHeaderOn:"

	| rawVer beVer lf |
	stream
		nextPutAll: 'fileformat utf8';
		lf.
	rawVer := System _version.
	beVer := ''.
	lf := String with: Character lf.	"Comment each newline"
	(rawVer subStrings: (Array with: Character lf))
		do: [ :line | beVer := beVer , '! ' , line , lf ].
	stream
		nextPutAll: '!';
		lf;
		nextPutAll: '! From ';
		nextPutAll: beVer;
		lf;
		nextPutAll: '! On ';
		nextPutAll: Date today printString;
		nextPutAll: ', ';
		nextPutAll: Time now printString;
		lf;
		nextPutAll: '!';
		lf;
		flush
]
