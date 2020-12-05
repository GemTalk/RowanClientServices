| package |
package := Package name: 'Rowan Client Services'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #BrowserUpdate;
	add: #JadeiteTreeModel;
	add: #JadeiteTreeNode;
	add: #RowanAnsweringService;
	add: #RowanAutoCommitService;
	add: #RowanBrowserService;
	add: #RowanClassService;
	add: #RowanComponentService;
	add: #RowanDebuggerService;
	add: #RowanDefinedProjectService;
	add: #RowanDictionaryService;
	add: #RowanFrameService;
	add: #RowanInspectorService;
	add: #RowanLoadedProjectService;
	add: #RowanLoggingService;
	add: #RowanMethodService;
	add: #RowanPackageService;
	add: #RowanProcessService;
	add: #RowanProjectService;
	add: #RowanQueryService;
	add: #RowanResolvedProjectService;
	add: #RowanSemaphore;
	add: #RowanService;
	add: #RowanSpecificationService;
	add: #RowanTestService;
	add: #RowanVariableService;
	yourself.

package methodNames
	add: #Object -> #isService;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\Jadeite\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\Jadeite\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Choice Prompter'
	'..\Jadeite\Core\Object Arts\Dolphin\Base\Dolphin Legacy Date & Time'
	'..\Jadeite\Core\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models'
	'..\Jadeite\Core\Object Arts\Dolphin\Base\Dolphin Message Box'
	'..\Jadeite\Core\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base'
	'..\Jadeite\Core\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Rich Text Presenter'
	'..\Jadeite\Core\Object Arts\Dolphin\MVP\Models\Tree\Dolphin Tree Models'
	'..\Jadeite\Core\Object Arts\Dolphin\MVP\Presenters\Tree\Dolphin Tree Presenter'
	'..\Jadeite\sources\GemStone C Interface'
	'..\Jadeite\sources\GemStone Session'
	'..\Jadeite\sources\Jade UI Base'
	'..\..\..\RemoteServiceReplication\src-dolphin\RemoteServiceReplication'
	'..\Jadeite\Core\Contributions\svenc\STON\STON-Core').

package!

"Class Definitions"!

Semaphore subclass: #RowanSemaphore
	instanceVariableNames: 'lockingProcess'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Model subclass: #BrowserUpdate
	instanceVariableNames: 'updates debug inUpdate logger applyingUpdates breakpointsEnabled returnedServices'
	classVariableNames: 'Current'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TreeModel subclass: #JadeiteTreeModel
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrService subclass: #RowanService
	instanceVariableNames: 'command commandArgs updateType organizer updates wasUpdated'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanService subclass: #RowanAnsweringService
	instanceVariableNames: 'answer'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanService subclass: #RowanAutoCommitService
	instanceVariableNames: 'autoCommit postUpdateBlock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanService subclass: #RowanBrowserService
	instanceVariableNames: 'projects removedMethods allClasses hierarchyServices testPackages testCount dictionaries selectedClass newCachedSelectors newCachedClasses name'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanService subclass: #RowanClassService
	instanceVariableNames: 'name comment instVarNames classVarNames classInstVarNames superclassName subclassType poolDictionaryNames classType meta isExtension version versions oop template filters filterType methods selectedPackageServices packageName definedPackageName selectedMethods projectName hierarchyServices variables categories isTestCase expand visibleTests isNewClass updateAfterCommand isInSymbolList dictionaryName wasRemoved renamedName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanService subclass: #RowanComponentService
	instanceVariableNames: 'name componentServices packageServices projectService basename'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanService subclass: #RowanDebuggerService
	instanceVariableNames: 'initialProcessOop processes name'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanService subclass: #RowanDictionaryService
	instanceVariableNames: 'classes hierarchyServices globals defaultTemplate name'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanService subclass: #RowanFrameService
	instanceVariableNames: 'label method stepPoint vars oop homeMethodSelector homeMethodClassName classIsResolvable name'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanService subclass: #RowanInspectorService
	instanceVariableNames: 'oop objects myself className indexedSize visibleIndices nextIndices maxIndexedVars compileErrorArray isOop instVarNames instVarsAreRemovable isDictionary isVariable selectionOop isUnordered statusText name'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanService subclass: #RowanLoggingService
	instanceVariableNames: 'fileName id groupId date time comment services mode location isLogging name'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanService subclass: #RowanMethodService
	instanceVariableNames: 'source selector methodDefinitions category packageName projectName className meta user hasSupers hasSubs isExtension inSelectedPackage references stepPoints selectedPackageServices superDisplayString accessedInstVars breakPoints oop compilationWarnings testResult definedPackage isTestMethod testRunClassName failedCompile comparisonSource firstReference renamedName isMethodForBlock homeMethodOop hasMethodHistory searchString definedClassName name'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanService subclass: #RowanPackageService
	instanceVariableNames: 'classes defaultTemplate projectName classCompilationFailures testClasses hierarchyServices selectedClass name isDirty'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanService subclass: #RowanProcessService
	instanceVariableNames: 'frames oop status name'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanService subclass: #RowanProjectService
	instanceVariableNames: 'sha branch isSkew packages changes existsOnDisk isLoaded projectUrl rowanProjectsHome isDiskDirty projectOop name isDirty specService componentServices'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanService subclass: #RowanQueryService
	instanceVariableNames: 'queryResults name'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanService subclass: #RowanTestService
	instanceVariableNames: 'name'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanService subclass: #RowanVariableService
	instanceVariableNames: 'oop key value className name'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanInspectorService subclass: #RowanSpecificationService
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanProjectService subclass: #RowanDefinedProjectService
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanProjectService subclass: #RowanLoadedProjectService
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanProjectService subclass: #RowanResolvedProjectService
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TreeNode subclass: #JadeiteTreeNode
	instanceVariableNames: 'visited'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Object methodsFor!

isService
	^false! !
!Object categoriesFor: #isService!public!testing! !

"End of package definition"!

"Source Globals"!

"Classes"!

RowanSemaphore guid: (GUID fromString: '{bee6b8be-8e92-4b1e-b1c1-9a35fa6d9cf8}')!
RowanSemaphore comment: ''!
!RowanSemaphore categoriesForClass!Kernel-Processes! !
!RowanSemaphore methodsFor!

critical: aBlock
	lockingProcess := Processor activeProcess.
	[aBlock value] ensure: [lockingProcess := nil]!

lockingProcess
	"Private - for testing"

	^lockingProcess! !
!RowanSemaphore categoriesFor: #critical:!process synchronisation!public! !
!RowanSemaphore categoriesFor: #lockingProcess!accessing!private! !

BrowserUpdate guid: (GUID fromString: '{9ec78c86-d74d-477a-bb5a-254ba1c54272}')!
BrowserUpdate comment: 'Coordinate client presenters with updates from the server.

returnedServices are the last services returned from the server. Handy for testing. '!
!BrowserUpdate categoriesForClass!MVP-Models! !
!BrowserUpdate methodsFor!

activeClassServices
	^self registeredListServices select: [:service | service isClassService]!

activeMethodServices
	^self registeredListServices select: [:service | service isMethodService]!

activePackageServices
	^self registeredListServices select: [:service | service isPackageService]!

activeServices
	"services which are open in windows in list browsers"

	| services |
	services := OrderedCollection new.
	services addAll: self activeMethodServices.
	services addAll: self activeClassServices.
	services addAll: self activePackageServices.
	^services!

applyUpdateWhile: block
	applyingUpdates := true.
	block ensure: 
			[applyingUpdates := false.
			self postUpdate]!

basicIssueCommand: services session: session
	| stonString stonResultString updateResult loggedServices newLoggingService |
	services do: [:service | service prepareForReplication].
	newLoggingService := logger newServiceLoggingService addServices: services.
	self logSentServices: services.
	loggedServices := services asOrderedCollection
				addFirst: newLoggingService;
				yourself. 
	stonString := STON toString: loggedServices. 
	[stonResultString := session serverPerform: #updateFromSton: with: stonString]
		ensure: [services do: [:service | service clearCommand]].
	stonResultString = 'nil' ifFalse: [
		updateResult := self update: services afterStonReplication: stonResultString].
	session releaseAllOops.
	^updateResult!

basicUpdateServices: services session: session
	services do: [:service | service command: #update].
	^self basicIssueCommand: services session: session!

breakpointsEnabled
	^breakpointsEnabled!

breakpointsEnabled: anObject
	breakpointsEnabled := anObject!

debug
	^debug!

debug: aBoolean
	aBoolean
		ifTrue: 
			[Smalltalk at: #roundTrips put: 0].
	debug := aBoolean!

initializeUpdates
	updates := OrderedCollection new.
	!

inUpdate
	"Private -  for testing"

	^inUpdate!

inUpdateWhile: block
	| result |
	BrowserUpdate current logComment: 'About to execute inUpdate block - ' , block printString.
	result := inUpdate critical: [Cursor wait showWhile: block].
	^result!

isApplyingUpdates
	"we've come back from the server and
	are in the middle of updating browsers. 
	Send this to avoid untimely updates to
	services until updates are fully applied"

	^applyingUpdates == true!

isLogging

	^logger isLogging!

isLogging: boolean
	boolean ifTrue: [self startLogging] ifFalse: [self stopLogging]!

issueCommand: service session: session
	^self issueCommands: (Array with: service) session: session!

issueCommands: services session: session
	self inUpdateWhile: 
			[| commandResult |
			commandResult := self basicIssueCommand: services session: session.
			BrowserUpdate current logComment: 'Released inUpdate semaphore'.
			^commandResult]!

logComment: string
	logger logComment: string!

logFileName

	^logger logFileName!

logFileName: string

	^logger logFileName: string!

logger
	"Private - for testing"

	^logger!

logger: anObject
	logger := anObject!

loggingService
	"for testing"

	^logger loggingService!

logReceivedServices: services
	logger logReceivedServices: services.
	logger shouldGetNewLoggerGroup: true!

logSentServices: services
	logger logSentServices: services!

performUpdate: updateBlock with: selector
	updates do: 
			[:update |
			update updateType
				ifNil: [updateBlock value: update]
				ifNotNil: 
					[:type |
					(type == selector or: [type isEmpty or: [type includes: selector]])
						ifTrue: [updateBlock value: update]]]!

postUpdate
	updates do: [:service | service postUpdateBlock ifNotNil: [:block | block value]]!

register: presenter selector: selector
	"send the update to the presenter so that when the presenter gets
	removed, it's events go with it."

	self
		when: #updateReady
		send: #update:withSelector:
		to: presenter
		withArguments: (Array with: self with: selector)!

register: presenter selector: selector browser: browser
	"send the update to the presenter so that when the presenter gets
	removed, it's events go with it."
	self
		when: #updateReady
		send: #update:withSelector:browser:
		to: presenter
		withArguments: (Array
				with: self
				with: selector
				with: browser)!

registeredListPresenters
	^self registeredPresenters select: [:presenter | presenter class canUnderstand: #list]!

registeredListServices
	| listServices |
	listServices := OrderedCollection new.
	self registeredListPresenters do: [:listPresenter | listServices addAll: listPresenter list].
	self registeredTreePresenters do:[:treePresenter | listServices addAll: treePresenter model]. 
	^listServices!

registeredPresenters
	| presenters |
	presenters := Set new.
	(self getEvents ifNil: [^Array new] ifNotNil: [:eventCollection | eventCollection at: #updateReady])
		messagesDo: [:message | presenters add: message receiver].
	^presenters!

registeredTreePresenters
	^self registeredPresenters select: [:presenter | presenter isKindOf: TreePresenter]!

removeEventsTriggeredFor: anObject
	super removeEventsTriggeredFor: anObject.
	self
		logComment: 'Remove events triggered for ' , anObject printString , ' {'
				, anObject identityHash printString , '}'!

resetLoggingService
	logger loggingService: nil. 
	logger newLoggingService!

selectedServices
	| services potentialSelections |
	services := OrderedCollection new.
	self registeredPresenters do: 
			[:presenter |
			(presenter class canUnderstand: #selections)
				ifTrue: 
					[potentialSelections := presenter selections.
					services addAll: (potentialSelections select: [:selection | selection isUpdatableService])]].
	services do:[:service | service command: #update]. 
	^services!

shouldApply: selector forSelector: update
	^update updateType isNil or: 
			[update updateType == selector
				or: [update updateType isEmpty or: [update updateType includes: selector]]]!

shouldGetNewLoggerGroup: boolean

	logger shouldGetNewLoggerGroup: boolean!

startLogging
	logger startLogging!

stopLogging
	logger stopLogging!

update: services afterStonReplication: stonResults
	"assume we get back the 'same' services as we sent"
	self initializeUpdates.
	returnedServices := STON fromString: stonResults.
	logger loggingService replicateFrom: returnedServices last.
	returnedServices := returnedServices copyWithout: returnedServices last.
	self logReceivedServices: returnedServices.
	returnedServices do: [:newService | services do: [:service | service replicateFrom: newService]].
	self updates: returnedServices.
	returnedServices do: [:service | service postUpdate].
	^returnedServices!

updateReady
	updates isEmpty ifFalse: [self trigger: #updateReady]!

updates
	^updates!

updates: aCollection
	"don't just reinitialize the updates instance variable because
	presenters are registered to that object"

	self applyUpdateWhile: 
			[updates removeAll: updates.
			updates addAll: aCollection.
			self updateReady]!

updateService: service session: session
	^self updateServices: (Array with: service) session: session!

updateServices: services session: session
	| commandResult |
	services isEmpty ifTrue: [^self].
	self inUpdateWhile: 
			[commandResult := self basicUpdateServices: services session: session.
			BrowserUpdate current logComment: 'Released inUpdate semaphore'.
			^commandResult]!

updatesPerform: selector presenter: presenter
	"the update may know which client updates 
	it wants us to care about. Respect that."

	
	[updates do: 
			[:update |
			(self shouldApply: selector forSelector: update) ifTrue: [update perform: selector with: presenter]]]
			on: Error
			do: [:ex | self halt]!

updatesPerform: selector presenter: presenter browser: browser
	"the update may know which client updates 
	it wants us to care about if updateType is set. 
	Respect the server's wishes."

	
	[updates do: 
			[:update |
			(self shouldApply: selector forSelector: update)
				ifTrue: 
					[update
						perform: selector
						with: presenter
						with: browser]]]
			on: Error
			do: [:ex | self halt]! !
!BrowserUpdate categoriesFor: #activeClassServices!accessing!public! !
!BrowserUpdate categoriesFor: #activeMethodServices!accessing!public! !
!BrowserUpdate categoriesFor: #activePackageServices!accessing!public! !
!BrowserUpdate categoriesFor: #activeServices!accessing!public! !
!BrowserUpdate categoriesFor: #applyUpdateWhile:!public!updating! !
!BrowserUpdate categoriesFor: #basicIssueCommand:session:!commands!private! !
!BrowserUpdate categoriesFor: #basicUpdateServices:session:!private!updating! !
!BrowserUpdate categoriesFor: #breakpointsEnabled!accessing!private! !
!BrowserUpdate categoriesFor: #breakpointsEnabled:!accessing!private! !
!BrowserUpdate categoriesFor: #debug!accessing!private! !
!BrowserUpdate categoriesFor: #debug:!accessing!private! !
!BrowserUpdate categoriesFor: #initializeUpdates!initialization!public! !
!BrowserUpdate categoriesFor: #inUpdate!private! !
!BrowserUpdate categoriesFor: #inUpdateWhile:!commands!public! !
!BrowserUpdate categoriesFor: #isApplyingUpdates!public!testing! !
!BrowserUpdate categoriesFor: #isLogging!logging!public!testing! !
!BrowserUpdate categoriesFor: #isLogging:!logging!public!testing! !
!BrowserUpdate categoriesFor: #issueCommand:session:!commands!public! !
!BrowserUpdate categoriesFor: #issueCommands:session:!commands!public! !
!BrowserUpdate categoriesFor: #logComment:!logging!public! !
!BrowserUpdate categoriesFor: #logFileName!accessing!public! !
!BrowserUpdate categoriesFor: #logFileName:!accessing!public! !
!BrowserUpdate categoriesFor: #logger!accessing!private! !
!BrowserUpdate categoriesFor: #logger:!accessing!private! !
!BrowserUpdate categoriesFor: #loggingService!accessing!public! !
!BrowserUpdate categoriesFor: #logReceivedServices:!logging!public! !
!BrowserUpdate categoriesFor: #logSentServices:!logging!public! !
!BrowserUpdate categoriesFor: #performUpdate:with:!private!updating! !
!BrowserUpdate categoriesFor: #postUpdate!public!updating! !
!BrowserUpdate categoriesFor: #register:selector:!public!registeringPresenters! !
!BrowserUpdate categoriesFor: #register:selector:browser:!public!registeringPresenters! !
!BrowserUpdate categoriesFor: #registeredListPresenters!private!registeringPresenters! !
!BrowserUpdate categoriesFor: #registeredListServices!private!registeringPresenters! !
!BrowserUpdate categoriesFor: #registeredPresenters!public!registeringPresenters! !
!BrowserUpdate categoriesFor: #registeredTreePresenters!private!registeringPresenters! !
!BrowserUpdate categoriesFor: #removeEventsTriggeredFor:!public!registeringPresenters! !
!BrowserUpdate categoriesFor: #resetLoggingService!initialization!public! !
!BrowserUpdate categoriesFor: #selectedServices!accessing!public! !
!BrowserUpdate categoriesFor: #shouldApply:forSelector:!private!testing! !
!BrowserUpdate categoriesFor: #shouldGetNewLoggerGroup:!accessing!public! !
!BrowserUpdate categoriesFor: #startLogging!logging!public! !
!BrowserUpdate categoriesFor: #stopLogging!logging!public! !
!BrowserUpdate categoriesFor: #update:afterStonReplication:!public!updating! !
!BrowserUpdate categoriesFor: #updateReady!public!updating! !
!BrowserUpdate categoriesFor: #updates!accessing!public! !
!BrowserUpdate categoriesFor: #updates:!accessing!public! !
!BrowserUpdate categoriesFor: #updateService:session:!public!updating! !
!BrowserUpdate categoriesFor: #updateServices:session:!public!updating! !
!BrowserUpdate categoriesFor: #updatesPerform:presenter:!public!updating! !
!BrowserUpdate categoriesFor: #updatesPerform:presenter:browser:!public!updating! !

!BrowserUpdate class methodsFor!

clearCurrent
"
	BrowserUpdate clearCurrent.
"
	Current := nil!

current
	^Current ifNil: [Current := self new]!

startLogging
	Current startLogging!

stopLogging
	Current stopLogging! !
!BrowserUpdate class categoriesFor: #clearCurrent!accessing!public! !
!BrowserUpdate class categoriesFor: #current!accessing!public! !
!BrowserUpdate class categoriesFor: #startLogging!logging!public! !
!BrowserUpdate class categoriesFor: #stopLogging!logging!public! !

JadeiteTreeModel guid: (GUID fromString: '{9da5c17c-b7f6-4238-a00b-164c06f3067b}')!
JadeiteTreeModel comment: ''!
!JadeiteTreeModel categoriesForClass!MVP-Models! !
!JadeiteTreeModel methodsFor!

nodeClass
	"Answer the class of object to be used to represent the receiver's nodes."

	^JadeiteTreeNode!

resetVisited
	self asBag do: [:object | (self lookupNode: object) resetVisited]! !
!JadeiteTreeModel categoriesFor: #nodeClass!constants!public! !
!JadeiteTreeModel categoriesFor: #resetVisited!constants!public! !

!JadeiteTreeModel class methodsFor!

defaultSearchPolicy
	"Answer the default <searchPolicy> used by instances of the receiver."

	^SearchPolicy equality! !
!JadeiteTreeModel class categoriesFor: #defaultSearchPolicy!constants!public! !

RowanService guid: (GUID fromString: '{c91bf577-a5a9-4782-b6be-c05df3222bc9}')!
RowanService comment: ''!
!RowanService categoriesForClass!Kernel-Objects! !
!RowanService methodsFor!

addCachedSymbols: unused
	!

autoCommitUpdate: browser!

basicPrepareForReplication
	"don't call anything potentially recursive here"!

basicReplicateFrom: newService
	1 to: self class instSize
		do: 
			[:index |
			(self shouldReplicateInstVarAtIndex: index newService: newService)
				ifTrue: [self instVarAt: index put: (newService instVarAt: index)]]!

breakpointSettingChanged: transcript!

chooseModel: presenter using: browser
	| treeModel |
	treeModel := browser projectListPresenter selectionOrNil isNil
				ifTrue: 
					[JadeiteTreeModel new
						searchPolicy: SearchPolicy equality;
						reset]
				ifFalse: [presenter model].
	^treeModel!

classCategoryUpdate: presenter
	!

classCommentUpdate: presenter
	
	!

classDefinitionUpdate: presenter
	!

classesHierarchyUpdate: presenter
!

classesUpdate: presenter browser: anObject
	!

classHierarchyUpdate: presenter
	!

classHierarchyUpdate: presenter browser: browser
!

classHierarchyUpdate: presenter browser: browser hierarchyServices: hierarchyServices
	| treeModel subclasses parent |
	hierarchyServices ifNil: [^self].
	hierarchyServices isEmpty ifTrue: [^self].
	browser isClassListTabSelected ifTrue: [^self].
	presenter selections notEmpty ifTrue: [(presenter selections includes: self) ifFalse: [^self]].
	treeModel := JadeiteTreeModel new
				searchPolicy: SearchPolicy equality;
				reset.
	parent := nil.
	subclasses := hierarchyServices at: #nil ifAbsent: [].
	subclasses
		ifNil: 
			[| subs |
			parent := self.
			treeModel := presenter model.
			subs := hierarchyServices at: #expand.
			1 to: subs size
				do: 
					[:index |
					| classService node |
					classService := subs at: index.
					node := treeModel getNodeFor: classService ifAbsent: [].
					node
						ifNotNil: 
							[treeModel remove: classService ifAbsent: [].
							subs at: index put: node object]].
			subclasses := subs].
	subclasses do: 
			[:classService |
			self
				possiblyAddHierarchyService: classService
				to: treeModel
				withParent: parent
				hierarchyServices: hierarchyServices].
	presenter model: treeModel.
	presenter view updateMode: #lazy.	"faster for big tree building"
	presenter view disableRedraw.
	
	[treeModel preOrderDo: 
			[:classService |
			classService selectedPackageServices: browser packageListPresenter selections browser: browser.
			classService expand == true ifTrue: [presenter view expand: classService]]]
			ensure: [presenter view enableRedraw].
	presenter selectionIfNone: [^presenter view ensureItemVisible: treeModel roots first].
	presenter view ensureSelectionVisible.
	presenter view updateMode: #dynamic!

classMethodsUpdate: presenter browser: browser
	!

classUpdate: presenter
	!

clearCommand

	command := nil. 
	self commandArgs: nil. !

clientDefinitionClass

	^self subclassResponsibility!

command
	^command!

command: symbol
	command := symbol.
	!

commandArgs
	^commandArgs!

commandArgs: anObject
	commandArgs := anObject!

componentPackagesUpdate: presenter browser: browser!

componentsUpdate: presenter browser: browser!

debuggerMethodSourceUpdate: presenter browser: browser!

debugPrintOn: aStream
	self printOn: aStream.
	self class instVarNames do: 
			[:instVarName |
			aStream
				tab;
				nextPutAll: instVarName;
				nextPut: $:;
				space;
				nextPutAll: (self instVarNamed: instVarName) printString;
				cr]!

debugPrintString
	| ws |
	ws := WriteStream on: String new.
	self debugPrintOn: ws.
	^ws contents!

dictionaryListUpdate: presenter

	!

dictionaryUpdate: presenter
	!

emptyFilterListsIn: browser
	browser categoryListPresenter sortBlock: [:x :y | x < y].
	browser variableListPresenter sortBlock: [:x :y | x < y].!

excludedInstVars

	^#( 'events' )
!

filterUpdate: presenter browser: anObject
	!

frameListUpdate: aPresenter!

globalsUpdate: presenter!

initialize
	commandArgs := Array new!

initializePresenterList: presenter
	presenter sortBlock: [:x :y | x sortAspect < y sortAspect].
	presenter list: ListModel new!

isAnsweringService

	^false!

isBrowserService

	^false!

isClassService

	^false!

isComponentService
	^false!

isDefinedProject

	^true!

isDictionaryService

	^false!

isInspectorService

	^false!

isLoggingService

	^false!

isMethodService

	^false!

isPackageService

	^false!

isProjectService

	^false!

isService
	^true!

isTestCase

	^false!

isUpdatableService
	^true!

methodFilterUpdate: presenter
	!

methodHistoryUpdated: historyBrowser
	!

methodListUpdate: presenter browser: browser!

methodSourceUpdate: presenter browser: anObject
	!

methodsUpdate: presenter
	!

methodUpdate: presenter browser: anObject
	!

name
	^nil!

newProject: presenter!

notRowanizedPackageName
	^self class notRowanizedPackageName!

packageServices
	"most services do not have packages"

	^Array new!

packagesUpdate: presenter!

packagesUpdate: presenter browser: browser parentPresenter: parentPresenter
	| packageServices selectedComponentPackages |
	(presenter class canUnderstand: #list) ifFalse: [^self].
	presenter list isEmpty ifTrue: [self initializePresenterList: presenter].
	parentPresenter selections detect: [:service | service name = self name] ifNone: [^self].
	selectedComponentPackages := (browser componentListPresenter selectionIfNone: [])
				ifNil: [self packageServices]
				ifNotNil: [:componentService | componentService packageServices].
	packageServices := self packageServices intersection: selectedComponentPackages.
	self
		updateList: presenter
		whilePreservingSelections: packageServices
		browser: browser.
	browser isClassSelected ifFalse: [self emptyFilterListsIn: browser]!

packageUpdate: presenter!

possiblyAddComponent: service to: treeModel withParent: parentService hierarchyServices: hierarchyServices
	| node |
	node := treeModel getNodeFor: service
				ifAbsent: 
					[treeModel add: service asChildOf: parentService.
					treeModel getNodeFor: service].
	node visited: true.
	(hierarchyServices at: service ifAbsent: [^self]) do: 
			[:aService |
			self
				possiblyAddHierarchyService: aService
				to: treeModel
				withParent: service
				hierarchyServices: hierarchyServices]!

possiblyAddHierarchyService: service to: treeModel withParent: parentService hierarchyServices: hierarchyServices
	| node |
	node := treeModel getNodeFor: service
				ifAbsent: 
					[treeModel add: service asChildOf: parentService.
					treeModel getNodeFor: service].
	node visited: true.
	(hierarchyServices at: service ifAbsent: [^self]) do: 
			[:aService |
			self
				possiblyAddHierarchyService: aService
				to: treeModel
				withParent: service
				hierarchyServices: hierarchyServices]!

postReload
	"most services will do nothing"

	!

postUpdate
	"Give the service a chance to clean up unnecessary objects after replication"

	!

postUpdateBlock
	"RowanAutoCommitService is the only use now. 
	Pushed the inst var to that class to conform with RSR
	replication protocols."

	^nil!

prepareForReplication
	"only replicate what is absolutely necessary. Each service knows how to do that. 
	This method prepares each command arg as well"
	commandArgs ifNotNil: [:args | args basicPrepareForReplication]!

printOn: aStream

	super printOn: aStream. 
	aStream nextPut: $:. 
	aStream nextPutAll: (self name ifNil: [nil printString])!

processListUpdate: aPresenter!

projectPackagesUpdate: aPresenter browser: anObject
	!

projectSelectionUpdate: aPresenter
!

projectsUpdate: aPresenter!

projectsUpdate: presenter browser: browser!

remoteServiceName
	self subclassResponsibility!

removed: presenter
	| service |
	(updateType = #removed:) ifFalse: [^self	"server must tell us to remove service"].
	service := presenter model asBag detect: [:svc | svc = self] ifNone: [^self].
	presenter model remove: service.
	presenter resetSelection!

removedClass: aPresenter!

removeDeletedClassesIn: presenter browser: browser classes: theClasses
	| updatedClasses removedClasses renamedClassNames |
	updatedClasses := theClasses select: [:classService | classService packageName = self name]
				thenCollect: [:classService | classService name asString].
	removedClasses := presenter list
				select: [:classService | classService packageName = self name and: [(updatedClasses includes: classService name) not]].
	renamedClassNames := theClasses collect: [:classService | classService renamedName].
	removedClasses := removedClasses
				reject: [:classService | renamedClassNames includes: classService name].
	removedClasses
		do: [:removedClassService | (presenter selections includes: removedClassService) ifTrue: [presenter view basicResetSelection]].
	removedClasses isEmpty ifTrue: [^self].
	presenter model
		setList: (ListModel withAll: (presenter list asArray copyWithoutAll: removedClasses))
		searchPolicy: SearchPolicy equality!

removedProject: presenter!

renamedClass: aClassService browser: anObject
	!

renamedClassInHierarchy: presenter browser: anObject
	!

replicateFrom: newService
	(newService class = self class and: [newService name = self name])
		ifTrue: [self basicReplicateFrom: newService]!

rsrCommand: symbol
	^self rsrCommand: symbol withArguments: Array new!

rsrCommand: symbol withArguments: arguments
	command := symbol.
	commandArgs := arguments.
	^remoteSelf perform: #executeCommand !

shouldReplicateInstVarAtIndex: index newService: anObject
	^true!

stonOn: stonWriter
	| instanceVariableNames |
	(instanceVariableNames := self class allInstVarNames
				reject: [:iv | self excludedInstVars includes: iv]) isEmpty
		ifTrue: [stonWriter writeObject: self do: [stonWriter encodeMap: #()]]
		ifFalse: 
			[stonWriter writeObject: self
				streamMap: 
					[:dictionary |
					instanceVariableNames do: 
							[:each |
							(self instVarNamed: each)
								ifNotNil: [:value | dictionary at: each asSymbol put: value]
								ifNil: [self stonShouldWriteNilInstVars ifTrue: [dictionary at: each asSymbol put: nil]]]]]!

sunitMethodsUpdate: presenter browser: browser!

superclassListUpdate: presenter
	!

testClasses: presenter browser: anObject
	!

testPackages: presenter!

testResultUpdate: presenter browser: anObject
	!

toolTip
	^self displayString
!

updateBreakPoints: presenter browser: browser!

updateClassCategorySelectionsFor: presenter!

updateClassHierarchySelectionFor: presenter!

updateClassSelectionFor: presenter!

updatedClass: aPresenter browser: browser
	!

updatedClassDefinition: classDefinitionPresenter browser: browser

	
	!

updatedClassInHierarchy: aPresenter browser: projectBrowser!

updateDictionarySelectionsFor: presenter
	!

updateList: presenter whilePreservingSelections: updates browser: anObject
	| replicate bag |
	bag := presenter list asBag.
	bag do: 
			[:service |
			replicate := updates detect: [:update | update = service] ifNone: [nil].
			replicate notNil ifTrue: [service replicateFrom: replicate]].
	updates
		do: [:update | ((bag includes: update) and: [update wasRenamed not]) ifFalse: [presenter model add: update]]!

updateMethodFilterSelectionFor: presenter!

updateMethodSelectionFor: presenter!

updatePackageSelectionsFor: presenter
	!

updateSelectorFor: presenter!

updateSuperclassSelectionFor: presenter!

updateSymbols: unused!

updateType
	^updateType!

updateType: anObject
	updateType := anObject!

updateVariable: listPresenter debugger: anObject
	!

variableDataUpdate: aPresenter!

variableListUpdate: aPresenter!

wasRenamed
	"not all services can be renamed"

	^false!

wasUpdated
	^wasUpdated!

wasUpdated: anObject
	wasUpdated := anObject! !
!RowanService categoriesFor: #addCachedSymbols:!public!updating! !
!RowanService categoriesFor: #autoCommitUpdate:!public!updating! !
!RowanService categoriesFor: #basicPrepareForReplication!public!replication! !
!RowanService categoriesFor: #basicReplicateFrom:!public!replication! !
!RowanService categoriesFor: #breakpointSettingChanged:!public!updating! !
!RowanService categoriesFor: #chooseModel:using:!private!updating! !
!RowanService categoriesFor: #classCategoryUpdate:!public!updating! !
!RowanService categoriesFor: #classCommentUpdate:!public!updating! !
!RowanService categoriesFor: #classDefinitionUpdate:!public!updating! !
!RowanService categoriesFor: #classesHierarchyUpdate:!public!updating! !
!RowanService categoriesFor: #classesUpdate:browser:!public!updating! !
!RowanService categoriesFor: #classHierarchyUpdate:!public!updating! !
!RowanService categoriesFor: #classHierarchyUpdate:browser:!public!updating! !
!RowanService categoriesFor: #classHierarchyUpdate:browser:hierarchyServices:!public!updating! !
!RowanService categoriesFor: #classMethodsUpdate:browser:!public!updating! !
!RowanService categoriesFor: #classUpdate:!public!updating! !
!RowanService categoriesFor: #clearCommand!Init / Release!public! !
!RowanService categoriesFor: #clientDefinitionClass!public! !
!RowanService categoriesFor: #command!accessing!public! !
!RowanService categoriesFor: #command:!accessing!public! !
!RowanService categoriesFor: #commandArgs!accessing!public! !
!RowanService categoriesFor: #commandArgs:!accessing!public! !
!RowanService categoriesFor: #componentPackagesUpdate:browser:!public!updating! !
!RowanService categoriesFor: #componentsUpdate:browser:!public!updating! !
!RowanService categoriesFor: #debuggerMethodSourceUpdate:browser:!public!updating! !
!RowanService categoriesFor: #debugPrintOn:!printing!public! !
!RowanService categoriesFor: #debugPrintString!printing!public! !
!RowanService categoriesFor: #dictionaryListUpdate:!public!updating! !
!RowanService categoriesFor: #dictionaryUpdate:!public!updating! !
!RowanService categoriesFor: #emptyFilterListsIn:!Init / Release!private! !
!RowanService categoriesFor: #excludedInstVars!public!ston! !
!RowanService categoriesFor: #filterUpdate:browser:!public!updating! !
!RowanService categoriesFor: #frameListUpdate:!Debugger!public!updating! !
!RowanService categoriesFor: #globalsUpdate:!public!updating! !
!RowanService categoriesFor: #initialize!Init / Release!public! !
!RowanService categoriesFor: #initializePresenterList:!Init / Release!private! !
!RowanService categoriesFor: #isAnsweringService!public!testing! !
!RowanService categoriesFor: #isBrowserService!public!testing! !
!RowanService categoriesFor: #isClassService!public!testing! !
!RowanService categoriesFor: #isComponentService!public!testing! !
!RowanService categoriesFor: #isDefinedProject!public!testing! !
!RowanService categoriesFor: #isDictionaryService!public!testing! !
!RowanService categoriesFor: #isInspectorService!public!testing! !
!RowanService categoriesFor: #isLoggingService!public!testing! !
!RowanService categoriesFor: #isMethodService!public!testing! !
!RowanService categoriesFor: #isPackageService!public!testing! !
!RowanService categoriesFor: #isProjectService!public!testing! !
!RowanService categoriesFor: #isService!public!testing! !
!RowanService categoriesFor: #isTestCase!public!testing! !
!RowanService categoriesFor: #isUpdatableService!public!testing! !
!RowanService categoriesFor: #methodFilterUpdate:!public!updating! !
!RowanService categoriesFor: #methodHistoryUpdated:!must not strip!public!updating! !
!RowanService categoriesFor: #methodListUpdate:browser:!public!updating! !
!RowanService categoriesFor: #methodSourceUpdate:browser:!public!updating! !
!RowanService categoriesFor: #methodsUpdate:!public!updating! !
!RowanService categoriesFor: #methodUpdate:browser:!public!updating! !
!RowanService categoriesFor: #name!accessing!public! !
!RowanService categoriesFor: #newProject:!public!updating! !
!RowanService categoriesFor: #notRowanizedPackageName!displaying!public! !
!RowanService categoriesFor: #packageServices!accessing!public! !
!RowanService categoriesFor: #packagesUpdate:!public!updating! !
!RowanService categoriesFor: #packagesUpdate:browser:parentPresenter:!public!updating! !
!RowanService categoriesFor: #packageUpdate:!public!updating! !
!RowanService categoriesFor: #possiblyAddComponent:to:withParent:hierarchyServices:!private!updating! !
!RowanService categoriesFor: #possiblyAddHierarchyService:to:withParent:hierarchyServices:!private! !
!RowanService categoriesFor: #postReload!public!replication! !
!RowanService categoriesFor: #postUpdate!Init / Release!public! !
!RowanService categoriesFor: #postUpdateBlock!accessing!private! !
!RowanService categoriesFor: #prepareForReplication!public!replication! !
!RowanService categoriesFor: #printOn:!printing!public! !
!RowanService categoriesFor: #processListUpdate:!Debugger!public!updating! !
!RowanService categoriesFor: #projectPackagesUpdate:browser:!public!updating! !
!RowanService categoriesFor: #projectSelectionUpdate:!public!updating! !
!RowanService categoriesFor: #projectsUpdate:!public!updating! !
!RowanService categoriesFor: #projectsUpdate:browser:!public!updating! !
!RowanService categoriesFor: #remoteServiceName!must not strip!public! !
!RowanService categoriesFor: #removed:!public!updating! !
!RowanService categoriesFor: #removedClass:!public!updating! !
!RowanService categoriesFor: #removeDeletedClassesIn:browser:classes:!private!updating! !
!RowanService categoriesFor: #removedProject:!public!updating! !
!RowanService categoriesFor: #renamedClass:browser:!public!updating! !
!RowanService categoriesFor: #renamedClassInHierarchy:browser:!public!updating! !
!RowanService categoriesFor: #replicateFrom:!public!replication! !
!RowanService categoriesFor: #rsrCommand:!public!rsr! !
!RowanService categoriesFor: #rsrCommand:withArguments:!public!rsr! !
!RowanService categoriesFor: #shouldReplicateInstVarAtIndex:newService:!public!testing! !
!RowanService categoriesFor: #stonOn:!must not strip!public!ston! !
!RowanService categoriesFor: #sunitMethodsUpdate:browser:!public!updating! !
!RowanService categoriesFor: #superclassListUpdate:!public!updating! !
!RowanService categoriesFor: #testClasses:browser:!public!updating! !
!RowanService categoriesFor: #testPackages:!public!updating! !
!RowanService categoriesFor: #testResultUpdate:browser:!public!updating! !
!RowanService categoriesFor: #toolTip!printing!public! !
!RowanService categoriesFor: #updateBreakPoints:browser:!public!updating! !
!RowanService categoriesFor: #updateClassCategorySelectionsFor:!public!updating! !
!RowanService categoriesFor: #updateClassHierarchySelectionFor:!public!updating! !
!RowanService categoriesFor: #updateClassSelectionFor:!public!updating! !
!RowanService categoriesFor: #updatedClass:browser:!public!updating! !
!RowanService categoriesFor: #updatedClassDefinition:browser:!public!updating! !
!RowanService categoriesFor: #updatedClassInHierarchy:browser:!public!updating! !
!RowanService categoriesFor: #updateDictionarySelectionsFor:!public!updating! !
!RowanService categoriesFor: #updateList:whilePreservingSelections:browser:!private! !
!RowanService categoriesFor: #updateMethodFilterSelectionFor:!public!updating! !
!RowanService categoriesFor: #updateMethodSelectionFor:!public!updating! !
!RowanService categoriesFor: #updatePackageSelectionsFor:!public!updating! !
!RowanService categoriesFor: #updateSelectorFor:!public!updating! !
!RowanService categoriesFor: #updateSuperclassSelectionFor:!public!updating! !
!RowanService categoriesFor: #updateSymbols:!public!updating! !
!RowanService categoriesFor: #updateType!accessing!public! !
!RowanService categoriesFor: #updateType:!accessing!public! !
!RowanService categoriesFor: #updateVariable:debugger:!public!updating! !
!RowanService categoriesFor: #variableDataUpdate:!Debugger!public!updating! !
!RowanService categoriesFor: #variableListUpdate:!Debugger!public!updating! !
!RowanService categoriesFor: #wasRenamed!public!testing! !
!RowanService categoriesFor: #wasUpdated!accessing!private! !
!RowanService categoriesFor: #wasUpdated:!accessing!private! !

!RowanService class methodsFor!

command: symbol withArgs: array
	| inst |
	inst := self new.
	inst
		command: symbol;
		commandArgs: array.
	^inst!

defaultIcon
	"Answer a suitable default icon to use for this class. Not all classes use their 
	default icon; one must define an appropriate #icon method in each class where the 
	icon required differs from that of the superclass."

	^(self environment at: #Icon) fromId: self defaultIconName.
!

defaultIconName
	"Answer a filename to use for an icon of this class."

	^File composeStem: 'Model' extension: 'ico'.!

icon
	"Answers an Icon that can be used to represent this class"

	^##(self) defaultIcon!

new
	^super new initialize!

notRowanizedPackageName
	^'(NONE)'!

templateClassName	^self name!

version
	"change this method carefully and only at Jadeite release boundaries.
	Failure to do so may prevent logins"

	^3200! !
!RowanService class categoriesFor: #command:withArgs:!instance creation!public! !
!RowanService class categoriesFor: #defaultIcon!private! !
!RowanService class categoriesFor: #defaultIconName!private! !
!RowanService class categoriesFor: #icon!private! !
!RowanService class categoriesFor: #new!initialization!public! !
!RowanService class categoriesFor: #notRowanizedPackageName!constants!public! !
!RowanService class categoriesFor: #templateClassName!public! !
!RowanService class categoriesFor: #version!public!version actions! !

RowanAnsweringService guid: (GUID fromString: '{d2ef2348-8273-4df1-b823-ba4509d53308}')!
RowanAnsweringService comment: 'An answer service doesn''t understand any update commands. 
He just responds with an answer object. '!
!RowanAnsweringService categoriesForClass!Kernel-Objects! !
!RowanAnsweringService methodsFor!

answer
	^answer!

answer: anObject
	answer := anObject!

autoCommitIn: session
	command := #autoCommit.
	BrowserUpdate current issueCommands: (Array with: self) session: session.
	^answer!

breakPointsAreEnabled: session
	command := #breakPointsAreEnabled.
	BrowserUpdate current issueCommands: (Array with: self) session: session.
	^answer!

breakpointSettingChanged: transcript
	updateType
		ifNotNil: 
			[transcript areBreakpointsEnabled value: answer.
			BrowserUpdate current breakpointsEnabled: answer]!

canAccessServiceClasses: session
	command := #canAccessServiceClasses.
	self commandArgs: Array new.
	BrowserUpdate current issueCommands: (Array with: self) session: session.
	^answer!

exec: string in: session
	command := #exec:.
	commandArgs := Array with: string.
	BrowserUpdate current issueCommands: (Array with: self) session: session!

expressionSelector: string session: session
	command := #expressionSelector:.
	commandArgs := Array with: string.
	BrowserUpdate current issueCommands: (Array with: self) session: session.
	^answer!

flipAutoCommitFor: session
	command := #flipAutoCommit.
	BrowserUpdate current issueCommands: (Array with: self) session: session.
	^answer!

flipTranscriptIn: session
	command := #flipTranscript.
	BrowserUpdate current issueCommands: (Array with: self) session: session!

isAnsweringService

	^true!

isTranscriptInstalledIn: session
	command := #transcriptInstalled.
	BrowserUpdate current issueCommands: (Array with: self) session: session.
	^answer!

lowercaseSelectorsMatching: string session: session
	"sender should pass in lowercase string which will be 
	compared to lowercase on the server. Avoids case misses"

	command := #lowercaseSelectorsMatching:.
	commandArgs := Array with: string.
	BrowserUpdate current issueCommands: (Array with: self) session: session.
	^answer!

needsCommit: session
	command := #needsCommit.
	self commandArgs: Array new.
	BrowserUpdate current issueCommands: (Array with: self) session: session.
	^answer!

printOn: stream
	stream
		nextPutAll: self class name;
		space;
		nextPut: $(;
		nextPutAll: command printString;
		nextPutAll: '->';
		nextPutAll: commandArgs printString;
		nextPut: $)!

setAutoCommit: boolean for: session
	command := #setAutoCommit:.
	self commandArgs: (Array with: boolean).
	BrowserUpdate current issueCommands: (Array with: self) session: session.
	^answer!

setBreakpointsEnabled: boolean session: session
	command := #setBreakPointsAreEnabled:.
	commandArgs := Array with: boolean.
	BrowserUpdate current issueCommands: (Array with: self) session: session!

symbolExists: aSymbol session: session
	command := #symbolExists:.
	commandArgs := Array with: aSymbol.
	BrowserUpdate current issueCommands: (Array with: self) session: session.
	^answer!

symbolsMatching: string session: session
	command := #symbolsMatching:.
	commandArgs := Array with: string.
	BrowserUpdate current issueCommands: (Array with: self) session: session.
	^answer! !
!RowanAnsweringService categoriesFor: #answer!accessing!private! !
!RowanAnsweringService categoriesFor: #answer:!accessing!private! !
!RowanAnsweringService categoriesFor: #autoCommitIn:!commands!public! !
!RowanAnsweringService categoriesFor: #breakPointsAreEnabled:!commands!public! !
!RowanAnsweringService categoriesFor: #breakpointSettingChanged:!public!updating! !
!RowanAnsweringService categoriesFor: #canAccessServiceClasses:!commands!public! !
!RowanAnsweringService categoriesFor: #exec:in:!commands!public! !
!RowanAnsweringService categoriesFor: #expressionSelector:session:!commands!public! !
!RowanAnsweringService categoriesFor: #flipAutoCommitFor:!commands!public! !
!RowanAnsweringService categoriesFor: #flipTranscriptIn:!commands!public! !
!RowanAnsweringService categoriesFor: #isAnsweringService!public!testing! !
!RowanAnsweringService categoriesFor: #isTranscriptInstalledIn:!commands!public! !
!RowanAnsweringService categoriesFor: #lowercaseSelectorsMatching:session:!commands!public!testing! !
!RowanAnsweringService categoriesFor: #needsCommit:!commands!public! !
!RowanAnsweringService categoriesFor: #printOn:!printing!public! !
!RowanAnsweringService categoriesFor: #setAutoCommit:for:!commands!public! !
!RowanAnsweringService categoriesFor: #setBreakpointsEnabled:session:!commands!public! !
!RowanAnsweringService categoriesFor: #symbolExists:session:!commands!public!testing! !
!RowanAnsweringService categoriesFor: #symbolsMatching:session:!commands!public!testing! !

RowanAutoCommitService guid: (GUID fromString: '{32b30a33-c470-43de-befd-cab5cd6cd8a7}')!
RowanAutoCommitService comment: ''!
!RowanAutoCommitService categoriesForClass!Unclassified! !
!RowanAutoCommitService methodsFor!

autoCommitUpdate: browser
	| shouldAbort |
	autoCommit == #failed
		ifTrue: 
			[postUpdateBlock := 
					[shouldAbort := MessageBox
								confirm: 'Autocommit has failed to commit. Your changes will not be commited in the repository until you abort. Abort now?

(If you choose ''No'' you will be prompted with a list of transaction conflicts)'.
					shouldAbort
						ifTrue: [JadePresenter abortTransaction: browser gciSession]
						ifFalse: [self displayTransactionConflicts: browser]]].
	browser autoCommit: autoCommit.
	^postUpdateBlock!

displayName
	"for logging. for now"

	^self printString!

postUpdateBlock

	^postUpdateBlock!

sortAspect
	"don't think this is used"
	^self printString ! !
!RowanAutoCommitService categoriesFor: #autoCommitUpdate:!public!updating! !
!RowanAutoCommitService categoriesFor: #displayName!displaying!public! !
!RowanAutoCommitService categoriesFor: #postUpdateBlock!accessing!public! !
!RowanAutoCommitService categoriesFor: #sortAspect!accessing!public! !

RowanBrowserService guid: (GUID fromString: '{9c5e8a88-ca64-403c-8f81-6c70c46926f0}')!
RowanBrowserService comment: ''!
!RowanBrowserService categoriesForClass!Unclassified! !
!RowanBrowserService methodsFor!

= browserService
	^browserService isBrowserService and: [name = browserService name]!

allClasses
	^allClasses!

allClasses: theClasses
	allClasses := theClasses!

basicPrepareForReplication
	"don't call anything potentially recursive here.
	method services don't iterate over subcollections"

	removedMethods
		ifNotNil: [removedMethods do: [:methodService | methodService basicPrepareForReplication]].
	dictionaries := nil .
	!

classHierarchyUpdate: presenter browser: browser
	browser isHierarchyTabSelected ifFalse: [^false].
	self
		classHierarchyUpdate: presenter
		browser: browser
		hierarchyServices: hierarchyServices!

dictionaryListUpdate: presenter
	"no sort. dictionaries maintain their server order"
	| removals replicate |	
	presenter list isEmpty ifTrue: [presenter list: ListModel new].
	removals := presenter list reject: [:dictionaryService | dictionaries includes: dictionaryService].
	presenter model removeAll: removals.
	presenter list copy do: 
			[:service |
			replicate := dictionaries detect: [:update | update = service] ifNone: [nil].
			replicate notNil ifTrue: [service replicateFrom: replicate]].
	1 to: dictionaries size
		do: 
			[:index |
			(dictionaries at: index) = (presenter model at: index ifAbsent: [RowanDictionaryService new])
				ifFalse: [presenter model add: (dictionaries at: index) afterIndex: index - 1]]!

displayName
	"for logging. for now"

	^name!

excludedInstVars
	^super excludedInstVars , #('hierarchyServices')!

hash
	^name hash!

initialize
	super initialize.
	allClasses := OrderedCollection new!

isBrowserService

	^true!

name
	"browser services aren't named, but (for convenience) can respond"

	^nil!

name: aString
	name := aString asString!

newProjectNamed: projectName session: session windowHandle: handle
	self
		command: #newProjectNamed:windowHandle:;
		commandArgs: (Array with: projectName with: handle).
	BrowserUpdate current issueCommands: (Array with: self) session: session!

prepareForReplication
	super prepareForReplication.
	self basicPrepareForReplication!

printOn: aStream
	super printOn: aStream.
	aStream nextPut: $(.
	command
		ifNotNil: 
			[aStream
				nextPutAll: 'command: ';
				print: command;
				space;
				nextPutAll: 'commandArgs: ';
				print: commandArgs].
	aStream nextPut: $)!

projects
	^projects!

projectsUpdate: presenter
	| removals |
	presenter list isEmpty ifTrue: [self initializePresenterList: presenter].
	removals := presenter list select: [:projectService | (projects includes: projectService) not].
	presenter model removeAll: removals.
	self
		updateList: presenter
		whilePreservingSelections: projects
		browser: nil!

projectsUpdate: presenter browser: browser
	| removals |
	presenter list isEmpty ifTrue: [self initializePresenterList: presenter].
	removals := presenter list select: 
					[:projectService |
					(projects includes: projectService) not and: [projectService ~= RowanProjectService noneProject]].
	presenter model removeAll: removals.
	presenter selections isEmpty ifTrue: [browser updateProjectPackages].
	self
		updateList: presenter
		whilePreservingSelections: projects
		browser: browser.
	browser addNoneProject.
	browser possiblyClearProjectInfoTab!

registerWindow: handle rootObject: oop session: session
	self
		command: #saveRootObject:windowHandle:;
		commandArgs: (Array with: oop with: handle).
	JadePresenter issueCommands: (Array with: self) session: session!

releaseWindow: handle session: session
	self
		command: #releaseWindowHandle:;
		commandArgs: (Array with: handle).
	JadePresenter issueCommands: (Array with: self) session: session!

reloadProjects: selectedProjectServices presenter: presenter
	| selectedServices |
	selectedServices := BrowserUpdate current selectedServices asSet asOrderedCollection.
	selectedServices removeAll: selectedProjectServices.
	Cursor wait showWhile: 
			[self
				command: #reloadProjects:andUpdateServices:;
				commandArgs: (Array with: selectedProjectServices asSet asArray with: selectedServices).
			BrowserUpdate current issueCommands: (Array with: self) session: presenter gciSession]!

removedMethods
	^removedMethods!

removedMethods: anObject
	removedMethods := anObject!

replicateFrom: newService
	self isBrowserService ifTrue: [super replicateFrom: newService]!

selectedClass

	^selectedClass!

selectedClass: anObject
	selectedClass := anObject!

sortAspect

	^name!

testCount
	^testCount!

testCount: anObject
	testCount := anObject!

testPackages: presenter
	| additions |
	additions := OrderedCollection new.
	testPackages do: 
			[:newPackageService |
			| updatedPackage |
			updatedPackage := presenter model detect: [:packageService | packageService = newPackageService]
						ifNone: 
							[additions add: newPackageService.
							nil].
			updatedPackage ifNotNil: [updatedPackage replicateFrom: newPackageService]].
	presenter model addAll: additions.
	presenter model copy do:[:oldPackageService |
		(testPackages includes: oldPackageService) ifFalse:[presenter model remove: oldPackageService]]. ! !
!RowanBrowserService categoriesFor: #=!comparing!public! !
!RowanBrowserService categoriesFor: #allClasses!accessing!private! !
!RowanBrowserService categoriesFor: #allClasses:!accessing!private! !
!RowanBrowserService categoriesFor: #basicPrepareForReplication!public!replication! !
!RowanBrowserService categoriesFor: #classHierarchyUpdate:browser:!public!updating! !
!RowanBrowserService categoriesFor: #dictionaryListUpdate:!public!updating! !
!RowanBrowserService categoriesFor: #displayName!displaying!public! !
!RowanBrowserService categoriesFor: #excludedInstVars!public!ston! !
!RowanBrowserService categoriesFor: #hash!comparing!public! !
!RowanBrowserService categoriesFor: #initialize!initialization!public! !
!RowanBrowserService categoriesFor: #isBrowserService!public!testing! !
!RowanBrowserService categoriesFor: #name!accessing!public! !
!RowanBrowserService categoriesFor: #name:!accessing!public! !
!RowanBrowserService categoriesFor: #newProjectNamed:session:windowHandle:!commands!public!registering windows! !
!RowanBrowserService categoriesFor: #prepareForReplication!public!replication! !
!RowanBrowserService categoriesFor: #printOn:!printing!public! !
!RowanBrowserService categoriesFor: #projects!accessing!public! !
!RowanBrowserService categoriesFor: #projectsUpdate:!public!updating! !
!RowanBrowserService categoriesFor: #projectsUpdate:browser:!public!updating! !
!RowanBrowserService categoriesFor: #registerWindow:rootObject:session:!commands!public!registering windows! !
!RowanBrowserService categoriesFor: #releaseWindow:session:!commands!public!registering windows! !
!RowanBrowserService categoriesFor: #reloadProjects:presenter:!commands!public! !
!RowanBrowserService categoriesFor: #removedMethods!accessing!private! !
!RowanBrowserService categoriesFor: #removedMethods:!accessing!private! !
!RowanBrowserService categoriesFor: #replicateFrom:!public!replication! !
!RowanBrowserService categoriesFor: #selectedClass!accessing!public! !
!RowanBrowserService categoriesFor: #selectedClass:!accessing!public! !
!RowanBrowserService categoriesFor: #sortAspect!accessing!public! !
!RowanBrowserService categoriesFor: #testCount!accessing!private! !
!RowanBrowserService categoriesFor: #testCount:!accessing!private! !
!RowanBrowserService categoriesFor: #testPackages:!public!updating! !

RowanClassService guid: (GUID fromString: '{ab96bbd9-d725-4add-b635-94cec9f12a19}')!
RowanClassService comment: 'Don''t change the order of the inst vars without checking
the index of the meta & methods inst vars. These have
constant methods for performance. 

#indexOfMetaInstVar
#indexOfMethodsInstVar'!
!RowanClassService categoriesForClass!Kernel-Objects! !
!RowanClassService methodsFor!

= classService
	^classService isClassService
		ifTrue: [name asString = classService name asString and: [meta isNil or: [classService meta = meta]]]
		ifFalse: [^false]!

addHierarchyService: service to: treeModel withParent: parentService
	treeModel getNodeFor: service ifAbsent: [treeModel add: service asChildOf: parentService].
	(hierarchyServices at: service ifAbsent: [Array new]) do: 
			[:classService |
			self
				addHierarchyService: classService
				to: treeModel
				withParent: service].
	service hierarchyServices ifNil:[^self].
	(service hierarchyServices at: #expand ifAbsent: [^self]) do: 
			[:classService |
			self
				addHierarchyService: classService
				to: treeModel
				withParent: service]!

addSorted: sortedAdditions to: theModel
	theModel beSorted addAll: sortedAdditions!

basicPrepareForReplication
	"don't call anything potentially recursive here.
	method & package services don't iterate over subcollections"
	methods := Array new. 
	hierarchyServices := Array new.
	selectedMethods := Array new. 
	visibleTests := Array new. 
	methods ifNotNil: [methods do: [:methodService | methodService basicPrepareForReplication]].
	selectedMethods
		ifNotNil: [selectedMethods do: [:methodService | methodService basicPrepareForReplication]].
	selectedPackageServices do: [:packageService | packageService basicPrepareForReplication]!

behaviorIdentifier
	"for old school stuff"

	^(WriteStream on: String new)
		nextPutAll: name;
		tab;
		nextPutAll: oop printString;
		tab;
		nextPutAll: name printString;
		contents!

categories
	^categories!

classHierarchyUpdate: presenter browser: browser
	| treeModel subclasses parent |
	hierarchyServices ifNil: [^self].
	browser isClassListTabSelected ifTrue: [^self].
	(browser isNoneProjectSelected not and: [browser packageListPresenter selections isEmpty])
		ifTrue: [^self].
	(browser isNoneProjectSelected and: [browser dictionaryPresenter selections isEmpty])
		ifTrue: [^self].
	(browser classHierarchyPresenter model notEmpty
		and: [(browser classHierarchyPresenter model includes: self) not]) ifTrue: [^self].
	presenter selections notEmpty ifTrue: [(presenter selections includes: self) ifFalse: [^self]].
	treeModel := TreeModel new
				searchPolicy: SearchPolicy equality;
				reset.
	parent := nil.
	subclasses := hierarchyServices at: #nil ifAbsent: [].
	subclasses
		ifNil: 
			[| subs |
			parent := self.
			treeModel := presenter model.
			subs := hierarchyServices at: #expand.
			1 to: subs size
				do: 
					[:index |
					| classService node |
					classService := subs at: index.
					node := treeModel getNodeFor: classService ifAbsent: [].
					node
						ifNotNil: 
							[treeModel remove: node object ifAbsent: [].
							subs at: index put: classService]].
			subclasses := subs].
	subclasses do: 
			[:classService |
			self
				addHierarchyService: classService
				to: treeModel
				withParent: parent].
	presenter model asArray = treeModel asArray ifTrue: [^self].	"no need for update"
	treeModel asBag
		do: [:classService | classService selectedPackageServices: browser packageListPresenter selections browser: browser].
	presenter model: treeModel.
	presenter view updateMode: #lazy.	"for big hierarchies, faster display"
	presenter view disableRedraw.
	[presenter view expandAll] ensure: [presenter view enableRedraw].
	presenter selectionIfNone: [^presenter view ensureItemVisible: treeModel roots first].
	presenter view ensureSelectionVisible.
	presenter view updateMode: #dynamic!

classMethodsUpdate: presenter browser: browser
	self = browser selectedClass ifFalse: [^self].
	browser methodsUpdate: self!

classOrHierarchyPresenter: browser
	^browser isClassListTabSelected
		ifTrue: [browser classListPresenter]
		ifFalse: [browser classHierarchyPresenter]!

comment
	^comment!

comment: anObject
	comment := anObject!

computeVisibleMethods: presenter browser: browser
	| visibleMethods |
	filters := browser selectedFilters.
	visibleMethods := filters isEmpty ifTrue: [methods] ifFalse: [self filterMethods: browser].
	visibleMethods
		do: [:svc | svc inSelectedPackage: (self selectedPackageServiceNames includes: svc packageName)].
	^visibleMethods!

definedClass
	#rowanFixMe. "looks like an update to RSR broke this. Maybe?" 
	^nil!

definedPackageName
	^definedPackageName!

definedPackageName: anObject
	definedPackageName := anObject!

dictionaryName
	^dictionaryName!

displayMethodsOn: presenter browser: browser
	| visibleMethods |
	visibleMethods := self computeVisibleMethods: presenter browser: browser.
	self updateListIn: presenter from: visibleMethods!

displayName
	"for logging. for now"

	^name!

displayString
	| displayString |
	displayString := self name ifNil: [String new]. 
	versions ~= 1
		ifTrue: 
			[displayString := displayString , ' (' , version printString , '/' , versions printString , ')'].
	isInSymbolList == false ifTrue: [displayString := displayString , ' {' , oop printString , '}'].
	^displayString!

displayStringFor: displayThing browser: browser
	(self shouldColorAsExtension: browser) ifTrue: [displayThing forecolor: Color darkMagenta].
	isInSymbolList ifFalse: [displayThing forecolor: Color darkRed].
	^self displayString!

equalBeforeRename: aClassService
	^self renamedName = aClassService name!

excludedInstVars

	^super excludedInstVars, #( 'hierarchyServices' )
!

expand
	^expand!

expand: anObject
	expand := anObject!

filterMethods: browser
	| visibleMethods |
	browser isCategoryTabSelected
		ifTrue: [visibleMethods := methods select: [:methodService | filters includes: methodService category]]
		ifFalse: 
			[visibleMethods := methods
						select: [:methodService | (filters intersection: methodService accessedInstVars) notEmpty]].
	^visibleMethods!

filters
	^filters!

filters: anObject
	filters := anObject!

filterUpdate: presenter browser: browser
	browser selectedClass ifNil: [^self].
	browser selectedClass name asString = name asString ifFalse: [^self]. 
	presenter model isEmpty ifTrue: [presenter list: SortedCollection new].
	browser isCategoryTabSelected
		ifTrue: 
			[presenter list = categories ifTrue: [^self].
			self updateCategories: presenter browser: browser]
		ifFalse: 
			[presenter list = variables ifTrue: [^self].
			self updateVariables: presenter]!

hash
	^self name hash!

hierarchyClassServiceNames
	^self hierarchyClassServices collect: [:service | service name]!

hierarchyClassServices
	"flatten the unique format that hierarchyServices is returned
	into a collection of classes found in the service"

	| classes |
	classes := Set new.
	hierarchyServices keysAndValuesDo: 
			[:anchor :chain |
			classes add: anchor.
			chain do: [:service | classes add: service]].
	^classes reject:[:service | service == #nil]. !

hierarchyServices
	^hierarchyServices!

hierarchyServices: anObject
	hierarchyServices := anObject!

indexOfMetaInstVar
	"performance enhancement"
	^17!

indexOfMethodsInstVar
	"performance enhancement"

	^25!

initialize
	
	super initialize. 
	filters := Array new. 
	selectedPackageServices := Array new. "method list browser will not have packages to select"
	isTestCase := false. 
	isNewClass := false. 
	wasRemoved := false. !

instVarNames
	^instVarNames!

isClassService

	^true!

isDefined

	^isExtension not!

isExtension

	^isExtension!

isExtension: anObject
	isExtension := anObject!

isInSymbolList
	^isInSymbolList!

isNewClass
	^isNewClass!

isNewClass: anObject
	isNewClass := anObject!

isTestCase
	^isTestCase!

meta
	^meta!

meta: anObject
	meta := anObject!

methods
	^methods!

methods: anObject
	methods := anObject!

methodsWithBreakpoints
	^methods detect: [:methodService | methodService breakPoints notEmpty]!

methodUpdate: presenter browser: anObject
	presenter list do: 
			[:methodService |
			| updatedMethodService |
			updatedMethodService := methods detect: [:newMethodService | newMethodService = methodService]
						ifNone: [].
			updatedMethodService
				ifNotNil: 
					[:service |
					methodService replicateFrom: updatedMethodService	"#replicateFrom: will ensure we only replicate the proper methods"]].
	presenter view invalidate!

moveNodeToBeChildOf: superclassService in: classHierarchyPresenter
	| superclassNode myNode |
	superclassNode := classHierarchyPresenter model getNodeFor: superclassService.
	myNode := classHierarchyPresenter model getNodeFor: self.
	((classHierarchyPresenter model childrenOfNode: superclassNode) includes: myNode)
		ifFalse: 
			[classHierarchyPresenter model move: self asChildOf: superclassService].
	classHierarchyPresenter view expand: myNode!

name
	^name!

name: aString
	name := aString asString!

oop
	^oop!

oop: anObject
	oop := anObject!

packageName
	^packageName!

packageName: anObject
	packageName := anObject!

postReload
	" don't retain reference to (possibly) different version of class"

	oop := nil!

postUpdate
	super postUpdate.
	hierarchyServices := nil.
	methods := Array new.
	renamedName := nil!

prepareForReplication
	super prepareForReplication.
	self basicPrepareForReplication!

projectName
	^projectName!

remoteServiceName
	^'Rowan classServiceClass'!

removedClass: presenter
	| removal |
	updateType == #removedClass: ifFalse: [^self].
	removal := presenter model asBag detect: [:classService | classService name = name] ifNone: [].
	removal ifNotNil: [presenter model removeWithoutNotification: removal ifAbsent: []]!

renamedClass: presenter browser: browser
	"because class service equality is name-based, 
	we can't just do a simple replication. We have to
	remove then add the class service to ensure the
	list doesn't get in trouble"

	presenter model asBag do: 
			[:classService |
			(self equalBeforeRename: classService)
				ifTrue: 
					[| wasSelected |
					wasSelected := (presenter selectionIfNone: []) = classService.
					presenter model remove: classService.
					classService basicReplicateFrom: self.
					classService renamedName: nil.
					presenter model add: classService.
					wasSelected
						ifTrue: 
							[presenter selection: classService.
							browser classDefinitionPresenter value: template.
							browser displayNoMods: browser classDefinitionPresenter]]].
	presenter view invalidate!

renamedClassInHierarchy: presenter browser: anObject
	"because class service equality is name-based, 
	we have to remove then add the class service
	to ensure the tree can find it"

	presenter model asBag do: 
			[:classService |
			(self equalBeforeRename: classService)
				ifTrue: 
					[| parent children wasSelected |
					wasSelected := (presenter selectionIfNone:[]) = classService.
					parent := presenter model parentOf: classService.
					children := presenter model getChildrenOf: classService.
					children do: [:child | presenter model remove: child ifAbsent: []].
					presenter model remove: classService ifAbsent: [].
					classService basicReplicateFrom: self.
					classService renamedName: nil.
					presenter model add: classService asChildOf: parent.
					children do: [:child | presenter model add: child asChildOf: classService].
					presenter view expand: parent.
					presenter view ensureItemVisible: classService.
					wasSelected ifTrue: [presenter selection: classService]]]!

renamedName
	^renamedName!

renamedName: anObject
	renamedName := anObject!

replicateFrom: newService
	newService isClassService
		ifTrue: [self = newService ifTrue: [^super basicReplicateFrom: newService]]!

saveMethod: source category: category session: session
	self
		command: #saveMethodSource:category:;
		commandArgs: (Array with: source with: category).
	BrowserUpdate current issueCommands: (Array with: self) session: session!

selectedMethods

	^selectedMethods
		!

selectedPackageNames
	^selectedPackageServices
		ifNil: [Array new]
		ifNotNil: [selectedPackageServices collect: [:packageService | packageService name]]!

selectedPackageServiceNames
	^selectedPackageServices collect: [:pkgService | pkgService name]!

selectedPackageServices
	^selectedPackageServices!

selectedPackageServices: collection
	selectedPackageServices := collection!

selectedPackageServices: anObject browser: browser
	"selected packages don't apply when dictionaries are showing"

	selectedPackageServices := browser isRowanProjectSelected ifTrue: [anObject] ifFalse: [Array new]!

shouldColorAsExtension: browser
	^browser isNoneProjectSelected
		ifTrue: [false]
		ifFalse: 
			[browser selectedPackageServices isEmpty
				ifTrue: [false]
				ifFalse: 
					[((browser selectedPackageServices collect: [:service | service name]) includes: definedPackageName)
						not]]!

shouldReplicateInstVarAtIndex: index newService: newService
	self indexOfMetaInstVar = index ifTrue: [^false].
	self indexOfMethodsInstVar = index ifTrue: [newService meta = meta ifFalse: [^false]].
	^true!

sortAspect

	^name!

sunitMethodsUpdate: presenter browser: browser
	| selections oldClassService |
	browser classListPresenter selections isEmpty ifTrue: [^self].
	browser classListPresenter selections size > 1
		ifFalse: [browser classListPresenter selection name asString = name asString ifFalse: [^self]].
	presenter list isEmpty ifTrue: [self initializePresenterList: presenter].
	selections := browser classListPresenter selections asOrderedCollection.
	oldClassService := selections detect: [:classService | classService = self] ifNone: [^self].
	oldClassService replicateFrom: self. 
	browser updateMethodsFrom: selections!

template
	^template!

template: anObject
	template := anObject!

toolTip
	^'Defined package: ' , self definedPackageName!

updateAfterCommand: boolean

	updateAfterCommand := boolean!

updateCategories: presenter browser: browser
	"Update without losing selections. If no selections, 
	just update a copy then replace the original."

	| theModel |
	meta = browser isClassSideSelected ifFalse: [^self].
	theModel := presenter hasSelection ifTrue: [presenter model] ifFalse: [presenter model copy].
	presenter list copy
		do: [:category | (categories includes: category) ifFalse: [theModel remove: category]].
	categories do: [:category | (presenter list includes: category) ifFalse: [theModel add: category]].
	presenter hasSelection ifFalse: [presenter model: theModel]!

updatedClass: aPresenter browser: browser
	browser updateClass: self!

updatedClassDefinition: classDefinitionPresenter browser: browser
	(browser selectedClass = self or: [self equalBeforeRename: browser selectedClass])
		ifTrue: 
			[classDefinitionPresenter view isModified
				ifFalse: 
					[classDefinitionPresenter value: template.
					browser displayNoMods: classDefinitionPresenter]]!

updatedClassInHierarchy: classHierarchyPresenter browser: projectBrowser
	"a newly added class should show up in the list but selection (say) of an existing class
	should in another browser that isn't in the displayed hierarchy but exists in the full 
	hierarchy should not"

	| classNode superclassService selectedPackageNames |
	classHierarchyPresenter view invalidate.
	classNode := classHierarchyPresenter model getNodeFor: self ifAbsent: [isNewClass ifFalse: [^self]].
	superclassService := classHierarchyPresenter model asBag
				detect: [:classService | classService name = superclassName]
				ifNone: [^self].
	(classNode notNil and: [classNode object isNewClass not])
		ifTrue: [^self moveNodeToBeChildOf: superclassService in: classHierarchyPresenter].
	projectBrowser packageListPresenter selections isEmpty ifTrue: [^self].
	selectedPackageNames := projectBrowser packageListPresenter selections
				collect: [:packageService | packageService name].
	(selectedPackageNames includes: packageName) ifFalse: [^self].
	classNode
		ifNil: [classHierarchyPresenter model add: self asChildOf: superclassService]
		ifNotNil: 
			[:node |
			node object replicateFrom: self.
			classHierarchyPresenter model move: node object asChildOf: superclassService]!

updateListIn: presenter from: visibleMethods
	| updated theModel toRemove toAdd |
	theModel := presenter hasSelection
				ifTrue: 
					[(visibleMethods includesAnyOf: presenter selections)
						ifTrue: [presenter model]
						ifFalse: [presenter model copy]]
				ifFalse: [presenter model copy].
	toRemove := OrderedCollection new.
	presenter list copy do: 
			[:old |
			updated := visibleMethods detect: [:new | new = old] ifNone: [].
			updated ifNil: [toRemove add: old] ifNotNil: [old replicateFrom: updated]].
	theModel removeAll: toRemove.
	toAdd := SortedCollection new sortBlock: [:x :y | x selector < y selector].
	visibleMethods do: 
			[:new |
			updated := theModel detect: [:old | new = old] ifNone: [].
			updated ifNil: [toAdd add: new]].
	self addSorted: toAdd to: theModel.
	(presenter parentPresenter class canUnderstand: #updateSUnitTab)
		ifTrue: [presenter parentPresenter updateSUnitTab].
	theModel == presenter model ifFalse: [presenter model: theModel]!

updateVariables: presenter
	variables ifNil: [^self].
	presenter list: variables. !

variables
	^variables!

version
	"Private - for testing"
	^version!

versions
	"Private - for testing"
	^versions!

visibleTests
	^visibleTests ifNil: [Array new]!

wasRemoved
	^wasRemoved == true!

wasRemoved: anObject
	wasRemoved := anObject!

wasRenamed

	^renamedName notNil! !
!RowanClassService categoriesFor: #=!comparing!public! !
!RowanClassService categoriesFor: #addHierarchyService:to:withParent:!private!updating support! !
!RowanClassService categoriesFor: #addSorted:to:!private!updating support! !
!RowanClassService categoriesFor: #basicPrepareForReplication!public!replication! !
!RowanClassService categoriesFor: #behaviorIdentifier!accessing!public! !
!RowanClassService categoriesFor: #categories!accessing!private! !
!RowanClassService categoriesFor: #classHierarchyUpdate:browser:!public!updating! !
!RowanClassService categoriesFor: #classMethodsUpdate:browser:!public!updating! !
!RowanClassService categoriesFor: #classOrHierarchyPresenter:!private!updating support! !
!RowanClassService categoriesFor: #comment!accessing!public! !
!RowanClassService categoriesFor: #comment:!accessing!public! !
!RowanClassService categoriesFor: #computeVisibleMethods:browser:!private!updating support! !
!RowanClassService categoriesFor: #definedClass!accessing!private! !
!RowanClassService categoriesFor: #definedPackageName!accessing!private! !
!RowanClassService categoriesFor: #definedPackageName:!accessing!private! !
!RowanClassService categoriesFor: #dictionaryName!accessing!private! !
!RowanClassService categoriesFor: #displayMethodsOn:browser:!public!updating! !
!RowanClassService categoriesFor: #displayName!displaying!public! !
!RowanClassService categoriesFor: #displayString!displaying!public! !
!RowanClassService categoriesFor: #displayStringFor:browser:!displaying!public! !
!RowanClassService categoriesFor: #equalBeforeRename:!comparing!public! !
!RowanClassService categoriesFor: #excludedInstVars!public!ston! !
!RowanClassService categoriesFor: #expand!accessing!private! !
!RowanClassService categoriesFor: #expand:!accessing!private! !
!RowanClassService categoriesFor: #filterMethods:!private!updating support! !
!RowanClassService categoriesFor: #filters!accessing!public! !
!RowanClassService categoriesFor: #filters:!accessing!public! !
!RowanClassService categoriesFor: #filterUpdate:browser:!public!updating! !
!RowanClassService categoriesFor: #hash!comparing!public! !
!RowanClassService categoriesFor: #hierarchyClassServiceNames!public! !
!RowanClassService categoriesFor: #hierarchyClassServices!public! !
!RowanClassService categoriesFor: #hierarchyServices!accessing!private! !
!RowanClassService categoriesFor: #hierarchyServices:!accessing!private! !
!RowanClassService categoriesFor: #indexOfMetaInstVar!constants!public! !
!RowanClassService categoriesFor: #indexOfMethodsInstVar!constants!public! !
!RowanClassService categoriesFor: #initialize!Init / Release!public! !
!RowanClassService categoriesFor: #instVarNames!accessing!public! !
!RowanClassService categoriesFor: #isClassService!public!testing! !
!RowanClassService categoriesFor: #isDefined!public!testing! !
!RowanClassService categoriesFor: #isExtension!public!testing! !
!RowanClassService categoriesFor: #isExtension:!accessing!public! !
!RowanClassService categoriesFor: #isInSymbolList!accessing!private! !
!RowanClassService categoriesFor: #isNewClass!accessing!private! !
!RowanClassService categoriesFor: #isNewClass:!accessing!private! !
!RowanClassService categoriesFor: #isTestCase!public!testing! !
!RowanClassService categoriesFor: #meta!accessing!private! !
!RowanClassService categoriesFor: #meta:!accessing!private! !
!RowanClassService categoriesFor: #methods!accessing!private! !
!RowanClassService categoriesFor: #methods:!accessing!private! !
!RowanClassService categoriesFor: #methodsWithBreakpoints!public!testing! !
!RowanClassService categoriesFor: #methodUpdate:browser:!public!updating! !
!RowanClassService categoriesFor: #moveNodeToBeChildOf:in:!private!updating support! !
!RowanClassService categoriesFor: #name!accessing!public! !
!RowanClassService categoriesFor: #name:!accessing!public! !
!RowanClassService categoriesFor: #oop!accessing!private! !
!RowanClassService categoriesFor: #oop:!accessing!private! !
!RowanClassService categoriesFor: #packageName!accessing!private! !
!RowanClassService categoriesFor: #packageName:!accessing!private! !
!RowanClassService categoriesFor: #postReload!public!replication! !
!RowanClassService categoriesFor: #postUpdate!Init / Release!public! !
!RowanClassService categoriesFor: #prepareForReplication!public!replication! !
!RowanClassService categoriesFor: #projectName!accessing!private! !
!RowanClassService categoriesFor: #remoteServiceName!must not strip!public! !
!RowanClassService categoriesFor: #removedClass:!public!updating! !
!RowanClassService categoriesFor: #renamedClass:browser:!public!updating! !
!RowanClassService categoriesFor: #renamedClassInHierarchy:browser:!public!updating! !
!RowanClassService categoriesFor: #renamedName!accessing!private! !
!RowanClassService categoriesFor: #renamedName:!accessing!private! !
!RowanClassService categoriesFor: #replicateFrom:!public!replication! !
!RowanClassService categoriesFor: #saveMethod:category:session:!actions!public! !
!RowanClassService categoriesFor: #selectedMethods!accessing!private! !
!RowanClassService categoriesFor: #selectedPackageNames!private! !
!RowanClassService categoriesFor: #selectedPackageServiceNames!private!updating support! !
!RowanClassService categoriesFor: #selectedPackageServices!accessing!private! !
!RowanClassService categoriesFor: #selectedPackageServices:!accessing!private! !
!RowanClassService categoriesFor: #selectedPackageServices:browser:!accessing!private! !
!RowanClassService categoriesFor: #shouldColorAsExtension:!public!testing! !
!RowanClassService categoriesFor: #shouldReplicateInstVarAtIndex:newService:!public!testing! !
!RowanClassService categoriesFor: #sortAspect!accessing!public! !
!RowanClassService categoriesFor: #sunitMethodsUpdate:browser:!public!updating! !
!RowanClassService categoriesFor: #template!accessing!private! !
!RowanClassService categoriesFor: #template:!accessing!private! !
!RowanClassService categoriesFor: #toolTip!public! !
!RowanClassService categoriesFor: #updateAfterCommand:!accessing!public! !
!RowanClassService categoriesFor: #updateCategories:browser:!private!updating support! !
!RowanClassService categoriesFor: #updatedClass:browser:!public!updating! !
!RowanClassService categoriesFor: #updatedClassDefinition:browser:!public!updating! !
!RowanClassService categoriesFor: #updatedClassInHierarchy:browser:!public!updating! !
!RowanClassService categoriesFor: #updateListIn:from:!private!updating support! !
!RowanClassService categoriesFor: #updateVariables:!private!updating support! !
!RowanClassService categoriesFor: #variables!accessing!private! !
!RowanClassService categoriesFor: #version!accessing!private! !
!RowanClassService categoriesFor: #versions!accessing!private! !
!RowanClassService categoriesFor: #visibleTests!accessing!private! !
!RowanClassService categoriesFor: #wasRemoved!accessing!private! !
!RowanClassService categoriesFor: #wasRemoved:!accessing!private! !
!RowanClassService categoriesFor: #wasRenamed!public!testing! !

!RowanClassService class methodsFor!

defaultIconName
	"Answer a filename to use for an icon of this class."

	^File composeStem: 'Behavior' extension: 'ico'.!

icon
	"Answers an Icon that can be used to represent this class"

	^##(self) defaultIcon!

named: theName
	| inst |
	inst := self new name: theName.
	^inst! !
!RowanClassService class categoriesFor: #defaultIconName!private! !
!RowanClassService class categoriesFor: #icon!private! !
!RowanClassService class categoriesFor: #named:!instance creation!public! !

RowanComponentService guid: (GUID fromString: '{842be73b-a371-499f-a6d4-dcd4a81c01e5}')!
RowanComponentService comment: ''!
!RowanComponentService categoriesForClass!Unclassified! !
!RowanComponentService methodsFor!

= componentService
	^componentService isService
		and: [componentService isComponentService and: [name = componentService name]]!

basename
	^basename!

basename: anObject
	basename := anObject!

componentPackagesUpdate: presenter browser: browser
	wasUpdated ifFalse:[^self].
	packageServices isEmpty ifTrue:[^self]. 
	self
		packagesUpdate: presenter
		browser: browser
		parentPresenter: browser componentListPresenter!

displayString
	^basename!

hash
	^self name hash!

isComponentService
	^true!

name
	^name!

packageServices
	^packageServices!

printOn: stream
	stream
		nextPutAll: self class name;
		nextPutAll: '->';
		nextPutAll: basename! !
!RowanComponentService categoriesFor: #=!comparing!public! !
!RowanComponentService categoriesFor: #basename!accessing!private! !
!RowanComponentService categoriesFor: #basename:!accessing!private! !
!RowanComponentService categoriesFor: #componentPackagesUpdate:browser:!public!updating! !
!RowanComponentService categoriesFor: #displayString!displaying!public! !
!RowanComponentService categoriesFor: #hash!comparing!public! !
!RowanComponentService categoriesFor: #isComponentService!public!testing! !
!RowanComponentService categoriesFor: #name!accessing!public! !
!RowanComponentService categoriesFor: #packageServices!accessing!private! !
!RowanComponentService categoriesFor: #printOn:!displaying!public! !

RowanDebuggerService guid: (GUID fromString: '{d8a038cb-84e2-4e03-bff9-1dc3bf097d57}')!
RowanDebuggerService comment: ''!
!RowanDebuggerService categoriesForClass!Kernel-Objects! !
!RowanDebuggerService methodsFor!

displayName
	"for logging. for now"

	^name!

initialize: aGsProcess

	initialProcessOop := aGsProcess oopType value.
!

name
	^name!

name: aString
	name := aString asString!

processes
	^processes!

processListUpdate: aPresenter
	aPresenter parentPresenter gsProcess ifNil:[^self]. 
	aPresenter parentPresenter gsProcess oop = processes first oop ifFalse: [^self].
	aPresenter list: processes!

sortAspect

	^name! !
!RowanDebuggerService categoriesFor: #displayName!displaying!public! !
!RowanDebuggerService categoriesFor: #initialize:!public! !
!RowanDebuggerService categoriesFor: #name!accessing!public! !
!RowanDebuggerService categoriesFor: #name:!accessing!public! !
!RowanDebuggerService categoriesFor: #processes!accessing!public! !
!RowanDebuggerService categoriesFor: #processListUpdate:!Debugger!public!updating! !
!RowanDebuggerService categoriesFor: #sortAspect!accessing!public! !

!RowanDebuggerService class methodsFor!

onProcess: aGsProcess

	^self basicNew
		initialize: aGsProcess;
		yourself! !
!RowanDebuggerService class categoriesFor: #onProcess:!public! !

RowanDictionaryService guid: (GUID fromString: '{4b891434-003f-48d1-a7d3-b09dfd2d390f}')!
RowanDictionaryService comment: ''!
!RowanDictionaryService categoriesForClass!Kernel-Objects! !
!RowanDictionaryService methodsFor!

= dictionaryService
	^dictionaryService isDictionaryService ifTrue: [name = dictionaryService name] ifFalse: [^false]!

basicPrepareForReplication
	super basicPrepareForReplication.
	classes := nil. 
	hierarchyServices := nil.!

classes

	^classes!

classes: anArray
	classes := anArray!

classesUpdate: presenter browser: browser
	| updatedClasses |
	presenter list isEmpty ifTrue: [self initializePresenterList: presenter].
	(self doesUpdateApply: browser) ifFalse: [^self].
	self removeDeletedClassesIn: presenter browser: browser.
	presenter selectionOrNil
		ifNil: 
			[updatedClasses := classes asSet reject: [:classService | classService wasRenamed].
			updatedClasses addAll: presenter list.
			presenter list: (ListModel withAll: updatedClasses)]
		ifNotNil: 
			[self
				updateList: presenter
				whilePreservingSelections: classes
				browser: browser].
	presenter list
		do: [:classService | classService selectedPackageServices: browser packageListPresenter selections browser: browser].
	presenter view invalidate.
	browser isClassSelected ifFalse: [self emptyFilterListsIn: browser]!

classHierarchyUpdate: presenter browser: browser
	browser isHierarchyTabSelected ifFalse: [^false].
	browser dictionaryPresenter selectionOrNil
		ifNil: [^self]
		ifNotNil: [:dictionaryService | dictionaryService name = name ifFalse: [^self]].
	self
		classHierarchyUpdate: presenter
		browser: browser
		hierarchyServices: hierarchyServices!

defaultTemplate
	^defaultTemplate!

defaultTemplate: anObject
	defaultTemplate := anObject!

displayName
	"for logging. for now"

	^name!

displayString
	^self name!

displayStringFor: displayThing
	displayThing forecolor: Color black!

doesUpdateApply: browser
	^browser isNoneProjectSelected
		ifTrue: [browser dictionaryPresenter selections includes: self]
		ifFalse: [false]!

globals
	^globals!

globals: anObject
	globals := anObject!

hash
	^self name hash!

isDictionaryService

	^true!

name
	^name!

name: aString
	name := aString asString!

postUpdate
	super postUpdate.
	hierarchyServices
		ifNotNil: 
			[hierarchyServices keysAndValuesDo: 
					[:key :value |
					key isBehavior ifTrue: [key postUpdate].
					value do: [:service | service postUpdate]]].
	hierarchyServices := nil.
	classes ifNotNil: [classes do: [:service | service postUpdate]].
	classes := Array new!

prepareForReplication
	super prepareForReplication.
	classes := nil. 
	hierarchyServices := nil.!

removeDeletedClassesIn: presenter browser: browser
	^super
		removeDeletedClassesIn: presenter
		browser: browser
		classes: classes!

sortAspect

	^name! !
!RowanDictionaryService categoriesFor: #=!comparing!public! !
!RowanDictionaryService categoriesFor: #basicPrepareForReplication!public!replication! !
!RowanDictionaryService categoriesFor: #classes!accessing!public! !
!RowanDictionaryService categoriesFor: #classes:!accessing!public! !
!RowanDictionaryService categoriesFor: #classesUpdate:browser:!public!updating! !
!RowanDictionaryService categoriesFor: #classHierarchyUpdate:browser:!public!updating! !
!RowanDictionaryService categoriesFor: #defaultTemplate!accessing!private! !
!RowanDictionaryService categoriesFor: #defaultTemplate:!accessing!private! !
!RowanDictionaryService categoriesFor: #displayName!displaying!public! !
!RowanDictionaryService categoriesFor: #displayString!displaying!public! !
!RowanDictionaryService categoriesFor: #displayStringFor:!displaying!public! !
!RowanDictionaryService categoriesFor: #doesUpdateApply:!private!testing! !
!RowanDictionaryService categoriesFor: #globals!accessing!public! !
!RowanDictionaryService categoriesFor: #globals:!accessing!public! !
!RowanDictionaryService categoriesFor: #hash!comparing!public! !
!RowanDictionaryService categoriesFor: #isDictionaryService!public!testing! !
!RowanDictionaryService categoriesFor: #name!accessing!public! !
!RowanDictionaryService categoriesFor: #name:!accessing!public! !
!RowanDictionaryService categoriesFor: #postUpdate!Init / Release!public! !
!RowanDictionaryService categoriesFor: #prepareForReplication!public!replication! !
!RowanDictionaryService categoriesFor: #removeDeletedClassesIn:browser:!private! !
!RowanDictionaryService categoriesFor: #sortAspect!accessing!public! !

!RowanDictionaryService class methodsFor!

defaultIconName
	"Answer a filename to use for an icon of this class."

	^File composeStem: 'Dictionary' extension: 'ico'.!

icon
	"Answers an Icon that can be used to represent this class"

	^##(self) defaultIcon! !
!RowanDictionaryService class categoriesFor: #defaultIconName!private! !
!RowanDictionaryService class categoriesFor: #icon!private! !

RowanFrameService guid: (GUID fromString: '{65407af5-8317-4fa0-a86d-7b9543e85cd0}')!
RowanFrameService comment: ''!
!RowanFrameService categoriesForClass!Kernel-Objects! !
!RowanFrameService methodsFor!

breaks

	^method breakPoints!

classIsResolvable
	^classIsResolvable!

copyBasicsFrom: oldService

	method := RowanMethodService new copyBasicsFrom: oldService method.
!

displayName
	"for logging. for now"

	^name!

gsMethod

	^method!

homeMethodClassName

	^homeMethodClassName!

homeMethodSelector

	^homeMethodSelector!

indexOfSubCollection: aString 

	^method source indexOfSubCollection: aString !

isExecutedBlockContext

	^method className isNil!

isUpdatableService
	^false!

label

	^label!

method
	^method!

name
	^name!

name: aString
	name := aString asString!

offsets

	^method stepPoints collect: [:each | each key start]!

oop
	^oop!

oop: anObject
	oop := anObject!

printOn: aStream

	aStream nextPutAll: (label ifNil: ['?'])
!

sortAspect

	^name!

source

	^method source!

stepPoint
	^stepPoint!

vars
	^vars! !
!RowanFrameService categoriesFor: #breaks!public! !
!RowanFrameService categoriesFor: #classIsResolvable!accessing!private! !
!RowanFrameService categoriesFor: #copyBasicsFrom:!public! !
!RowanFrameService categoriesFor: #displayName!displaying!public! !
!RowanFrameService categoriesFor: #gsMethod!public! !
!RowanFrameService categoriesFor: #homeMethodClassName!accessing!public! !
!RowanFrameService categoriesFor: #homeMethodSelector!accessing!public! !
!RowanFrameService categoriesFor: #indexOfSubCollection:!public! !
!RowanFrameService categoriesFor: #isExecutedBlockContext!public!testing! !
!RowanFrameService categoriesFor: #isUpdatableService!public!testing! !
!RowanFrameService categoriesFor: #label!accessing!public! !
!RowanFrameService categoriesFor: #method!public! !
!RowanFrameService categoriesFor: #name!accessing!public! !
!RowanFrameService categoriesFor: #name:!accessing!public! !
!RowanFrameService categoriesFor: #offsets!public! !
!RowanFrameService categoriesFor: #oop!accessing!private! !
!RowanFrameService categoriesFor: #oop:!accessing!private! !
!RowanFrameService categoriesFor: #printOn:!public! !
!RowanFrameService categoriesFor: #sortAspect!accessing!public! !
!RowanFrameService categoriesFor: #source!public! !
!RowanFrameService categoriesFor: #stepPoint!public! !
!RowanFrameService categoriesFor: #vars!public! !

RowanInspectorService guid: (GUID fromString: '{b80c7b87-3ce0-4b36-a288-1e52506958dd}')!
RowanInspectorService comment: ''!
!RowanInspectorService categoriesForClass!Unclassified! !
!RowanInspectorService methodsFor!

= inspectorService
	^inspectorService isInspectorService ifTrue: [oop = inspectorService oop] ifFalse: [false]!

className
	^className!

className: anObject
	className := anObject!

compileErrorArray
	^compileErrorArray!

compileErrorArray: anObject
	compileErrorArray := anObject!

defaultIndexedSize
	^1000!

displayName
	"for logging. for now"

	^name!

executeThenInspect: string context: anOop session: session windowHandle: handle
	self
		command: #executeThenInspect:context:inWindow:;
		commandArgs: (Array
					with: string
					with: anOop
					with: handle).
	BrowserUpdate current issueCommands: (Array with: self) session: session!

executeThenInspect: string context: anOop session: session windowHandle: handle debugger: debugger
	self
		command: #executeThenInspect:inFrame:process:context:inWindow:;
		commandArgs: (Array
					with: string
					with: debugger frameListPresenter selectionByIndex
					with: debugger gsProcess oop
					with: anOop
					with: handle).
	BrowserUpdate current issueCommands: (Array with: self) session: session!

hash
	^oop hash!

indexedSize
	^indexedSize!

indexedSize: anObject
	indexedSize := anObject!

initialize
	super initialize. 
	objects := OrderedCollection new. 
	maxIndexedVars := self defaultIndexedSize.
	indexedSize := 0.
	isOop := true. 
	isDictionary := false.
	isVariable := false. !

inspect: oopOrObject session: session
	oop := (oopOrObject isKindOf: ExternalInteger)
				ifTrue: [oopOrObject value]
				ifFalse: 
					[isOop := false.
					oopOrObject].
	self
		command: #inspect:;
		commandArgs: (Array with: oop).
	BrowserUpdate current issueCommands: (Array with: self) session: session!

inspect: oopOrObject session: session inWindow: handle
	oop := (oopOrObject isKindOf: ExternalInteger)
				ifTrue: [oopOrObject value]
				ifFalse: 
					[isOop := false.
					oopOrObject].
	self
		command: #inspect:inWindow:;
		commandArgs: (Array with: oop with: handle).
	BrowserUpdate current issueCommands: (Array with: self) session: session!

instVarNames
	^instVarNames!

instVarNames: anObject
	instVarNames := anObject!

instVarsAreRemovable
	^instVarsAreRemovable!

instVarsAreRemovable: anObject
	instVarsAreRemovable := anObject!

isDictionary
	^isDictionary!

isInspectorService

	^true!

isShowingAllIndexedVars

	^visibleIndices = indexedSize!

isUnordered
	^isUnordered!

isUnordered: anObject
	isUnordered := anObject!

isVariable
	^isVariable!

maxIndexedVars
	^maxIndexedVars!

maxIndexedVars: anObject
	maxIndexedVars := anObject!

myself
	^myself!

myself: anObject
	myself := anObject!

name
	^name!

name: aString
	name := aString asString!

nextIndexedVarsFrom: indexStart to: indexStop session: session
	self
		command: #nextIndexedVarsFrom:to:;
		commandArgs: (Array with: indexStart with: indexStop).
	BrowserUpdate current issueCommands: (Array with: self) session: session.
	^nextIndices!

nextIndices
	^nextIndices!

nextIndices: anObject
	nextIndices := anObject!

objects
	^objects!

objects: anObject
	objects := anObject!

oop
	^oop!

oop: anObject
	oop := anObject!

removeDynamicInstVarsNamed: dynamicInstVarNames session: session
	self
		command: #removeDynamicInstVars:;
		commandArgs: (Array with: dynamicInstVarNames).
	BrowserUpdate current issueCommands: (Array with: self) session: session!

removeIndexedInstVarsAt: indices session: session
	self
		command: #removeIndexedInstVarsAt:;
		commandArgs: (Array with: indices).
	BrowserUpdate current issueCommands: (Array with: self) session: session!

removeKeys: keyOops session: session
	self
		command: #removeKeys:;
		commandArgs: (Array with: keyOops).
	BrowserUpdate current issueCommands: (Array with: self) session: session!

removeOop: theOop session: session
	self
		command: #removeOop:;
		commandArgs: (Array with: theOop).
	BrowserUpdate current issueCommands: (Array with: self) session: session!

selectionOop
	^selectionOop!

sortAspect

	^name!

statusText
	^statusText!

statusText: anObject
	statusText := anObject!

visibleIndices
	^visibleIndices!

visibleIndices: anObject
	visibleIndices := anObject! !
!RowanInspectorService categoriesFor: #=!comparison!public! !
!RowanInspectorService categoriesFor: #className!accessing!private! !
!RowanInspectorService categoriesFor: #className:!accessing!private! !
!RowanInspectorService categoriesFor: #compileErrorArray!accessing!private! !
!RowanInspectorService categoriesFor: #compileErrorArray:!accessing!private! !
!RowanInspectorService categoriesFor: #defaultIndexedSize!constants!public! !
!RowanInspectorService categoriesFor: #displayName!displaying!public! !
!RowanInspectorService categoriesFor: #executeThenInspect:context:session:windowHandle:!inspecting!public! !
!RowanInspectorService categoriesFor: #executeThenInspect:context:session:windowHandle:debugger:!inspecting!public! !
!RowanInspectorService categoriesFor: #hash!comparison!private! !
!RowanInspectorService categoriesFor: #indexedSize!accessing!private! !
!RowanInspectorService categoriesFor: #indexedSize:!accessing!private! !
!RowanInspectorService categoriesFor: #initialize!initialization!public! !
!RowanInspectorService categoriesFor: #inspect:session:!inspecting!public! !
!RowanInspectorService categoriesFor: #inspect:session:inWindow:!inspecting!public! !
!RowanInspectorService categoriesFor: #instVarNames!accessing!private! !
!RowanInspectorService categoriesFor: #instVarNames:!accessing!private! !
!RowanInspectorService categoriesFor: #instVarsAreRemovable!accessing!private! !
!RowanInspectorService categoriesFor: #instVarsAreRemovable:!accessing!private! !
!RowanInspectorService categoriesFor: #isDictionary!accessing!public! !
!RowanInspectorService categoriesFor: #isInspectorService!public!testing! !
!RowanInspectorService categoriesFor: #isShowingAllIndexedVars!public!testing! !
!RowanInspectorService categoriesFor: #isUnordered!accessing!private! !
!RowanInspectorService categoriesFor: #isUnordered:!accessing!private! !
!RowanInspectorService categoriesFor: #isVariable!accessing!private! !
!RowanInspectorService categoriesFor: #maxIndexedVars!accessing!private! !
!RowanInspectorService categoriesFor: #maxIndexedVars:!accessing!private! !
!RowanInspectorService categoriesFor: #myself!accessing!private! !
!RowanInspectorService categoriesFor: #myself:!accessing!private! !
!RowanInspectorService categoriesFor: #name!accessing!public! !
!RowanInspectorService categoriesFor: #name:!accessing!public! !
!RowanInspectorService categoriesFor: #nextIndexedVarsFrom:to:session:!inspecting!public! !
!RowanInspectorService categoriesFor: #nextIndices!accessing!private! !
!RowanInspectorService categoriesFor: #nextIndices:!accessing!private! !
!RowanInspectorService categoriesFor: #objects!accessing!private! !
!RowanInspectorService categoriesFor: #objects:!accessing!private! !
!RowanInspectorService categoriesFor: #oop!accessing!private! !
!RowanInspectorService categoriesFor: #oop:!accessing!private! !
!RowanInspectorService categoriesFor: #removeDynamicInstVarsNamed:session:!operations!public! !
!RowanInspectorService categoriesFor: #removeIndexedInstVarsAt:session:!operations!public! !
!RowanInspectorService categoriesFor: #removeKeys:session:!operations!public! !
!RowanInspectorService categoriesFor: #removeOop:session:!operations!public! !
!RowanInspectorService categoriesFor: #selectionOop!accessing!private! !
!RowanInspectorService categoriesFor: #sortAspect!accessing!public! !
!RowanInspectorService categoriesFor: #statusText!accessing!private! !
!RowanInspectorService categoriesFor: #statusText:!accessing!private! !
!RowanInspectorService categoriesFor: #visibleIndices!accessing!private! !
!RowanInspectorService categoriesFor: #visibleIndices:!accessing!private! !

RowanLoggingService guid: (GUID fromString: '{869571d2-e7e2-4517-b2af-6c37a999c7cd}')!
RowanLoggingService comment: 'Any service sent from the client gives it''s group id to the client send, server receive, server return, client receive. 
id is the numbering within that group for display sorting. '!
!RowanLoggingService categoriesForClass!Unclassified! !
!RowanLoggingService methodsFor!

= methodService
	(methodService isKindOf: RowanService) ifFalse: [^false].
	^methodService isLoggingService and: [groupId = methodService groupId and: [id = methodService id]]!

addServices: newServices
	services addAll: newServices!

clientDisplayString
	^location == #client ifTrue: [self displayString] ifFalse: [String new]!

clientGroupIdDisplayString

	^groupId printString!

clientIdDisplayString

	^id printString!

comment
	^comment!

comment: anObject
	comment := anObject!

date
	^date!

date: anObject
	date := anObject!

displayCommand
	| ws |
	ws := WriteStream on: String new.
	services do: 
			[:service |
			service command
				ifNil: 
					[ws nextPutAll: (service updateType ifNil: [String new] ifNotNil: [:updates | updates printString])]
				ifNotNil: 
					[ws nextPutAll: (service command ifNil: [String new] ifNotNil: [:updates | updates printString])].
			ws
				space;
				nextPutAll: (service displayName ifNil: [String new])].
	^ws contents!

displayName
	"for logging. for now"

	^name!

displayString
	| ws |
	ws := WriteStream on: String new.
	ws
		nextPutAll: date shortString;
		space.
	time printOn: ws format: 'HH:mm:ss'.
	comment
		ifNotNil: 
			[ws
				space;
				nextPutAll: comment].
	mode
		ifNotNil: 
			[ws
				space;
				nextPutAll: mode;
				space;
				nextPut: $-;
				space;
				nextPutAll: self displayCommand].
	^ws contents!

fileName
	^fileName!

fileName: anObject
	fileName := anObject!

groupId
	^groupId!

groupId: anObject
	groupId := anObject!

hash
	^groupId hash bitXor: id hash!

id
	^id!

id: anObject
	id := anObject!

initialize
	super initialize.
	services := OrderedCollection new.
	isLogging := false. !

isLogging
	^isLogging!

isLogging: aBoolean 
	isLogging := aBoolean!

isLoggingService

	^true!

isTestLoggingService
	^false!

location
	^location!

location: anObject
	location := anObject!

logComment: aString
	| stonString ws |
	id := id + 1.
	comment := aString.
	date := Date today. 
	time := Time now.
	location := #client.
	mode := nil.
	stonString := STON toString: self.
	ws := FileStream write: fileName mode: #append.
	[ws nextPutAll: stonString] ensure: [ws close].
	comment := nil	"service may be reused. Clear comment"!

logReceivedServices: theServices
	mode := #received.
	self logServices: theServices!

logSentServices: theServices
	mode := #sent.
	self logServices: theServices!

logServices: theServices
	| stonString ws |
	id := id + 1.
	date := Date today.
	time := Time now.
	location := #client.
	stonString := STON toString: self.
	ws := FileStream write: fileName mode: #append.
	[ws nextPutAll: stonString] ensure: [ws close]!

mode
	^mode!

name
	^name!

name: aString
	name := aString asString!

printString
	^self class name , ' ' , self displayString!

serverDisplayString
	^location == #server ifTrue: [self displayString] ifFalse: [String new]!

services

	^services!

sortAspect

	^name!

time
	^time!

time: anObject
	time := anObject! !
!RowanLoggingService categoriesFor: #=!comparing!public! !
!RowanLoggingService categoriesFor: #addServices:!accessing!public! !
!RowanLoggingService categoriesFor: #clientDisplayString!displaying!public! !
!RowanLoggingService categoriesFor: #clientGroupIdDisplayString!displaying!public! !
!RowanLoggingService categoriesFor: #clientIdDisplayString!displaying!public! !
!RowanLoggingService categoriesFor: #comment!accessing!private! !
!RowanLoggingService categoriesFor: #comment:!accessing!private! !
!RowanLoggingService categoriesFor: #date!accessing!private! !
!RowanLoggingService categoriesFor: #date:!accessing!private! !
!RowanLoggingService categoriesFor: #displayCommand!displaying!public! !
!RowanLoggingService categoriesFor: #displayName!displaying!public! !
!RowanLoggingService categoriesFor: #displayString!displaying!public! !
!RowanLoggingService categoriesFor: #fileName!accessing!private! !
!RowanLoggingService categoriesFor: #fileName:!accessing!private! !
!RowanLoggingService categoriesFor: #groupId!accessing!private! !
!RowanLoggingService categoriesFor: #groupId:!accessing!private! !
!RowanLoggingService categoriesFor: #hash!comparing!public! !
!RowanLoggingService categoriesFor: #id!accessing!private! !
!RowanLoggingService categoriesFor: #id:!accessing!private! !
!RowanLoggingService categoriesFor: #initialize!initialization!public! !
!RowanLoggingService categoriesFor: #isLogging!accessing!public! !
!RowanLoggingService categoriesFor: #isLogging:!public! !
!RowanLoggingService categoriesFor: #isLoggingService!public!testing! !
!RowanLoggingService categoriesFor: #isTestLoggingService!public!testing! !
!RowanLoggingService categoriesFor: #location!accessing!private! !
!RowanLoggingService categoriesFor: #location:!accessing!private! !
!RowanLoggingService categoriesFor: #logComment:!logging!public! !
!RowanLoggingService categoriesFor: #logReceivedServices:!logging!public! !
!RowanLoggingService categoriesFor: #logSentServices:!logging!public! !
!RowanLoggingService categoriesFor: #logServices:!logging!public! !
!RowanLoggingService categoriesFor: #mode!accessing!public! !
!RowanLoggingService categoriesFor: #name!accessing!public! !
!RowanLoggingService categoriesFor: #name:!accessing!public! !
!RowanLoggingService categoriesFor: #printString!displaying!public! !
!RowanLoggingService categoriesFor: #serverDisplayString!displaying!public! !
!RowanLoggingService categoriesFor: #services!accessing!public! !
!RowanLoggingService categoriesFor: #sortAspect!accessing!public! !
!RowanLoggingService categoriesFor: #time!accessing!private! !
!RowanLoggingService categoriesFor: #time:!accessing!private! !

!RowanLoggingService class methodsFor!

basicNewWithGroupId: integer logger: logger
	| inst |
	inst := self new.
	inst
		groupId: integer;
		id: 0.
	^inst!

loggingServiceClass
	^Smalltalk at: #JadeiteTestLoggingServiceClass ifAbsent: [self]!

newWithGroupId: integer logger: logger
	^self basicNewWithGroupId: integer logger: logger! !
!RowanLoggingService class categoriesFor: #basicNewWithGroupId:logger:!instance creation!public! !
!RowanLoggingService class categoriesFor: #loggingServiceClass!accessing!public! !
!RowanLoggingService class categoriesFor: #newWithGroupId:logger:!instance creation!public! !

RowanMethodService guid: (GUID fromString: '{f5550fdc-dbb4-4382-af82-88c561c0a9d1}')!
RowanMethodService comment: 'Don''t change the order of the inst vars without checking
the index of the firstReference & testResults inst vars. These have
constant methods for performance. 

#indexOfTestResultInstVar
#indexOfFirstReferenceInstVar'!
!RowanMethodService categoriesForClass!Kernel-Objects! !
!RowanMethodService methodsFor!

<= methodService
	(methodService isKindOf: RowanMethodService) ifFalse: [^false].
	^className = methodService className
		ifTrue: [selector <= methodService selector]
		ifFalse: [className <= methodService className]!

= methodService
	methodService isMethodService ifFalse: [^false].
	^selector = methodService selector
		and: [className = methodService className and: [meta = methodService meta]]!

>= methodService
	(methodService isKindOf: RowanMethodService) ifFalse: [^false].
	^className = methodService className
		ifTrue: [selector >= methodService selector]
		ifFalse: [className >= methodService className]!

accessedInstVars
	^accessedInstVars!

accessedInstVars: anObject
	accessedInstVars := anObject!

addBreakPointMenuAt: stepPoint to: aMenu presenter: presenter
	| desc args menuCommand |
	args := Array with: stepPoint with: presenter parentPresenter.
	(self positiveBreakpoints includes: stepPoint)
		ifTrue: 
			[command := #clearBreakPointAt:in:.
			desc := 'Clear break at step point ' , stepPoint printString.
			menuCommand := MessageSend
						receiver: presenter parentPresenter
						selector: command
						arguments: args.
			aMenu addCommand: menuCommand description: desc.
			(breakPoints includes: stepPoint)
				ifTrue: 
					[command := #disableBreakPointAt:in:.
					desc := 'Disable break at step point ' , stepPoint printString]
				ifFalse: 
					[command := #enableBreakPointAt:in:.
					desc := 'Enable break at step point ' , stepPoint printString].
			menuCommand := MessageSend
						receiver: presenter parentPresenter
						selector: command
						arguments: args.
			aMenu addCommand: menuCommand description: desc]
		ifFalse: 
			[command := #setBreakPointAt:in:.
			desc := 'Break at step point ' , stepPoint printString.
			menuCommand := MessageSend
						receiver: presenter parentPresenter
						selector: command
						arguments: args.
			aMenu addCommand: menuCommand description: desc].
	^menuCommand!

appendToSourceMenu: aMenu presenter: methodSourcePresenter
	| stepPoint menuCommand messageSelector |
	self removeTrailingMenuItemsFrom: aMenu.
	stepPoint := self stepPointFromCursorIn: methodSourcePresenter.
	stepPoint isNil ifTrue: [^self removeJadeiteSeparatorFrom: aMenu].
	('*Edit*' match: aMenu text) ifFalse: [^self removeJadeiteSeparatorFrom: aMenu].	"popup or edit menu only"	
	menuCommand := self
				addBreakPointMenuAt: stepPoint
				to: aMenu
				presenter: methodSourcePresenter.
	messageSelector := (stepPoints at: stepPoint) value.
	messageSelector isEmpty ifTrue: [^self removeJadeiteSeparatorFrom: aMenu].
	aMenu addSeparator.
	menuCommand := MessageSend
				receiver: methodSourcePresenter parentPresenter
				selector: #browseImplementorsOf:
				argument: messageSelector.
	aMenu addCommand: menuCommand description: 'Browse Implementors of ' , messageSelector printString.
	menuCommand := MessageSend
				receiver: methodSourcePresenter parentPresenter
				selector: #browseSendersOf:
				argument: messageSelector.
	aMenu addCommand: menuCommand description: 'Browse Senders of ' , messageSelector printString!

basicPrepareForReplication
	testRunClassName := nil!

breakpointDisplayString
	| count writeStream disabledCount |
	breakPoints ifNil: [^self displayString].
	count := breakPoints size.
	disabledCount := (breakPoints select: [:bp | bp < 0]) size.
	writeStream := WriteStream on: String new.
	meta ifTrue: [writeStream nextPutAll: 'class>>'].
	writeStream
		nextPutAll: self displayString;
		space.
	writeStream
		nextPut: $(;
		nextPutAll: count printString.
	disabledCount > 0
		ifTrue: 
			[writeStream
				nextPut: $,; space;
				nextPutAll: 'disabled=';
				nextPutAll: disabledCount printString].
	writeStream nextPut: $).
	^writeStream contents!

breakPoints
	^breakPoints!

breakPoints: anObject
	breakPoints := anObject!

category
	^category ifNil: ['']!

category: anObject
	category := anObject!

classMethodDisplayString
	| stream |
	stream := WriteStream on: String new.
	self displayOn: stream.
	^stream contents!

className
	^className!

className: anObject
	className := anObject asString!

classService

	^RowanClassService named: className!

comparisonSource
	^comparisonSource!

comparisonSource: anObject
	comparisonSource := anObject!

compilationWarnings
	^compilationWarnings!

copyBasicsFrom: oldService

	oop 		:= oldService oop.
	selector 	:= oldService selector.
	className := oldService className asString.
	meta 	:= oldService meta.!

debuggerMethodSourceUpdate: presenter browser: browser
	"don't replicate the method - debugger is debugging the old method"

	^browser refreshBreakPointsIn: presenter!

definedClassName
	^definedClassName!

definedClassName: anObject
	definedClassName := anObject!

definedPackage
	^definedPackage!

displayClassName
	^className , (meta ifTrue: [' class'] ifFalse: [String new])!

displayName
	"protect against invalid entries"

	^[className , '>>' , selector] on: Error
		do: [:ex | className printString , '>>' , selector printString]!

displayOn: aStream
	aStream
		nextPutAll: (className ifNil: ['?']);
		nextPutAll: '>>';
		nextPutAll: (selector ifNil: ['?'])!

displayString
	^selector!

displayStringFor: displayThing
	isExtension
		ifTrue: 
			[displayThing forecolor: Color darkMagenta.
			inSelectedPackage ifFalse: [displayThing font beUnderlined ]]!

displayStringFor: displayThing browser: browser 
	browser displayStringFor: self in:  displayThing
	!

equalBeforeRename: aMethodService
	^renamedName = aMethodService className
		and: [selector = aMethodService selector and: [meta = aMethodService meta]]!

failedCompile
	^failedCompile!

firstReference
	^firstReference!

firstReference: integer
	firstReference := integer!

hash
	^(selector hash bitXor: className hash) bitXor: meta hash!

hasMethodHistory

	^hasMethodHistory!

hasSubs
	^hasSubs!

hasSubs: anObject
	hasSubs := anObject!

hasSupers
	^hasSupers!

hasSupers: anObject
	hasSupers := anObject!

homeMethodOop
	^homeMethodOop!

icon
	testResult ifNil: [^self class icon].
	testResult = 'passed' ifTrue: [^true icon].
	testResult = 'failure' ifTrue: [^Warning icon].
	testResult = 'error' ifTrue: [^false icon].
	^self class icon!

implementorsOf: aString

	^methodDefinitions!

indexOfFirstReferenceInstVar

	^37!

indexOfTestResultInstVar
	^31!

initialize
	super initialize.
	inSelectedPackage := true.
	failedCompile := false.
	meta := false.
	stepPoints := Array new.
	breakPoints := OrderedCollection new.
	hasSupers := false.
	hasSubs := false.
	hasMethodHistory := true. !

inSelectedPackage

	^inSelectedPackage!

inSelectedPackage: anObject
	inSelectedPackage := anObject!

isExtension
	^isExtension!

isExtension: anObject
	isExtension := anObject!

isMethodService

	^true!

isReadOnly

	self rowanFixMe.
	^false!

isTestMethod
	^isTestMethod!

meta
	^meta!

meta: anObject
	meta := anObject!

methodDefinitions
	^methodDefinitions!

methodDefinitions: anObject
	methodDefinitions := anObject.
	self trigger: #changed!

methodHistoryUpdated: historyBrowser
	
	^historyBrowser methodHistoryUpdated: self!

methodListUpdate: presenter browser: browser
	browser updateMethodList: self!

methodName
	"for the old sunit in class browser tab"
	^selector!

methodSourceUpdate: presenter browser: methodSourcePresenter
	| selection |
	selection := methodSourcePresenter methodListSelection.
	selection ifNil: [^self].
	selection = self
		ifTrue: 
			[selection replicateFrom: self.
			presenter view isModified
				ifFalse: 
					[presenter value: source.
					methodSourcePresenter refreshBreakPointsIn: presenter.
					methodSourcePresenter displayNoMods: presenter]]!

methodUpdate: presenter browser: browser
	presenter list do: 
			[:methodService |
			methodService replicateFrom: self	"#replicateFrom: will ensure we only replicate the proper methods"].
	browser updateMethodSource!

name
	selector isNil ifTrue:[^String new].
	^selector asString!

name: aString
	name := aString asString!

oop

	^oop!

oopType

	^OopType64 fromInteger: oop!

packageName
	^packageName!

packageName: anObject
	packageName := anObject!

positiveBreakpoints
	^breakPoints collect: [:bp | bp abs]!

postUpdate
	super postUpdate.
	renamedName := nil!

prepareForReplication
	super prepareForReplication.
	self basicPrepareForReplication!

printOn: target
	"Append, to the <puttableStream>, target, a string whose characters are a 
	the same as those which would result from sending a #printString
	message to the receiver.
	N.B. This is really intended for development use. #displayOn: and #displayString
	are complementary methods for generating strings for presentation to an
	end-user."

	| serviceClassName |
	serviceClassName := self class name.
	target 
		nextPutAll: (serviceClassName first isVowel ifTrue: ['an '] ifFalse: ['a ']);
		nextPutAll: serviceClassName;
		nextPut: $(;
		nextPutAll: className; 
		nextPutAll: '>>';
		nextPutAll: selector asString;
		nextPut: $). !

projectName
	^projectName!

projectName: anObject
	projectName := anObject!

references
	^references!

references: anObject
	references := anObject!

remoteServiceName
	^'Rowan methodServiceClass'!

removeJadeiteSeparatorFrom: aMenu
	| last |
	last := aMenu items last.
	last id = #jadeiteDivider ifTrue: [aMenu removeItem: last]!

renamedName
	^renamedName!

renamedName: anObject
	renamedName := anObject!

replicateFrom: newService
	^(self isMethodService and: [newService isMethodService])
		ifTrue: 
			[(self = newService or: [newService equalBeforeRename: self])
				ifTrue: 
					[super replicateFrom: newService.
					renamedName := nil]]!

searchString
	^searchString!

searchString: anObject
	searchString := anObject!

selectedPackageServices
	^selectedPackageServices!

selectedPackageServices: anObject browser: anObject1
	selectedPackageServices := anObject!

selector
	^selector!

selector: anObject
	selector := anObject!

selectorsDisplayString

	^className, '>>', selector!

setError
	testResult := 'error'!

setFailure
	testResult := 'failure'!

setPassed
	testResult := 'passed'!

shouldReplicateInstVarAtIndex: index newService: newService
	"first reference is only valid on the first return of a method service for a particular browser. 
	It may be better to (someday) move firstReference to the browser state"

	| isTestResultInstVar |
	index = self indexOfFirstReferenceInstVar ifTrue: [firstReference ifNotNil: [^false]].
	isTestResultInstVar := self indexOfTestResultInstVar = index.
	^isTestResultInstVar not or: [isTestResultInstVar and: [(newService instVarAt: index) notNil]]!

sortAspect

	^selector!

source
	^source!

source: anObject
	source := anObject!

stepPointFromCursorIn: methodSourcePresenter
	| charIndex |
	charIndex := methodSourcePresenter view caretPosition.
	^self stepPointFromCursorIn: methodSourcePresenter charIndex: charIndex!

stepPointFromCursorIn: methodSourcePresenter charIndex: charIndex
	self stepPoints size to: 1
		by: -1
		do: 
			[:index |
			| range |
			range := (self stepPoints at: index) key.
			(charIndex between: range start and: range stop) ifTrue: [^index]].
	^nil!

stepPoints
	(stepPoints notEmpty and: [stepPoints first key isInteger]) ifTrue: [
		stepPoints := stepPoints collect: [:each | 
			| range start char length |
			start := each key.
			char := source at: start.
			length := (char isAlphaNumeric or: [char = $_])
				ifTrue: [(source copyFrom: start + 1 to: source size) findFirst: [:eachChar | (eachChar isAlphaNumeric or: [eachChar = $_ or: [eachChar = $:]]) not]]
				ifFalse: [2].
			length = 0 ifTrue: [length := source size - start + 1].
			[
				2 < length and: [(source at: start) = $_].
			] whileTrue: [
				start := start + 1.
				length := length - 1.
			].
			range := Interval from: start to: start + length - 1.
			range -> each value.
		].
	].
	^stepPoints!

subSuperIcon
	^(self basicSubSuperIcon) imageIndex!

superDisplayString
	^superDisplayString!

superDisplayString: anObject
	superDisplayString := anObject!

testResult
	^testResult!

testResult: anObject
	testResult := anObject!

testResultUpdate: presenter browser: browser
	| existingService |
	existingService := presenter list detect: [:methodService | methodService = self] ifNone: [^self].
	testRunClassName ifNotNil: [browser selectedClass name = testRunClassName ifFalse: [^self]].
	existingService replicateFrom: self.
	presenter view invalidate!

toolTip
	| ws |
	ws := WriteStream on: String new.
	ws
		nextPutAll: 'Defined package: ';
		nextPutAll: definedPackage;
		cr;
		nextPutAll: 'Category: '; 
		nextPutAll: category.
	^ws contents!

unimplementedSelectors

	self rowanFixMe.
	^#()!

updateBreakPoints: presenter browser: browser
	presenter view invalidate!

updateSelectorFromCompiledMethod: aString

	| stream | 
	stream := ReadStream on: aString. 
	stream skipWhile:[:char | char isDigit].
	selector := stream upToEnd. 


!

user
	^user!

user: anObject
	user := anObject! !
!RowanMethodService categoriesFor: #<=!comparing!public! !
!RowanMethodService categoriesFor: #=!comparing!public! !
!RowanMethodService categoriesFor: #>=!comparing!public! !
!RowanMethodService categoriesFor: #accessedInstVars!accessing!private! !
!RowanMethodService categoriesFor: #accessedInstVars:!accessing!private! !
!RowanMethodService categoriesFor: #addBreakPointMenuAt:to:presenter:!method menu support!private! !
!RowanMethodService categoriesFor: #appendToSourceMenu:presenter:!method menu support!public! !
!RowanMethodService categoriesFor: #basicPrepareForReplication!public!replication! !
!RowanMethodService categoriesFor: #breakpointDisplayString!displaying!public! !
!RowanMethodService categoriesFor: #breakPoints!public! !
!RowanMethodService categoriesFor: #breakPoints:!accessing!public! !
!RowanMethodService categoriesFor: #category!accessing!private! !
!RowanMethodService categoriesFor: #category:!accessing!private! !
!RowanMethodService categoriesFor: #classMethodDisplayString!displaying!public! !
!RowanMethodService categoriesFor: #className!accessing!private! !
!RowanMethodService categoriesFor: #className:!accessing!private! !
!RowanMethodService categoriesFor: #classService!public! !
!RowanMethodService categoriesFor: #comparisonSource!accessing!private! !
!RowanMethodService categoriesFor: #comparisonSource:!accessing!private! !
!RowanMethodService categoriesFor: #compilationWarnings!accessing!private! !
!RowanMethodService categoriesFor: #copyBasicsFrom:!public!updating! !
!RowanMethodService categoriesFor: #debuggerMethodSourceUpdate:browser:!public!updating! !
!RowanMethodService categoriesFor: #definedClassName!accessing!private! !
!RowanMethodService categoriesFor: #definedClassName:!accessing!private! !
!RowanMethodService categoriesFor: #definedPackage!accessing!private! !
!RowanMethodService categoriesFor: #displayClassName!displaying!public! !
!RowanMethodService categoriesFor: #displayName!displaying!public! !
!RowanMethodService categoriesFor: #displayOn:!displaying!public! !
!RowanMethodService categoriesFor: #displayString!displaying!public! !
!RowanMethodService categoriesFor: #displayStringFor:!displaying!public! !
!RowanMethodService categoriesFor: #displayStringFor:browser:!displaying!public! !
!RowanMethodService categoriesFor: #equalBeforeRename:!comparing!public! !
!RowanMethodService categoriesFor: #failedCompile!accessing!public! !
!RowanMethodService categoriesFor: #firstReference!accessing!private! !
!RowanMethodService categoriesFor: #firstReference:!accessing!private! !
!RowanMethodService categoriesFor: #hash!comparing!public! !
!RowanMethodService categoriesFor: #hasMethodHistory!accessing!private! !
!RowanMethodService categoriesFor: #hasSubs!accessing!private! !
!RowanMethodService categoriesFor: #hasSubs:!accessing!private! !
!RowanMethodService categoriesFor: #hasSupers!accessing!private! !
!RowanMethodService categoriesFor: #hasSupers:!accessing!private! !
!RowanMethodService categoriesFor: #homeMethodOop!accessing!private! !
!RowanMethodService categoriesFor: #icon!public!sunit! !
!RowanMethodService categoriesFor: #implementorsOf:!method accessing!public! !
!RowanMethodService categoriesFor: #indexOfFirstReferenceInstVar!constants!public! !
!RowanMethodService categoriesFor: #indexOfTestResultInstVar!constants!public! !
!RowanMethodService categoriesFor: #initialize!initialization!public! !
!RowanMethodService categoriesFor: #inSelectedPackage!public!testing! !
!RowanMethodService categoriesFor: #inSelectedPackage:!accessing!public! !
!RowanMethodService categoriesFor: #isExtension!accessing!public! !
!RowanMethodService categoriesFor: #isExtension:!accessing!public! !
!RowanMethodService categoriesFor: #isMethodService!public!testing! !
!RowanMethodService categoriesFor: #isReadOnly!public!testing! !
!RowanMethodService categoriesFor: #isTestMethod!public!testing! !
!RowanMethodService categoriesFor: #meta!accessing!private! !
!RowanMethodService categoriesFor: #meta:!accessing!private! !
!RowanMethodService categoriesFor: #methodDefinitions!accessing!private! !
!RowanMethodService categoriesFor: #methodDefinitions:!accessing!private! !
!RowanMethodService categoriesFor: #methodHistoryUpdated:!public!updating! !
!RowanMethodService categoriesFor: #methodListUpdate:browser:!public!updating! !
!RowanMethodService categoriesFor: #methodName!accessing!private! !
!RowanMethodService categoriesFor: #methodSourceUpdate:browser:!public!updating! !
!RowanMethodService categoriesFor: #methodUpdate:browser:!public!updating! !
!RowanMethodService categoriesFor: #name!accessing!public! !
!RowanMethodService categoriesFor: #name:!accessing!public! !
!RowanMethodService categoriesFor: #oop!public! !
!RowanMethodService categoriesFor: #oopType!public! !
!RowanMethodService categoriesFor: #packageName!accessing!private! !
!RowanMethodService categoriesFor: #packageName:!accessing!private! !
!RowanMethodService categoriesFor: #positiveBreakpoints!accessing!public! !
!RowanMethodService categoriesFor: #postUpdate!public!updating! !
!RowanMethodService categoriesFor: #prepareForReplication!public!replication! !
!RowanMethodService categoriesFor: #printOn:!printing!public! !
!RowanMethodService categoriesFor: #projectName!accessing!private! !
!RowanMethodService categoriesFor: #projectName:!accessing!private! !
!RowanMethodService categoriesFor: #references!accessing!private! !
!RowanMethodService categoriesFor: #references:!accessing!private! !
!RowanMethodService categoriesFor: #remoteServiceName!must not strip!public! !
!RowanMethodService categoriesFor: #removeJadeiteSeparatorFrom:!method menu support!private! !
!RowanMethodService categoriesFor: #renamedName!accessing!private! !
!RowanMethodService categoriesFor: #renamedName:!accessing!private! !
!RowanMethodService categoriesFor: #replicateFrom:!public!replication! !
!RowanMethodService categoriesFor: #searchString!accessing!private! !
!RowanMethodService categoriesFor: #searchString:!accessing!private! !
!RowanMethodService categoriesFor: #selectedPackageServices!accessing!private! !
!RowanMethodService categoriesFor: #selectedPackageServices:browser:!accessing!private! !
!RowanMethodService categoriesFor: #selector!accessing!private! !
!RowanMethodService categoriesFor: #selector:!accessing!private! !
!RowanMethodService categoriesFor: #selectorsDisplayString!displaying!public! !
!RowanMethodService categoriesFor: #setError!public!sunit! !
!RowanMethodService categoriesFor: #setFailure!public!sunit! !
!RowanMethodService categoriesFor: #setPassed!public!sunit! !
!RowanMethodService categoriesFor: #shouldReplicateInstVarAtIndex:newService:!public!replication!testing! !
!RowanMethodService categoriesFor: #sortAspect!accessing!public! !
!RowanMethodService categoriesFor: #source!accessing!private! !
!RowanMethodService categoriesFor: #source:!accessing!private! !
!RowanMethodService categoriesFor: #stepPointFromCursorIn:!presenter support!private! !
!RowanMethodService categoriesFor: #stepPointFromCursorIn:charIndex:!presenter support!private! !
!RowanMethodService categoriesFor: #stepPoints!public! !
!RowanMethodService categoriesFor: #subSuperIcon!displaying!public! !
!RowanMethodService categoriesFor: #superDisplayString!accessing!private! !
!RowanMethodService categoriesFor: #superDisplayString:!accessing!private! !
!RowanMethodService categoriesFor: #testResult!accessing!public! !
!RowanMethodService categoriesFor: #testResult:!accessing!public! !
!RowanMethodService categoriesFor: #testResultUpdate:browser:!public!updating! !
!RowanMethodService categoriesFor: #toolTip!accessing!displaying!private! !
!RowanMethodService categoriesFor: #unimplementedSelectors!public! !
!RowanMethodService categoriesFor: #updateBreakPoints:browser:!public!updating! !
!RowanMethodService categoriesFor: #updateSelectorFromCompiledMethod:!private! !
!RowanMethodService categoriesFor: #user!accessing!private! !
!RowanMethodService categoriesFor: #user:!accessing!private! !

!RowanMethodService class methodsFor!

defaultIconName
	"Answer a filename to use for an icon of this class."

	^File composeStem: 'Message' extension: 'ico'.!

fakeMethodFor: className selector: selector source: source
	| inst |
	inst := self new.
	inst
		className: className;
		selector: selector;
		source: source.
	^inst!

icon
	"Answers an Icon that can be used to represent this class"

	^##(self) defaultIcon ! !
!RowanMethodService class categoriesFor: #defaultIconName!private! !
!RowanMethodService class categoriesFor: #fakeMethodFor:selector:source:!instance creation!public! !
!RowanMethodService class categoriesFor: #icon!private! !

RowanPackageService guid: (GUID fromString: '{16c34093-697c-4a97-8953-e92983a2f084}')!
RowanPackageService comment: ''!
!RowanPackageService categoriesForClass!Kernel-Objects! !
!RowanPackageService methodsFor!

<= packageService
	^packageService isPackageService ifTrue: [name <= packageService name] ifFalse: [^false]!

= packageService
	^packageService isPackageService ifTrue: [name = packageService name] ifFalse: [^false]!

addHierarchyService: service to: treeModel withParent: parentService
	treeModel getNodeFor: service ifAbsent: [treeModel add: service asChildOf: parentService].
	(hierarchyServices at: service ifAbsent: [^self]) do: 
			[:classService |
			self
				addHierarchyService: classService
				to: treeModel
				withParent: service]!

basicPrepareForReplication
	"don't iterate over sub colletions"
	classes := nil.
	testClasses := nil. 
	selectedClass := nil. !

classes

	^classes!

classes: anArray
	classes := anArray!

classesUpdate: presenter browser: browser
	"If the selected packages have the same class defined and extended, show the defined class"

	| condensedList selections selectedPackageServices |
	presenter list isEmpty ifTrue: [self initializePresenterList: presenter].
	(self doesUpdateApply: browser) ifFalse: [^self].
	self removeDeletedClassesIn: presenter browser: browser.
	condensedList := self condenseExtensions: presenter.
	presenter selectionOrNil
		ifNil: [presenter list: (ListModel withAll: condensedList keys)]
		ifNotNil: 
			[self
				updateList: presenter
				whilePreservingSelections: condensedList
				browser: browser].
	"#selections is very expensive in Dolphin. Don't send it in a loop"
	selections := browser packageListPresenter selections.
	selectedPackageServices := browser isRowanProjectSelected ifTrue: [selections] ifFalse: [Array new].
	presenter list do: [:classService | classService selectedPackageServices: selectedPackageServices].
	presenter view invalidate.
	browser isClassSelected ifFalse: [self emptyFilterListsIn: browser]!

classHierarchyUpdate: presenter browser: browser
	browser isHierarchyTabSelected ifFalse: [^false].
	browser packageListPresenter selectionOrNil
		ifNil: [^self]
		ifNotNil: [:packageService | packageService name = name ifFalse: [^self]].
	self
		classHierarchyUpdate: presenter
		browser: browser
		hierarchyServices: hierarchyServices!

condenseExtensions: presenter
	| defined dictionary rejections |
	dictionary := Dictionary new.
	dictionary
		atAll: presenter list put: nil;
		atAll: classes put: nil.
	defined := dictionary keys select: [:svc | svc isDefined] thenCollect: [:svc | svc name].
	rejections := dictionary keys select: [:svc | svc isExtension and: [defined includes: svc name]].
	dictionary removeAllKeys: rejections. 
	rejections := dictionary keys select: [:svc | svc wasRenamed].
	dictionary removeAllKeys: rejections. 
	^dictionary!

defaultTemplate
	^defaultTemplate!

defaultTemplate: anObject
	defaultTemplate := anObject!

displayName
	"for logging. for now"

	^name!

displayString
	^self name!

displayStringFor: displayThing
	name ifNil: [^self].
	self isDirty == true
		ifTrue: 
			[displayThing font
				beItalic;
				beBold.
			displayThing forecolor: Color black]!

doesUpdateApply: browser
	| selections |
	selections := browser packageListPresenter selections.
	^browser isNoneProjectSelected ifTrue: [false] ifFalse: [selections includes: self]!

hash
	^self name hash!

hierarchyServices: collection

	hierarchyServices := collection!

isDirty
	^isDirty!

isDirty: anObject
	isDirty := anObject!

isPackageService

	^true!

name
	^name!

name: aString
	name := aString asString!

packageUpdate: presenter
	| packageInList |
	packageInList := presenter list detect: [:packageService | packageService name = name] ifNone: [].
	packageInList ifNotNil: [:packageService | packageService isDirty: self isDirty].
	presenter view invalidate!

postUpdate
	super postUpdate.
	classes ifNotNil: [classes do: [:service | service postUpdate]].
	classes := Array new!

prepareForReplication
	super prepareForReplication.
	self basicPrepareForReplication!

projectName
	^projectName!

projectName: anObject
	projectName := anObject!

remoteServiceName
	^'Rowan packageServiceClass'!

removeDeletedClassesIn: presenter browser: browser
	^super
		removeDeletedClassesIn: presenter
		browser: browser
		classes: classes!

replicateFrom: newService
	^(self isPackageService and: [newService isPackageService])
		ifTrue: [super replicateFrom: newService]!

selectedClass
	^selectedClass!

selectedClass: anObject
	selectedClass := anObject!

sortAspect

	^name!

testClasses: presenter browser: browser
	| additions |
	additions := OrderedCollection new.
	testClasses ifNil: [^self].
	testClasses do: 
			[:newClassService |
			| updatedClass |
			updatedClass := presenter model detect: [:classService | classService = newClassService]
						ifNone: 
							[additions add: newClassService.
							nil].
			updatedClass ifNotNil: [updatedClass replicateFrom: newClassService]].
	browser addUpdatedTestClasses: additions!

updateList: presenter whilePreservingSelections: updates browser: browser
	| replicate |
	presenter list do: 
			[:service |
			replicate := updates at: service ifAbsent: [].
			replicate notNil ifTrue: [service replicateFrom: replicate]].
	updates keysDo: 
			[:update |
			| existing |
			existing := presenter model detect: [:service | service name = update name] ifNone: [].
			existing ifNil: [update wasRenamed ifFalse: [presenter model add: update]]]! !
!RowanPackageService categoriesFor: #<=!comparing!public! !
!RowanPackageService categoriesFor: #=!comparing!public! !
!RowanPackageService categoriesFor: #addHierarchyService:to:withParent:!private!updating! !
!RowanPackageService categoriesFor: #basicPrepareForReplication!public!replication! !
!RowanPackageService categoriesFor: #classes!accessing!public! !
!RowanPackageService categoriesFor: #classes:!accessing!public! !
!RowanPackageService categoriesFor: #classesUpdate:browser:!public!updating! !
!RowanPackageService categoriesFor: #classHierarchyUpdate:browser:!public!updating! !
!RowanPackageService categoriesFor: #condenseExtensions:!private!updating! !
!RowanPackageService categoriesFor: #defaultTemplate!accessing!private! !
!RowanPackageService categoriesFor: #defaultTemplate:!accessing!private! !
!RowanPackageService categoriesFor: #displayName!displaying!public! !
!RowanPackageService categoriesFor: #displayString!displaying!public! !
!RowanPackageService categoriesFor: #displayStringFor:!displaying!public! !
!RowanPackageService categoriesFor: #doesUpdateApply:!private!testing! !
!RowanPackageService categoriesFor: #hash!comparing!public! !
!RowanPackageService categoriesFor: #hierarchyServices:!public!updating! !
!RowanPackageService categoriesFor: #isDirty!accessing!public! !
!RowanPackageService categoriesFor: #isDirty:!accessing!public! !
!RowanPackageService categoriesFor: #isPackageService!public!testing! !
!RowanPackageService categoriesFor: #name!accessing!public! !
!RowanPackageService categoriesFor: #name:!accessing!public! !
!RowanPackageService categoriesFor: #packageUpdate:!public!updating! !
!RowanPackageService categoriesFor: #postUpdate!Init / Release!public! !
!RowanPackageService categoriesFor: #prepareForReplication!public!replication! !
!RowanPackageService categoriesFor: #projectName!accessing!private! !
!RowanPackageService categoriesFor: #projectName:!accessing!private! !
!RowanPackageService categoriesFor: #remoteServiceName!must not strip!public! !
!RowanPackageService categoriesFor: #removeDeletedClassesIn:browser:!private!updating! !
!RowanPackageService categoriesFor: #replicateFrom:!public!replication! !
!RowanPackageService categoriesFor: #selectedClass!accessing!public! !
!RowanPackageService categoriesFor: #selectedClass:!accessing!public! !
!RowanPackageService categoriesFor: #sortAspect!accessing!public! !
!RowanPackageService categoriesFor: #testClasses:browser:!public!updating! !
!RowanPackageService categoriesFor: #updateList:whilePreservingSelections:browser:!private!updating! !

!RowanPackageService class methodsFor!

defaultIconName
	"Answer a filename to use for an icon of this class."

	^File composeStem: 'Package' extension: 'ico'.!

icon
	"Answers an Icon that can be used to represent this class"

	^##(self) defaultIcon! !
!RowanPackageService class categoriesFor: #defaultIconName!private! !
!RowanPackageService class categoriesFor: #icon!private! !

RowanProcessService guid: (GUID fromString: '{ef0255f2-38fa-473c-8886-4f6beea18afa}')!
RowanProcessService comment: ''!
!RowanProcessService categoriesForClass!Kernel-Objects! !
!RowanProcessService methodsFor!

displayName
	"for logging. for now"

	^name!

frameForLevel: anInteger 

	^frames at: anInteger!

frames

	^frames!

gsProcessForSession: session
	^GsProcess session: session oopType: self oopType!

isUpdatableService
	^false!

name
	^name!

name: aString
	name := aString asString!

oop

	^oop!

oop: oopInteger

	oop := oopInteger!

oopType

	^OopType64 fromInteger: oop!

printOn: aStream
	aStream
		nextPutAll: 'RowanProcessService(';
		print: oop;
		nextPutAll: ')';
		yourself!

sortAspect

	^name!

stack

	^frames collect: [:each | each printString]!

terminate
	"temporary until debugger is rewritten"
	(GsProcess session: GciSession current oopType: (GciSession current oopTypeWithOop: oop))
		terminate.
	
	! !
!RowanProcessService categoriesFor: #displayName!displaying!public! !
!RowanProcessService categoriesFor: #frameForLevel:!public! !
!RowanProcessService categoriesFor: #frames!public! !
!RowanProcessService categoriesFor: #gsProcessForSession:!public! !
!RowanProcessService categoriesFor: #isUpdatableService!public!testing! !
!RowanProcessService categoriesFor: #name!accessing!public! !
!RowanProcessService categoriesFor: #name:!accessing!public! !
!RowanProcessService categoriesFor: #oop!public! !
!RowanProcessService categoriesFor: #oop:!public! !
!RowanProcessService categoriesFor: #oopType!public! !
!RowanProcessService categoriesFor: #printOn:!printing!public! !
!RowanProcessService categoriesFor: #sortAspect!accessing!public! !
!RowanProcessService categoriesFor: #stack!public! !
!RowanProcessService categoriesFor: #terminate!operations!public! !

RowanProjectService guid: (GUID fromString: '{d65732a3-f4fb-489b-ae7e-d104cddbf3d8}')!
RowanProjectService comment: ''!
!RowanProjectService categoriesForClass!Unclassified! !
!RowanProjectService methodsFor!

= projectService
	^projectService isProjectService ifTrue: [name = projectService name] ifFalse: [^false]!

addPackageNamed: newPackageName using: presenter
	| packageService answeringService services |
	self
		command: #addPackageNamed:;
		commandArgs: (Array with: newPackageName).
	services := presenter issueCommand: (Array with: self).
	answeringService := services detect: [:service | service isAnsweringService].
	answeringService answer = #duplicatePackage
		ifTrue: [^MessageBox notify: 'Duplicate Package. Package named ' , newPackageName , ' not added'].
	packageService := presenter packageListPresenter list
				detect: [:service | service name = newPackageName]
				ifNone: [^MessageBox notify: 'Package named ' , newPackageName , ' not found'].
	presenter packageListPresenter selection: packageService.
	presenter refreshFromServer!

allClasses
	"for testing"

	^packages inject: OrderedCollection new
		into: 
			[:coll :packageService |
			packageService classes ifNotNil: [:classes | coll addAll: classes].
			coll]!

basicChangesUsing: session
	| operations |
	command := #changes.
	BrowserUpdate current issueCommands: (Array with: self) session: session.
	operations := OrderedCollection new.
	changes
		do: [:changeString | operations addAll: (RowanPatch fromString: changeString session: session) operations].
	^operations!

basicCheckout: branchName using: presenter
	self
		command: #checkout:;
		commandArgs: (Array with: (branchName copyWithout: $*)).
	presenter issueCommand: (Array with: self).
	presenter refresh!

basicCheckoutTag: tag using: session
	self
		command: #checkoutTag:;
		commandArgs: (Array with: tag).
	BrowserUpdate current issueCommand: self session: session!

basicCheckoutUsing: presenter
	| branches query branchString |
	query := RowanQueryService new
				command: #projectBranches:;
				commandArgs: (Array with: self name).
	branchString := (presenter issueCommand: (Array with: query)) first answer.
	branches := (branchString subStrings: Character lf) asOrderedCollection.
	branches := branches reject: [:string | '*HEAD*' match: string].
	branches := branches collect: [:string | string copyWithout: Character space].
	^branches!

basicName: aString
	"don't round trip to the server to get information" 
	name := aString!

branch
	^branch!

branch: anObject
	branch := anObject!

checkoutUsing: presenter
	| branchName branches |
	branches := self basicCheckoutUsing: presenter.
	branchName := ChoicePrompter
				on: branch
				choices: branches
				caption: 'Select branch to checkout:'.
	branchName isNil ifTrue: [^self].
	self basicCheckout: branchName using: presenter!

componentServices

	^componentServices!

componentsUpdate: presenter browser: browser
	"component services will be a dictionary. 
	The key is the parent component.
	The value is an array of the children components.

	Example:
	nil -> #(A) - A is a top level component
	A -> #(B C) - B & C are children of A
	B -> #(D) - D is a child of B
	C -> nil
	D -> nil - neither C nor D have children"

	| treeModel parent topLevelComponents removals |
	wasUpdated ifFalse:[^self].
	presenter model class = TreeModel
		ifTrue: [^self	"not sure why the model starts out as a TreeModel, not a JadeiteTreeModel"].
	browser projectListPresenter selectionOrNil
		ifNil: [^self]
		ifNotNil: [:service | service name = name ifFalse: [^self]].
	treeModel := self chooseModel: presenter using: browser.
	topLevelComponents := componentServices at: #nil ifAbsent: [^self].
	treeModel resetVisited.
	parent := nil.
	topLevelComponents do: 
			[:componentService |
			self
				possiblyAddComponent: componentService
				to: treeModel
				withParent: parent
				hierarchyServices: componentServices].
	removals := treeModel asBag reject: [:service | (treeModel getNodeFor: service) visited].
	removals do: [:service | treeModel remove: service ifAbsent: []].
	presenter view ensureSelectionVisible!

displayName
	| displayName |
	name ifNil: [^String new].
	displayName := name.
	existsOnDisk == false ifTrue: [displayName := '(' , displayName , ')'].
	isDiskDirty ifTrue: [displayName := displayName , '*'].
	^displayName!

displayStringFor: displayThing
	name ifNil: [^self].
	self isDirty
		ifTrue: 
			[displayThing font
				beItalic;
				beBold.
			displayThing forecolor: Color black].
	self isLoaded
		ifTrue: 
			[self isSkew
				ifTrue: [displayThing forecolor: Color red]
				ifFalse: [displayThing forecolor: Color black]]
		ifFalse: [displayThing forecolor: Color black]!

gitTags: session
	| query tags |
	query := RowanQueryService new
				command: #gitTagListUsing:;
				commandArgs: (Array with: self).
	tags := (BrowserUpdate current issueCommand: query session: session) first answer.
	^tags!

hash
	^self name hash!

initialize
	super initialize.
	isDirty := false.
	isLoaded := false.
	isSkew := false.
	sha := 'not a project'.
	isDiskDirty := false. !

isDirty
	^isDirty!

isDirty: anObject
	isDirty := anObject!

isDiskDirty
	^isDiskDirty!

isLoaded
	"Unpackaged project may have a nil sha"

	^sha isNil not and: [sha isEmpty not]!

isProjectService

	^true!

isSkew
	^isSkew!

isSkew: anObject
	isSkew := anObject!

name
	^name!

name: aString
	name := aString asString!

newTextView: session using: parentPresenter
	| edit shell |
	shell := JadeShell showOnSession: session.
	shell view
		layoutManager: BorderLayout new;
		hasMaximize: false;
		hasMinimize: false;
		extent: 800 @ 900;
		caption: 'About';
		addSubView: (edit := RichTextEdit new);
		contextMenu: parentPresenter view contextMenu;
		show.
	edit
		arrangement: #center;
		isReadOnly: true;
		canVScroll: true;
		font: (Font name: 'Arial' pointSize: 10);
		alignment: #center;
		contextMenu: parentPresenter view contextMenu.
	^edit!

packages

	^packages!

packageServices
	"really, packages are a RowanPackageService"
	^packages!

performGitCommand: gitCommand with: argsString in: session
	self
		command: #performGitCommand:with:;
		commandArgs: (Array with: gitCommand with: argsString).
	BrowserUpdate current issueCommand: self session: session!

postUpdate
	super postUpdate.
	packages ifNotNil: [packages do: [:service | service postUpdate]].
	componentServices := Dictionary new. !

prepareForReplication

	"I don't *think* we need packages when sending to the server"
	super prepareForReplication.
	packages := nil.!

projectPackagesUpdate: presenter browser: browser
	wasUpdated ifFalse: [^self].
	self
		packagesUpdate: presenter
		browser: browser
		parentPresenter: browser projectListPresenter!

projectSelectionUpdate: presenter
	| newSelections |
	newSelections := presenter list select: [:service | self name = service name].
	presenter selections: newSelections!

projectsUpdate: presenter
	| listProject |
	wasUpdated ifFalse: [^self].
	listProject := presenter list detect: [:listProj | listProj name = name] ifNone: [^self].
	listProject replicateFrom: self.
	presenter view invalidate!

projectUrl
	^projectUrl!

remoteServiceName
	^'Rowan projectServiceClass'!

removed: presenter
	self = RowanProjectService noneProject ifTrue: [^self].
	self isDefinedProject ifTrue:[^self]. 
	^super removed: presenter!

removedProject: presenter
	| removedProject |
	updateType == #removedProject: ifFalse:[^self].
	removedProject := presenter list detect: [:projectService | projectService name = name] ifNone: [].
	removedProject ifNotNil: [presenter model remove: removedProject]!

replicateFrom: newService
	^self isProjectService ifTrue: [name isNil ifFalse: [super replicateFrom: newService]]!

rowanProjectsHome
	^rowanProjectsHome!

sha
	^sha!

sha: anObject
	sha := anObject!

sortAspect

	^name!

specService
	^specService!

specService: anObject
	specService := anObject! !
!RowanProjectService categoriesFor: #=!comparing!public! !
!RowanProjectService categoriesFor: #addPackageNamed:using:!presenter support!public! !
!RowanProjectService categoriesFor: #allClasses!accessing!public! !
!RowanProjectService categoriesFor: #basicChangesUsing:!presenter support!public! !
!RowanProjectService categoriesFor: #basicCheckout:using:!presenter support!public! !
!RowanProjectService categoriesFor: #basicCheckoutTag:using:!presenter support!public! !
!RowanProjectService categoriesFor: #basicCheckoutUsing:!presenter support!private! !
!RowanProjectService categoriesFor: #basicName:!accessing!public! !
!RowanProjectService categoriesFor: #branch!accessing!public! !
!RowanProjectService categoriesFor: #branch:!accessing!public! !
!RowanProjectService categoriesFor: #checkoutUsing:!presenter support!public! !
!RowanProjectService categoriesFor: #componentServices!public! !
!RowanProjectService categoriesFor: #componentsUpdate:browser:!public!updating! !
!RowanProjectService categoriesFor: #displayName!accessing!displaying!public! !
!RowanProjectService categoriesFor: #displayStringFor:!displaying!public! !
!RowanProjectService categoriesFor: #gitTags:!presenter support!public! !
!RowanProjectService categoriesFor: #hash!comparing!public! !
!RowanProjectService categoriesFor: #initialize!Init / Release!public! !
!RowanProjectService categoriesFor: #isDirty!accessing!public! !
!RowanProjectService categoriesFor: #isDirty:!accessing!public! !
!RowanProjectService categoriesFor: #isDiskDirty!accessing!public! !
!RowanProjectService categoriesFor: #isLoaded!public!testing! !
!RowanProjectService categoriesFor: #isProjectService!public!testing! !
!RowanProjectService categoriesFor: #isSkew!accessing!private! !
!RowanProjectService categoriesFor: #isSkew:!accessing!private! !
!RowanProjectService categoriesFor: #name!accessing!public! !
!RowanProjectService categoriesFor: #name:!accessing!public! !
!RowanProjectService categoriesFor: #newTextView:using:!displaying!private! !
!RowanProjectService categoriesFor: #packages!accessing!public! !
!RowanProjectService categoriesFor: #packageServices!accessing!public! !
!RowanProjectService categoriesFor: #performGitCommand:with:in:!presenter support!public! !
!RowanProjectService categoriesFor: #postUpdate!Init / Release!public! !
!RowanProjectService categoriesFor: #prepareForReplication!public!replication! !
!RowanProjectService categoriesFor: #projectPackagesUpdate:browser:!public!updating! !
!RowanProjectService categoriesFor: #projectSelectionUpdate:!public!updating! !
!RowanProjectService categoriesFor: #projectsUpdate:!public!updating! !
!RowanProjectService categoriesFor: #projectUrl!accessing!public! !
!RowanProjectService categoriesFor: #remoteServiceName!must not strip!public! !
!RowanProjectService categoriesFor: #removed:!public!updating! !
!RowanProjectService categoriesFor: #removedProject:!public!updating! !
!RowanProjectService categoriesFor: #replicateFrom:!public!replication! !
!RowanProjectService categoriesFor: #rowanProjectsHome!accessing!public! !
!RowanProjectService categoriesFor: #sha!accessing!public! !
!RowanProjectService categoriesFor: #sha:!accessing!public! !
!RowanProjectService categoriesFor: #sortAspect!accessing!public! !
!RowanProjectService categoriesFor: #specService!accessing!private! !
!RowanProjectService categoriesFor: #specService:!accessing!private! !

!RowanProjectService class methodsFor!

defaultIconName
	"Answer a filename to use for an icon of this class."

	^File composeStem: 'Collection' extension: 'ico'.!

icon
	"Answers an Icon that can be used to represent this class"

	^##(self) defaultIcon!

noneProject
	| inst |
	inst := self new.
	inst
		name: self notRowanizedPackageName;
		isDirty: false;
		isSkew: false;
		sha: 'not a project'.
	^inst! !
!RowanProjectService class categoriesFor: #defaultIconName!private! !
!RowanProjectService class categoriesFor: #icon!private! !
!RowanProjectService class categoriesFor: #noneProject!instance creation!public! !

RowanQueryService guid: (GUID fromString: '{b3af0738-b123-4ff6-8610-4344673a459d}')!
RowanQueryService comment: ''!
!RowanQueryService categoriesForClass!Unclassified! !
!RowanQueryService methodsFor!

answer
	^queryResults	!

displayName
	"for logging. for now"

	^name!

hasResults

	^queryResults notEmpty!

name
	^name!

name: aString
	name := aString asString!

queryResults
	^queryResults!

queryResults: anObject
	queryResults := anObject!

sortAspect

	^name! !
!RowanQueryService categoriesFor: #answer!accessing!private! !
!RowanQueryService categoriesFor: #displayName!displaying!public! !
!RowanQueryService categoriesFor: #hasResults!public!testing! !
!RowanQueryService categoriesFor: #name!accessing!public! !
!RowanQueryService categoriesFor: #name:!accessing!public! !
!RowanQueryService categoriesFor: #queryResults!accessing!private! !
!RowanQueryService categoriesFor: #queryResults:!accessing!private! !
!RowanQueryService categoriesFor: #sortAspect!accessing!public! !

RowanTestService guid: (GUID fromString: '{cbed4495-aa12-43d9-a2c3-31722275a0a8}')!
RowanTestService comment: 'Client test support service'!
!RowanTestService categoriesForClass!Unclassified! !
!RowanTestService methodsFor!

displayName
	"for logging. for now"

	^name!

execute: block overArrayOop: arrayOop session: session
	"assume the oop is an array with simple elements. 
	Evaluate the block for each element"

	| size theOop |
	theOop := OopType64 fromInteger: arrayOop.
	size := session
				executeString: 'self size'
				fromContext: theOop
				environment: 0.
	1 to: size
		do: 
			[:index |
			| remoteElement |
			remoteElement := session
						executeString: 'self at: ' , index printString
						fromContext: theOop
						environment: 0.
			block value: remoteElement]!

name
	^name!

name: aString
	name := aString asString!

remoteServiceClassNamesOop: session
	| answeringService |
	answeringService := RowanAnsweringService new.
	answeringService
		command: #exec:context:;
		commandArgs: (Array with: 'Rowan platform serviceClasses collect:[:cls | cls name]' with: session oopNil value).
	BrowserUpdate current issueCommand: answeringService session: session.
	^answeringService answer value "key is whether result compiled" !

sortAspect

	^name! !
!RowanTestService categoriesFor: #displayName!displaying!public! !
!RowanTestService categoriesFor: #execute:overArrayOop:session:!public! !
!RowanTestService categoriesFor: #name!accessing!public! !
!RowanTestService categoriesFor: #name:!accessing!public! !
!RowanTestService categoriesFor: #remoteServiceClassNamesOop:!public! !
!RowanTestService categoriesFor: #sortAspect!accessing!public! !

RowanVariableService guid: (GUID fromString: '{7fef01ea-b803-4a8a-bb67-843c948bbc31}')!
RowanVariableService comment: ''!
!RowanVariableService categoriesForClass!Kernel-Objects! !
!RowanVariableService methodsFor!

_key

	^key!

className

	^className!

displayName
	"for logging. for now"

	^name!

displayOop

	^oop printString!

displayStringOn: displayThing!

isDisplayLabel
	^false!

isUpdatableService
	^false!

key
	"provided to match expectation of UI for JadeDebugger"

	^oop printString -> key!

name
	^name!

name: aString
	name := aString asString!

oop

	^oop!

printOn: aStream
	super printOn: aStream.
	aStream
		nextPutAll: '( ';
		print: key;
		space;
		nextPutAll: '->';
		space;
		print: value;
		nextPutAll: ' )'!

sortAspect

	^name!

updateVariable: listPresenter debugger: debugger
	listPresenter list
		do: [:variableService | variableService _key = key ifTrue: [variableService replicateFrom: self]].
	listPresenter list: listPresenter list.
	listPresenter view invalidate.!

value

	^value! !
!RowanVariableService categoriesFor: #_key!public! !
!RowanVariableService categoriesFor: #className!public! !
!RowanVariableService categoriesFor: #displayName!displaying!public! !
!RowanVariableService categoriesFor: #displayOop!printing!public! !
!RowanVariableService categoriesFor: #displayStringOn:!printing!public! !
!RowanVariableService categoriesFor: #isDisplayLabel!public!testing! !
!RowanVariableService categoriesFor: #isUpdatableService!public!testing! !
!RowanVariableService categoriesFor: #key!public! !
!RowanVariableService categoriesFor: #name!accessing!public! !
!RowanVariableService categoriesFor: #name:!accessing!public! !
!RowanVariableService categoriesFor: #oop!public! !
!RowanVariableService categoriesFor: #printOn:!printing!public! !
!RowanVariableService categoriesFor: #sortAspect!accessing!public! !
!RowanVariableService categoriesFor: #updateVariable:debugger:!public!updating! !
!RowanVariableService categoriesFor: #value!public! !

RowanSpecificationService guid: (GUID fromString: '{c09518cc-ba47-4785-b288-a1bea7cfaac6}')!
RowanSpecificationService comment: ''!
!RowanSpecificationService categoriesForClass!Unclassified! !
RowanDefinedProjectService guid: (GUID fromString: '{10fbdfa7-183d-43e5-b1f1-18b76d206b16}')!
RowanDefinedProjectService comment: ''!
!RowanDefinedProjectService categoriesForClass!Unclassified! !
!RowanDefinedProjectService methodsFor!

isDefinedProject

	^true!

newProject: presenter
	presenter list detect: [:projectService | projectService = self]
		ifNone: 
			[presenter list add: self.
			presenter view invalidate]! !
!RowanDefinedProjectService categoriesFor: #isDefinedProject!public!testing! !
!RowanDefinedProjectService categoriesFor: #newProject:!public!updating! !

RowanLoadedProjectService guid: (GUID fromString: '{03220f16-85c1-4112-a8d6-1b004d26424a}')!
RowanLoadedProjectService comment: ''!
!RowanLoadedProjectService categoriesForClass!Unclassified! !
RowanResolvedProjectService guid: (GUID fromString: '{a8e74b3d-49f3-45fc-b281-23ee7f83cb31}')!
RowanResolvedProjectService comment: ''!
!RowanResolvedProjectService categoriesForClass!Unclassified! !
JadeiteTreeNode guid: (GUID fromString: '{94ece110-8aca-4f84-9f75-546040c70901}')!
JadeiteTreeNode comment: 'The inst var `marked` is used when updating a JadeiteTreeModel. 
After a node is added, it is marked.  Nodes that are not marked are subsequently removed. '!
!JadeiteTreeNode categoriesForClass!MVP-Models-Support! !
!JadeiteTreeNode methodsFor!

initialize
	super initialize.
	visited := true!

resetVisited

	visited := false!

visited
	^visited!

visited: anObject
	visited := anObject! !
!JadeiteTreeNode categoriesFor: #initialize!initialization!public! !
!JadeiteTreeNode categoriesFor: #resetVisited!public! !
!JadeiteTreeNode categoriesFor: #visited!accessing!private! !
!JadeiteTreeNode categoriesFor: #visited:!accessing!private! !

"Binary Globals"!

