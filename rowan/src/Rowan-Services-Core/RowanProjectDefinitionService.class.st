"
RowanProjectDefinitionService presents a gui-ready presentation of a Rowan project definition and load specification.
"
Class {
	#name : 'RowanProjectDefinitionService',
	#superclass : 'RowanDefinitionService',
	#instVars : [
		'definitionOop',
		'componentDefinitions',
		'packageDefinitions',
		'specService',
		'comment',
		'conditionalAttributes',
		'platformProperties',
		'packageGroups',
		'showLoadedPackageGroupsOnly',
		'definition'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'client commands' }
RowanProjectDefinitionService >> addComponent: componentName [
	self definition addLoadComponentNamed: componentName.
	self initializeComponentDefinitions.
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> addPackageGroupNamed: aComponentName [
	"for adding new package groups. condition and comment added later by the user"

	self definition
		addPackageGroupNamed: aComponentName
		condition: String new
		comment: String new
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> addPackageGroupNamed: packageGroupName condition: condition comment: aString [
	(self definition packageGroupNamed: packageGroupName) condition: condition.
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> addPackageNamed: packageName toComponentNamed: componentName [
	| componentDefinitionService |
	self definition addPackageNamed: packageName toComponentNamed: componentName.
	componentDefinitionService := (self
		componentDefinitionServiceClassFor: componentName)
		forComponentNamed: componentName
		projectDefinition: self definition.
	componentDefinitionService computePackageNames: self definition.
	RowanCommandResult addResult: componentDefinitionService
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> addPackagesNamed: packageNames toPackageGroupNamed: packageGroupName [
	| packageGroupService |
	(self definition packageGroupNamed: packageGroupName) addPackageNames: packageNames.
	packageGroupService := self packageGroupServiceClass forPackageGroupNamed: packageGroupName projectDefinition: self.
	RowanCommandResult addResult: packageGroupService
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> addPostloadDoitName: doitName withSource: doitSource toComponentNamed: aComponentName [
	self definition
		addPostloadDoitName: doitName
		withSource: doitSource
		toComponentNamed: aComponentName.
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> addPreloadDoitName: doitName withSource: doitSource toComponentNamed: aComponentName [
	self definition
		addPreloadDoitName: doitName
		withSource: doitSource
		toComponentNamed: aComponentName.
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> addSubcomponentNamed: componentName toComponentNamed: toComponentName [
	self rowanFixMe.	"What should the default condition be?"
	self definition
		addSubcomponentNamed: componentName
		condition: 'common'
		toComponentNamed: toComponentName.
	self initializeComponentDefinitions.
	RowanCommandResult addResult: self
]

{ #category : 'constants' }
RowanProjectDefinitionService >> allUsersName [

	^self definition loadSpecification _gemstoneAllUsersName
]

{ #category : 'accessing' }
RowanProjectDefinitionService >> comment: projectComment [
	self definition loadSpecification comment: projectComment
]

{ #category : 'accessing' }
RowanProjectDefinitionService >> componentDefinitions [
	^componentDefinitions
]

{ #category : 'accessing' }
RowanProjectDefinitionService >> componentDefinitions: object [
	componentDefinitions := object
]

{ #category : 'accessing' }
RowanProjectDefinitionService >> componentDefinitionServiceClassFor: componentName [
	| theComponent |
	theComponent := self definition componentNamed: componentName.
	^ RowanComponentDefinitionService
		componentDefinitionServiceClassFor: theComponent
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> componentDefinitionServicesFor: projectDefinition [
	| componentDictionary |
	componentDictionary := Dictionary new.
	componentDictionary
		at: #'nil'
		put:
			(projectDefinition componentNames
				collect: [ :componentName | 
					| componentDefinitionService |
					componentDefinitionService := (self
						componentDefinitionServiceClassFor: componentName)
						forComponentNamed: componentName
						projectDefinition: projectDefinition.
					componentDefinitionService
						ifNotNil: [ 
							componentDefinitionService addFlattenedHierarchyTo: componentDictionary.
							componentDefinitionService ] ]) asSet asArray.	"eliminate nils"
	^ componentDictionary
]

{ #category : 'accessing' }
RowanProjectDefinitionService >> definition [

	^Object _objectForOop: definitionOop
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> editInWindow: handle [
	self initializeDefinitionOop.
	handle
		ifNotNil: [ 
			RowanBrowserService new saveRootObject: self asOop windowHandle: handle.
			RowanBrowserService new saveRootObject: definitionOop windowHandle: handle ].
	RowanCommandResult addResult: self
]

{ #category : 'replication' }
RowanProjectDefinitionService >> excludedInstVars [
	^ super excludedInstVars , #(#'definition')
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> export [

	self definition resolveProject export
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> exportLoadSpecification [
	self definition resolveProject exportLoadSpecification
]

{ #category : 'initialization' }
RowanProjectDefinitionService >> initialize [

	super initialize. 
	showLoadedPackageGroupsOnly := true.
]

{ #category : 'initialization' }
RowanProjectDefinitionService >> initializeComponentDefinitions [
	self
		componentDefinitions: (self componentDefinitionServicesFor: self definition)
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> initializeDefinitionOop [
	definitionOop
		ifNil: [ 
			definition := (Rowan projectNamed: name) defined.
			definitionOop := definition asOop ]
]

{ #category : 'initialization' }
RowanProjectDefinitionService >> initializePackageGroups [
	definitionOop ifNil: [self initializeDefinitionOop].
	self
		packageGroups:
			(self definition packageGroupNames
				collect: [ :theName | 
					self packageGroupServiceClass
						forPackageGroupNamed: theName
						projectDefinition: self ]).
	wasUpdated := true.
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> movePackageNamed: aPackageName toComponentNamed: aComponentName [
	self definition movePackageNamed: aPackageName toComponentNamed: aComponentName
]

{ #category : 'accessing' }
RowanProjectDefinitionService >> packageDefinitions [
	^packageDefinitions
]

{ #category : 'accessing' }
RowanProjectDefinitionService >> packageDefinitions: object [
	packageDefinitions := object
]

{ #category : 'accessing' }
RowanProjectDefinitionService >> packageGroups [
	^packageGroups
]

{ #category : 'accessing' }
RowanProjectDefinitionService >> packageGroups: object [
	packageGroups := object
]

{ #category : 'accessing' }
RowanProjectDefinitionService >> packageGroupServiceClass [
	^ showLoadedPackageGroupsOnly
		ifTrue: [ RowanPackageGroupService ]
		ifFalse: [ RowanPackageGroupDefinitionService ]
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> removeComponentNamed: componentName [
	self definition removeComponentNamed: componentName.
	self initializeComponentDefinitions.
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> removePackageGroupNamed: aComponentName [
	self definition removePackageGroupNamed: aComponentName
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> removePackageGroupsNamed: packageGroupNames [
	packageGroupNames
		do: [ :packageGroupName | self definition removePackageGroupNamed: packageGroupName].
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> removePackagesFromProjectDefinition: packageNames [
	packageNames
		do: [ :packageName | self definition removePackageNamed: packageName ]
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> removePackagesNamed: packageNames fromPackageGroupNamed: packageGroupName [
	packageNames
		do: [ :packageName | 
			(self definition packageGroupNamed: packageGroupName)
				removePackageNamed: packageName ]
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> renameComponent: aComponentPath to: aComponentName [
	self definition renameComponentNamed: aComponentPath to: aComponentName.
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> renamePackageGroup: packageGroupName to: newPackageGroupName [
	self definition renamePackageGroupNamed: packageGroupName to: newPackageGroupName.
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> renamePackageNamed: packageName to: newPackageName [
	self definition renamePackageNamed: packageName to: newPackageName.
	self update. 
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> setConditionalAttributes: array [
	self definition customConditionalAttributes: array
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> setLoadSpecProperty: property to: value [
	self definition loadSpecification
		perform: (property , ':') asSymbol
		with: value
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> setPlatformProperty: property to: value [
	property = #'defaultMethodEnv'
		ifTrue: [ self definition loadSpecification gemstoneSetDefaultMethodEnvTo: value ].
	property = #'defaultSymbolDict'
		ifTrue: [ self definition loadSpecification gemstoneSetDefaultSymbolDictNameTo: value ].

	property = #'useSessionMethods'
		ifTrue: [ 
			self definition loadSpecification
				gemstoneSetDefaultUseSessionMethodsForExtensionsTo: value ]
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> subcomponentNamed: subcomponentName condition: condition [
	| subcomponent |
	subcomponent := self definition componentNamed: subcomponentName.
	subcomponent condition: condition.
]

{ #category : 'updating' }
RowanProjectDefinitionService >> update [
	self initializeComponentDefinitions.
	self initializePackageGroups. 
	self updateLoadSpecService.
	comment := self definition loadSpecification comment. 
	conditionalAttributes := self definition customConditionalAttributes asOrderedCollection. 
	self updatePlatformProperties. 
	RowanCommandResult addResult: self
]

{ #category : 'updating' }
RowanProjectDefinitionService >> updateLoadSpecService [
	specService := RowanLoadSpecService new
		initialize: self definition loadSpecification asOop.
	specService removeHiddenAttributes.
]

{ #category : 'updating' }
RowanProjectDefinitionService >> updatePlatformProperties [
	platformProperties := Dictionary new.
	platformProperties
		at: #'defaultMethodEnv'
		put:
			(self definition loadSpecification
				gemstoneDefaultMethodEnvForUser: self allUsersName).
	platformProperties
		at: #'defaultSymbolDict'
		put: self definition loadSpecification gemstoneDefaultSymbolDictName.
	platformProperties
		at: #'useSessionMethods'
		put:
			(self definition loadSpecification
				gemstoneDefaultUseSessionMethodsForExtensionsForUser: self allUsersName)
]
