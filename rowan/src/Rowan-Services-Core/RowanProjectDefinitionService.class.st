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
		'loadSpecification',
		'specService',
		'comment',
		'conditionalAttributes',
		'platformProperties',
		'packageGroups'
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
RowanProjectDefinitionService >> addPackageNamed: packageName toComponentNamed: componentName [
	| componentDefinitionService |
	self definition addPackageNamed: packageName toComponentNamed: componentName.
	componentDefinitionService := RowanComponentDefinitionService forComponentNamed: componentName projectDefinition: self definition.
	componentDefinitionService computePackageNames: self definition.
	RowanCommandResult addResult: componentDefinitionService
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> addPackagesNamed: packageNames toPackageGroupNamed: packageGroupName [
	| packageGroupService |
	(self definition packageGroupNamed: packageGroupName) addPackageNames: packageNames.
	packageGroupService := RowanPackageGroupService forPackageGroupNamed: packageGroupName projectDefinition: self definition.
	packageGroupService computePackageNames: self definition.
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
RowanProjectDefinitionService >> componentDefinitions [
	^componentDefinitions
]

{ #category : 'accessing' }
RowanProjectDefinitionService >> componentDefinitions: object [
	componentDefinitions := object
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
					componentDefinitionService := RowanComponentDefinitionService
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
	definitionOop := (Rowan projectNamed: name) defined asOop.
	handle
		ifNotNil: [ RowanBrowserService new saveRootObject: definitionOop windowHandle: handle ].
	RowanCommandResult addResult: self
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
RowanProjectDefinitionService >> initializeComponentDefinitions [
	self
		componentDefinitions: (self componentDefinitionServicesFor: self definition)
]

{ #category : 'initialization' }
RowanProjectDefinitionService >> initializePackageGroups [
	self
		packageGroups:
			(self definition packageGroupNames
				collect: [ :theName | 
					RowanPackageGroupService
						forPackageGroupNamed: theName
						projectDefinition: self definition ])
]

{ #category : 'accessing' }
RowanProjectDefinitionService >> loadSpecification [
	^loadSpecification
]

{ #category : 'accessing' }
RowanProjectDefinitionService >> loadSpecification: object [
	loadSpecification := object
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

{ #category : 'client commands' }
RowanProjectDefinitionService >> removeComponentNamed: componentName [
	self definition removeComponentNamed: componentName.
	self initializeComponentDefinitions.
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanProjectDefinitionService >> renameComponent: aComponentPath to: aComponentName [
	self definition renameComponentNamed: aComponentPath to: aComponentName.
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

{ #category : 'updating' }
RowanProjectDefinitionService >> update [
	self initializeComponentDefinitions.
	self initializePackageGroups. 
	self updateLoadSpecService.
	comment := self definition comment. 
	conditionalAttributes := self definition customConditionalAttributes asOrderedCollection. 
	self updatePlatformProperties. 
	RowanCommandResult addResult: self
]

{ #category : 'updating' }
RowanProjectDefinitionService >> updateLoadSpecService [
	specService := RowanSpecificationService new
		initialize: self definition loadSpecification asOop.
	specService removeHiddenAttributes
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
