Class {
	#name : 'RowanProjectDefinitionService',
	#superclass : 'RowanDefinitionService',
	#instVars : [
		'definitionOop',
		'componentDefinitions',
		'packageDefinitions',
		'loadSpecification',
		'specService'
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
RowanProjectDefinitionService >> addSubcomponentNamed: componentName toComponentNamed: toComponentName [
	self rowanFixMe.	"What should the default condition be?"
	self definition
		addSubcomponentNamed: componentName
		condition: 'common'
		toComponentNamed: toComponentName.
	self initializeComponentDefinitions.
	RowanCommandResult addResult: self
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
					componentDefinitionService addFlattenedHierarchyTo: componentDictionary. 
					componentDefinitionService ]).

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

{ #category : 'initialization' }
RowanProjectDefinitionService >> initializeComponentDefinitions [
	self
		componentDefinitions: (self componentDefinitionServicesFor: self definition)
]

{ #category : 'accessing' }
RowanProjectDefinitionService >> loadSpecification [
	^loadSpecification
]

{ #category : 'accessing' }
RowanProjectDefinitionService >> loadSpecification: object [
	loadSpecification := object
]

{ #category : 'accessing' }
RowanProjectDefinitionService >> packageDefinitions [
	^packageDefinitions
]

{ #category : 'accessing' }
RowanProjectDefinitionService >> packageDefinitions: object [
	packageDefinitions := object
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
RowanProjectDefinitionService >> setLoadSpecProperty: property to: value [
	self definition loadSpecification
		perform: (property , ':') asSymbol
		with: value
]

{ #category : 'updating' }
RowanProjectDefinitionService >> update [
	self initializeComponentDefinitions.
	specService := RowanSpecificationService new
		initialize: self definition loadSpecification asOop.
	RowanCommandResult addResult: self
]
