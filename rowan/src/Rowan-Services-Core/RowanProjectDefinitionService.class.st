Class {
	#name : 'RowanProjectDefinitionService',
	#superclass : 'RowanDefinitionService',
	#instVars : [
		'definitionOop',
		'componentDefinitions',
		'packageDefinitions',
		'loadSpecification'
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
RowanProjectDefinitionService >> renameComponent: aComponentPath to: aComponentName [
	self definition renameComponentNamed: aComponentPath to: aComponentName.
	RowanCommandResult addResult: self
]

{ #category : 'updating' }
RowanProjectDefinitionService >> update [

	self initializeComponentDefinitions.
	RowanCommandResult addResult: self
]
