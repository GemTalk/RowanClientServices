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

{ #category : 'accessing' }
RowanProjectDefinitionService >> initializeComponentDefinitions [
	self componentDefinitions: (self componentDefinitionServicesFor: self definition)
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

{ #category : 'accessing' }
RowanProjectDefinitionService >> update [

	self initializeComponentDefinitions.
	RowanCommandResult addResult: self
]
