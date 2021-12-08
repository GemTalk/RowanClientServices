Class {
	#name : 'RowanProjectDefinitionService',
	#superclass : 'RowanDefinitionService',
	#instVars : [
		'definition',
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

{ #category : 'updating' }
RowanProjectDefinitionService >> update [
	"|  componentNames |"
	super update.
	definition := (Rowan projectNamed: name) defined. 
	RowanCommandResult addResult: self
	"self halt. 
	componentNames := definition componentNames.
	componentNames do:[:componentName | 
		| component |
		component := projectDefinition componentNamed: componentName].

(projectDefinition componentNamed: 'Core')  componentNames "
]
