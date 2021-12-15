Class {
	#name : 'RowanComponentDefinitionService',
	#superclass : 'RowanDefinitionService',
	#instVars : [
		'subcomponentDefinitions',
		'packageNames',
		'projectDefinitionService'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'client commands support' }
RowanComponentDefinitionService >> computePackageNames [
	| projectDefinition component |
	projectDefinition := projectDefinitionService definition.
	component := projectDefinition componentNamed: name.
	packageNames := component packageNames
]

{ #category : 'client commands' }
RowanComponentDefinitionService >> packageNames [
	packageNames ifNil: [self computePackageNames]. 
	RowanCommandResult addResult: self.
]

{ #category : 'accessing' }
RowanComponentDefinitionService >> subcomponentDefinitions [
	^subcomponentDefinitions
]

{ #category : 'accessing' }
RowanComponentDefinitionService >> subcomponentDefinitions: object [
	subcomponentDefinitions := object
]
