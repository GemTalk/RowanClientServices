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

	| projectDefinition |
	projectDefinition := projectDefinitionService definition. 
	self halt.
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
