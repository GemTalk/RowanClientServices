Class {
	#name : 'RowanComponentDefinitionService',
	#superclass : 'RowanDefinitionService',
	#instVars : [
		'subcomponentDefinitions'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'accessing' }
RowanComponentDefinitionService >> subcomponentDefinitions [
	^subcomponentDefinitions
]

{ #category : 'accessing' }
RowanComponentDefinitionService >> subcomponentDefinitions: object [
	subcomponentDefinitions := object
]
