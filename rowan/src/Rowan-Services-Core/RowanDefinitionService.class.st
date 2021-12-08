Class {
	#name : 'RowanDefinitionService',
	#superclass : 'RowanService',
	#instVars : [
		'name',
		'properties'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'accessing' }
RowanDefinitionService class >> named: aName [

	^self new name: aName
]

{ #category : 'accessing' }
RowanDefinitionService >> name [
	^name
]

{ #category : 'accessing' }
RowanDefinitionService >> name: object [
	name := object
]

{ #category : 'accessing' }
RowanDefinitionService >> properties [
	^properties
]

{ #category : 'accessing' }
RowanDefinitionService >> properties: object [
	properties := object
]
