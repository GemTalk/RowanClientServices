Class {
	#name : 'RowanSubcomponentDefinitionService',
	#superclass : 'RowanComponentDefinitionService',
	#instVars : [
		'condition'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'accessing' }
RowanSubcomponentDefinitionService >> setConditionFrom: component [

	condition := component condition
]
