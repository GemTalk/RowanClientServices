Class {
	#name : 'RowanPackageGroupService',
	#superclass : 'RowanComponentDefinitionService',
	#instVars : [
		'condition',
		'packageServices'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'constants' }
RowanPackageGroupService class >> allPackagesGroupName [
	^'All Packages'
]

{ #category : 'instance creation' }
RowanPackageGroupService class >> forPackageGroupNamed: packageGroupName projectDefinition: projectDefService [
	| inst projectDefinition |
	inst := self new name: packageGroupName; projectDefinitionService: projectDefService.
	projectDefinition := projectDefService definition. 
	inst condition: (projectDefinition packageGroupNamed: packageGroupName) condition.
	inst computePackageNames.
	^ inst
]

{ #category : 'client commands support' }
RowanPackageGroupService >> computePackageNames [
	name = self class allPackagesGroupName
		ifTrue: [ packageNames := self projectDefinition packageNames ]
		ifFalse: [ 
			| packageGroup |
			packageGroup := self projectDefinition packageGroupNamed: name.
			packageNames := packageGroup packageNames ]
]

{ #category : 'accessing' }
RowanPackageGroupService >> condition [
	^condition
]

{ #category : 'accessing' }
RowanPackageGroupService >> condition: object [
	condition := object
]

{ #category : 'replication' }
RowanPackageGroupService >> excludedInstVars [

	^super excludedInstVars, #( #projectDefinition)
]

{ #category : 'accessing' }
RowanPackageGroupService >> projectDefinition [
	^projectDefinitionService initializeDefinitionOop definition
]

{ #category : 'updating' }
RowanPackageGroupService >> update [
	self computePackageNames. 
	packageServices := packageNames
		collect: [ :packageName | (RowanPackageService forPackageNamed: packageName) update ].
	wasUpdated := true. 
	RowanCommandResult addResult: self.
]
