Class {
	#name : 'RowanPackageGroupService',
	#superclass : 'RowanComponentDefinitionService',
	#instVars : [
		'condition',
		'packageServices'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'instance creation' }
RowanPackageGroupService class >> forPackageGroupNamed: packageGroupName projectDefinition: projectDefinition [
	| inst |
	inst := self new name: packageGroupName.
	inst
		condition: (projectDefinition packageGroupNamed: packageGroupName) condition.
	inst computePackageNames: projectDefinition.
	^ inst
]

{ #category : 'client commands support' }
RowanPackageGroupService >> computePackageNames: projectDefinition [
	| packageGroup |
	packageGroup := projectDefinition packageGroupNamed: name.
	packageNames := packageGroup packageNames.
]

{ #category : 'accessing' }
RowanPackageGroupService >> condition [
	^condition
]

{ #category : 'accessing' }
RowanPackageGroupService >> condition: object [
	condition := object
]

{ #category : 'updating' }
RowanPackageGroupService >> update [
	packageServices := packageNames
		collect: [ :packageName | (RowanPackageService forPackageNamed: packageName) update ].
	wasUpdated := true. 
	RowanCommandResult addResult: self.
]
