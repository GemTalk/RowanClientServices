Class {
	#name : 'RowanComponentDefinitionService',
	#superclass : 'RowanDefinitionService',
	#instVars : [
		'subcomponentDefinitions',
		'packageNames',
		'projectDefinitionService',
		'preloadDoitName',
		'postloadDoitName',
		'projectNames'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'instance creation' }
RowanComponentDefinitionService class >> forComponentNamed: componentName projectDefinition: projectDefinition [
	| inst subcomponentNames component |
	inst := self new.
	inst name: componentName.
	component := projectDefinition
		componentOrPackageGroupNamed: componentName
		ifAbsent: [ nil ].
	subcomponentNames := component
		ifNil: [ Array new ]
		ifNotNil: [ component componentNames ].
	inst
		subcomponentDefinitions:
			(subcomponentNames
				collect: [ :subcomponentName | self forComponentNamed: subcomponentName projectDefinition: projectDefinition ]).
	^ inst
]

{ #category : 'other' }
RowanComponentDefinitionService >> addFlattenedHierarchyTo: dictionary [
	dictionary at: self put: subcomponentDefinitions.
	subcomponentDefinitions
		do: [ :componentService | componentService addFlattenedHierarchyTo: dictionary ]
]

{ #category : 'client commands support' }
RowanComponentDefinitionService >> computePackageNames [
	| projectDefinition component |
	projectDefinition := projectDefinitionService definition.
	component := projectDefinition
		componentNamed: name
		ifAbsent: [ ^ packageNames := Array new ].
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
