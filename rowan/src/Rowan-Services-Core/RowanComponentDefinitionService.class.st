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
	| inst subcomponentNames |
	inst := self new.
	inst name: componentName.
	subcomponentNames := (projectDefinition componentOrPackageGroupNamed: componentName)
		componentNames.
	inst subcomponentDefinitions: (subcomponentNames
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
