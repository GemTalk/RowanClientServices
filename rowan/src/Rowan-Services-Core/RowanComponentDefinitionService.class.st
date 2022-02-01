Class {
	#name : 'RowanComponentDefinitionService',
	#superclass : 'RowanDefinitionService',
	#instVars : [
		'subcomponentDefinitions',
		'packageNames',
		'preloadDoitName',
		'projectDefinitionService',
		'postloadDoitName',
		'projectNames',
		'comment'
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
	(projectDefinition componentNamed: componentName ifAbsent: [ ^ nil ])
		ifNotNil: [ self setPrePostDoItsFor: inst component: component ].
	subcomponentNames := component
		ifNil: [ Array new ]
		ifNotNil: [ component componentNames ].
	inst
		subcomponentDefinitions:
			(subcomponentNames
				collect: [ :subcomponentName | 
					(projectDefinition componentNamed: componentName ifAbsent: [  ])
						ifNotNil: [ self forComponentNamed: subcomponentName projectDefinition: projectDefinition ] ])
				asSet asArray.
	^ inst
]

{ #category : 'private' }
RowanComponentDefinitionService class >> setPrePostDoItsFor: inst component: component [
	inst
		preloadDoitName:
			(component preloadDoitName
				ifNotNil: [ :theName | theName -> (component doitDict at: theName) ]).
	inst
		postloadDoitName:
			(component postloadDoitName
				ifNotNil: [ :theName | theName -> (component doitDict at: theName) ])
]

{ #category : 'other' }
RowanComponentDefinitionService >> addFlattenedHierarchyTo: dictionary [
	dictionary at: self put: subcomponentDefinitions.
	subcomponentDefinitions
		do: [ :componentService | componentService addFlattenedHierarchyTo: dictionary ]
]

{ #category : 'client commands support' }
RowanComponentDefinitionService >> computePackageNames [
	self computePackageNames: projectDefinitionService definition
]

{ #category : 'client commands support' }
RowanComponentDefinitionService >> computePackageNames: projectDefinition [
	| component |
	component := projectDefinition
		componentNamed: name
		ifAbsent: [ ^ packageNames := Array new ].
	packageNames := component packageNames
]

{ #category : 'instance creation' }
RowanComponentDefinitionService >> forComponentNamed: componentName projectDefinition: projectDefinition [
	| inst subcomponentNames component |
	inst := self new.
	inst name: componentName.
	component := projectDefinition
		componentOrPackageGroupNamed: componentName
		ifAbsent: [ nil ].
	(projectDefinition componentNamed: componentName ifAbsent: [ ^nil ])
		ifNotNil: [ 
			self setPrePostDoItsFor: inst component: component ].
	subcomponentNames := component
		ifNil: [ Array new ]
		ifNotNil: [ component componentNames ].
	inst
		subcomponentDefinitions:
			(subcomponentNames
				collect: [ :subcomponentName | self forComponentNamed: subcomponentName projectDefinition: projectDefinition ]) asSet asArray.
	^ inst
]

{ #category : 'client commands' }
RowanComponentDefinitionService >> packageNames [
	self computePackageNames. 
	RowanCommandResult addResult: self.
]

{ #category : 'accessing' }
RowanComponentDefinitionService >> packageNames: object [
	packageNames := object
]

{ #category : 'accessing' }
RowanComponentDefinitionService >> postloadDoitName [
	^postloadDoitName
]

{ #category : 'accessing' }
RowanComponentDefinitionService >> postloadDoitName: object [
	postloadDoitName := object
]

{ #category : 'accessing' }
RowanComponentDefinitionService >> preloadDoitName [
	^preloadDoitName
]

{ #category : 'accessing' }
RowanComponentDefinitionService >> preloadDoitName: object [
	preloadDoitName := object
]

{ #category : 'accessing' }
RowanComponentDefinitionService >> subcomponentDefinitions [
	^subcomponentDefinitions
]

{ #category : 'accessing' }
RowanComponentDefinitionService >> subcomponentDefinitions: object [
	subcomponentDefinitions := object
]
