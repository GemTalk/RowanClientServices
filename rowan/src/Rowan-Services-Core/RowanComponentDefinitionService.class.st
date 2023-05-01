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

{ #category : 'private' }
RowanComponentDefinitionService class >> componentDefinitionServiceClassFor: theComponent [
	^ (theComponent isKindOf: RwPlatformSubcomponent)
		ifTrue: [ RowanPlatformSubcomponentDefinitionService ]
		ifFalse: [ 
			(theComponent isKindOf: RwSubcomponent)
				ifTrue: [ RowanSubcomponentDefinitionService ]
				ifFalse: [ RowanComponentDefinitionService ] ]
]

{ #category : 'instance creation' }
RowanComponentDefinitionService class >> forComponentNamed: componentName projectDefinition: projectDefinition [
	| inst subcomponentNames component |
	inst := self new.
	inst name: componentName.
	component := projectDefinition
		componentOrPackageGroupNamed: componentName
		ifAbsent: [ nil ].
	(projectDefinition componentNamed: componentName ifAbsent: [ ^ nil ])
		ifNotNil: [ 
			inst setPrePostDoItsFrom: component.
			inst setConditionFrom: component ].
	subcomponentNames := component
		ifNil: [ Array new ]
		ifNotNil: [ component componentNames ].
	inst
		subcomponentDefinitions:
			(subcomponentNames
				collect: [ :subcomponentName | 
					| subComponent |
					(subComponent := projectDefinition
						componentNamed: subcomponentName
						ifAbsent: [  ])
						ifNotNil: [ 
							(self componentDefinitionServiceClassFor: subComponent)
								forComponentNamed: subcomponentName
								projectDefinition: projectDefinition ] ]) asSet asArray.
	^ inst
]

{ #category : 'rsr' }
RowanComponentDefinitionService class >> templateClassName [

	^#RowanComponentDefinitionService
]

{ #category : 'other' }
RowanComponentDefinitionService >> addFlattenedHierarchyTo: dictionary [
	| sortedSubcomponents |
	sortedSubcomponents := subcomponentDefinitions asSortedCollection:[:x :y | x name < y name]. 
	dictionary at: self put: sortedSubcomponents asArray.
	sortedSubcomponents
		do: [ :componentService | componentService addFlattenedHierarchyTo: dictionary ]
]

{ #category : 'client commands support' }
RowanComponentDefinitionService >> computePackageNames [
	self computePackageNames: projectDefinitionService definition
]

{ #category : 'client commands support' }
RowanComponentDefinitionService >> computePackageNames: projectDefinition [
	| component  |
	component := projectDefinition
		componentNamed: name
		ifAbsent: [ ^ packageNames := Array new ].
	packageNames := component packageNames
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
RowanComponentDefinitionService >> projectDefinitionService [
	^projectDefinitionService
]

{ #category : 'accessing' }
RowanComponentDefinitionService >> projectDefinitionService: object [
	projectDefinitionService := object
]

{ #category : 'accessing' }
RowanComponentDefinitionService >> setConditionFrom: component [

	"only subcomponents have conditions"
]

{ #category : 'accessing' }
RowanComponentDefinitionService >> setPrePostDoItsFrom: component [
	self
		preloadDoitName:
			(component preloadDoitName
				ifNotNil: [ :theName | theName -> (component doitDict at: theName) ]).
	self
		postloadDoitName:
			(component postloadDoitName
				ifNotNil: [ :theName | theName -> (component doitDict at: theName) ])
]

{ #category : 'accessing' }
RowanComponentDefinitionService >> subcomponentDefinitions [
	^subcomponentDefinitions
]

{ #category : 'accessing' }
RowanComponentDefinitionService >> subcomponentDefinitions: object [
	subcomponentDefinitions := object
]
