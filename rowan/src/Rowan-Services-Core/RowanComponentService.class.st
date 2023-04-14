Class {
	#name : 'RowanComponentService',
	#superclass : 'RowanService',
	#instVars : [
		'name',
		'componentServices',
		'packageServices',
		'projectService',
		'basename'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'instance creation' }
RowanComponentService class >> forComponentNamed: componentName projectService: theProjectService [
	| inst component |
	component := theProjectService rwProject
		componentOrPackageGroupNamed: componentName.
	inst := self new
		name: componentName;
		basename: component label.
	inst computeSubComponentsUsingProjectService: theProjectService.
	inst projectService: theProjectService.
	^ inst
]

{ #category : 'rsr' }
RowanComponentService class >> templateClassName [

	^#RowanComponentService
]

{ #category : 'operations' }
RowanComponentService >> addFlattenedHierarchyTo: dictionary [
	| sortedComponents |
	sortedComponents := componentServices
		asSortedCollection: [ :x :y | x name < y name ].
	dictionary at: self put: sortedComponents asArray.
	componentServices
		do: [ :componentService | componentService addFlattenedHierarchyTo: dictionary ]
]

{ #category : 'accessing' }
RowanComponentService >> basename [
	^basename
]

{ #category : 'accessing' }
RowanComponentService >> basename: object [
	basename := object
]

{ #category : 'accessing' }
RowanComponentService >> component [
	^ projectService rwProject componentNamed: name
]

{ #category : 'accessing' }
RowanComponentService >> componentServices [
	^componentServices
]

{ #category : 'accessing' }
RowanComponentService >> componentServices: anArray [

	componentServices := anArray
]

{ #category : 'operations' }
RowanComponentService >> computeSubComponentsUsingProjectService: theProjectService [
	componentServices := (theProjectService rwProject
		loadedSubcomponentsOf: name
		ifNone: [ Array new ])
		collect: [ :subcomponent | 
			RowanComponentService
				forComponentNamed: subcomponent name
				projectService: theProjectService ]
]

{ #category : 'instance creation' }
RowanComponentService >> forComponentNamed: componentName projectService: theProjectService [
	| inst component |
	component := theProjectService rwProject componentOrPackageGroupNamed: componentName.
	inst := self new
		name: componentName;
		basename: component label.
	inst computeSubComponentsUsingProjectService: theProjectService.
	inst projectService: theProjectService. 
	^ inst
]

{ #category : 'initialization' }
RowanComponentService >> initialize [
	" to do "
	super initialize.
	componentServices := Array new.
]

{ #category : 'accessing' }
RowanComponentService >> name [
	^name
]

{ #category : 'accessing' }
RowanComponentService >> name: object [
	name := object
]

{ #category : 'accessing' }
RowanComponentService >> projectService [
	^projectService
]

{ #category : 'accessing' }
RowanComponentService >> projectService: theProjectService [
	projectService := theProjectService
]

{ #category : 'updating' }
RowanComponentService >> update [
	super update.
	self updatePackageServices
]

{ #category : 'clientCommands' }
RowanComponentService >> updatePackageServices [
	packageServices := (projectService rwProject allPackageNamesIn: name)
		collect: [ :packageName | RowanPackageService forPackageNamed: packageName ].
	RowanCommandResult addResult: self
]
