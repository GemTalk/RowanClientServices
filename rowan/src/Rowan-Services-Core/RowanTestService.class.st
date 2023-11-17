"
No class-specific documentation for RowanTestService, hierarchy is:
Object
  RowanService( definition updates command commandArgs updateType organizer)
    RowanTestService

"
Class {
	#name : 'RowanTestService',
	#superclass : 'RowanService',
	#instVars : [
		'name'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'rsr' }
RowanTestService class >> templateClassName [

	^#RowanTestService
]

{ #category : 'sample projects' }
RowanTestService >> addRowanSample1Class [
	| packageService |
	packageService := RowanPackageService new name: 'RowanSample1-Core'.
	packageService
		createClassNamed: 'RowanSample1'
		superclass: 'Object'
		instVars: Array new
]

{ #category : 'sample projects' }
RowanTestService >> addRowanSample1Methods [
	| classService |
	classService := RowanClassService new
		name: 'RowanSample1';
		update.
	classService organizer: organizer.
	classService saveMethodSource: 'foo ^''foo''' category: 'accessing'.
	classService meta: true.
	classService
		saveMethodSource: 'bar ^''bar'''
		category: '*rowansample1-extensions'
]

{ #category : 'sample projects' }
RowanTestService >> addRowanSample1Test [
	| packageService |
	packageService := RowanPackageService new name: 'RowanSample1-Tests'.
	packageService
		createClassNamed: 'RowanSample1Test'
		superclass: 'TestCase'
		instVars: Array new
]

{ #category : 'sample projects' }
RowanTestService >> addRowanSample1TestMethods [

	| classService |
	classService := RowanClassService new name: 'RowanSample1Test';
		update.
	classService organizer: organizer. 
	classService saveMethodSource: 'test1
	
	self assert: RowanSample1 new foo = ''foo''' category: 'tests'.
		
	classService saveMethodSource: 'test2

	self assert: RowanSample1 bar = ''bar''' category: 'tests'.
	
	classService saveMethodSource: 'testError

	self assert: RowanSample1 new bar = ''bar''' category: 'tests'.
	
	classService saveMethodSource: 'testFailure

	self assert: RowanSample1 new foo = ''bar''' category: 'tests'.
]

{ #category : 'sample projects' }
RowanTestService >> createRowanSample1Project [
	| projectService  |
	projectService := RowanProjectService new name: 'RowanSample1'.
	projectService
		createProjectComponent: 'Core'
		symDict: 'SampleSymbolDict'
		convention: 'RowanHybrid'
		format: 'tonel'
		projectsHome: '$ROWAN_PROJECTS_HOME'
		type: #'none'
		shouldWrite: false.
	projectService
		addPackagesNamed: self rowanSample1PackageNames.
	self addRowanSample1Class.
	self addRowanSample1Methods.  
	self addRowanSample1Test.
	self addRowanSample1TestMethods.
	RowanBrowserService new updateProjects
]

{ #category : 'sample projects' }
RowanTestService >> rowanSample1PackageNames [

	^ self rowanSample1PackageToClassDictionary keys
]

{ #category : 'sample projects' }
RowanTestService >> rowanSample1PackageToClassDictionary [
	| dictionary |
	dictionary := Dictionary new.
	dictionary at: 'RowanSample1-Core' put: (Array with:  'RowanSample1').
	dictionary at: 'RowanSample1-Extensions' put: (Array new). 
	dictionary at:  'RowanSample1-Tests'  put: (Array with: 'RowanSample1Test'). 
	^dictionary
]
