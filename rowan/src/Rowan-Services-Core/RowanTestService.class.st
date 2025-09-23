"
Support for client tests
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

{ #category : 'project support' }
RowanTestService >> addRowanSample1Class [
	| packageService |
	packageService := RowanPackageService new name: 'RowanSample1-Core'.
	packageService
		createClassNamed: 'RowanSample1'
		superclass: 'Object'
		instVars: Array new
		 category: 'Kernel'
]

{ #category : 'project support' }
RowanTestService >> addRowanSample1ClassWithEmpyClassCategory [
        "must have created the project with convention 'Rowan' (not 'RowanHybrid')"
        Object
                rwSubclass: 'RowanSample1'
                instVarNames: #(  )
                classVars: #(  )
                classInstVars: #(  )
                poolDictionaries: #(  )
                category: ''
                packageName: 'RowanSample1-Core'
                options: #(  )
]

{ #category : 'project support' }
RowanTestService >> addRowanSample1Methods [
	| classService |
	classService := RowanClassService new
		name: 'RowanSample1';
		update.
	classService organizer: self organizer.
	classService saveMethodSource: 'foo ^''foo''' category: 'accessing'.
	classService meta: true.
	classService
		saveMethodSource: 'bar ^''bar'''
		category: '*rowansample1-extensions'
]

{ #category : 'project support' }
RowanTestService >> addRowanSample1Test [
	| packageService |
	packageService := RowanPackageService new name: 'RowanSample1-Tests'.
	packageService
		createClassNamed: 'RowanSample1Test'
		superclass: 'TestCase'
		instVars: Array new
]

{ #category : 'project support' }
RowanTestService >> addRowanSample1TestMethods [

	| classService |
	classService := RowanClassService new name: 'RowanSample1Test';
		update.
	classService organizer: self organizer. 
	classService saveMethodSource: 'test1
	
	self assert: RowanSample1 new foo = ''foo''' category: 'tests'.
		
	classService saveMethodSource: 'test2

	self assert: RowanSample1 bar = ''bar''' category: 'tests'.
	
	classService saveMethodSource: 'testError

	self assert: RowanSample1 new bar = ''bar''' category: 'tests'.
	
	classService saveMethodSource: 'testFailure

	self assert: RowanSample1 new foo = ''bar''' category: 'tests'.
]

{ #category : 'dictionary support' }
RowanTestService >> createRowanSample1Dictionary [
	"RowanTestService new createRowanSample1Dictionary"
	| dictionaryService  |
	dictionaryService := RowanDictionaryService new name: self rowanSampleDictionaryName.
	dictionaryService insertAt: 1.
	self jfpwor_addRowanSample1Class.
	self addRowanSample1Methods.  
	self jfpwor_addRowanSample1Test.
	self addRowanSample1TestMethods.
	dictionaryService update.
]

{ #category : 'project support' }
RowanTestService >> createRowanSample1Project [
	| projectService  |
	projectService := RowanProjectService new name: 'RowanSample1'.
	projectService
		createProjectComponent: 'Core'
		symDict: self rowanSampleDictionaryName
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
]

{ #category : 'project support' }
RowanTestService >> createRowanSample1ProjectWithEmpyClassCategory [
        | projectService  |
        projectService := RowanProjectService new name: 'RowanSample1'.
        [projectService
                createProjectComponent: 'Core'
                symDict: self rowanSampleDictionaryName
                convention: 'Rowan'
                format: 'tonel'
                projectsHome: '$ROWAN_PROJECTS_HOME'
                type: #'none'
                shouldWrite: false.
        projectService
                addPackagesNamed: self rowanSample1PackageNames.
        self addRowanSample1ClassWithEmpyClassCategory.
        self addRowanSample1Methods.  
        self addRowanSample1Test.
        self addRowanSample1TestMethods] on: GsInteractionRequest do: [:ex | ex resume: true ].
]

{ #category : 'project support' }
RowanTestService >> deleteProjectNamedOnDisk: projectName [
	"warning! This will aggressively delete directories on disk. Use cautiously"

	('$ROWAN_PROJECTS_HOME' asFileReference / projectName) ensureDeleteAll
]

{ #category : 'dictionary support' }
RowanTestService >> jfpwor_addRowanSample1Class [

	| browserService |
	browserService := RowanBrowserService new.
	browserService jfpwor_compileClass: '(Object
				 subclass: ''RowanSample1''
				 instVarNames: #(  )
				 classVars: #(  )
				 classInstVars: #(  )
				 poolDictionaries: #(  )
				 inDictionary:
				 (System myUserProfile resolveSymbol: #RowanSample1Dictionary) value
				 options: #(  ))
		category: ''Rowan-Sample-Core'''
]

{ #category : 'dictionary support' }
RowanTestService >> jfpwor_addRowanSample1ClassWithEmpyClassCategory [

	| browserService |
	browserService := RowanBrowserService new.
	browserService jfpwor_compileClass: '(Object
				 subclass: ''RowanSample1''
				 instVarNames: #(  )
				 classVars: #(  )
				 classInstVars: #(  )
				 poolDictionaries: #(  )
				 inDictionary:
				 (System myUserProfile resolveSymbol: #RowanSample1Dictionary) value
				 options: #(  ))
		category: '''''
]

{ #category : 'dictionary support' }
RowanTestService >> jfpwor_addRowanSample1Test [

	| browserService |
	browserService := RowanBrowserService new.
	browserService jfpwor_compileClass: '(Object
				 subclass: ''RowanSample1Test''
				 instVarNames: #(  )
				 classVars: #(  )
				 classInstVars: #(  )
				 poolDictionaries: #(  )
				 inDictionary:
				 (System myUserProfile resolveSymbol: #RowanSample1Dictionary) value
				 options: #(  ))
		category: ''Rowan-Sample-Test'''
]

{ #category : 'project support' }
RowanTestService >> rowanSample1PackageNames [

	^ self rowanSample1PackageToClassDictionary keys
]

{ #category : 'project support' }
RowanTestService >> rowanSample1PackageToClassDictionary [
	| dictionary |
	dictionary := Dictionary new.
	dictionary at: 'RowanSample1-Core' put: (Array with:  'RowanSample1').
	dictionary at: 'RowanSample1-Extensions' put: (Array new). 
	dictionary at:  'RowanSample1-Tests'  put: (Array with: 'RowanSample1Test'). 
	^dictionary
]

{ #category : 'dictionary support' }
RowanTestService >> rowanSampleDictionaryName [

	^ 'RowanSample1Dictionary'
]
