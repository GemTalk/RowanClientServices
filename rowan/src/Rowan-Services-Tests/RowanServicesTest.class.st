"
RowanServicesTest subclasses require RowanSample1 project to be loaded.
"
Class {
	#name : 'RowanServicesTest',
	#superclass : 'TestCase',
	#instVars : [
		'defaultProjectDefinition',
		'preserveRowanProjectsHome'
	],
	#category : 'Rowan-Services-Tests'
}

{ #category : 'Testing' }
RowanServicesTest class >> isAbstract [
  ^ self sunitName = #'RowanServicesTest'
]

{ #category : 'private' }
RowanServicesTest >> _testRowanProjectsSandbox [
	"working repository for use as ROWAN_PROJECTS_HOME for tests"

	| rowanProject |
	(System gemEnvironmentVariable: 'ROWAN_PROJECTS_SANDBOX')
		ifNotNil: [ :sandboxPath | ^ sandboxPath asFileReference ].
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ rowanProject repositoryRoot / 'test/testRepositories/repos/'
]

{ #category : 'unicode method' }
RowanServicesTest >> addUnicodeSymbolKeyToUserGlobals [
  "RowanServicesTest new addUnicodeSymbolKeyToUserGlobals"

  self userGlobals
    at: (String with: (Character withValue: 16r3DA)) asSymbol
    put: 'fnoodle'
]

{ #category : 'unicode method' }
RowanServicesTest >> addUnicodeValueToUserGlobals [
  "RowanServicesTest new addUnicodeValueToUserGlobals"

  self userGlobals at: #'bad' put: '£¥' asSymbol
]

{ #category : 'support' }
RowanServicesTest >> allChars [
^'( ) * +  - / 0 1 2 3 4 5 6 7 8 9 : ; < = > ? @ A B C D E F G
H I J K L M N O P Q R S T U V W X Y Z [ \ ] ^ _ ` a b c d e f
g h i j k l m n o p q r s t u v w x y z { | } ~   ¡ ¢ £ ¤ ¥ ¦ § ¨ © ª
� ° ± ² ³ ´ µ ¶ · ¸ ¹ º » ¼ ½ ¾ ¿ À Á Â Ã Ä Å Æ Ç È É
� Í Î Ï Ð Ñ Ò Ó Ô Õ Ö × Ø Ù Ú Û Ü Ý Þ ß à á â ã ä å æ
í î ï ð ñ ò ó ô õ ö ÷ ø ù ú û ü ý þ ÿ  ı Ł ł Œ œ Š š'
]

{ #category : 'unicode method' }
RowanServicesTest >> compileUnicodeMethod [
	"RowanServicesTest new compileUnicodeMethod"

	RowanServicesTest rwCompileMethod:
	'iAmAUnicodeMethod

		| abc |
		abc := ''', (String with: (Character withValue: 16r3DA)), '''.
		self halt. 
		^abc'

	category: 'unicode method'
]

{ #category : 'support' }
RowanServicesTest >> createClassDefinitionNamed: className [

	| classDefinition |
	classDefinition := RwClassDefinition
		newForClassNamed: className
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: 'services test'
			comment: String new
			pools: #()
			type: 'normal'.
	^classDefinition
]

{ #category : 'support' }
RowanServicesTest >> createNonDiskTestProjectNamed: projectName packageName: packageName [
	| project componentName |
	Rowan image
		newOrExistingSymbolDictionaryNamed: self defaultSymbolDictionaryName.	"make sure symbol dictionary is created at same time project is created"
	componentName := self servicesTestComponentName.
	project := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self defaultSymbolDictionaryName;
		repoType: #'none';
		yourself.
	project resolveProject.

	project
		addSubcomponentNamed: componentName
			condition: 'tests'
			comment: 'a test component';
		addPackageNamed: packageName toComponentNamed: componentName;
		gemstoneSetSymbolDictName: self defaultSymbolDictionaryName
			forPackageNamed: packageName;
		yourself.

	project load.
	^ project
]

{ #category : 'support' }
RowanServicesTest >> createProjectDefinitionNamed: projectName [
	| project |
	Rowan image
		newOrExistingSymbolDictionaryNamed: self defaultSymbolDictionaryName.	"make sure symbol dictionary is created at same time project is created"
	project := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self defaultSymbolDictionaryName;
		yourself.
	project resolveProject.
	project
		addSubcomponentNamed: self servicesTestComponentName
		condition: 'tests'
		comment: 'a test component'.
	project load.
	^ project
]

{ #category : 'support' }
RowanServicesTest >> createServicesTestClass [
  | packageDefinition classDefinition |
  packageDefinition := self createServicesTestPackage.
  packageDefinition classDefinitions
    do: [ :classDef | 
      classDef key = self servicesDefaultClassName
        ifTrue: [ ^ classDef ] ].
  classDefinition := self
    createClassDefinitionNamed: self servicesDefaultClassName.
  packageDefinition addClassDefinition: classDefinition.
  ^ classDefinition
]

{ #category : 'support' }
RowanServicesTest >> createServicesTestPackage [
	| projectDefinition |
	defaultProjectDefinition
		ifNotNil: [ :projectDef | ^ projectDef packageNamed: self servicesTestPackageName ].
	projectDefinition := self defaultProjectDefinition.
	projectDefinition
		addPackageNamed: self servicesTestPackageName
		toComponentNamed: self servicesTestComponentName.
	^ projectDefinition packageNamed: self servicesTestPackageName
]

{ #category : 'support' }
RowanServicesTest >> createServicesTestTestClass [
  | packageDefinition classDefinition |
  packageDefinition := self defaultProjectDefinition
    packageNamed: self servicesTestPackageName.
  classDefinition := self
    createTestClassDefinitionNamed: self servicesDefaultTestClassName.
  packageDefinition addClassDefinition: classDefinition.
  ^ classDefinition
]

{ #category : 'support' }
RowanServicesTest >> createTestClassDefinitionNamed: className [

	| classDefinition |
	classDefinition := RwClassDefinition
		newForClassNamed: className
			super: 'TestCase'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: 'services test'
			comment: String new
			pools: #()
			type: 'normal'.
	^classDefinition
]

{ #category : 'support' }
RowanServicesTest >> defaultProjectDefinition [
  ^ defaultProjectDefinition
    ifNil: [ 
      defaultProjectDefinition := self
        createProjectDefinitionNamed: self servicesTestProjectName ]
]

{ #category : 'support' }
RowanServicesTest >> defaultSymbolDictionaryName [

	^'ServicesTestDictionary'
]

{ #category : 'support' }
RowanServicesTest >> jadeiteIssueTested: aSymbol withTitle: anObject [
	"send this to help identify which tests test which issues
	format: #issue<issueNumber>
	Example self issueTested: #issue235 withTitle: 'Need Find Class/Method from console'

	Issues currently reside in: 
		https://github.com/GemTalk/Jadeite/issues/"
]

{ #category : 'support' }
RowanServicesTest >> loadRowanSample1 [ 
	| projectsHome projectName loadSpec resolvedProject |
	projectName := 'RowanSample1'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].
	projectsHome := self _testRowanProjectsSandbox.
	(projectsHome / projectName) ensureDeleteAll.
	loadSpec := (RwSpecification
		fromUrl:
			'file:$ROWAN_PROJECTS_HOME/Rowan/samples/RowanSample1_resolved_v2.ston')
		projectsHome: projectsHome;
		yourself.
	resolvedProject := loadSpec resolve.
	resolvedProject load
]

{ #category : 'support' }
RowanServicesTest >> loadServicesTestProject [
	self defaultProjectDefinition load
]

{ #category : 'unicode method' }
RowanServicesTest >> removeUnicodeSymbolsFromUserGlobals [
  "RowanServicesTest new addUnicodeSymbolToUserGlobals"

  self userGlobals
    removeKey: (String with: (Character withValue: 16r3DA)) asSymbol
    ifAbsent: [  ].
  self userGlobals removeKey: #'bad' ifAbsent: [  ]
]

{ #category : 'constants' }
RowanServicesTest >> servicesDefaultClassName [

	^'RowanServicesTestClass'
]

{ #category : 'constants' }
RowanServicesTest >> servicesDefaultTestClassName [

	^'RowanServicesTestClassForTesting'
]

{ #category : 'accessing' }
RowanServicesTest >> servicesTestComponentName [ 

	^'Core'
]

{ #category : 'constants' }
RowanServicesTest >> servicesTestPackageName [

	^'RowanServicesTestPackage'
]

{ #category : 'constants' }
RowanServicesTest >> servicesTestProjectName [

	^'RowanServicesTestProject'
]

{ #category : 'setup teardown' }
RowanServicesTest >> setUp [
	| user symListP symListT dictP dictT index projectsHome |
	"ensure results are clean as service requests not coming through #updateFromSton: like the client"
	super setUp.
	Rowan platform _alternateImageClass: Rowan image testImageClass.
	projectsHome := self _testRowanProjectsSandbox.
	preserveRowanProjectsHome := System gemEnvironmentVariable: 'ROWAN_PROJECTS_HOME'.
	System gemEnvironmentVariable: 'ROWAN_PROJECTS_HOME' put: projectsHome pathString.
	RowanAnsweringService new setEnableInteractionHandler: false. 
	RowanCommandResult initializeResults.
"ensure that project is unloaded"
	(Rowan image loadedProjectNamed: self servicesTestProjectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].
	(self _testRowanProjectsSandbox / self servicesTestProjectName) ensureDeleteAll.
"ensure that the default symbol dictionary has been removed and that any classes used in tests are not preset"
	user := System myUserProfile.
	symListP := user symbolList.
	symListT := GsCurrentSession currentSession symbolList.

	{self defaultSymbolDictionaryName}
		do: [ :symDictName | 
			dictP := symListP objectNamed: symDictName.
			dictP
				ifNotNil: [ 
					(dictP at: GsPackagePolicy globalName otherwise: nil)
						ifNotNil: [ :policy | policy disable ].
					index := symListP indexOf: dictP.
					index > 0
						ifTrue: [ user removeDictionaryAt: index ] ].

			dictT := symListT objectNamed: symDictName.
			dictT
				ifNotNil: [ 
					(dictT at: GsPackagePolicy globalName otherwise: nil)
						ifNotNil: [ :policy | policy disable ].
					index := symListT indexOf: dictT.
					index > 0
						ifTrue: [ symListT removeAtIndex: index ] ] ].
	GsPackagePolicy current refreshSessionMethodDictionary.
]

{ #category : 'setup teardown' }
RowanServicesTest >> tearDown [
	super tearDown.
	System
		gemEnvironmentVariable: 'ROWAN_PROJECTS_HOME'
		put: preserveRowanProjectsHome.
	Rowan platform _alternateImageClass: nil.
	RowanCommandResult initializeResults.	"squash the gui updates"
	RowanAnsweringService new setEnableInteractionHandler: true.
]

{ #category : 'tests' }
RowanServicesTest >> test_isAbstract [
	self class name = #'RowanServicesTest'
		ifTrue: [ self assert: self class isAbstract ]
		ifFalse: [ self deny: self class isAbstract ]
]

{ #category : 'support' }
RowanServicesTest >> unloadServicesTestProject [

	Rowan image loadedProjectNamed: self servicesTestProjectName ifAbsent:[^self].
	Rowan projectTools delete deleteProjectNamed: self servicesTestProjectName.
	defaultProjectDefinition := nil
]

{ #category : 'accessing' }
RowanServicesTest >> userGlobals [
  ^ Rowan image symbolList objectNamed: #'UserGlobals'
]
