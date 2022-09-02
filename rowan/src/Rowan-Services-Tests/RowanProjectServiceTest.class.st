Class {
	#name : 'RowanProjectServiceTest',
	#superclass : 'RowanServicesTest',
	#category : 'Rowan-Services-Tests'
}

{ #category : 'support' }
RowanProjectServiceTest >> createDefaultProjectNamed: projectName [
	| projectService |
	projectService := RowanProjectService new name: projectName.
	projectService
		createProjectComponent: 'Core'
		symDict: 'UserGlobals'
		convention: 'RowanHybrid'
		format: 'tonel'
		projectsHome: '$ROWAN_PROJECTS_HOME'
		type: 'git'
]

{ #category : 'support' }
RowanProjectServiceTest >> existingProjectNamed: projectName [

	| projectService |
	projectService := RowanProjectService newNamed: projectName. 
	self assert: (Rowan image loadedProjectNamed: projectName) name equals: projectName.
	^projectService
]

{ #category : 'support' }
RowanProjectServiceTest >> projectServiceNamed: projectName [

	| projectService |
	projectService := RowanProjectService newNamed: projectName. 
	self assert: (Rowan image loadedProjectNamed: projectName) name equals: projectName.
	^projectService
]

{ #category : 'setup teardown' }
RowanProjectServiceTest >> setUp [
	super setUp.
	self loadServicesTestProject.

"ensure that test project is unloaded"
	(Rowan image loadedProjectNamed: self testProjectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].
	(self _testRowanProjectsSandbox / self testProjectName) ensureDeleteAll
]

{ #category : 'setup teardown' }
RowanProjectServiceTest >> tearDown [

	self unloadServicesTestProject.
	super tearDown.
]

{ #category : 'tests' }
RowanProjectServiceTest >> test_addedProjectNotOnDisk [

	| projectService projectName |
	projectName := self testProjectName. 
	self jadeiteIssueTested: #issue246 withTitle: 'Jadeite handling project that''s not committed'. 
	self createNonDiskTestProjectNamed:  projectName packageName: 'Packagekent'. 
	projectService := RowanProjectService newNamed: projectName. 
	projectService refresh. "<-- walkback occured here" 
	[self deny: projectService existsOnDisk.
	self deny: projectService isSkew "no skew if not on disk"]
		ensure: [RowanBrowserService new unloadProjectsNamed: (Array with: projectName)]
]

{ #category : 'tests' }
RowanProjectServiceTest >> test_commandResultSessionTemp [

	self assert: RowanCommandResult results == (SessionTemps current at: #rowanCommandResults)
]

{ #category : 'tests' }
RowanProjectServiceTest >> test_createProject [
	"a git project without a remote origin should not be able to be loaded with resolve strict true"

	| gitDirectoryReference projectName |
	projectName := 'Test_createProject'.
	gitDirectoryReference := FileSystem disk / '$ROWAN_PROJECTS_HOME'
		/ projectName.
	self deny: gitDirectoryReference exists.
	[ 
	self createDefaultProjectNamed: projectName.
	self assert: gitDirectoryReference exists ]
		ensure: [ 
			(RowanProjectService new name: projectName) unload. 
			gitDirectoryReference deleteAll.
			self deny: gitDirectoryReference exists ]
]

{ #category : 'tests' }
RowanProjectServiceTest >> test_loadedGitProject [

	| projectService projectName |
	projectName := 'RowanSample9'. 
	self loadRowanSample9. 
	projectService := RowanProjectService newNamed: projectName. 
	projectService refresh. 
	[self assert: projectService existsOnDisk.
	self deny: projectService isSkew. "no skew if freshly loaded"
	self deny: projectService isGitDirty. "no git dirty if freshly loaded"
	self assert: projectService rowanBranch = 'spec_0002'.
	projectService rwProject gitCheckout: 'spec_0003'.
	projectService basicRefresh. 
	self assert: projectService isSkew. "skew after checking out different branch"
]
		ensure: [RowanBrowserService new unloadProjectsNamed: (Array with: projectName)]
]

{ #category : 'tests' }
RowanProjectServiceTest >> test_unloadNotLoadedProjectDoesNotWalkback [

	"just make sure if a project we gracefully handle trying to remove an unloaded project"
	self deny: (Rowan image loadedProjectNamed: 'BadProjectName' ifAbsent:[false]). 
	RowanBrowserService new unloadProjectsNamed: #('BadProjectName').
	self deny: (Rowan image loadedProjectNamed: 'BadProjectName' ifAbsent:[false]).
]

{ #category : 'tests' }
RowanProjectServiceTest >> test_updateAddsCommandResult [
	| projectService |
	projectService := self projectServiceNamed: self servicesTestProjectName.
	RowanCommandResult initializeResults. 
	self assert: RowanCommandResult results size equals: 0. 
	projectService update. 
	self assert: RowanCommandResult results size equals: 2.  "project service & load spec service" 
	self assert: RowanCommandResult results last == projectService.
]

{ #category : 'support' }
RowanProjectServiceTest >> testProjectName [

	^'Tashkent'
]
