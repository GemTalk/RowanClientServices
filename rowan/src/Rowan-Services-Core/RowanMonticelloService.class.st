Class {
	#name : 'RowanMonticelloService',
	#superclass : 'RowanService',
	#instVars : [
		'packages',
		'repositories'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'as yet unclassified' }
RowanMonticelloService class >> templateClassName [

	^#RowanMonticelloService

]

{ #category : 'client commands' }
RowanMonticelloService >> addFiletreeRepository: aFileDirectoryString [

	| repo |
	repo := (Rowan globalNamed: 'MCFileTreeRepository') new directory:
		        ((Rowan globalNamed: 'FileDirectory') on:
			         aFileDirectoryString).
	(Rowan globalNamed: 'MCRepositoryGroup') default addRepository: repo.
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanMonticelloService >> addGithubRepository: aString [

	| repo |
	repo := (Rowan globalNamed: 'MCGitHubRepository') location: aString.
	(Rowan globalNamed: 'MCRepositoryGroup') default addRepository: repo.
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanMonticelloService >> addHttpRepository: aString [

	| repo |
	repo := (Rowan globalNamed: 'MCHttpRepository')
		        location: aString
		        user: ''
		        password: ''.
	(Rowan globalNamed: 'MCRepositoryGroup') default addRepository: repo.
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanMonticelloService >> addMCZRepository: aFileDirectoryString [

	| repo |
	repo := (Rowan globalNamed: 'MCServerDirectoryRepository') new
		        directory:
			        (Rowan
				         globalNamed: 'ServerFileDirectory'
				         on: aFileDirectoryString);
		        yourself.
	(Rowan globalNamed: 'MCRepositoryGroup') default addRepository: repo
]

{ #category : 'accessing' }
RowanMonticelloService >> packages [
	^packages

]

{ #category : 'accessing' }
RowanMonticelloService >> packages: object [
	packages := object

]

{ #category : 'client commands' }
RowanMonticelloService >> removeRepository: repositoryService [

	| repo | 
	repo :=Object _objectForOop:  repositoryService repositoryOop. 
	 (Rowan globalNamed: #MCRepositoryGroup) default removeRepository: repo.
	self updateRepositories.
]

{ #category : 'accessing' }
RowanMonticelloService >> repositories [
	^repositories
]

{ #category : 'accessing' }
RowanMonticelloService >> repositories: object [
	repositories := object
]

{ #category : 'client commands' }
RowanMonticelloService >> update [

	| packageManagers |
	packageManagers := (Rowan globalNamed: #MCWorkingCopy) allManagers
		                   asSortedCollection: [ :a :b |
		                   a package name <= b package name ].
	packages := packageManagers collect: [ :packageManager |
			            | service |
			            service := RowanMonticelloPackageService new
				                       name: packageManager packageName;
				                       description: packageManager description;
				                       packageOop: packageManager asOop;
				                       repositoryGroups:
					                       (RowanMonticelloRepositoryGroupServiceServer
						                        fromRepositoryGroup:
							                        packageManager repositoryGroup).
			            service hasChanges: packageManager modified.
			            service sourceRepository: service repositoryGroups first repositories last.
			            service ].
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanMonticelloService >> updateRepositories [

	| repos |
	repos := (Rowan globalNamed: #MCRepositoryGroup) default repositories
		         asSortedCollection: [ :a :b |
		         a description <= b description ].
	repositories := repos collect: [ :repo |
			                RowanMonticelloRepositoryServiceServer new
				                description: repo description;
				                repositoryOop: repo asOop ].
	RowanCommandResult addResult: self
]
