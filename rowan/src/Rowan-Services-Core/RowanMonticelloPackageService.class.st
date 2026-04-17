Class {
	#name : 'RowanMonticelloPackageService',
	#superclass : 'RowanService',
	#instVars : [
		'name',
		'description',
		'repositoryGroups',
		'packageOop',
		'hasChanges',
		'changes',
		'classes',
		'extensionClasses'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'as yet unclassified' }
RowanMonticelloPackageService class >> templateClassName [

	^#RowanMonticelloPackageService

]

{ #category : 'client commands' }
RowanMonticelloPackageService >> addFiletreeRepository: aFileDirectoryString [

	| packageManager repo |
	packageManager := Object _objectForOop: packageOop.
	repo := (Rowan globalNamed: 'MCFileTreeRepository') new directory:
		        ((Rowan globalNamed: 'FileDirectory') on:
			         aFileDirectoryString).
	packageManager repositoryGroup addRepository: repo.
	RowanCommandResult add: self
]

{ #category : 'client commands' }
RowanMonticelloPackageService >> addGithubRepository: aString [

	| packageManager repo |
	packageManager := Object _objectForOop: packageOop.
	repo := (Rowan globalNamed: 'MCGitHubRepository') location: aString.
	packageManager repositoryGroup addRepository: repo.
	RowanCommandResult add: self
]

{ #category : 'client commands' }
RowanMonticelloPackageService >> addHttpRepository: aString [

	| packageManager repo |
	packageManager := Object _objectForOop: packageOop.
	repo := (Rowan globalNamed: 'MCHttpRepository')
		        location: aString
		        user: ''
		        password: ''.
	packageManager repositoryGroup addRepository: repo.
	RowanCommandResult add: self
]

{ #category : 'client commands' }
RowanMonticelloPackageService >> addMCZRepository: aFileDirectoryString [

	| packageManager repo |
	packageManager := Object _objectForOop: packageOop.
	repo := (Rowan globalNamed: 'MCServerDirectoryRepository') new
		        directory:
			        (Rowan globalNamed: 'ServerFileDirectory' on: aFileDirectoryString);
		        yourself.
	packageManager repositoryGroup addRepository: repo.
	RowanCommandResult add: self
]

{ #category : 'client commands' }
RowanMonticelloPackageService >> changesAgainst: aRepositoryOop [

	| repository packageManager patch |
	repository := Object _objectForOop: aRepositoryOop.
	packageManager := Object _objectForOop: packageOop.
	patch := packageManager changesRelativeToRepository: repository.
	changes :=  JadeServer new _mcDescriptionOfPatch: patch baseName: 'closest ancestor' alternateName: nil packageName: packageManager packageName.
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanMonticelloPackageService >> classes [

	| packageManager |
	packageManager := Object _objectForOop: packageOop.
	classes := (packageManager package packageInfo classes asSet
		            asSortedCollection: [ :x :y | x name < y name ])
		           collect: [ :cls |
				           (RowanClassService new name: cls thisClass name)
						meta: false; 
					           updateClassHistoryFrom: cls thisClass;
					           setIsTestCase;
					           isExtension: false ].
	extensionClasses := (packageManager package packageInfo
		                     extensionClasses asSet asSortedCollection: [
		                     :x
		                     :y | x name < y name ]) collect: [ :cls |
			                    (RowanClassService new name: cls thisClass name)
						meta: false; 
				                    updateClassHistoryFrom: cls thisClass;
				                    setIsTestCase;
				                    isExtension: true ].
	RowanCommandResult addResult: self
]

{ #category : 'accessing' }
RowanMonticelloPackageService >> description [
	^description

]

{ #category : 'accessing' }
RowanMonticelloPackageService >> description: object [
	description := object

]

{ #category : 'accessing' }
RowanMonticelloPackageService >> extensionClasses [
	^extensionClasses
]

{ #category : 'accessing' }
RowanMonticelloPackageService >> extensionClasses: object [
	extensionClasses := object
]

{ #category : 'accessing' }
RowanMonticelloPackageService >> hasChanges [
	^hasChanges
]

{ #category : 'accessing' }
RowanMonticelloPackageService >> hasChanges: object [
	hasChanges := object
]

{ #category : 'accessing' }
RowanMonticelloPackageService >> name [
	^name

]

{ #category : 'accessing' }
RowanMonticelloPackageService >> name: object [
	name := object

]

{ #category : 'accessing' }
RowanMonticelloPackageService >> packageOop [
	^packageOop
]

{ #category : 'accessing' }
RowanMonticelloPackageService >> packageOop: object [
	packageOop := object
]

{ #category : 'accessing' }
RowanMonticelloPackageService >> repositoryGroups [
	^repositoryGroups

]

{ #category : 'accessing' }
RowanMonticelloPackageService >> repositoryGroups: object [
	repositoryGroups := object

]
