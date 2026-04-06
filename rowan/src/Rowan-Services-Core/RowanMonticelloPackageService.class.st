Class {
	#name : 'RowanMonticelloPackageService',
	#superclass : 'RowanService',
	#instVars : [
		'name',
		'description',
		'repositoryGroups',
		'packageOop',
		'hasChanges',
		'changes'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'as yet unclassified' }
RowanMonticelloPackageService class >> templateClassName [

	^#RowanMonticelloPackageService

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

{ #category : 'accessing' }
RowanMonticelloPackageService >> description [
	^description

]

{ #category : 'accessing' }
RowanMonticelloPackageService >> description: object [
	description := object

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
RowanMonticelloPackageService >> repositoryGroups [
	^repositoryGroups

]

{ #category : 'accessing' }
RowanMonticelloPackageService >> repositoryGroups: object [
	repositoryGroups := object

]
