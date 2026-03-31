Class {
	#name : 'RowanMonticelloPackageService',
	#superclass : 'RowanService',
	#instVars : [
		'name',
		'description',
		'repositoryGroups',
		'hasChanges'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'as yet unclassified' }
RowanMonticelloPackageService class >> templateClassName [

	^#RowanMonticelloPackageService

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
