Class {
	#name : 'RowanMonticelloRepositoryService',
	#superclass : 'RowanService',
	#instVars : [
		'description',
		'repositoryPackages',
		'repositoryOop'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'as yet unclassified' }
RowanMonticelloRepositoryService class >> templateClassName [

	^#'RowanMonticelloRepositoryService'

]

{ #category : 'accessing' }
RowanMonticelloRepositoryService >> description [
	^description

]

{ #category : 'accessing' }
RowanMonticelloRepositoryService >> description: object [
	description := object

]

{ #category : 'accessing' }
RowanMonticelloRepositoryService >> repositoryOop [
	^repositoryOop
]

{ #category : 'accessing' }
RowanMonticelloRepositoryService >> repositoryOop: object [
	repositoryOop := object
]

{ #category : 'accessing' }
RowanMonticelloRepositoryService >> repositoryPackages [
	^repositoryPackages
]

{ #category : 'accessing' }
RowanMonticelloRepositoryService >> repositoryPackages: object [
	repositoryPackages := object
]
