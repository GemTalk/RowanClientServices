Class {
	#name : 'RowanMonticelloPackageService',
	#superclass : 'RowanService',
	#instVars : [
		'name',
		'description'
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
RowanMonticelloPackageService >> name [
	^name
]

{ #category : 'accessing' }
RowanMonticelloPackageService >> name: object [
	name := object
]
