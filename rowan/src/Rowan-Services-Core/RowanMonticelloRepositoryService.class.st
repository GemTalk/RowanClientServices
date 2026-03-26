Class {
	#name : 'RowanMonticelloRepositoryService',
	#superclass : 'RowanService',
	#instVars : [
		'location'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'as yet unclassified' }
RowanMonticelloRepositoryService class >> templateClassName [

	^#'RowanMonticelloRepositoryService'

]

{ #category : 'accessing' }
RowanMonticelloRepositoryService >> location [
	^location

]

{ #category : 'accessing' }
RowanMonticelloRepositoryService >> location: object [
	location := object

]
