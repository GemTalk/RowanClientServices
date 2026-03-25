Class {
	#name : 'RowanMonticelloService',
	#superclass : 'RowanService',
	#instVars : [
		'packages'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'as yet unclassified' }
RowanMonticelloService class >> templateClassName [

	^#RowanMonticelloService
]

{ #category : 'accessing' }
RowanMonticelloService >> packages [
	^packages
]

{ #category : 'accessing' }
RowanMonticelloService >> packages: object [
	packages := object
]
