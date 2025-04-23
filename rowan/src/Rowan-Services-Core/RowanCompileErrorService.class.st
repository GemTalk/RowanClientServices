Class {
	#name : 'RowanCompileErrorService',
	#superclass : 'RowanService',
	#instVars : [
		'gsArguments',
		'messageText'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'rsr' }
RowanCompileErrorService class >> templateClassName [

	^#RowanCompileErrorService
]

{ #category : 'accessing' }
RowanCompileErrorService >> gsArguments [
	^gsArguments
]

{ #category : 'accessing' }
RowanCompileErrorService >> gsArguments: object [
	gsArguments := object
]

{ #category : 'accessing' }
RowanCompileErrorService >> messageText [
	^messageText
]

{ #category : 'accessing' }
RowanCompileErrorService >> messageText: object [
	messageText := object
]
