Class {
	#name : 'RowanCompileErrorService',
	#superclass : 'RowanService',
	#instVars : [
		'gsArguments'
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
