Class {
	#name : 'RowanTranscriptService',
	#superclass : 'RowanService',
	#instVars : [
		'string'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'rsr' }
RowanTranscriptService class >> templateClassName [

	^#RowanTranscriptService
]

{ #category : 'actions' }
RowanTranscriptService >> installTranscript [
	SessionTemps current at: #'TranscriptStream_SessionStream' put: nil.
	RowanAnsweringService new flipTranscript.
	self jadeiteServer transcriptService: self.
]

{ #category : 'actions' }
RowanTranscriptService >> nextPutAll: aString [
	| promise |
	string := aString. 
	promise := remoteSelf nextPutAll.
	promise wait.
]
