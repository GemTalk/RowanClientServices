Class {
	#name : 'JadeServer64bit35',
	#superclass : 'JadeServer64bit32',
	#instVars : [
		'transcriptService'
	],
	#category : 'Rowan-JadeServer'
}

{ #category : 'other' }
JadeServer64bit35 >> nextPutAll: string [
	transcriptService ifNil:[^super nextPutAll: string].
	transcriptService nextPutAll: string.
]

{ #category : 'other' }
JadeServer64bit35 >> stepThrough: aGsProcess inFrame: anInteger [
	aGsProcess setStepThroughBreaksAtLevel: anInteger breakpointLevel: nil
]

{ #category : 'accessing' }
JadeServer64bit35 >> transcriptService: object [
	transcriptService := object
]
