Class {
	#name : 'RowanProcessServiceServer',
	#superclass : 'RowanProcessService',
	#category : 'Rowan-Services-Core'
}

{ #category : 'debugger' }
RowanProcessServiceServer >> openDebugger [
	| promise |
	promise := remoteSelf openDebugger.
	^ promise wait
]
