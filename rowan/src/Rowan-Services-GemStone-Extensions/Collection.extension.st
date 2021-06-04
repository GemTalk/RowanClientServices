Extension { #name : 'Collection' }

{ #category : '*rowan-services-gemstone-extensions' }
Collection >> asRSR [
	^ self collect: [ :each | each asRSR ]
]
