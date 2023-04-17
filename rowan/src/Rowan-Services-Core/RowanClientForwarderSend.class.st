Class {
	#name : 'RowanClientForwarderSend',
	#superclass : 'ClientForwarderSend',
	#category : 'Rowan-Services-Core'
}

{ #category : 'initialize' }
RowanClientForwarderSend >> initialize [
	super initialize. 
	gsTrappable := true . "override so client forwarders are caught by rsr"
]
