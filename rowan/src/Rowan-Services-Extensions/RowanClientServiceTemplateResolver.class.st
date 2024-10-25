Class {
	#name : 'RowanClientServiceTemplateResolver',
	#superclass : 'RsrTemplateResolver',
	#category : 'Rowan-Services-Extensions'
}

{ #category : 'resolving' }
RowanClientServiceTemplateResolver >> templateNamed: aTemplateName ifAbsent: aBlock [
	^ Rowan platform
		serviceClassFor: aTemplateName
		ifAbsent: [ super templateNamed: aTemplateName ifAbsent: aBlock ]
]
