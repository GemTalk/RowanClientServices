Class {
	#name : 'RowanClientServiceTemplateResolver',
	#superclass : 'RsrTemplateResolver',
	#category : 'Rowan-Services-Extensions'
}

{ #category : 'resolving' }
RowanClientServiceTemplateResolver >> clientClassForTemplate: aTemplate ifAbsent: absentBlock [
	^ Rowan platform
		serviceClassFor: aTemplate clientClassName
		ifAbsent: [ super clientClassForTemplate: aTemplate ifAbsent: absentBlock ]
]

{ #category : 'resolving' }
RowanClientServiceTemplateResolver >> serverClassForTemplate: aTemplate ifAbsent: absentBlock [
	^ Rowan platform
		serviceClassFor: aTemplate serverClassName
		ifAbsent: [ super serverClassForTemplate: aTemplate ifAbsent: absentBlock ]
]

{ #category : 'resolving' }
RowanClientServiceTemplateResolver >> templateNamed: aTemplateName ifAbsent: aBlock [
	^ Rowan platform
		serviceClassFor: aTemplateName
		ifAbsent: [ super templateNamed: aTemplateName ifAbsent: aBlock ]
]
