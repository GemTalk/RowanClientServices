Class {
	#name : 'RowanClientServiceTemplateResolver',
	#superclass : 'RsrTemplateResolver',
	#category : 'Rowan-Services-Extensions'
}

{ #category : 'resolving' }
RowanClientServiceTemplateResolver >> templateNamed: aTemplateName ifAbsent: aBlock [
	"serviceClass := (((AllUsers userWithId: 'SystemUser') objectNamed: 'Rowan')
										ifNotNil: [ :rowan | rowan platform serviceClassFor: name ])"
	GsFile gciLogServer: 'RowanClientServiceTemplateResolver>>templateNamed:ifAbsent:'. 
	GsFile gciLogServer: ' Rowan platform: ',  Rowan platform printString. 
	GsFile gciLogServer: 'serviceClasses: ', Rowan platform serviceClasses printString. 
	^ Rowan platform
		serviceClassFor: aTemplateName
		ifAbsent: [ super templateNamed: aTemplateName ifAbsent: aBlock ]
]
