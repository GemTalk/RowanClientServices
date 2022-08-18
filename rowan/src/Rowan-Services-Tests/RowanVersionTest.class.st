Class {
	#name : 'RowanVersionTest',
	#superclass : 'RowanServicesTest',
	#category : 'Rowan-Services-Tests'
}

{ #category : 'tests' }
RowanVersionTest >> test_majorVersion [
	| semanticVersion |
	self jadeiteIssueTested: #'issue521' withTitle: 'Jadeite version check'.
	semanticVersion := RwSemanticVersionNumber fromString: RowanService version.
	self
		deny:
			(RowanService
				isRowanClientServicesVersionSupported:
					semanticVersion incrementMajorVersion printString
				lowerLimit: RowanService version)
]

{ #category : 'tests' }
RowanVersionTest >> test_minorVersion [
	| semanticVersion |
	self jadeiteIssueTested: #'issue521' withTitle: 'Jadeite version check'.
	semanticVersion := RwSemanticVersionNumber fromString: RowanService version.
	self
		deny:
			(RowanService
				isRowanClientServicesVersionSupported:
					semanticVersion copy incrementMinorVersion printString
				lowerLimit: RowanService version)
]

{ #category : 'tests' }
RowanVersionTest >> test_patchVersion [
	"patch version increments should be supported"

	| semanticVersion |
	self jadeiteIssueTested: #'issue521' withTitle: 'Jadeite version check'.
	semanticVersion := RwSemanticVersionNumber fromString: RowanService version.
	50
		timesRepeat: [ 
			self
				assert:
					(RowanService
						isRowanClientServicesVersionSupported:
							semanticVersion incrementPatchVersion printString
						lowerLimit: RowanService version) ]
]

{ #category : 'tests' }
RowanVersionTest >> test_version [
	"validate we are running on a supported version"

	self jadeiteIssueTested: #'issue521' withTitle: 'Jadeite version check'.
	self assert: (RowanService isRowanClientServicesVersionSupported: RowanService version)
]
