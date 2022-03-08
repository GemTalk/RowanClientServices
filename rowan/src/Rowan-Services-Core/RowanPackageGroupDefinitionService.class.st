"
The packages in this class may or may not be loaded.
"
Class {
	#name : 'RowanPackageGroupDefinitionService',
	#superclass : 'RowanPackageGroupService',
	#category : 'Rowan-Services-Core'
}

{ #category : 'client commands' }
RowanPackageGroupDefinitionService >> computePackageNames [
	name = self class allPackagesGroupName
		ifTrue: [ packageNames := self projectDefinition packageNames ]
		ifFalse: [ 
			| packageGroup |
			packageGroup := self projectDefinition packageGroupNamed: name.
			packageNames := packageGroup packageNames ]
]

{ #category : 'packages' }
RowanPackageGroupDefinitionService >> computePackageServices [
	packageServices := packageNames
		collect: [ :packageName | RowanPackageDefinitionService new name: packageName ]
]
