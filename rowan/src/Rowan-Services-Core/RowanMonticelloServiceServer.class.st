Class {
	#name : 'RowanMonticelloServiceServer',
	#superclass : 'RowanMonticelloService',
	#category : 'Rowan-Services-Core'
}

{ #category : 'as yet unclassified' }
RowanMonticelloServiceServer >> update [

	| packageManagers |
	packageManagers := (Rowan globalNamed: #MCWorkingCopy) allManagers
		                   asSortedCollection: [ :a :b |
		                   a package name <= b package name ].
	packages := packageManagers collect: [ :packageManager |
			            | service |
			            service := RowanMonticelloPackageService new
				                       name: packageManager packageName;
				                       description: packageManager description;
				                       repositoryGroups:
					                       (RowanMonticelloRepositoryGroupServiceServer
						                        fromRepositoryGroup:
							                        packageManager repositoryGroup).
			            service hasChanges:
				            (packageManager changesRelativeToRepository:
					             packageManager repositoryGroup repositories last)
					            notEmpty service ].
	RowanCommandResult addResult: self
]
