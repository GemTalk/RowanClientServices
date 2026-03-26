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
			            RowanMonticelloPackageService new
				            name: packageManager packageName;
				            description: packageManager description;
				            repositoryGroup:
					            (RowanMonticelloRepositoryGroupServiceServer fromRepositoryGroup:
							             packageManager repositoryGroup) ].
	RowanCommandResult addResult: self

]
