Class {
	#name : 'RowanMonticelloServiceServer',
	#superclass : 'RowanMonticelloService',
	#category : 'Rowan-Services-Core'
}

{ #category : 'client commands' }
RowanMonticelloServiceServer >> removeRepository: repositoryService [

	| repo | 
	repo :=Object _objectForOop:  repositoryService repositoryOop. 
	 (Rowan globalNamed: #MCRepositoryGroup) default removeRepository: repo.
	self updateRepositories.
]

{ #category : 'client commands' }
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
				                       packageOop: packageManager asOop;
				                       repositoryGroups:
					                       (RowanMonticelloRepositoryGroupServiceServer
						                        fromRepositoryGroup:
							                        packageManager repositoryGroup).
			            service hasChanges: packageManager modified.
			            service sourceRepository: service repositoryGroups first repositories last.
			            service ].
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanMonticelloServiceServer >> updateRepositories [

	| repos |
	repos := (Rowan globalNamed: #MCRepositoryGroup) default repositories
		         asSortedCollection: [ :a :b |
		         a description <= b description ].
	repositories := repos collect: [ :repo |
			                RowanMonticelloRepositoryServiceServer new
				                description: repo description;
				                repositoryOop: repo asOop ].
	RowanCommandResult addResult: self
]
