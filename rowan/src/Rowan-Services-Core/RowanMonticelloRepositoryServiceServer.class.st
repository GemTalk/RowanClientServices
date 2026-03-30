Class {
	#name : 'RowanMonticelloRepositoryServiceServer',
	#superclass : 'RowanMonticelloRepositoryService',
	#category : 'Rowan-Services-Core'
}

{ #category : 'updating' }
RowanMonticelloRepositoryServiceServer >> update [

	| packages repository |
	repository := Object _objectFromOop: repositoryOop.
	packages := repository allFileNames collect: [ :fileName |
		            (repository versionFromFileNamed: fileName) package ].
	repositoryPackages := packages collect: [ :pkg |
			                      RowanMonticelloPackageService new
				                      name: pkg packageName;
				                      description: pkg description ].
	RowanCommandResult addResult: self
]
