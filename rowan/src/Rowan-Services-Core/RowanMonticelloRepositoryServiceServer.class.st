Class {
	#name : 'RowanMonticelloRepositoryServiceServer',
	#superclass : 'RowanMonticelloRepositoryService',
	#category : 'Rowan-Services-Core'
}

{ #category : 'updating' }
RowanMonticelloRepositoryServiceServer >> update [

	| repository |
	repository := Object _objectForOop: repositoryOop.
	(repository class canUnderstand: #dictionary)
		ifTrue: [
				repositoryPackages := repository dictionary keys collect: [
						                      :versionInfo |
						                      RowanMonticelloPackageService new
							                      name: versionInfo name;
							                      description: versionInfo name ] ]
		ifFalse: [
				repositoryPackages := Array new.
				repository allVersionNames do: [ :versionName |
							repositoryPackages add: ( RowanMonticelloPackageService new
							                      name: versionName;
							                      description: versionName) ] ].
	RowanCommandResult addResult: self
]
