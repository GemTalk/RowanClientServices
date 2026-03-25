Class {
	#name : 'RowanMonticelloServiceServer',
	#superclass : 'RowanMonticelloService',
	#category : 'Rowan-Services-Core'
}

{ #category : 'updating' }
RowanMonticelloServiceServer >> update [

	| packageManagers |
	packageManagers := (UserGlobals at: #MCWorkingCopy) allManagers
		                   asSortedCollection: [ :a :b |
		                   a package name <= b package name ].
	packages := packageManagers collect: [ :packageManager |
			            RowanMonticelloPackageService new
				            name: packageManager name;
				            description: packageManager description ].
	RowanCommandResult addResult: self
]
