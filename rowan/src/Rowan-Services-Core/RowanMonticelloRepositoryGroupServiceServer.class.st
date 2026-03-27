Class {
	#name : 'RowanMonticelloRepositoryGroupServiceServer',
	#superclass : 'RowanMonticelloRepositoryGroupService',
	#category : 'Rowan-Services-Core'
}

{ #category : 'instance creation' }
RowanMonticelloRepositoryGroupServiceServer class >> fromRepositoryGroup: aRepositoryGroup [

	| inst |
	inst := self new.
	inst repositories:
		(aRepositoryGroup repositories collect: [ :repository |
				 RowanMonticelloRepositoryServiceServer new description:
					 repository description ]).
	^ inst

]
