Class {
	#name : 'RowanMonticelloRepositoryGroupService',
	#superclass : 'RowanService',
	#instVars : [
		'repositories'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'constants' }
RowanMonticelloRepositoryGroupService class >> templateClassName [

	^#RowanMonticelloRepositoryGroupService

]

{ #category : 'accessing' }
RowanMonticelloRepositoryGroupService >> repositories [
	^repositories

]

{ #category : 'accessing' }
RowanMonticelloRepositoryGroupService >> repositories: object [
	repositories := object

]
