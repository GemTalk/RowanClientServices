"
Simple service that returns the contents of directories. 

A server file browser could be built on top of me.
"
Class {
	#name : 'RowanFileService',
	#superclass : 'RowanAnsweringService',
	#instVars : [
		'path'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'client commands' }
RowanFileService >> directoryContents [
	answer := (GsFile isServerDirectory: path)
		ifTrue: [ 
			(GsFile contentsOfDirectory: path onClient: false)
				collect: [ :subpath | subpath -> (GsFile isServerDirectory: subpath) ] ]
		ifFalse: [ nil ].
	answer ifNil:[^self]. 
	answer := (answer asSortedCollection:[:x :y | x key < y key]) asArray.
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanFileService >> diveInto: directory [
	path := (Path from: path) resolveString: directory.
	self directoryContents
]

{ #category : 'client commands' }
RowanFileService >> isDirectory: directory [
	answer := GsFile isServerDirectory: directory. 
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanFileService >> pop [
	path := (Path from: path) parent asString. 
	self directoryContents
]
