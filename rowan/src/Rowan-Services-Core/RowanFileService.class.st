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
	answer := ((GsFile isServerDirectory: path) ifNil: [ false ])
		ifTrue: [ 
			(GsFile contentsOfDirectory: path onClient: false)
				collect: [ :subpath | subpath -> ((GsFile isServerDirectory: subpath) ifNil: [ false ]) ] ]
		ifFalse: [ nil ].
	answer ifNil: [ ^ self ].
	answer := (answer asSortedCollection: [ :x :y | x key < y key ]) asArray.
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
	path := (Path from: path) parent pathString. 
	self directoryContents
]

{ #category : 'client commands' }
RowanFileService >> readmeContents [
	answer := (GsFile existsOnServer: path)
		ifTrue: [ GsFile getContentsOfServerFile: path ]
		ifFalse: [ String new ].
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanFileService >> write: contents [
	| file |
	file := GsFile openWriteOnServer: path.
	[ file nextPutAll: contents ]
		ensure: [ file close ]
]
