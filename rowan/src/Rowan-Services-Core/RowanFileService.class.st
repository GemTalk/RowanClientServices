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

{ #category : 'private' }
RowanFileService >> behaviorFromMethodService: methodService [ 

	| behavior |
	behavior := Rowan globalNamed: methodService className.
	methodService meta == true ifTrue:[behavior := behavior class].
	^behavior
]

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
RowanFileService >> fileContents [
	answer := (GsFile existsOnServer: path)
		ifTrue: [ GsFile getContentsOfServerFile: path ]
		ifFalse: [ String new ].
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanFileService >> fileIn: filePath [
	"don't halt on compile warnings" 
	[GsFileIn fromServerPath: filePath] on: CompileWarning do:[:ex | ex resume]. 
	answer := GsFile getContentsOfServerFile: filePath.
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanFileService >> fileInChunk: aString [
	"don't halt on compile warnings"
	| fileIn |
	fileIn := GsFileIn _fromStream: (ReadStream on: aString).
	[ fileIn doFileIn ]
		on: CompileWarning
		do: [ :ex | ex resume ]
]

{ #category : 'client commands' }
RowanFileService >> fileoutDictionaries: dictionaryNames [
	| ws file |
	ws := WriteStream on: String new.
	self writeFileOutHeaderOn: ws.
	dictionaryNames
		do: [ :dictionaryName | 
			organizer
				fileOutClassesAndMethodsInDictionary: (Rowan globalNamed: dictionaryName)
				on: ws ].
	(GsFile existsOnServer: path)
		ifTrue: [ 
			(self confirm: 'File exists. File out anyway?')
				ifFalse: [ ^ self ] ].
	file := GsFile openAppendOnServer: path.
	[ file nextPutAll: ws contents ]
		ensure: [ file close ]
]

{ #category : 'client commands' }
RowanFileService >> fileoutMethods: array [
	| ws file |
	ws := WriteStream on: String new.
	self writeFileOutHeaderOn: ws.
	array
		do: [ :service | 
			ws
				nextPutAll:
					((self behaviorFromMethodService: service) fileOutMethod: service selector) ].
	(GsFile existsOnServer: path)
		ifTrue: [ 
			(self confirm: 'File exists. File out anyway?')
				ifFalse: [ ^ self ] ].
	file := GsFile openAppendOnServer: path.
	[ file nextPutAll: ws contents ]
		ensure: [ file close ]
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
RowanFileService >> remove [
	GsFile removeServerFile: path
]

{ #category : 'client commands' }
RowanFileService >> write: contents [
	| file thePath |
	thePath := path last = $/
		ifTrue: [ path copyFrom: 1 to: path size - 1 ]
		ifFalse: [ path ].
	file := GsFile openWriteOnServer: thePath.
	[ file nextPutAll: contents ]
		ensure: [ file close ]
]
