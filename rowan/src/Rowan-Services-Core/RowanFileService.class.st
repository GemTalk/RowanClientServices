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

{ #category : 'command support' }
RowanFileService >> basicIsDirectory: thePath [
	| fileReference |
	fileReference := FileReference fileSystem: FileSystem disk path: thePath asPath.
	answer := fileReference isDirectory.
	RowanCommandResult addResult: self
]

{ #category : 'private' }
RowanFileService >> behaviorFromMethodService: methodService [ 

	| behavior |
	behavior := Rowan globalNamed: methodService className.
	methodService meta == true ifTrue:[behavior := behavior class].
	^behavior
]

{ #category : 'client commands' }
RowanFileService >> deleteFileWithUnicode [
	" for tests"

	| fileReference |
	fileReference := FileReference
		fileSystem: FileSystem disk
		path: FileSystem workingDirectory pathString , '/testJadeiteUnicodeFile.txt'.
	fileReference delete
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
RowanFileService >> directoryPath [
	| fileReference |
	fileReference := FileReference 
		fileSystem: FileSystem disk 
		path: path asPath.
	fileReference isDirectory 
		ifTrue: [ answer := fileReference pathString ]
		ifFalse: [ answer := fileReference parent pathString ].
	RowanCommandResult addResult: self.
	^ answer	"return for testing"
]

{ #category : 'client commands' }
RowanFileService >> diveInto: directory [
	path := (Path from: path) resolveString: directory.
	self directoryContents
]

{ #category : 'client commands' }
RowanFileService >> fileContents [
	| fileReference |
	fileReference := FileReference fileSystem: FileSystem disk path: path asPath.
	answer := fileReference exists
		ifTrue: [ fileReference readStream contents ]
		ifFalse: [ String new asUnicodeString ].
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanFileService >> fileIn [
	^ self fileIn: path
]

{ #category : 'client commands' }
RowanFileService >> fileIn: filePath [
	"don't halt on compile warnings"

	| fileReference |
	fileReference := filePath asFileReference.
	fileReference exists
		ifFalse: [ ^ self inform: 'File does not exist' ].
	[ GsFileIn fromServerPath: filePath ]
		on: CompileWarning
		do: [ :ex | 
			Transcript
				cr;
				show: ex description;
				flush.
			ex resume ].
	answer := fileReference readStream contents.
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanFileService >> fileInChunk: aString [
	"don't halt on compile warnings"

	| fileIn |
	fileIn := GsFileIn _fromStream: (ReadStream on: aString).
	[ fileIn doFileIn ]
		on: CompileWarning
		do: [ :ex | 
			Transcript
				cr;
				show: ex description;
				flush.
			ex resume ]
]

{ #category : 'client commands' }
RowanFileService >> fileName [
	answer := (Path from: path) basename. 
	RowanCommandResult addResult: self.
	^answer "return for testing"
]

{ #category : 'client commands' }
RowanFileService >> fileoutDictionaries: dictionaryNames [
	| ws |
	ws := WriteStream on: String new.
	self writeFileOutHeaderOn: ws.
	dictionaryNames
		do: [ :dictionaryName | 
			organizer
				fileOutClassesAndMethodsInDictionary: (Rowan globalNamed: dictionaryName)
				on: ws ].
	self fileOut: ws on: path
]

{ #category : 'client commands' }
RowanFileService >> fileoutMethods: array [
	| ws |
	ws := WriteStream on: String new.
	self writeFileOutHeaderOn: ws.
	array
		do: [ :service | 
			ws
				nextPutAll:
					((self behaviorFromMethodService: service) fileOutMethod: service selector) ].
	self fileOut: ws on: path
]

{ #category : 'client commands' }
RowanFileService >> isDirectory [
	self basicIsDirectory: path
]

{ #category : 'client commands' }
RowanFileService >> isDirectory: directory [
	self basicIsDirectory: directory
]

{ #category : 'client commands' }
RowanFileService >> parentDirectoryPath [
	answer := (Path from: path) parent pathString. 
	RowanCommandResult addResult: self.
	^answer "return for testing"
]

{ #category : 'accessing' }
RowanFileService >> path [
	^path
]

{ #category : 'accessing' }
RowanFileService >> path: object [
	path := object
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
	| thePath fileReference writeStream |
	thePath := path last = $/
		ifTrue: [ path copyFrom: 1 to: path size - 1 ]
		ifFalse: [ path ].
	fileReference := FileReference fileSystem: FileSystem disk path: thePath asPath.
	writeStream := fileReference writeStream.
	[ 
	writeStream
		nextPutAll: contents asUnicodeString;
		flush ]
		ensure: [ writeStream close ]
]

{ #category : 'client commands' }
RowanFileService >> writeFileWithUnicode [
	" for tests" 
| unicode fileReference writeStream |
	unicode := 'This is some unicode text
  
ý š

Another extended char - Ü  aaa œ'.

	fileReference := FileReference fileSystem: FileSystem disk path:  FileSystem workingDirectory pathString, '/testJadeiteUnicodeFile.txt'.
	writeStream := fileReference writeStream.
	[ 
	writeStream
		nextPutAll: unicode asUnicodeString;
		flush ]
		ensure: [ writeStream close ].
]
