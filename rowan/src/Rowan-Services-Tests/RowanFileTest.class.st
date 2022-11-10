"
test services that are file related
"
Class {
	#name : 'RowanFileTest',
	#superclass : 'RowanServicesTest',
	#instVars : [
		'filesystem',
		'filename'
	],
	#category : 'Rowan-Services-Tests'
}

{ #category : 'constants' }
RowanFileTest class >> testFileInSource [


^'fileformat utf8
!
! From ! GEMSTONE: 3.7.0, Wed Oct  5 13:41:47 2022 ewinger private build (branch 370_rowanv3); IMAGE: GemStone/S64 v3.7.0 kernel classes filein completed at 05/10/2022 13:54:52
! 

! On 27/10/2022, 15:23:11
!
set compile_env: 0
! ------------------- Class definition for RowanSampleTestFileIn
expectvalue /Class
doit
Object subclass: ''RowanSampleTestFileIn''
  instVarNames: #()
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: SampleSymbolDict
  options: #()

%
expectvalue /Class
doit
RowanSampleTestFileIn category: ''RowanSampleTestFileIn-Core''
%
! ------------------- Remove existing behavior from RowanSampleTestFileIn
expectvalue /Metaclass3
doit
RowanSampleTestFileIn removeAllMethods.
RowanSampleTestFileIn class removeAllMethods.
%
set compile_env: 0
! ------------------- Class methods for RowanSampleTestFileIn
category: ''*RowanSampleTestFileIn-extensions''
classmethod: RowanSampleTestFileIn
bar ^''bar''
%
! ------------------- Instance methods for RowanSampleTestFileIn
category: ''accessing''
method: RowanSampleTestFileIn
foo ^''foo''
%
fileformat utf8
!
! From ! GEMSTONE: 3.7.0, Mon Oct 31 14:03:00 2022 ewinger private build (branch 370_rowanv3); IMAGE: GemStone/S64 v3.7.0 kernel classes filein completed at 31/10/2022 14:16:18
! 

! On 04/11/2022, 15:39:38
!
set compile_env: 0
! ------------------- Class definition for RowanSampleTestFileIn2
expectvalue /Class
doit
Object subclass: ''RowanSampleTestFileIn2''
  instVarNames: #()
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: SampleSymbolDict
  options: #()

%
expectvalue /Class
doit
RowanSampleTestFileIn2 category: ''RowanSampleTestFileIn2-Core''
%
! ------------------- Remove existing behavior from RowanSampleTestFileIn2
expectvalue /Metaclass3
doit
RowanSampleTestFileIn2 removeAllMethods.
RowanSampleTestFileIn2 class removeAllMethods.
%
set compile_env: 0
! ------------------- Class methods for RowanSampleTestFileIn2
category: ''*RowanSampleTestFileIn2-extensions''
classmethod: RowanSampleTestFileIn2
bar ^''bar''
%
! ------------------- Instance methods for RowanSampleTestFileIn2
category: ''accessing''
method: RowanSampleTestFileIn2
foo
	^''foo''
%'
]

{ #category : 'constants' }
RowanFileTest class >> testFileInSourceWithWarnings [


^'fileformat utf8
!
! From ! GEMSTONE: 3.7.0, Wed Oct  5 13:41:47 2022 ewinger private build (branch 370_rowanv3); IMAGE: GemStone/S64 v3.7.0 kernel classes filein completed at 05/10/2022 13:54:52
! 

! On 27/10/2022, 15:23:11
!
set compile_env: 0
! ------------------- Class definition for RowanSampleTestFileIn
expectvalue /Class
doit
Object subclass: ''RowanSampleTestFileIn''
  instVarNames: #()
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: SampleSymbolDict
  options: #()

%
expectvalue /Class
doit
RowanSampleTestFileIn category: ''RowanSampleTestFileIn-Core''
%
! ------------------- Remove existing behavior from RowanSampleTestFileIn
expectvalue /Metaclass3
doit
RowanSampleTestFileIn removeAllMethods.
RowanSampleTestFileIn class removeAllMethods.
%
set compile_env: 0
! ------------------- Class methods for RowanSampleTestFileIn
category: ''*RowanSampleTestFileIn-extensions''
classmethod: RowanSampleTestFileIn
bar | unused | ^''bar''
%
! ------------------- Instance methods for RowanSampleTestFileIn
category: ''accessing''
method: RowanSampleTestFileIn
foo | unused | ^''foo''
%
fileformat utf8
!
! From ! GEMSTONE: 3.7.0, Mon Oct 31 14:03:00 2022 ewinger private build (branch 370_rowanv3); IMAGE: GemStone/S64 v3.7.0 kernel classes filein completed at 31/10/2022 14:16:18
! 

! On 04/11/2022, 15:39:38
!
set compile_env: 0
! ------------------- Class definition for RowanSampleTestFileIn2
expectvalue /Class
doit
Object subclass: ''RowanSampleTestFileIn2''
  instVarNames: #()
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: SampleSymbolDict
  options: #()

%
expectvalue /Class
doit
RowanSampleTestFileIn2 category: ''RowanSampleTestFileIn2-Core''
%
! ------------------- Remove existing behavior from RowanSampleTestFileIn2
expectvalue /Metaclass3
doit
RowanSampleTestFileIn2 removeAllMethods.
RowanSampleTestFileIn2 class removeAllMethods.
%
set compile_env: 0
! ------------------- Class methods for RowanSampleTestFileIn2
category: ''*RowanSampleTestFileIn2-extensions''
classmethod: RowanSampleTestFileIn2
bar | unused | ^''bar''
%
! ------------------- Instance methods for RowanSampleTestFileIn2
category: ''accessing''
method: RowanSampleTestFileIn2
foo
	| unused | ^''foo''
%'
]

{ #category : 'constants' }
RowanFileTest >> filedInClass1 [

	^Rowan globalNamed: 'RowanSampleTestFileIn'
]

{ #category : 'constants' }
RowanFileTest >> filedInClass2 [

	^Rowan globalNamed: 'RowanSampleTestFileIn2'
]

{ #category : 'support' }
RowanFileTest >> fileReference [
	^ filesystem workingDirectory / filename
]

{ #category : 'support' }
RowanFileTest >> saveTestFile [
	|  reference writeStream |
	filename := 'fileInTestFile'.
	filesystem := FileSystem disk.
	reference := filesystem workingDirectory / filename. 
	self deny: reference isFile.
	writeStream := reference writeStream.
	[ writeStream nextPutAll: self class testFileInSource ]
		ensure: [ writeStream close ].
	self assert: reference isFile
]

{ #category : 'support' }
RowanFileTest >> saveTestFileWithCompileWarnings [
	|  reference writeStream |
	filename := 'testFileInSourceWithWarnings'.
	filesystem := FileSystem disk.
	reference := filesystem workingDirectory / filename. 
	self deny: reference isFile.
	writeStream := reference writeStream.
	[ writeStream nextPutAll: self class testFileInSourceWithWarnings ]
		ensure: [ writeStream close ].
	self assert: reference isFile
]

{ #category : 'running' }
RowanFileTest >> test_fileIn [
	| service |
	self
		jadeiteIssueTested: #'issue70'
		withTitle:
			'Fileout and filein: especially Dictionaries Browser Dictionaries > File In Code'.
	self saveTestFile.
	self assert: self filedInClass1 isNil.
	self assert: self filedInClass2 isNil.
	service := RowanFileService new.
	service fileIn: self fileReference pathString.
	[ 
	self deny: self filedInClass1 isNil.
	self
		assert: (self filedInClass1 compiledMethodAt: #'foo') sourceString
		equals:
			'foo ^''foo''
'.
	self deny: self filedInClass2 isNil.	"return the service filled with the file contents"
	self
		assert: (self filedInClass2 class compiledMethodAt: #'bar') sourceString
		equals:
			'bar ^''bar''
'.
	self assert: (RowanCommandResult results includes: service).
	self assert: service answer = self class testFileInSource ]
		ensure: [ 
			[ 
			filesystem delete: self fileReference.
			self deny: self fileReference exists.
			Rowan platform projectTools browser removeClassNamed: self filedInClass1 name.
			Rowan platform projectTools browser removeClassNamed: self filedInClass2 name ]
				on: RwPerformingUnpackagedEditNotification
				do: [ :ex | ex resume ].
			self assert: self filedInClass1 isNil.
			self assert: self filedInClass2 isNil ]
]

{ #category : 'running' }
RowanFileTest >> test_fileInDoesNotWarn [
	"compile warnings are ignored on filein"

	| service filenameWithWarnings |
	self jadeiteIssueTested: #issue70 withTitle: 'Fileout and filein: especially Dictionaries Browser Dictionaries > File In Code'.
	filenameWithWarnings := 'testFileInSourceWithWarnings'.
	self saveTestFileWithCompileWarnings.
	self assert: self filedInClass1 isNil.
	self assert: self filedInClass2 isNil.
	service := RowanFileService new.
	service fileIn: (filesystem workingDirectory / filenameWithWarnings) pathString.
	[ 
	self deny: self filedInClass1 isNil.
	self
		assert: (self filedInClass1 compiledMethodAt: #'foo') sourceString
		equals:
			'foo | unused | ^''foo''
'.
	self deny: self filedInClass2 isNil.	"return the service filled with the file contents"
	self
		assert: (self filedInClass2 class compiledMethodAt: #'bar') sourceString
		equals:
			'bar | unused | ^''bar''
' ]
		ensure: [ 
			[ 
			filesystem delete: filesystem workingDirectory / filenameWithWarnings.
			self deny: (filesystem workingDirectory / filenameWithWarnings) exists.
			Rowan platform projectTools browser removeClassNamed: self filedInClass1 name.
			Rowan platform projectTools browser removeClassNamed: self filedInClass2 name ]
				on: RwPerformingUnpackagedEditNotification
				do: [ :ex | ex resume ].
			self assert: self filedInClass1 isNil.
			self assert: self filedInClass2 isNil ]
]

{ #category : 'running' }
RowanFileTest >> test_fileServiceAttributes [
	"if path is a full path to a file, be able to retrieve some information about it"
	| fileService |
	self jadeiteIssueTested: #issue953 withTitle: 'File Browser for server files'.
	fileService := RowanFileService new path: '/export/gcm/where/gemstone64/releases/3.6.1/x86_64.Linux/fast/product/version.txt'. 
	self assert: fileService fileName equals: 'version.txt'. 
	self assert: fileService parentDirectoryPath equals: '/export/gcm/where/gemstone64/releases/3.6.1/x86_64.Linux/fast/product'.
]
