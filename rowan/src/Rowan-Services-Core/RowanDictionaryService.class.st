"
No class-specific documentation for RowanDictionaryService, hierarchy is:
Object
  RowanService( definition updates command commandArgs updateType organizer)
    RowanDictionaryService( name classes hierarchyServices globals defaultTemplate)

"
Class {
	#name : 'RowanDictionaryService',
	#superclass : 'RowanService',
	#instVars : [
		'name',
		'classes',
		'hierarchyServices',
		'globals',
		'defaultTemplate',
		'testClasses',
		'classCategories'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'rsr' }
RowanDictionaryService class >> templateClassName [

	^#RowanDictionaryService
]

{ #category : 'client commands' }
RowanDictionaryService >> addClassesNamed: classNames [

	| theClass dict |
	dict := Rowan image symbolList objectNamed: name.
	classNames do: [ :className |
			theClass := Rowan image symbolList objectNamed: className.
			dict at: className asSymbol put: theClass ]
]

{ #category : 'private' }
RowanDictionaryService >> categoryIn: selectedClassCategories satisfying: theClass [
	^ (theClass category = String new
		and: [ 
			(selectedClassCategories
				detect: [ :classCategory | classCategory includes: theClass category ]
				ifNone: [  ]) notNil ])
		or: [ 
			(selectedClassCategories
				detect: [ :classCategory | theClass category match: classCategory ]
				ifNone: [  ]) notNil ]
]

{ #category : 'accessing' }
RowanDictionaryService >> classes [
	^classes
]

{ #category : 'accessing' }
RowanDictionaryService >> classes: object [
	classes := object
]

{ #category : 'client commands' }
RowanDictionaryService >> classHierarchy [
	| theClasses |
	self update. 
	theClasses := classes collect:[:classService | classService theClass].
	"reuse behavior in package service for now" 
	hierarchyServices := (RowanPackageService new classes: classes) classHierarchy: theClasses. 
	RowanCommandResult addResult: self.
]

{ #category : 'command support' }
RowanDictionaryService >> classServiceFor: className in: dictionary [

	| classService theClass |
	theClass := dictionary at: className.
	classService := RowanClassService new name: className asString.
	classService versions: theClass classHistory size.
	classService version: (theClass classHistory indexOf: theClass).
	classService setIsTestCase.
	classService classCategory: theClass category.
	^classService
]

{ #category : 'Updating' }
RowanDictionaryService >> defaultTemplate: newValue [
	defaultTemplate := newValue
]

{ #category : 'command support' }
RowanDictionaryService >> genericClassCreationTemplate [

	^self browserTool classCreationTemplateForSubclassOf: 'Object' category: nil packageName: nil
]

{ #category : 'initialization' }
RowanDictionaryService >> initialize [

	self setDefaultTemplate
]

{ #category : 'client commands' }
RowanDictionaryService >> insertAt: index [

	| theDictionary |
	theDictionary := SymbolDictionary new. 
	theDictionary at: name asSymbol put: theDictionary. 
	System myUserProfile insertDictionary: theDictionary at: index. 
	RowanBrowserService new updateDictionaries.
]

{ #category : 'testing' }
RowanDictionaryService >> isDictionaryService [
  ^ true
]

{ #category : 'client commands' }
RowanDictionaryService >> jfpwor_removeClass: classService [
	self jfpwor_removeClassNamed: classService name. 
	self setDefaultTemplate.
	classService updateType: #removedClass:.
	RowanCommandResult addResult: classService
]

{ #category : 'client commands' }
RowanDictionaryService >> jfpwor_removeClassNamed: className [

	| dict |
	dict := Rowan image symbolList objectNamed: name.
	dict removeKey: className asSymbol ifAbsent: [  ]
]

{ #category : 'accessing' }
RowanDictionaryService >> name [
	^name
]

{ #category : 'accessing' }
RowanDictionaryService >> name: object [
	name := object
]

{ #category : 'client commands' }
RowanDictionaryService >> removeClass: classService [
	classService ifNil: [^self]. 
	self removeClassNamed: classService name. 
	self setDefaultTemplate.
	classService updateType: #removedClass:.
	RowanCommandResult addResult: classService
]

{ #category : 'client commands' }
RowanDictionaryService >> removeClassNamed: className [
	[ self browserTool removeClassNamed: className ]
		on: RwPerformingUnpackagedEditNotification
		do: [ :ex | ex resume ]
]

{ #category : 'client commands' }
RowanDictionaryService >> removeGlobalNamed: symbol [
	| dictionary |
	dictionary := (System myUserProfile resolveSymbol: name asSymbol) value.
	dictionary ifNotNil: [
		dictionary removeKey: symbol ifAbsent:[]].
	self update.
]

{ #category : 'perform' }
RowanDictionaryService >> servicePerform: symbol withArguments: collection [
	self isUpdatingButFoundToBeDeleted
		ifTrue: [ ^ self handleDeletedService ].
	super
		servicePerform: symbol
		withArguments: collection
		shouldUpdate: self shouldUpdate
]

{ #category : 'client commands' }
RowanDictionaryService >> setDefaultTemplate [

	defaultTemplate := self genericClassCreationTemplate.
]

{ #category : 'Updating' }
RowanDictionaryService >> testClasses [

	self update. 
	testClasses := Set new.
	self classes
		do: [ :classService | 
			| cls |
			cls := classService theClass.
			(cls inheritsFrom: TestCase)
				ifTrue: [ 
					cls isAbstract
						ifFalse: [ testClasses add: classService update] ] ].
	updateType := #'testClasses:browser:'.
	testClasses := testClasses asArray.
	RowanCommandResult addResult: self
]

{ #category : 'updates' }
RowanDictionaryService >> update [

	| dictionary theClasses sortedGlobals |
	super update.
	organizer := ClassOrganizer new.
	(organizer class canUnderstand: #classCategoryNamesInDictionaryName:)
		ifFalse: [ ^ self updatePre375 ].
	dictionary := Rowan image symbolList objectNamed: name.
	dictionary ifNil: [ ^ self ].
	(dictionary isKindOf: SymbolDictionary) ifFalse: [ ^ self ].
	classCategories := organizer classCategoryNamesInDictionaryName: name.
	theClasses := Array new.
	classCategories do: [ :classCategory |
			| classNames |
			classNames := organizer
				              classNamesInClassCategoryNamed: classCategory
				              dictionaryName: name.
			theClasses addAll: (classNames collect: [ :className |
					 self classServiceFor: className in: dictionary ]) ].
	classes := (theClasses asSortedCollection: [ :x :y | x name < y name ])
		           asArray.
	sortedGlobals := SortedCollection sortBlock: [ :x :y |
		                 x first < y first ].
	self updateGlobalsFor: dictionary.
	RowanCommandResult addResult: self
]

{ #category : 'updates' }
RowanDictionaryService >> updateClassCategories [
	"just do a full update for now" 
	self update.
]

{ #category : 'private' }
RowanDictionaryService >> updateForClassCategories: selectedClassCategories [
	| dictionary sorted theClasses theClassCategories |
	theClasses := OrderedCollection new.
	sorted := SortedCollection sortBlock: [ :x :y | x first < y first ].
	dictionary := Rowan image symbolList objectNamed: name.
	dictionary ifNil: [ ^ self ].
	(dictionary isKindOf: SymbolDictionary)
		ifFalse: [ ^ self ].
	theClassCategories := Set new.
	dictionary
		keysAndValuesDo: [ :key :value | 
			value isClass
				ifTrue: [ 
					| classService |
					(self categoryIn: selectedClassCategories satisfying: value)
						ifTrue: [ 
							classService := RowanClassService new name: key asString.
							classService versions: value classHistory size.
							classService version: (value classHistory indexOf: value).
							classService setIsTestCase.
							classService classCategory: value category.
							theClasses add: classService ].
					theClassCategories add: value category ]
				ifFalse: [ 
					| printString theKey |
					printString := [ 
					value printString charSize > 1
						ifTrue: [ '<<unprintable string. charSize > 1>>' ]
						ifFalse: [ value printString ] ]
						on: Error
						do: [ :ex | 'unprintable string. Error - <' , ex printString , '>' ].
					key charSize = 1
						ifTrue: [ theKey := key ]
						ifFalse: [ theKey := '<<unprintable string. charSize > 1>>' ].
					sorted
						add:
							(Array
								with: name , '.' , theKey
								with: value class name
								with: value asOop
								with: printString) ] ].
	classes := theClasses.
	globals := sorted asArray.
	classCategories := theClassCategories asSortedCollection asArray.
	RowanCommandResult addResult: self
]

{ #category : 'updates' }
RowanDictionaryService >> updateForClassCategory: classCategory [

	| dictionary theClasses classNames |
	self update.
	classNames := organizer
		              classNamesUnderClassCategoryNamed: classCategory
		              dictionaryName: name.
	theClasses addAll: (classNames collect: [ :className |
			 self classServiceFor: className in: dictionary ]).
	classes := (theClasses asSortedCollection: [ :x :y | x name < y name ])
		           asArray
]

{ #category : 'updates' }
RowanDictionaryService >> updateGlobalsFor: dictionary [

	| sorted |
	sorted := SortedCollection sortBlock: [ :x :y | x first < y first ].
	dictionary keysAndValuesDo: [ :key :value |
		value isClass ifFalse: [
			| printString theKey |
			printString := [
			               value printString charSize > 1
				               ifTrue: [ '<<unprintable string. charSize > 1>>' ]
				               ifFalse: [ value printString ] ]
				               on: Error
				               do: [ :ex |
				               'unprintable string. Error - <' , ex printString
				               , '>' ].
			key charSize = 1
				ifTrue: [ theKey := key ]
				ifFalse: [ theKey := '<<unprintable string. charSize > 1>>' ].
			sorted add: (Array
					 with: name , '.' , theKey
					 with: value class name
					 with: value asOop
					 with: printString) ] ].
	globals := sorted asArray
]

{ #category : 'updates' }
RowanDictionaryService >> updatePre375 [
	"delete when we no  longer support organizers without class category support"
	| dictionary sorted theClasses theClassCategories |
	theClasses := Array new.
	sorted := SortedCollection sortBlock: [ :x :y | x first < y first ].
	dictionary := Rowan image symbolList objectNamed: name.
	dictionary ifNil: [ ^ self ].
	(dictionary isKindOf: SymbolDictionary) ifFalse: [ ^ self ].
	theClassCategories := Set new.
	dictionary keysAndValuesDo: [ :key :value |
			value isClass
				ifTrue: [
						| classService |
						classService := RowanClassService new name: key asString.
						classService versions: value classHistory size.
						classService version: (value classHistory indexOf: value).
						classService setIsTestCase.
						classService classCategory: value category.
						theClasses add: classService.
						theClassCategories add: value category ]
				ifFalse: [
						| printString theKey |
						printString := [
							               value printString charSize > 1
								               ifTrue: [
								               '<<unprintable string. charSize > 1>>' ]
								               ifFalse: [ value printString ] ]
							               on: Error
							               do: [ :ex |
							               'unprintable string. Error - <' , ex printString
							               , '>' ].
						key charSize = 1
							ifTrue: [ theKey := key ]
							ifFalse: [ theKey := '<<unprintable string. charSize > 1>>' ].
						sorted add: (Array
								 with: name , '.' , theKey
								 with: value class name
								 with: value asOop
								 with: printString) ] ].
	classes := theClasses.
	globals := sorted asArray.
	classCategories := theClassCategories asSortedCollection asArray.
	RowanCommandResult addResult: self
]

{ #category : 'testing' }
RowanDictionaryService >> wasDeleted [
  ^ (Rowan globalNamed: name) isNil
]
