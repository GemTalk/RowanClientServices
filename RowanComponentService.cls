"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanComponentService
	instanceVariableNames: 'name componentServices packageServices projectService basename'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanComponentService guid: (GUID fromString: '{842be73b-a371-499f-a6d4-dcd4a81c01e5}')!
RowanComponentService comment: ''!
!RowanComponentService categoriesForClass!Unclassified! !
!RowanComponentService methodsFor!

basename
	^basename!

basename: anObject
	basename := anObject!

componentPackagesUpdate: presenter browser: browser
	packageServices isEmpty ifTrue:[^self]. 
	self
		packagesUpdate: presenter
		browser: browser
		parentPresenter: browser componentListPresenter!

displayString
	^basename!

name
	^name!

packageServices
	^packageServices!

printOn: stream
	stream
		nextPutAll: self class name;
		nextPutAll: '->';
		nextPutAll: basename! !
!RowanComponentService categoriesFor: #basename!accessing!private! !
!RowanComponentService categoriesFor: #basename:!accessing!private! !
!RowanComponentService categoriesFor: #componentPackagesUpdate:browser:!public!updating! !
!RowanComponentService categoriesFor: #displayString!displaying!public! !
!RowanComponentService categoriesFor: #name!accessing!public! !
!RowanComponentService categoriesFor: #packageServices!accessing!private! !
!RowanComponentService categoriesFor: #printOn:!displaying!public! !

