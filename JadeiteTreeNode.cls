"Filed out from Dolphin Smalltalk 7"!

TreeNode subclass: #JadeiteTreeNode
	instanceVariableNames: 'shouldRemove'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeiteTreeNode guid: (GUID fromString: '{94ece110-8aca-4f84-9f75-546040c70901}')!
JadeiteTreeNode comment: 'The inst var `marked` is used when updating a JadeiteTreeModel. 
After a node is added, it is marked.  Nodes that are not marked are subsequently removed. '!
!JadeiteTreeNode categoriesForClass!MVP-Models-Support! !
!JadeiteTreeNode methodsFor!

initialize
	super initialize.
	shouldRemove := true!

shouldRemove
	^shouldRemove!

shouldRemove: anObject
	shouldRemove := anObject! !
!JadeiteTreeNode categoriesFor: #initialize!initialization!public! !
!JadeiteTreeNode categoriesFor: #shouldRemove!accessing!private! !
!JadeiteTreeNode categoriesFor: #shouldRemove:!accessing!private! !

