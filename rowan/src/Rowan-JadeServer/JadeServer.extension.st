Extension { #name : 'JadeServer' }

{ #category : 'category' }
JadeServer >> mcInitials: aString [
	"Do initial setup and return useful information"

	| mcPlatformSupport packagePolicyEnabledFlag string x |
	string := 'Jade-' , GsSession currentSession serialNumber printString , '-' , System myUserProfile userId.
	[
		self mcInitialsA: string.
	] whileFalse: [	"Keep shortening it till it fits!"
		string := string copyFrom: 1 to: string size - 1.
	].
	mcPlatformSupport := self objectInBaseNamed: #'MCPlatformSupport'.
	mcPlatformSupport notNil ifTrue: [mcPlatformSupport setAuthorInitials: aString].
	packagePolicyEnabledFlag := (x := self objectInBaseNamed: #'GsPackagePolicy') isNil ifTrue: ['0'] ifFalse: [x current enabled ifTrue: ['1'] ifFalse: ['0']].
	^System session printString , Character space asString , 
		(GsSession serialOfSession: System session) printString , Character space asString , 
		packagePolicyEnabledFlag

]

{ #category : 'category' }
JadeServer >> mcInitialsA: aString [
	"Subclasses provide error handling, typically means string is too long"

	System _cacheName: aString.

]

{ #category : 'category' }
JadeServer >> objectInBaseNamed: aString [ 

	^[(SymbolList withAll: self class sharedPools) objectNamed: aString asSymbol] on: Error do: [:ex | ex return: nil].

]
