'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:38:03 pm'!



((KRONO_Reporter createSubApplication: #KRONO_Reporter_System_Extensions in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

KRONO_Reporter_System_Extensions becomeDefault!

SubApplication subclass: #KRONO_Reporter_System_Extensions
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_System_Extensions becomeDefault!

!ArithmeticValue publicMethodsFor: 'KR-reporter'!

accumulatorFor: t1 

	^t1 accumulatorForArithmetic! !

!Behavior publicMethodsFor: 'KR-reporter'!

initialLetter

	^self name first!

twoFirstLetters

	^self name copyFrom: 1 to: 2! !

!ComponentSpec publicMethodsFor: 'KR-reporter'!

fullyRealize
	self layout: self layout fullyRealized.
	^self!

preferredHeightUsingValue: t1 
	^self subclassResponsibility!

realizeXFrom: t1 
	self layout realizeXFrom: t1.
	^self!

realizeYFrom: t1 dynamicHeight: t2 
	self layout realizeYFrom: t1 dynamicHeight: t2.
	^self! !

!Integer publicMethodsFor: 'KR-reporter'!

asKRPosition
	^KRAbsolutePosition value: self!

asKRPositioningSpec

	^KRAbsolutePosition value: self!

isDynamicPosition

	^false!

realizeFrom: t1 dynamicHeight: t2 

	^self! !

!Object publicMethodsFor: 'KR-reporter'!

accumulatorFor: t1 

	^t1 accumulatorForObject! !

!PostScriptPrinter publicMethodsFor: 'KR-report-extensions'!

withHardcopyPreferencesDo: t1 
	| t2 |
	self defaultPaintPreferences.
	t2 := defaultPaintPreferences.
	
	[(defaultPaintPreferences := defaultPaintPreferences collect: [:t3 | t3 copy]) do: [:t4 | t4 == nil
			ifTrue: [nil]
			ifFalse: 
				[t4 at: #inactive_backgroundColor put: ColorValue white.
				t4 at: #backgroundColor put: ColorValue white.
				t4 at: #inactive_backgroundColor put: ColorValue white.
				t4 at: #inactive_foregroundColor put: ColorValue black.
				t4 at: #shadowColor put: ColorValue white.
				t4 at: #traversalHiliteColor put: ColorValue white]].
	t1 value]
		valueNowOrOnUnwindDo: [defaultPaintPreferences := t2].
	^self!

withVisualComponentPreferencesDo: t1 

	| t2 |
	self defaultPaintPreferences.
	t2 := defaultPaintPreferences.
	
	[(defaultPaintPreferences := defaultPaintPreferences collect: [:t3 | t3 copy]) do: [:t4 | t4 == nil
			ifTrue: [nil]
			ifFalse: [nil]].
	t1 value]
		valueNowOrOnUnwindDo: [defaultPaintPreferences := t2].
	^self! !

!Rectangle publicMethodsFor: 'KR-reporter'!

fullyRealized
	^self!

realizeXFrom: t1 
	^self!

realizeYFrom: t1 dynamicHeight: t2 

	^self! !

!Symbol publicMethodsFor: 'aspect evaluating'!

aspectValue: t1 

	^t1 perform: self! !

!Symbol publicMethodsFor: 'converting'!

aspectStringOn: t1 

	t1 nextPutAll: self asString.
	^self! !

!TextEditorSpec publicMethodsFor: 'KR-reporter'!

preferredHeightUsingValue: t1 

	| t2 |
	t2 := TextAttributes styleNamed: self style ifAbsent: [TextAttributes default].
	^(ComposedText
		withText: t1
		style: t2
		compositionWidth: self layout width - 12) height + 2!

preferredHeightUsingValue: t1 forDevice: t2 

	| t3 t4 |
	t3 := TextAttributes styleNamed: self style ifAbsent: [TextAttributes default].
	(t4 := ComposedText
				withText: t1
				style: t3
				compositionWidth: self layout width - 12) newGraphicsDevice: t2.
	^t4 height + 2! !

KRONO_Reporter_System_Extensions initializeAfterLoad!

KRONO_Reporter_System_Extensions loaded!
