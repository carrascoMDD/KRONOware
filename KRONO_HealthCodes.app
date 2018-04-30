'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:38:03 pm'!



Application create: #KRONO_HealthCodes with: 
    (#( CODE_META)
        collect: [:each | Smalltalk at: each ifAbsent: [
        self error: 'Not all of the prerequisites are loaded']])!

KRONO_HealthCodes becomeDefault!

CMInfoPersistencyHolder subclass: #KRIcd9CMCodesInfoHolder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_HealthCodes becomeDefault!

Object subclass: #KRIcd9CMCodesImporter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_HealthCodes becomeDefault!

Application subclass: #KRONO_HealthCodes
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_HealthCodes becomeDefault!

!KRIcd9CMCodesImporter class publicMethodsFor: 'file names'!

diseasesTablePathName
	^'C:\Works\VW\KronoWare\specs\Codificacion\Dtab02Import.txt' copy!

proceduresTablePathName
	^'C:\Works\VW\KronoWare\specs\Codificacion\Ptab02Import.txt' copy! !

!KRIcd9CMCodesImporter class publicMethodsFor: 'import'!

buildCategoryCoded: theCategoryCode named: theCategoryName onCodificacionEnfermedades: theCodificacion

	| aCategoryCode aCategoryName aCategory |
	(theCategoryCode isNil or: [ theCategoryCode isEmpty]) ifTrue: [^nil].
	aCategoryCode := theCategoryCode trimBlanks.
	aCategoryCode isEmpty ifTrue: [^nil].

	(theCategoryName isNil or: [ theCategoryName isEmpty]) ifTrue: [^nil].
	aCategoryName := theCategoryName trimBlanks.
	aCategoryName isEmpty ifTrue: [^nil].

	aCategory := theCodificacion metaInfo addToObject: theCodificacion 
		featureNamedValue: 'categoriasEnfermedades'
		create: (Array 
			with: (Array with: 'codigoCategoria' with: aCategoryCode) 
			with: (Array with: 'nombreCategoria' with: aCategoryName)).

	self trace ifTrue:  [ Transcript show: '+C '; show: aCategoryCode; space; show: aCategoryName; cr].

	^aCategory!

buildCategoryCoded: theCategoryCode named: theCategoryName onCodificacionProcedimientos: theCodificacion

	| aCategoryCode aCategoryName aCategory |
	(theCategoryCode isNil or: [ theCategoryCode isEmpty]) ifTrue: [^nil].
	aCategoryCode := theCategoryCode trimBlanks.
	aCategoryCode isEmpty ifTrue: [^nil].

	(theCategoryName isNil or: [ theCategoryName isEmpty]) ifTrue: [^nil].
	aCategoryName := theCategoryName trimBlanks.
	aCategoryName isEmpty ifTrue: [^nil].

	aCategory := theCodificacion metaInfo addToObject: theCodificacion 
		featureNamedValue: 'categoriasProcedimientos'
		create: (Array 
			with: (Array with: 'codigoCategoria' with: aCategoryCode) 
			with: (Array with: 'nombreCategoria' with: aCategoryName)).

	self trace ifTrue:  [ Transcript show: '+C '; show: aCategoryCode; space; show: aCategoryName; cr].

	^aCategory!

buildEN1Coded: theEN1Code named: theEN1Name onCategoryEnfermedades: theCategory

	| aEN1Code aEN1Name aEN1 |
	(theEN1Code isNil or: [ theEN1Code isEmpty]) ifTrue: [^nil].
	aEN1Code := theEN1Code trimBlanks.
	aEN1Code isEmpty ifTrue: [^nil].

	(theEN1Name isNil or: [ theEN1Name isEmpty]) ifTrue: [^nil].
	aEN1Name := theEN1Name trimBlanks.
	aEN1Name isEmpty ifTrue: [^nil].

	aEN1 := theCategory metaInfo addToObject: theCategory 
		featureNamedValue: 'enfermedadesNivel1'
		create: (Array 
			with: (Array with: 'codigoEnfermedad' with: aEN1Code) 
			with: (Array with: 'nombreEnfermedad' with: aEN1Name)).

	self trace ifTrue:  [ Transcript show: '+EN1 '; show: aEN1Code; space; show: aEN1Name; cr].

	^aEN1!

buildEN2Coded: theEN2Code named: theEN2Name onEN1: theEN1

	| aEN2Code aEN2Name aEN2 |
	(theEN2Code isNil or: [ theEN2Code isEmpty]) ifTrue: [^nil].
	aEN2Code := theEN2Code trimBlanks.
	aEN2Code isEmpty ifTrue: [^nil].

	(theEN2Name isNil or: [ theEN2Name isEmpty]) ifTrue: [^nil].
	aEN2Name := theEN2Name trimBlanks.
	aEN2Name isEmpty ifTrue: [^nil].

	aEN2 := theEN1 metaInfo addToObject: theEN1 
		featureNamedValue: 'enfermedadesNivel2'
		create: (Array 
			with: (Array with: 'codigoEnfermedad' with: aEN2Code) 
			with: (Array with: 'nombreEnfermedad' with: aEN2Name)).

	self trace ifTrue:  [ Transcript show: '+EN2 '; show: aEN2Code; space; show: aEN2Name; cr].

	^aEN2!

buildEN3Coded: theEN3Code named: theEN3Name onEN2: theEN2

	| aEN3Code aEN3Name aEN3 |
	(theEN3Code isNil or: [ theEN3Code isEmpty]) ifTrue: [^nil].
	aEN3Code := theEN3Code trimBlanks.
	aEN3Code isEmpty ifTrue: [^nil].

	(theEN3Name isNil or: [ theEN3Name isEmpty]) ifTrue: [^nil].
	aEN3Name := theEN3Name trimBlanks.
	aEN3Name isEmpty ifTrue: [^nil].

	aEN3 := theEN2 metaInfo addToObject: theEN2 
		featureNamedValue: 'enfermedadesNivel3'
		create: (Array 
			with: (Array with: 'codigoEnfermedad' with: aEN3Code) 
			with: (Array with: 'nombreEnfermedad' with: aEN3Name)).

	self trace ifTrue:  [ Transcript show: '+EN3 '; show: aEN3Code; space; show: aEN3Name; cr].

	^aEN3!

buildPR1Coded: thePR1Code named: thePR1Name onCategoryProcedimientos: theCategory

	| aPR1Code aPR1Name aPR1 |
	(thePR1Code isNil or: [ thePR1Code isEmpty]) ifTrue: [^nil].
	aPR1Code := thePR1Code trimBlanks.
	aPR1Code isEmpty ifTrue: [^nil].

	(thePR1Name isNil or: [ thePR1Name isEmpty]) ifTrue: [^nil].
	aPR1Name := thePR1Name trimBlanks.
	aPR1Name isEmpty ifTrue: [^nil].

	aPR1 := theCategory metaInfo addToObject: theCategory 
		featureNamedValue: 'procedimientosNivel1'
		create: (Array 
			with: (Array with: 'codigoProcedimiento' with: aPR1Code) 
			with: (Array with: 'nombreProcedimiento' with: aPR1Name)).

	self trace ifTrue:  [ Transcript show: '+PR1 '; show: aPR1Code; space; show: aPR1Name; cr].

	^aPR1!

buildPR2Coded: thePR2Code named: thePR2Name onPR1: thePR1

	| aPR2Code aPR2Name aPR2 |
	(thePR2Code isNil or: [ thePR2Code isEmpty]) ifTrue: [^nil].
	aPR2Code := thePR2Code trimBlanks.
	aPR2Code isEmpty ifTrue: [^nil].

	(thePR2Name isNil or: [ thePR2Name isEmpty]) ifTrue: [^nil].
	aPR2Name := thePR2Name trimBlanks.
	aPR2Name isEmpty ifTrue: [^nil].

	aPR2 := thePR1 metaInfo addToObject: thePR1 
		featureNamedValue: 'procedimientosNivel2'
		create: (Array 
			with: (Array with: 'codigoProcedimiento' with: aPR2Code) 
			with: (Array with: 'nombreProcedimiento' with: aPR2Name)).

	self trace ifTrue:  [ Transcript show: '+PR2 '; show: aPR2Code; space; show: aPR2Name; cr].

	^aPR2!

buildPR3Coded: thePR3Code named: thePR3Name onPR2: thePR2

	| aPR3Code aPR3Name aPR3 |
	(thePR3Code isNil or: [ thePR3Code isEmpty]) ifTrue: [^nil].
	aPR3Code := thePR3Code trimBlanks.
	aPR3Code isEmpty ifTrue: [^nil].

	(thePR3Name isNil or: [ thePR3Name isEmpty]) ifTrue: [^nil].
	aPR3Name := thePR3Name trimBlanks.
	aPR3Name isEmpty ifTrue: [^nil].

	aPR3 := thePR2 metaInfo addToObject: thePR2 
		featureNamedValue: 'procedimientosNivel3'
		create: (Array 
			with: (Array with: 'codigoProcedimiento' with: aPR3Code) 
			with: (Array with: 'nombreProcedimiento' with: aPR3Name)).

	self trace ifTrue:  [ Transcript show: '+PR3 '; show: aPR3Code; space; show: aPR3Name; cr].

	^aPR3!

enCodeFromLineStream: theStream
	| aStream aChar |
	theStream isNil ifTrue: [ ^'' copy].
	theStream atEnd ifTrue: [ ^'' copy].

	aStream := WriteStream on: (String new: 8).
	
	[ theStream atEnd] whileFalse: [ 
		aChar := theStream next.
		(aChar = $. or: [ aChar isSeparator]) ifTrue: [ ^aStream contents].
		aStream nextPut: aChar.
	].
	^aStream contents!

importDiseasesFromStream: theStream  intoCodificacion: theCodificacion

	| aCurrentCategory aCurrentEN1 aCurrentEN2 aCurrentEN3 aState aLine aLineStream aCategoryCode aCategoryName aCategory aWord aNoteText aIncludeText aCommentText aExcludeText aEN1Code aEN1Name aEN1 aEN2Code aEN2Name aEN2 someStates aStateBlock aEN3Code aEN3Name aEN3 aTransitionEx aENSubCode aPrevState aNumProcessCategoryConsecutiveStates |

	
	aCurrentCategory := nil.
	aCurrentEN1 := nil.
	aCurrentEN2 := nil.
	aCurrentEN3 := nil.

	aTransitionEx := Signal genericSignal newSignalMayProceed: true.

	someStates := IdentityDictionary new: 37.

	aState := #start.

		someStates at: #start put: [ 
			aState := #scanForCategory
		].
		someStates at: #scanForCategory put: [ "self halt: aState."
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not and: [ aLine first isDigit]]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aState := #processCategory
		].
		someStates at: #processCategory put: [ "self halt: aState." 
			aCategoryCode := aLineStream upTo: $. .
			aLineStream next; next.
			aCategoryName := aLineStream upToEnd.
			aCategory := self buildCategoryCoded: aCategoryCode named: aCategoryName onCodificacionEnfermedades: theCodificacion.
			aCategory isNil ifFalse: [ 
				aCurrentCategory := aCategory.
				aState := #scanForEN1
			]
		].
		someStates at: #scanForEN1 put: [ "self halt: aState." 
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processEN1] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #processCategoryNote ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #processCategoryIncludes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #processCategoryExcludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			aState := #processCategoryComment
			]]]]].
		].
		someStates at: #processCategoryNote put: [ "self halt: aState." 
			aNoteText := aLineStream upToEnd.
			self setCategory: aCurrentCategory note: aNoteText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processEN1] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note:' ifTrue: [  aState := #processCategoryNote ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #processCategoryIncludes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #processCategoryExcludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].
		someStates at: #processCategoryIncludes put: [ "self halt: aState." 
			aIncludeText := aLineStream upToEnd.
			self setCategory: aCurrentCategory includes: aIncludeText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processEN1] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #processCategoryNote ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #processCategoryIncludes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #processCategoryExcludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			] 
			]]]].
		].
		someStates at: #processCategoryExcludes put: [ "self halt: aState." 
			aExcludeText := aLineStream upToEnd.
			self setCategory: aCurrentCategory excludes: aExcludeText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processEN1] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #processCategoryNote ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #processCategoryIncludes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #processCategoryExcludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].
		someStates at: #processCategoryComment put: [ "self halt: aState." 
			aCommentText := aLineStream upToEnd.
			self setCategory: aCurrentCategory comment: aCommentText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processEN1] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #processCategoryNote ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #processCategoryIncludes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #processCategoryExcludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].
		someStates at: #processFifthDigitSubclassifications put: [ "self halt: aState. "
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not and: [ aLine first isDigit and: [(aLine at: 2) = Character tab ]]]] whileTrue.
			aLineStream := ReadStream on: aLine.
			aState := #processEN1.
		].





		someStates at: #processEN1 put: [ "self halt: aState." 
			aEN1Code := self enCodeFromLineStream: aLineStream.
			aEN1Code size < 3 ifTrue: [ aLineStream := ReadStream on: aLineStream contents.  aState := #processCategory. aTransitionEx raise].
			aENSubCode := (aLineStream peek = Character space or: [ aLineStream peek isLetter])
				ifTrue: [ nil] ifFalse: [ aLineStream upTo: Character tab].
			aENSubCode isNil not ifTrue: [ 
				aENSubCode size = 1 ifTrue: [  aLineStream := ReadStream on: aLineStream contents. aState := #processEN2. aTransitionEx raise].
				aENSubCode size = 2 ifTrue: [  aLineStream := ReadStream on: aLineStream contents. aState := #processEN3. aTransitionEx raise].
			].
			aLineStream skipSeparators.
			aEN1Name := aLineStream upToEnd.
			aEN1 := self buildEN1Coded: aEN1Code named: aEN1Name onCategoryEnfermedades: aCurrentCategory.
			aEN1 isNil ifFalse: [ 
				aCurrentEN1 := aEN1.
				aState := #scanForEN2
			]
		].
		someStates at: #scanForEN2 put: [ "self halt: aState." 
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processEN2] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesEN1Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesEN1Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesEN1Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			aState := #procesEN1Comment
			]]]]].
		].
		someStates at: #procesEN1Note put: [ "self halt: aState." 
			aNoteText := aLineStream upToEnd.
			self setEN: aCurrentEN1 note: aNoteText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processEN2] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesEN1Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesEN1Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesEN1Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].
		someStates at: #procesEN1Includes put: [ "self halt: aState." 
			aIncludeText := aLineStream upToEnd.
			self setEN: aCurrentEN1 includes: aIncludeText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processEN2] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesEN1Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesEN1Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesEN1Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].
		someStates at: #procesEN1Excludes put: [ "self halt: aState." 
			aExcludeText := aLineStream upToEnd.
			self setEN: aCurrentEN1 excludes: aExcludeText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processEN2] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesEN1Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesEN1Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesEN1Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].
		someStates at: #procesEN1Comment put: [ "self halt: aState." 
			aCommentText := aLineStream upToEnd.
			self setEN: aCurrentEN1 comment: aCommentText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processEN2] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesEN1Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesEN1Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesEN1Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].



		someStates at: #processEN2 put: [ "self halt: aState." 
			aEN2Code := self enCodeFromLineStream: aLineStream.
			aEN2Code size < 3 ifTrue: [ aLineStream := ReadStream on: aLineStream contents.  aState := #processCategory. aTransitionEx raise].
			aENSubCode := (aLineStream peek = Character space or: [ aLineStream peek isLetter])
				ifTrue: [ nil] ifFalse: [ aLineStream upTo: Character tab].
			aENSubCode isNil 
				ifTrue: [ aLineStream := ReadStream on: aLineStream contents. aState := #processEN1. aTransitionEx raise]
				ifFalse: [ 
					aENSubCode size = 2 ifTrue: [  aLineStream := ReadStream on: aLineStream contents.  aState := #processEN3. aTransitionEx raise]
				].
			aLineStream skipSeparators.
			aEN2Name := aLineStream upToEnd.
			aEN2 := self buildEN2Coded: aENSubCode named: aEN2Name onEN1: aCurrentEN1.
			aEN2 isNil ifFalse: [ 
				aCurrentEN2 := aEN2.
				aState := #scanForEN3
			].
	
		].
		someStates at: #scanForEN3 put: [ "self halt: aState." 
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processEN3] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesEN2Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesEN2Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesEN2Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			aState := #procesEN2Comment
			]]]]].
		].
		someStates at: #procesEN2Note put: [ "self halt: aState." 
			aNoteText := aLineStream upToEnd.
			self setEN: aCurrentEN2 note: aNoteText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processEN3] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesEN2Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesEN2Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesEN2Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].
		someStates at: #procesEN2Includes put: [ "self halt: aState." 
			aIncludeText := aLineStream upToEnd.
			self setEN: aCurrentEN2 includes: aIncludeText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processEN3] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesEN2Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesEN2Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesEN2Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].
		someStates at: #procesEN2Excludes put: [ "self halt: aState." 
			aExcludeText := aLineStream upToEnd.
			self setEN: aCurrentEN2 excludes: aExcludeText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processEN3] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesEN2Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesEN2Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesEN2Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].
		someStates at: #procesEN2Comment put: [ "self halt: aState." 
			aCommentText := aLineStream upToEnd.
			self setEN: aCurrentEN2 comment: aCommentText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processEN3] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesEN2Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesEN2Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesEN2Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].
		someStates at: #processEN3 put: [ "self halt: aState. "
			aEN3Code := self enCodeFromLineStream: aLineStream.
			aEN3Code size < 3 ifTrue: [ aLineStream := ReadStream on: aLineStream contents.  aState := #processCategory. aTransitionEx raise].
			aENSubCode := (aLineStream peek = Character space or: [ aLineStream peek isLetter])
				ifTrue: [ nil] ifFalse: [ aLineStream upTo: Character tab].
			aENSubCode isNil 
				ifTrue: [ aLineStream := ReadStream on: aLineStream contents. aState := #processEN1. aTransitionEx raise]
				ifFalse: [ 
					aENSubCode size = 1 ifTrue: [ aLineStream := ReadStream on: aLineStream contents. aState := #processEN2. aTransitionEx raise]
				].
			aLineStream skipSeparators.
			aEN3Name := aLineStream upToEnd.
			aEN3 := self buildEN3Coded: aENSubCode named: aEN3Name onEN2: aCurrentEN2.
			aEN3 isNil ifFalse: [ 
				aCurrentEN3 := aEN3.
				aState := #scanAfterEN3
			]
		].



		someStates at: #scanAfterEN3 put: [ "self halt: aState." 
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processEN3] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesEN3Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesEN3Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesEN3Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			aState := #procesEN3Comment
			]]]]].
		].
		someStates at: #procesEN3Note put: [ "self halt: aState." 
			aNoteText := aLineStream upToEnd.
			self setEN: aCurrentEN3 note: aNoteText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processEN3] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesEN3Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesEN3Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesEN3Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].
		someStates at: #procesEN3Includes put: [ "self halt: aState." 
			aIncludeText := aLineStream upToEnd.
			self setEN: aCurrentEN3 includes: aIncludeText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processEN3] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesEN3Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesEN3Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesEN3Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].
		someStates at: #procesEN3Excludes put: [ "self halt: aState." 
			aExcludeText := aLineStream upToEnd.
			self setEN: aCurrentEN3 excludes: aExcludeText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processEN3] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesEN3Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesEN3Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesEN3Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].
		someStates at: #procesEN3Comment put: [ "self halt: aState." 
			aCommentText := aLineStream upToEnd.
			self setEN: aCurrentEN3 comment: aCommentText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processEN3] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesEN3Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesEN3Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesEN3Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].


	aNumProcessCategoryConsecutiveStates := 0.

	[ aState = #end] whileFalse: [ 
		aStateBlock := someStates at: aState ifAbsent: [nil].
		aStateBlock isNil ifTrue: [ self halt: 'State ', aState, ' not found in automata'. ^nil].
		aPrevState := aState.

		self trace ifTrue:  [ Transcript show: aState; cr.].
		
		aTransitionEx handle: [:anEx | ] do: [ aStateBlock value].
		(aState = #processCategory and: [ aPrevState = #processCategory]) 
			ifFalse: [ aNumProcessCategoryConsecutiveStates := 0]
			ifTrue: [ 
				aNumProcessCategoryConsecutiveStates := aNumProcessCategoryConsecutiveStates + 1.
				aNumProcessCategoryConsecutiveStates > 10 ifTrue: [ self halt: 'Looping on categories']
			]

	].!

importDiseasesFromTableFile: theFileName

	"KRIcd9CMCodesImporter importDiseasesFromTableFile: KRIcd9CMCodesImporter diseasesTablePathName"

	| aDomainCMGO aHomeArchivo anArchivoGeneralRoot aCodificacionEnfermedades aFilename aStream |

	(theFileName isNil or: [ theFileName isEmpty]) ifTrue: [ ^nil].

	aDomainCMGO := KRSimpleInfoHolder  currentInfoStoreMethodSelector: #kronoSimpleInfoStore.
	aDomainCMGO isNil ifTrue: [ ^self].

	aHomeArchivo := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Archivo'].
	aHomeArchivo isNil ifTrue: [ ^self].

	anArchivoGeneralRoot := aHomeArchivo metaInfo getObject: aHomeArchivo featureNamedValue: 'homeRootsCMGO' 
		detect: 'nombre' test: [:anArchivoName | anArchivoName = 'General'] orCreate: 'General'.
	anArchivoGeneralRoot  isNil ifTrue: [ ^self].

	aCodificacionEnfermedades := anArchivoGeneralRoot metaInfo getObject: anArchivoGeneralRoot 
		featureNamedValue: 'codificacionEnfermedades'.
	aCodificacionEnfermedades isNil ifTrue: [ ^self].

	aFilename := theFileName asFilename.
	aFilename exists ifFalse: [ ^self].
	aFilename isDirectory ifTrue: [ ^self].

	aStream := nil.
	[
		aStream := aFilename readStream.
		^self importDiseasesFromStream: aStream intoCodificacion: aCodificacionEnfermedades
	]
		valueNowOrOnUnwindDo: 
	[
		aStream isNil ifFalse: [ aStream close]
	].
	aCodificacionEnfermedades browsePath.
	^aCodificacionEnfermedades!

importProceduresFromStream: theStream  intoCodificacion: theCodificacion

	| aCurrentCategory aCurrentPR1 aCurrentPR2 aCurrentPR3 aState aLine aLineStream aCategoryCode aCategoryName aCategory aWord aNoteText aIncludeText aCommentText aExcludeText aPR1Code aPR1Name aPR1 aPR2Code aPR2Name aPR2 someStates aStateBlock aPR3Code aPR3Name aPR3 aTransitionEx aPRSubCode aPrevState aNumProcessCategoryConsecutiveStates aLastBuilt |

	
	aCurrentCategory := nil.
	aCurrentPR1 := nil.
	aCurrentPR2 := nil.
	aCurrentPR3 := nil.

	aTransitionEx := Signal genericSignal newSignalMayProceed: true.

	someStates := IdentityDictionary new: 37.

	aState := #start.

		someStates at: #start put: [ 
			aState := #scanForCategory
		].
		someStates at: #scanForCategory put: [ "self halt: aState."
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not and: [ aLine first isDigit]]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aState := #processCategory
		].
		someStates at: #processCategory put: [ "self halt: aState." 
			aCategoryCode := aLineStream upTo: $. .
			aLineStream next; next.
			aCategoryName := aLineStream upToEnd.
			aCategory := self buildCategoryCoded: aCategoryCode named: aCategoryName onCodificacionProcedimientos: theCodificacion.
			aCategory isNil ifFalse: [ 
				aCurrentCategory := aCategory.
				aState := #scanForPR1
			]
		].
		someStates at: #scanForPR1 put: [ "self halt: aState." 
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processPR1] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #processCategoryNote ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #processCategoryIncludes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #processCategoryExcludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			aState := #processCategoryComment
			]]]]].
		].
		someStates at: #processCategoryNote put: [ "self halt: aState." 
			aNoteText := aLineStream upToEnd.
			self setCategory: aCurrentCategory note: aNoteText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processPR1] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note:' ifTrue: [  aState := #processCategoryNote ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #processCategoryIncludes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #processCategoryExcludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].
		someStates at: #processCategoryIncludes put: [ "self halt: aState." 
			aIncludeText := aLineStream upToEnd.
			self setCategory: aCurrentCategory includes: aIncludeText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processPR1] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #processCategoryNote ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #processCategoryIncludes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #processCategoryExcludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			] 
			]]]].
		].
		someStates at: #processCategoryExcludes put: [ "self halt: aState." 
			aExcludeText := aLineStream upToEnd.
			self setCategory: aCurrentCategory excludes: aExcludeText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processPR1] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #processCategoryNote ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #processCategoryIncludes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #processCategoryExcludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].
		someStates at: #processCategoryComment put: [ "self halt: aState." 
			aCommentText := aLineStream upToEnd.
			self setCategory: aCurrentCategory comment: aCommentText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processPR1] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #processCategoryNote ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #processCategoryIncludes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #processCategoryExcludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].
		someStates at: #processFifthDigitSubclassifications put: [ "self halt: aState. "
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not and: [ aLine first isDigit and: [(aLine at: 2) = Character tab ]]]] whileTrue.
			aLineStream := ReadStream on: aLine.
			aState := #processPR1.
		].





		someStates at: #processPR1 put: [ "self halt: aState." 
			aPR1Code := self enCodeFromLineStream: aLineStream.
			aPR1Code size < 2 ifTrue: [ aLineStream := ReadStream on: aLineStream contents.  aState := #processCategory. aTransitionEx raise].
			aPRSubCode := (aLineStream peek = Character space or: [ aLineStream peek isLetter])
				ifTrue: [ nil] ifFalse: [ aLineStream upTo: Character tab].
			aPRSubCode isNil not ifTrue: [ 
				aPRSubCode size = 1 ifTrue: [  aLineStream := ReadStream on: aLineStream contents. aState := #processPR2. aTransitionEx raise].
				aPRSubCode size = 2 ifTrue: [  aLineStream := ReadStream on: aLineStream contents. aState := #processPR3. aTransitionEx raise].
			].
			aLineStream skipSeparators.
			aPR1Name := aLineStream upToEnd.
			aPR1 := self buildPR1Coded: aPR1Code named: aPR1Name onCategoryProcedimientos: aCurrentCategory.
			aPR1 isNil ifFalse: [ 
				aLastBuilt := aPR1.
				aCurrentPR1 := aPR1.
				aState := #scanForPR2
			]
		].
		someStates at: #scanForPR2 put: [ "self halt: aState." 
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processPR2] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesPR1Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesPR1Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesPR1Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			aState := #procesPR1Comment
			]]]]].
		].
		someStates at: #procesPR1Note put: [ "self halt: aState." 
			aNoteText := aLineStream upToEnd.
			self setPR: aCurrentPR1 note: aNoteText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processPR2] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesPR1Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesPR1Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesPR1Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].
		someStates at: #procesPR1Includes put: [ "self halt: aState." 
			aIncludeText := aLineStream upToEnd.
			self setPR: aCurrentPR1 includes: aIncludeText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processPR2] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesPR1Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesPR1Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesPR1Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].
		someStates at: #procesPR1Excludes put: [ "self halt: aState." 
			aExcludeText := aLineStream upToEnd.
			self setPR: aCurrentPR1 excludes: aExcludeText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processPR2] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesPR1Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesPR1Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesPR1Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].
		someStates at: #procesPR1Comment put: [ "self halt: aState." 
			aCommentText := aLineStream upToEnd.
			self setPR: aCurrentPR1 comment: aCommentText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processPR2] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesPR1Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesPR1Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesPR1Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].



		someStates at: #processPR2 put: [ "self halt: aState." 
			aPR2Code := self enCodeFromLineStream: aLineStream.
			aPR2Code size < 2 ifTrue: [ aLineStream := ReadStream on: aLineStream contents.  aState := #processCategory. aTransitionEx raise].
			aPRSubCode := (aLineStream peek = Character space or: [ aLineStream peek isLetter])
				ifTrue: [ nil] ifFalse: [ aLineStream upTo: Character tab].
			aPRSubCode isNil 
				ifTrue: [ aLineStream := ReadStream on: aLineStream contents. aState := #processPR1. aTransitionEx raise]
				ifFalse: [ 
					aPRSubCode size = 2 ifTrue: [  aLineStream := ReadStream on: aLineStream contents.  aState := #processPR3. aTransitionEx raise]
				].
			aLineStream skipSeparators.
			aPR2Name := aLineStream upToEnd.
			aPR2 := self buildPR2Coded: aPRSubCode named: aPR2Name onPR1: aCurrentPR1.
			aPR2 isNil ifFalse: [ 
				aLastBuilt := aPR2.
				aCurrentPR2 := aPR2.
				aState := #scanForPR3
			].
	
		].
		someStates at: #scanForPR3 put: [ "self halt: aState." 
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processPR3] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesPR2Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesPR2Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesPR2Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			aState := #procesPR2Comment
			]]]]].
		].
		someStates at: #procesPR2Note put: [ "self halt: aState." 
			aNoteText := aLineStream upToEnd.
			self setPR: aCurrentPR2 note: aNoteText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processPR3] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesPR2Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesPR2Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesPR2Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].
		someStates at: #procesPR2Includes put: [ "self halt: aState." 
			aIncludeText := aLineStream upToEnd.
			self setPR: aCurrentPR2 includes: aIncludeText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processPR3] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesPR2Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesPR2Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesPR2Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].
		someStates at: #procesPR2Excludes put: [ "self halt: aState." 
			aExcludeText := aLineStream upToEnd.
			self setPR: aCurrentPR2 excludes: aExcludeText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processPR3] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesPR2Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesPR2Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesPR2Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].
		someStates at: #procesPR2Comment put: [ "self halt: aState." 
			aCommentText := aLineStream upToEnd.
			self setPR: aCurrentPR2 comment: aCommentText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processPR3] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesPR2Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesPR2Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesPR2Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].
		someStates at: #processPR3 put: [ "self halt: aState. "
			aPR3Code := self enCodeFromLineStream: aLineStream.
			aPR3Code size < 2 ifTrue: [ aLineStream := ReadStream on: aLineStream contents.  aState := #processCategory. aTransitionEx raise].
			aPRSubCode := (aLineStream peek = Character space or: [ aLineStream peek isLetter])
				ifTrue: [ nil] ifFalse: [ aLineStream upTo: Character tab].
			aPRSubCode isNil 
				ifTrue: [ aLineStream := ReadStream on: aLineStream contents. aState := #processPR1. aTransitionEx raise]
				ifFalse: [ 
					aPRSubCode size = 1 ifTrue: [ aLineStream := ReadStream on: aLineStream contents. aState := #processPR2. aTransitionEx raise]
				].
			aLineStream skipSeparators.
			aPR3Name := aLineStream upToEnd.
			aPR3 := self buildPR3Coded: aPRSubCode named: aPR3Name onPR2: aCurrentPR2.
			aPR3 isNil ifFalse: [ 
				aLastBuilt := aPR3.
				aCurrentPR3 := aPR3.
				aState := #scanAfterPR3
			]
		].



		someStates at: #scanAfterPR3 put: [ "self halt: aState." 
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processPR3] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesPR3Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesPR3Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesPR3Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			aState := #procesPR3Comment
			]]]]].
		].
		someStates at: #procesPR3Note put: [ "self halt: aState." 
			aNoteText := aLineStream upToEnd.
			self setPR: aCurrentPR3 note: aNoteText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processPR3] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesPR3Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesPR3Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesPR3Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].
		someStates at: #procesPR3Includes put: [ "self halt: aState." 
			aIncludeText := aLineStream upToEnd.
			self setPR: aCurrentPR3 includes: aIncludeText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processPR3] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesPR3Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesPR3Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesPR3Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].
		someStates at: #procesPR3Excludes put: [ "self halt: aState." 
			aExcludeText := aLineStream upToEnd.
			self setPR: aCurrentPR3 excludes: aExcludeText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processPR3] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesPR3Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesPR3Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesPR3Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].
		someStates at: #procesPR3Comment put: [ "self halt: aState." 
			aCommentText := aLineStream upToEnd.
			self setPR: aCurrentPR3 comment: aCommentText.
			[theStream atEnd not and: [ 
				aLine := theStream nextLine.
				aLine isEmpty not]] whileFalse.
			aLineStream := ReadStream on: aLine.
			aLineStream peek isDigit ifTrue: [ aState := #processPR3] ifFalse: [ 
			aWord := aLineStream upTo: $:.
			aWord = 'Note' ifTrue: [  aState := #procesPR3Note ] ifFalse: [
			aWord = 'Includes' ifTrue: [  aState := #procesPR3Includes ] ifFalse: [ 
			aWord = 'Excludes' ifTrue: [  aState := #procesPR3Excludes ] ifFalse: [ 
			aWord = 'The_following' ifTrue: [ aState := #processFifthDigitSubclassifications ] ifFalse: [ 
			]]]]].
		].


	aNumProcessCategoryConsecutiveStates := 0.

	[ aState = #end] whileFalse: [ 
		aStateBlock := someStates at: aState ifAbsent: [nil].
		aStateBlock isNil ifTrue: [ self halt: 'State ', aState, ' not found in automata'. ^nil].
		aPrevState := aState.

		self trace ifTrue:  [ Transcript show: aState; cr.].
		
		aTransitionEx handle: [:anEx | ] do: [ aStateBlock value].
		(aState = #processCategory and: [ aPrevState = #processCategory]) 
			ifFalse: [ aNumProcessCategoryConsecutiveStates := 0]
			ifTrue: [ 
				aNumProcessCategoryConsecutiveStates := aNumProcessCategoryConsecutiveStates + 1.
				aNumProcessCategoryConsecutiveStates > 10 ifTrue: [ self halt: 'Looping on categories']
			]

	].!

importProceduresFromTableFile: theFileName

	"KRIcd9CMCodesImporter importProceduresFromTableFile: KRIcd9CMCodesImporter diseasesTablePathName"

	| aDomainCMGO aHomeArchivo anArchivoGeneralRoot aCodificacionProcedimientos aFilename aStream |

	(theFileName isNil or: [ theFileName isEmpty]) ifTrue: [ ^nil].

	aDomainCMGO := KRSimpleInfoHolder  currentInfoStoreMethodSelector: #kronoSimpleInfoStore.
	aDomainCMGO isNil ifTrue: [ ^self].

	aHomeArchivo := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Archivo'].
	aHomeArchivo isNil ifTrue: [ ^self].

	anArchivoGeneralRoot := aHomeArchivo metaInfo getObject: aHomeArchivo featureNamedValue: 'homeRootsCMGO' 
		detect: 'nombre' test: [:anArchivoName | anArchivoName = 'General'] orCreate: 'General'.
	anArchivoGeneralRoot  isNil ifTrue: [ ^self].

	aCodificacionProcedimientos := anArchivoGeneralRoot metaInfo getObject: anArchivoGeneralRoot 
		featureNamedValue: 'codificacionProcedimientos'.
	aCodificacionProcedimientos isNil ifTrue: [ ^self].

	aFilename := theFileName asFilename.
	aFilename exists ifFalse: [ ^self].
	aFilename isDirectory ifTrue: [ ^self].

	aStream := nil.
	[
		aStream := aFilename readStream.
		^self importProceduresFromStream: aStream intoCodificacion: aCodificacionProcedimientos
	]
		valueNowOrOnUnwindDo: 
	[
		aStream isNil ifFalse: [ aStream close]
	].
	aCodificacionProcedimientos browsePath.
	^aCodificacionProcedimientos!

setCategory: theCategory comment: theText

	| aText aNoteText aNewText |

	theCategory isNil ifTrue: [^nil].

	(theText isNil or: [ theText isEmpty]) ifTrue: [^nil].

	aText := theText trimSeparators.
	aText isEmpty ifTrue: [^nil].

	aNoteText := theCategory metaInfo getObject: theCategory featureNamedValue: 'comentarioCategoria'.
	aNewText :=  (aNoteText isNil or: [aNoteText isEmpty])
		ifTrue: [ theText] 
		ifFalse: [ aNoteText , (String with: Character cr), aText].
	
	theCategory metaInfo setObject: theCategory featureNamed: 'comentarioCategoria' value: aNewText asText.
	^aNewText!

setCategory: theCategory excludes: theText

	| aText aNoteText aNewText |

	theCategory isNil ifTrue: [^nil].

	(theText isNil or: [ theText isEmpty]) ifTrue: [^nil].

	aText := theText trimSeparators.
	aText isEmpty ifTrue: [^nil].

	aNoteText := theCategory metaInfo getObject: theCategory featureNamedValue: 'exclusionCategoria'.
	aNewText :=  (aNoteText isNil or: [aNoteText isEmpty])
		ifTrue: [ theText] 
		ifFalse: [ aNoteText , (String with: Character cr), aText].
	
	theCategory metaInfo setObject: theCategory featureNamed: 'exclusionCategoria' value: aNewText asText.
	^aNewText!

setCategory: theCategory includes: theText

	| aText aNoteText aNewText |

	theCategory isNil ifTrue: [^nil].

	(theText isNil or: [ theText isEmpty]) ifTrue: [^nil].

	aText := theText trimSeparators.
	aText isEmpty ifTrue: [^nil].

	aNoteText := theCategory metaInfo getObject: theCategory featureNamedValue: 'inclusionCategoria'.
	aNewText :=  (aNoteText isNil or: [aNoteText isEmpty])
		ifTrue: [ theText] 
		ifFalse: [ aNoteText , (String with: Character cr), aText].
	
	theCategory metaInfo setObject: theCategory featureNamed: 'inclusionCategoria' value: aNewText asText.
	^aNewText!

setCategory: theCategory note: theText

	| aText aNoteText aNewText |

	theCategory isNil ifTrue: [^nil].

	(theText isNil or: [ theText isEmpty]) ifTrue: [^nil].

	aText := theText trimSeparators.
	aText isEmpty ifTrue: [^nil].

	aNoteText := theCategory metaInfo getObject: theCategory featureNamedValue: 'notaCategoria'.
	aNewText :=  (aNoteText isNil or: [aNoteText isEmpty])
		ifTrue: [ theText] 
		ifFalse: [ aNoteText , (String with: Character cr), aText].
	
	theCategory metaInfo setObject: theCategory featureNamed: 'notaCategoria' value: aNewText asText.
	^aNewText!

setEN: theEN comment: theText

	| aText aNoteText aNewText |

	theEN isNil ifTrue: [^nil].

	(theText isNil or: [ theText isEmpty]) ifTrue: [^nil].

	aText := theText trimSeparators.
	aText isEmpty ifTrue: [^nil].

	aNoteText := theEN metaInfo getObject: theEN featureNamedValue: 'comentarioEnfermedad'.
	aNewText :=  (aNoteText isNil or: [aNoteText isEmpty])
		ifTrue: [ theText] 
		ifFalse: [ aNoteText , (String with: Character cr), aText].
	
	theEN metaInfo setObject: theEN featureNamed: 'comentarioEnfermedad' value: aNewText asText.
	^aNewText!

setEN: theEN excludes: theText

	| aText aNoteText aNewText |

	theEN isNil ifTrue: [^nil].

	(theText isNil or: [ theText isEmpty]) ifTrue: [^nil].

	aText := theText trimSeparators.
	aText isEmpty ifTrue: [^nil].

	aNoteText := theEN metaInfo getObject: theEN featureNamedValue: 'exclusionEnfermedades'.
	aNewText :=  (aNoteText isNil or: [aNoteText isEmpty])
		ifTrue: [ theText] 
		ifFalse: [ aNoteText , (String with: Character cr), aText].
	
	theEN metaInfo setObject: theEN featureNamed: 'exclusionEnfermedades' value: aNewText asText.
	^aNewText!

setEN: theEN includes: theText

	| aText aNoteText aNewText |

	theEN isNil ifTrue: [^nil].

	(theText isNil or: [ theText isEmpty]) ifTrue: [^nil].

	aText := theText trimSeparators.
	aText isEmpty ifTrue: [^nil].

	aNoteText := theEN metaInfo getObject: theEN featureNamedValue: 'inclusionEnfermedades'.
	aNewText :=  (aNoteText isNil or: [aNoteText isEmpty])
		ifTrue: [ theText] 
		ifFalse: [ aNoteText , (String with: Character cr), aText].
	
	theEN metaInfo setObject: theEN featureNamed: 'inclusionEnfermedades' value: aNewText asText.
	^aNewText!

setEN: theEN note: theText

	| aText aNoteText aNewText |

	theEN isNil ifTrue: [^nil].

	(theText isNil or: [ theText isEmpty]) ifTrue: [^nil].

	aText := theText trimSeparators.
	aText isEmpty ifTrue: [^nil].

	aNoteText := theEN metaInfo getObject: theEN featureNamedValue: 'notaEnfermedad'.
	aNewText :=  (aNoteText isNil or: [aNoteText isEmpty])
		ifTrue: [ theText] 
		ifFalse: [ aNoteText , (String with: Character cr), aText].
	
	theEN metaInfo setObject: theEN featureNamed: 'notaEnfermedad' value: aNewText asText.
	^aNewText!

setPR: thePR comment: theText

	| aText aNoteText aNewText |

	thePR isNil ifTrue: [^nil].

	(theText isNil or: [ theText isEmpty]) ifTrue: [^nil].

	aText := theText trimSeparators.
	aText isEmpty ifTrue: [^nil].

	aNoteText := thePR metaInfo getObject: thePR featureNamedValue: 'comentarioEnfermedad'.
	aNewText :=  (aNoteText isNil or: [aNoteText isEmpty])
		ifTrue: [ theText] 
		ifFalse: [ aNoteText , (String with: Character cr), aText].
	
	thePR metaInfo setObject: thePR featureNamed: 'comentarioEnfermedad' value: aNewText asText.
	^aNewText!

setPR: thePR excludes: theText

	| aText aNoteText aNewText |

	thePR isNil ifTrue: [^nil].

	(theText isNil or: [ theText isEmpty]) ifTrue: [^nil].

	aText := theText trimSeparators.
	aText isEmpty ifTrue: [^nil].

	aNoteText := thePR metaInfo getObject: thePR featureNamedValue: 'exclusionEnfermedades'.
	aNewText :=  (aNoteText isNil or: [aNoteText isEmpty])
		ifTrue: [ theText] 
		ifFalse: [ aNoteText , (String with: Character cr), aText].
	
	thePR metaInfo setObject: thePR featureNamed: 'exclusionEnfermedades' value: aNewText asText.
	^aNewText!

setPR: thePR includes: theText

	| aText aNoteText aNewText |

	thePR isNil ifTrue: [^nil].

	(theText isNil or: [ theText isEmpty]) ifTrue: [^nil].

	aText := theText trimSeparators.
	aText isEmpty ifTrue: [^nil].

	aNoteText := thePR metaInfo getObject: thePR featureNamedValue: 'inclusionEnfermedades'.
	aNewText :=  (aNoteText isNil or: [aNoteText isEmpty])
		ifTrue: [ theText] 
		ifFalse: [ aNoteText , (String with: Character cr), aText].
	
	thePR metaInfo setObject: thePR featureNamed: 'inclusionEnfermedades' value: aNewText asText.
	^aNewText!

setPR: thePR note: theText

	| aText aNoteText aNewText |

	thePR isNil ifTrue: [^nil].

	(theText isNil or: [ theText isEmpty]) ifTrue: [^nil].

	aText := theText trimSeparators.
	aText isEmpty ifTrue: [^nil].

	aNoteText := thePR metaInfo getObject: thePR featureNamedValue: 'notaEnfermedad'.
	aNewText :=  (aNoteText isNil or: [aNoteText isEmpty])
		ifTrue: [ theText] 
		ifFalse: [ aNoteText , (String with: Character cr), aText].
	
	thePR metaInfo setObject: thePR featureNamed: 'notaEnfermedad' value: aNewText asText.
	^aNewText! !

!KRIcd9CMCodesImporter class publicMethodsFor: 'trace'!

trace
	^false! !

!KRIcd9CMCodesInfoHolder class publicMethodsFor: 'all'!

resetCurrentInfos
	"self  resetCurrentInfos"
	
	 self softResetCurrentInfos.
	self getSoftResetHealthCodesParameterNameParameterValue ifFalse: [ 
		super resetCurrentInfos
	]!

resetCurrentInfosStoreMethodSelector: theStoreMethodSelector
	"self resetCurrentInfosStoreMethodSelector: self defaultCurrentInfoSelector "
	
	^self getSoftResetHealthCodesParameterNameParameterValue
		ifTrue: [ self softResetCurrentInfosStoreMethodSelector: theStoreMethodSelector]
		ifFalse: [ super resetCurrentInfosStoreMethodSelector: theStoreMethodSelector]!

softResetCurrentInfos
	"self  resetCurrentInfos"

	self currentInfos values do: [:aCodificacion |
		aCodificacion metaInfo isNil ifFalse: [ 
			aCodificacion metaInfo setObject: aCodificacion featureNamed: 'archivo' value: nil
		].
		aCodificacion  recursiveDetachFromMetaInfo
	]!

softResetCurrentInfosStoreMethodSelector: theStoreMethodSelector
	"self resetCurrentInfosStoreMethodSelector: self defaultCurrentInfoSelector "
	
	| aCodificacion |

	currentInfos isNil  ifTrue: [ ^self].

	aCodificacion := currentInfos at: theStoreMethodSelector ifAbsent: [ nil].
	aCodificacion isNil ifTrue: [ ^self].
	
	aCodificacion metaInfo isNil ifTrue: [ ^self].

	aCodificacion metaInfo setObject: aCodificacion featureNamed: 'archivo' value: nil.
	aCodificacion  recursiveDetachFromMetaInfo! !

!KRIcd9CMCodesInfoHolder class publicMethodsFor: 'configuration'!

developerConfiguration
	^KRUIDeveloperConfiguration current!

getSoftResetHealthCodesParameterNameParameterValue
	| aValue aConfig |

	aConfig := self developerConfiguration.
	aValue := aConfig isNil ifTrue: [ nil] ifFalse: [ aConfig getParameter: aConfig class softResetHealthCodesParameterName].
	^aValue == true! !

!KRIcd9CMCodesInfoHolder class publicMethodsFor: 'current'!

currentMessages
	"self  currentInfo browsePath"
	"self  resetCurrentInfos"
	"currentInfos"
	"(self  currentInfoStoreMethodSelector: self defaultCurrentInfoSelector) browsePath"
	"self  resetCurrentInfoStoreMethodSelector: self defaultCurrentInfoSelector"!

defaultCurrentInfoSelector
	"self  defaultCurrentInfoSelector "

	^#codificacionEnfermedadesStore! !

!KRIcd9CMCodesInfoHolder class publicMethodsFor: 'file names'!

codificacionesEnfermedadesFileName

	^'Codificaciones\Enfermedades.boss' copy!

codificacionesEnfermedadesPathName

	^Filename defaultDirectory constructString: self codificacionesEnfermedadesFileName!

codificacionesProcedimientosFileName

	^'Codificaciones\Procedimientos.boss' copy!

codificacionesProcedimientosPathName

	^Filename defaultDirectory constructString: self codificacionesProcedimientosFileName! !

!KRIcd9CMCodesInfoHolder class publicMethodsFor: 'info creation'!

codificacionEnfermedadesStore

	| aCodificacionEnfermedades |

	aCodificacionEnfermedades := CMGenericObject  bossInFromFileNamed: self codificacionesEnfermedadesPathName.
	^aCodificacionEnfermedades!

codificacionProcedimientosStore

	| aCodificacionProcedimientos |

	aCodificacionProcedimientos := CMGenericObject  bossInFromFileNamed: self codificacionesProcedimientosPathName.
	^aCodificacionProcedimientos! !

KRIcd9CMCodesInfoHolder initializeAfterLoad!
KRIcd9CMCodesImporter initializeAfterLoad!
KRONO_HealthCodes initializeAfterLoad!

KRONO_HealthCodes loaded!
