'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:38:03 pm'!



((KRONO_Reporter createSubApplication: #KRONO_Reporter_Core in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

KRONO_Reporter_Core becomeDefault!

Object subclass: #KRAccumulator
	instanceVariableNames: 'accumulateToVector vector '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

KRAccumulator subclass: #KRArithAccumGatheringState
	instanceVariableNames: 'count sum sumOfSquares '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

KRAccumulator subclass: #KRArithmeticAccumulator
	instanceVariableNames: 'arithAccumState '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

KRAccumulator subclass: #KRFirstLastAccumulator
	instanceVariableNames: 'flAccumState '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

KRAccumulator subclass: #KRFLAccumGatheringState
	instanceVariableNames: 'firstObject lastObject '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

KRAccumulator subclass: #KRFLAccumInitialState
	instanceVariableNames: 'firstObject lastObject '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

Object subclass: #KRArithAccumInitialState
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

Object subclass: #KRArithAccumState
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

Object subclass: #KRBreakDefinition
	instanceVariableNames: 'headerSection footerSection selectors level newPage '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

Object subclass: #KRCellSpec
	instanceVariableNames: 'layout getBlock putBlock '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

KRCellSpec subclass: #KRTextCellSpec
	instanceVariableNames: 'alignment type formatString style extraEmphasis printConverter '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

Object subclass: #KRFLAccumState
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

Object subclass: #KRLayout
	instanceVariableNames: 'left top right bottom '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

Object subclass: #KRPosition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

KRPosition subclass: #KRAbsolutePosition
	instanceVariableNames: 'value '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

KRPosition subclass: #KRCompositeRelativePosition
	instanceVariableNames: 'relatedCellIndices relatedCellAccessor offset binaryCompositor '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

KRPosition subclass: #KRComputedPosition
	instanceVariableNames: 'receiver message arguments '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

KRPosition subclass: #KRDynamicPosition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

KRPosition subclass: #KRRelativePosition
	instanceVariableNames: 'relatedCellIndex relatedCellAccessor offset '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

Object subclass: #KRPrintJob
	instanceVariableNames: 'document preferenceManager numberOfCopies file pagerDictionary interactive currentPage startPage endPage '
	classVariableNames: 'DefaultPrinter Printers '
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

Object subclass: #KRPrintPager
	instanceVariableNames: 'pageSize margins scalingFactor landscape pageDimensionX pageDimensionY '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

Object subclass: #KRPrintPagerPreferenceManager
	instanceVariableNames: 'physicalPageSize logicalPageSize printerCapabilities documentPreferenceMap '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

Object subclass: #KRPrintPagerPreferenceMap
	instanceVariableNames: 'defaultPreferences printPagerPreferenceDictionary '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

Object subclass: #KRPrintPagerPreferences
	instanceVariableNames: 'landscape fitToPage scalingFactor fillLayout '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

Object subclass: #KRReportAccumulator
	instanceVariableNames: 'reportObject '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

KRReportAccumulator subclass: #KRArraySummingAccumulator
	instanceVariableNames: 'dataPacket runningTotals '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

Object subclass: #KRReportBreak
	instanceVariableNames: 'selectorValueMap newPage lastBreakIndex breakFlag '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

Object subclass: #KRReportDefinition
	instanceVariableNames: 'width height leftMargin topMargin rightMargin bottomMargin headerSection bodySection footerSection breakDefinitions titleSection summarySection printPagerPreferenceMap '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

Object subclass: #KRReportWriter
	instanceVariableNames: 'dataStream packetSize sectionOffset pageCount sections breakPoints intervalStack currentAggregateSections report currentPacketHolder '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

KRReportWriter subclass: #KRClassComplexityReportWriter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

Object subclass: #KRRWApplicationNode
	instanceVariableNames: 'application '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

Object subclass: #KRRWReportNodeCreator
	classInstanceVariableNames: 'defaultAspectAccessor '
	instanceVariableNames: 'reportComponentSpec cellSpecs cellUISpecs '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

KRRWReportNodeCreator subclass: #KRRWAggregateNodeCreator
	instanceVariableNames: 'accumulatorPrototype '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

KRRWReportNodeCreator subclass: #KRRWBodyNodeCreator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

Object subclass: #KRSectionDefinition
	instanceVariableNames: 'applicationClass specSelector showHeader showFooter separatePage '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

Object subclass: #KRUnixPrinter
	instanceVariableNames: 'name queueString '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

ApplicationModel subclass: #KRReport
	instanceVariableNames: 'topComponent reportObject pageSize margins pages printPagerPreferenceMap '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

ApplicationModel subclass: #KRReportSection
	instanceVariableNames: 'dataObject firstIndex lastIndex pageNumber today reportObject sectionSelector '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

KRReportSection subclass: #KRAccumulatingReportSection
	instanceVariableNames: 'attributes count '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

KRReportSection subclass: #KRRWReportNode
	instanceVariableNames: 'reportComponentSpec cellSpecs aspectAccessors cellValues height width '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

KRRWReportNode subclass: #KRRWAggregateNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

KRRWReportNode subclass: #KRRWBodyNode
	instanceVariableNames: 'dataPacket '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

KRReportSection subclass: #KRSimpleReportSection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

SimpleDialog subclass: #KRPrintJobOptions
	instanceVariableNames: 'numberOfCopies toFile fileName allPages start end logicalPageSize physicalPageSize scalingFactor pagesWide pagesTall fitPage landscape '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

SubApplication subclass: #KRONO_Reporter_Core
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

CompositePart subclass: #KRReportComposite
	instanceVariableNames: 'componentPool firstIndex lastIndex scrollOffsetHolder componentsWithValidBounds model '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Core becomeDefault!

!KRAbsolutePosition class publicMethodsFor: 'instance creation'!

value: t1 
	| t2 |
	(t2 := self new) value: t1.
	^t2! !

!KRAbsolutePosition publicMethodsFor: 'accessing'!

value
	^value!

value: t1 
	value := t1.
	^self! !

!KRAbsolutePosition publicMethodsFor: 'realizing'!

realizeFrom: t1 dynamicHeight: t2 
	^self value! !

!KRAccumulatingReportSection publicMethodsFor: 'accessing'!

attributesToAccumulate
	^#()!

count
	^count!

vectorAttributes
	^#()! !

!KRAccumulatingReportSection publicMethodsFor: 'adding'!

addDataPacket: t1 
	self dataObject: t1.
	count := count + 1.
	self attributesToAccumulate
		do: 
			[:t2 | 
			| t3 |
			t3 := t1 perform: t2.
			(self attributeAt: t2 ifAbsentInitializeFor: t3)
				add: t3].
	^self! !

!KRAccumulatingReportSection publicMethodsFor: 'aspects'!

aspectFor: t1 
	t1 == #count ifTrue: [^(AspectAdaptor accessWith: t1 assignWith: nil)
			subject: self].
	self dataObject perform: t1.
	^(PluggableAdaptor on: self)
		getBlock: [:t2 | t2 attributeAt: t1]
		putBlock: [:t3 :t4 | false]
		updateBlock: [:t5 :t6 :t7 | false]! !

!KRAccumulatingReportSection publicMethodsFor: 'initializing'!

initialize
	super initialize.
	count := 0.
	^self! !

!KRAccumulatingReportSection publicMethodsFor: 'private'!

attributeAt: t1 
	^self attributes at: t1!

attributeAt: t1 ifAbsentInitializeFor: t2 
	^self attributes at: t1
		ifAbsentPut: 
			[| t3 |
			t3 := KRAccumulator accumulatorForValue: t2.
			(self vectorAttributes includes: t1)
				ifTrue: [t3 accumulateToVector: true].
			t3]!

attributes
	attributes == nil ifTrue: [attributes := IdentityDictionary new].
	^attributes! !

!KRAccumulator class publicMethodsFor: 'instance creation'!

accumulatorForArithmetic
	^KRArithmeticAccumulator new!

accumulatorForObject
	^KRFirstLastAccumulator new!

accumulatorForValue: t1 
	^t1 accumulatorFor: self!

new
	^super new initialize! !

!KRAccumulator publicMethodsFor: 'accessing'!

accumulateToVector: t1 
	accumulateToVector := t1.
	t1 ifTrue: [vector := OrderedCollection new: 30].
	^self!

vector
	^vector! !

!KRAccumulator publicMethodsFor: 'adding'!

add: t1 
	accumulateToVector ifTrue: [vector addLast: t1].
	^self! !

!KRAccumulator publicMethodsFor: 'initializing'!

initialize
	accumulateToVector := false.
	^self! !

!KRArithAccumGatheringState class publicMethodsFor: 'instance creation'!

withInitialValue: t1 
	^self new initialValue: t1! !

!KRArithAccumGatheringState publicMethodsFor: 'accessing'!

count
	^count!

mean
	^self sum asDouble / self count!

standardDeviation
	^self variance sqrt!

sum
	^sum!

sumOfSquares
	^sumOfSquares!

variance
	^self sumOfSquares - (self mean squared * self count) / self size! !

!KRArithAccumGatheringState publicMethodsFor: 'adding'!

add: t1 
	count := count + 1.
	sum := sum + t1.
	sumOfSquares := sumOfSquares + t1 squared.
	^self! !

!KRArithAccumGatheringState publicMethodsFor: 'initializing'!

initialValue: t1 
	count := 1.
	sum := t1.
	sumOfSquares := t1 squared.
	^self! !

!KRArithAccumInitialState publicMethodsFor: 'accessing'!

count
	^0!

mean
	^nil!

standardDeviation
	^nil!

sum
	^nil!

sumOfSquares
	^nil!

variance
	^nil! !

!KRArithAccumInitialState publicMethodsFor: 'adding'!

add: t1 
	^KRArithAccumGatheringState withInitialValue: t1! !

!KRArithAccumState publicMethodsFor: 'accessing'!

count
	^self subclassResponsibility!

mean
	^self subclassResponsibility!

standardDeviation
	^self subclassResponsibility!

sum
	^self subclassResponsibility!

sumOfSquares
	^self subclassResponsibility!

variance
	^self subclassResponsibility! !

!KRArithAccumState publicMethodsFor: 'adding'!

add: t1 
	^self subclassResponsibility! !

!KRArithmeticAccumulator publicMethodsFor: 'accessing'!

count
	^self arithAccumState count!

mean
	^self arithAccumState mean!

standardDeviation
	^self arithAccumState standardDeviation!

sum
	^self arithAccumState sum!

sumOfSquares
	^self arithAccumState sumOfSquares!

variance
	^self arithAccumState variance! !

!KRArithmeticAccumulator publicMethodsFor: 'adding'!

add: t1 
	super add: t1.
	self arithAccumState: (self arithAccumState add: t1).
	^self! !

!KRArithmeticAccumulator publicMethodsFor: 'initialize'!

initialize
	super initialize.
	arithAccumState := KRArithAccumInitialState new.
	^self! !

!KRArithmeticAccumulator publicMethodsFor: 'private'!

arithAccumState
	^arithAccumState!

arithAccumState: t1 
	arithAccumState := t1.
	^self! !

!KRArraySummingAccumulator publicMethodsFor: 'accessing'!

computeDataPacket
	| t1 t2 t3 t4 |
	t1 := Array new: self runningTotals size.
	t2 := 1.
	t3 := self runningTotals size.
	[t2 <= t3]
		whileTrue: 
			[((t4 := self runningTotals at: t2) == nil or: [t4 respondsToArithmetic])
				ifTrue: [t1 at: t2 put: t4]
				ifFalse: [t1 at: t2 put: 'Total ' , t4].
			t2 := t2 + 1].
	^t1!

dataPacket
	dataPacket == nil ifTrue: [dataPacket := self computeDataPacket].
	^dataPacket!

runningTotals
	^runningTotals! !

!KRArraySummingAccumulator publicMethodsFor: 'adding'!

addDataPacket: t1 
	| t2 t3 t4 t5 |
	self runningTotals == nil ifTrue: [runningTotals := Array new: t1 size withAll: 0].
	t2 := self runningTotals.
	t3 := 1.
	t4 := t1 size.
	[t3 <= t4]
		whileTrue: 
			[(t5 := t1 at: t3) respondsToArithmetic
				ifTrue: [t2 at: t3 put: (t2 at: t3)
							+ t5]
				ifFalse: [(t2 at: t3) respondsToArithmetic
						ifTrue: [t2 at: t3 put: t5]
						ifFalse: [(t2 at: t3)
								= t5 ifFalse: [t2 at: t3 put: nil]]].
			t3 := t3 + 1].
	^self! !

!KRBreakDefinition class publicMethodsFor: 'accessing'!

reportSectionClass
	^KRSectionDefinition! !

!KRBreakDefinition class publicMethodsFor: 'instance creation'!

decodeFromLiteralArray: t1 
	^self new fromLiteralArrayEncoding: t1!

new
	^super new initialize! !

!KRBreakDefinition publicMethodsFor: 'accessing'!

footerClass
	^self footerSection applicationClass!

footerClass: t1 
	self footerSection applicationClass: t1.
	^self!

footerClassObject
	| t1 |
	(t1 := self footerSection applicationClass) isSymbol ifTrue: [^Smalltalk at: t1].
	^t1!

footerSelector
	^self footerSection specSelector!

footerSelector: t1 
	self footerSection specSelector: t1.
	^self!

headerClass
	^self headerSection applicationClass!

headerClass: t1 
	self headerSection applicationClass: t1.
	^self!

headerClassObject
	| t1 |
	(t1 := self headerSection applicationClass) isSymbol ifTrue: [^Smalltalk at: t1].
	^t1!

headerSelector
	^self headerSection specSelector!

headerSelector: t1 
	self headerSection specSelector: t1.
	^self!

level
	^level!

level: t1 
	level := t1.
	^self!

newPage
	^newPage!

newPage: t1 
	newPage := t1.
	^self!

selectorCollection
	| t1 t2 t3 t4 t5 t6 t7 t8 t9 |
	t1 := Scanner new.
	t7 := self selectorDelimiter.
	t2 := OrderedCollection new.
	self selectors notNil
		ifTrue: 
			[t4 := (t3 := self selectors , (String with: t7)) size.
			t5 := 1.
			(t6 := self selectorsString indexOf: t7) = 0
				ifTrue: 
					[(t8 := t1 scanTokens: self selectors) size = 1 ifTrue: [t8 := t8 first].
					t2 addLast: t8]
				ifFalse: [[t6 isNil]
						whileFalse: 
							[(t9 := t1 scanTokens: (t3 copyFrom: t5 to: t6 - 1)) size = 1 ifTrue: [t9 := t9 first].
							t2 addLast: t9.
							t5 := t6 + 1.
							t6 := t3
										nextIndexOf: t7
										from: t5
										to: t4]]].
	^t2!

selectorCollection: t1 
	| t2 |
	t2 := WriteStream on: String new.
	t1 do: [:t3 | t3 aspectStringOn: t2]
		separatedBy: [t2 nextPut: self selectorDelimiter].
	self selectors: t2 contents.
	^self!

selectors
	^selectors!

selectors: t1 
	selectors := t1.
	^self! !

!KRBreakDefinition publicMethodsFor: 'converting'!

literalArrayEncoding
	| t1 |
	(t1 := OrderedCollection new: 15) add: self class name.
	#(#headerClass #headerSelector #footerClass #footerSelector #level #newPage #selectorCollection) do: 
		[:t2 | 
		| t3 |
		(t3 := self perform: t2) == nil
			ifTrue: [nil]
			ifFalse: 
				[t1 add: (t2 , ':') asSymbol.
				t1 add: t3 literalArrayEncoding]].
	^t1 asArray! !

!KRBreakDefinition publicMethodsFor: 'initialize-release'!

fromLiteralArrayEncoding: t1 
	| t2 t3 |
	t2 := 2.
	t3 := t1 size.
	[t2 <= t3]
		whileTrue: 
			[self perform: (t1 at: t2)
				with: (t1 at: t2 + 1).
			t2 := t2 + 2].
	^self!

initialize
	self headerSection: self class reportSectionClass new; footerSection: self class reportSectionClass new; newPage: false; selectors: ''; level: 1.
	^self! !

!KRBreakDefinition publicMethodsFor: 'printing'!

forge: t1 toLength: t2 
	| t3 t4 |
	(t3 := t1) size > t2
		ifTrue: [t3 := t3 contractTo: t2]
		ifFalse: 
			[t4 := t3 size + 1.
			[t4 <= t2]
				whileTrue: 
					[t3 := t3 , ' '.
					t4 := t4 + 1]].
	^t3!

levelString
	self level isNil ifTrue: [^''].
	^self level printString!

printString
	| t1 |
	t1 := ''.
	^t1 := (t1 := self forge: self selectorsString toLength: 20) , '     ' , self levelString!

selectorsString
	self selectors isNil ifTrue: [^''].
	^self selectors! !

!KRBreakDefinition publicMethodsFor: 'private'!

footerSection
	^footerSection!

footerSection: t1 
	footerSection := t1.
	^self!

headerSection
	^headerSection!

headerSection: t1 
	headerSection := t1.
	^self!

selectorDelimiter
	^$\! !

!KRBreakDefinition publicMethodsFor: 'testing'!

canConstructFooterCanvas
	^self footerSection canConstructCanvas!

canConstructHeaderCanvas
	^self headerSection canConstructCanvas!

isFooterSpecified
	self footerSection applicationClass notNil ifTrue: [^self footerSection specSelector notNil].
	^false!

isHeaderSpecified
	self headerSection applicationClass notNil ifTrue: [^self headerSection specSelector notNil].
	^false!

isSelectorSpecified
	^self selectorsString size > 0! !

!KRCellSpec publicMethodsFor: 'accessing'!

access: t1 
	^getBlock value: t1!

getBlock
	^getBlock!

getBlock: t1 
	getBlock := t1.
	^self!

mutate: t1 with: t2 
	^putBlock value: t1 value: t2!

putBlock
	^putBlock!

putBlock: t1 
	putBlock := t1.
	^self! !

!KRCellSpec publicMethodsFor: 'initialize-release'!

initialize
	getBlock := [:t1 | t1].
	putBlock := [:t2 :t3 | t3].
	^self! !

!KRCellSpec publicMethodsFor: 'layout'!

bottom: t1 
	self layout bottom: t1.
	^self!

height: t1 
	self layout height: t1.
	^self!

left: t1 
	self layout left: t1.
	^self!

right: t1 
	self layout right: t1.
	^self!

top: t1 
	self layout top: t1.
	^self!

width: t1 
	self layout width: t1.
	^self! !

!KRCellSpec publicMethodsFor: 'private-accessing'!

layout
	layout == nil ifTrue: [layout := KRLayout new].
	^layout!

layout: t1 
	layout := t1.
	^self! !

!KRClassComplexityReportWriter class publicMethodsFor: 'report specs'!

applicationReportSpec
	"UIPainter new openOnClass: self andSelector: #applicationReportSpec"

	^#(#KRReportDefinition #bodyClass: #KRRWApplicationComplexityBody #bodySelector: #reportComponentSpec #headerClass: #KRRWApplicationComplexityHeader #headerSelector: #reportComponentSpec #footerSelector: #reportSpec #titleSelector: #reportSpec #summarySelector: #reportSpec #height: 10.5 #width: 8 #leftMargin: 0.0 #topMargin: 0.0 #rightMargin: 0.0 #bottomMargin: 0.0 #separateSummaryPage: false #separateTitlePage: false #showSummaryHeader: true #showSummaryFooter: true #showTitleHeader: true #showTitleFooter: true #breakDefinitions: #())!

classAverageReport
	<resource: #jpmReport>
	^#(#KRReportDefinition #bodyClass: #KRRWClassComplexitySections #bodySelector: #bodySpec #headerSelector: #reportSpec #footerSelector: #reportSpec #titleSelector: #reportSpec #summarySelector: #reportSpec #height: 10.5 #width: 8 #leftMargin: 0.0 #topMargin: 0.0 #rightMargin: 0.0 #bottomMargin: 0.0 #separateSummaryPage: false #separateTitlePage: false #showSummaryHeader: true #showSummaryFooter: true #showTitleHeader: true #showTitleFooter: true #breakDefinitions: #(#(#KRBreakDefinition #headerClass: #KRRWClassComplexityAccumulatingSections #headerSelector: #meanSpec #footerSelector: #reportSpec #level: 1 #newPage: false #selectorCollection: #(#applicationName))))!

classReportSpec
	<resource: #jpmReport>
	^#(#KRReportDefinition #bodyClass: #KRRWClassComplexitySections #bodySelector: #bodySpec #headerSelector: #reportSpec #footerSelector: #reportSpec #titleSelector: #reportSpec #summarySelector: #reportSpec #height: 10.5 #width: 8 #leftMargin: 0.0 #topMargin: 0.0 #rightMargin: 0.0 #bottomMargin: 0.0 #separateSummaryPage: false #separateTitlePage: false #showSummaryHeader: true #showSummaryFooter: true #showTitleHeader: true #showTitleFooter: true #breakDefinitions: #(#(#KRBreakDefinition #headerClass: #KRRWClassComplexityAccumulatingSections #headerSelector: #summarySpec #footerSelector: #reportSpec #level: 1 #newPage: false #selectorCollection: #(#applicationName))))!

simpleClassReportSpec
	<resource: #jpmReport>
	^#(#KRReportDefinition #bodyClass: #KRRWClassComplexitySections #bodySelector: #bodySpec #headerSelector: #reportSpec #footerSelector: #reportSpec #titleSelector: #reportSpec #summarySelector: #reportSpec #height: 10.5 #width: 8 #leftMargin: 0.0 #topMargin: 0.0 #rightMargin: 0.0 #bottomMargin: 0.0 #separateSummaryPage: false #separateTitlePage: false #showSummaryHeader: true #showSummaryFooter: true #showTitleHeader: true #showTitleFooter: true #breakDefinitions: #())! !

!KRCompositeRelativePosition class publicMethodsFor: 'instance creation'!

relativeToThe: t1 of: t2 composedBy: t3 offsetBy: t4 
	| t5 |
	(t5 := self new) relatedCellIndices: t2.
	t5 relatedCellAccessor: t1.
	t5 binaryCompositor: t3.
	t5 offset: t4.
	^t5! !

!KRCompositeRelativePosition publicMethodsFor: 'accessing'!

binaryCompositor
	^binaryCompositor!

binaryCompositor: t1 
	binaryCompositor := t1.
	^self!

offset
	offset == nil ifTrue: [offset := 0].
	^offset!

offset: t1 
	offset := t1.
	^self!

relatedCellAccessor
	relatedCellAccessor == nil ifTrue: [relatedCellAccessor := #bottom].
	^relatedCellAccessor!

relatedCellAccessor: t1 
	relatedCellAccessor := t1.
	^self!

relatedCellIndices
	relatedCellIndices == nil ifTrue: [relatedCellIndices := 1 to: 1].
	^relatedCellIndices!

relatedCellIndices: t1 
	relatedCellIndices := t1.
	^self! !

!KRCompositeRelativePosition publicMethodsFor: 'realizing'!

realizeFrom: t1 dynamicHeight: t2 
	| t3 t4 t5 |
	(self relatedCellIndices == nil or: [self relatedCellIndices isEmpty])
		ifTrue: [^0].
	t3 := (t1 at: self relatedCellIndices first) layout perform: self relatedCellAccessor.
	t4 := 2.
	t5 := self relatedCellIndices size.
	[t4 <= t5]
		whileTrue: 
			[t3 := self binaryCompositor value: t3 value: ((t1 at: (self relatedCellIndices at: t4)) layout perform: self relatedCellAccessor).
			t4 := t4 + 1].
	^t3 + self offset! !

!KRComputedPosition class publicMethodsFor: 'instance creation'!

receiver: t1 message: t2 arguments: t3 
	| t4 |
	(t4 := self new) receiver: t1.
	t4 message: t2.
	t4 arguments: t3.
	^t4! !

!KRComputedPosition publicMethodsFor: 'accessing'!

arguments
	^arguments!

arguments: t1 
	arguments := t1.
	^self!

message
	^message!

message: t1 
	message := t1.
	^self!

receiver
	^receiver!

receiver: t1 
	receiver := t1.
	^self! !

!KRComputedPosition publicMethodsFor: 'realizing'!

realizeFrom: t1 dynamicHeight: t2 
	^(self receiver realizeFrom: t1 dynamicHeight: t2)
		perform: message withArguments: (self arguments collect: [:t3 | t3 realizeFrom: t1 dynamicHeight: t2])! !

!KRComputedPosition publicMethodsFor: 'testing'!

isDynamicPosition
	self receiver isDynamicPosition ifTrue: [^true].
	self arguments
		do: 
			[:t1 | 
			t1 isDynamicPosition ifTrue: [^true].
			nil].
	^false! !

!KRDynamicPosition publicMethodsFor: 'realizing'!

realizeFrom: t1 dynamicHeight: t2 
	^t2!

realizeFrom: t1 variableRealization: t2 
	^t2! !

!KRDynamicPosition publicMethodsFor: 'testing'!

isDynamicPosition
	^true! !

!KRFirstLastAccumulator publicMethodsFor: 'accessing'!

any
	^self first!

first
	^self flAccumState first!

last
	^self flAccumState last!

mean
	^self flAccumState mean! !

!KRFirstLastAccumulator publicMethodsFor: 'adding'!

add: t1 
	super add: t1.
	self flAccumState: (self flAccumState add: t1).
	^self! !

!KRFirstLastAccumulator publicMethodsFor: 'initializing'!

initialize
	super initialize.
	flAccumState := KRFLAccumInitialState new.
	^self! !

!KRFirstLastAccumulator publicMethodsFor: 'private'!

flAccumState
	^flAccumState!

flAccumState: t1 
	flAccumState := t1.
	^self! !

!KRFLAccumGatheringState class publicMethodsFor: 'instance creation'!

withInitialValue: t1 
	^self new initialValue: t1! !

!KRFLAccumGatheringState publicMethodsFor: 'accessing'!

any
	^self first!

count

	^2!

first
	^firstObject!

last
	^lastObject!

mean

	^self sum asDouble / self count!

sum

	^firstObject +  lastObject! !

!KRFLAccumGatheringState publicMethodsFor: 'adding'!

add: t1 
	lastObject := t1.
	^self! !

!KRFLAccumGatheringState publicMethodsFor: 'initializing'!

initialValue: t1 
	firstObject := t1.
	lastObject := t1.
	^self! !

!KRFLAccumInitialState publicMethodsFor: 'accessing'!

any
	^nil!

first
	^nil!

last
	^nil! !

!KRFLAccumInitialState publicMethodsFor: 'adding'!

add: t1 
	^KRFLAccumGatheringState withInitialValue: t1! !

!KRFLAccumState publicMethodsFor: 'accessing'!

any
	^self subclassResponsibility!

first
	^self subclassResponsibility!

last
	^self subclassResponsibility! !

!KRFLAccumState publicMethodsFor: 'adding'!

add: t1 
	^self subclassResponsibility! !

!KRLayout publicMethodsFor: 'accessing'!

bottom
	bottom == nil ifTrue: [bottom := 0].
	^bottom!

bottom: t1 
	bottom := t1.
	^self!

height: t1 
	bottom == nil
		ifTrue: [self bottom: self top asKRPosition + t1]
		ifFalse: [self top: self bottom asKRPosition - t1].
	^self!

left
	left == nil ifTrue: [left := 0].
	^left!

left: t1 
	left := t1.
	^self!

right
	right == nil ifTrue: [right := 0].
	^right!

right: t1 
	right := t1.
	^self!

top
	top == nil ifTrue: [top := 0].
	^top!

top: t1 
	top := t1.
	^self!

width
	^self right - self left!

width: t1 
	self right: self left asKRPosition + t1.
	^self! !

!KRLayout publicMethodsFor: 'realizing'!

fullyRealized
	^self left @ self top corner: self right @ self bottom!

realizeXFrom: t1 
	self left: (self left realizeFrom: t1 dynamicHeight: 0).
	self right: (self right realizeFrom: t1 dynamicHeight: 0).
	^self!

realizeYFrom: t1 dynamicHeight: t2 
	self top: (self top realizeFrom: t1 dynamicHeight: t2).
	self bottom: (self bottom realizeFrom: t1 dynamicHeight: t2).
	^self! !

!KRPosition publicMethodsFor: 'computing'!

+ t1 
	^KRComputedPosition
		receiver: self
		message: #+
		arguments: (Array with: t1 asKRPosition)!

- t1 
	^KRComputedPosition
		receiver: self
		message: #-
		arguments: (Array with: t1 asKRPosition)!

max: t1 
	^KRComputedPosition
		receiver: self
		message: #max:
		arguments: (Array with: t1 asKRPosition)! !

!KRPosition publicMethodsFor: 'converting'!

asKRPosition
	^self! !

!KRPosition publicMethodsFor: 'realizing'!

realizeFrom: t1 dynamicHeight: t2 
	^self subclassResponsibility! !

!KRPosition publicMethodsFor: 'testing'!

isDynamicPosition
	^false! !

!KRPrintJob class publicMethodsFor: 'accessing'!

defaultPrinter
	^DefaultPrinter!

defaultPrinter: t1 
	t1 queueString == nil
		ifTrue: [DefaultPrinter := nil]
		ifFalse: [DefaultPrinter := t1].
	^self!

defaultPrinters
	^Array with: KRUnixPrinter new!

printers
	Printers == nil ifTrue: [^self defaultPrinters].
	^Printers!

printers: t1 
	Printers := t1.
	^self! !

!KRPrintJob class publicMethodsFor: 'instance creation'!

new
	^super new initialize! !

!KRPrintJob publicMethodsFor: 'accessing'!

currentPage
	currentPage == nil ifTrue: [currentPage := 1].
	^currentPage!

currentPrinter
	^self class defaultPrinter!

document
	^document!

document: t1 
	document := t1.
	^self!

endPage
	^endPage!

endPage: t1 
	endPage := t1.
	^self!

file
	^file!

file: t1 
	file := t1.
	^self!

incrementCurrentPage
	currentPage := self currentPage + 1.
	^self!

numberOfCopies
	^numberOfCopies!

numberOfCopies: t1 
	numberOfCopies := t1.
	^self!

physicalPageSize
	^self file pageSize!

preferenceManager
	preferenceManager == nil ifTrue: [preferenceManager := KRPrintPagerPreferenceManager new].
	^preferenceManager!

preferenceManager: t1 
	preferenceManager := t1.
	^self!

startPage
	^startPage!

startPage: t1 
	startPage := t1.
	^self! !

!KRPrintJob publicMethodsFor: 'dialogs'!

adjustPagerFor: t1 
	| t2 t3 t4 |
	t2 := self pagerForPageSize: t1.
	(t3 := KRPrintJobOptions new) logicalPageSize: t1.
	t3 physicalPageSize: self physicalPageSize - (0.5 @ 0.5) * Screen default resolution.
	t3 scalingFactor: (t2 scalingFactor * 100) rounded.
	t3 pagesWide: t2 pageDimensionX.
	t3 pagesTall: t2 pageDimensionY.
	t3 landscape: t2 landscape.
	(t4 := t3 open)
		ifTrue: 
			[t2 scalingFactor: t3 scalingFactor value / 100.
			t2 pageDimensionX: t3 pagesWide value ceiling.
			t2 pageDimensionY: t3 pagesTall value ceiling.
			t2 landscape: t3 landscape value.
			self numberOfCopies: t3 numberOfCopies value.
			self startPage: t3 start value.
			self endPage: t3 end value].
	^t4! !

!KRPrintJob publicMethodsFor: 'hardcopy'!

execute
	self file: (Printer startPrintJobNamed: 'temp.ps').
	file graphicsDevice withHardcopyPreferencesDo: [self document hardcopyForPrintJob: self].
	self file print.
	self file: nil.
	^self!

executeUsingDialog
	self file: (Printer startPrintJobNamed: 'temp.ps').
	
	[| t1 |
	(t1 := self adjustPagerFor: self document pageSize)
		ifTrue: 
			[Cursor wait showWhile: [file graphicsDevice withHardcopyPreferencesDo: [self document hardcopyForPrintJob: self]].
			self file print].
	^t1]
		valueOnUnwindDo: 
			[self file close.
			self file: nil].
	^self!

hardcopy: t1 at: t2 scaledBy: t3 
	| t4 t5 t6 |
	t5 := t1 container.
	t6 := t1 bounds.
	t4 := t1 copy.
	
	[| t7 |
	(t4 := BoundedWrapper on: t4 at: 0 @ 0) bounds: t6.
	t4 := TranslatingWrapper on: t4 at: t2 negated.
	(t4 := (ScalingWrapper on: t4)
				scale: t3) container: t5.
	(t7 := MockMedium
				on: t4
				in: t4 bounds
				for: self file) newGraphicsDevice: self file graphicsDevice.
	t4 bounds: t4 bounds.
	t7 component displayOn: t7 graphicsContext]
		valueNowOrOnUnwindDo: 
			[t1 container: t5.
			t1 bounds: t6].
	^self!

hardcopyComponents: t1 in: t2 translatedBy: t3 scaledBy: t4 
	| t5 |
	t5 := self pagerForPageSize: t2.
	self file setLandscape: t5 landscape.
	t5
		physicalPagesDo: 
			[:t6 :t7 | 
			self shouldPrintCurrentPage
				ifTrue: 
					[t1 do: [:t8 | self
							hardcopy: t8
							at: t3 + t6
							scaledBy: t4 * t7].
					self file newPage].
			self incrementCurrentPage].
	^self! !

!KRPrintJob publicMethodsFor: 'initializing'!

initialize
	numberOfCopies := 1.
	^self! !

!KRPrintJob publicMethodsFor: 'pager generation'!

pagerDictionary
	pagerDictionary == nil ifTrue: [pagerDictionary := Dictionary new].
	^pagerDictionary!

pagerForPageSize: t1 
	^self pagerDictionary at: t1
		ifAbsentPut: 
			[| t2 |
			(t2 := self preferenceManager) physicalPageSize: self physicalPageSize - (0.5 @ 0.5) * Screen default resolution.
			t2 logicalPageSize: t1.
			t2 documentPreferenceMap: self document printPagerPreferenceMap.
			t2 bestPrintPager]! !

!KRPrintJob publicMethodsFor: 'testing'!

shouldPrintCurrentPage
	(self startPage == nil or: [self currentPage >= self startPage])
		ifTrue: 
			[self endPage == nil ifTrue: [^true].
			^self currentPage <= self endPage].
	^false! !

!KRPrintJobOptions class publicMethodsFor: 'instance creation'!

new
	^super new initialize! !

!KRPrintJobOptions class publicMethodsFor: 'interface specs'!

windowSpec
	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Printer Setup' #bounds: #(#Rectangle 335 373 833 582)) #component: #(#SpecCollection #collection: #(#(#LabelSpec #layout: #(#Point 10 13) #label: 'Copies:') #(#LabelSpec #layout: #(#Point 110 13) #label: 'Pages:') #(#LabelSpec #layout: #(#Point 301 13) #name: #to: #label: 'To:') #(#GroupBoxSpec #layout: #(#Rectangle 172 46 404 109)) #(#LabelSpec #layout: #(#AlignmentOrigin 174 0 46 0 0 0.5) #isOpaque: true #label: 'Destination') #(#LabelSpec #layout: #(#Point 210 135) #label: '%') #(#GroupBoxSpec #layout: #(#Rectangle 13 46 159 109)) #(#LabelSpec #layout: #(#AlignmentOrigin 14 0 46 0 0 0.5) #isOpaque: true #label: 'Orientation') #(#LabelSpec #layout: #(#Point 131 155) #label: 'pages wide by') #(#LabelSpec #layout: #(#Point 278 155) #label: 'tall') #(#GroupBoxSpec #layout: #(#Rectangle 13 122 309 186)) #(#LabelSpec #layout: #(#AlignmentOrigin 14 0 122 0 0 0.5) #isOpaque: true #label: 'Scaling') #(#RadioButtonSpec #layout: #(#Point 200 14) #model: #allPages #callbacksSpec: #(#UIEventCallbackSubSpec #valueChangeSelector: #fromSelected) #tabable: false #label: 'From:' #select: false) #(#RadioButtonSpec #layout: #(#Point 185 79) #model: #toFile #callbacksSpec: #(#UIEventCallbackSubSpec #valueChangeSelector: #fileNameSelected) #tabable: false #label: 'File Named:' #select: true) #(#RadioButtonSpec #layout: #(#Point 26 135) #model: #fitPage #callbacksSpec: #(#UIEventCallbackSubSpec #valueChangeSelector: #scalingFactorSelected) #tabable: false #label: 'Reduce or Enlarge:' #select: false) #(#RadioButtonSpec #layout: #(#Point 26 155) #model: #fitPage #callbacksSpec: #(#UIEventCallbackSubSpec #valueChangeSelector: #fitToSelected) #tabable: false #label: 'Fit to:' #select: true) #(#ActionButtonSpec #layout: #(#LayoutFrame -80 1 9 0 -13 1 37 0) #name: #accept #model: #accept #label: 'Print' #isDefault: true #defaultable: true) #(#ActionButtonSpec #layout: #(#LayoutFrame -80 1 43 0 -13 1 71 0) #model: #cancel #label: 'Cancel' #defaultable: true) #(#InputFieldSpec #layout: #(#Rectangle 63 13 95 33) #model: #numberOfCopies #alignment: #right #type: #number) #(#RadioButtonSpec #layout: #(#Point 158 14) #model: #allPages #callbacksSpec: #(#UIEventCallbackSubSpec #valueChangeSelector: #allPagesSelected) #label: 'All' #select: true) #(#InputFieldSpec #layout: #(#Rectangle 262 13 299 33) #name: #start #model: #start #callbacksSpec: #(#UIEventCallbackSubSpec #focusInSelector: #startEntry #focusOutSelector: #startExit) #alignment: #right #type: #number) #(#InputFieldSpec #layout: #(#Rectangle 326 13 364 33) #model: #end #callbacksSpec: #(#UIEventCallbackSubSpec #focusInSelector: #endEntry #focusOutSelector: #endExit) #alignment: #right #type: #number) #(#RadioButtonSpec #layout: #(#Point 26 59) #model: #landscape #callbacksSpec: #(#UIEventCallbackSubSpec #valueChangeSelector: #orientationChanged) #label: 'Portrait' #select: false) #(#RadioButtonSpec #layout: #(#Point 26 79) #model: #landscape #callbacksSpec: #(#UIEventCallbackSubSpec #valueChangeSelector: #orientationChanged) #label: 'Landscape' #select: true) #(#RadioButtonSpec #layout: #(#Point 185 59) #model: #toFile #callbacksSpec: #(#UIEventCallbackSubSpec #valueChangeSelector: #printerSelected) #label: 'Printer' #select: false) #(#InputFieldSpec #layout: #(#Rectangle 280 76 392 96) #name: #fileName #model: #fileName #callbacksSpec: #(#UIEventCallbackSubSpec #focusInSelector: #fileNameEntry #focusOutSelector: #fileNameExit) #tabable: true) #(#InputFieldSpec #layout: #(#Rectangle 170 133 209 153) #name: #scalingFactor #model: #scalingFactor #callbacksSpec: #(#UIEventCallbackSubSpec #focusInSelector: #scalingFactorEntry #valueChangeSelector: #scalingFactorChange) #tabable: true #alignment: #right #type: #number) #(#InputFieldSpec #layout: #(#Rectangle 90 153 128 173) #name: #pagesWide #model: #pagesWide #callbacksSpec: #(#UIEventCallbackSubSpec #focusInSelector: #pagesWideEntry #valueChangeSelector: #pagesWideChange) #tabable: true #alignment: #right #type: #fixedpoint) #(#InputFieldSpec #layout: #(#Rectangle 231 153 269 173) #name: #pagesTall #model: #pagesTall #callbacksSpec: #(#UIEventCallbackSubSpec #focusInSelector: #pagesTallEntry #valueChangeSelector: #pagesTallChange) #tabable: true #alignment: #right #type: #fixedpoint) #(#ActionButtonSpec #layout: #(#LayoutFrame -80 1 83 0 -13 1 111 0) #name: #editOptions #flags: 24 #model: #editOptions #label: 'Options' #defaultable: true) #(#DividerSpec #layout: #(#LayoutFrame -86 1 76 0 -7 1 80 0) #name: #divider #flags: 16))))! !

!KRPrintJobOptions publicMethodsFor: 'accessing'!

landscape: t1 
	self landscape value: t1.
	^self!

logicalPageSize
	^logicalPageSize!

logicalPageSize: t1 
	logicalPageSize := t1.
	^self!

pagesTall: t1 
	self pagesTall value: t1.
	^self!

pagesWide: t1 
	self pagesWide value: t1.
	^self!

physicalPageSize
	self landscape value ifTrue: [^physicalPageSize transpose].
	^physicalPageSize!

physicalPageSize: t1 
	physicalPageSize := t1.
	^self!

scalingFactor: t1 
	self scalingFactor value: t1.
	^self! !

!KRPrintJobOptions publicMethodsFor: 'actions'!

editOptions
	| t1 |
	(t1 := Dialog
				choose: 'Choose a printer'
				fromList: (KRPrintJob printers collect: [:t2 | t2 name])
				values: KRPrintJob printers
				lines: 2
				cancel: [nil]
				for: self builder window) == nil ifFalse: [KRPrintJob defaultPrinter: t1].
	^self! !

!KRPrintJobOptions publicMethodsFor: 'aspects'!

allPages
	allPages isNil ifTrue: [^allPages := true asValue].
	^allPages!

end
	end isNil ifTrue: [^end := nil asValue].
	^end!

fileName
	fileName isNil ifTrue: [^fileName := nil asValue].
	^fileName!

fitPage
	fitPage isNil ifTrue: [^fitPage := false asValue].
	^fitPage!

landscape
	landscape isNil ifTrue: [^landscape := false asValue].
	^landscape!

numberOfCopies
	numberOfCopies isNil ifTrue: [^numberOfCopies := 1 asValue].
	^numberOfCopies!

pagesTall
	pagesTall isNil ifTrue: [^pagesTall := self initialPagesTall asValue].
	^pagesTall!

pagesWide
	pagesWide isNil ifTrue: [^pagesWide := self initialPagesWide asValue].
	^pagesWide!

scalingFactor
	scalingFactor isNil ifTrue: [^scalingFactor := 100 asValue].
	^scalingFactor!

start
	start isNil ifTrue: [^start := nil asValue].
	^start!

toFile
	toFile isNil ifTrue: [^toFile := false asValue].
	^toFile! !

!KRPrintJobOptions publicMethodsFor: 'computing'!

setPagesDimensions
	| t1 t2 |
	t1 := self asEfficientNumber: self logicalPageSize x / self physicalPageSize x * self scalingFactor value / 100.
	t2 := self asEfficientNumber: self logicalPageSize y / self physicalPageSize y * self scalingFactor value / 100.
	self pagesWide value: t1.
	self pagesTall value: t2.
	^self!

setScalingFactor
	| t1 t2 |
	t1 := self pagesWide value * self physicalPageSize x / self logicalPageSize x.
	t2 := self pagesTall value * self physicalPageSize y / self logicalPageSize y.
	self scalingFactor value: ((t1 min: t2)
			* 100) rounded.
	^self! !

!KRPrintJobOptions publicMethodsFor: 'initializing'!

initialize
	super initialize.
	physicalPageSize := 8 * 75 @ (10.5 * 75).
	logicalPageSize := 8 * 75 @ (10.5 * 75).
	^self!

initialPagesTall
	^1!

initialPagesWide
	^1!

initialScalingFactor
	^100! !

!KRPrintJobOptions publicMethodsFor: 'interface opening'!

postBuildWith: t1 
	self toFile onChangeSend: #destinationChanged to: self.
	ExternalInterface currentPlatform first == #unix
		ifTrue: 
			[(self builder componentAt: #divider) beVisible.
			(self builder componentAt: #editOptions) beVisible].
	^self! !

!KRPrintJobOptions publicMethodsFor: 'notification - all pages'!

allPagesSelected
	self start value: nil.
	self end value: nil.
	^self!

endEntry
	self allPages value: false.
	^self!

endExit
	self end value == nil ifTrue: [self allPages value: true].
	^self!

fromSelected
	(self builder componentAt: #start) takeKeyboardFocus.
	^self!

startEntry
	self allPages value: false.
	^self!

startExit
	self start value == nil ifTrue: [self allPages value: true].
	^self! !

!KRPrintJobOptions publicMethodsFor: 'notification - destination'!

destinationChanged
	(self builder componentAt: #accept)
		labelString: (self toFile value
				ifTrue: ['Save']
				ifFalse: ['Print']).
	^self!

fileNameEntry
	self toFile value: true.
	^self!

fileNameExit
	self fileName value == nil ifTrue: [self toFile value: false].
	^self!

fileNameSelected
	self fileName value == nil ifTrue: [self fileName value: 'temp.ps'].
	(self builder componentAt: #fileName) takeKeyboardFocus.
	^self!

printerSelected
	self fileName value: nil.
	^self! !

!KRPrintJobOptions publicMethodsFor: 'notification - orientation'!

orientationChanged
	| t1 |
	self fitPage value
		ifTrue: 
			[t1 := self pagesWide value.
			self pagesWide value: self pagesTall value.
			self pagesTall value: t1.
			self landscape value
				ifTrue: [self pagesTallChange]
				ifFalse: [self pagesWideChange]]
		ifFalse: [self scalingFactorChange].
	^self! !

!KRPrintJobOptions publicMethodsFor: 'notification - scaling'!

fitToSelected
	(self builder componentAt: #pagesWide) takeKeyboardFocus.
	^self!

pagesTallChange
	| t1 t2 t3 |
	t2 := self logicalPageSize y / self logicalPageSize x.
	t3 := self physicalPageSize y / self physicalPageSize x.
	t1 := self asEfficientNumber: self pagesTall value / (t2 / t3).
	self pagesWide value: t1.
	self setScalingFactor.
	^self!

pagesTallEntry
	self fitPage value: true.
	^self!

pagesWideChange
	| t1 t2 t3 |
	t2 := self logicalPageSize y / self logicalPageSize x.
	t3 := self physicalPageSize y / self physicalPageSize x.
	t1 := self asEfficientNumber: self pagesWide value * t2 / t3.
	self pagesTall value: t1.
	self setScalingFactor.
	^self!

pagesWideEntry
	self fitPage value: true.
	^self!

scalingFactorChange
	self setPagesDimensions.
	^self!

scalingFactorEntry
	self fitPage value: false.
	^self!

scalingFactorSelected
	(self builder componentAt: #scalingFactor) takeKeyboardFocus.
	^self! !

!KRPrintJobOptions publicMethodsFor: 'support'!

asEfficientNumber: t1 
	t1 asInteger = t1 ifTrue: [^t1 asInteger].
	^t1 asFloat! !

!KRPrintPager publicMethodsFor: 'accessing'!

landscape
	landscape == nil ifTrue: [landscape := false].
	^landscape!

landscape: t1 
	landscape := t1.
	^self!

margins
	margins == nil ifTrue: [margins := 18.75 @ 18.75 corner: 18.75 @ 18.75].
	^margins!

pageDimensionX
	pageDimensionX == nil ifTrue: [pageDimensionX := 1].
	^pageDimensionX!

pageDimensionX: t1 
	pageDimensionX := t1.
	^self!

pageDimensionY
	pageDimensionY == nil ifTrue: [pageDimensionY := 1].
	^pageDimensionY!

pageDimensionY: t1 
	pageDimensionY := t1.
	^self!

pageSize
	pageSize == nil ifTrue: [pageSize := 600 @ 787.5].
	^pageSize / self scalingFactor!

pageSize: t1 
	pageSize := t1.
	^self!

scalingFactor
	scalingFactor == nil ifTrue: [scalingFactor := 1.0].
	^scalingFactor!

scalingFactor: t1 
	scalingFactor := t1.
	^self! !

!KRPrintPager publicMethodsFor: 'paging'!

physicalPagesDo: t1 
	| t2 t3 t4 t5 t6 t7 t8 t9 |
	t2 := self pageSize x.
	t4 := self margins left / self scalingFactor.
	t3 := self pageSize y.
	t5 := self margins bottom / self scalingFactor.
	t6 := 1.
	t7 := self pageDimensionY.
	[t6 <= t7]
		whileTrue: 
			[t8 := 1.
			t9 := self pageDimensionX.
			[t8 <= t9]
				whileTrue: 
					[t1 value: t8 - 1 * t2 - t4 @ (t6 - 1 * t3 - t5) value: self scalingFactor.
					t8 := t8 + 1].
			t6 := t6 + 1].
	^self! !

!KRPrintPagerPreferenceManager publicMethodsFor: 'accessing'!

documentPreferenceMap
	^documentPreferenceMap!

documentPreferenceMap: t1 
	documentPreferenceMap := t1.
	^self!

logicalPageSize
	^logicalPageSize!

logicalPageSize: t1 
	logicalPageSize := t1.
	^self!

physicalPageSize
	^physicalPageSize!

physicalPageSize: t1 
	physicalPageSize := t1.
	^self!

printerCapabilties
	^printerCapabilities!

printerCapabilties: t1 
	printerCapabilities := t1.
	^self! !

!KRPrintPagerPreferenceManager publicMethodsFor: 'pager generation'!

bestPrintPager
	| t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 |
	t1 := KRPrintPager new.
	t3 := (t2 := self documentPreferenceMap printPagerPreferencesForPageSize: self physicalPageSize) landscape == nil
				ifTrue: [self logicalPageSize x > self logicalPageSize y]
				ifFalse: [t2 landscape].
	t1 landscape: t3.
	t3 ifTrue: [self physicalPageSize: self physicalPageSize transpose].
	t1 pageSize: self physicalPageSize.
	(t4 := t2 fitToPage) == true
		ifTrue: 
			[t1 scalingFactor: (self physicalPageSize x / self logicalPageSize x min: self physicalPageSize y / self logicalPageSize y).
			t1 pageDimensionX: 1.
			t1 pageDimensionY: 1.
			^t1].
	t5 := t2 scalingFactor == nil
				ifTrue: [1.0]
				ifFalse: [t2 scalingFactor].
	(t6 := t2 fillLayout) == true
		ifTrue: 
			[t8 := (t7 := self logicalPageSize * t5) x / self physicalPageSize x.
			t9 := t7 y / self physicalPageSize y.
			t1 scalingFactor: (t8 ceiling / t8 min: t9 ceiling / t9).
			t1 pageDimensionX: t8 ceiling.
			t1 pageDimensionY: t9 ceiling.
			^t1].
	(t10 := (t7 := self logicalPageSize * t5) x / self physicalPageSize x max: t7 y / self physicalPageSize y) < 1.2
		ifTrue: 
			[t10 := t10 * t5.
			t1 scalingFactor: (1.0 / t10 min: 1.0).
			t1 pageDimensionX: 1.
			t1 pageDimensionY: 1.
			^t1].
	t1 scalingFactor: t5.
	t1 pageDimensionX: (self logicalPageSize x * t5 / self physicalPageSize x) ceiling.
	t1 pageDimensionY: (self logicalPageSize y * t5 / self physicalPageSize y) ceiling.
	^t1! !

!KRPrintPagerPreferenceMap class publicMethodsFor: 'instance creation'!

new
	^super new initialize! !

!KRPrintPagerPreferenceMap publicMethodsFor: 'accessing'!

atSize: t1 putPrintPagerPreference: t2 
	printPagerPreferenceDictionary at: t1 put: t2.
	^self!

defaultPreferences
	defaultPreferences == nil ifTrue: [defaultPreferences := KRPrintPagerPreferences new].
	^defaultPreferences!

defaultPreferences: t1 
	defaultPreferences := t1.
	^self!

printPagerPreferencesForPageSize: t1 
	^printPagerPreferenceDictionary at: t1 ifAbsent: [self defaultPreferences]! !

!KRPrintPagerPreferenceMap publicMethodsFor: 'initializing'!

initialize
	printPagerPreferenceDictionary := Dictionary new.
	^self! !

!KRPrintPagerPreferences publicMethodsFor: 'accessing'!

fillLayout
	^fillLayout!

fillLayout: t1 
	fillLayout := t1.
	^self!

fitToPage
	^fitToPage!

fitToPage: t1 
	fitToPage := t1.
	^self!

landscape
	^landscape!

landscape: t1 
	landscape := t1.
	^self!

scalingFactor
	^scalingFactor!

scalingFactor: t1 
	^scalingFactor := t1! !

!KRRelativePosition class publicMethodsFor: 'instance creation'!

relativeToThe: t1 of: t2 offsetBy: t3 
	| t4 |
	(t4 := self new) relatedCellIndex: t2.
	t4 relatedCellAccessor: t1.
	t4 offset: t3.
	^t4! !

!KRRelativePosition publicMethodsFor: 'accessing'!

offset
	offset == nil ifTrue: [offset := 0].
	^offset!

offset: t1 
	offset := t1.
	^self!

relatedCellAccessor
	relatedCellAccessor == nil ifTrue: [relatedCellAccessor := #bottom].
	^relatedCellAccessor!

relatedCellAccessor: t1 
	relatedCellAccessor := t1.
	^self!

relatedCellIndex
	relatedCellIndex == nil ifTrue: [relatedCellIndex := 1].
	^relatedCellIndex!

relatedCellIndex: t1 
	relatedCellIndex := t1.
	^self! !

!KRRelativePosition publicMethodsFor: 'realizing'!

realizeFrom: t1 dynamicHeight: t2 
	^((t1 at: self relatedCellIndex) layout perform: self relatedCellAccessor)
		+ self offset! !

!KRReport class publicMethodsFor: 'accessing'!

printPagerPreferenceManagerClass
	^KRPrintPagerPreferenceManager! !

!KRReport class publicMethodsFor: 'class accessing'!

reportCompositeClass
	^KRReportComposite! !

!KRReport class publicMethodsFor: 'interface specs'!

windowSpec
	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Report Viewer' #min: #(#Point 50 50) #bounds: #(#Rectangle 292 261 932 681)) #component: #(#SpecCollection #collection: #(#(#ArbitraryComponentSpec #layout: #(#LayoutFrame 0 0 50 0 1 1 1 1) #name: #topComponent #flags: 11 #component: #topComponent) #(#ActionButtonSpec #layout: #(#Rectangle 13 9 80 37) #model: #printReportUsingDialog #label: 'Print...' #isDefault: true #defaultable: true) #(#ActionButtonSpec #layout: #(#Rectangle 93 9 160 37) #model: #printReport #label: 'Print' #isDefault: false #defaultable: true) #(#ActionButtonSpec #layout: #(#Rectangle 173 11 240 39) #model: #closeRequest #label: 'Close' #isDefault: false #defaultable: true))))! !

!KRReport class publicMethodsFor: 'resources'!

reportMenu
	^#(#PopUpMenu #('Print') #() #(#(#PopUpMenu #('Print Report' 'Print Report ...') #() #(#printReport #printReportUsingDialog)))) decodeAsLiteralArray! !

!KRReport publicMethodsFor: 'accessing'!

margins
	^margins!

margins: t1 
	margins := t1.
	^self!

pages
	^pages!

pages: t1 
	pages := t1.
	^self!

pageSize
	^pageSize!

pageSize: t1 
	pageSize := t1.
	^self!

printPagerPreferenceManager
	^self printPagerPreferenceManagerClass new!

printPagerPreferenceManagerClass
	^self class printPagerPreferenceManagerClass!

printPagerPreferenceMap
	^printPagerPreferenceMap!

printPagerPreferenceMap: t1 
	printPagerPreferenceMap := t1.
	^self!

reportObject
	^reportObject!

reportObject: t1 
	reportObject := t1.
	^self!

topComponent
	^topComponent!

topComponent: t1 
	topComponent := t1.
	^self!

width: t1 height: t2 
	| t3 |
	t3 := Screen default resolution.
	self pageSize: t1 * t3 x @ (t2 * t3 y).
	^self! !

!KRReport publicMethodsFor: 'execution'!

displayReport
	| t1 t2 t3 t4 t5 |
	t4 := 0.
	t2 := 1.
	t3 := self builder window edgeDecorationPolicy.
	(t5 := (self builder componentAt: #topComponent) spec) hasHorizontalScrollBar ifTrue: [t2 := t2 + t5 layout topOffset + t5 layout bottomOffset].
	t5 hasHorizontalScrollBar ifTrue: [t2 := t2 + t3 scrollbarThickness + t3 border top + t3 border bottom].
	t5 hasVerticalScrollBar ifTrue: [t4 := t4 + t3 scrollbarThickness + t3 border left + t3 border right].
	t5 hasBorder
		ifTrue: 
			[t2 := (t2 := t2 + t3 border top) + t3 border bottom.
			t4 := (t4 := t4 + t3 border left) + t3 border right].
	t1 := (self pageWidth + t4 min: Screen default bounds width)
				@ (self pageHeight + t2 min: Screen default bounds height).
	self openWithExtent: t1.
	^self! !

!KRReport publicMethodsFor: 'hardcopy'!

createPrintJob
	| t1 t2 |
	pages isEmpty ifTrue: [^self].
	(t2 := self topComponent copy) container: nil.
	t2 bounds: (0 @ 0 extent: self pageSize y @ self topComponent preferredBounds height).
	(t1 := KRPrintJob new) document: self.
	t1 numberOfCopies: 1.
	t1 preferenceManager: self printPagerPreferenceManager.
	^t1!

hardcopyForPrintJob: t1 
	| t2 |
	(t2 := self pages keys asSortedCollection)
		do: 
			[:t3 | 
			| t4 t5 t6 |
			t4 := 0 @ self pageSize y * (t3 - 1).
			t6 := 72.0 / 75.0.
			t5 := self pages at: t3.
			t1
				hardcopyComponents: t5
				in: self pageSize
				translatedBy: t4
				scaledBy: t6].
	^self!

openWithExtent: t1 
	self builder doFinalHookup.
	self builder window openWithExtent: t1 andType: #normal.
	^self!

printReport
	Cursor wait showWhile: [self createPrintJob execute].
	^self!

printReportUsingDialog
	Cursor wait showWhile: [self createPrintJob executeUsingDialog].
	^self! !

!KRReport publicMethodsFor: 'initialize-release'!

initialize
	super initialize.
	self topComponent: (self class reportCompositeClass new model: self).
	self allButOpenInterface: #windowSpec.
	self pages: Dictionary new; width: 8.5 height: 11.
	^self! !

!KRReport publicMethodsFor: 'private-accessing'!

bottomMargin
	^self margins bottom!

leftMargin
	^self margins left!

pageHeight
	^self pageSize y!

pageWidth
	^self pageSize x!

rightMargin
	^self margins right!

topMargin
	^self margins top! !

!KRReport publicMethodsFor: 'public interface'!

pageNumber: t1 on: t2 
	| t3 t4 t5 |
	t5 := self adjustPageInternal: t1.
	(self pages includesKey: t5)
		ifFalse: [^self].
	self report isOpen ifTrue: [t3 := self report bounds].
	t4 := t2 copy.
	self report bounds: (0 @ 0 extent: self pageWidth @ self pageHeight).
	t4 translateBy: 0 @ (t5 - 1 * self pageHeight) negated.
	(self pages at: t5)
		do: [:t6 | t6 displayOn: t4 copy].
	t3 notNil ifTrue: [self report bounds: t3].
	^self!

reportBounds
	^self pageSize x @ (self pageSize y * self pages keys size)! !

!KRReportAccumulator publicMethodsFor: 'accessing'!

dataPacket
	^self subclassResponsibility!

reportObject
	^reportObject!

reportObject: t1 
	reportObject := t1.
	^self! !

!KRReportAccumulator publicMethodsFor: 'adding'!

addDataPacket: t1 
	^t1! !

!KRReportAccumulator publicMethodsFor: 'copying'!

copyWithReportObject: t1 
	| t2 |
	(t2 := self copy) reportObject: t1.
	^t2! !

!KRReportBreak class publicMethodsFor: 'creation'!

new
	^super new initialize! !

!KRReportBreak publicMethodsFor: 'accessing'!

addSelector: t1 
	self selectorValueMap at: t1 put: #empty.
	^self!

dependOn: t1 
	BlockValue block: self dependencyBlock arguments: (Array with: self asValue with: t1).
	^self!

didBreak
	^self breakFlag!

lastBreakIndex
	^lastBreakIndex!

lastBreakIndex: t1 
	lastBreakIndex := t1.
	^self!

pageAfterBreak
	^self newPage!

selectors
	^self selectorValueMap keys!

selectors: t1 
	t1 do: [:t2 | self addSelector: t2].
	^self! !

!KRReportBreak publicMethodsFor: 'initialization-release'!

initialize
	self selectorValueMap: Dictionary new; newPage: false; breakFlag: false; lastBreakIndex: 1.
	^self! !

!KRReportBreak publicMethodsFor: 'private'!

breakFlag
	^breakFlag!

breakFlag: t1 
	breakFlag := t1.
	^self!

dependencyBlock
	^
	[:t1 :t2 | 
	| t3 |
	t1 breakFlag: false.
	(t3 := t1 selectorValueMap)
		keysDo: 
			[:t4 | 
			| t5 t6 |
			t5 := t3 at: t4.
			t6 := t4 aspectValue: t2.
			t5 == #empty ifFalse: [t5 ~= t6 ifTrue: [t1 breakFlag: true]].
			t3 at: t4 put: t6]]!

newPage
	^newPage!

newPage: t1 
	newPage := t1.
	^self!

selectorValueMap
	^selectorValueMap!

selectorValueMap: t1 
	selectorValueMap := t1.
	^self! !

!KRReportComposite publicMethodsFor: 'accessing'!

componentPool
	^componentPool!

componentPool: t1 
	componentPool := t1.
	^self!

componentsWithValidBounds
	^componentsWithValidBounds!

componentsWithValidBounds: t1 
	componentsWithValidBounds := t1.
	^self!

firstIndex
	^firstIndex!

firstIndex: t1 
	firstIndex := t1.
	^self!

lastIndex
	^lastIndex!

lastIndex: t1 
	lastIndex := t1.
	^self!

model
	^model!

model: t1 
	model := t1.
	^self!

offsetHolder
	^scrollOffsetHolder!

offsetHolder: t1 
	scrollOffsetHolder := t1.
	^self!

sortComponentPool
	self componentPool: (self componentPool asSortedCollection: self componentSortBlock).
	^self! !

!KRReportComposite publicMethodsFor: 'bounds accessing'!

computeBoundsFor: t1 using: t2 
	t1 bounds: (self computeDisplayBoxFor: t1 inDisplayBox: t2).
	self componentsWithValidBounds add: t1.
	^self!

computePreferredBounds
	^preferredBounds := 0 @ 0 extent: self model reportBounds! !

!KRReportComposite publicMethodsFor: 'initialization-release'!

initialize
	super initialize.
	self firstIndex: 1; componentPool: (OrderedCollection new: 1000); componentsWithValidBounds: Set new; offsetHolder: Point zero asValue.
	^self!

release
	Cursor execute showWhile: [self componentPool isNil
			ifTrue: [nil]
			ifFalse: 
				[self offsetHolder removeDependent: self.
				self componentPool do: [:t1 | t1 release].
				components := #().
				self componentPool: nil; componentsWithValidBounds: nil; offsetHolder: nil]].
	^super release!

scrollOffsetHolder: t1 
	super scrollOffsetHolder: t1.
	t1 addDependent: self.
	t1 grid: 1 @ 1.
	self offsetHolder: t1.
	^self! !

!KRReportComposite publicMethodsFor: 'private'!

addComponent: t1 
	| t2 |
	self isOpen
		ifTrue: 
			[t2 := self preferredBounds.
			self componentPool add: t1.
			t1 container: self.
			self computeBoundsFor: t1 using: (0 @ 0 extent: self bounds extent).
			(self isVisible: t1)
				ifTrue: 
					[components addLast: t1.
					self lastIndex: self componentPool size].
			preferredBounds := self computePreferredBounds.
			t2 = preferredBounds ifFalse: [self changedPreferredBounds: t2].
			self invalidateRectangle: t1 bounds]
		ifFalse: 
			[self componentPool add: t1.
			t1 container: self].
	^self!

componentSortBlock
	^[:t1 :t2 | t1 component layout topOffset < t2 component layout topOffset]!

layoutComponentsForBounds: t1 
	| t2 t3 t4 |
	self componentsWithValidBounds: Set new.
	components := OrderedCollection new: 100.
	self sortComponentPool.
	t2 := self firstIndex.
	t3 := self componentPool size.
	[t2 <= t3]
		whileTrue: 
			[t4 := self componentPool at: t2.
			self computeBoundsFor: t4 using: t1.
			(self isVisible: t4)
				ifTrue: [components addLast: t4]
				ifFalse: [^self lastIndex: t2 - 1].
			t2 := t2 + 1].
	self lastIndex: self componentPool size.
	^self!

removeComponent: t1 
	self componentPool remove: t1.
	^super removeComponent: t1! !

!KRReportComposite publicMethodsFor: 'testing'!

hasBounds: t1 
	^self componentsWithValidBounds includes: t1!

isVisible: t1 
	| t2 t3 t4 |
	t3 := (t2 := self offsetHolder value y abs) + self bounds height.
	((t4 := t1 bounds) origin y between: t2 and: t3)
		ifTrue: [^true].
	(t4 corner y between: t2 and: t3)
		ifTrue: [^true].
	t4 origin y < t2 ifTrue: [^t4 corner y > t3].
	^false! !

!KRReportComposite publicMethodsFor: 'updating'!

adjustForDownScroll
	| t1 t2 t3 |
	t2 := self components.
	t3 := self componentPool.
	[t2 isEmpty or: [self isVisible: t2 first]]
		whileFalse: 
			[t2 removeFirst.
			self firstIndex: self firstIndex + 1].
	t2 isEmpty
		ifTrue: 
			[[self firstIndex >= t3 size
				or: 
					[t1 := t3 at: self firstIndex.
					(self hasBounds: t1)
						ifFalse: [self computeBoundsFor: t1 using: (0 @ 0 extent: self bounds extent)].
					self isVisible: t1]]
				whileFalse: [self firstIndex: self firstIndex + 1].
			self firstIndex > t3 size
				ifFalse: 
					[t2 addFirst: (t3 at: self firstIndex).
					self lastIndex: self firstIndex]].
	[self lastIndex = t3 size
		ifTrue: [false]
		ifFalse: 
			[t1 := t3 at: self lastIndex + 1.
			(self hasBounds: t1)
				ifFalse: [self computeBoundsFor: t1 using: (0 @ 0 extent: self bounds extent)].
			self isVisible: t1]]
		whileTrue: 
			[t2 addLast: (t3 at: self lastIndex + 1).
			self lastIndex: self lastIndex + 1].
	(t2 isEmpty not and: [(self isVisible: t2 first) not])
		ifTrue: 
			[t2 removeFirst.
			self firstIndex: 1; lastIndex: 1].
	^self!

adjustForUpScroll
	| t1 t2 t3 |
	t2 := self components.
	t3 := self componentPool.
	[t2 isEmpty or: [self isVisible: t2 last]]
		whileFalse: 
			[t2 removeLast.
			self lastIndex: self lastIndex - 1].
	t2 isEmpty
		ifTrue: 
			[[self lastIndex <= 1
				or: 
					[t1 := t3 at: self lastIndex.
					(self hasBounds: t1)
						ifFalse: [self computeBoundsFor: t1 using: (0 @ 0 extent: self bounds extent)].
					self isVisible: t1]]
				whileFalse: [self lastIndex: self lastIndex - 1].
			self lastIndex < 1
				ifFalse: 
					[t2 addLast: (t3 at: self lastIndex).
					self firstIndex: self lastIndex]].
	[self firstIndex = 1
		ifTrue: [false]
		ifFalse: 
			[t1 := t3 at: self firstIndex - 1.
			(self hasBounds: t1)
				ifFalse: [self computeBoundsFor: t1 using: (0 @ 0 extent: self bounds extent)].
			self isVisible: t1]]
		whileTrue: 
			[t2 addFirst: (t3 at: self firstIndex - 1).
			self firstIndex: self firstIndex - 1].
	(t2 isEmpty not and: [(self isVisible: t2 first) not])
		ifTrue: 
			[t2 removeFirst.
			self firstIndex: t3 size; lastIndex: t3 size].
	^self!

update: t1 with: t2 from: t3 
	(t1 == #value and: [t3 == self offsetHolder])
		ifTrue: 
			[t2 y < self offsetHolder value y ifTrue: [^self adjustForUpScroll].
			t2 y > self offsetHolder value y ifTrue: [^self adjustForDownScroll].
			^nil].
	^super
		update: t1
		with: t2
		from: t3! !

!KRReportDefinition class publicMethodsFor: 'accessing'!

defaultReportDimensions
	^8 @ 10.5!

defaultReportMargins
	^0.0 @ 0.0 corner: 0.0 @ 0.0!

reportSectionClass
	^KRSectionDefinition! !

!KRReportDefinition class publicMethodsFor: 'instance creation'!

decodeFromLiteralArray: t1 
	^self new fromLiteralArrayEncoding: t1!

new
	^super new initialize! !

!KRReportDefinition publicMethodsFor: 'accessing'!

atSize: t1 putPrintPagerPreference: t2 
	self printPagerPreferenceMap at: t1 put: t2.
	^self!

bodyClass
	^self bodySection applicationClass!

bodyClass: t1 
	self bodySection applicationClass: t1.
	^self!

bodyClassObject
	| t1 |
	(t1 := self bodySection applicationClass) isSymbol ifTrue: [^Smalltalk at: t1].
	^t1!

bodySelector
	^self bodySection specSelector!

bodySelector: t1 
	self bodySection specSelector: t1.
	^self!

bottomMargin
	^bottomMargin!

bottomMargin: t1 
	bottomMargin := t1.
	^self!

breakDefinitions
	^breakDefinitions!

breakDefinitions: t1 
	breakDefinitions := t1.
	^self!

defaultPrintPagerPreferences: t1 
	self printPagerPreferenceMap defaultPreferences: t1.
	^self!

footerClass
	^self footerSection applicationClass!

footerClass: t1 
	self footerSection applicationClass: t1.
	^self!

footerClassObject
	| t1 |
	(t1 := self footerSection applicationClass) isSymbol ifTrue: [^Smalltalk at: t1].
	^t1!

footerSelector
	^self footerSection specSelector!

footerSelector: t1 
	self footerSection specSelector: t1.
	^self!

headerClass
	^self headerSection applicationClass!

headerClass: t1 
	self headerSection applicationClass: t1.
	^self!

headerClassObject
	| t1 |
	(t1 := self headerSection applicationClass) isSymbol ifTrue: [^Smalltalk at: t1].
	^t1!

headerSelector
	^self headerSection specSelector!

headerSelector: t1 
	self headerSection specSelector: t1.
	^self!

height
	^height!

height: t1 
	height := t1.
	^self!

leftMargin
	^leftMargin!

leftMargin: t1 
	leftMargin := t1.
	^self!

printPagerPreferenceMap
	printPagerPreferenceMap == nil ifTrue: [printPagerPreferenceMap := KRPrintPagerPreferenceMap new].
	^printPagerPreferenceMap!

printPagerPreferenceMap: t1 
	printPagerPreferenceMap := t1.
	^self!

rightMargin
	^rightMargin!

rightMargin: t1 
	rightMargin := t1.
	^self!

separateSummaryPage
	^self summarySection separatePage!

separateSummaryPage: t1 
	self summarySection separatePage: t1.
	^self!

separateTitlePage
	^self titleSection separatePage!

separateTitlePage: t1 
	self titleSection separatePage: t1.
	^self!

showSummaryFooter
	^self summarySection showFooter!

showSummaryFooter: t1 
	^self summarySection showFooter: t1!

showSummaryHeader
	^self summarySection showHeader!

showSummaryHeader: t1 
	^self summarySection showHeader: t1!

showTitleFooter
	^self titleSection showFooter!

showTitleFooter: t1 
	^self titleSection showFooter: t1!

showTitleHeader
	^self titleSection showHeader!

showTitleHeader: t1 
	^self titleSection showHeader: t1!

summaryClass
	^self summarySection applicationClass!

summaryClass: t1 
	self summarySection applicationClass: t1.
	^self!

summaryClassObject
	| t1 |
	(t1 := self summarySection applicationClass) isSymbol ifTrue: [^Smalltalk at: t1].
	^t1!

summarySelector
	^self summarySection specSelector!

summarySelector: t1 
	self summarySection specSelector: t1.
	^self!

titleClass
	^self titleSection applicationClass!

titleClass: t1 
	self titleSection applicationClass: t1.
	^self!

titleClassObject
	| t1 |
	(t1 := self titleSection applicationClass) isSymbol ifTrue: [^Smalltalk at: t1].
	^t1!

titleSelector
	^self titleSection specSelector!

titleSelector: t1 
	self titleSection specSelector: t1.
	^self!

topMargin
	^topMargin!

topMargin: t1 
	topMargin := t1.
	^self!

width
	^width!

width: t1 
	width := t1.
	^self! !

!KRReportDefinition publicMethodsFor: 'converting'!

literalArrayEncoding
	| t1 t2 |
	t1 := #(#bodyClass #bodySelector #headerClass #headerSelector #footerClass #footerSelector #titleClass #titleSelector #summaryClass #summarySelector #height #width #leftMargin #topMargin #rightMargin #bottomMargin #separateSummaryPage #separateTitlePage #showSummaryHeader #showSummaryFooter #showTitleHeader #showTitleFooter #breakDefinitions).
	(t2 := OrderedCollection new: t1 size * 2) add: self class name.
	t1 do: 
		[:t3 | 
		| t4 |
		(t4 := self perform: t3) == nil
			ifTrue: [nil]
			ifFalse: 
				[t2 add: (t3 , ':') asSymbol.
				t2 add: t4 literalArrayEncoding]].
	^t2! !

!KRReportDefinition publicMethodsFor: 'initialize-release'!

fromLiteralArrayEncoding: t1 
	| t2 t3 |
	t2 := 2.
	t3 := t1 size.
	[t2 <= t3]
		whileTrue: 
			[self perform: (t1 at: t2)
				with: (t1 at: t2 + 1).
			t2 := t2 + 2].
	breakDefinitions := breakDefinitions asOrderedCollection collect: [:t4 | t4 decodeAsLiteralArray].
	^self!

initialize
	| t1 t2 |
	t1 := self class defaultReportDimensions.
	self width: t1 x; height: t1 y.
	t2 := self class defaultReportMargins.
	self leftMargin: t2 left; topMargin: t2 top; rightMargin: t2 right; bottomMargin: t2 bottom.
	self titleSection: self class reportSectionClass new; headerSection: self class reportSectionClass new; bodySection: self class reportSectionClass new; footerSection: self class reportSectionClass new; summarySection: self class reportSectionClass new; breakDefinitions: OrderedCollection new.
	^self! !

!KRReportDefinition publicMethodsFor: 'private'!

bodySection
	^bodySection!

bodySection: t1 
	bodySection := t1.
	^self!

footerSection
	^footerSection!

footerSection: t1 
	footerSection := t1.
	^self!

headerSection
	^headerSection!

headerSection: t1 
	headerSection := t1.
	^self!

summarySection
	^summarySection!

summarySection: t1 
	summarySection := t1.
	^self!

titleSection
	^titleSection!

titleSection: t1 
	titleSection := t1.
	^self! !

!KRReportDefinition publicMethodsFor: 'testing'!

canConstructBodyCanvas
	^self bodySection canConstructCanvas!

canConstructFooterCanvas
	^self footerSection canConstructCanvas!

canConstructHeaderCanvas
	^self headerSection canConstructCanvas!

canConstructSummaryCanvas
	^self summarySection canConstructCanvas!

canConstructTitleCanvas
	^self titleSection canConstructCanvas!

isSummarySpecified
	self summaryClass notNil ifTrue: [^self summarySelector notNil].
	^false!

isTitleSpecified
	self titleClass notNil ifTrue: [^self titleSelector notNil].
	^false! !

!KRReportSection class publicMethodsFor: 'class accessing'!

reportSectionsSuperClass
	^KRReportSection! !

!KRReportSection class publicMethodsFor: 'instance creation'!

on: t1 
	| t2 |
	(t2 := self new initialize) dataObject: t1.
	^t2! !

!KRReportSection class publicMethodsFor: 'interface specs'!

bodyDiagnosticSpec
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 0 0 637.5 37.5)) #component: #(#SpecCollection #collection: #(#(#LabelSpec #layout: #(#Point 184 8) #label: 'DataObject:') #(#InputFieldSpec #layout: #(#Rectangle 264 4 364 28) #flags: 0 #isOpaque: true #model: #dataObjectString #tabable: false #isReadOnly: true) #(#LabelSpec #layout: #(#Point 8 8) #isOpaque: true #colors: #(#LookPreferences #setBackgroundColor: #(#ColorValue 6143 4096 8191)) #label: 'BODY'))))!

indexDiagnosticSpec
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 98 442 735.5 479.5)) #component: #(#SpecCollection #collection: #(#(#InputFieldSpec #layout: #(#Rectangle 152 4 248 28) #flags: 0 #isOpaque: true #model: #firstIndexString #tabable: false #isReadOnly: true) #(#LabelSpec #layout: #(#Point 72 8) #label: 'First Index:') #(#InputFieldSpec #layout: #(#Rectangle 352 4 452 28) #flags: 0 #isOpaque: true #model: #lastIndexString #tabable: false #isReadOnly: true) #(#LabelSpec #layout: #(#Point 264 8) #label: 'Last Index:') #(#LabelSpec #layout: #(#Point 472 8) #label: 'Page:') #(#InputFieldSpec #layout: #(#Rectangle 520 4 620 28) #flags: 0 #isOpaque: true #model: #pageString #tabable: false #isReadOnly: true))))! !

!KRReportSection class publicMethodsFor: 'private-creation'!

on: t1 firstIndex: t2 lastIndex: t3 page: t4 reportObject: t5 
	| t6 |
	(t6 := self on: t5) firstIndex: t2; lastIndex: t3; pageNumber: t4; dataObject: t1.
	^t6! !

!KRReportSection publicMethodsFor: 'accessors'!

dataObject
	^dataObject!

dataObject: t1 
	dataObject := t1.
	^self!

firstIndex
	^firstIndex!

firstIndex: t1 
	firstIndex := t1.
	^self!

lastIndex
	^lastIndex!

lastIndex: t1 
	lastIndex := t1.
	^self!

pageNumber
	pageNumber isNil ifTrue: [^pageNumber := nil asValue].
	^pageNumber!

pageNumber: t1 
	self pageNumberValue: t1.
	^self!

pageNumberValue: t1 
	self pageNumber value: t1.
	^self!

reportObject: t1 
	reportObject := t1.
	^self!

sectionSelector
	^sectionSelector!

sectionSelector: t1 
	sectionSelector := t1.
	^self! !

!KRReportSection publicMethodsFor: 'aspects'!

today
	today isNil ifTrue: [^today := Date today printString asValue].
	^today! !

!KRReportSection publicMethodsFor: 'change messages'!

changedObject! !

!KRReportSection publicMethodsFor: 'diagnostic functions'!

dataObjectString
	^self dataObject value printString asValue!

firstIndexString
	^self firstIndex printString asValue!

lastIndexString
	^self lastIndex printString asValue! !

!KRReportSection publicMethodsFor: 'functions'!

avg
	^(self sum value / (self lastIndex - self firstIndex + 1)) asFloat asValue!

count
	^(self lastIndex - self firstIndex + 1) asValue!

dateToday
	^Date today asValue!

dateTodayString
	^Date today printString asValue!

max
	| t1 |
	t1 := self builder spec name.
	^((self dataObject value copyFrom: self firstIndex + 1 to: self lastIndex)
		inject: ((self dataObject value at: self firstIndex)
				perform: t1)
		into: [:t2 :t3 | t2 max: (t3 perform: t1)]) asValue!

median
	| t1 t2 t3 |
	t2 := SortedCollection new.
	t1 := self builder spec name.
	(self dataObject value copyFrom: self firstIndex to: self lastIndex)
		do: [:t4 | t2 add: (t4 perform: t1)].
	t2 size even
		ifTrue: 
			[t3 := t2 size // 2.
			^((t2 at: t3)
				+ (t2 at: t3 + 1) / 2) asFloat asValue].
	^(t2 at: t2 size // 2 + 1) asValue!

min
	| t1 |
	t1 := self builder spec name.
	^((self dataObject value copyFrom: self firstIndex + 1 to: self lastIndex)
		inject: ((self dataObject value at: self firstIndex)
				perform: t1)
		into: [:t2 :t3 | t2 min: (t3 perform: t1)]) asValue!

page
	^self pageNumber value!

pageString
	^self pageNumber printString asValue!

range
	^(self max value - self min value) asValue!

stdev
	| t1 t2 t3 |
	t3 := self builder spec name.
	t1 := self avg value.
	^((t2 := (self dataObject value copyFrom: self firstIndex + 1 to: self lastIndex)
				inject: ((self dataObject value at: self firstIndex)
						perform: t3)
				into: [:t4 :t5 | ((t5 perform: t3)
						- t1 raisedTo: 2)
						+ t4]) / (self count value - 1)) sqrt asFloat asValue!

sum
	| t1 |
	t1 := self builder spec name.
	^((self dataObject value copyFrom: self firstIndex + 1 to: self lastIndex)
		inject: ((self dataObject value at: self firstIndex)
				perform: t1)
		into: [:t2 :t3 | t2 + (t3 perform: t1)]) asValue!

timeNow
	^Time now asValue!

timeNowString
	^Time now printString asValue!

variance
	^(self stdev value raisedTo: 2) asValue! !

!KRReportSection publicMethodsFor: 'initilization-release'!

initialize
	super initialize.
	pageNumber := nil asValue.
	^self! !

!KRReportSection publicMethodsFor: 'interface specs'!

height
	^nil!

width
	^nil! !

!KRReportSection publicMethodsFor: 'report opening'!

postCanvasAppendWith: t1 
	^self! !

!KRReportSection publicMethodsFor: 'streaming'!

addDataPacket: t1 
	^self!

addPacketToSection: t1 
	^self! !

!KRReportSection publicMethodsFor: 'testing'!

isReportFunction: t1 
	^(self class reportSectionsSuperClass organization listAtCategoryNamed: #functions)
		includes: t1! !

!KRReportWriter class publicMethodsFor: 'body generation'!

asBodyFrom: t1 column: t2 definitions: t3 previousLayout: t4 
	| t5 t6 t7 |
	t6 := OrderedCollection new.
	t7 := CompositeSpecCollection new.
	t6 add: (self generateBodyWidgetFrom: t1).
	t1 leadingVerticalLine ifTrue: [t6 add: self firstVerticalLineSpec].
	t1 trailingVerticalLine ifTrue: [t6 add: self lastVerticalLineSpec].
	(t5 := CompositeSpec new) opaque; layout: (self
			layout: t1
			column: t2
			columnCount: t3 size
			previousLayout: t4).
	t7 compositeSpec: t5; collection: t6 asArray.
	^t7!

generateBodyWidgetFrom: t1 
	| t2 |
	(t2 := t1 generateBodyWidgetSpec) layout: ((LayoutFrame new) rightFraction: 1.0; bottomFraction: 1.0).
	^t2!

makeBodySpecCollection: t1 
	| t2 t3 t4 t5 |
	t2 := Array new: t1 size.
	t3 := SpecCollection new.
	t4 := 1.
	t5 := nil.
	t1
		do: 
			[:t6 | 
			| t7 |
			t7 := self
						asBodyFrom: t6
						column: t4
						definitions: t1
						previousLayout: t5.
			t2 at: t4 put: t7.
			t5 := t7 compositeSpec layout.
			t4 := t4 + 1].
	t3 collection: t2.
	^t3!

makeMainBodySpec: t1 
	| t2 |
	(t2 := FullSpec new) window: self makeWindowSpec.
	t2 component: (self makeBodySpecCollection: t1).
	^t2! !

!KRReportWriter class publicMethodsFor: 'class accessing'!

breakPointClass
	^KRReportBreak!

reportClass
	^KRReport! !

!KRReportWriter class publicMethodsFor: 'header generation'!

asHeaderFrom: t1 column: t2 definitions: t3 previousLayout: t4 
	| t5 t6 t7 |
	t6 := OrderedCollection new.
	t7 := CompositeSpecCollection new.
	t6 add: (self generateHeaderWidgetFrom: t1).
	t1 leadingVerticalLine ifTrue: [t6 add: self firstVerticalLineSpec].
	t1 trailingVerticalLine ifTrue: [t6 add: self lastVerticalLineSpec].
	(t5 := CompositeSpec new) opaque; layout: (self
			layout: t1
			column: t2
			columnCount: t3 size
			previousLayout: t4).
	t7 compositeSpec: t5; collection: t6 asArray.
	^t7!

generateHeaderWidgetFrom: t1 
	| t2 |
	(t2 := t1 generateHeaderWidgetSpec) layout: ((LayoutFrame new) rightFraction: 1.0; bottomFraction: 1.0).
	^t2!

makeHeaderSpec: t1 
	| t2 |
	(t2 := FullSpec new) window: self makeWindowSpec.
	t2 component: (self makeHeaderSpecCollection: t1).
	^t2!

makeHeaderSpecCollection: t1 
	| t2 t3 t4 t5 |
	t2 := Array new: t1 size.
	t3 := SpecCollection new.
	t4 := 1.
	t5 := nil.
	t1
		do: 
			[:t6 | 
			t2 at: t4 put: (self
					asHeaderFrom: t6
					column: t4
					definitions: t1
					previousLayout: t5).
			t5 := (t2 at: t4) compositeSpec layout.
			t4 := t4 + 1].
	t3 collection: t2.
	^t3! !

!KRReportWriter class publicMethodsFor: 'instance creation'!

new
	^super new initialize! !

!KRReportWriter class publicMethodsFor: 'public interface'!

reportDefinition: t1 
	| t2 |
	(t2 := self new) reportDefinition: t1.
	^t2!

reportName: t1 executeOnStream: t2 reportObject: t3 
	| t4 t5 |
	t4 := self reportSpecFor: t1.
	((t5 := self new) reportDefinition: t4)
		ifTrue: 
			[t5 reportObject: t3.
			^t5 executeOnStream: t2].
	^nil! !

!KRReportWriter class publicMethodsFor: 'spec generation'!

firstVerticalLineSpec
	| t1 |
	(t1 := DividerSpec new) orientation: #vertical; layout: ((LayoutFrame new) leftOffset: 1; rightFraction: 1.0; bottomFraction: 1.0).
	^t1!

lastVerticalLineSpec
	| t1 |
	(t1 := DividerSpec new) orientation: #vertical; layout: ((LayoutFrame new) leftFraction: 1.0; leftOffset: -1; rightFraction: 1.0; bottomFraction: 1.0).
	^t1!

makeWindowSpec: t1 
	| t2 |
	(t2 := WindowSpec new) min: 50 @ 50.
	t2 max: nil.
	t2 label: 'Generated Canvas'.
	t2 bounds: (Rectangle origin: 0 @ 0 extent: t1 @ 32).
	^t2!

reportSpecFor: t1 
	| t2 |
	(t2 := self perform: t1) class == Array ifTrue: [^t2 decodeAsLiteralArray].
	^t2! !

!KRReportWriter publicMethodsFor: 'break points'!

breakPoints
	^breakPoints!

breakPoints: t1 
	breakPoints := t1.
	^self!

levelWhichBroke
	| t1 t2 |
	^(t2 := (t1 := self breakPoints) keys asSortedCollection) detect: [:t3 | (t1 at: t3) didBreak]
		ifNone: [nil]! !

!KRReportWriter publicMethodsFor: 'execution'!

computePacketSize
	self packetSize: 1.
	^self!

executionLoop
	self printTitle.
	self makeRoomForBreakHeadersFromLevel: 1.
	[self dataStream atEnd]
		whileFalse: 
			[self isolateDataPacket.
			self process: self currentPacket].
	self printBreaksForIndex: self dataStream position level: 1.
	self printSummary.
	self release.
	^self report!

footerClass: t1 selector: t2 
	self sectionAt: #footer put: (self sectionClass: t1 selector: t2).
	^self!

fromBreakDefinition: t1 
	| t2 t3 t4 t5 |
	t2 := self breakPoints.
	t4 := t1 level.
	(t2 includesKey: t4)
		ifFalse: 
			[(t3 := self class breakPointClass new) newPage: t1 newPage; dependOn: self currentPacketHolder.
			t2 at: t4 put: t3.
			t5 := Dictionary new.
			t1 isHeaderSpecified ifTrue: [t5 at: #header put: (self sectionClass: t1 headerClassObject selector: t1 headerSelector asSymbol)].
			t1 isFooterSpecified ifTrue: [t5 at: #footer put: (self sectionClass: t1 footerClassObject selector: t1 footerSelector asSymbol)].
			self sectionAt: t3 put: t5].
	(t2 at: t4)
		selectors: t1 selectorCollection.
	^self!

headerClass: t1 selector: t2 
	self sectionAt: #header put: (self sectionClass: t1 selector: t2).
	^self!

isolateDataPacket
	| t1 t2 t3 |
	self packetSize > 1
		ifTrue: 
			[t1 := Array new: self packetSize.
			t2 := 1.
			t3 := t1 size.
			[t2 <= t3]
				whileTrue: 
					[t1 at: t2 put: self dataStream next.
					t2 := t2 + 1]]
		ifFalse: [t1 := self dataStream next].
	self currentPacket: t1.
	^self!

mainBodyClass: t1 selector: t2 
	self sectionAt: #mainBody put: (self sectionClass: t1 selector: t2).
	^self!

pop
	| t1 |
	(t1 := self intervalStack) isEmpty ifTrue: [^self error: 'Attempt to pop from empty stack.'].
	^t1 removeFirst!

process: t1 
	| t2 |
	(t2 := self levelWhichBroke) notNil
		ifTrue: 
			[self printBreaksForIndex: self dataStream position - 1 level: t2.
			(self breakPoints at: t2) pageAfterBreak
				ifTrue: 
					[self push: (self pop first to: self dataStream position - 1); printHeaderAndFooter.
					self nextPage].
			self makeRoomForBreakHeadersFromLevel: t2].
	self addPacketToSections: t1.
	self printMainBody: t1.
	^self!

push: t1 
	self intervalStack addFirst: t1.
	^self!

resetDataStream
	self dataStream reset.
	^self!

sectionClass: t1 selector: t2 
	| t3 t4 t5 |
	t3 := t1 interfaceSpecFor: t2.
	t4 := (SubCanvasSpec new) majorKey: t1 name; minorKey: t2.
	(t5 := Dictionary new) at: #applicationClass put: t1; at: #spec put: t4; at: #bounds put: t3 window bounds; at: #fullSpec put: t3.
	t4 colors: t3 window colors; hasBorder: t3 window hasBorder.
	^t5!

sort: t1 
	| t2 t3 t4 t5 t6 |
	(t2 := self breakPoints) isEmpty ifTrue: [^t1].
	t3 := t2 keys asSortedCollection.
	t4 := OrderedCollection new.
	t3 do: [:t7 | t4 addAll: (t2 at: t7) selectors].
	t5 := '[:a :b | 
			| index  theSelectors lastA lastB | 
			index := 1.
			theSelectors := ' , t4 asArray printString , '. 
			[ (index <= theSelectors size) and:
				[(((lastA := a perform: (theSelectors at: index) asSymbol) < (lastB :=b perform: (theSelectors at: index) asSymbol)) not ) and:[lastA = lastB] ] ]
				whileTrue:[ index := index + 1].  
			index <= theSelectors size and:[lastA < lastB]]'.
	((Smalltalk includesKey: #JPSortedCollectionFrame)
		and: [Smalltalk includesKey: #JPNBlock])
		ifTrue: [(t6 := (Smalltalk at: #JPSortedCollectionFrame)
						perform: #newShared) sortBlock: ((Smalltalk at: #JPNBlock)
					source: t5)]
		ifFalse: [(t6 := SortedCollection new) sortBlock: (Compiler evaluate: t5)].
	t6 addAll: t1.
	^t6!

summaryClass: t1 selector: t2 reportDefinition: t3 
	| t4 |
	self sectionAt: #summary put: (t4 := self sectionClass: t1 selector: t2).
	t4 at: #separatePage put: t3 separateSummaryPage; at: #showHeader put: t3 showSummaryHeader; at: #showFooter put: t3 showSummaryFooter.
	^self!

titleClass: t1 selector: t2 reportDefinition: t3 
	| t4 |
	self sectionAt: #title put: (t4 := self sectionClass: t1 selector: t2).
	t4 at: #separatePage put: t3 separateTitlePage; at: #showHeader put: t3 showTitleHeader; at: #showFooter put: t3 showTitleFooter.
	^self!

top
	| t1 |
	(t1 := self intervalStack) isEmpty ifTrue: [^self error: 'Attempt to access empty stack.'].
	^t1 first! !

!KRReportWriter publicMethodsFor: 'initialize-release'!

changeRequest
	self releaseModels.
	^true!

initialize
	self report: self class reportClass new.
	self sections: Dictionary new; breakPoints: Dictionary new; currentAggregateSections: Dictionary new; intervalStack: OrderedCollection new; pageCount: 1; currentPacketHolder: ValueHolder new.
	^self!

release
	self sections do: [:t1 | t1 release].
	self dataStream release.
	self breakPoints do: [:t2 | t2 release].
	self currentPacketHolder release.
	super release.
	^self! !

!KRReportWriter publicMethodsFor: 'private-accessing'!

currentPacket
	^self currentPacketHolder value!

currentPacket: t1 
	self currentPacketHolder value: t1.
	^self!

currentPacketHolder
	^currentPacketHolder!

currentPacketHolder: t1 
	currentPacketHolder := t1.
	^self!

dataStream
	^dataStream!

dataStream: t1 
	dataStream := t1.
	^self!

footerHeight
	| t1 |
	(t1 := self footerSection) notNil ifTrue: [^(t1 at: #bounds) height].
	^0!

footerSection
	^self sectionAt: #footer ifAbsent: [nil]!

headerHeight
	| t1 |
	(t1 := self headerSection) notNil ifTrue: [^(t1 at: #bounds) height].
	^0!

headerSection
	^self sectionAt: #header ifAbsent: [nil]!

intervalStack
	^intervalStack!

intervalStack: t1 
	intervalStack := t1.
	^self!

mainBodyHeight
	| t1 |
	(t1 := self mainBodySection) notNil ifTrue: [^(t1 at: #bounds) height].
	^0!

mainBodySection
	^self sectionAt: #mainBody ifAbsent: [nil]!

packetSize
	^packetSize!

packetSize: t1 
	packetSize := t1.
	^self!

pageCount
	^pageCount!

pageCount: t1 
	pageCount := t1.
	^self!

report
	^report!

report: t1 
	report := t1.
	^self!

sectionAt: t1 
	^sections at: t1!

sectionAt: t1 ifAbsent: t2 
	^sections at: t1 ifAbsent: t2!

sectionAt: t1 put: t2 
	| t3 t4 |
	self sections at: t1 put: t2.
	t1 isSymbol
		ifTrue: [self putNewCurrentAggregateSectionAt: t2]
		ifFalse: 
			[(t2 includesKey: #header)
				ifTrue: 
					[self putNewCurrentAggregateSectionAt: (t2 at: #header).
					(t3 := self currentAggregateSectionAt: (t2 at: #header)) sectionSelector: t1].
			(t2 includesKey: #footer)
				ifTrue: 
					[self putNewCurrentAggregateSectionAt: (t2 at: #footer).
					(t4 := self currentAggregateSectionAt: (t2 at: #header)) sectionSelector: t1]].
	^self!

sectionOffset
	^sectionOffset!

sectionOffset: t1 
	sectionOffset := t1.
	^self!

sections
	^sections!

sections: t1 
	sections := t1.
	^self!

separateSummaryPage
	| t1 |
	(t1 := self summarySection) notNil ifTrue: [^t1 at: #separatePage].
	^false!

separateTitlePage
	| t1 |
	(t1 := self titleSection) notNil ifTrue: [^t1 at: #separatePage].
	^false!

summaryHeight
	| t1 |
	(t1 := self summarySection) notNil ifTrue: [^(t1 at: #bounds) height].
	^0!

summarySection
	^self sectionAt: #summary ifAbsent: [nil]!

titleHeight
	| t1 |
	(t1 := self titleSection) notNil ifTrue: [^(t1 at: #bounds) height].
	^0!

titleSection
	^self sectionAt: #title ifAbsent: [nil]! !

!KRReportWriter publicMethodsFor: 'private-printing'!

adjustPageExternal: t1 
	(self titleHeight > 0 and: [self separateTitlePage])
		ifTrue: [^t1 - 1].
	^t1!

adjustPageInternal: t1 
	(self titleHeight > 0 and: [self separateTitlePage])
		ifTrue: [^t1 + 1].
	^t1!

makeRoomForBreakHeadersFromLevel: t1 
	| t2 t3 |
	(t3 := self breakPoints) isEmpty ifTrue: [^self].
	t2 := SortedCollection sortBlock: [:t4 :t5 | t4 < t5].
	t3 keysDo: [:t6 | (((self sectionAt: (t3 at: t6))
			includesKey: #header)
			and: [t6 >= t1])
			ifTrue: [t2 add: t6]
			ifFalse: [nil]].
	t2
		do: 
			[:t7 | 
			| t8 |
			t8 := (self sectionAt: (t3 at: t7))
						at: #header.
			(self canPageHold: (t8 at: #bounds) height)
				ifFalse: 
					[self push: (self pop first to: self dataStream position - 1); printHeaderAndFooter.
					self nextPage].
			t8 at: #displayOffset put: self sectionOffset; at: #displayPage put: self pageCount.
			self sectionOffset: self sectionOffset + (t8 at: #bounds) height].
	^self!

makeRoomForHeader
	self sectionOffset: self sectionOffset + self headerHeight.
	^self!

model: t1 
	| t2 |
	t2 := self pop.
	^(self currentAggregateSectionAt: t1)
		firstIndex: t2 first;
		lastIndex: t2 last;
		pageNumber: (self adjustPageExternal: self pageCount)!

model: t1 on: t2 
	^((t1 at: #applicationClass)
		on: t2)
		reportObject: self reportObject!

nextPage
	self sectionOffset: self pageHeight * self pageCount + self topMargin.
	self pageCount: self pageCount + 1.
	self printPageDivider.
	self makeRoomForHeader.
	self push: ((self dataStream position max: 1)
			to: #unknown).
	^self!

print: t1 model: t2 
	| t3 t4 t5 t6 |
	(t3 := self builder newSubBuilder) source: t2; composite: self report topComponent.
	t2 builder: t3.
	(t5 := t2 height) == nil ifTrue: [t5 := (t1 at: #bounds) height].
	t6 := self pageWidth.
	t4 := (LayoutFrame new) leftFraction: 0; leftOffset: self leftMargin; rightFraction: 0; rightOffset: t6; topOffset: self sectionOffset; bottomOffset: self sectionOffset + t5.
	(t1 at: #spec)
		layout: t4.
	t3 add: (t1 at: #spec).
	t2 postCanvasAppendWith: self.
	self sectionOffset: self sectionOffset + t5.
	(self pages includesKey: self pageCount)
		ifFalse: [self pages at: self pageCount put: OrderedCollection new].
	(self pages at: self pageCount)
		add: t3 wrapper.
	^self!

printBreaksForIndex: t1 level: t2 
	self printHeaderBreaksDownFromLevel: t2 usingIndex: t1; printFooterBreaksUpToLevel: t2 stopIndex: t1.
	^self!

printFooter
	| t1 t2 t3 |
	(t3 := self footerHeight) > 0
		ifTrue: 
			[t2 := self sectionOffset.
			self sectionOffset: self pageHeight * self pageCount - self bottomMargin - self footerHeight.
			t1 := self footerSection.
			self print: t1 model: (self model: t1).
			self replaceCurrentAggregateSectionAt: t1.
			self sectionOffset: t2]
		ifFalse: [self pop].
	^t3 > 0!

printFooterBreaksUpToLevel: t1 stopIndex: t2 
	| t3 t4 |
	(t4 := self breakPoints) isEmpty ifTrue: [^self].
	t3 := SortedCollection sortBlock: [:t5 :t6 | t5 > t6].
	t4 keysDo: [:t7 | (((self sectionAt: (t4 at: t7))
			includesKey: #footer)
			and: [t7 >= t1])
			ifTrue: [t3 add: t7]
			ifFalse: [nil]].
	t3
		do: 
			[:t8 | 
			| t9 t10 |
			t9 := t4 at: t8.
			t10 := (self sectionAt: t9)
						at: #footer.
			(self canPageHold: (t10 at: #bounds) height)
				ifFalse: 
					[self push: (self pop first to: (self dataStream atEnd
								ifTrue: [self dataStream position]
								ifFalse: [self dataStream position - 1])); printHeaderAndFooter.
					self nextPage].
			self push: (t9 lastBreakIndex to: t2).
			t9 lastBreakIndex: self dataStream position.
			self print: t10 model: (self model: t10).
			self replaceCurrentAggregateSectionAt: t10].
	^self!

printHeader
	| t1 t2 t3 |
	(t3 := self headerHeight) > 0
		ifTrue: 
			[t1 := self sectionOffset.
			self sectionOffset: self pageHeight * (self pageCount - 1) + self topMargin.
			t2 := self headerSection.
			self print: t2 model: (self model: t2).
			self replaceCurrentAggregateSectionAt: t2.
			self sectionOffset: t1]
		ifFalse: [self pop].
	^t3 > 0!

printHeaderAndFooter
	| t1 |
	t1 := self top.
	self printHeader; push: t1; printFooter.
	^self!

printHeaderBreaksDownFromLevel: t1 usingIndex: t2 
	| t3 t4 t5 t6 t7 |
	(t5 := self breakPoints) isEmpty ifTrue: [^self].
	t3 := SortedCollection sortBlock: [:t8 :t9 | t8 < t9].
	t6 := self sectionOffset.
	t7 := self pageCount.
	t5 keysDo: [:t10 | (((self sectionAt: (t5 at: t10))
			includesKey: #header)
			and: [t10 >= t1])
			ifTrue: [t3 add: t10]
			ifFalse: [nil]].
	t3
		do: 
			[:t11 | 
			| t12 |
			t12 := t5 at: t11.
			t4 := (self sections at: t12)
						at: #header.
			self push: (t12 lastBreakIndex to: t2); sectionOffset: (t4 at: #displayOffset); pageCount: (t4 at: #displayPage); print: t4 model: (self model: t4).
			((self sections at: t12)
				includesKey: #footer)
				ifFalse: [t12 lastBreakIndex: self dataStream position].
			t4 removeKey: #displayOffset; removeKey: #displayPage].
	self replaceCurrentAggregateSectionAt: t4.
	self sectionOffset: t6; pageCount: t7.
	^self!

printMainBody: t1 
	| t2 t3 t4 |
	t2 := self mainBodySection.
	(t3 := self model: t2 on: t1) addDataPacket: t1.
	(t4 := t3 height) == nil ifTrue: [t4 := self mainBodyHeight].
	t4 > 0
		ifTrue: 
			[(self canPageHold: t4)
				ifFalse: 
					[self push: (self pop first to: self dataStream position - 1).
					self printHeaderAndFooter.
					self nextPage].
			self print: t2 model: t3].
	^self!

printPageDivider
	| t1 t2 |
	(t1 := self builder newSubBuilder) source: nil; composite: self report topComponent.
	(t2 := #(#DividerSpec #layout: #(#Rectangle 23 71 123 75)) decodeAsLiteralArray) layout: ((LayoutFrame new) leftOffset: 0; rightOffset: self pageWidth; topOffset: self sectionOffset - self topMargin - 3; bottomOffset: self sectionOffset).
	t1 add: t2.
	^self!

printSummary
	| t1 t2 t3 t4 t5 |
	(t1 := self summaryHeight) > 0
		ifTrue: 
			[t4 := (t2 := self summarySection) at: #separatePage.
			(t3 := self sectionOffset - self topMargin \\ self pageHeight = 0)
				ifTrue: [self sectionOffset // self pageHeight = (self pageCount - 1) ifFalse: [self push: (self pop first to: self dataStream position); printHeaderAndFooter; nextPage]]
				ifFalse: [(t4 or: [(self canPageHold: t1) not])
						ifTrue: [self push: (self pop first to: self dataStream position); printHeaderAndFooter; nextPage]].
			(t3 or: [self shouldPrintSummaryHeader])
				ifFalse: [self removeRoomForHeader].
			self push: (1 to: self dataStream size); print: t2 model: (self model: t2).
			t5 := t4
						ifTrue: [1 to: 0]
						ifFalse: [self pop first to: self dataStream position]]
		ifFalse: [t5 := self pop first to: self dataStream position].
	self shouldPrintSummaryHeader ifTrue: [self push: t5; printHeader].
	self shouldPrintSummaryFooter ifTrue: [self push: t5; printFooter].
	^t1 > 0!

printTitle
	| t1 t2 t3 t4 |
	(t1 := self titleHeight) > 0
		ifTrue: 
			[((t3 := (t2 := self titleSection) at: #separatePage) not or: [t3 and: [t2 at: #showHeader]])
				ifTrue: [self makeRoomForHeader].
			self push: (1 to: self dataStream size); print: t2 model: (self model: t2); push: (1 to: #unknown).
			t3
				ifTrue: 
					[t4 := self pop first to: 0.
					self shouldPrintTitleHeader ifTrue: [self push: t4; printHeader].
					self shouldPrintTitleFooter ifTrue: [self push: t4; printFooter].
					self nextPage]]
		ifFalse: [self makeRoomForHeader; push: (1 to: #unknown)].
	^t1 > 0!

removeRoomForHeader
	self sectionOffset: self sectionOffset - self headerHeight.
	^self! !

!KRReportWriter publicMethodsFor: 'public interface'!

executeOn: t1 
	self dataStream: (ReadStream on: t1).
	self resetDataStream; computePacketSize.
	self executionLoop.
	^self report!

leftMargin: t1 topMargin: t2 rightMargin: t3 bottomMargin: t4 
	| t5 |
	t5 := Screen default resolution.
	self margins: (t1 * t5 x @ (t2 * t5 y) corner: t3 * t5 x @ (t4 * t5 y)).
	^self!

reportDefinition: t1 
	t1 canConstructTitleCanvas ifTrue: [self
			titleClass: t1 titleClassObject
			selector: t1 titleSelector asSymbol
			reportDefinition: t1].
	t1 canConstructHeaderCanvas ifTrue: [self headerClass: t1 headerClassObject selector: t1 headerSelector asSymbol].
	t1 canConstructBodyCanvas ifTrue: [self mainBodyClass: t1 bodyClassObject selector: t1 bodySelector asSymbol].
	t1 canConstructFooterCanvas ifTrue: [self footerClass: t1 footerClassObject selector: t1 footerSelector asSymbol].
	t1 canConstructSummaryCanvas ifTrue: [self
			summaryClass: t1 summaryClassObject
			selector: t1 summarySelector asSymbol
			reportDefinition: t1].
	t1 breakDefinitions do: [:t2 | (t2 isSelectorSpecified or: [t2 level = 0])
			ifTrue: [self fromBreakDefinition: t2]
			ifFalse: [(SimpleDialog new confirm: 'Break at level: ' , t2 level printString , ' is not completely specified. Skip it?' initialAnswer: true)
					ifTrue: [nil]
					ifFalse: [^false]]].
	self width: t1 width height: t1 height.
	self
		leftMargin: t1 leftMargin
		topMargin: t1 topMargin
		rightMargin: t1 rightMargin
		bottomMargin: t1 bottomMargin.
	self printPagerPreferenceMap: t1 printPagerPreferenceMap.
	^true!

width: t1 height: t2 
	self report width: t1 height: t2.
	^self! !

!KRReportWriter publicMethodsFor: 'report accessing'!

bottomMargin
	^self report bottomMargin!

builder
	^self report builder!

leftMargin
	^self report leftMargin!

margins
	^self report margins!

margins: t1 
	self report margins: t1.
	self sectionOffset: t1 top.
	^self!

pageHeight
	^self report pageHeight!

pages
	^self report pages!

pageSize
	^self report pageSize!

pageSize: t1 
	self report pageSize: t1.
	^self!

pageWidth
	^self report pageWidth!

printPagerPreferenceMap: t1 
	self report printPagerPreferenceMap: t1.
	^self!

reportObject
	^self report reportObject!

reportObject: t1 
	self report reportObject: t1.
	self currentAggregateSections do: [:t2 | t2 reportObject: t1].
	^self!

rightMargin
	^self margins right!

topMargin
	^self margins top! !

!KRReportWriter publicMethodsFor: 'streaming'!

addPacketToSections: t1 
	self currentAggregateSections do: [:t2 | t2 addDataPacket: t1].
	^self!

currentAggregateSectionAt: t1 
	^self currentAggregateSections at: t1!

currentAggregateSections
	^currentAggregateSections!

currentAggregateSections: t1 
	currentAggregateSections := t1.
	^self!

executeOnStream: t1 
	self dataStream: t1.
	self resetDataStream; computePacketSize.
	self executionLoop.
	^self report!

putNewCurrentAggregateSectionAt: t1 
	^self currentAggregateSections at: t1 put: (self model: t1 on: self reportObject)!

replaceCurrentAggregateSectionAt: t1 
	| t2 |
	(t2 := self model: t1 on: self reportObject) sectionSelector: (self currentAggregateSectionAt: t1) sectionSelector.
	^self currentAggregateSections at: t1 put: t2! !

!KRReportWriter publicMethodsFor: 'testing'!

canPageHold: t1 
	self sectionOffset - self headerHeight - self topMargin = (self pageHeight * (self pageCount - 1)) ifTrue: [^true].
	^self sectionOffset + t1 + self footerHeight <= (self pageHeight * self pageCount - self bottomMargin)!

shouldPrintSummaryFooter
	self footerHeight > 0
		ifTrue: 
			[self separateSummaryPage not ifTrue: [^true].
			^self summarySection at: #showFooter].
	^false!

shouldPrintSummaryHeader
	self headerHeight > 0
		ifTrue: 
			[self separateSummaryPage not ifTrue: [^true].
			^self summarySection at: #showHeader].
	^false!

shouldPrintTitleFooter
	self footerHeight > 0
		ifTrue: 
			[self separateTitlePage not ifTrue: [^true].
			^self titleSection at: #showFooter].
	^false!

shouldPrintTitleHeader
	self headerHeight > 0
		ifTrue: 
			[self separateTitlePage not ifTrue: [^true].
			^self titleSection at: #showHeader].
	^false! !

!KRRWAggregateNode publicMethodsFor: 'accessing'!

accumulator
	^self dataObject!

dataPacket
	^self accumulator dataPacket! !

!KRRWAggregateNode publicMethodsFor: 'adding'!

addDataPacket: t1 
	super addDataPacket: t1.
	self accumulator == nil ifFalse: [self accumulator addDataPacket: t1].
	^t1! !

!KRRWAggregateNodeCreator publicMethodsFor: 'accessing'!

accumulatorPrototype
	^accumulatorPrototype!

accumulatorPrototype: t1 
	accumulatorPrototype := t1.
	^self!

cellSpecs: t1 
	cellSpecs := t1.
	self realizeCellSpecs.
	^self!

nodeClass
	^KRRWAggregateNode!

wrappedModel: t1 
	self accumulatorPrototype == nil ifTrue: [^t1].
	^self accumulatorPrototype copyWithReportObject: t1! !

!KRRWAggregateNodeCreator publicMethodsFor: 'realizing'!

realizeCellSpecs
	| t1 t2 t3 t4 |
	t1 := self cellSpecs.
	t2 := 1.
	t3 := t1 size.
	[t2 <= t3]
		whileTrue: 
			[(t4 := t1 at: t2) layout realizeXFrom: t1.
			t4 layout: t4 layout fullyRealized.
			t2 := t2 + 1].
	^self! !

!KRRWApplicationNode class publicMethodsFor: 'instance creation'!

application: t1 
	| t2 |
	(t2 := self new) application: t1.
	^t2! !

!KRRWApplicationNode publicMethodsFor: 'accessing'!

application
	^application!

application: t1 
	application := t1.
	^self! !

!KRRWApplicationNode publicMethodsFor: 'report accessing'!

applicationName
	^self application name!

numberOfClasses
	^self application subApplications inject: self application classes size into: [:t1 :t2 | t1 + (self class application: t2) numberOfClasses]!

numberOfLocalMethods
	^self application classes inject: 0 into: [:t1 :t2 | t1 + (KRRWClassNode containedClass: t2 application: self application) numberOfMethods]!

numberOfMethods
	^self application subApplications inject: self numberOfLocalMethods into: [:t1 :t2 | t1 + (self class application: t2) numberOfLocalMethods]! !

!KRRWBodyNode publicMethodsFor: 'accessing'!

dataPacket
	^dataPacket!

dataPacket: t1 
	dataPacket := t1.
	^self! !

!KRRWBodyNode publicMethodsFor: 'adding'!

addDataPacket: t1 
	super addDataPacket: t1.
	self dataPacket: t1.
	^t1! !

!KRRWBodyNodeCreator publicMethodsFor: 'accessing'!

nodeClass
	^KRRWBodyNode! !

!KRRWReportNode class publicMethodsFor: 'instance creation'!

on: t1 withSpec: t2 withCellSpecs: t3 withAspectAccessors: t4 
	| t5 |
	(t5 := self on: t1) reportComponentSpec: t2.
	t5 cellSpecs: t3.
	t5 aspectAccessors: t4.
	^t5! !

!KRRWReportNode publicMethodsFor: 'accessing'!

aspectAccessors
	^aspectAccessors!

aspectAccessors: t1 
	aspectAccessors := t1.
	^self!

cellValues
	cellValues == nil ifTrue: [cellValues := self computeCellValues asValue].
	^cellValues!

cellValues: t1 
	cellValues := t1.
	^self!

dataPacket
	^self subclassResponsibility!

width
	width == nil ifTrue: [width := self computeWidth].
	^width! !

!KRRWReportNode publicMethodsFor: 'initializing'!

computeCellValues
	^self aspectAccessors collect: [:t1 | t1 access: self dataPacket]!

computeHeight
	| t1 t2 t3 t4 |
	t2 := 0.
	t4 := 0.
	t1 := self reportComponentSpec.
	t3 := self cellSpecs.
	t1 window bounds bottom == #variable
		ifFalse: 
			[self cellSpecs
				do: 
					[:t5 | 
					t5 realizeXFrom: t3.
					t5 realizeYFrom: t3 dynamicHeight: 0.
					t5 fullyRealize].
			^t1 window bounds height].
	self
		cellSpecObjectsAndValuesDo: 
			[:t6 :t7 | 
			t6 realizeXFrom: t3.
			t6 realizeYFrom: t3 dynamicHeight: (t6 preferredHeightUsingValue: t7 forDevice: PostScriptPrinter default).
			t6 fullyRealize.
			t2 := t2 max: t6 layout bottom.
			t4 := t4 max: t6 layout right].
	t1 component collection: t3.
	t1 window bounds bottom: t2.
	t1 window bounds right: t4.
	^t2 - t1 window bounds top!

computeWidth
	| t1 |
	self height.
	^(t1 := self reportComponentSpec window bounds) right + 15! !

!KRRWReportNode publicMethodsFor: 'interface specs'!

cellSpecs
	^cellSpecs!

cellSpecs: t1 
	cellSpecs := t1.
	^self!

height
	height == nil ifTrue: [height := self computeHeight].
	^height!

reportComponentSpec
	^reportComponentSpec!

reportComponentSpec: t1 
	reportComponentSpec := t1.
	^self!

specificationFor: t1 
	^self reportComponentSpec! !

!KRRWReportNode publicMethodsFor: 'iterating'!

cellSpecObjectsAndValuesDo: t1 
	| t2 t3 t4 t5 |
	t2 := self cellSpecs.
	t3 := self cellValues value.
	t4 := 1.
	t5 := t2 size.
	[t4 <= t5]
		whileTrue: 
			[t1 value: (t2 at: t4)
				value: (t3 at: t4).
			t4 := t4 + 1].
	^self! !

!KRRWReportNodeCreator publicMethodsFor: 'accessing'!

cellSpecs
	^cellSpecs!

cellSpecs: t1 
	cellSpecs := t1.
	^self!

cellSpecsAddingArrayAccess: t1 
	| t2 t3 |
	t2 := 1.
	t3 := t1 size.
	[t2 <= t3]
		whileTrue: 
			[(t1 at: t2)
				getBlock: [:t4 | t4 at: t2].
			t2 := t2 + 1].
	self cellSpecs: t1.
	^self!

cellUISpecs
	cellUISpecs == nil ifTrue: [cellUISpecs := self uiSpecsFromCellSpecs].
	^cellUISpecs!

cellUISpecs: t1 
	cellUISpecs := t1.
	^self!

nodeClass
	^self subclassResponsibility!

reportComponentSpec
	reportComponentSpec == nil ifTrue: [reportComponentSpec := self reportComponentSpecWithComponents: self cellSpecs].
	^reportComponentSpec!

reportComponentSpec: t1 
	reportComponentSpec := t1.
	^self!

wrappedModel: t1 
	^t1! !

!KRRWReportNodeCreator publicMethodsFor: 'initializing'!

uiSpecsFromCellSpecs
	| t1 t2 t3 |
	t1 := Array new: self cellSpecs size.
	t2 := 1.
	t3 := t1 size.
	[t2 <= t3]
		whileTrue: 
			[t1 at: t2 put: (self uiSpecFromCellSpec: (self cellSpecs at: t2)
					index: t2).
			t2 := t2 + 1].
	^t1! !

!KRRWReportNodeCreator publicMethodsFor: 'report generation'!

interfaceSpecFor: t1 
	^UISpecification from: (self perform: t1)!

name
	^nil!

on: t1 
	| t2 t3 |
	t2 := self cellUISpecs collect: 
				[:t4 | 
				| t5 |
				(t5 := t4 copy) layout: t5 layout copy].
	t3 := self reportComponentSpecWithComponents: t2.
	^self nodeClass
		on: (self wrappedModel: t1)
		withSpec: t3
		withCellSpecs: t2
		withAspectAccessors: self cellSpecs! !

!KRRWReportNodeCreator publicMethodsFor: 'spec generation'!

inputSpecTemplate
	^#(#TextEditorSpec #layout: #(#Rectangle 0 0 100 16) #flags: 0 #model: #cellValueAccessor #alignment: #right #style: #pixelSmall #isReadOnly: true)!

reportComponentSpecWithComponents: t1 
	| t2 |
	t2 := self reportSpecTemplate decodeAsLiteralArray.
	t1 do: [:t3 | t3 layout bottom isDynamicPosition
			ifTrue: [t2 window bounds bottom: #variable]
			ifFalse: [nil]].
	t2 component collection: t1.
	^t2!

reportSpecTemplate
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 0 0) #bounds: #(#Rectangle 0 0 633 16)) #component: #(#SpecCollection #collection: #colllectionOfSpecs))!

uiSpecFromCellSpec: t1 index: t2 
	| t3 |
	(t3 := self inputSpecTemplate decodeAsLiteralArray) model: ('cellValues ' , t2 printString) asSymbol.
	t3 layout: t1 layout.
	t3 alignment: t1 alignment.
	t3 style: t1 style.
	^t3! !

!KRSectionDefinition class publicMethodsFor: 'creation'!

new
	^super new initialize! !

!KRSectionDefinition publicMethodsFor: 'accessing'!

applicationClass
	^applicationClass!

applicationClass: t1 
	t1 ~= applicationClass ifTrue: [self specSelector: self defaultSpecSelector].
	applicationClass := t1.
	^self!

separatePage
	^separatePage!

separatePage: t1 
	separatePage := t1.
	^self!

showFooter
	^showFooter!

showFooter: t1 
	showFooter := t1.
	^self!

showHeader
	^showHeader!

showHeader: t1 
	showHeader := t1.
	^self!

specSelector
	^specSelector!

specSelector: t1 
	specSelector := t1.
	^self! !

!KRSectionDefinition publicMethodsFor: 'initialization-release'!

initialize
	self specSelector: self defaultSpecSelector; showHeader: true; showFooter: true; separatePage: false.
	^self! !

!KRSectionDefinition publicMethodsFor: 'private'!

defaultSpecSelector
	^#reportComponentSpec! !

!KRSectionDefinition publicMethodsFor: 'testing'!

canConstructCanvas
	| t1 t2 |
	(self applicationClass notNil and: [self specSelector notNil])
		ifFalse: [^false].
	self applicationClass isString ifFalse: [^true].
	t1 := self applicationClass asSymbol.
	t2 := self specSelector asSymbol.
	(Smalltalk includesKey: t1)
		ifTrue: [^(Smalltalk at: t1)
				respondsTo: t2].
	^false! !

!KRSimpleReportSection publicMethodsFor: 'aspects'!

aspectFor: t1 
	self dataObject perform: t1.
	^(AspectAdaptor accessWith: t1 assignWith: nil)
		subject: self dataObject! !

!KRTextCellSpec class publicMethodsFor: 'instance creation'!

newNumber
	| t1 |
	(t1 := self new) type: #number.
	t1 alignment: #right.
	^t1!

newString
	| t1 |
	(t1 := self new) type: #string.
	^t1!

newText
	| t1 |
	(t1 := self new) type: #text.
	^t1! !

!KRTextCellSpec publicMethodsFor: 'accessing'!

alignment
	alignment == nil ifTrue: [alignment := #left].
	^alignment!

alignment: t1 
	alignment := t1.
	^self!

extraEmphases
	self extraEmphasis isSymbol ifTrue: [^Array with: self extraEmphasis].
	^self extraEmphasis!

extraEmphasis
	extraEmphasis == nil ifTrue: [extraEmphasis := #transparent].
	^extraEmphasis!

extraEmphasis: t1 
	extraEmphasis := t1.
	^self!

formatString
	^formatString!

formatString: t1 
	formatString := t1.
	^self!

style
	style == nil ifTrue: [style := #small].
	^style!

style: t1 
	style := t1.
	^self!

type
	type == nil ifTrue: [^#object].
	^type!

type: t1 
	type := t1.
	^self! !

!KRTextCellSpec publicMethodsFor: 'applying'!

access: t1 
	| t2 |
	t2 := self printConverter hasSpecialFormating
				ifTrue: [self printConverter formatStringFor: (super access: t1)]
				ifFalse: [(self printConverter printStringFor: (super access: t1)) asText].
	self extraEmphasis == #transparent ifFalse: [t2
			addEmphasis: self extraEmphases
			removeEmphasis: #()
			allowDuplicates: false].
	^t2! !

!KRTextCellSpec publicMethodsFor: 'private'!

defaultPrintConverter
	^UIBuilder defaultPolicy defaultConverterClass for: self type withFormatString: self formatString!

printConverter
	printConverter == nil ifTrue: [printConverter := self defaultPrintConverter].
	^printConverter!

printConverter: t1 
	printConverter := t1.
	^self! !

!KRUnixPrinter publicMethodsFor: 'accessing'!

name
	name == nil ifTrue: [^'The Default Printer'].
	^name!

name: t1 
	name := t1.
	^self!

queueString
	^queueString!

queueString: t1 
	queueString := t1.
	^self! !

KRAccumulator initializeAfterLoad!
KRArithAccumGatheringState initializeAfterLoad!
KRArithmeticAccumulator initializeAfterLoad!
KRFirstLastAccumulator initializeAfterLoad!
KRFLAccumGatheringState initializeAfterLoad!
KRFLAccumInitialState initializeAfterLoad!
KRArithAccumInitialState initializeAfterLoad!
KRArithAccumState initializeAfterLoad!
KRBreakDefinition initializeAfterLoad!
KRCellSpec initializeAfterLoad!
KRTextCellSpec initializeAfterLoad!
KRFLAccumState initializeAfterLoad!
KRLayout initializeAfterLoad!
KRPosition initializeAfterLoad!
KRAbsolutePosition initializeAfterLoad!
KRCompositeRelativePosition initializeAfterLoad!
KRComputedPosition initializeAfterLoad!
KRDynamicPosition initializeAfterLoad!
KRRelativePosition initializeAfterLoad!
KRPrintJob initializeAfterLoad!
KRPrintPager initializeAfterLoad!
KRPrintPagerPreferenceManager initializeAfterLoad!
KRPrintPagerPreferenceMap initializeAfterLoad!
KRPrintPagerPreferences initializeAfterLoad!
KRReportAccumulator initializeAfterLoad!
KRArraySummingAccumulator initializeAfterLoad!
KRReportBreak initializeAfterLoad!
KRReportDefinition initializeAfterLoad!
KRReportWriter initializeAfterLoad!
KRClassComplexityReportWriter initializeAfterLoad!
KRRWApplicationNode initializeAfterLoad!
KRRWReportNodeCreator initializeAfterLoad!
KRRWAggregateNodeCreator initializeAfterLoad!
KRRWBodyNodeCreator initializeAfterLoad!
KRSectionDefinition initializeAfterLoad!
KRUnixPrinter initializeAfterLoad!
KRReport initializeAfterLoad!
KRReportSection initializeAfterLoad!
KRAccumulatingReportSection initializeAfterLoad!
KRRWReportNode initializeAfterLoad!
KRRWAggregateNode initializeAfterLoad!
KRRWBodyNode initializeAfterLoad!
KRSimpleReportSection initializeAfterLoad!
KRPrintJobOptions initializeAfterLoad!
KRONO_Reporter_Core initializeAfterLoad!
KRReportComposite initializeAfterLoad!

KRONO_Reporter_Core loaded!
