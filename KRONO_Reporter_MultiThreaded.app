'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:38:03 pm'!



((KRONO_Reporter createSubApplication: #KRONO_Reporter_MultiThreaded in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

KRONO_Reporter_MultiThreaded becomeDefault!

KRBreakDefinition subclass: #KRMuThBreakDefinition
	instanceVariableNames: 'hasSummaryInfo '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded becomeDefault!

Object subclass: #KRMuThReportBroker
	instanceVariableNames: 'client clientOpened clientOpenedCritical report reportProcessSynchro reportProcess reportProcessFinished reportProcessFinishedCritical pages pagesCritical printJob printJobSynchro printJobProcess printJobProcessFinished printJobProcessFinishedCritical printJobProcessFinishedSynchro maxCachedPages displayClientWindowLabel numberOfPagesSentToPrintJob numberOfPagesSentToPrintJobCritical monitor monitorOpened monitorOpenedCritical '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded becomeDefault!

Object subclass: #KRMuThReportElement
	instanceVariableNames: 'model spec layout section page isPageDivider '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded becomeDefault!

Object subclass: #KRMuThReportPage
	instanceVariableNames: 'pageNumber translation elements missingElements '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded becomeDefault!

Object subclass: #KRMuThReportPageLayout
	instanceVariableNames: 'name unit width height leftMargin topMargin rightMargin bottomMargin noPrintableHeight noPrintableWidth '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded becomeDefault!

KRPrintJob subclass: #KRMuThPrintJob
	instanceVariableNames: 'reportBroker currentPageNumber pageSize printPagerPreferenceMap page '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded becomeDefault!

KRReportBreak subclass: #KRMuThReportBreak
	instanceVariableNames: 'hasSummaryInfo '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded becomeDefault!

KRReportDefinition subclass: #KRMuThReportDefinition
	instanceVariableNames: 'splitBodyOnPageBreak minBodyOrphanHeight displayClientWindowLabel pageLayout '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded becomeDefault!

KRReportWriter subclass: #KRMuThReportWriter
	instanceVariableNames: 'splitBodyOnPageBreak minBodyOrphanHeight displayClientWindowLabel pageLayout '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded becomeDefault!

ApplicationModel subclass: #KRMuThApplicationOrDialogModel
	instanceVariableNames: 'isDialog close accept cancel preBuildBlock postBuildBlock escapeIsCancel returnSelectionHolder dialogLabel postOpenTrigger '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded becomeDefault!

KRMuThApplicationOrDialogModel subclass: #KRMuThReportMonitor
	instanceVariableNames: 'reportBroker statusText oldReportState oldPrintJobState oldReportedNumberOfPages oldPrintingNumberOfPages properlyFinished '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded becomeDefault!

KRMuThApplicationOrDialogModel subclass: #KRMuThReportPageLayoutChooser
	instanceVariableNames: 'initialLayoutName predefinedLayouts customTitle selectedLayout layoutName layoutNameMenu layoutEditor '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded becomeDefault!

ApplicationModel subclass: #KRMuThPageLayoutEditor
	instanceVariableNames: 'pageLayout name unit orientation width height leftMargin topMargin rightMargin bottomMargin noPrintableWidth noPrintableHeight '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded becomeDefault!

ApplicationModel subclass: #KRMuThReportPageDisplayClient
	instanceVariableNames: 'reportDisplayClient pageNumber page '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded becomeDefault!

KRReport subclass: #KRMuThPrintPage
	instanceVariableNames: 'reportPage '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded becomeDefault!

KRReport subclass: #KRMuThReport
	instanceVariableNames: 'reportBroker currentPageNumber currentPage '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded becomeDefault!

METAApplicationOrDialog subclass: #KRMuThReportDisplayClient
	instanceVariableNames: 'reportBroker reportPage numberOfPages currentPageNumber reportViewerMenu displayClientWindowLabel '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded becomeDefault!

SubApplication subclass: #KRONO_Reporter_MultiThreaded
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded becomeDefault!

Depth1Image subclass: #KRMuThDialogPostOpenTrigger
	instanceVariableNames: 'triggerBlock already '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded becomeDefault!

VisualPart subclass: #KRMuThPrintPageVisualPart
	classInstanceVariableNames: 'visualPartClasses newVisualPartClasses '
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded becomeDefault!

!KRMuThApplicationOrDialogModel class publicMethodsFor: 'interface opening'!

openDialog

	^self new openDialog! !

!KRMuThApplicationOrDialogModel class publicMethodsFor: 'interface specs'!

dialogSpec

	^self windowSpec! !

!KRMuThApplicationOrDialogModel class publicMethodsFor: 'preferences'!

preferredDialogPostOpenTriggerClass

	^KRMuThDialogPostOpenTrigger! !

!KRMuThApplicationOrDialogModel publicMethodsFor: 'dlog:accessing'!

bindings

	^builder bindings!

dialogValue

	^close value!

isDialog

	isDialog isNil ifTrue: [isDialog := false].
	^isDialog!

isDialog: t1 

	(isDialog := t1 == true) ifTrue: [self initialize].
	^self!

postBuildBlock: t1 

	postBuildBlock := t1.
	^self!

postOpenTrigger
	| aTrigger  aDialog aTriggerBlock |

	postOpenTrigger isNil ifFalse: [^postOpenTrigger].

	aTrigger := self preferredDialogPostOpenTriggerClass image.
	aTriggerBlock := self isDialog 
		ifFalse: [nil]
		ifTrue: [
			aDialog := self.
			[aDialog doPostOpenActions]].
	aTrigger triggerBlock: aTriggerBlock.
	postOpenTrigger := aTrigger.

	^aTrigger!

preBuildBlock: aBlock
	"Installation of a preBuildBlock will override any preBuildWith: 
	method."

	preBuildBlock := aBlock!

returnSelection: t1 

	returnSelectionHolder value: t1.
	^self!

returnSelectionHolder

	returnSelectionHolder isNil ifTrue: [self initReturnSelectionHolder].
	^returnSelectionHolder!

returnSelectionHolder: t1 

	returnSelectionHolder := t1.
	^self!

source: t1 

	builder source: t1.
	^self!

value

	self isDialog ifTrue: [^self dialogValue].
	^true!

value: t1 

	^close value: t1! !

!KRMuThApplicationOrDialogModel publicMethodsFor: 'dlog:aspects'!

accept

	^accept!

cancel

	^cancel!

closeChannel

	^close!

dialogLabel

	dialogLabel isNil ifTrue: [^dialogLabel := String new asValue].
	^dialogLabel! !

!KRMuThApplicationOrDialogModel publicMethodsFor: 'dlog:interface opening'!

allButOpenFrom: t1 

	preBuildBlock == nil
		ifTrue: [self preBuildWith: builder]
		ifFalse: [preBuildBlock value: self value: builder].
	builder source isNil ifTrue: [builder source: self].
	builder add: t1 window.
	builder add: t1 component.
	builder window model: self.
	escapeIsCancel ifTrue: [builder keyboardProcessor keyboardHook: [:t2 :t3 | self keyboardEvent: t2]].
	accept onChangeSend: #close to: self.
	cancel onChangeSend: #close to: self.
	postBuildBlock == nil
		ifTrue: [self postBuildWith: builder]
		ifFalse: [postBuildBlock value: self value: builder].
	^self!

dialogOpenInterface: t1 

	^self openFor: self interface: t1!

dialogPostOpenWith: t1 

	self shouldNotImplement.
	^self!

doPostOpenActions

	^self!

installPostOpenTriggerWith: t1 

	| t2 |
	t2 := (LabelSpec new) layout: ((LayoutFrame new) topFraction: 0; topOffset: 0; leftFraction: 0; leftOffset: 0; bottomFraction: 1; bottomOffset: 0; rightFraction: 1; rightOffset: 0); hasCharacterOrientedLabel: false; setLabel: self postOpenTrigger.
	t1 add: t2.
	^self!

newDialogControllerWithBuilder: t1 

	^t1 policy dialogWindowControllerClass new!

open

	| t1 |
	super open.
	self isDialog
		ifTrue: 
			[t1 := self accept value.
			self release.
			^t1].
	^self!

openDialogWithBuilder: t1 

	| t2 |
	t2 := (t2 := 0 @ 0 extent: t1 window displayBox extent) align: t2 center with: WindowSensor cursorPoint.
	^self
		openPopUpIn: t2
		type: #imageModalDialog
		withBuilder: t1!

openFor: t1 interface: t2 

	builder source: t1.
	^self openFrom: (t1 class interfaceSpecFor: t2)!

openFrom: t1 

	| t2 |
	self allButOpenFrom: t1.
	self openDialogWithBuilder: builder.
	t2 := self accept value.
	self release.
	^t2!

openInterface: t1 

	self isDialog ifTrue: [^self dialogOpenInterface: t1].
	^super openInterface: t1!

openPopUpIn: t1 type: t2 withBuilder: t3 

	| t4 |
	t3 doFinalHookup.
	t4 := self newDialogControllerWithBuilder: t3.
	t3 window controller: t4.
	t3 window openTransientIn: t1 type: t2.
	^t3 window!

postOpenWith: t1 

	self isDialog ifTrue: [^self dialogPostOpenWith: t1].
	^super postOpenWith: t1! !

!KRMuThApplicationOrDialogModel publicMethodsFor: 'dlog:private'!

close

	builder window model: true.
	builder window checkForEvents.
	^self!

computeExtentAndSetLabel: t1 

	| t2 t3 t4 t5 t6 t7 t8 |
	t2 := builder componentAt: #label.
	t4 := (t3 := ComposedText withText: t1 asText allBold style: (TextAttributes default alignment: 2)) preferredBounds extent.
	t5 := builder window displayBox extent.
	t6 := t2 widget preferredBounds extent.
	t2 label: t3.
	t4 y > t6 y
		ifTrue: [t7 := t5 y + (t4 y - t6 y)]
		ifFalse: [t7 := t5 y].
	^(t8 := t5 x max: t4 x + 20) @ t7!

doCancel

	cancel isNil ifFalse: [cancel value: true].
	^self!

invokePostOpen

	^self!

keyboardEvent: t1 

	t1 keyValue = Character esc
		ifTrue: 
			[self doCancel.
			^nil].
	^t1!

resize

	self builder window controller resize.
	^self!

simulateEscapeKey

	escapeIsCancel == true ifTrue: [self doCancel].
	^self! !

!KRMuThApplicationOrDialogModel publicMethodsFor: 'initialize-release'!

dialogInitialize

	accept := false asValue.
	cancel := false asValue.
	close := false asValue.
	(builder := UIBuilder new) aspectAt: #accept put: accept.
	builder aspectAt: #cancel put: cancel.
	builder aspectAt: #close put: close.
	escapeIsCancel := true.
	^self!

escapeIsCancel: t1 

	escapeIsCancel := t1.
	^self!

initialize

	self isDialog ifTrue: [^self dialogInitialize].
	^super initialize!

initReturnSelectionHolder

	returnSelectionHolder := nil asValue.
	^self!

release

	isDialog := nil.
	close isNil
		ifFalse: 
			[close release.
			close := nil].
	accept isNil
		ifFalse: 
			[accept release.
			accept := nil].
	cancel isNil
		ifFalse: 
			[cancel release.
			cancel := nil].
	preBuildBlock := nil.
	postBuildBlock := nil.
	escapeIsCancel := nil.
	dialogLabel := nil.
	postOpenTrigger := nil.
	^super release! !

!KRMuThApplicationOrDialogModel publicMethodsFor: 'interface opening'!

addNoDialogWidgetsWith: t1 

	^self!

openDialog

	| t1 |
	self isDialog: true.
	self returnSelectionHolder: nil asValue.
	t1 := self openInterface: #dialogSpec.
	^Array with: t1 with: self returnSelectionHolder value!

postBuildWith: t1 

	self isDialog ifFalse: [self addNoDialogWidgetsWith: t1].
	self isDialog ifTrue: [self installPostOpenTriggerWith: t1].
	^self! !

!KRMuThApplicationOrDialogModel publicMethodsFor: 'preferences'!

preferredDialogPostOpenTriggerClass

	^self class preferredDialogPostOpenTriggerClass! !

!KRMuThBreakDefinition publicMethodsFor: 'accessing'!

hasSummaryInfo

	^hasSummaryInfo == true!

hasSummaryInfo: t1 

	hasSummaryInfo := t1 == true.
	^self! !

!KRMuThBreakDefinition publicMethodsFor: 'converting'!

literalArrayEncoding

	| t1 t2 |
	self hasSummaryInfo
		ifFalse: 
			[(t2 := super literalArrayEncoding) at: 1 put: self class superclass name.
			^t2].
	(t1 := OrderedCollection new: 15) add: self class name.
	#(#headerClass #headerSelector #footerClass #footerSelector #level #newPage #selectorCollection #hasSummaryInfo) do: 
		[:t3 | 
		| t4 |
		(t4 := self perform: t3) == nil
			ifTrue: [nil]
			ifFalse: 
				[t1 add: (t3 , ':') asSymbol.
				t1 add: t4 literalArrayEncoding]].
	^t1 asArray! !

!KRMuThBreakDefinition publicMethodsFor: 'initialize-release'!

release

	hasSummaryInfo := nil.
	^super release! !

!KRMuThDialogPostOpenTrigger class publicMethodsFor: 'resources'!

image

	^self new
		extent: 10 @ 10
		depth: 1
		palette: CoveragePalette monoMaskPalette! !

!KRMuThDialogPostOpenTrigger publicMethodsFor: 'accessing'!

triggerBlock: t1 

	triggerBlock := t1.
	^self! !

!KRMuThDialogPostOpenTrigger publicMethodsFor: 'displaying'!

displayOn: t1 at: t2 

	already isNil
		ifTrue: 
			[already := true.
			triggerBlock isNil ifFalse: [triggerBlock value]].
	^self! !

!KRMuThDialogPostOpenTrigger publicMethodsFor: 'initialize-release'!

release

	triggerBlock := nil.
	already := nil.
	^super release! !

!KRMuThPageLayoutEditor class publicMethodsFor: 'examples'!

example1

	| t1 t2 |
	t2 := KRMuThReportPageLayout newLetterNoMarginsPortrait.
	(t1 := KRMuThPageLayoutEditor new) connectPageLayout: t2.
	t1 open.
	^self! !

!KRMuThPageLayoutEditor class publicMethodsFor: 'interface specs'!

customPageWindowSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Unlabeled Canvas' #min: #(#Point 231 233) #bounds: #(#Rectangle 186 157 525 393)) #component: #(#SpecCollection #collection: #(#(#LabelSpec #layout: #(#Rectangle 5 5 60 30) #name: #nameLabelID #label: 'Name') #(#InputFieldSpec #layout: #(#LayoutFrame 60 0 5 0 -5 1 30 0) #name: #nameFieldID #model: #name #isReadOnly: true) #(#LabelSpec #layout: #(#Rectangle 5 35 60 60) #name: #unitLabelID #label: 'Unit') #(#MenuButtonSpec #layout: #(#LayoutFrame 60 0 35 0 0 0.5 60 0) #name: #unitMenuID #isOpaque: true #model: #unit #menu: #unitMenu #isPopUp: false) #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 115 0 60 0.5 140 0) #name: #topMarginLabelID #label: 'Top') #(#MenuButtonSpec #layout: #(#LayoutFrame 60 0.5 35 0 -5 1 60 0) #name: #orientationMenuID #isOpaque: true #model: #orientation #menu: #orientationMenu) #(#LabelSpec #layout: #(#Rectangle 5 65 60 90) #name: #widthLabelID #label: 'Width') #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 65 0 60 0.5 90 0) #name: #heightLabelID #label: 'Height') #(#InputFieldSpec #layout: #(#LayoutFrame 60 0 65 0 0 0.5 90 0) #name: #widthFieldID #model: #width #isReadOnly: true #type: #number) #(#InputFieldSpec #layout: #(#LayoutFrame 60 0.5 65 0 -5 1 90 0) #name: #heightFieldID #model: #height #isReadOnly: true #type: #number) #(#GroupBoxSpec #layout: #(#LayoutFrame 5 0 95 0 -5 1 175 0) #name: #marginsGroupID #label: 'Margins') #(#LabelSpec #layout: #(#Rectangle 10 115 60 140) #name: #leftMarginLabelID #label: 'Left') #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 65 0 60 0.5 90 0) #name: #heightLabelID #label: 'Height') #(#InputFieldSpec #layout: #(#LayoutFrame 60 0 115 0 0 0.5 140 0) #name: #leftMarginFieldID #model: #leftMargin #type: #number) #(#InputFieldSpec #layout: #(#LayoutFrame 60 0.5 115 0 -10 1 140 0) #name: #topMarginFieldID #model: #topMargin #type: #number) #(#LabelSpec #layout: #(#Rectangle 10 145 60 170) #name: #rightMarginLabelID #label: 'Right') #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 145 0 60 0.5 170 0) #name: #bottomMarginLabelID #label: 'Bottom') #(#InputFieldSpec #layout: #(#LayoutFrame 60 0 145 0 0 0.5 170 0) #name: #rightMarginFieldID #model: #rightMargin #type: #number) #(#InputFieldSpec #layout: #(#LayoutFrame 60 0.5 145 0 -10 1 170 0) #name: #bottomMarginFieldID #model: #bottomMargin #type: #number) #(#GroupBoxSpec #layout: #(#LayoutFrame 5 0 180 0 -5 1 230 0) #name: #marginsGroupID #label: 'Non Printable Area (both sides)') #(#LabelSpec #layout: #(#Rectangle 10 200 60 225) #name: #nonPrintableWidthLabelID #label: 'Width') #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 200 0 60 0.5 225 0) #name: #nonPrintableHeightLabelID #label: 'Height') #(#InputFieldSpec #layout: #(#LayoutFrame 60 0 200 0 0 0.5 225 0) #name: #noPrintableWidthFieldID #model: #noPrintableWidth #type: #number) #(#InputFieldSpec #layout: #(#LayoutFrame 60 0.5 200 0 -10 1 225 0) #name: #noPrintableHeightFieldID #model: #noPrintableHeight #type: #number) #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 35 0 60 0.5 60 0) #name: #orientationLabelID #label: 'Orientat.'))))!

noNameCustomPageWindowSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Unlabeled Canvas' #min: #(#Point 334 218) #bounds: #(#Rectangle 386 439 725 675)) #component: #(#SpecCollection #collection: #(#(#LabelSpec #layout: #(#Rectangle 5 5 60 30) #name: #unitLabelID #label: 'Unit') #(#MenuButtonSpec #layout: #(#LayoutFrame 60 0 5 0 0 0.5 30 0) #name: #unitMenuID #isOpaque: true #model: #unit #menu: #unitMenu #isPopUp: false) #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 85 0 60 0.5 110 0) #name: #topMarginLabelID #label: 'Top') #(#MenuButtonSpec #layout: #(#LayoutFrame 60 0.5 5 0 -5 1 30 0) #name: #orientationMenuID #isOpaque: true #model: #orientation #menu: #orientationMenu) #(#LabelSpec #layout: #(#Rectangle 5 35 60 60) #name: #widthLabelID #label: 'Width') #(#InputFieldSpec #layout: #(#LayoutFrame 60 0 35 0 0 0.5 60 0) #name: #widthFieldID #model: #width #type: #number) #(#InputFieldSpec #layout: #(#LayoutFrame 60 0.5 35 0 -5 1 60 0) #name: #heightFieldID #model: #height #type: #number) #(#GroupBoxSpec #layout: #(#LayoutFrame 5 0 65 0 -5 1 150 0) #name: #marginsGroupID #label: 'Margins') #(#LabelSpec #layout: #(#Rectangle 10 85 60 110) #name: #leftMarginLabelID #label: 'Left') #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 35 0 60 0.5 60 0) #name: #heightLabelID #label: 'Height') #(#InputFieldSpec #layout: #(#LayoutFrame 60 0 85 0 0 0.5 110 0) #name: #leftMarginFieldID #model: #leftMargin #type: #number) #(#InputFieldSpec #layout: #(#LayoutFrame 60 0.5 85 0 -10 1 110 0) #name: #topMarginFieldID #model: #topMargin #type: #number) #(#LabelSpec #layout: #(#Rectangle 10 115 60 140) #name: #rightMarginLabelID #label: 'Right') #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 115 0 60 0.5 140 0) #name: #bottomMarginLabelID #label: 'Bottom') #(#InputFieldSpec #layout: #(#LayoutFrame 60 0 115 0 0 0.5 140 0) #name: #rightMarginFieldID #model: #rightMargin #type: #number) #(#InputFieldSpec #layout: #(#LayoutFrame 60 0.5 115 0 -10 1 140 0) #name: #bottomMarginFieldID #model: #bottomMargin #type: #number) #(#GroupBoxSpec #layout: #(#LayoutFrame 5 0 155 0 -5 1 210 0) #name: #marginsGroupID #label: 'Non Printable Area (both sides)') #(#LabelSpec #layout: #(#Rectangle 10 175 60 200) #name: #nonPrintableWidthLabelID #label: 'Width') #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 175 0 60 0.5 200 0) #name: #nonPrintableHeightLabelID #label: 'Height') #(#InputFieldSpec #layout: #(#LayoutFrame 60 0 175 0 0 0.5 200 0) #name: #noPrintableWidthFieldID #model: #noPrintableWidth #type: #number) #(#InputFieldSpec #layout: #(#LayoutFrame 60 0.5 175 0 -10 1 200 0) #name: #noPrintableHeightFieldID #model: #noPrintableHeight #type: #number) #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 5 0 60 0.5 30 0) #name: #orientationLabelID #label: 'Orientat.'))))!

noNameWindowSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Unlabeled Canvas' #min: #(#Point 334 218) #bounds: #(#Rectangle 386 439 725 675)) #component: #(#SpecCollection #collection: #(#(#LabelSpec #layout: #(#Rectangle 5 5 60 30) #name: #unitLabelID #label: 'Unit') #(#MenuButtonSpec #layout: #(#LayoutFrame 60 0 5 0 0 0.5 30 0) #name: #unitMenuID #isOpaque: true #model: #unit #menu: #unitMenu #isPopUp: false) #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 85 0 60 0.5 110 0) #name: #topMarginLabelID #label: 'Top') #(#MenuButtonSpec #layout: #(#LayoutFrame 60 0.5 5 0 -5 1 30 0) #name: #orientationMenuID #isOpaque: true #model: #orientation #menu: #orientationMenu) #(#LabelSpec #layout: #(#Rectangle 5 35 60 60) #name: #widthLabelID #label: 'Width') #(#InputFieldSpec #layout: #(#LayoutFrame 60 0 35 0 0 0.5 60 0) #name: #widthFieldID #model: #width #isReadOnly: true #type: #number) #(#InputFieldSpec #layout: #(#LayoutFrame 60 0.5 35 0 -5 1 60 0) #name: #heightFieldID #model: #height #isReadOnly: true #type: #number) #(#GroupBoxSpec #layout: #(#LayoutFrame 5 0 65 0 -5 1 150 0) #name: #marginsGroupID #label: 'Margins') #(#LabelSpec #layout: #(#Rectangle 10 85 60 110) #name: #leftMarginLabelID #label: 'Left') #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 35 0 60 0.5 60 0) #name: #heightLabelID #label: 'Height') #(#InputFieldSpec #layout: #(#LayoutFrame 60 0 85 0 0 0.5 110 0) #name: #leftMarginFieldID #model: #leftMargin #type: #number) #(#InputFieldSpec #layout: #(#LayoutFrame 60 0.5 85 0 -10 1 110 0) #name: #topMarginFieldID #model: #topMargin #type: #number) #(#LabelSpec #layout: #(#Rectangle 10 115 60 140) #name: #rightMarginLabelID #label: 'Right') #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 115 0 60 0.5 140 0) #name: #bottomMarginLabelID #label: 'Bottom') #(#InputFieldSpec #layout: #(#LayoutFrame 60 0 115 0 0 0.5 140 0) #name: #rightMarginFieldID #model: #rightMargin #type: #number) #(#InputFieldSpec #layout: #(#LayoutFrame 60 0.5 115 0 -10 1 140 0) #name: #bottomMarginFieldID #model: #bottomMargin #type: #number) #(#GroupBoxSpec #layout: #(#LayoutFrame 5 0 155 0 -5 1 210 0) #name: #marginsGroupID #label: 'Non Printable Area (both sides)') #(#LabelSpec #layout: #(#Rectangle 10 175 60 200) #name: #nonPrintableWidthLabelID #label: 'Width') #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 175 0 60 0.5 200 0) #name: #nonPrintableHeightLabelID #label: 'Height') #(#InputFieldSpec #layout: #(#LayoutFrame 60 0 175 0 0 0.5 200 0) #name: #noPrintableWidthFieldID #model: #noPrintableWidth #type: #number) #(#InputFieldSpec #layout: #(#LayoutFrame 60 0.5 175 0 -10 1 200 0) #name: #noPrintableHeightFieldID #model: #noPrintableHeight #type: #number) #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 5 0 60 0.5 30 0) #name: #orientationLabelID #label: 'Orientat.'))))!

windowSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Unlabeled Canvas' #min: #(#Point 231 233) #bounds: #(#Rectangle 193 153 532 389)) #component: #(#SpecCollection #collection: #(#(#LabelSpec #layout: #(#Rectangle 5 5 60 30) #name: #nameLabelID #label: 'Name') #(#InputFieldSpec #layout: #(#LayoutFrame 60 0 5 0 -5 1 30 0) #name: #nameFieldID #model: #name #isReadOnly: true) #(#LabelSpec #layout: #(#Rectangle 5 35 60 60) #name: #unitLabelID #label: 'Unit') #(#MenuButtonSpec #layout: #(#LayoutFrame 60 0 35 0 0 0.5 60 0) #name: #unitMenuID #isOpaque: true #model: #unit #menu: #unitMenu #isPopUp: false) #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 115 0 60 0.5 140 0) #name: #topMarginLabelID #label: 'Top') #(#MenuButtonSpec #layout: #(#LayoutFrame 60 0.5 35 0 -5 1 60 0) #name: #orientationMenuID #isOpaque: true #model: #orientation #menu: #orientationMenu) #(#LabelSpec #layout: #(#Rectangle 5 65 60 90) #name: #widthLabelID #label: 'Width') #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 65 0 60 0.5 90 0) #name: #heightLabelID #label: 'Height') #(#InputFieldSpec #layout: #(#LayoutFrame 60 0 65 0 0 0.5 90 0) #name: #widthFieldID #model: #width #type: #number) #(#InputFieldSpec #layout: #(#LayoutFrame 60 0.5 65 0 -5 1 90 0) #name: #heightFieldID #model: #height #type: #number) #(#GroupBoxSpec #layout: #(#LayoutFrame 5 0 95 0 -5 1 175 0) #name: #marginsGroupID #label: 'Margins') #(#LabelSpec #layout: #(#Rectangle 10 115 60 140) #name: #leftMarginLabelID #label: 'Left') #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 65 0 60 0.5 90 0) #name: #heightLabelID #label: 'Height') #(#InputFieldSpec #layout: #(#LayoutFrame 60 0 115 0 0 0.5 140 0) #name: #leftMarginFieldID #model: #leftMargin #type: #number) #(#InputFieldSpec #layout: #(#LayoutFrame 60 0.5 115 0 -10 1 140 0) #name: #topMarginFieldID #model: #topMargin #type: #number) #(#LabelSpec #layout: #(#Rectangle 10 145 60 170) #name: #rightMarginLabelID #label: 'Right') #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 145 0 60 0.5 170 0) #name: #bottomMarginLabelID #label: 'Bottom') #(#InputFieldSpec #layout: #(#LayoutFrame 60 0 145 0 0 0.5 170 0) #name: #rightMarginFieldID #model: #rightMargin #type: #number) #(#InputFieldSpec #layout: #(#LayoutFrame 60 0.5 145 0 -10 1 170 0) #name: #bottomMarginFieldID #model: #bottomMargin #type: #number) #(#GroupBoxSpec #layout: #(#LayoutFrame 5 0 180 0 -5 1 230 0) #name: #marginsGroupID #label: 'Non Printable Area (both sides)') #(#LabelSpec #layout: #(#Rectangle 10 200 60 225) #name: #nonPrintableWidthLabelID #label: 'Width') #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 200 0 60 0.5 225 0) #name: #nonPrintableHeightLabelID #label: 'Height') #(#InputFieldSpec #layout: #(#LayoutFrame 60 0 200 0 0 0.5 225 0) #name: #noPrintableWidthFieldID #model: #noPrintableWidth #type: #number) #(#InputFieldSpec #layout: #(#LayoutFrame 60 0.5 200 0 -10 1 225 0) #name: #noPrintableHeightFieldID #model: #noPrintableHeight #type: #number) #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 35 0 60 0.5 60 0) #name: #orientationLabelID #label: 'Orientat.'))))!

xnoNameCustomPageWindowSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Unlabeled Canvas' #min: #(#Point 231 233) #bounds: #(#Rectangle 186 157 525 393)) #component: #(#SpecCollection #collection: #(#(#LabelSpec #layout: #(#Rectangle 5 35 60 60) #name: #unitLabelID #label: 'Unit') #(#MenuButtonSpec #layout: #(#LayoutFrame 60 0 35 0 0 0.5 60 0) #name: #unitMenuID #isOpaque: true #model: #unit #menu: #unitMenu #isPopUp: false) #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 115 0 60 0.5 140 0) #name: #topMarginLabelID #label: 'Top') #(#MenuButtonSpec #layout: #(#LayoutFrame 60 0.5 35 0 -5 1 60 0) #name: #orientationMenuID #isOpaque: true #model: #orientation #menu: #orientationMenu) #(#LabelSpec #layout: #(#Rectangle 5 65 60 90) #name: #widthLabelID #label: 'Width') #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 65 0 60 0.5 90 0) #name: #heightLabelID #label: 'Height') #(#InputFieldSpec #layout: #(#LayoutFrame 60 0 65 0 0 0.5 90 0) #name: #widthFieldID #model: #width #isReadOnly: true #type: #number) #(#InputFieldSpec #layout: #(#LayoutFrame 60 0.5 65 0 -5 1 90 0) #name: #heightFieldID #model: #height #isReadOnly: true #type: #number) #(#GroupBoxSpec #layout: #(#LayoutFrame 5 0 95 0 -5 1 175 0) #name: #marginsGroupID #label: 'Margins') #(#LabelSpec #layout: #(#Rectangle 10 115 60 140) #name: #leftMarginLabelID #label: 'Left') #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 65 0 60 0.5 90 0) #name: #heightLabelID #label: 'Height') #(#InputFieldSpec #layout: #(#LayoutFrame 60 0 115 0 0 0.5 140 0) #name: #leftMarginFieldID #model: #leftMargin #type: #number) #(#InputFieldSpec #layout: #(#LayoutFrame 60 0.5 115 0 -10 1 140 0) #name: #topMarginFieldID #model: #topMargin #type: #number) #(#LabelSpec #layout: #(#Rectangle 10 145 60 170) #name: #rightMarginLabelID #label: 'Right') #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 145 0 60 0.5 170 0) #name: #bottomMarginLabelID #label: 'Bottom') #(#InputFieldSpec #layout: #(#LayoutFrame 60 0 145 0 0 0.5 170 0) #name: #rightMarginFieldID #model: #rightMargin #type: #number) #(#InputFieldSpec #layout: #(#LayoutFrame 60 0.5 145 0 -10 1 170 0) #name: #bottomMarginFieldID #model: #bottomMargin #type: #number) #(#GroupBoxSpec #layout: #(#LayoutFrame 5 0 180 0 -5 1 230 0) #name: #marginsGroupID #label: 'Non Printable Area (both sides)') #(#LabelSpec #layout: #(#Rectangle 10 200 60 225) #name: #nonPrintableWidthLabelID #label: 'Width') #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 200 0 60 0.5 225 0) #name: #nonPrintableHeightLabelID #label: 'Height') #(#InputFieldSpec #layout: #(#LayoutFrame 60 0 200 0 0 0.5 225 0) #name: #noPrintableWidthFieldID #model: #noPrintableWidth #type: #number) #(#InputFieldSpec #layout: #(#LayoutFrame 60 0.5 200 0 -10 1 225 0) #name: #noPrintableHeightFieldID #model: #noPrintableHeight #type: #number) #(#LabelSpec #layout: #(#LayoutFrame 5 0.5 35 0 60 0.5 60 0) #name: #orientationLabelID #label: 'Orientat.'))))! !

!KRMuThPageLayoutEditor class publicMethodsFor: 'resources'!

orientationMenu

	| t1 |
	t1 := KRMuThReportPageLayout allOrientationSymbols.
	^Menu
		labelArray: t1
		lines: #()
		values: t1!

unitMenu

	| t1 |
	t1 := KRMuThReportPageLayout allUnitSymbols.
	^Menu
		labelArray: t1
		lines: #()
		values: t1! !

!KRMuThPageLayoutEditor publicMethodsFor: 'aspects'!

bottomMargin

	bottomMargin isNil ifTrue: [self initBottomMargin].
	^bottomMargin!

height

	height isNil ifTrue: [self initHeight].
	^height!

leftMargin

	leftMargin isNil ifTrue: [self initLeftMargin].
	^leftMargin!

name

	name isNil ifTrue: [self initName].
	^name!

noPrintableHeight

	noPrintableHeight isNil ifTrue: [self initNoPrintableHeight].
	^noPrintableHeight!

noPrintableWidth

	noPrintableWidth isNil ifTrue: [self initNoPrintableWidth].
	^noPrintableWidth!

orientation

	orientation isNil ifTrue: [self initOrientation].
	^orientation!

pageLayout

	pageLayout isNil ifTrue: [self initPageLayout].
	^pageLayout!

rightMargin

	rightMargin isNil ifTrue: [self initRightMargin].
	^rightMargin!

topMargin

	topMargin isNil ifTrue: [self initTopMargin].
	^topMargin!

unit

	unit isNil ifTrue: [self initUnit].
	^unit!

width

	width isNil ifTrue: [self initWidth].
	^width! !

!KRMuThPageLayoutEditor publicMethodsFor: 'connecting'!

connectPageLayout: t1 

	self pageLayout value: t1.
	^self!

disconnect

	pageLayout := nil.
	^self release! !

!KRMuThPageLayoutEditor publicMethodsFor: 'initialize-release'!

initBottomMargin

	(bottomMargin := AspectAdaptor subjectChannel: self pageLayout) forAspect: #bottomMargin.
	bottomMargin subjectSendsUpdates: true.
	^bottomMargin!

initHeight

	(height := AspectAdaptor subjectChannel: self pageLayout) forAspect: #height.
	height subjectSendsUpdates: true.
	^height!

initLeftMargin

	(leftMargin := AspectAdaptor subjectChannel: self pageLayout) forAspect: #leftMargin.
	leftMargin subjectSendsUpdates: true.
	^leftMargin!

initName

	(name := AspectAdaptor subjectChannel: self pageLayout) forAspect: #name.
	name subjectSendsUpdates: true.
	^name!

initNoPrintableHeight

	(noPrintableHeight := AspectAdaptor subjectChannel: self pageLayout) forAspect: #noPrintableHeight.
	noPrintableHeight subjectSendsUpdates: true.
	^noPrintableHeight!

initNoPrintableWidth

	(noPrintableWidth := AspectAdaptor subjectChannel: self pageLayout) forAspect: #noPrintableWidth.
	noPrintableWidth subjectSendsUpdates: true.
	^noPrintableWidth!

initOrientation

	(orientation := AspectAdaptor subjectChannel: self pageLayout) forAspect: #orientation.
	orientation subjectSendsUpdates: true.
	^orientation!

initPageLayout

	pageLayout := nil asValue.
	^self!

initRightMargin

	(rightMargin := AspectAdaptor subjectChannel: self pageLayout) forAspect: #rightMargin.
	rightMargin subjectSendsUpdates: true.
	^rightMargin!

initTopMargin

	(topMargin := AspectAdaptor subjectChannel: self pageLayout) forAspect: #topMargin.
	topMargin subjectSendsUpdates: true.
	^topMargin!

initUnit

	(unit := AspectAdaptor subjectChannel: self pageLayout) forAspect: #unit.
	unit subjectSendsUpdates: true.
	^unit!

initWidth

	(width := AspectAdaptor subjectChannel: self pageLayout) forAspect: #width.
	width subjectSendsUpdates: true.
	^width!

release

	self releaseParts.
	pageLayout := nil.
	name := nil.
	unit := nil.
	orientation := nil.
	width := nil.
	height := nil.
	leftMargin := nil.
	topMargin := nil.
	rightMargin := nil.
	bottomMargin := nil.
	noPrintableWidth := nil.
	noPrintableHeight := nil.
	^super release!

releaseParts

	pageLayout isNil
		ifFalse: 
			[pageLayout release.
			pageLayout := nil].
	name isNil
		ifFalse: 
			[name retractInterestsFor: self.
			name release.
			name := nil].
	unit isNil
		ifFalse: 
			[unit retractInterestsFor: self.
			unit release.
			unit := nil].
	orientation isNil
		ifFalse: 
			[orientation retractInterestsFor: self.
			orientation release.
			orientation := nil].
	width isNil
		ifFalse: 
			[width retractInterestsFor: self.
			width release.
			width := nil].
	height isNil
		ifFalse: 
			[height retractInterestsFor: self.
			height release.
			height := nil].
	leftMargin isNil
		ifFalse: 
			[leftMargin retractInterestsFor: self.
			leftMargin release.
			leftMargin := nil].
	topMargin isNil
		ifFalse: 
			[topMargin retractInterestsFor: self.
			topMargin release.
			topMargin := nil].
	rightMargin isNil
		ifFalse: 
			[rightMargin retractInterestsFor: self.
			rightMargin release.
			rightMargin := nil].
	bottomMargin isNil
		ifFalse: 
			[bottomMargin retractInterestsFor: self.
			bottomMargin release.
			bottomMargin := nil].
	noPrintableWidth isNil
		ifFalse: 
			[noPrintableWidth retractInterestsFor: self.
			noPrintableWidth release.
			noPrintableWidth := nil].
	noPrintableHeight isNil
		ifFalse: 
			[noPrintableHeight retractInterestsFor: self.
			noPrintableHeight release.
			noPrintableHeight := nil].
	^self! !

!KRMuThPageLayoutEditor publicMethodsFor: 'updating'!

changeRequest

	| t1 |
	t1 := super changeRequest.
	self release.
	^t1! !

!KRMuThPrintJob class publicMethodsFor: 'instance creation'!

newForReportBroker: t1 

	| t2 |
	(t2 := self new) reportBroker: t1.
	^t2! !

!KRMuThPrintJob class publicMethodsFor: 'preferences'!

preferredPrintPageClass

	^KRMuThPrintPage! !

!KRMuThPrintJob publicMethodsFor: 'accessing'!

currentPageNumber

	currentPageNumber isNil ifTrue: [self initCurrentPageNumber].
	^currentPageNumber!

page

	^page!

page: t1 

	page := t1.
	^self!

pageSize

	^pageSize!

pageSize: t1 

	pageSize := t1.
	^self!

printPagerPreferenceMap

	^printPagerPreferenceMap!

printPagerPreferenceMap: t1 

	printPagerPreferenceMap := t1.
	^self!

reportBroker

	^reportBroker!

reportBroker: t1 

	reportBroker := t1.
	^self! !

!KRMuThPrintJob publicMethodsFor: 'broker services'!

getNextPage

	| t1 |
	(t1 := self advancePageNumber) <= self endPage
		ifTrue: [(page := self reportBroker
						rqstWaitAndGetPageNumbered: t1
						from: self
						process: Processor activeProcess) == #EndOfReport ifTrue: [page := nil]]
		ifFalse: [page := nil].
	^page!

informHasFinished

	self reportBroker isNil ifTrue: [^nil].
	^self reportBroker informPrintJobProcessHasFinishedFrom: self process: Processor activeProcess! !

!KRMuThPrintJob publicMethodsFor: 'hardcopy'!

advancePageNumber

	^currentPageNumber := self currentPageNumber + 1!

elementsInCurrentPage

	page isNil ifTrue: [^nil].
	^page elements!

execute

	| t1 |
	
	[KRMuThPrintPageVisualPart initVisualPartClasses.
	file file lineEndTransparent.
	t1 := file file stream name.
	file graphicsDevice withHardcopyPreferencesDo: [self hardcopyLoop].
	(Smalltalk at: #Browser ifAbsent: [nil]) isNil
		ifTrue: [self file print]
		ifFalse: [(Dialog confirm: 'DVPT eyes ONLY !!!!!! Do you want to send report file ' , t1 , ' to printer ?' initialAnswer: false)
				ifTrue: [self file print]
				ifFalse: [Transcript show: 'DVPT eyes ONLY !!!!!! IMAGINE that file ' , t1 , ' has been sent to printer'; cr]]]
		valueNowOrOnUnwindDo: [file isNil
				ifTrue: [nil]
				ifFalse: [file isOpen
						ifTrue: [file close]
						ifFalse: [nil]]].
	self file: nil.
	^self!

hardcopyComponents: t1 in: t2 translatedBy: t3 scaledBy: t4 

	| t5 |
	t5 := self pagerForPageSize: t2.
	self file setLandscape: t5 landscape.
	t5
		physicalPagesDo: 
			[:t6 :t7 | 
			t1 do: [:t8 | self
					hardcopy: t8
					at: t3 + t6
					scaledBy: t4 * t7].
			self file newPage.
			self incrementCurrentPage].
	^self!

hardcopyLoop

	| t1 t2 |
	t1 := 72.0 / 75.0.
	t2 := self pageSize.
	
	[Processor yield.
	self hardcopyNextPageWithPageSize: t2 andScale: t1] whileTrue.
	^self!

hardcopyNextPageWithPageSize: t1 andScale: t2 

	| t3 t4 t5 t6 |
	self getNextPage.
	(t3 := self elementsInCurrentPage) isNil ifTrue: [^false].
	t4 := self translationForCurrentPage.
	(t5 := self preferredPrintPageClass new) reportPage: page.
	t6 := t5 addElements: t3 withTranslation: t4.
	self
		hardcopyComponents: t6
		in: t1
		translatedBy: 0 @ 0
		scaledBy: t2.
	^true!

translationForCurrentPage

	page isNil ifTrue: [^nil].
	^page translation! !

!KRMuThPrintJob publicMethodsFor: 'initialize-release'!

initCurrentPageNumber

	self startPage isNil ifTrue: [self startPage: 1].
	self endPage isNil ifTrue: [self endPage: 1000000].
	currentPageNumber := self startPage - 1.
	^self!

release

	reportBroker := nil.
	currentPageNumber := nil.
	pageSize := nil.
	printPagerPreferenceMap := nil.
	page := nil.
	^super release! !

!KRMuThPrintJob publicMethodsFor: 'pager generation'!

pagerForPageSize: t1 

	^self pagerDictionary at: t1
		ifAbsentPut: 
			[| t2 |
			(t2 := self preferenceManager) physicalPageSize: self physicalPageSize - (0.5 @ 0.5) * Screen default resolution.
			t2 logicalPageSize: t1.
			t2 documentPreferenceMap: self printPagerPreferenceMap.
			t2 bestPrintPager]! !

!KRMuThPrintJob publicMethodsFor: 'preferences'!

preferredPrintPageClass

	^self class preferredPrintPageClass! !

!KRMuThPrintPage publicMethodsFor: 'accessing'!

page

	^self!

reportPage

	^reportPage!

reportPage: t1 

	reportPage := t1.
	^self! !

!KRMuThPrintPage publicMethodsFor: 'hardcopy'!

addElements: t1 withTranslation: t2 

	| t3 t4 |
	t1 isNil ifTrue: [^nil].
	t2 isNil ifTrue: [^nil].
	t3 := OrderedCollection new: t1 size.
	(1 to: t1 size)
		collect: 
			[:t5 | 
			| t6 t7 t8 |
			(t6 := t1 at: t5) model isNil
				ifTrue: [t6 isPageDivider
						ifTrue: [nil]
						ifFalse: [(t8 := self addWidget: t6 translated: t2) isNil
								ifTrue: [nil]
								ifFalse: [t3 add: t8]]]
				ifFalse: 
					[t7 := t5 = t1 size
								ifTrue: [nil]
								ifFalse: [t1 at: t5 + 1].
					(t8 := self
								addSubCanvas: t6
								number: t5
								translated: t2
								nextElement: t7) isNil
						ifTrue: [nil]
						ifFalse: [t3 add: t8]]].
	(t4 := self topComponent copy) container: nil.
	t4 bounds: (0 @ 0 extent: self pageSize y @ self topComponent preferredBounds height).
	self changeClassOfVisualParts.
	^t3!

addSubCanvas: t1 number: t2 translated: t3 nextElement: t4 

	| t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 |
	t5 := t1 layout.
	t8 := t1 section.
	t10 := t1 model.
	t12 := t8 at: #myInterfaceSpecSelector.
	t7 := t4 notNil and: [t4 model notNil and: [t4 model class = t1 model class and: [(t1 section at: #myInterfaceSpecSelector)
							= (t4 section at: #myInterfaceSpecSelector) and: [t4 layout topFraction = t5 bottomFraction and: [t4 layout topOffset = t5 bottomOffset]]]]].
	t6 := self
				copyTranslateLayout: t5
				by: t3
				expandBottom: t7 not.
	(t9 := self builder newSubBuilder) source: t10; composite: self topComponent.
	t10 builder: t9.
	t14 := true.
	t13 := Object messageNotUnderstoodSignal
				handle: 
					[:t16 | 
					t14 := false.
					t16 returnWith: (t10 class interfaceSpecFor: t12)]
				do: [t10 perform: t12].
	t14 ifTrue: [t9
			subCanvasAt: t10 class name
			at: t12
			put: t13].
	(t11 := (SubCanvasSpec new) majorKey: t10 class name; minorKey: t12) colors: t13 window colors; hasBorder: t13 window hasBorder.
	t11 layout: t6.
	t9 add: t11.
	t15 := t9 wrapper.
	t10 postBuildWith: t9.
	^t15!

addWidget: t1 translated: t2 

	| t3 t4 t5 t6 |
	(t6 := self builder newSubBuilder) source: nil; composite: self topComponent.
	t3 := t1 spec.
	t4 := t1 layout.
	t5 := self copyTranslateLayout: t4 by: t2.
	t3 layout: t5.
	t6 add: t3.
	^t6 wrapper!

copyTranslateLayout: t1 by: t2 

	^self
		copyTranslateLayout: t1
		by: t2
		expandBottom: false!

copyTranslateLayout: t1 by: t2 expandBottom: t3 

	| t4 |
	t1 isNil ifTrue: [^nil].
	t2 isNil ifTrue: [^t1].
	(t4 := t1 copy) leftOffset: t1 leftOffset + t2 x.
	t4 topOffset: t1 topOffset + t2 y.
	t4 rightOffset: t1 rightOffset + t2 x.
	t4 bottomOffset: t1 bottomOffset + t2 y + (t3
				ifTrue: [1]
				ifFalse: [0]).
	^t4!

reportBounds

	^self pageSize! !

!KRMuThPrintPage publicMethodsFor: 'initialilze-release'!

release

	reportPage := nil.
	^super release! !

!KRMuThPrintPage publicMethodsFor: 'optimize'!

changeClassOfVisualParts

	KRMuThPrintPageVisualPart changeClassOf: self topComponent.
	^self! !

!KRMuThPrintPageVisualPart class publicMethodsFor: 'optimized'!

changeClassOf: t1 

	| t2 t3 |
	t2 := t1 class.
	(self newVisualPartClasses includesKey: t2 name)
		ifTrue: [^t1].
	(t3 := self visualPartClasses at: t2 ifAbsent: [t3 := self produceClassAndInstanceForClass: t2]) isNil ifTrue: [^nil].
	t1 changeClassToThatOf: t3.
	^t1!

initVisualPartClasses

	visualPartClasses := IdentityDictionary new.
	newVisualPartClasses := IdentityDictionary new.
	^self!

newVisualPartClasses

	newVisualPartClasses isNil ifTrue: [self initVisualPartClasses].
	^newVisualPartClasses!

produceClassAndInstanceForClass: t1 

	| t2 t3 t4 t5 |
	t1 isNil ifTrue: [^nil].
	(t2 := t1 copy) setName: (t1 name , 'CopyToOptimizeRW') asSymbol.
	t2 superclass: t1.
	self newVisualPartClasses at: t2 name put: 0.
	(t4 := self findSelector: #isOpen) isNil ifTrue: [^nil].
	t5 := t4 at: 2.
	t2 addSelector: #isOpen withMethod: t5.
	t3 := t2 new.
	visualPartClasses at: t1 put: t3.
	^t3!

sumNewVisualPartClasses

	^self newVisualPartClasses values inject: 0 into: [:t1 :t2 | t1 + t2]!

visualPartClasses

	visualPartClasses isNil ifTrue: [self initVisualPartClasses].
	^visualPartClasses! !

!KRMuThPrintPageVisualPart publicMethodsFor: 'testing-optimized'!

isOpen

	| t1 |
	((t1 := thisContext sender receiver) isKindOf: VisualPart)
		ifTrue: [KRMuThPrintPageVisualPart changeClassOf: t1].
	^false! !

!KRMuThReport class publicMethodsFor: 'preferences'!

preferredReportPageClass

	^KRMuThReportPage! !

!KRMuThReport publicMethodsFor: 'accessing'!

currentPage

	currentPage isNil ifTrue: [self initCurrentPage].
	^currentPage!

currentPageNumber

	currentPageNumber isNil ifTrue: [self initCurrentPageNumber].
	^currentPageNumber!

reportBroker

	^reportBroker!

reportBroker: t1 

	reportBroker := t1.
	^self!

translationForPageNumber: t1 

	t1 isNil ifTrue: [^0 @ 0].
	t1 < 1 ifTrue: [^0 @ 0].
	^(0 @ (self pageSize y * (t1 - 1))) negated! !

!KRMuThReport publicMethodsFor: 'broker services'!

addMissingElement: t1 toPageNumbered: t2 

	^self reportBroker
		rqstAddMissingElement: t1
		toPageNumbered: t2
		from: self
		process: Processor activeProcess!

deliverPage

	^self reportBroker
		rqstAddPage: self currentPage
		from: self
		process: Processor activeProcess!

informHasFinished

	self reportBroker isNil ifTrue: [^nil].
	^self reportBroker informReportProcessHasFinishedFrom: self process: Processor activeProcess! !

!KRMuThReport publicMethodsFor: 'initialize-release'!

initCurrentPage

	(currentPage := self preferredReportPageClass new) pageNumber: self currentPageNumber.
	currentPage translation: (self translationForPageNumber: currentPageNumber).
	^self!

initCurrentPageNumber

	currentPageNumber := 1.
	^self!

release

	reportBroker := nil.
	currentPage := nil.
	currentPageNumber := nil.
	^super release! !

!KRMuThReport publicMethodsFor: 'preferences'!

preferredMuThReportPageClass

	^self class preferredMuThReportPageClass!

preferredReportPageClass

	^self class preferredReportPageClass! !

!KRMuThReport publicMethodsFor: 'printing'!

addElement: t1 wasMissing: t2 

	| t3 t4 |
	((t4 := ((t3 := t1 model) notNil and: [t3 pageNumber notNil])
				ifTrue: [t3 pageNumber value]
				ifFalse: [nil]) isNil or: [t4 < 1 or: [t4 = self currentPageNumber]])
		ifTrue: [^self currentPage addElement: t1 wasMissing: t2].
	^self addMissingElement: t1 toPageNumbered: t4!

incrementPageNumber

	currentPageNumber := self currentPageNumber + 1.
	^self!

oneMoreMissingElement

	self currentPage oneMoreMissingElement.
	^self!

terminateCurrentPage

	self deliverPage.
	self incrementPageNumber.
	self initCurrentPage.
	^self!

terminateReport

	self currentPage elements isEmpty ifFalse: [self terminateCurrentPage].
	self informHasFinished.
	^self! !

!KRMuThReportBreak publicMethodsFor: 'accessing'!

hasSummaryInfo

	^hasSummaryInfo == true!

hasSummaryInfo: t1 

	hasSummaryInfo := t1 == true.
	^self! !

!KRMuThReportBreak publicMethodsFor: 'initialize-release'!

release

	hasSummaryInfo := nil.
	^super release! !

!KRMuThReportBroker class publicMethodsFor: 'instance creation'!

newForReport: t1 withBlock: t2 displayClientWindowLabel: t3 

	| t4 |
	^(t4 := self new initialize)
		forReport: t1
		withBlock: t2
		displayClientWindowLabel: t3!

newForReport: t1 withBlock: t2 displayClientWindowLabel: t3 from: t4 

	| t5 |
	^(t5 := self new initialize)
		forReport: t1
		withBlock: t2
		displayClientWindowLabel: t3
		from: t4! !

!KRMuThReportBroker class publicMethodsFor: 'preferences'!

preferredPrintJobClass

	^KRMuThPrintJob!

preferredReportDisplayClientClass

	^KRMuThReportDisplayClient!

preferredReportMonitorClass

	^KRMuThReportMonitor! !

!KRMuThReportBroker publicMethodsFor: 'accessing'!

client: t1 

	client := t1.
	^self!

displayClientWindowLabel

	displayClientWindowLabel isNil ifTrue: [self initDisplayClientWindowLabel].
	^displayClientWindowLabel!

displayClientWindowLabel: t1 

	displayClientWindowLabel := t1.
	^self!

maxCachedPages

	maxCachedPages isNil ifTrue: [^3].
	^maxCachedPages!

monitor: t1 

	monitor := t1.
	^self!

report: t1 

	report := t1.
	^self!

reportProcess: t1 

	reportProcess := t1.
	^self! !

!KRMuThReportBroker publicMethodsFor: 'broker services-client'!

informDisplayClientHasClosedFrom: t1 process: t2 

	(self canServiceClient: t1 process: t2)
		ifFalse: [^nil].
	clientOpenedCritical critical: [clientOpened := false].
	(self printJobProcessFinished and: [self reportProcessFinished not])
		ifTrue: [self terminateReportProcess].
	(self clientIsOpened not and: [self printJobProcessFinished and: [self reportProcessFinished]])
		ifTrue: [self release].
	^self!

informDisplayClientHasOpenedFrom: t1 process: t2 

	(self canServiceClient: t1 process: t2)
		ifFalse: [^nil].
	clientOpenedCritical critical: [clientOpened := true].
	reportProcessSynchro signal.
	^self!

rqstNumberOfPagesAllowIncomplete: t1 from: t2 process: t3 

	(self canServiceClient: t2 process: t3)
		ifFalse: [^nil].
	t1 == true ifTrue: [^pagesCritical critical: [pages size]].
	^self numberOfReadyPages!

rqstPageNumbered: t1 allowIncomplete: t2 from: t3 process: t4 

	(self canServiceClient: t3 process: t4)
		ifFalse: [^nil].
	t1 isNil ifTrue: [^nil].
	t1 < 1 ifTrue: [^nil].
	pagesCritical
		critical: 
			[| t5 |
			t1 > pages size ifTrue: [^nil].
			(t5 := pages at: t1) hasMissingElements ifFalse: [^t5].
			t2 == true ifTrue: [^t5 copy].
			nil].
	^nil!

rqstPrintJobStateFrom: t1 process: t2 

	(self canServiceClient: t1 process: t2)
		ifFalse: [^nil].
	printJob notNil
		ifTrue: 
			[printJobProcess notNil ifTrue: [^(self printJobProcessFinished == true) not].
			^false].
	^false!

rqstPrintReportUsingDialog: t1 from: t2 process: t3 

	(self canServiceClient: t2 process: t3)
		ifFalse: [^nil].
	self printReportUsingDialog: t1.
	^self!

rqstReportStateFrom: t1 process: t2 

	(self canServiceClient: t1 process: t2)
		ifFalse: [^nil].
	report notNil
		ifTrue: 
			[reportProcess notNil ifTrue: [^(self reportProcessFinished == true) not].
			^false].
	^false!

rqstStopPrintJobFrom: t1 process: t2 

	(self canServiceClient: t1 process: t2)
		ifFalse: [^nil].
	self terminatePrintJobProcess.
	self allowProceedReport.
	^self!

rqstStopReportFrom: t1 process: t2 

	(self canServiceClient: t1 process: t2)
		ifFalse: [^nil].
	self terminateReportProcess.
	printJobSynchro signal.
	^self! !

!KRMuThReportBroker publicMethodsFor: 'broker services-client-private'!

canServiceClient: t1 process: t2 

	^true!

clientIsOpened

	^clientOpenedCritical critical: [clientOpened == true]!

numberOfReadyPages

	| t1 |
	t1 := 0.
	pagesCritical critical: [pages do: [:t2 | t2 notNil
				ifTrue: 
					[t2 hasMissingElements ifTrue: [^t1].
					t1 := t1 + 1]
				ifFalse: [nil]]].
	^t1! !

!KRMuThReportBroker publicMethodsFor: 'broker services-monitor'!

informMonitorHasClosedFrom: t1 process: t2 

	(self canServiceMonitor: t1 process: t2)
		ifFalse: [^nil].
	monitorOpenedCritical critical: [monitorOpened := false].
	^self!

informMonitorHasOpenedFrom: t1 process: t2 

	(self canServiceMonitor: t1 process: t2)
		ifFalse: [^nil].
	monitorOpenedCritical critical: [monitorOpened := true].
	^self!

rqstNumberOfCompletedPagesFrom: t1 process: t2 

	(self canServiceMonitor: t1 process: t2)
		ifFalse: [^nil].
	^self numberOfCompletedPages!

rqstNumberOfPagesSentToPrintJobFrom: t1 process: t2 

	(self canServiceMonitor: t1 process: t2)
		ifFalse: [^nil].
	^numberOfPagesSentToPrintJob! !

!KRMuThReportBroker publicMethodsFor: 'broker services-monitor-private'!

canServiceMonitor: t1 process: t2 

	^true!

monitorIsOpened

	^monitorOpenedCritical critical: [monitorOpened == true]!

numberOfCompletedPages

	| t1 |
	t1 := 0.
	pagesCritical
		critical: 
			[(1 to: pages size)
				do: 
					[:t2 | 
					| t3 |
					(t3 := pages at: t2) notNil
						ifTrue: 
							[t3 hasMissingElements ifTrue: [^t1].
							t1 := t2]
						ifFalse: [nil]].
			t1 := t1 = 0
						ifTrue: [pages size]
						ifFalse: [t1]].
	^t1! !

!KRMuThReportBroker publicMethodsFor: 'broker services-notification'!

allowProceedPrintJob

	printJobSynchro signal.
	^self!

allowProceedReport

	reportProcessSynchro signal.
	^self!

eventToClient

	| t1 t2 t3 |
	t1 := self clientIsOpened.
	t3 := self monitorIsOpened.
	t1
		ifTrue: 
			[(t2 := PseudoEvent new) setTime: Time now.
			client builder window sensor addMetaInput: t2.
			client builder window becomeActive.
			t3 ifFalse: [client builder window isOpen ifTrue: [client builder window raise]]].
	^self!

eventToMonitor

	| t1 t2 |
	(t1 := self monitorIsOpened)
		ifTrue: 
			[(t2 := PseudoEvent new) setTime: Time now.
			monitor builder window sensor addMetaInput: t2.
			monitor builder window becomeActive.
			monitor builder window isOpen ifTrue: [monitor builder window raise]].
	^self!

notifyPageAdded

	self allowProceedPrintJob.
	^self!

notifyPageCompleted

	self allowProceedPrintJob.
	^self!

notifyPageConsumed

	self allowProceedReport.
	^self! !

!KRMuThReportBroker publicMethodsFor: 'broker services-printJob'!

informPrintJobProcessHasFinishedFrom: t1 process: t2 

	(self canServicePrintJob: t1 process: t2)
		ifFalse: [^nil].
	printJobProcessFinishedCritical critical: [printJobProcessFinished := true].
	printJobProcess := nil.
	printJobProcessFinishedSynchro signal.
	(self clientIsOpened not and: [self reportProcessFinished not])
		ifTrue: [self terminateReportProcess].
	self eventToClient.
	self eventToMonitor.
	(self clientIsOpened not and: [self printJobProcessFinished and: [self reportProcessFinished]])
		ifTrue: [self release].
	^self!

rqstWaitAndGetPageNumbered: t1 from: t2 process: t3 

	| t4 t5 |
	(self canServicePrintJob: t2 process: t3)
		ifFalse: [^nil].
	t1 isNil ifTrue: [^nil].
	t1 < 1 ifTrue: [^nil].
	[true]
		whileTrue: 
			[t4 := false.
			t5 := self clientIsOpened.
			[pagesCritical critical: 
				[| t6 t7 t8 t9 |
				t1 > pages size
					ifTrue: 
						[self reportProcessFinished ifTrue: [^#EndOfReport].
						nil]
					ifFalse: 
						[(t6 := pages at: t1) isNil ifTrue: [^nil].
						t6 hasMissingElements
							ifTrue: [nil]
							ifFalse: 
								[t4 := true.
								t5
									ifFalse: 
										[pages at: t1 put: nil.
										t7 := 1.
										t8 := t1 - 1.
										[t7 <= t8]
											whileTrue: 
												[(t9 := pages at: t7) isNil ifFalse: [t9 release].
												t7 := t7 + 1]].
								numberOfPagesSentToPrintJob := numberOfPagesSentToPrintJob + 1.
								^t6]]]]
				valueOnUnwindDo: [t4
						ifTrue: 
							[self notifyPageConsumed.
							self eventToClient.
							self eventToMonitor]
						ifFalse: [nil]].
			printJobSynchro wait].
	^self! !

!KRMuThReportBroker publicMethodsFor: 'broker services-printJob-private'!

canServicePrintJob: t1 process: t2 

	^true!

printJobProcessFinished

	^printJobProcessFinishedCritical critical: [printJobProcessFinished == true]!

registerStartOfNewPrintJobProcess

	^printJobProcessFinishedCritical critical: [printJobProcessFinished := false]!

registerStartOfNewPrintJobProcessIfFinished

	^printJobProcessFinishedCritical critical: [printJobProcessFinished == true
			and: 
				[printJobProcessFinished := false.
				true]]!

startNewIfPrintJobProcessFinished

	^printJobProcessFinishedCritical critical: [printJobProcessFinished == true
			and: 
				[printJobProcessFinished := false.
				true]]!

terminatePrintJobProcess

	printJobProcess isNil ifTrue: [^self].
	printJobProcess terminate.
	^self! !

!KRMuThReportBroker publicMethodsFor: 'broker services-report'!

informReportProcessHasFinishedFrom: t1 process: t2 

	(self canServiceReport: t1 process: t2)
		ifFalse: [^nil].
	reportProcessFinishedCritical critical: [reportProcessFinished := true].
	self allowProceedPrintJob.
	reportProcess := nil.
	self eventToClient.
	self eventToMonitor.
	(self clientIsOpened not and: [self printJobProcessFinished and: [self reportProcessFinished]])
		ifTrue: [self release].
	^self!

rqstAddMissingElement: t1 toPageNumbered: t2 from: t3 process: t4 

	| t5 t6 t7 |
	(self canServiceReport: t3 process: t4)
		ifFalse: [^nil].
	t2 isNil ifTrue: [^nil].
	t2 < 1 ifTrue: [^nil].
	t6 := false.
	t7 := pagesCritical critical: [t2 > pages size
					ifTrue: [nil]
					ifFalse: 
						[(t5 := pages at: t2) addElement: t1.
						t5 oneLessMissingElement = 0
							ifTrue: [t6 := true]
							ifFalse: [nil]]].
	t6
		ifTrue: 
			[self notifyPageCompleted.
			self eventToClient.
			self eventToMonitor].
	^t7!

rqstAddPage: t1 from: t2 process: t3 

	| t4 t5 |
	(self canServiceReport: t2 process: t3)
		ifFalse: [^nil].
	pagesCritical critical: [pages add: t1].
	self notifyPageAdded.
	t4 := self maxCachedPages.
	t5 := self clientIsOpened.
	self eventToClient.
	self eventToMonitor.
	(t5 not and: [t4 > 0])
		ifTrue: [self currentReadyPages > t4 ifTrue: [self waitForPageBeingConsumed]].
	^self! !

!KRMuThReportBroker publicMethodsFor: 'broker services-report-private'!

canServiceReport: t1 process: t2 

	^true!

currentReadyPages

	| t1 |
	t1 := 0.
	pagesCritical critical: [pages do: [:t2 | t2 notNil
				ifTrue: 
					[t2 hasMissingElements ifTrue: [^t1].
					t1 := t1 + 1]
				ifFalse: [nil]]].
	^t1!

reportProcessFinished

	^reportProcessFinishedCritical critical: [reportProcessFinished == true]!

terminateReportProcess

	reportProcess isNil ifTrue: [^self].
	reportProcess terminate.
	^self!

waitForPageBeingConsumed

	| t1 |
	self clientIsOpened ifTrue: [^self].
	(t1 := self maxCachedPages) < 0 ifTrue: [^self].
	[self currentReadyPages > t1]
		whileTrue: 
			[self clientIsOpened ifTrue: [^self].
			reportProcessSynchro wait].
	^self! !

!KRMuThReportBroker publicMethodsFor: 'execution'!

createDisplayClient

	| t1 |
	(t1 := self preferredReportDisplayClientClass newForReportBroker: self) isNil ifTrue: [^nil].
	t1 displayClientWindowLabel: self displayClientWindowLabel.
	self client: t1.
	^t1!

createMonitor

	| t1 |
	(t1 := self preferredReportMonitorClass newForReportBroker: self) isNil ifTrue: [^nil].
	self monitor: t1.
	^t1!

displayReport

	| t1 |
	(t1 := self createDisplayClient) isNil ifTrue: [^nil].
	t1 displayReport.
	^self!

displayReportConcurrently

	| t1 |
	(t1 := self createDisplayClient) isNil ifTrue: [^nil].
	t1 displayReportConcurrently.
	^self!

forReport: t1 withBlock: t2 displayClientWindowLabel: t3 from: t4 

	| t5 |
	self report: t1.
	self displayClientWindowLabel: t3.
	(t5 := 
			[| t6 t7 |
			t6 := [Process terminateSignal
						handle: 
							[:t8 | 
							t1 informHasFinished.
							t1 release.
							t4 release.
							t8 reject]
						do: [Object errorSignal
								handle: 
									[:t9 | 
									Dialog warn: 'ERROR while generating report.\Report generation cancelled.\' withCRs , (t9 errorString contractTo: 64).
									(Smalltalk at: #Browser ifAbsent: [nil]) isNil
										ifTrue: 
											[t1 informHasFinished.
											t1 release.
											t4 release.
											t9 returnWith: nil]
										ifFalse: [t9 reject]]
								do: [t2
										valueNowOrOnUnwindDo: 
											[t1 informHasFinished.
											t1 release.
											t4 release]]]].
			(t7 := Smalltalk at: #TimeProfiler ifAbsent: [nil]) isNil
				ifTrue: [t6 value]
				ifFalse: [false
						ifTrue: [t7 profile: t6]
						ifFalse: [t6 value]]] newProcess) priority: Processor userBackgroundPriority.
	self reportProcess: t5.
	t1 reportBroker: self.
	self resumeReport.
	^self!

monitorReport

	| t1 |
	(t1 := self createMonitor) isNil ifTrue: [^nil].
	^t1 monitorReport!

printReport

	^self printReportUsingDialog: false waitTermination: false!

printReportAndWaitTermination

	^self printReportUsingDialog: false waitTermination: true!

printReportConcurrently

	^self printReportConcurrentlyUsingDialog: false!

printReportConcurrentlyUsingDialog: t1 

	| t2 |
	printJob isNil
		ifTrue: [self registerStartOfNewPrintJobProcess]
		ifFalse: [self registerStartOfNewPrintJobProcessIfFinished
				ifFalse: 
					[Dialog warn: 'A Print Job is already active'.
					^nil]].
	printJobProcess isNil ifFalse: [printJobProcess terminate].
	printJobProcess := nil.
	printJob := nil.
	self reportProcessFinished ifFalse: [pagesCritical
			critical: 
				[| t3 t4 |
				t3 := 1.
				t4 := pages size.
				[t3 <= t4]
					whileTrue: 
						[printJobSynchro signal.
						t3 := t3 + 1].
				nil]].
	(printJob := self preferredPrintJobClass newForReportBroker: self) isNil ifTrue: [^nil].
	printJob pageSize: report pageSize.
	printJob numberOfCopies: 1.
	printJob preferenceManager: report printPagerPreferenceManager.
	printJob printPagerPreferenceMap: report printPagerPreferenceMap.
	
	[t2 := Printer startPrintJobNamed: 'temp.ps'.
	printJob file: t2.
	t1 == true
		ifTrue: [(printJob adjustPagerFor: printJob pageSize)
				ifTrue: [nil]
				ifFalse: 
					[printJob := nil.
					^nil]]
		ifFalse: [nil]]
		valueOnUnwindDo: [t2 notNil
				ifTrue: [t2 close]
				ifFalse: [nil]].
	numberOfPagesSentToPrintJob := 0.
	(printJobProcess := 
			[| t5 t6 |
			t5 := [Process terminateSignal
						handle: 
							[:t7 | 
							printJob isNil ifFalse: [printJob informHasFinished].
							t7 reject]
						do: [Object errorSignal
								handle: 
									[:t8 | 
									printJob isNil ifFalse: [printJob informHasFinished].
									Dialog warn: 'ERROR while printing report.\Print report cancelled\' withCRs , (t8 errorString contractTo: 64).
									(Smalltalk at: #Browser ifAbsent: [nil]) isNil
										ifTrue: [t8 returnWith: nil]
										ifFalse: [t8 reject]]
								do: [[printJob isNil
										ifTrue: [nil]
										ifFalse: [printJob execute]]
										valueNowOrOnUnwindDo: [printJob isNil
												ifTrue: [nil]
												ifFalse: [printJob informHasFinished]]]]].
			(t6 := Smalltalk at: #TimeProfiler ifAbsent: [nil]) isNil
				ifTrue: [t5 value]
				ifFalse: [false
						ifTrue: [t6 profile: t5]
						ifFalse: [t5 value]]] newProcess) priority: Processor userBackgroundPriority.
	printJobProcess resume.
	^self!

printReportUsingDialog: t1 

	^self printReportUsingDialog: t1 waitTermination: false!

printReportUsingDialog: t1 waitTermination: t2 

	self printReportConcurrentlyUsingDialog: t1.
	t2 == true
		ifTrue: 
			[printJobProcessFinishedSynchro wait.
			^true].
	^self monitorReport!

resumeReport

	reportProcess isNil ifFalse: [reportProcess resume].
	^self! !

!KRMuThReportBroker publicMethodsFor: 'initialize-release'!

defaultDisplayClientWindowLabel

	^'Report Viewer' copy!

initClientOpenedCritical

	clientOpenedCritical := Semaphore forMutualExclusion.
	^self!

initDisplayClientWindowLabel

	displayClientWindowLabel := self defaultDisplayClientWindowLabel.
	^self!

initialize

	self initPages.
	self initPagesCritical.
	self initPrintJobSynchro.
	self initReportProcessFinishedCritical.
	self initPrintJobProcessFinishedCritical.
	self initClientOpenedCritical.
	self initMonitorOpenedCritical.
	self initReportProcessSynchro.
	self initPrintJobProcessFinishedSynchro.
	printJobProcessFinished := true.
	reportProcessFinished := false.
	^self!

initMonitorOpenedCritical

	monitorOpenedCritical := Semaphore forMutualExclusion.
	^self!

initPages

	pages := OrderedCollection new.
	^self!

initPagesCritical

	pagesCritical := Semaphore forMutualExclusion.
	^self!

initPrintJobProcessFinishedCritical

	printJobProcessFinishedCritical := Semaphore forMutualExclusion.
	^self!

initPrintJobProcessFinishedSynchro

	printJobProcessFinishedSynchro := Semaphore new.
	^self!

initPrintJobSynchro

	printJobSynchro := Semaphore new.
	^self!

initReportProcessFinishedCritical

	reportProcessFinishedCritical := Semaphore forMutualExclusion.
	^self!

initReportProcessSynchro

	reportProcessSynchro := Semaphore new.
	^self!

release

	client isNil
		ifFalse: 
			[client builder isNil ifFalse: [client builder window isNil ifFalse: [client builder window isOpen ifTrue: [client builder window controller closeAndUnschedule]]].
			client release].
	client := nil.
	clientOpened := nil.
	clientOpenedCritical isNil ifFalse: [clientOpenedCritical signal].
	clientOpenedCritical := nil.
	report isNil ifFalse: [report release].
	report := nil.
	reportProcessSynchro isNil ifFalse: [reportProcessSynchro signal].
	reportProcessSynchro := nil.
	reportProcess isNil ifFalse: [reportProcess terminate].
	reportProcess := nil.
	reportProcessFinished := nil.
	reportProcessFinishedCritical isNil ifFalse: [reportProcessFinishedCritical signal].
	reportProcessFinishedCritical := nil.
	pages isNil ifFalse: [pages do: [:t1 | t1 release]].
	pages := nil.
	pagesCritical isNil ifFalse: [pagesCritical signal].
	pagesCritical := nil.
	printJob isNil ifFalse: [printJob release].
	printJob := nil.
	printJobSynchro isNil ifFalse: [printJobSynchro signal].
	printJobSynchro := nil.
	printJobProcess isNil ifFalse: [printJobProcess terminate].
	printJobProcess := nil.
	printJobProcessFinished := nil.
	printJobProcessFinishedCritical isNil ifFalse: [printJobProcessFinishedCritical signal].
	printJobProcessFinishedCritical := nil.
	printJobProcessFinishedSynchro isNil ifFalse: [printJobProcessFinishedSynchro signal].
	printJobProcessFinishedSynchro := nil.
	maxCachedPages := nil.
	displayClientWindowLabel := nil.
	numberOfPagesSentToPrintJob := nil.
	numberOfPagesSentToPrintJobCritical isNil ifFalse: [numberOfPagesSentToPrintJobCritical signal].
	numberOfPagesSentToPrintJobCritical := nil.
	monitor isNil
		ifFalse: 
			[monitor builder isNil ifFalse: [monitor builder window isNil ifFalse: [monitor builder window isOpen ifTrue: [monitor builder window controller closeAndUnschedule]]].
			monitor release].
	monitor := nil.
	monitorOpened := nil.
	monitorOpenedCritical isNil ifFalse: [monitorOpenedCritical signal].
	monitorOpenedCritical := nil.
	super release.
	^self! !

!KRMuThReportBroker publicMethodsFor: 'preferences'!

preferredPrintJobClass

	^self class preferredPrintJobClass!

preferredReportDisplayClientClass

	^self class preferredReportDisplayClientClass!

preferredReportMonitorClass

	^self class preferredReportMonitorClass! !

!KRMuThReportDefinition publicMethodsFor: 'accessing'!

displayClientWindowLabel

	displayClientWindowLabel isNil ifTrue: [self initDisplayClientWindowLabel].
	^displayClientWindowLabel!

displayClientWindowLabel: t1 

	displayClientWindowLabel := t1.
	^self!

height

	^super height!

height: t1 

	super height: t1.
	self changed: #height.
	^self!

minBodyOrphanHeight

	minBodyOrphanHeight isNil ifTrue: [self initMinBodyOrphanHeight].
	^minBodyOrphanHeight!

minBodyOrphanHeight: t1 

	minBodyOrphanHeight := t1.
	^self!

pageLayout

	^pageLayout!

pageLayout: t1 

	pageLayout := t1.
	^self!

pageLayoutOrCreateCustom

	pageLayout isNil
		ifTrue: 
			[pageLayout := KRMuThReportPageLayout findOrCreateCustomFromPrintableInchWidth: self width andHeight: self height.
			self changed: #layoutName].
	^pageLayout!

setReportPageLayout: t1 

	t1 isNil ifTrue: [^nil].
	pageLayout := t1.
	self changed: #pageLayout.
	self height: t1 printableHeightInch.
	self width: t1 printableWidthInch.
	self leftMargin: t1 leftMargin.
	self topMargin: t1 topMargin.
	self rightMargin: t1 rightMargin.
	self bottomMargin: t1 bottomMargin.
	^self!

splitBodyOnPageBreak

	^splitBodyOnPageBreak == true!

splitBodyOnPageBreak: t1 

	splitBodyOnPageBreak := t1 == true.
	^self!

width

	^super width!

width: t1 

	super width: t1.
	self changed: #width.
	^self! !

!KRMuThReportDefinition publicMethodsFor: 'converting'!

fromLiteralArrayEncoding: t1 

	super fromLiteralArrayEncoding: t1.
	pageLayout isNil ifFalse: [pageLayout := pageLayout decodeAsLiteralArray].
	^self!

literalArrayEncoding

	| t1 t2 t3 |
	(self splitBodyOnPageBreak not and: [(self displayClientWindowLabel isNil or: [self displayClientWindowLabel == self defaultDisplayClientWindowLabel])
			and: [self pageLayout isNil]])
		ifTrue: 
			[(t3 := super literalArrayEncoding) at: 1 put: self class superclass name.
			^t3].
	t1 := #(#bodyClass #bodySelector #headerClass #headerSelector #footerClass #footerSelector #titleClass #titleSelector #summaryClass #summarySelector #height #width #leftMargin #topMargin #rightMargin #bottomMargin #separateSummaryPage #separateTitlePage #showSummaryHeader #showSummaryFooter #showTitleHeader #showTitleFooter #splitBodyOnPageBreak #minBodyOrphanHeight #displayClientWindowLabel #breakDefinitions #pageLayout).
	(t2 := OrderedCollection new: t1 size * 2) add: self class name.
	t1 do: 
		[:t4 | 
		| t5 |
		(t5 := self perform: t4) == nil
			ifTrue: [nil]
			ifFalse: 
				[t2 add: (t4 , ':') asSymbol.
				t2 add: t5 literalArrayEncoding]].
	^t2 asArray! !

!KRMuThReportDefinition publicMethodsFor: 'defaults'!

defaultDisplayClientWindowLabel

	^'Report Viewer' copy!

defaultMinBodyOrphanHeight

	^16! !

!KRMuThReportDefinition publicMethodsFor: 'initialize-release'!

initDisplayClientWindowLabel

	displayClientWindowLabel := self defaultDisplayClientWindowLabel.
	^self!

initMinBodyOrphanHeight

	minBodyOrphanHeight := self defaultMinBodyOrphanHeight.
	^self!

release

	splitBodyOnPageBreak := nil.
	minBodyOrphanHeight := nil.
	displayClientWindowLabel := nil.
	pageLayout := nil.
	^super release! !

!KRMuThReportDisplayClient class publicMethodsFor: 'constants'!

fileSubmenuIndex

	^1!

firstPageMenuItemIndex

	^3!

lastPageMenuItemIndex

	^4!

nextPageMenuItemIndex

	^1!

pageByNumberMenuItemIndex

	^5!

pageSubmenuIndex

	^2!

prevPageMenuItemIndex

	^2!

printReportMenuItemIndex

	^1!

printReportUsingDialogMenuItemIndex

	^2!

processSubmenuIndex

	^3!

stopPrintJobMenuItemIndex

	^2!

stopReportMenuItemIndex

	^1! !

!KRMuThReportDisplayClient class publicMethodsFor: 'instance creation'!

newForReportBroker: t1 

	| t2 |
	(t2 := self new) reportBroker: t1.
	^t2! !

!KRMuThReportDisplayClient class publicMethodsFor: 'interface specs'!

dialogSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Report Viewer' #min: #(#Point 50 50) #bounds: #(#Rectangle 473 246 1113 666) #flags: 4 #menu: #reportViewerMenu #isEventDriven: true) #component: #(#SpecCollection #collection: #(#(#LabelSpec #layout: #(#Rectangle 5 5 55 30) #name: #currentPageNumberLabelID #label: 'Page #') #(#InputFieldSpec #layout: #(#Rectangle 60 5 110 30) #name: #currentPageNumberFieldID #model: #currentPageNumber #alignment: #right #type: #number #formatString: '0') #(#LabelSpec #layout: #(#Rectangle 115 5 140 30) #name: #numberOfPagesLabelID #label: 'of') #(#InputFieldSpec #layout: #(#Rectangle 145 5 195 30) #name: #numberOfPagesFieldID #model: #numberOfPages #tabable: false #alignment: #right #isReadOnly: true #type: #number #formatString: '0') #(#ActionButtonSpec #layout: #(#Rectangle 210 1 242 33) #name: #firstPageButtonID #model: #firstPage #label: #firstPageIcon #hasCharacterOrientedLabel: false #isDefault: false) #(#ActionButtonSpec #layout: #(#Rectangle 242 1 274 33) #name: #prevPageButtonID #model: #prevPage #label: #prevPageIcon #hasCharacterOrientedLabel: false #isDefault: false) #(#ActionButtonSpec #layout: #(#Rectangle 274 1 306 33) #name: #nextPageButtonID #model: #nextPage #label: #nextPageIcon #hasCharacterOrientedLabel: false #isDefault: false) #(#ActionButtonSpec #layout: #(#Rectangle 306 1 338 33) #name: #lastPageButtonID #model: #lastPage #label: #lastPageIcon #hasCharacterOrientedLabel: false #isDefault: false) #(#SubCanvasSpec #layout: #(#LayoutFrame 0 0 35 0 0 1 0 1) #name: #pagesHolderWidgetID #flags: 11) #(#ActionButtonSpec #layout: #(#Rectangle 354 1 386 33) #name: #stopReportButtonID #model: #stopReport #label: #stopReportIcon #hasCharacterOrientedLabel: false #isDefault: false) #(#ActionButtonSpec #layout: #(#Rectangle 402 1 434 33) #name: #printJobButtonID #model: #printReport #label: #printJobIcon #hasCharacterOrientedLabel: false #isDefault: false) #(#ActionButtonSpec #layout: #(#Rectangle 434 1 466 33) #name: #stopPrintJobButtonID #model: #stopPrintJob #label: #stopPrintJobIcon #hasCharacterOrientedLabel: false #isDefault: false) #(#ActionButtonSpec #layout: #(#Rectangle 482 1 514 33) #name: #closeRequestButtonID #model: #cancel #label: #closeRequestIcon #hasCharacterOrientedLabel: false #isDefault: false))))!

windowSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Report Viewer' #min: #(#Point 50 50) #bounds: #(#Rectangle 473 246 1113 666) #flags: 4 #menu: #reportViewerMenu #isEventDriven: true) #component: #(#SpecCollection #collection: #(#(#LabelSpec #layout: #(#Rectangle 5 5 55 30) #name: #currentPageNumberLabelID #label: 'Page #') #(#InputFieldSpec #layout: #(#Rectangle 60 5 110 30) #name: #currentPageNumberFieldID #model: #currentPageNumber #alignment: #right #type: #number #formatString: '0') #(#LabelSpec #layout: #(#Rectangle 115 5 140 30) #name: #numberOfPagesLabelID #label: 'of') #(#InputFieldSpec #layout: #(#Rectangle 145 5 195 30) #name: #numberOfPagesFieldID #model: #numberOfPages #tabable: false #alignment: #right #isReadOnly: true #type: #number #formatString: '0') #(#ActionButtonSpec #layout: #(#Rectangle 210 1 242 33) #name: #firstPageButtonID #model: #firstPage #label: #firstPageIcon #hasCharacterOrientedLabel: false #isDefault: false) #(#ActionButtonSpec #layout: #(#Rectangle 242 1 274 33) #name: #prevPageButtonID #model: #prevPage #label: #prevPageIcon #hasCharacterOrientedLabel: false #isDefault: false) #(#ActionButtonSpec #layout: #(#Rectangle 274 1 306 33) #name: #nextPageButtonID #model: #nextPage #label: #nextPageIcon #hasCharacterOrientedLabel: false #isDefault: false) #(#ActionButtonSpec #layout: #(#Rectangle 306 1 338 33) #name: #lastPageButtonID #model: #lastPage #label: #lastPageIcon #hasCharacterOrientedLabel: false #isDefault: false) #(#SubCanvasSpec #layout: #(#LayoutFrame 0 0 35 0 0 1 0 1) #name: #pagesHolderWidgetID #flags: 11) #(#ActionButtonSpec #layout: #(#Rectangle 354 1 386 33) #name: #stopReportButtonID #model: #stopReport #label: #stopReportIcon #hasCharacterOrientedLabel: false #isDefault: false) #(#ActionButtonSpec #layout: #(#Rectangle 402 1 434 33) #name: #printJobButtonID #model: #printReport #label: #printJobIcon #hasCharacterOrientedLabel: false #isDefault: false) #(#ActionButtonSpec #layout: #(#Rectangle 434 1 466 33) #name: #stopPrintJobButtonID #model: #stopPrintJob #label: #stopPrintJobIcon #hasCharacterOrientedLabel: false #isDefault: false) #(#ActionButtonSpec #layout: #(#Rectangle 482 1 514 33) #name: #closeRequestButtonID #model: #closeViewer #label: #closeRequestIcon #hasCharacterOrientedLabel: false #isDefault: false))))! !

!KRMuThReportDisplayClient class publicMethodsFor: 'preferences'!

preferredPagesHolderClass

	^KRMuThReportPageDisplayClient!

preferredReportDisplayControllerClass

	^KRMuThReportDisplayController! !

!KRMuThReportDisplayClient class publicMethodsFor: 'resources'!

closeRequestIcon

	^OpaqueImageWithEnablement
		figure: self closeRequestImage
		shape: self closeRequestMask
		inactiveFigure: self closeRequestInactiveImage
		inactiveShape: self closeRequestMask!

closeRequestImage

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 1
			bitsPerPixel: 1
			palette: (MappedPalette withColors: ((Array new: 2)
						at: 1 put: ColorValue red;
						at: 2 put: ColorValue white; yourself))
			usingBits: #[255 255 0 0 159 243 0 0 159 231 0 0 207 199 0 0 199 143 0 0 227 143 0 0 243 31 0 0 248 31 0 0 248 63 0 0 248 63 0 0 240 31 0 0 225 15 0 0 195 199 0 0 135 243 0 0 223 255 0 0 255 255 0 0])!

closeRequestInactiveImage

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 1
			bitsPerPixel: 1
			palette: (MappedPalette withColors: ((Array new: 2)
						at: 1 put: ColorValue gray;
						at: 2 put: ColorValue white; yourself))
			usingBits: #[255 255 0 0 159 243 0 0 159 231 0 0 207 199 0 0 199 143 0 0 227 143 0 0 243 31 0 0 248 31 0 0 248 63 0 0 248 63 0 0 240 31 0 0 225 15 0 0 195 199 0 0 135 243 0 0 223 255 0 0 255 255 0 0])!

closeRequestMask

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 1
			bitsPerPixel: 1
			palette: CoveragePalette monoMaskPalette
			usingBits: #[0 0 0 0 96 12 0 0 96 24 0 0 48 56 0 0 56 112 0 0 28 112 0 0 12 224 0 0 7 224 0 0 7 192 0 0 7 192 0 0 15 224 0 0 30 240 0 0 60 56 0 0 120 12 0 0 32 0 0 0 0 0 0 0])!

firstPageIcon

	^OpaqueImageWithEnablement
		figure: self firstPageImage
		shape: self firstPageMask
		inactiveFigure: self firstPageInactiveImage
		inactiveShape: self firstPageMask!

firstPageImage

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 1
			bitsPerPixel: 1
			palette: (MappedPalette withColors: ((Array new: 2)
						at: 1 put: ColorValue blue;
						at: 2 put: ColorValue white; yourself))
			usingBits: #[255 255 0 0 207 239 0 0 207 207 0 0 207 143 0 0 207 15 0 0 206 15 0 0 204 15 0 0 200 15 0 0 200 15 0 0 204 15 0 0 206 15 0 0 207 15 0 0 207 143 0 0 207 207 0 0 207 239 0 0 255 255 0 0])!

firstPageInactiveImage

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 1
			bitsPerPixel: 1
			palette: (MappedPalette withColors: ((Array new: 2)
						at: 1 put: ColorValue white;
						at: 2 put: ColorValue gray; yourself))
			usingBits: #[0 0 0 0 48 16 0 0 48 48 0 0 48 112 0 0 48 240 0 0 49 240 0 0 51 240 0 0 55 240 0 0 55 240 0 0 51 240 0 0 49 240 0 0 48 240 0 0 48 112 0 0 48 48 0 0 48 16 0 0 0 0 0 0])!

firstPageMask

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 1
			bitsPerPixel: 1
			palette: CoveragePalette monoMaskPalette
			usingBits: #[0 0 0 0 48 16 0 0 48 48 0 0 48 112 0 0 48 240 0 0 49 240 0 0 51 240 0 0 55 240 0 0 55 240 0 0 51 240 0 0 49 240 0 0 48 240 0 0 48 112 0 0 48 48 0 0 48 16 0 0 0 0 0 0])!

lastPageIcon

	^OpaqueImageWithEnablement
		figure: self lastPageImage
		shape: self lastPageMask
		inactiveFigure: self lastPageInactiveImage
		inactiveShape: self lastPageMask!

lastPageImage

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 1
			bitsPerPixel: 1
			palette: (MappedPalette withColors: ((Array new: 2)
						at: 1 put: ColorValue blue;
						at: 2 put: ColorValue white; yourself))
			usingBits: #[255 255 0 0 247 243 0 0 243 243 0 0 241 243 0 0 240 243 0 0 240 115 0 0 240 51 0 0 240 19 0 0 240 19 0 0 240 51 0 0 240 115 0 0 240 243 0 0 241 243 0 0 243 243 0 0 247 243 0 0 255 255 0 0])!

lastPageInactiveImage

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 1
			bitsPerPixel: 1
			palette: (MappedPalette withColors: ((Array new: 2)
						at: 1 put: ColorValue white;
						at: 2 put: ColorValue gray; yourself))
			usingBits: #[0 0 0 0 8 12 0 0 12 12 0 0 14 12 0 0 15 12 0 0 15 140 0 0 15 204 0 0 15 236 0 0 15 236 0 0 15 204 0 0 15 140 0 0 15 12 0 0 14 12 0 0 12 12 0 0 8 12 0 0 0 0 0 0])!

lastPageMask

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 1
			bitsPerPixel: 1
			palette: CoveragePalette monoMaskPalette
			usingBits: #[0 0 0 0 8 12 0 0 12 12 0 0 14 12 0 0 15 12 0 0 15 140 0 0 15 204 0 0 15 236 0 0 15 236 0 0 15 204 0 0 15 140 0 0 15 12 0 0 14 12 0 0 12 12 0 0 8 12 0 0 0 0 0 0])!

nextPageIcon

	^OpaqueImageWithEnablement
		figure: self nextPageImage
		shape: self nextPageMask
		inactiveFigure: self nextPageInactiveImage
		inactiveShape: self nextPageMask!

nextPageImage

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 1
			bitsPerPixel: 1
			palette: (MappedPalette withColors: ((Array new: 2)
						at: 1 put: ColorValue blue;
						at: 2 put: ColorValue white; yourself))
			usingBits: #[255 255 0 0 247 255 0 0 243 255 0 0 241 255 0 0 240 255 0 0 240 127 0 0 240 63 0 0 240 31 0 0 240 31 0 0 240 63 0 0 240 127 0 0 240 255 0 0 241 255 0 0 243 255 0 0 247 255 0 0 255 255 0 0])!

nextPageInactiveImage

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 1
			bitsPerPixel: 1
			palette: (MappedPalette withColors: ((Array new: 2)
						at: 1 put: ColorValue white;
						at: 2 put: ColorValue gray; yourself))
			usingBits: #[0 0 0 0 8 0 0 0 12 0 0 0 14 0 0 0 15 0 0 0 15 128 0 0 15 192 0 0 15 224 0 0 15 224 0 0 15 192 0 0 15 128 0 0 15 0 0 0 14 0 0 0 12 0 0 0 8 0 0 0 0 0 0 0])!

nextPageMask

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 1
			bitsPerPixel: 1
			palette: CoveragePalette monoMaskPalette
			usingBits: #[0 0 0 0 8 0 0 0 12 0 0 0 14 0 0 0 15 0 0 0 15 128 0 0 15 192 0 0 15 224 0 0 15 224 0 0 15 192 0 0 15 128 0 0 15 0 0 0 14 0 0 0 12 0 0 0 8 0 0 0 0 0 0 0])!

prevPageIcon

	^OpaqueImageWithEnablement
		figure: self prevPageImage
		shape: self prevPageMask
		inactiveFigure: self prevPageInactiveImage
		inactiveShape: self prevPageMask!

prevPageImage

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 1
			bitsPerPixel: 1
			palette: (MappedPalette withColors: ((Array new: 2)
						at: 1 put: ColorValue blue;
						at: 2 put: ColorValue white; yourself))
			usingBits: #[255 255 0 0 255 239 0 0 255 207 0 0 255 143 0 0 255 15 0 0 254 15 0 0 252 15 0 0 248 15 0 0 248 15 0 0 252 15 0 0 254 15 0 0 255 15 0 0 255 143 0 0 255 207 0 0 255 239 0 0 255 255 0 0])!

prevPageInactiveImage

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 1
			bitsPerPixel: 1
			palette: (MappedPalette withColors: ((Array new: 2)
						at: 1 put: ColorValue white;
						at: 2 put: ColorValue gray; yourself))
			usingBits: #[0 0 0 0 0 16 0 0 0 48 0 0 0 112 0 0 0 240 0 0 1 240 0 0 3 240 0 0 7 240 0 0 7 240 0 0 3 240 0 0 1 240 0 0 0 240 0 0 0 112 0 0 0 48 0 0 0 16 0 0 0 0 0 0])!

prevPageMask

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 1
			bitsPerPixel: 1
			palette: CoveragePalette monoMaskPalette
			usingBits: #[0 0 0 0 0 16 0 0 0 48 0 0 0 112 0 0 0 240 0 0 1 240 0 0 3 240 0 0 7 240 0 0 7 240 0 0 3 240 0 0 1 240 0 0 0 240 0 0 0 112 0 0 0 48 0 0 0 16 0 0 0 0 0 0])!

printJobIcon

	^OpaqueImageWithEnablement
		figure: self printJobImage
		shape: self printJobMask
		inactiveFigure: self printJobInactiveImage
		inactiveShape: self printJobMask!

printJobImage

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 2
			bitsPerPixel: 2
			palette: (MappedPalette withColors: ((Array new: 3)
						at: 1 put: ColorValue black;
						at: 2 put: ColorValue yellow;
						at: 3 put: ColorValue white; yourself))
			usingBits: #[170 170 170 170 170 160 0 170 170 138 168 170 170 136 8 170 170 138 168 170 170 32 32 2 168 42 161 82 161 0 1 66 133 85 85 18 128 0 0 82 133 85 84 74 133 85 84 10 128 0 0 74 161 85 81 42 160 0 0 170 170 170 170 170])!

printJobInactiveImage

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 2
			bitsPerPixel: 2
			palette: (MappedPalette withColors: ((Array new: 3)
						at: 1 put: ColorValue gray;
						at: 2 put: ColorValue white;
						at: 3 put: ColorValue lightGray; yourself))
			usingBits: #[85 85 85 85 85 85 85 85 85 85 85 85 85 85 85 85 85 85 85 85 85 0 0 1 84 42 170 161 82 0 2 129 74 170 170 33 64 0 0 161 74 170 168 133 74 170 168 5 64 0 0 133 82 170 162 21 80 0 0 85 85 85 85 85])!

printJobMask

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 1
			bitsPerPixel: 1
			palette: CoveragePalette monoMaskPalette
			usingBits: #[0 0 0 0 3 240 0 0 7 240 0 0 7 240 0 0 7 240 0 0 15 254 0 0 31 254 0 0 63 254 0 0 127 254 0 0 127 254 0 0 127 252 0 0 127 252 0 0 127 252 0 0 63 248 0 0 63 240 0 0 0 0 0 0])!

reportIcon

	^OpaqueImageWithEnablement
		figure: self reportImage
		shape: self reportMask
		inactiveFigure: self reportInactiveImage
		inactiveShape: self reportMask!

reportImage

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 2
			bitsPerPixel: 2
			palette: (MappedPalette withColors: ((Array new: 3)
						at: 1 put: ColorValue black;
						at: 2 put: ColorValue yellow;
						at: 3 put: ColorValue white; yourself))
			usingBits: #[170 170 170 170 160 0 0 42 161 85 85 42 161 0 65 42 161 85 85 42 161 0 65 42 161 85 85 42 161 0 65 42 161 85 85 42 161 0 65 42 161 85 85 42 161 0 65 42 161 85 85 42 161 85 65 42 160 0 0 42 170 170 170 170])!

reportInactiveImage

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 2
			bitsPerPixel: 2
			palette: (MappedPalette withColors: ((Array new: 3)
						at: 1 put: ColorValue gray;
						at: 2 put: ColorValue white;
						at: 3 put: ColorValue lightGray; yourself))
			usingBits: #[85 85 85 85 80 0 0 21 82 170 170 21 82 0 130 21 82 170 170 21 82 0 130 21 82 170 170 21 82 0 130 21 82 170 170 21 82 0 130 21 82 170 170 21 82 0 130 21 82 170 170 21 82 170 130 21 80 0 0 21 85 85 85 85])!

reportMask

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 1
			bitsPerPixel: 1
			palette: CoveragePalette monoMaskPalette
			usingBits: #[0 0 0 0 63 248 0 0 63 248 0 0 63 248 0 0 63 248 0 0 63 248 0 0 63 248 0 0 63 248 0 0 63 248 0 0 63 248 0 0 63 248 0 0 63 248 0 0 63 248 0 0 63 248 0 0 63 248 0 0 0 0 0 0])!

reportViewerMenu

	<resource: #menu>
	^#(#Menu #(#(#MenuItem #rawLabel: 'File' #submenu: #(#Menu #(#(#MenuItem #rawLabel: 'Print' #value: #printReport) #(#MenuItem #rawLabel: 'Print...' #value: #printReportUsingDialog) #(#MenuItem #rawLabel: 'Close Viewer' #value: #cancel)) #(2 1) nil)) #(#MenuItem #rawLabel: 'Page' #submenu: #(#Menu #(#(#MenuItem #rawLabel: 'Next' #value: #nextPage) #(#MenuItem #rawLabel: 'Previous' #value: #prevPage) #(#MenuItem #rawLabel: 'First' #value: #firstPage) #(#MenuItem #rawLabel: 'Last' #value: #lastPage) #(#MenuItem #rawLabel: 'Number...' #value: #pageByNumber)) #(2 2 1) nil)) #(#MenuItem #rawLabel: 'Process' #submenu: #(#Menu #(#(#MenuItem #rawLabel: 'Stop Report' #value: #stopReport) #(#MenuItem #rawLabel: 'Stop Print Job' #value: #stopPrintJob)) #(1 1) nil))) #(3) nil) decodeAsLiteralArray!

stopPrintJobIcon

	^OpaqueImageWithEnablement
		figure: self stopPrintJobImage
		shape: self stopPrintJobMask
		inactiveFigure: self stopPrintJobInactiveImage
		inactiveShape: self stopPrintJobMask!

stopPrintJobImage

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 2
			bitsPerPixel: 2
			palette: (MappedPalette withColors: ((Array new: 4)
						at: 1 put: ColorValue black;
						at: 2 put: ColorValue red;
						at: 3 put: ColorValue yellow;
						at: 4 put: ColorValue white; yourself))
			usingBits: #[255 255 255 255 255 240 5 255 255 215 245 255 255 212 20 255 255 197 220 255 255 53 80 3 252 61 82 163 242 5 82 131 202 166 86 35 192 17 21 163 202 86 165 143 201 86 165 79 193 80 1 79 245 106 162 127 245 0 0 255 255 255 255 255])!

stopPrintJobInactiveImage

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 2
			bitsPerPixel: 2
			palette: (MappedPalette withColors: ((Array new: 3)
						at: 1 put: ColorValue gray;
						at: 2 put: ColorValue white;
						at: 3 put: ColorValue lightGray; yourself))
			usingBits: #[85 85 85 85 85 85 85 85 85 85 85 85 85 85 85 85 85 85 85 85 85 0 0 1 84 42 170 161 82 0 2 129 74 170 170 33 64 0 0 161 74 170 168 133 74 170 168 5 64 0 0 133 82 170 162 21 80 0 0 85 85 85 85 85])!

stopPrintJobMask

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 1
			bitsPerPixel: 1
			palette: CoveragePalette monoMaskPalette
			usingBits: #[0 0 0 0 3 240 0 0 7 240 0 0 7 240 0 0 7 240 0 0 15 254 0 0 31 254 0 0 63 254 0 0 127 254 0 0 127 254 0 0 127 252 0 0 127 252 0 0 127 252 0 0 63 248 0 0 63 240 0 0 0 0 0 0])!

stopReportIcon

	^OpaqueImageWithEnablement
		figure: self stopReportImage
		shape: self stopReportMask
		inactiveFigure: self stopReportInactiveImage
		inactiveShape: self stopReportMask!

stopReportImage

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 2
			bitsPerPixel: 2
			palette: (MappedPalette withColors: ((Array new: 4)
						at: 1 put: ColorValue black;
						at: 2 put: ColorValue red;
						at: 3 put: ColorValue yellow;
						at: 4 put: ColorValue white; yourself))
			usingBits: #[255 255 255 255 245 0 1 127 245 170 169 127 241 64 133 63 242 90 150 63 242 80 82 63 242 149 106 63 242 5 130 63 242 165 106 63 242 4 82 63 242 86 150 63 242 16 134 63 241 106 165 63 245 170 129 127 245 0 1 127 255 255 255 255])!

stopReportInactiveImage

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 2
			bitsPerPixel: 2
			palette: (MappedPalette withColors: ((Array new: 3)
						at: 1 put: ColorValue gray;
						at: 2 put: ColorValue white;
						at: 3 put: ColorValue lightGray; yourself))
			usingBits: #[85 85 85 85 80 0 0 21 82 170 170 21 82 0 130 21 82 170 170 21 82 0 130 21 82 170 170 21 82 0 130 21 82 170 170 21 82 0 130 21 82 170 170 21 82 0 130 21 82 170 170 21 82 170 130 21 80 0 0 21 85 85 85 85])!

stopReportMask

	<resource: #image>
	^CachedImage on: (Image
			extent: 16 @ 16
			depth: 1
			bitsPerPixel: 1
			palette: CoveragePalette monoMaskPalette
			usingBits: #[0 0 0 0 63 248 0 0 63 248 0 0 63 248 0 0 63 248 0 0 63 248 0 0 63 248 0 0 63 248 0 0 63 248 0 0 63 248 0 0 63 248 0 0 63 248 0 0 63 248 0 0 63 248 0 0 63 248 0 0 0 0 0 0])! !

!KRMuThReportDisplayClient class publicMethodsFor: 'widget-IDs'!

pagesHolderWidgetID

	^#pagesHolderWidgetID! !

!KRMuThReportDisplayClient publicMethodsFor: 'accessing'!

allowIncomplete

	^false!

displayClientWindowLabel

	displayClientWindowLabel isNil ifTrue: [self initDisplayClientWindowLabel].
	^displayClientWindowLabel!

displayClientWindowLabel: t1 

	displayClientWindowLabel := t1.
	^self!

reportBroker

	^reportBroker!

reportBroker: t1 

	reportBroker := t1.
	^self!

reportPage

	^reportPage! !

!KRMuThReportDisplayClient publicMethodsFor: 'actions'!

firstPage

	| t1 |
	(t1 := self numberOfPages value) isNil
		ifTrue: 
			[Dialog warn: 'No pages available'.
			^self].
	t1 > 0
		ifFalse: 
			[Dialog warn: 'No pages available'.
			^self].
	self currentPageNumber value: 1.
	^self!

inspectWindow

	^self!

lastPage

	| t1 |
	(t1 := self numberOfPages value) isNil
		ifTrue: 
			[Dialog warn: 'No pages available'.
			^self].
	t1 > 0
		ifFalse: 
			[Dialog warn: 'No pages available'.
			^self].
	self currentPageNumber value: t1.
	^self!

nextPage

	| t1 t2 |
	(t1 := self currentPageNumber value) isNil ifTrue: [^self].
	(t2 := self numberOfPages value) isNil
		ifTrue: 
			[Dialog warn: 'No pages available'.
			^self].
	t1 < t2
		ifFalse: 
			[Dialog warn: 'No more pages available'.
			^self].
	self currentPageNumber value: t1 + 1.
	^self!

pageByNumber

	| t1 t2 t3 t4 |
	(t1 := self currentPageNumber value) isNil ifTrue: [^self].
	(t2 := self numberOfPages value) isNil
		ifTrue: 
			[Dialog warn: 'No pages available'.
			^self].
	((t3 := Dialog request: 'Enter desired page number.\Between' withCRs , 1 printString , ' and ' , t2 printString initialAnswer: (t1 + 1) printString) isNil or: [t3 isEmpty])
		ifTrue: [^self].
	((t4 := Number readFrom: t3 readStream) isNil or: [t4 = 0])
		ifTrue: [^self].
	t4 <= t2
		ifFalse: 
			[Dialog warn: 'Page number ' , t4 printString , ' is not available'.
			^self].
	self currentPageNumber value: t4.
	^self!

prevPage

	| t1 t2 t3 |
	(t1 := self currentPageNumber value) isNil ifTrue: [^self].
	(t2 := self numberOfPages value) isNil
		ifTrue: 
			[Dialog warn: 'No pages available'.
			^self].
	t2 > 0
		ifFalse: 
			[Dialog warn: 'No pages available'.
			^self].
	t3 := t1 - 1.
	t1 > 1 ifFalse: [t3 := 1].
	self currentPageNumber value: t3.
	^self!

stopPrintJob

	(Dialog confirm: 'Do you really want to stop current Print Job ?' initialAnswer: false)
		ifFalse: [^nil].
	self reportBroker rqstStopPrintJobFrom: self process: Processor activeProcess.
	^self!

stopReport

	(Dialog confirm: 'Do you really want to stop current Report ?' initialAnswer: false)
		ifFalse: [^nil].
	self reportBroker rqstStopReportFrom: self process: Processor activeProcess.
	^self! !

!KRMuThReportDisplayClient publicMethodsFor: 'aspects'!

currentPageNumber

	currentPageNumber isNil ifTrue: [^currentPageNumber := 0 asValue].
	^currentPageNumber!

numberOfPages

	numberOfPages isNil ifTrue: [^numberOfPages := 0 asValue].
	^numberOfPages!

reportViewerMenu

	reportViewerMenu isNil ifTrue: [reportViewerMenu := self class reportViewerMenu asValue].
	^reportViewerMenu! !

!KRMuThReportDisplayClient publicMethodsFor: 'broker services'!

getNumberOfPages

	^self reportBroker
		rqstNumberOfPagesAllowIncomplete: self allowIncomplete
		from: self
		process: Processor activeProcess!

getPrintJobState

	^self reportBroker rqstPrintJobStateFrom: self process: Processor activeProcess!

getReportState

	^self reportBroker rqstReportStateFrom: self process: Processor activeProcess!

informHasClosed

	^self reportBroker informDisplayClientHasClosedFrom: self process: Processor activeProcess!

informHasOpened

	^self reportBroker informDisplayClientHasOpenedFrom: self process: Processor activeProcess!

launchPrintReportUsingDialog: t1 

	self reportBroker
		rqstPrintReportUsingDialog: t1 == true
		from: self
		process: Processor activeProcess.
	^self!

pageNumbered: t1 

	t1 isNil ifTrue: [^nil].
	t1 < 1 ifTrue: [^nil].
	t1 > self numberOfPages value ifTrue: [^nil].
	^self reportBroker
		rqstPageNumbered: t1
		allowIncomplete: self allowIncomplete
		from: self
		process: Processor activeProcess! !

!KRMuThReportDisplayClient publicMethodsFor: 'connecting'!

connectReportPage

	| t1 t2 |
	(t1 := self currentPageNumber value) isNil ifTrue: [^self].
	(t2 := self numberOfPages value) isNil ifTrue: [^self].
	t2 < 1 ifTrue: [^self].
	(reportPage notNil and: [reportPage pageNumber = t1])
		ifTrue: [^self].
	(t1 < 1 or: [t1 > t2])
		ifTrue: 
			[t1 < 1
				ifTrue: [self currentPageNumber value: 1]
				ifFalse: [self currentPageNumber value: t2].
			^self].
	self disconnectReportPage.
	self showReportPage.
	(reportPage := self preferredPagesHolderClass new) reportDisplayClient: self.
	reportPage pageNumber: t1.
	reportPage page: (self pageNumbered: t1).
	(builder componentAt: self pagesHolderWidgetID) widget client: reportPage.
	reportPage postOpenWith: reportPage builder.
	^self!

disconnectReportPage

	reportPage isNil
		ifFalse: 
			[reportPage disconnect.
			reportPage := nil].
	^self!

hideReportPage

	(self builder componentAt: self pagesHolderWidgetID) beInvisible.
	^self!

showReportPage

	(self builder componentAt: self pagesHolderWidgetID) beVisible.
	^self! !

!KRMuThReportDisplayClient publicMethodsFor: 'constants'!

fileSubmenuIndex

	^self class fileSubmenuIndex!

firstPageMenuItemIndex

	^self class firstPageMenuItemIndex!

lastPageMenuItemIndex

	^self class lastPageMenuItemIndex!

nextPageMenuItemIndex

	^self class nextPageMenuItemIndex!

pageByNumberMenuItemIndex

	^self class pageByNumberMenuItemIndex!

pageSubmenuIndex

	^self class pageSubmenuIndex!

prevPageMenuItemIndex

	^self class prevPageMenuItemIndex!

printReportMenuItemIndex

	^self class printReportMenuItemIndex!

printReportUsingDialogMenuItemIndex

	^self class printReportUsingDialogMenuItemIndex!

processSubmenuIndex

	^self class processSubmenuIndex!

stopPrintJobMenuItemIndex

	^self class stopPrintJobMenuItemIndex!

stopReportMenuItemIndex

	^self class stopReportMenuItemIndex! !

!KRMuThReportDisplayClient publicMethodsFor: 'dlog:accessing'!

value

	(builder notNil and: [ builder window notNil and: [ builder window isOpen]]) ifTrue: [
		self updateDisplayState
	].

	^super value! !

!KRMuThReportDisplayClient publicMethodsFor: 'dlog:interface opening'!

doPostOpenActions

	self registerInterests.
	self informHasOpened.
	self setWindowLabel.
	self updateButtonsForNumberOfPages: 0.
	self updateButtonsForPageNumber: 0 numberOfPages: 0.
	self updateMenuForNumberOfPages: 0.
	self updateMenuForPageNumber: 0 numberOfPages: 0.
	^self!

openFrom: aSpec 
	"Open a dialog based on aSpec"

	| aValue |
	self allButOpenFrom: aSpec.
	builder openDialog.
	aValue := self accept value.
	self informHasClosed.
	self release.
	^aValue! !

!KRMuThReportDisplayClient publicMethodsFor: 'execution'!

displayReport

	self openDialog.
	^self!

displayReportConcurrently

	self open.
	^self!

printReport

	self launchPrintReportUsingDialog: false.
	^self!

printReportUsingDialog

	self launchPrintReportUsingDialog: true.
	^self! !

!KRMuThReportDisplayClient publicMethodsFor: 'initialize-release'!

defaultDisplayClientWindowLabel

	^'Report Viewer' copy!

disconnect

	self release.
	^self!

initDisplayClientWindowLabel

	displayClientWindowLabel := self defaultDisplayClientWindowLabel.
	^self!

release

	self releaseParts.
	reportBroker := nil.
	reportPage := nil.
	numberOfPages := nil.
	currentPageNumber := nil.
	reportViewerMenu := nil.
	displayClientWindowLabel := nil.
	^super release!

releaseParts

	numberOfPages isNil
		ifFalse: 
			[numberOfPages retractInterestsFor: self.
			numberOfPages release.
			numberOfPages := nil].
	currentPageNumber isNil
		ifFalse: 
			[currentPageNumber retractInterestsFor: self.
			currentPageNumber release.
			currentPageNumber := nil].
	^self! !

!KRMuThReportDisplayClient publicMethodsFor: 'interface opening'!

postBuildWith: t1 

	super postBuildWith: t1.
	self isDialog ifFalse: [self setWindowControllerWith: t1].
	self hideReportPage.
	^self!

postOpenWith: t1 

	self registerInterests.
	self informHasOpened.
	self setWindowLabel.
	self updateButtonsForNumberOfPages: 0.
	self updateButtonsForPageNumber: 0 numberOfPages: 0.
	self updateMenuForNumberOfPages: 0.
	self updateMenuForPageNumber: 0 numberOfPages: 0.
	^self!

registerInterests

	| t1 t2 |
	(t1 := self currentPageNumber) notNil ifTrue: [t1 onChangeSend: #currentPageNumberChanged to: self].
	(t2 := self numberOfPages) notNil ifTrue: [t2 onChangeSend: #numberOfPagesChanged to: self].
	^self!

setWindowControllerWith: theBuilder 

	| aController |
	(aController := self preferredReportDisplayControllerClass new) reportDisplayClient: self.
	theBuilder window controller: aController.! !

!KRMuThReportDisplayClient publicMethodsFor: 'poll-updating'!

updateDisplayState

	reportBroker isNil
		ifFalse: 
			[self updateNumberOfPages.
			self updateMonitorState].
	^self!

updateMonitorState

	| t1 t2 |
	t1 := self getReportState.
	t2 := self getPrintJobState.
	self updateButtonsForReportState: t1 printJobState: t2.
	self updateMenuForReportState: t1 printJobState: t2.
	^self!

updateNumberOfPages

	| t1 |
	(t1 := self getNumberOfPages) = self numberOfPages value ifFalse: [self numberOfPages value: t1].
	^self! !

!KRMuThReportDisplayClient publicMethodsFor: 'preferences'!

preferredPagesHolderClass

	^self class preferredPagesHolderClass!

preferredReportDisplayControllerClass

	^self class preferredReportDisplayControllerClass! !

!KRMuThReportDisplayClient publicMethodsFor: 'updating'!

closeRequest
self halt.
	super closeRequest.
	self informHasClosed.
	self release.
	^self!

currentPageNumberChanged

	| t1 t2 |
	(t1 := self currentPageNumber value) isNil ifTrue: [^self].
	(t2 := self numberOfPages value) isNil ifTrue: [^self].
	self updateButtonsForPageNumber: t1 numberOfPages: t2.
	self updateMenuForPageNumber: t1 numberOfPages: t2.
	Cursor execute showWhile: [self connectReportPage].
	^self!

numberOfPagesChanged

	| t1 t2 |
	(t1 := self numberOfPages value) isNil ifTrue: [^self].
	t2 := self currentPageNumber value.
	self updateButtonsForNumberOfPages: t1.
	self updateMenuForNumberOfPages: t1.
	(t1 > 0 and: [self currentPageNumber value isNil or: [self currentPageNumber value < 1]])
		ifTrue: [self currentPageNumber value: 1].
	t2 := self currentPageNumber value.
	self updateButtonsForPageNumber: t2 numberOfPages: t1.
	self updateMenuForPageNumber: t2 numberOfPages: t1.
	^self!

setWindowLabel

	self builder window label: self displayClientWindowLabel.
	^self!

updateButtonsForNumberOfPages: t1 

	t1 > 0
		ifTrue: [(self builder componentAt: self printJobButtonIDSymbol) enable]
		ifFalse: [(self builder componentAt: self printJobButtonIDSymbol) disable].
	^self!

updateButtonsForPageNumber: t1 numberOfPages: t2 

	(t1 < 1 or: [t2 < 1])
		ifTrue: 
			[(self builder componentAt: self firstPageButtonIDSymbol) disable.
			(self builder componentAt: self prevPageButtonIDSymbol) disable.
			(self builder componentAt: self nextPageButtonIDSymbol) disable.
			(self builder componentAt: self lastPageButtonIDSymbol) disable.
			^self].
	t1 > 1
		ifTrue: 
			[(self builder componentAt: self firstPageButtonIDSymbol) enable.
			(self builder componentAt: self prevPageButtonIDSymbol) enable]
		ifFalse: 
			[(self builder componentAt: self firstPageButtonIDSymbol) disable.
			(self builder componentAt: self prevPageButtonIDSymbol) disable].
	t1 < t2
		ifTrue: 
			[(self builder componentAt: self nextPageButtonIDSymbol) enable.
			(self builder componentAt: self lastPageButtonIDSymbol) enable]
		ifFalse: 
			[(self builder componentAt: self nextPageButtonIDSymbol) disable.
			(self builder componentAt: self lastPageButtonIDSymbol) disable].
	^self!

updateButtonsForReportState: t1 printJobState: t2 

	t1 == true
		ifTrue: [(self builder componentAt: self stopReportButtonIDSymbol) enable]
		ifFalse: [(self builder componentAt: self stopReportButtonIDSymbol) disable].
	t2 == true
		ifTrue: 
			[(self builder componentAt: self printJobButtonIDSymbol) disable.
			(self builder componentAt: self stopPrintJobButtonIDSymbol) enable]
		ifFalse: 
			[(self builder componentAt: self printJobButtonIDSymbol) enable.
			(self builder componentAt: self stopPrintJobButtonIDSymbol) disable].
	^self!

updateMenuForNumberOfPages: t1 

	| t2 t3 |
	t3 := ((t2 := self reportViewerMenu value) menuItemAt: self fileSubmenuIndex) submenu.
	t1 > 0
		ifTrue: 
			[(t3 menuItemAt: self printReportMenuItemIndex) enable.
			(t3 menuItemAt: self printReportUsingDialogMenuItemIndex) enable]
		ifFalse: 
			[(t3 menuItemAt: self printReportMenuItemIndex) disable.
			(t3 menuItemAt: self printReportUsingDialogMenuItemIndex) disable].
	^self!

updateMenuForPageNumber: t1 numberOfPages: t2 

	| t3 t4 |
	t4 := ((t3 := self reportViewerMenu value) menuItemAt: self pageSubmenuIndex) submenu.
	(t1 < 1 or: [t2 < 1])
		ifTrue: 
			[(t4 menuItemAt: self nextPageMenuItemIndex) disable.
			(t4 menuItemAt: self prevPageMenuItemIndex) disable.
			(t4 menuItemAt: self firstPageMenuItemIndex) disable.
			(t4 menuItemAt: self lastPageMenuItemIndex) disable.
			(t4 menuItemAt: self pageByNumberMenuItemIndex) disable.
			^self].
	(t4 menuItemAt: self pageByNumberMenuItemIndex) enable.
	t1 > 1
		ifTrue: 
			[(t4 menuItemAt: self firstPageMenuItemIndex) enable.
			(t4 menuItemAt: self prevPageMenuItemIndex) enable]
		ifFalse: 
			[(t4 menuItemAt: self firstPageMenuItemIndex) disable.
			(t4 menuItemAt: self prevPageMenuItemIndex) disable].
	t1 < t2
		ifTrue: 
			[(t4 menuItemAt: self nextPageMenuItemIndex) enable.
			(t4 menuItemAt: self lastPageMenuItemIndex) enable]
		ifFalse: 
			[(t4 menuItemAt: self nextPageMenuItemIndex) disable.
			(t4 menuItemAt: self lastPageMenuItemIndex) disable].
	^self!

updateMenuForReportState: t1 printJobState: t2 

	| t3 t4 |
	t4 := ((t3 := self reportViewerMenu value) menuItemAt: self processSubmenuIndex) submenu.
	t1 == true
		ifTrue: [(t4 menuItemAt: self stopReportMenuItemIndex) enable]
		ifFalse: [(t4 menuItemAt: self stopReportMenuItemIndex) disable].
	t2 == true
		ifTrue: [(t4 menuItemAt: self stopPrintJobMenuItemIndex) enable]
		ifFalse: [(t4 menuItemAt: self stopPrintJobMenuItemIndex) disable].
	^self! !

!KRMuThReportDisplayClient publicMethodsFor: 'widget-IDs'!

firstPageButtonIDSymbol

	^#firstPageButtonID!

lastPageButtonIDSymbol

	^#lastPageButtonID!

nextPageButtonIDSymbol

	^#nextPageButtonID!

pagesHolderWidgetID

	^self class pagesHolderWidgetID!

prevPageButtonIDSymbol

	^#prevPageButtonID!

printJobButtonIDSymbol

	^#printJobButtonID!

printReportButtonIDSymbol

	^#printReportButtonID!

printReportUsingDialogButtonIDSymbol

	^#printReportUsingDialogButtonID!

reportButtonIDSymbol

	^#reportButtonID!

stopPrintJobButtonIDSymbol

	^#stopPrintJobButtonID!

stopReportButtonIDSymbol

	^#stopReportButtonID! !

!KRMuThReportElement class publicMethodsFor: 'instance creation'!

model: t1 layout: t2 section: t3 page: t4 

	| t5 |
	(t5 := self new) model: t1; layout: t2; section: t3; page: t4.
	^t5!

spec: t1 layout: t2 page: t3 

	| t4 |
	(t4 := self new) spec: t1; layout: t2; page: t3.
	^t4! !

!KRMuThReportElement publicMethodsFor: 'accessing'!

isPageDivider

	^isPageDivider == true!

isPageDivider: t1 

	isPageDivider := t1 == true.
	^self!

layout

	^layout!

layout: t1 

	layout := t1.
	^self!

model

	^model!

model: t1 

	model := t1.
	^self!

page

	^page!

page: t1 

	page := t1.
	^self!

section

	^section!

section: t1 

	section := t1.
	^self!

spec

	^spec!

spec: t1 

	spec := t1.
	^self! !

!KRMuThReportElement publicMethodsFor: 'release'!

release

	model := nil.
	spec := nil.
	layout := nil.
	section := nil.
	page := nil.
	isPageDivider := nil.
	^super release! !

!KRMuThReportMonitor class publicMethodsFor: 'instance creation'!

newForReportBroker: t1 

	| t2 |
	(t2 := self new) reportBroker: t1.
	^t2! !

!KRMuThReportMonitor class publicMethodsFor: 'interface specs'!

dialogSpec

	^self windowSpec!

windowSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Report Monitor' #min: #(#Point 241 147) #bounds: #(#Rectangle 166 325 407 472) #isEventDriven: true) #component: #(#SpecCollection #collection: #(#(#TextEditorSpec #layout: #(#LayoutFrame 10 0 45 0 -10 1 0 0.666666) #name: #statusTextFieldID #model: #statusText #tabable: false #isReadOnly: true) #(#ActionButtonSpec #layout: #(#LayoutFrame 50 0 10 0.666666 -50 1 -10 1) #name: #closeRequestButtonID #model: #closeMonitor #label: 'Cancel' #isDefault: true #defaultable: true) #(#LabelSpec #layout: #(#AlignmentOrigin 0 0.5 10 0 0.5 0) #label: 'Report Monitor'))))! !

!KRMuThReportMonitor class publicMethodsFor: 'preferences'!

preferredReportDisplayControllerClass

	^KRMuThReportDisplayController! !

!KRMuThReportMonitor class publicMethodsFor: 'resources'!

closeRequestIcon

	^KRMuThReportDisplayClient closeRequestIcon!

stopPrintJobIcon

	^KRMuThReportDisplayClient stopPrintJobIcon!

stopReportIcon

	^KRMuThReportDisplayClient stopReportIcon! !

!KRMuThReportMonitor publicMethodsFor: 'accessing'!

reportBroker

	^reportBroker!

reportBroker: t1 

	reportBroker := t1.
	^self! !

!KRMuThReportMonitor publicMethodsFor: 'actions'!

closeMonitor

	self isDialog
		ifTrue: 
			[(Dialog confirm: 'Do you really want to cancel the Report ?' initialAnswer: false)
				ifFalse: [^nil].
			properlyFinished := false.
			^self stopProcessesAndCloseRequest].
	properlyFinished := false.
	self closeRequest.
	^self!

stopPrintJob

	self reportBroker rqstStopPrintJobFrom: self process: Processor activeProcess.
	^self!

stopProcessesAndCloseRequest

	self stopReport.
	self stopPrintJob.
	self closeRequest.
	^self!

stopReport

	self reportBroker rqstStopReportFrom: self process: Processor activeProcess.
	^self! !

!KRMuThReportMonitor publicMethodsFor: 'aspects'!

statusText

	statusText isNil ifTrue: [^statusText := '' copy asValue].
	^statusText! !

!KRMuThReportMonitor publicMethodsFor: 'broker services'!

getNumberOfPages

	^self reportBroker rqstNumberOfCompletedPagesFrom: self process: Processor activeProcess!

getNumberOfPagesSentToPrintJob

	^self reportBroker rqstNumberOfPagesSentToPrintJobFrom: self process: Processor activeProcess!

getPrintJobState

	^self reportBroker rqstPrintJobStateFrom: self process: Processor activeProcess!

getReportState

	^self reportBroker rqstReportStateFrom: self process: Processor activeProcess!

informHasClosed

	^self reportBroker informMonitorHasClosedFrom: self process: Processor activeProcess!

informHasOpened

	^self reportBroker informMonitorHasOpenedFrom: self process: Processor activeProcess! !

!KRMuThReportMonitor publicMethodsFor: 'dlog:interface opening'!

doPostOpenActions

	self informHasOpened.
	self updateDisplayState.
	^self!

newDialogControllerWithBuilder: t1 

	| t2 |
	(t2 := KRMuThReportDisplayDialogController new) reportDisplayClient: self.
	^t2! !

!KRMuThReportMonitor publicMethodsFor: 'execution'!

monitorReport

	properlyFinished := false.
	self openDialog.
	^properlyFinished!

monitorReportConcurrently

	properlyFinished := false.
	self open.
	^self! !

!KRMuThReportMonitor publicMethodsFor: 'initialize-release'!

defaultDisplayClientWindowLabel

	^'Report Watch' copy!

disconnect

	self release.
	^self!

release

	self releaseParts.
	reportBroker := nil.
	statusText := nil.
	oldReportState := nil.
	oldPrintJobState := nil.
	oldReportedNumberOfPages := nil.
	oldPrintingNumberOfPages := nil.
	^super release!

releaseParts

	^self! !

!KRMuThReportMonitor publicMethodsFor: 'interface opening'!

postBuildWith: t1 

	super postBuildWith: t1.
	self isDialog ifFalse: [self setWindowControllerWith: t1].
	^self!

postOpenWith: t1 

	self informHasOpened.
	self updateDisplayState.
	^self!

setWindowControllerWith: t1 

	| t2 |
	(t2 := self preferredReportDisplayControllerClass new) reportDisplayClient: self.
	t1 window controller: t2.
	^self! !

!KRMuThReportMonitor publicMethodsFor: 'poll-updating'!

updateDisplayState

	| t1 t2 t3 t4 t5 t6 |
	reportBroker isNil ifTrue: [^self].
	(t2 := (t2 := self getPrintJobState) == true)
		ifFalse: 
			[properlyFinished := true.
			^self stopProcessesAndCloseRequest].
	t1 := (t1 := self getReportState) == true.
	t3 := ((t3 := self getNumberOfPages) isKindOf: Number)
				ifTrue: [t3]
				ifFalse: [0].
	t4 := ((t4 := self getNumberOfPagesSentToPrintJob) isKindOf: Number)
				ifTrue: [t4]
				ifFalse: [0].
	(t1 == oldReportState and: [t2 == oldPrintJobState and: [t3 == oldReportedNumberOfPages and: [t4 == oldPrintingNumberOfPages]]])
		ifTrue: [^self].
	oldReportState := t1.
	oldPrintJobState := t2.
	oldReportedNumberOfPages := t3.
	oldPrintingNumberOfPages := t4.
	(t5 := WriteStream on: (String new: 128)) nextPutAll: 'Printing page '; print: t4; nextPutAll: ' of '; print: t3; cr.
	t1 ifTrue: [t5 nextPutAll: ' (and growing).'].
	t6 := t5 contents.
	self statusText value: t6.
	^self! !

!KRMuThReportMonitor publicMethodsFor: 'preferences'!

preferredReportDisplayControllerClass

	^self class preferredReportDisplayControllerClass! !

!KRMuThReportMonitor publicMethodsFor: 'updating'!

closeRequest

	super closeRequest.
	self informHasClosed.
	self release.
	^self! !

!KRMuThReportPage publicMethodsFor: 'accessing'!

addElement: t1 

	self elements add: t1.
	^self!

addElement: t1 wasMissing: t2 

	self elements add: t1.
	t2 == true ifTrue: [self oneLessMissingElement].
	^self!

elements

	elements isNil ifTrue: [self initElements].
	^elements!

hasMissingElements

	^self missingElements > 0!

missingElements

	missingElements isNil ifTrue: [self initMissingElements].
	^missingElements!

oneLessMissingElement

	^missingElements := self missingElements - 1 max: 0!

oneMoreMissingElement

	^missingElements := self missingElements + 1!

page

	^self!

pageNumber

	pageNumber isNil ifTrue: [self initPageNumber].
	^pageNumber!

pageNumber: t1 

	pageNumber := t1.
	^self!

translation

	translation isNil ifTrue: [self initTranslation].
	^translation!

translation: t1 

	translation := t1.
	^self! !

!KRMuThReportPage publicMethodsFor: 'copying'!

postCopy

	super postCopy.
	elements := elements copy.
	missingElements := missingElements copy.
	^self! !

!KRMuThReportPage publicMethodsFor: 'initialize-release'!

initElements

	elements := OrderedCollection new.
	^self!

initMissingElements

	missingElements := 0.
	^self!

initPageNumber

	pageNumber := 1.
	^self!

initTranslation

	translation := 0 @ 0.
	^self!

release

	pageNumber := nil.
	translation := nil.
	elements := nil.
	missingElements := nil.
	^super release! !

!KRMuThReportPageDisplayClient class publicMethodsFor: 'interface specs'!

windowSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Report Viewer' #min: #(#Point 50 50) #bounds: #(#Rectangle 87 143 727 563)) #component: #(#SpecCollection #collection: #()))! !

!KRMuThReportPageDisplayClient publicMethodsFor: 'accessing'!

page

	^page!

page: t1 

	page := t1.
	^self!

pageNumber

	^pageNumber!

pageNumber: t1 

	pageNumber := t1.
	^self!

reportDisplayClient

	^reportDisplayClient!

reportDisplayClient: t1 

	reportDisplayClient := t1.
	^self! !

!KRMuThReportPageDisplayClient publicMethodsFor: 'initialize-release'!

disconnect

	self release.
	^self!

release

	self releaseParts.
	reportDisplayClient := nil.
	pageNumber := nil.
	page := nil.
	^super release!

releaseParts

	^self! !

!KRMuThReportPageDisplayClient publicMethodsFor: 'interface opening'!

nameForSubCanvasNumber: t1 

	^('subCanvas' , t1 printString , 'ID') asSymbol!

postBuildWith: t1 

	self addElementsWith: t1.
	^self!

postOpenWith: t1 

	self connectElementsWith: t1.
	page := nil.
	^self! !

!KRMuThReportPageDisplayClient publicMethodsFor: 'ojo'!

ojo

	^self! !

!KRMuThReportPageDisplayClient publicMethodsFor: 'widgets-accessing'!

currentPageElements

	self page isNil ifTrue: [^nil].
	^self page elements!

translationForCurrentPage

	self page isNil ifTrue: [^nil].
	^self page translation! !

!KRMuThReportPageDisplayClient publicMethodsFor: 'widgets-building'!

addElementsWith: t1 

	| t2 t3 |
	((t2 := self currentPageElements) isNil or: [t2 isEmpty])
		ifTrue: [^self].
	(t3 := self translationForCurrentPage) isNil ifTrue: [^self].
	(1 to: t2 size)
		do: 
			[:t4 | 
			| t5 t6 |
			(t5 := t2 at: t4) model isNil
				ifTrue: [t5 isPageDivider
						ifTrue: [nil]
						ifFalse: [self
								addWidget: t5
								with: t1
								translated: t3]]
				ifFalse: 
					[t6 := t4 = t2 size
								ifTrue: [nil]
								ifFalse: [t2 at: t4 + 1].
					self
						addSubCanvas: t5
						with: t1
						number: t4
						translated: t3
						nextElement: t6]].
	^self!

addSubCanvas: t1 with: t2 number: t3 translated: t4 nextElement: t5 

	| t6 t7 t8 t9 t10 |
	t6 := t1 layout.
	t10 := t5 notNil and: [t5 model notNil and: [t5 model class = t1 model class and: [(t1 section at: #myInterfaceSpecSelector)
							= (t5 section at: #myInterfaceSpecSelector) and: [t5 layout topFraction = t6 bottomFraction and: [t5 layout topOffset = t6 bottomOffset]]]]].
	t9 := self
				copyTranslateLayout: t6
				by: t4
				expandBottom: t10 not.
	t7 := self nameForSubCanvasNumber: t3.
	t8 := (SubCanvasSpec new) name: t7; layout: t9; hasBorder: false.
	t2 add: t8.
	^self!

addWidget: t1 with: t2 translated: t3 

	| t4 t5 t6 |
	t4 := t1 spec.
	t5 := t1 layout.
	t6 := self copyTranslateLayout: t5 by: t3.
	t4 layout: t6.
	t2 add: t4.
	^self!

copyTranslateLayout: t1 by: t2 

	^self
		copyTranslateLayout: t1
		by: t2
		expandBottom: false!

copyTranslateLayout: t1 by: t2 expandBottom: t3 

	| t4 |
	t1 isNil ifTrue: [^nil].
	t2 isNil ifTrue: [^t1].
	(t4 := t1 copy) leftOffset: t1 leftOffset + t2 x.
	t4 topOffset: t1 topOffset + t2 y.
	t4 rightOffset: t1 rightOffset + t2 x.
	t4 bottomOffset: t1 bottomOffset + t2 y + (t3
				ifTrue: [1]
				ifFalse: [0]).
	^t4! !

!KRMuThReportPageDisplayClient publicMethodsFor: 'widgets-connecting'!

connectElementsWith: t1 

	| t2 |
	((t2 := self currentPageElements) isNil or: [t2 isEmpty])
		ifTrue: [^self].
	(1 to: t2 size)
		do: 
			[:t3 | 
			| t4 |
			(t4 := t2 at: t3) model notNil
				ifTrue: [self
						connectSubCanvas: t4
						with: t1
						number: t3]
				ifFalse: [nil]].
	^self!

connectSubCanvas: t1 with: t2 number: t3 

	| t4 t5 t6 t7 t8 t9 t10 |
	t5 := t1 model.
	t6 := t1 section.
	t8 := t1 page.
	t4 := self nameForSubCanvasNumber: t3.
	t7 := t6 at: #myInterfaceSpecSelector.
	t9 := builder componentAt: t4.
	t10 := Object messageNotUnderstoodSignal handle: [:t11 | t11 returnWith: t7]
				do: [t5 perform: t7].
	t9 widget client: t5 spec: t10.
	t5 postOpenWith: t5 builder.
	self ojo.
	^self! !

!KRMuThReportPageLayout class publicMethodsFor: 'constants'!

a3HeightCm

	^self a4WidthCm * 2!

a3WidthCm

	^self a4HeightCm!

a4HeightCm

	^29.7!

a4HeightInch

	^self cmToInch: self a4HeightCm!

a4WidthCm

	^21!

a4WidthInch

	^self cmToInch: self a4WidthCm!

a5HeightCm

	^self a4WidthCm!

a5WidthCm

	^self a4HeightCm / 2!

b5HeightCm

	^25!

b5WidthCm

	^17.6!

customName

	^'Custom' copy!

defaultMargin

	^0!

laserNoPrintableHeightCm

	^self inchToCm: self laserNoPrintableHeightInch!

laserNoPrintableHeightInch

	^0.5!

laserNoPrintableWidthCm

	^self inchToCm: self laserNoPrintableWidthInch!

laserNoPrintableWidthInch

	^0.5!

legalHeightInch

	^14!

legalWidthInch

	^8.5!

letterHeightInch

	^11!

letterWidthInch

	^8.5!

tabloidHeightInch

	^self letterWidthInch * 2!

tabloidWidthInch

	^self letterHeightInch! !

!KRMuThReportPageLayout class publicMethodsFor: 'conversion'!

cmToInch: t1 

	^self cmToInchFactor * t1!

cmToInchFactor

	^1 / self inchToCmFactor!

cmToMm: t1 

	^self cmToMmFactor * t1!

cmToMmFactor

	^10!

convert: t1 toCm: t2 

	t1 = self cmUnitSymbol ifTrue: [^t2].
	t1 = self inchUnitSymbol ifTrue: [^self inchToCm: t2].
	t1 = self mmUnitSymbol ifTrue: [^self mmToCm: t2].
	^t2!

convert: t1 toInch: t2 

	t1 = self inchUnitSymbol ifTrue: [^t2].
	t1 = self cmUnitSymbol ifTrue: [^self cmToInch: t2].
	t1 = self mmUnitSymbol ifTrue: [^self mmToInch: t2].
	^t2!

convert: t1 toMm: t2 

	t1 = self mmUnitSymbol ifTrue: [^t2].
	t1 = self inchUnitSymbol ifTrue: [^self inchToMm: t2].
	t1 = self cmUnitSymbol ifTrue: [^self cmToMm: t2].
	^t2!

convert: t1 toUnit: t2 value: t3 

	t2 = self cmUnitSymbol ifTrue: [^self convert: t1 toCm: t3].
	t2 = self inchUnitSymbol ifTrue: [^self convert: t1 toInch: t3].
	t2 = self mmUnitSymbol ifTrue: [^self convert: t1 toMm: t3].
	^t3!

inchToCm: t1 

	^self inchToCmFactor * t1!

inchToCmFactor

	^self inchToMmFactor / 10!

inchToMm: t1 

	^self inchToMmFactor * t1!

inchToMmFactor

	^25.4!

mmToCm: t1 

	^self mmToCmFactor * t1!

mmToCmFactor

	^1 / self cmToMmFactor!

mmToInch: t1 

	^self mmToInchFactor * t1!

mmToInchFactor

	^1 / self inchToMmFactor! !

!KRMuThReportPageLayout class publicMethodsFor: 'decoding'!

decodeFromLiteralArray: t1 

	^self new fromLiteralArrayEncoding: t1! !

!KRMuThReportPageLayout class publicMethodsFor: 'defaults'!

defaultBottomMargin

	^self defaultMargin!

defaultHeight

	^self letterHeightInch!

defaultLeftMargin

	^self defaultMargin!

defaultName

	^'Custom' copy!

defaultNoPrintableHeight

	^self laserNoPrintableHeightInch!

defaultNoPrintableWidth

	^self laserNoPrintableWidthInch!

defaultRightMargin

	^self defaultMargin!

defaultTopMargin

	^self defaultMargin!

defaultUnit

	^self inchUnitSymbol!

defaultWidth

	^self letterWidthInch! !

!KRMuThReportPageLayout class publicMethodsFor: 'instance creation'!

createCustomFromPrintableInchWidth: t1 andHeight: t2 

	| t3 |
	(t3 := self newCustom) setFromPrintableInchWidth: t1 andHeight: t2.
	^t3!

findEquivalentLayoutTo: t1 among: t2 

	| t3 t4 |
	(t3 := t2 detect: [:t5 | t5 isSameSizesAs: t1]
				ifNone: [nil]) isNil ifFalse: [^t3].
	t4 := t1 bePortrait.
	(t3 := t2 detect: [:t6 | t6 isSameSizesAs: t4]
				ifNone: [nil]) isNil ifFalse: [^t3 beLandscape].
	t4 := t1 beLandscape.
	(t3 := t2 detect: [:t7 | t7 isSameSizesAs: t4]
				ifNone: [nil]) isNil ifFalse: [^t3 bePortrait].
	^nil!

findOrCreateCustomFromPrintableInchWidth: t1 andHeight: t2 

	| t3 t4 |
	(t3 := self createCustomFromPrintableInchWidth: t1 andHeight: t2) isNil ifTrue: [^nil].
	(t4 := self findEquivalentLayoutTo: t3 among: self predefinedReportPageLayouts) isNil ifTrue: [^t3].
	^t4!

newA3

	^self newA3NoMargins!

newA3NoMargins

	^self newA3NoMarginsPortrait!

newA3NoMarginsLandscape

	^self newA3NoMarginsPortrait beLandscape!

newA3NoMarginsPortrait

	| t1 |
	(t1 := self new initialize) name: self a3NameSymbol.
	t1 unitPrivate: self cmUnitSymbol.
	t1 width: self a3WidthCm.
	t1 height: self a3HeightCm.
	t1 leftMargin: 0.
	t1 topMargin: 0.
	t1 rightMargin: 0.
	t1 bottomMargin: 0.
	t1 noPrintableWidth: self laserNoPrintableWidthCm.
	t1 noPrintableHeight: self laserNoPrintableHeightCm.
	^t1!

newA4

	^self newA4NoMargins!

newA4NoMargins

	^self newA4NoMarginsPortrait!

newA4NoMarginsLandscape

	^self newA4NoMarginsPortrait beLandscape!

newA4NoMarginsPortrait

	| t1 |
	(t1 := self new initialize) name: self a4NameSymbol.
	t1 unitPrivate: self cmUnitSymbol.
	t1 width: self a4WidthCm.
	t1 height: self a4HeightCm.
	t1 leftMargin: 0.
	t1 topMargin: 0.
	t1 rightMargin: 0.
	t1 bottomMargin: 0.
	t1 noPrintableWidth: self laserNoPrintableWidthCm.
	t1 noPrintableHeight: self laserNoPrintableHeightCm.
	^t1!

newA5

	^self newA5NoMargins!

newA5NoMargins

	^self newA5NoMarginsPortrait!

newA5NoMarginsLandscape

	^self newA5NoMarginsPortrait beLandscape!

newA5NoMarginsPortrait

	| t1 |
	(t1 := self new initialize) name: self a5NameSymbol.
	t1 unitPrivate: self cmUnitSymbol.
	t1 width: self a5WidthCm.
	t1 height: self a5HeightCm.
	t1 leftMargin: 0.
	t1 topMargin: 0.
	t1 rightMargin: 0.
	t1 bottomMargin: 0.
	t1 noPrintableWidth: self laserNoPrintableWidthCm.
	t1 noPrintableHeight: self laserNoPrintableHeightCm.
	^t1!

newB5

	^self newB5NoMargins!

newB5NoMargins

	^self newB5NoMarginsPortrait!

newB5NoMarginsLandscape

	^self newB5NoMarginsPortrait beLandscape!

newB5NoMarginsPortrait

	| t1 |
	(t1 := self new initialize) name: self b5NameSymbol.
	t1 unitPrivate: self cmUnitSymbol.
	t1 width: self b5WidthCm.
	t1 height: self b5HeightCm.
	t1 leftMargin: 0.
	t1 topMargin: 0.
	t1 rightMargin: 0.
	t1 bottomMargin: 0.
	t1 noPrintableWidth: self laserNoPrintableWidthCm.
	t1 noPrintableHeight: self laserNoPrintableHeightCm.
	^t1!

newCustom

	| t1 |
	(t1 := self new initialize) name: self customNameSymbol.
	t1 unitPrivate: self inchUnitSymbol.
	t1 width: self letterWidthInch.
	t1 height: self letterHeightInch.
	t1 leftMargin: 0.
	t1 topMargin: 0.
	t1 rightMargin: 0.
	t1 bottomMargin: 0.
	t1 noPrintableWidth: self laserNoPrintableWidthInch.
	t1 noPrintableHeight: self laserNoPrintableHeightInch.
	^t1!

newDefault

	^self newLetterNoMarginsPortrait!

newLegal

	^self newLegalNoMargins!

newLegalNoMargins

	^self newLegalNoMarginsPortrait!

newLegalNoMarginsLandscape

	^self newLegalNoMarginsPortrait beLandscape!

newLegalNoMarginsPortrait

	| t1 |
	(t1 := self new initialize) name: self legalNameSymbol.
	t1 unitPrivate: self inchUnitSymbol.
	t1 width: self legalWidthInch.
	t1 height: self legalHeightInch.
	t1 leftMargin: 0.
	t1 topMargin: 0.
	t1 rightMargin: 0.
	t1 bottomMargin: 0.
	t1 noPrintableWidth: self laserNoPrintableWidthInch.
	t1 noPrintableHeight: self laserNoPrintableHeightInch.
	^t1!

newLetter

	^self newLetterNoMargins!

newLetterNoMargins

	^self newLetterNoMarginsPortrait!

newLetterNoMarginsLandscape

	^self newLetterNoMarginsPortrait beLandscape!

newLetterNoMarginsPortrait

	| t1 |
	(t1 := self new initialize) name: self letterNameSymbol.
	t1 unitPrivate: self inchUnitSymbol.
	t1 width: self letterWidthInch.
	t1 height: self letterHeightInch.
	t1 leftMargin: 0.
	t1 topMargin: 0.
	t1 rightMargin: 0.
	t1 bottomMargin: 0.
	t1 noPrintableWidth: self laserNoPrintableWidthInch.
	t1 noPrintableHeight: self laserNoPrintableHeightInch.
	^t1!

newTabloid

	^self newTabloidNoMargins!

newTabloidNoMargins

	^self newTabloidNoMarginsPortrait!

newTabloidNoMarginsLandscape

	^self newTabloidNoMarginsPortrait beLandscape!

newTabloidNoMarginsPortrait

	| t1 |
	(t1 := self new initialize) name: self tabloidNameSymbol.
	t1 unitPrivate: self inchUnitSymbol.
	t1 width: self tabloidWidthInch.
	t1 height: self tabloidHeightInch.
	t1 leftMargin: 0.
	t1 topMargin: 0.
	t1 rightMargin: 0.
	t1 bottomMargin: 0.
	t1 noPrintableWidth: self laserNoPrintableWidthInch.
	t1 noPrintableHeight: self laserNoPrintableHeightInch.
	^t1! !

!KRMuThReportPageLayout class publicMethodsFor: 'predefined'!

predefinedReportPageLayouts

	^(OrderedCollection new: 10)
		add: self newLetterNoMarginsPortrait;
		add: self newLegalNoMarginsPortrait;
		add: self newTabloidNoMarginsPortrait;
		add: self newA3NoMarginsPortrait;
		add: self newA4NoMarginsPortrait;
		add: self newA5NoMarginsPortrait;
		add: self newB5NoMarginsPortrait;
		add: self newCustom; yourself!

predefinedReportPageLayoutsAndInitialWith: t1 

	| t2 t3 t4 |
	t2 := self predefinedReportPageLayouts.
	t1 isNil ifTrue: [^t2].
	t1 isCustom
		ifTrue: 
			[t4 := (t3 := self findEquivalentLayoutTo: t1 among: t2) isNil
						ifTrue: [(t2 reject: [:t5 | t5 isCustom]) asArray , (Array with: t1)]
						ifFalse: [t2 collect: [:t6 | t6 == t3
									ifTrue: [t3]
									ifFalse: [t6]]].
			^Array with: t4 with: t3].
	t4 := t2 collect: [:t7 | t7 name = t1 name
					ifTrue: [t1]
					ifFalse: [t7]].
	^Array with: t4 with: t1!

predefinedReportPageLayoutsWith: t1 

	| t2 t3 |
	t2 := self predefinedReportPageLayouts.
	t1 isNil ifTrue: [^t2].
	t1 isCustom
		ifTrue: 
			[(t3 := self findEquivalentLayoutTo: t1 among: t2) isNil ifTrue: [^(t2 reject: [:t4 | t4 isCustom]) asArray , (Array with: t1)].
			^t2 collect: [:t5 | t5 == t3
					ifTrue: [t3]
					ifFalse: [t5]]].
	^t2 collect: [:t6 | t6 name = t1 name
			ifTrue: [t1]
			ifFalse: [t6]]! !

!KRMuThReportPageLayout class publicMethodsFor: 'symbols'!

a3NameSymbol

	^#A3!

a4NameSymbol

	^#A4!

a5NameSymbol

	^#A5!

allNameSymbols

	^((OrderedCollection new: 10)
		add: self letterNameSymbol;
		add: self legalNameSymbol;
		add: self tabloidNameSymbol;
		add: self a3NameSymbol;
		add: self a4NameSymbol;
		add: self a5NameSymbol;
		add: self b5NameSymbol;
		add: self customNameSymbol; yourself) asArray!

allOrientationSymbols

	^Array with: self portraitOrientationSymbol with: self landscapeOrientationSymbol!

allUnitSymbols

	^Array
		with: self inchUnitSymbol
		with: self cmUnitSymbol
		with: self mmUnitSymbol!

b5NameSymbol

	^#B5!

cmUnitSymbol

	^#cm!

customNameSymbol

	^#Custom!

inchUnitSymbol

	^#in!

landscapeOrientationSymbol

	^#Landscape!

legalNameSymbol

	^#Legal!

letterNameSymbol

	^#Letter!

mmUnitSymbol

	^#mm!

portraitOrientationSymbol

	^#Portrait!

tabloidNameSymbol

	^#Tabloid! !

!KRMuThReportPageLayout class publicMethodsFor: 'testing'!

isKnownUnit: t1 

	t1 = self mmUnitSymbol ifTrue: [^true].
	t1 = self cmUnitSymbol ifTrue: [^true].
	^t1 = self inchUnitSymbol! !

!KRMuThReportPageLayout publicMethodsFor: 'basic-accessing'!

bottomMargin

	bottomMargin isNil ifTrue: [self initBottomMargin].
	^bottomMargin!

bottomMargin: t1 

	bottomMargin := t1.
	self changed: #bottomMargin.
	^self!

height

	height isNil ifTrue: [self initHeight].
	^height!

height: t1 

	height := t1.
	self changed: #height.
	self changed: #orientation.
	^self!

leftMargin

	leftMargin isNil ifTrue: [self initLeftMargin].
	^leftMargin!

leftMargin: t1 

	leftMargin := t1.
	self changed: #leftMargin.
	^self!

name

	name isNil ifTrue: [self initName].
	^name!

name: t1 

	name := t1.
	self changed: #name.
	^self!

noPrintableHeight

	noPrintableHeight isNil ifTrue: [self initNoPrintableHeight].
	^noPrintableHeight!

noPrintableHeight: t1 

	noPrintableHeight := t1.
	self changed: #noPrintableHeight.
	^self!

noPrintableWidth

	noPrintableWidth isNil ifTrue: [self initNoPrintableWidth].
	^noPrintableWidth!

noPrintableWidth: t1 

	noPrintableWidth := t1.
	self changed: #noPrintableWidth.
	^self!

orientation

	self isLandscape ifTrue: [^self class landscapeOrientationSymbol].
	^self class portraitOrientationSymbol!

orientation: t1 

	t1 = self class landscapeOrientationSymbol ifTrue: [self beLandscape].
	t1 = self class portraitOrientationSymbol ifTrue: [self bePortrait].
	self changed: #orientation.
	^self!

rightMargin

	rightMargin isNil ifTrue: [self initRightMargin].
	^rightMargin!

rightMargin: t1 

	rightMargin := t1.
	self changed: #rightMargin.
	^self!

topMargin

	topMargin isNil ifTrue: [self initTopMargin].
	^topMargin!

topMargin: t1 

	topMargin := t1.
	self changed: #topMargin.
	^self!

unit

	unit isNil ifTrue: [self initUnit].
	^unit!

unit: t1 

	self convertToUnit: t1.
	self changed: #unit.
	^self!

width

	width isNil ifTrue: [self initWidth].
	^width!

width: t1 

	width := t1.
	self changed: #width.
	self changed: #orientation.
	^self! !

!KRMuThReportPageLayout publicMethodsFor: 'converted-accessing'!

bottomMarginInch

	^self class convert: self unit toInch: self bottomMargin!

heightInch

	^self class convert: self unit toInch: self height!

leftMarginInch

	^self class convert: self unit toInch: self leftMargin!

noPrintableHeightInch

	^self class convert: self unit toInch: self noPrintableHeight!

noPrintableWidthInch

	^self class convert: self unit toInch: self noPrintableWidth!

printableHeightInch

	^self heightInch - self noPrintableHeightInch!

printableWidthInch

	^self widthInch - self noPrintableWidthInch!

rightMarginInch

	^self class convert: self unit toInch: self rightMargin!

setFromPrintableInchWidth: t1 andHeight: t2 

	| t3 t4 |
	t3 := self class
				convert: self class inchUnitSymbol
				toUnit: self unit
				value: t1.
	t4 := self class
				convert: self class inchUnitSymbol
				toUnit: self unit
				value: t2.
	self width: t3 + self noPrintableWidth.
	self height: t4 + self noPrintableHeight.
	^self!

topMarginInch

	^self class convert: self unit toInch: self topMargin!

widthInch

	^self class convert: self unit toInch: self width! !

!KRMuThReportPageLayout publicMethodsFor: 'converting'!

literalArrayEncoding

	| t1 t2 |
	t1 := #(#name #unitPrivate #width #height #leftMargin #topMargin #rightMargin #bottomMargin #noPrintableHeight #noPrintableWidth).
	(t2 := OrderedCollection new: t1 size * 2) add: self class name.
	t1 do: 
		[:t3 | 
		| t4 |
		(t4 := self perform: t3) == nil
			ifTrue: [nil]
			ifFalse: 
				[t2 add: (t3 , ':') asSymbol.
				t2 add: t4 literalArrayEncoding]].
	^t2 asArray! !

!KRMuThReportPageLayout publicMethodsFor: 'decoding'!

fromLiteralArrayEncoding: t1 

	| t2 t3 |
	t2 := 2.
	t3 := t1 size.
	[t2 <= t3]
		whileTrue: 
			[self perform: (t1 at: t2)
				with: (t1 at: t2 + 1).
			t2 := t2 + 2].
	^self! !

!KRMuThReportPageLayout publicMethodsFor: 'defaults'!

defaultBottomMargin

	^self class defaultBottomMargin!

defaultHeight

	^self class defaultHeight!

defaultLeftMargin

	^self class defaultLeftMargin!

defaultName

	^self class defaultName!

defaultNoPrintableHeight

	^self class defaultNoPrintableHeight!

defaultNoPrintableWidth

	^self class defaultNoPrintableWidth!

defaultRightMargin

	^self class defaultRightMargin!

defaultTopMargin

	^self class defaultTopMargin!

defaultUnit

	^self class defaultUnit!

defaultWidth

	^self class defaultWidth! !

!KRMuThReportPageLayout publicMethodsFor: 'initialize-release'!

initBottomMargin

	topMargin := self defaultBottomMargin.
	^self!

initHeight

	height := self defaultHeight.
	^self!

initialize

	self initName.
	self initUnit.
	self initWidth.
	self initHeight.
	self initLeftMargin.
	self initTopMargin.
	self initRightMargin.
	self initBottomMargin.
	^self!

initLeftMargin

	leftMargin := self defaultLeftMargin.
	^self!

initName

	name := self defaultName.
	^self!

initNoPrintableHeight

	noPrintableHeight := self defaultNoPrintableHeight.
	^self!

initNoPrintableWidth

	noPrintableWidth := self defaultNoPrintableWidth.
	^self!

initRightMargin

	rightMargin := self defaultRightMargin.
	^self!

initTopMargin

	topMargin := self defaultTopMargin.
	^self!

initUnit

	unit := self defaultUnit.
	^self!

initWidth

	width := self defaultWidth.
	^self!

release

	name := nil.
	unit := nil.
	width := nil.
	height := nil.
	leftMargin := nil.
	topMargin := nil.
	rightMargin := nil.
	bottomMargin := nil.
	noPrintableHeight := nil.
	noPrintableWidth := nil.
	^super release! !

!KRMuThReportPageLayout publicMethodsFor: 'operations'!

beLandscape

	self isLandscape ifTrue: [^self].
	^self rotateClockwise!

bePortrait

	self isPortrait ifTrue: [^self].
	^self rotateCounterClockwise!

convertToUnit: t1 

	| t2 |
	(self isKnownUnit: t1)
		ifFalse: [^self].
	t2 := self unit.
	self width: (self class
			convert: t2
			toUnit: t1
			value: self width).
	self height: (self class
			convert: t2
			toUnit: t1
			value: self height).
	self leftMargin: (self class
			convert: t2
			toUnit: t1
			value: self leftMargin).
	self topMargin: (self class
			convert: t2
			toUnit: t1
			value: self topMargin).
	self rightMargin: (self class
			convert: t2
			toUnit: t1
			value: self rightMargin).
	self bottomMargin: (self class
			convert: t2
			toUnit: t1
			value: self bottomMargin).
	self noPrintableHeight: (self class
			convert: t2
			toUnit: t1
			value: self noPrintableHeight).
	self noPrintableWidth: (self class
			convert: t2
			toUnit: t1
			value: self noPrintableWidth).
	unit := t1.
	^self!

isLandscape

	^self width >= self height!

isPortrait

	^self width <= self height!

rotateClockwise

	| t1 t2 t3 t4 t5 t6 t7 t8 |
	t1 := self width.
	t2 := self height.
	t6 := self leftMargin.
	t3 := self topMargin.
	t4 := self rightMargin.
	t5 := self bottomMargin.
	t7 := self noPrintableWidth.
	t8 := self noPrintableHeight.
	self width: t2.
	self height: t1.
	self leftMargin: t5.
	self topMargin: t6.
	self rightMargin: t3.
	self bottomMargin: t4.
	self noPrintableWidth: t8.
	self noPrintableHeight: t7.
	^self!

rotateCounterClockwise

	| t1 t2 t3 t4 t5 t6 t7 t8 |
	t1 := self width.
	t2 := self height.
	t6 := self leftMargin.
	t3 := self topMargin.
	t4 := self rightMargin.
	t5 := self bottomMargin.
	t7 := self noPrintableWidth.
	t8 := self noPrintableHeight.
	self width: t2.
	self height: t1.
	self leftMargin: t3.
	self topMargin: t4.
	self rightMargin: t5.
	self bottomMargin: t6.
	self noPrintableWidth: t8.
	self noPrintableHeight: t7.
	^self! !

!KRMuThReportPageLayout publicMethodsFor: 'printing'!

layoutNameAndDimensionsDescriptionString

	| t1 |
	(t1 := WriteStream on: (String new: 128)) nextPutAll: self name; nextPutAll: ' ('; nextPutAll: self unit; nextPutAll: ')'; nextPutAll: ' Print Area : W '; print: self width; nextPutAll: '  H '; print: self height; nextPutAll: ' - Margins : L '; print: self leftMargin; nextPutAll: '  R '; print: self rightMargin; nextPutAll: '  T '; print: self topMargin; nextPutAll: '  B '; print: self bottomMargin.
	^t1 contents!

printOn: t1 

	t1 nextPutAll: '( a'; nextPutAll: self class name; nextPutAll: ' named: '''; nextPutAll: self name; nextPutAll: ''' <'; nextPutAll: self unit; nextPutAll: '> W '; print: self width; nextPutAll: '  H '; print: self height; nextPutAll: '  LM '; print: self leftMargin; nextPutAll: '  TM '; print: self topMargin; nextPutAll: '  RM '; print: self rightMargin; nextPutAll: '  BM '; print: self bottomMargin; nextPutAll: ' ) '.
	^self! !

!KRMuThReportPageLayout publicMethodsFor: 'private-accessing'!

unitPrivate

	^unit!

unitPrivate: t1 

	unit := t1.
	^self! !

!KRMuThReportPageLayout publicMethodsFor: 'testing'!

isCustom

	^self name = self class customNameSymbol!

isKnownUnit: t1 

	^self class isKnownUnit: t1!

isSameSizesAs: t1 

	t1 isNil ifTrue: [^false].
	self widthInch = t1 widthInch
		ifTrue: 
			[self heightInch = t1 heightInch
				ifTrue: 
					[self leftMarginInch = t1 leftMarginInch
						ifTrue: 
							[self topMarginInch = t1 topMarginInch
								ifTrue: 
									[self rightMarginInch = t1 rightMarginInch
										ifTrue: 
											[self bottomMarginInch = t1 bottomMarginInch
												ifTrue: 
													[self noPrintableHeightInch = t1 noPrintableHeightInch ifTrue: [^self noPrintableWidthInch = t1 noPrintableWidthInch].
													^false].
											^false].
									^false].
							^false].
					^false].
			^false].
	^false! !

!KRMuThReportPageLayoutChooser class publicMethodsFor: 'examples'!

example1

	self open.
	^self!

example2

	^self choosePageLayout! !

!KRMuThReportPageLayoutChooser class publicMethodsFor: 'instance creation'!

choosePageLayout

	^self openDialog!

choosePageLayoutFrom: t1 initialNamed: t2 

	| t3 |
	t3 := self new.
	(t1 isNil or: [t1 isEmpty])
		ifTrue: [t3 predefinedLayouts: KRMuThReportPageLayout predefinedReportPageLayouts]
		ifFalse: [t3 predefinedLayouts: t1].
	t2 isNil ifFalse: [t3 initialLayoutName: t2].
	^t3 openDialog!

choosePageLayoutInitial: t1 

	| t2 t3 t4 t5 |
	t2 := self new.
	((t3 := KRMuThReportPageLayout predefinedReportPageLayoutsAndInitialWith: t1) notNil and: [t3 size >= 2])
		ifTrue: 
			[t4 := t3 first.
			t5 := t3 at: 2.
			t2 predefinedLayouts: t4.
			t2 initialLayoutName: t5 name].
	^t2 openDialog!

choosePageLayoutInitialNamed: t1 

	| t2 |
	(t2 := self new) predefinedLayouts: KRMuThReportPageLayout predefinedReportPageLayouts.
	t1 isNil ifFalse: [t2 initialLayoutName: t1].
	^t2 openDialog! !

!KRMuThReportPageLayoutChooser class publicMethodsFor: 'interface specs'!

dialogSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Report Page Layout Chooser' #min: #(#Point 318 356) #max: #(#Point 318 356) #bounds: #(#Rectangle 256 374 574 730) #isEventDriven: true) #component: #(#SpecCollection #collection: #(#(#InputFieldSpec #layout: #(#LayoutFrame 5 0 25 0 -5 1 45 0) #name: #customTitleFieldID #flags: 0 #isOpaque: true #model: #customTitle #tabable: false #alignment: #center #isReadOnly: true) #(#MenuButtonSpec #layout: #(#LayoutFrame 5 0 50 0 -5 1 75 0) #name: #layoutNameMenuID #isOpaque: true #model: #layoutName #menu: #layoutNameMenu #isPopUp: false) #(#LabelSpec #layout: #(#AlignmentOrigin 0 0.5 5 0 0.5 0) #name: #titleLabelID #label: 'Select Page Layout') #(#SubCanvasSpec #layout: #(#LayoutFrame 5 0 80 0 -5 1 -45 1) #name: #layoutEditorCanvasID) #(#ActionButtonSpec #layout: #(#LayoutFrame 25 0 -40 1 -25 0.5 -5 1) #name: #acceptButtonID #model: #accept #label: 'OK' #isDefault: true #defaultable: true) #(#ActionButtonSpec #layout: #(#LayoutFrame 25 0.5 -40 1 -25 1 -5 1) #name: #cancelButtonID #model: #cancel #label: 'Cancel' #isDefault: false #defaultable: true))))!

windowSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Report Page Layout Chooser' #min: #(#Point 318 311) #max: #(#Point 318 311) #bounds: #(#Rectangle 463 173 781 484) #isEventDriven: true) #component: #(#SpecCollection #collection: #(#(#InputFieldSpec #layout: #(#LayoutFrame 5 0 25 0 -5 1 45 0) #name: #customTitleFieldID #flags: 0 #isOpaque: true #model: #customTitle #tabable: false #alignment: #center #isReadOnly: true) #(#MenuButtonSpec #layout: #(#LayoutFrame 5 0 50 0 -5 1 75 0) #name: #layoutNameMenuID #isOpaque: true #model: #layoutName #menu: #layoutNameMenu #isPopUp: false) #(#LabelSpec #layout: #(#AlignmentOrigin 0 0.5 5 0 0.5 0) #name: #titleLabelID #label: 'Select Page Layout') #(#SubCanvasSpec #layout: #(#LayoutFrame 5 0 80 0 -5 1 -5 1) #name: #layoutEditorCanvasID))))! !

!KRMuThReportPageLayoutChooser class publicMethodsFor: 'menu'!

buildLayoutNameMenuWith: t1 

	| t2 t3 t4 t5 t6 |
	t1 isNil ifTrue: [^nil].
	t2 := (t4 := t1 asArray) collect: [:t7 | t7 name].
	t3 := t4 collect: [:t8 | t8 isCustom
					ifTrue: [t8 name]
					ifFalse: [t8 name , (String with: Character tab with: Character tab) , ' (' , t8 width printString , ' ' , t8 unit , ' * ' , t8 height printString , ' ' , t8 unit , ')']].
	t5 := OrderedCollection new: t4 size.
	t6 := nil.
	(1 to: t4 size - 1)
		do: 
			[:t9 | 
			| t10 |
			t10 := t4 at: t9 + 1.
			((t6 notNil and: [(t6 = t10 unit) not])
				or: [t10 name = KRMuThReportPageLayout customNameSymbol])
				ifTrue: [t5 add: t9].
			t6 := t10 unit].
	^Menu
		labelArray: t3
		lines: t5
		values: t2! !

!KRMuThReportPageLayoutChooser class publicMethodsFor: 'preferences'!

preferredPageLayoutEditorClass

	^KRMuThPageLayoutEditor! !

!KRMuThReportPageLayoutChooser publicMethodsFor: 'accessing'!

initialLayoutName

	initialLayoutName isNil ifTrue: [self initInitialLayoutName].
	^initialLayoutName!

initialLayoutName: t1 

	initialLayoutName := t1 asSymbol.
	^self!

predefinedLayouts

	predefinedLayouts isNil ifTrue: [self initPredefinedLayouts].
	^predefinedLayouts!

predefinedLayouts: t1 

	predefinedLayouts := t1.
	^self! !

!KRMuThReportPageLayoutChooser publicMethodsFor: 'aspects'!

customTitle

	customTitle isNil ifTrue: [self initCustomTitle].
	^customTitle!

layoutName

	layoutName isNil ifTrue: [self initLayoutName].
	^layoutName!

layoutNameMenu

	layoutNameMenu isNil ifTrue: [self initLayoutNameMenu].
	^layoutNameMenu! !

!KRMuThReportPageLayoutChooser publicMethodsFor: 'connecting'!

connectLayoutEditorTo: t1 

	| t2 |
	t1 isNil ifTrue: [^nil].
	layoutEditor isNil
		ifTrue: 
			[t2 := t1 isCustom
						ifTrue: [self windowSpecNameForCustomLayout]
						ifFalse: [self windowSpecName].
			layoutEditor := self preferredPageLayoutEditorClass new.
			(builder componentAt: self layoutEditorWidgetID) widget client: layoutEditor spec: t2.
			layoutEditor postOpenWith: layoutEditor builder].
	layoutEditor connectPageLayout: t1.
	^self!

disconnectLayoutEditor

	layoutEditor isNil
		ifFalse: 
			[layoutEditor disconnect.
			layoutEditor := nil].
	^self!

hideLayoutEditor

	(self builder componentAt: self layoutEditorWidgetID) beInvisible.
	^self!

showLayoutEditor

	(self builder componentAt: self layoutEditorWidgetID) beVisible.
	^self!

windowSpecName

	^#noNameWindowSpec!

windowSpecNameForCustomLayout

	^#noNameCustomPageWindowSpec! !

!KRMuThReportPageLayoutChooser publicMethodsFor: 'dlog:interface opening'!

doPostOpenActions

	self registerInterests.
	self setWindowLabel.
	self setCustomTitle.
	self setInitialLayoutName.
	^self! !

!KRMuThReportPageLayoutChooser publicMethodsFor: 'initialize-release'!

initCustomTitle

	customTitle := '' copy asValue.
	^self!

initInitialLayoutName

	initialLayoutName := self predefinedLayouts first name.
	^self!

initLayoutName

	layoutName := nil asValue.
	^self!

initLayoutNameMenu

	layoutNameMenu := self buildLayoutNameMenu.
	^self!

initPredefinedLayouts

	predefinedLayouts := KRMuThReportPageLayout predefinedReportPageLayouts.
	^self!

release

	self releaseParts.
	initialLayoutName := nil.
	predefinedLayouts := nil.
	customTitle := nil.
	selectedLayout := nil.
	layoutName := nil.
	layoutNameMenu := nil.
	layoutEditor := nil.
	^super release!

releaseParts

	self disconnectLayoutEditor.
	self layoutName retractInterestsFor: self.
	^self! !

!KRMuThReportPageLayoutChooser publicMethodsFor: 'interface opening'!

postOpenWith: t1 

	self registerInterests.
	self setWindowLabel.
	self setCustomTitle.
	self setInitialLayoutName.
	^self!

registerInterests

	self layoutName onChangeSend: #layoutNameChanged to: self.
	^self!

setCustomTitle

	^self!

setInitialLayoutName

	self layoutName value: self initialLayoutName.
	^self!

setWindowLabel

	^self! !

!KRMuThReportPageLayoutChooser publicMethodsFor: 'layouts'!

findLayoutNamed: t1 

	t1 isNil ifTrue: [^nil].
	^self predefinedLayouts detect: [:t2 | t2 name = t1]
		ifNone: [nil]! !

!KRMuThReportPageLayoutChooser publicMethodsFor: 'menu'!

buildLayoutNameMenu

	^self class buildLayoutNameMenuWith: self predefinedLayouts! !

!KRMuThReportPageLayoutChooser publicMethodsFor: 'preferences'!

preferredPageLayoutEditorClass

	^self class preferredPageLayoutEditorClass! !

!KRMuThReportPageLayoutChooser publicMethodsFor: 'updating'!

changeRequest

	| t1 |
	t1 := super changeRequest.
	self release.
	^t1!

layoutNameChanged

	| t1 t2 |
	t1 := self layoutName value.
	selectedLayout := t2 := self findLayoutNamed: t1.
	self returnSelectionHolder value: t2.
	(t2 isNil or: [layoutEditor notNil and: [(layoutEditor pageLayout value isCustom = t2 isCustom) not]])
		ifTrue: 
			[self hideLayoutEditor.
			self disconnectLayoutEditor].
	self connectLayoutEditorTo: t2.
	self showLayoutEditor.
	^self! !

!KRMuThReportPageLayoutChooser publicMethodsFor: 'widgets'!

layoutEditorWidgetID

	^#layoutEditorCanvasID! !

!KRMuThReportWriter class publicMethodsFor: 'class accessing'!

breakPointClass

	^KRMuThReportBreak!

reportClass

	^KRMuThReport! !

!KRMuThReportWriter class publicMethodsFor: 'preferences'!

preferredReportBrokerClass

	^KRMuThReportBroker!

preferredReportElementClass

	^KRMuThReportElement! !

!KRMuThReportWriter class publicMethodsFor: 'public interface'!

reportName: t1 executeOnStream: t2 reportObject: t3 

	| t4 t5 t6 |
	t4 := self reportSpecFor: t1.
	^t6 := ((t5 := self new) reportDefinition: t4)
				ifTrue: 
					[t5 reportObject: t3.
					t5 executeOnStream: t2]
				ifFalse: [nil]!

reportName: t1 executeOnStream: t2 reportObject: t3 windowLabel: t4 

	| t5 t6 t7 |
	t5 := self reportSpecFor: t1.
	((t6 := self new) reportDefinition: t5)
		ifFalse: [^nil].
	t4 isNil ifFalse: [t6 displayClientWindowLabel: t4].
	t6 reportObject: t3.
	^t7 := t6 executeOnStream: t2!

reportName: t1 executeOnStream: t2 reportObject: t3 windowLabel: t4 choosePageLayoutFrom: t5 defaultPageLayoutName: t6 

	| t7 t8 t9 t10 t11 |
	t9 := nil.
	((t10 := KRMuThReportPageLayoutChooser choosePageLayoutFrom: t5 initialNamed: t6) notNil and: [t10 size >= 2 and: [t10 first == true]])
		ifTrue: [t9 := t10 at: 2].
	t7 := self reportSpecFor: t1.
	((t8 := self new) reportDefinition: t7)
		ifFalse: [^nil].
	t9 isNil ifFalse: [t8 setPageLayout: t9].
	t4 isNil ifFalse: [t8 displayClientWindowLabel: t4].
	t8 reportObject: t3.
	^t11 := t8 executeOnStream: t2! !

!KRMuThReportWriter publicMethodsFor: 'accessing'!

displayClientWindowLabel

	displayClientWindowLabel isNil ifTrue: [self initDisplayClientWindowLabel].
	^displayClientWindowLabel!

displayClientWindowLabel: t1 

	displayClientWindowLabel := t1.
	^self!

minBodyOrphanHeight

	minBodyOrphanHeight isNil ifTrue: [self initMinBodyOrphanHeight].
	^minBodyOrphanHeight!

minBodyOrphanHeight: t1 

	minBodyOrphanHeight := t1.
	^self!

pageLayout

	^pageLayout!

pageLayout: t1 

	pageLayout := t1.
	^self!

setPageLayout: t1 

	| t2 t3 t4 t5 t6 t7 |
	self pageLayout: t1.
	self pageLayout isNil ifTrue: [^self].
	t2 := self pageLayout printableWidthInch.
	t3 := self pageLayout printableHeightInch.
	self width: t2 height: t3.
	t4 := self pageLayout leftMarginInch.
	t5 := self pageLayout topMarginInch.
	t6 := self pageLayout rightMarginInch.
	t7 := self pageLayout bottomMarginInch.
	self
		leftMargin: t4
		topMargin: t5
		rightMargin: t6
		bottomMargin: t7.
	^self!

splitBodyOnPageBreak

	^splitBodyOnPageBreak == true!

splitBodyOnPageBreak: t1 

	splitBodyOnPageBreak := t1 == true.
	^self! !

!KRMuThReportWriter publicMethodsFor: 'execution'!

executionLoop

	| t1 |
	self printTitle.
	t1 := true.
	[self dataStream atEnd]
		whileFalse: 
			[self isolateDataPacket.
			t1
				ifTrue: 
					[self
						printOrMakeRoomForBreakHeadersWithDataPacket: self currentPacket
						forIndex: 1
						fromLevel: 1.
					t1 := false].
			self process: self currentPacket].
	self printOrSkipBreaksForIndex: self dataStream position level: 1.
	self printSummary.
	self release.
	Processor yield.
	^self report!

fromBreakDefinition: t1 

	| t2 t3 t4 t5 t6 |
	t6 := (t1 respondsTo: #hasSummaryInfo)
				and: [t1 hasSummaryInfo].
	t2 := self breakPoints.
	t4 := t1 level.
	(t2 includesKey: t4)
		ifFalse: 
			[(t3 := self class breakPointClass new) newPage: t1 newPage; hasSummaryInfo: t6; dependOn: self currentPacketHolder.
			t2 at: t4 put: t3.
			t5 := Dictionary new.
			t1 isHeaderSpecified ifTrue: [t5 at: #header put: (self sectionClass: t1 headerClassObject selector: t1 headerSelector asSymbol)].
			t1 isFooterSpecified ifTrue: [t5 at: #footer put: (self sectionClass: t1 footerClassObject selector: t1 footerSelector asSymbol)].
			self sectionAt: t3 put: t5].
	(t2 at: t4)
		selectors: t1 selectorCollection.
	^self!

isolateDataPacket

	^super isolateDataPacket!

process: t1 

	| t2 |
	(t2 := self levelWhichBroke) notNil
		ifTrue: 
			[self printOrSkipBreaksForIndex: self dataStream position - 1 level: t2.
			(self breakPoints at: t2) pageAfterBreak
				ifTrue: 
					[self push: (self pop first to: self dataStream position - 1); printHeaderAndFooter.
					self nextPage].
			self
				printOrMakeRoomForBreakHeadersWithDataPacket: t1
				forIndex: self dataStream position
				fromLevel: t2].
	self addPacketToSections: t1.
	self printMainBody: t1.
	^self!

sectionClass: t1 selector: t2 

	| t3 t4 |
	t3 := t1 interfaceSpecFor: t2.
	(t4 := Dictionary new) at: #applicationClass put: t1; at: #spec put: nil; at: #bounds put: t3 window bounds; at: #fullSpec put: nil; at: #myInterfaceSpecSelector put: t2.
	^t4! !

!KRMuThReportWriter publicMethodsFor: 'initialize-release'!

defaultDisplayClientWindowLabel

	^'Report Viewer' copy!

defaultMinBodyOrphanHeight

	^16!

initDisplayClientWindowLabel

	displayClientWindowLabel := self defaultDisplayClientWindowLabel.
	^self!

initMinBodyOrphanHeight

	minBodyOrphanHeight := self defaultMinBodyOrphanHeight.
	^self!

release

	report notNil ifTrue: [report terminateReport].
	report := nil.
	splitBodyOnPageBreak := nil.
	minBodyOrphanHeight := nil.
	displayClientWindowLabel := nil.
	pageLayout := nil.
	^super release! !

!KRMuThReportWriter publicMethodsFor: 'preferences'!

preferredReportBrokerClass

	^self class preferredReportBrokerClass!

preferredReportElementClass

	^self class preferredReportElementClass! !

!KRMuThReportWriter publicMethodsFor: 'private-printing'!

defaultSplitMarkHeigh

	^8!

makeRoomForBreakHeaderAtLevel: t1 withDataPacket: t2 

	| t3 t4 t5 t6 |
	(t3 := self breakPoints) isEmpty ifTrue: [^nil].
	t5 := t3 at: t1.
	t4 := (self sectionAt: t5)
				at: #header.
	(self currentAggregateSectionAt: t4)
		addDataPacket: t2.
	(t6 := (self currentAggregateSectionAt: t4) height) isNil ifTrue: [t6 := (t4 at: #bounds) height].
	(self canPageHold: t6)
		ifFalse: 
			[self push: (self pop first to: self dataStream position - 1); printHeaderAndFooter.
			self nextPage].
	t4 at: #displayOffset put: self sectionOffset; at: #displayPage put: self pageCount.
	self sectionOffset: self sectionOffset + t6.
	self report oneMoreMissingElement.
	^t4!

minOrphanHeight

	^16!

print: t1 model: t2 

	^self
		print: t1
		model: t2
		wasMissing: false!

print: t1 model: t2 butTopHeight: t3 

	| t4 t5 t6 t7 t8 t9 |
	t9 := self defaultSplitMarkHeigh.
	t8 := self sectionOffset.
	(t5 := t2 height) == nil ifTrue: [t5 := (t1 at: #bounds) height].
	t5 <= t3 ifTrue: [^self].
	t6 := self pageWidth.
	t4 := (LayoutFrame new) leftFraction: 0; leftOffset: self leftMargin; rightFraction: 0; rightOffset: (t6 - self rightMargin max: 1); topOffset: self sectionOffset - t3 + t9; bottomOffset: self sectionOffset + t5 - t3 + t9.
	t7 := self preferredReportElementClass
				model: t2
				layout: t4
				section: t1
				page: self pageCount.
	t2 postCanvasAppendWith: self.
	self report addElement: t7 wasMissing: false.
	self sectionOffset: self sectionOffset + t5 - t3 + t9.
	self printHideFromTopOfPageUpTo: t8 splitMarkHeight: t9.
	^self!

print: t1 model: t2 butTopHeight: t3 additionalOverlap: t4 

	| t5 t6 t7 t8 t9 t10 |
	t10 := self defaultSplitMarkHeigh.
	t9 := self sectionOffset.
	(t6 := t2 height) == nil ifTrue: [t6 := (t1 at: #bounds) height].
	t6 <= t3 ifTrue: [^self].
	t7 := self pageWidth.
	t5 := (LayoutFrame new) leftFraction: 0; leftOffset: self leftMargin; rightFraction: 0; rightOffset: (t7 - self rightMargin max: 1); topOffset: self sectionOffset - t3 + t10 + t4; bottomOffset: self sectionOffset + t6 - t3 + t10 + t4.
	t8 := self preferredReportElementClass
				model: t2
				layout: t5
				section: t1
				page: self pageCount.
	t2 postCanvasAppendWith: self.
	self report addElement: t8 wasMissing: false.
	self sectionOffset: self sectionOffset + t6 - t3 + t10 + t4.
	self
		printHideFromTopOfPageUpTo: t9
		splitMarkHeight: t10
		additionalOverlap: t4.
	^self!

print: t1 model: t2 onlyTopHeight: t3 

	| t4 t5 t6 t7 |
	(t5 := t2 height) == nil ifTrue: [t5 := (t1 at: #bounds) height].
	t5 := t5 min: t3.
	t6 := self pageWidth.
	t4 := (LayoutFrame new) leftFraction: 0; leftOffset: self leftMargin; rightFraction: 0; rightOffset: (t6 - self rightMargin max: 1); topOffset: self sectionOffset; bottomOffset: self sectionOffset + t5.
	t7 := self preferredReportElementClass
				model: t2
				layout: t4
				section: t1
				page: self pageCount.
	t2 postCanvasAppendWith: self.
	self report addElement: t7 wasMissing: false.
	self sectionOffset: self sectionOffset + t5.
	^self!

print: t1 model: t2 wasMissing: t3 

	| t4 t5 t6 t7 |
	(t5 := t2 height) == nil ifTrue: [t5 := (t1 at: #bounds) height].
	t6 := self pageWidth.
	t4 := (LayoutFrame new) leftFraction: 0; leftOffset: self leftMargin; rightFraction: 0; rightOffset: (t6 - self rightMargin max: 1); topOffset: self sectionOffset; bottomOffset: self sectionOffset + t5.
	t7 := self preferredReportElementClass
				model: t2
				layout: t4
				section: t1
				page: self pageCount.
	t2 postCanvasAppendWith: self.
	self report addElement: t7 wasMissing: t3.
	self sectionOffset: self sectionOffset + t5.
	^self!

printHideFromTopOfPageUpTo: t1 splitMarkHeight: t2 

	| t3 t4 t5 t6 t7 t8 |
	(t3 := RegionSpec new) isOpaque: true.
	t4 := (LayoutFrame new) leftOffset: 0; rightOffset: (self pageWidth - self rightMargin max: 1); topOffset: self pageHeight * (self pageCount - 1) - 32; bottomOffset: t1 + t2.
	(t5 := self preferredReportElementClass
				spec: t3
				layout: t4
				page: self pageCount) isPageDivider: false.
	self report addElement: t5 wasMissing: false.
	(t6 := RegionSpec new) isOpaque: true.
	(t7 := LookPreferences new) setBackgroundColor: ColorValue gray.
	t7 setForegroundColor: ColorValue lightGray.
	t6 colors: t7.
	t6 lineWidth: 2.
	t8 := (LayoutFrame new) leftOffset: 0; rightOffset: self pageWidth; topOffset: t1; bottomOffset: t1 + t2.
	(t5 := self preferredReportElementClass
				spec: t6
				layout: t8
				page: self pageCount) isPageDivider: false.
	self report addElement: t5 wasMissing: false.
	^self!

printHideFromTopOfPageUpTo: t1 splitMarkHeight: t2 additionalOverlap: t3 

	| t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 |
	t12 := self pageWidth.
	(t4 := RegionSpec new) isOpaque: true.
	t5 := (LayoutFrame new) leftOffset: 0; rightOffset: (t12 - self rightMargin max: 1); topOffset: self pageHeight * (self pageCount - 1) - 32; bottomOffset: t1 + t2.
	(t6 := self preferredReportElementClass
				spec: t4
				layout: t5
				page: self pageCount) isPageDivider: false.
	self report addElement: t6 wasMissing: false.
	(t7 := RegionSpec new) isOpaque: true.
	(t8 := LookPreferences new) setBackgroundColor: ColorValue gray.
	t8 setForegroundColor: ColorValue lightGray.
	t7 colors: t8.
	t7 lineWidth: 2.
	t9 := (LayoutFrame new) leftOffset: 0; rightOffset: t12; topOffset: t1; bottomOffset: t1 + t2.
	(t6 := self preferredReportElementClass
				spec: t7
				layout: t9
				page: self pageCount) isPageDivider: false.
	self report addElement: t6 wasMissing: false.
	(t10 := RegionSpec new) isOpaque: false.
	(t8 := LookPreferences new) setForegroundColor: ColorValue lightGray.
	t10 colors: t8.
	t10 lineWidth: 1.
	t11 := (LayoutFrame new) leftOffset: 0; rightOffset: t12; topOffset: t1 + t2; bottomOffset: t1 + t2 + t3.
	(t6 := self preferredReportElementClass
				spec: t10
				layout: t11
				page: self pageCount) isPageDivider: false.
	self report addElement: t6 wasMissing: false.
	(t1 + t2 + 4 to: t1 + t2 + t3 - 4 by: 4)
		do: 
			[:t14 | 
			t13 := DividerSpec new.
			(t8 := LookPreferences new) setBackgroundColor: ColorValue lightGray.
			t8 setForegroundColor: ColorValue lightGray.
			t13 colors: t8.
			t5 := (LayoutFrame new) leftOffset: 0; rightOffset: t12; topOffset: t14; bottomOffset: t14 + 1.
			(t6 := self preferredReportElementClass
						spec: t13
						layout: t5
						page: self pageCount) isPageDivider: false.
			self report addElement: t6 wasMissing: false].
	^self!

printHideTopHeight: t1 

	| t2 t3 t4 |
	(t2 := RegionSpec new) isOpaque: true.
	t3 := (LayoutFrame new) leftOffset: 0; rightOffset: (self pageWidth - self rightMargin max: 1); topOffset: self pageHeight * (self pageCount - 1) - 32; bottomOffset: t1.
	(t4 := self preferredReportElementClass
				spec: t2
				layout: t3
				page: self pageCount) isPageDivider: false.
	self report addElement: t4 wasMissing: false.
	^self!

printMainBody: t1 

	| t2 t3 t4 t5 t6 |
	t2 := self mainBodySection.
	(t3 := self model: t2 on: t1) addDataPacket: t1.
	(t4 := t3 height) == nil ifTrue: [t4 := self mainBodyHeight].
	t4 > 0
		ifTrue: 
			[(self canPageHold: t4)
				ifFalse: 
					[(self splitBodyOnPageBreak and: [(t6 := self minBodyOrphanHeight) > 0 and: [t4 > (t6 * 2) and: [self canPageHold: t6]]])
						ifTrue: 
							[t5 := t6 max: self remainingAvailableBodyHeight.
							t4 - t5 < t6 ifTrue: [t5 := t4 - t6].
							self
								print: t2
								model: t3
								onlyTopHeight: t5.
							self push: (self pop first to: self dataStream position - 1).
							self printHeaderAndFooter.
							self nextPage.
							self
								print: t2
								model: t3
								butTopHeight: t5
								additionalOverlap: t6.
							^self].
					self push: (self pop first to: self dataStream position - 1).
					self printHeaderAndFooter.
					self nextPage].
			self print: t2 model: t3].
	^self!

printOrMakeRoomForBreakHeadersWithDataPacket: t1 forIndex: t2 fromLevel: t3 

	| t4 t5 |
	(t4 := self breakPoints) isEmpty ifTrue: [^self].
	t5 := SortedCollection sortBlock: [:t6 :t7 | t6 < t7].
	t4 keysDo: [:t8 | (((self sectionAt: (t4 at: t8))
			includesKey: #header)
			and: [t8 >= t3])
			ifTrue: [t5 add: t8]
			ifFalse: [nil]].
	t5
		do: 
			[:t9 | 
			| t10 t11 t12 |
			(t10 := t4 at: t9) hasSummaryInfo
				ifTrue: [t12 := self makeRoomForBreakHeaderAtLevel: t9 withDataPacket: t1]
				ifFalse: 
					[t12 := (self sectionAt: t10)
								at: #header.
					(t11 := (self currentAggregateSectionAt: t12) height) isNil ifTrue: [t11 := (t12 at: #bounds) height].
					(self canPageHold: t11)
						ifFalse: 
							[self push: (self pop first to: self dataStream position - 1); printHeaderAndFooter.
							self nextPage].
					(self currentAggregateSectionAt: t12)
						addDataPacket: t1.
					self push: (t10 lastBreakIndex to: t2); print: t12 model: (self model: t12)].
			t12 notNil
				ifTrue: [self replaceCurrentAggregateSectionAt: t12]
				ifFalse: [nil]].
	^self!

printOrSkipBreaksForIndex: t1 level: t2 

	self printOrSkipHeaderBreaksDownFromLevel: t2 usingIndex: t1; printFooterBreaksUpToLevel: t2 stopIndex: t1.
	^self!

printOrSkipHeaderBreaksDownFromLevel: t1 usingIndex: t2 

	| t3 t4 t5 t6 |
	(t3 := self breakPoints) isEmpty ifTrue: [^self].
	t4 := SortedCollection sortBlock: [:t7 :t8 | t7 < t8].
	t3 keysDo: [:t9 | (((self sectionAt: (t3 at: t9))
			includesKey: #header)
			and: [t9 >= t1])
			ifTrue: [t4 add: t9]
			ifFalse: [nil]].
	t5 := self sectionOffset.
	t6 := self pageCount.
	t4
		do: 
			[:t10 | 
			| t11 t12 t13 |
			(t11 := t3 at: t10) hasSummaryInfo
				ifTrue: 
					[t13 := (self sections at: t11)
								at: #header.
					self push: (t11 lastBreakIndex to: t2); sectionOffset: (t13 at: #displayOffset); pageCount: (t13 at: #displayPage).
					t12 := self model: t13.
					self
						print: t13
						model: t12
						wasMissing: true.
					((self sections at: t11)
						includesKey: #footer)
						ifFalse: [t11 lastBreakIndex: self dataStream position].
					t13 removeKey: #displayOffset; removeKey: #displayPage].
			t13 notNil
				ifTrue: [self replaceCurrentAggregateSectionAt: t13]
				ifFalse: [nil]].
	self sectionOffset: t5; pageCount: t6.
	^self!

printPageDivider

	| t1 t2 t3 |
	t1 := DividerSpec new.
	t2 := (LayoutFrame new) leftOffset: 0; rightOffset: self pageWidth; topOffset: self sectionOffset - self topMargin - 3; bottomOffset: self sectionOffset.
	(t3 := self preferredReportElementClass
				spec: t1
				layout: t2
				page: self pageCount) isPageDivider: true.
	self report addElement: t3 wasMissing: false.
	self report terminateCurrentPage.
	Processor yield.
	^self!

remainingAvailableBodyHeight

	^self pageHeight * self pageCount - self bottomMargin - self footerHeight - self sectionOffset! !

!KRMuThReportWriter publicMethodsFor: 'public interface'!

reportDefinition: t1 

	| t2 |
	t2 := super reportDefinition: t1.
	Object messageNotUnderstoodSignal handle: [:t3 | self splitBodyOnPageBreak: false]
		do: [self splitBodyOnPageBreak: t1 splitBodyOnPageBreak].
	Object messageNotUnderstoodSignal handle: [:t4 | self minBodyOrphanHeight: false]
		do: [self minBodyOrphanHeight: t1 minBodyOrphanHeight].
	Object messageNotUnderstoodSignal handle: [:t5 | self displayClientWindowLabel: self defaultDisplayClientWindowLabel]
		do: [self displayClientWindowLabel: t1 displayClientWindowLabel].
	^t2! !

!KRMuThReportWriter publicMethodsFor: 'streaming'!

executeOnStream: t1 

	| t2 |
	t2 := 
			[self dataStream: t1.
			self resetDataStream; computePacketSize.
			self executionLoop].
	^self preferredReportBrokerClass
		newForReport: self report
		withBlock: t2
		displayClientWindowLabel: self displayClientWindowLabel
		from: self! !

KRMuThBreakDefinition initializeAfterLoad!
KRMuThReportBroker initializeAfterLoad!
KRMuThReportElement initializeAfterLoad!
KRMuThReportPage initializeAfterLoad!
KRMuThReportPageLayout initializeAfterLoad!
KRMuThPrintJob initializeAfterLoad!
KRMuThReportBreak initializeAfterLoad!
KRMuThReportDefinition initializeAfterLoad!
KRMuThReportWriter initializeAfterLoad!
KRMuThApplicationOrDialogModel initializeAfterLoad!
KRMuThReportMonitor initializeAfterLoad!
KRMuThReportPageLayoutChooser initializeAfterLoad!
KRMuThPageLayoutEditor initializeAfterLoad!
KRMuThReportPageDisplayClient initializeAfterLoad!
KRMuThPrintPage initializeAfterLoad!
KRMuThReport initializeAfterLoad!
KRMuThReportDisplayClient initializeAfterLoad!
KRONO_Reporter_MultiThreaded initializeAfterLoad!
KRMuThDialogPostOpenTrigger initializeAfterLoad!
KRMuThPrintPageVisualPart initializeAfterLoad!

KRONO_Reporter_MultiThreaded loaded!
