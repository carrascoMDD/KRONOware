'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:38:03 pm'!



((KRONO_Reporter createSubApplication: #KRONO_Reporter_MultiThreaded_Test in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

KRONO_Reporter_MultiThreaded_Test becomeDefault!

Object subclass: #KRMuThClassComplexityReportWriterTestHarness
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded_Test becomeDefault!

Object subclass: #KRMuThReportWriterTestHarness
	instanceVariableNames: ''
	classVariableNames: 'AllClasses '
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded_Test becomeDefault!

KRMuThReportWriter subclass: #KRMuThClassComplexityReportWriter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded_Test becomeDefault!

KRMuThReportWriter subclass: #KRMuThReportWriterTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded_Test becomeDefault!

Object subclass: #KRRWMuThDynamicGenerationTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded_Test becomeDefault!

KRReportSection subclass: #KRDemoSummary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded_Test becomeDefault!

KRDemoSummary subclass: #KRDemoSummaryProportional
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded_Test becomeDefault!

KRReportSection subclass: #KRDemoTitle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded_Test becomeDefault!

KRDemoTitle subclass: #KRDemoTitleProportional
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded_Test becomeDefault!

SubApplication subclass: #KRONO_Reporter_MultiThreaded_Test
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_MultiThreaded_Test becomeDefault!

!KRDemoSummary class publicMethodsFor: 'interface specs'!

reportComponentSpec

	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 0 0 637.5 32)) #component: #(#SpecCollection #collection: #(#(#LabelSpec #layout: #(#Point 200 8) #label: '-30-'))))! !

!KRDemoSummaryProportional class publicMethodsFor: 'interface specs'!

reportComponentSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 262 269 899.5 301)) #component: #(#SpecCollection #collection: #(#(#LabelSpec #layout: #(#AlignmentOrigin 0 0.5 5 0 0.5 0) #label: '-30-'))))! !

!KRDemoTitle class publicMethodsFor: 'interface specs'!

reportComponentSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 0 0 637.5 32)) #component: #(#SpecCollection #collection: #(#(#LabelSpec #layout: #(#Point 176 0) #label: 'Class Report') #(#DividerSpec #layout: #(#Rectangle 0 22 597 26)))))! !

!KRDemoTitleProportional class publicMethodsFor: 'interface specs'!

reportComponentSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 147 347 784.5 379)) #component: #(#SpecCollection #collection: #(#(#LabelSpec #layout: #(#AlignmentOrigin 0 0.5 3 0 0.5 0) #label: 'Class Report') #(#DividerSpec #layout: #(#LayoutFrame 0 0 23 0 0 1 27 0)))))! !

!KRMuThClassComplexityReportWriter class publicMethodsFor: 'report specs'!

applicationReportSpec

	<resource: #jpmReport>
	^#(#KRReportDefinition #bodyClass: #KRRWApplicationComplexityBody #bodySelector: #reportComponentSpec #headerClass: #KRRWApplicationComplexityHeader #headerSelector: #reportComponentSpec #footerSelector: #reportSpec #titleSelector: #reportSpec #summarySelector: #reportSpec #height: 10.5 #width: 8 #leftMargin: 0.0 #topMargin: 0.0 #rightMargin: 0.0 #bottomMargin: 0.0 #separateSummaryPage: false #separateTitlePage: false #showSummaryHeader: true #showSummaryFooter: true #showTitleHeader: true #showTitleFooter: true #breakDefinitions: #())!

classAverageReport

	<resource: #jpmReport>
	^#(#KRReportDefinition #bodyClass: #KRRWClassComplexitySections #bodySelector: #bodySpec #headerSelector: #reportSpec #footerSelector: #reportSpec #titleSelector: #reportSpec #summarySelector: #reportSpec #height: 10.5 #width: 8 #leftMargin: 0.0 #topMargin: 0.0 #rightMargin: 0.0 #bottomMargin: 0.0 #separateSummaryPage: false #separateTitlePage: false #showSummaryHeader: true #showSummaryFooter: true #showTitleHeader: true #showTitleFooter: true #breakDefinitions: #(#(#KRMuThBreakDefinition #headerClass: #KRRWClassComplexityAccumulatingSections #headerSelector: #meanSpec #footerSelector: #reportSpec #level: 1 #newPage: false #selectorCollection: #(#applicationName) #hasSummaryInfo: true)))!

classReportSpec

	<resource: #jpmReport>
	^#(#KRReportDefinition #bodyClass: #KRRWClassComplexitySections #bodySelector: #bodySpec #headerSelector: #reportSpec #footerSelector: #reportSpec #titleSelector: #reportSpec #summarySelector: #reportSpec #height: 10.5 #width: 8 #leftMargin: 0.0 #topMargin: 0.0 #rightMargin: 0.0 #bottomMargin: 0.0 #separateSummaryPage: false #separateTitlePage: false #showSummaryHeader: true #showSummaryFooter: true #showTitleHeader: true #showTitleFooter: true #breakDefinitions: #(#(#KRBreakDefinition #headerClass: #KRRWClassComplexityAccumulatingSections #headerSelector: #summarySpec #footerSelector: #reportSpec #level: 1 #newPage: false #selectorCollection: #(#applicationName))))!

simpleClassReportSpec

	<resource: #jpmReport>
	^#(#KRReportDefinition #bodyClass: #KRRWClassComplexitySections #bodySelector: #bodySpec #headerSelector: #reportSpec #footerSelector: #reportSpec #titleSelector: #reportSpec #summarySelector: #reportSpec #height: 10.5 #width: 8 #leftMargin: 0.0 #topMargin: 0.0 #rightMargin: 0.0 #bottomMargin: 0.0 #separateSummaryPage: false #separateTitlePage: false #showSummaryHeader: true #showSummaryFooter: true #showTitleHeader: true #showTitleFooter: true #breakDefinitions: #())! !

!KRMuThClassComplexityReportWriterTestHarness class publicMethodsFor: 'data generating'!

applicationsInUserSelectedConfigMap

	| t1 |
	(t1 := EtDoubleListChooser
				prompt: 'Select the configuration map name and version.'
				chooseFrom: EmConfigurationMap configurationMapNames asSortedCollection
				printBlock: [:t2 | t2]
				dependentListBlock: [:t3 | (EmConfigurationMap editionsFor: t3) asArray]
				dependentPrintBlock: [:t4 | t4 timeStamp signature]) == nil ifTrue: [^self].
	^t1 shadowApplications collect: [:t5 | KRRWApplicationNode application: t5]!

classesInUserSelectedConfigMap

	| t1 |
	t1 := OrderedCollection new.
	self applicationsInUserSelectedConfigMap do: [:t2 | t2 application classes do: [:t3 | t1 add: (KRRWClassNode containedClass: t3 application: t2 application)]].
	^t1! !

!KRMuThClassComplexityReportWriterTestHarness class publicMethodsFor: 'examples'!

examples
"
KRMuThClassComplexityReportWriterTestHarness  simpleClassTest  displayReport.
KRMuThClassComplexityReportWriterTestHarness  applicationReportTest  displayReport.
KRMuThClassComplexityReportWriterTestHarness  classAverageReport  displayReport.
KRMuThClassComplexityReportWriterTestHarness  classReportTest  displayReport.
KRMuThClassComplexityReportWriterTestHarness  programmatic1  displayReport.
KRMuThClassComplexityReportWriterTestHarness  programmatic2  displayReport.
"! !

!KRMuThClassComplexityReportWriterTestHarness class publicMethodsFor: 'tests'!

applicationReportTest

	^KRMuThClassComplexityReportWriter
		reportName: #applicationReportSpec
		executeOnStream: self applicationsInUserSelectedConfigMap readStream
		reportObject: nil!

classAverageReport

	^KRMuThClassComplexityReportWriter
		reportName: #classAverageReport
		executeOnStream: self classesInUserSelectedConfigMap readStream
		reportObject: nil!

classReportTest

	^KRMuThClassComplexityReportWriter
		reportName: #classReportSpec
		executeOnStream: self classesInUserSelectedConfigMap readStream
		reportObject: nil!

programmatic1

	| t1 t2 |
	(t1 := KRReportDefinition new) bodyClass: KRRWApplicationComplexityBody.
	t1 headerClass: KRRWApplicationComplexityHeader.
	t1 footerClass: TestFooter.
	^(t2 := KRMuThReportWriter reportDefinition: t1) executeOn: self applicationsInUserSelectedConfigMap!

programmatic2

	| t1 t2 |
	(t1 := KRReportDefinition new) bodyClass: KRRWApplicationComplexityBody.
	t1 headerClass: KRRWApplicationComplexityHeader.
	t1 footerClass: TestFooter.
	t1 titleClass: KRDemoTitle.
	t1 showSummaryHeader: false.
	^(t2 := KRMuThReportWriter reportDefinition: t1) executeOnStream: self applicationsInUserSelectedConfigMap readStream!

simpleClassTest

	^KRMuThClassComplexityReportWriter
		reportName: #simpleClassReportSpec
		executeOnStream: self classesInUserSelectedConfigMap readStream
		reportObject: nil! !

!KRMuThReportWriterTest class publicMethodsFor: 'examples'!

examples
"
	KRMuThReportWriterTestHarness someClassesDisplayReport.
	KRMuThReportWriterTestHarness allClassesDisplayReport.
	KRMuThReportWriterTestHarness someClassesMuThDisplayReport.
	KRMuThReportWriterTestHarness allClassesMuThDisplayReport.



"! !

!KRMuThReportWriterTest class publicMethodsFor: 'report specs'!

testReport

	<resource: #jpmReport>
	^#(#KRReportDefinition #bodyClass: #TestBody #bodySelector: #reportComponentSpec #headerClass: #TestHeader #headerSelector: #reportComponentSpec #footerClass: #TestFooter #footerSelector: #reportComponentSpec #titleClass: #KRDemoTitle #titleSelector: #reportComponentSpec #summaryClass: #KRDemoSummary #summarySelector: #reportComponentSpec #height: 10.5 #width: 8 #leftMargin: 0.0 #topMargin: 0.0 #rightMargin: 0.0 #bottomMargin: 0.0 #separateSummaryPage: false #separateTitlePage: false #showSummaryHeader: true #showSummaryFooter: true #showTitleHeader: true #showTitleFooter: true #breakDefinitions: #(#(#KRBreakDefinition #headerClass: #TestBreak #headerSelector: #reportComponentSpec #footerClass: #TestBreakFooter #footerSelector: #reportComponentSpec #level: 1 #newPage: false #selectorCollection: #(#initialLetter))))!

testReportProportionalX

	<resource: #jpmReport>
	^#(#KRMuThReportDefinition #bodyClass: #TestBodyProportional #bodySelector: #reportComponentSpec #headerClass: #TestHeaderProportional #headerSelector: #reportComponentSpec #footerClass: #TestFooterProportional #footerSelector: #reportComponentSpec #titleClass: #KRDemoTitleProportional #titleSelector: #reportComponentSpec #summaryClass: #KRDemoSummaryProportional #summarySelector: #reportComponentSpec #height: 10.5 #width: 8.0 #leftMargin: 0.0 #topMargin: 0.0 #rightMargin: 0.0 #bottomMargin: 0.0 #separateSummaryPage: false #separateTitlePage: false #showSummaryHeader: true #showSummaryFooter: true #showTitleHeader: true #showTitleFooter: true #splitBodyOnPageBreak: false #minBodyOrphanHeight: 16 #displayClientWindowLabel: 'Report Viewer' #breakDefinitions: #(#(#KRBreakDefinition #headerClass: #TestBreakProportional #headerSelector: #reportComponentSpec #footerClass: #TestBreakFooterProportional #footerSelector: #reportComponentSpec #level: 1 #newPage: false #selectorCollection: #(#initialLetter))) #pageLayout: #(#KRMuThReportPageLayout #name: #Letter #unitPrivate: #in #width: 8.5 #height: 11 #leftMargin: 0 #topMargin: 0 #rightMargin: 0 #bottomMargin: 0 #noPrintableHeight: 0.5 #noPrintableWidth: 0.5))!

testReportWithBreakHeaderWithSummaryInfo

	<resource: #jpmReport>
	^#(#KRReportDefinition #bodyClass: #TestBody #bodySelector: #reportComponentSpec #headerClass: #TestHeader #headerSelector: #reportComponentSpec #footerClass: #TestFooter #footerSelector: #reportComponentSpec #titleClass: #KRDemoTitle #titleSelector: #reportComponentSpec #summaryClass: #KRDemoSummary #summarySelector: #reportComponentSpec #height: 10.5 #width: 8 #leftMargin: 0.0 #topMargin: 0.0 #rightMargin: 0.0 #bottomMargin: 0.0 #separateSummaryPage: false #separateTitlePage: false #showSummaryHeader: true #showSummaryFooter: true #showTitleHeader: true #showTitleFooter: true #breakDefinitions: #(#(#KRMuThBreakDefinition #headerClass: #TestBreakWithSummaryInfo #headerSelector: #reportComponentSpec #footerClass: #TestBreakFooter #footerSelector: #reportComponentSpec #level: 1 #newPage: false #selectorCollection: #(#initialLetter) #hasSummaryInfo: true)))!

testReportWithBreakHeaderWithSummaryInfoWithVariableHeight

	<resource: #jpmReport>
	^#(#KRReportDefinition #bodyClass: #TestBody #bodySelector: #reportComponentSpec #headerClass: #TestHeader #headerSelector: #reportComponentSpec #footerClass: #TestFooter #footerSelector: #reportComponentSpec #titleClass: #KRDemoTitle #titleSelector: #reportComponentSpec #summaryClass: #KRDemoSummary #summarySelector: #reportComponentSpec #height: 10.5 #width: 8 #leftMargin: 0.0 #topMargin: 0.0 #rightMargin: 0.0 #bottomMargin: 0.0 #separateSummaryPage: false #separateTitlePage: false #showSummaryHeader: true #showSummaryFooter: true #showTitleHeader: true #showTitleFooter: true #breakDefinitions: #(#(#KRMuThBreakDefinition #headerClass: #TestBreakWithSummaryInfoWithVariableHeight #headerSelector: #reportComponentSpec #footerClass: #TestBreakFooter #footerSelector: #reportComponentSpec #level: 1 #newPage: false #selectorCollection: #(#initialLetter) #hasSummaryInfo: true)))!

testReportWithBreaksWithVariableHeight

	<resource: #jpmReport>
	^#(#KRReportDefinition #bodyClass: #TestBody #bodySelector: #reportComponentSpec #headerClass: #TestHeader #headerSelector: #reportComponentSpec #footerClass: #TestFooter #footerSelector: #reportComponentSpec #titleClass: #KRDemoTitle #titleSelector: #reportComponentSpec #summaryClass: #KRDemoSummary #summarySelector: #reportComponentSpec #height: 10.5 #width: 8 #leftMargin: 0.0 #topMargin: 0.0 #rightMargin: 0.0 #bottomMargin: 0.0 #separateSummaryPage: false #separateTitlePage: false #showSummaryHeader: true #showSummaryFooter: true #showTitleHeader: true #showTitleFooter: true #breakDefinitions: #(#(#KRBreakDefinition #headerClass: #TestBreakWithVariableHeight #headerSelector: #reportComponentSpec #footerClass: #TestBreakFooter #footerSelector: #reportComponentSpec #level: 1 #newPage: false #selectorCollection: #(#initialLetter))))!

testReportWithHeaderAndFooterBreaksWithVariableHeight

	<resource: #jpmReport>
	^#(#KRReportDefinition #bodyClass: #TestBody #bodySelector: #reportComponentSpec #headerClass: #TestHeader #headerSelector: #reportComponentSpec #footerClass: #TestFooter #footerSelector: #reportComponentSpec #titleClass: #KRDemoTitle #titleSelector: #reportComponentSpec #summaryClass: #KRDemoSummary #summarySelector: #reportComponentSpec #height: 10.5 #width: 8 #leftMargin: 0.0 #topMargin: 0.0 #rightMargin: 0.0 #bottomMargin: 0.0 #separateSummaryPage: false #separateTitlePage: false #showSummaryHeader: true #showSummaryFooter: true #showTitleHeader: true #showTitleFooter: true #breakDefinitions: #(#(#KRBreakDefinition #headerClass: #TestBreakWithVariableHeight #headerSelector: #reportComponentSpec #footerClass: #TestBreakFooterWithVariableHeight #footerSelector: #reportComponentSpec #level: 1 #newPage: false #selectorCollection: #(#initialLetter))))!

testReportWithLongText

	<resource: #jpmReport>
	^#(#KRReportDefinition #bodyClass: #TestBodyWithLongText #bodySelector: #reportComponentSpec #headerClass: #TestHeader #headerSelector: #reportComponentSpec #footerClass: #TestFooter #footerSelector: #reportComponentSpec #titleClass: #KRDemoTitle #titleSelector: #reportComponentSpec #summaryClass: #KRDemoSummary #summarySelector: #reportComponentSpec #height: 10.5 #width: 8 #leftMargin: 0.0 #topMargin: 0.0 #rightMargin: 0.0 #bottomMargin: 0.0 #separateSummaryPage: false #separateTitlePage: false #showSummaryHeader: true #showSummaryFooter: true #showTitleHeader: true #showTitleFooter: true #breakDefinitions: #(#(#KRBreakDefinition #headerClass: #TestBreak #headerSelector: #reportComponentSpec #footerClass: #TestBreakFooter #footerSelector: #reportComponentSpec #level: 1 #newPage: false #selectorCollection: #(#initialLetter))))!

testReportWithLongTextAllowSplit

	<resource: #jpmReport>
	^#(#KRMuThReportDefinition #bodyClass: #TestBodyWithLongText #bodySelector: #reportComponentSpec #headerClass: #TestHeader #headerSelector: #reportComponentSpec #footerClass: #TestFooter #footerSelector: #reportComponentSpec #titleClass: #KRDemoTitle #titleSelector: #reportComponentSpec #summaryClass: #KRDemoSummary #summarySelector: #reportComponentSpec #height: 10.5 #width: 8 #leftMargin: 0.0 #topMargin: 0.0 #rightMargin: 0.0 #bottomMargin: 0.0 #separateSummaryPage: false #separateTitlePage: false #showSummaryHeader: true #showSummaryFooter: true #showTitleHeader: true #showTitleFooter: true #splitBodyOnPageBreak: true #minBodyOrphanHeight: 16 #breakDefinitions: #(#(#KRBreakDefinition #headerClass: #TestBreak #headerSelector: #reportComponentSpec #footerClass: #TestBreakFooter #footerSelector: #reportComponentSpec #level: 1 #newPage: false #selectorCollection: #(#initialLetter))))!

testReportWithMultipleBreaks

	<resource: #jpmReport>
	^#(#KRReportDefinition #bodyClass: #TestBody #bodySelector: #reportComponentSpec #headerClass: #TestHeader #headerSelector: #reportComponentSpec #footerClass: #TestFooter #footerSelector: #reportComponentSpec #titleClass: #KRDemoTitle #titleSelector: #reportComponentSpec #summaryClass: #KRDemoSummary #summarySelector: #reportComponentSpec #height: 10.5 #width: 8 #leftMargin: 0.0 #topMargin: 0.0 #rightMargin: 0.0 #bottomMargin: 0.0 #separateSummaryPage: false #separateTitlePage: false #showSummaryHeader: true #showSummaryFooter: true #showTitleHeader: true #showTitleFooter: true #breakDefinitions: #(#(#KRBreakDefinition #headerClass: #TestBreak #headerSelector: #reportComponentSpec #footerClass: #TestBreakFooter #footerSelector: #reportComponentSpec #level: 1 #newPage: false #selectorCollection: #(#initialLetter)) #(#KRBreakDefinition #headerClass: #TestBreakSecondLevel #headerSelector: #reportComponentSpec #footerClass: #TestBreakFooterSecondLevel #footerSelector: #reportComponentSpec #level: 2 #newPage: false #selectorCollection: #(#twoFirstLetters))))!

testReportWithMultipleBreaksWithHeaderAndFooterWithVariableHeightWithSummaryInfo

	<resource: #jpmReport>
	^#(#KRReportDefinition #bodyClass: #TestBody #bodySelector: #reportComponentSpec #headerClass: #TestHeader #headerSelector: #reportComponentSpec #footerClass: #TestFooter #footerSelector: #reportComponentSpec #titleClass: #KRDemoTitle #titleSelector: #reportComponentSpec #summaryClass: #KRDemoSummary #summarySelector: #reportComponentSpec #height: 10.5 #width: 8 #leftMargin: 0.0 #topMargin: 0.0 #rightMargin: 0.0 #bottomMargin: 0.0 #separateSummaryPage: false #separateTitlePage: false #showSummaryHeader: true #showSummaryFooter: true #showTitleHeader: true #showTitleFooter: true #breakDefinitions: #(#(#KRMuThBreakDefinition #headerClass: #TestBreakWithSummaryInfoWithVariableHeight #headerSelector: #reportComponentSpec #footerClass: #TestBreakFooterWithVariableHeight #footerSelector: #reportComponentSpec #level: 1 #newPage: false #selectorCollection: #(#initialLetter) #hasSummaryInfo: true) #(#KRMuThBreakDefinition #headerClass: #TestBreakSecondLevelWithVariableHeightWithSummaryInfo #headerSelector: #reportComponentSpec #footerClass: #TestBreakFooterSecondLevelWithVariableHeight #footerSelector: #reportComponentSpec #level: 2 #newPage: false #selectorCollection: #(#twoFirstLetters) #hasSummaryInfo: true)))!

testReportWithMultipleBreaksWithSummaryInfo

	<resource: #jpmReport>
	^#(#KRReportDefinition #bodyClass: #TestBody #bodySelector: #reportComponentSpec #headerClass: #TestHeader #headerSelector: #reportComponentSpec #footerClass: #TestFooter #footerSelector: #reportComponentSpec #titleClass: #KRDemoTitle #titleSelector: #reportComponentSpec #summaryClass: #KRDemoSummary #summarySelector: #reportComponentSpec #height: 10.5 #width: 8 #leftMargin: 0.0 #topMargin: 0.0 #rightMargin: 0.0 #bottomMargin: 0.0 #separateSummaryPage: false #separateTitlePage: false #showSummaryHeader: true #showSummaryFooter: true #showTitleHeader: true #showTitleFooter: true #breakDefinitions: #(#(#KRMuThBreakDefinition #headerClass: #TestBreakWithSummaryInfo #headerSelector: #reportComponentSpec #footerClass: #TestBreakFooter #footerSelector: #reportComponentSpec #level: 1 #newPage: false #selectorCollection: #(#initialLetter) #hasSummaryInfo: true) #(#KRMuThBreakDefinition #headerClass: #TestBreakSecondLevelWithSummaryInfo #headerSelector: #reportComponentSpec #footerClass: #TestBreakFooterSecondLevel #footerSelector: #reportComponentSpec #level: 2 #newPage: false #selectorCollection: #(#twoFirstLetters) #hasSummaryInfo: true)))!

testReportWithMultipleBreaksWithVariableHeight

	<resource: #jpmReport>
	^#(#KRReportDefinition #bodyClass: #TestBody #bodySelector: #reportComponentSpec #headerClass: #TestHeader #headerSelector: #reportComponentSpec #footerClass: #TestFooter #footerSelector: #reportComponentSpec #titleClass: #KRDemoTitle #titleSelector: #reportComponentSpec #summaryClass: #KRDemoSummary #summarySelector: #reportComponentSpec #height: 10.5 #width: 8 #leftMargin: 0.0 #topMargin: 0.0 #rightMargin: 0.0 #bottomMargin: 0.0 #separateSummaryPage: false #separateTitlePage: false #showSummaryHeader: true #showSummaryFooter: true #showTitleHeader: true #showTitleFooter: true #breakDefinitions: #(#(#KRBreakDefinition #headerClass: #TestBreakWithVariableHeight #headerSelector: #reportComponentSpec #footerClass: #TestBreakFooter #footerSelector: #reportComponentSpec #level: 1 #newPage: false #selectorCollection: #(#initialLetter)) #(#KRBreakDefinition #headerClass: #TestBreakSecondLevelWithVariableHeight #headerSelector: #reportComponentSpec #footerClass: #TestBreakFooterSecondLevel #footerSelector: #reportComponentSpec #level: 2 #newPage: false #selectorCollection: #(#twoFirstLetters))))!

testReportWithPageLayout

	<resource: #jpmReport>
	^#(#KRMuThReportDefinition #bodyClass: #TestBody #bodySelector: #reportComponentSpec #headerClass: #TestHeader #headerSelector: #reportComponentSpec #footerClass: #TestFooter #footerSelector: #reportComponentSpec #titleClass: #KRDemoTitle #titleSelector: #reportComponentSpec #summaryClass: #KRDemoSummary #summarySelector: #reportComponentSpec #height: 10.5 #width: 8.0 #leftMargin: 0.0 #topMargin: 0.0 #rightMargin: 0.0 #bottomMargin: 0.0 #separateSummaryPage: false #separateTitlePage: false #showSummaryHeader: true #showSummaryFooter: true #showTitleHeader: true #showTitleFooter: true #splitBodyOnPageBreak: false #minBodyOrphanHeight: 16 #displayClientWindowLabel: 'Report Viewer' #breakDefinitions: #(#(#KRBreakDefinition #headerClass: #TestBreak #headerSelector: #reportComponentSpec #footerClass: #TestBreakFooter #footerSelector: #reportComponentSpec #level: 1 #newPage: false #selectorCollection: #(#initialLetter))) #pageLayout: #(#KRMuThReportPageLayout #name: #Custom #unitPrivate: #in #width: 8.5 #height: 11 #leftMargin: 0 #topMargin: 0 #rightMargin: 0 #bottomMargin: 0 #noPrintableHeight: 0.5 #noPrintableWidth: 0.5))!

testReportWithTopAndLeftMargins

	<resource: #jpmReport>
	^#(#KRReportDefinition #bodyClass: #TestBody #bodySelector: #reportComponentSpec #headerClass: #TestHeader #headerSelector: #reportComponentSpec #footerClass: #TestFooter #footerSelector: #reportComponentSpec #titleClass: #KRDemoTitle #titleSelector: #reportComponentSpec #summaryClass: #KRDemoSummary #summarySelector: #reportComponentSpec #height: 6 #width: 7 #leftMargin: 0 #topMargin: 0 #rightMargin: 0.0 #bottomMargin: 0.0 #separateSummaryPage: false #separateTitlePage: false #showSummaryHeader: true #showSummaryFooter: true #showTitleHeader: true #showTitleFooter: true #breakDefinitions: #(#(#KRBreakDefinition #headerClass: #TestBreak #headerSelector: #reportComponentSpec #footerClass: #TestBreakFooter #footerSelector: #reportComponentSpec #level: 1 #newPage: false #selectorCollection: #(#initialLetter))))!

testReportWithVariableHeight

	<resource: #jpmReport>
	^#(#KRMuThReportDefinition #bodyClass: #TestBodyWithVariableHeight #bodySelector: #reportComponentSpec #headerClass: #TestHeader #headerSelector: #reportComponentSpec #footerClass: #TestFooter #footerSelector: #reportComponentSpec #titleClass: #KRDemoTitle #titleSelector: #reportComponentSpec #summaryClass: #KRDemoSummary #summarySelector: #reportComponentSpec #height: 10.5 #width: 8 #leftMargin: 0.0 #topMargin: 0.0 #rightMargin: 0.0 #bottomMargin: 0.0 #separateSummaryPage: false #separateTitlePage: false #showSummaryHeader: true #showSummaryFooter: true #showTitleHeader: true #showTitleFooter: true #splitBodyOnPageBreak: false #minBodyOrphanHeight: 16 #displayClientWindowLabel: 'Report Viewer w/ Var height' #breakDefinitions: #(#(#KRBreakDefinition #headerClass: #TestBreak #headerSelector: #reportComponentSpec #footerClass: #TestBreakFooter #footerSelector: #reportComponentSpec #level: 1 #newPage: false #selectorCollection: #(#initialLetter))))! !

!KRMuThReportWriterTestHarness class publicMethodsFor: 'data'!

allClasses

	AllClasses isNil ifTrue: [self initAllClasses].
	^AllClasses!

clearAllClasses

	AllClasses := nil.
	^self!

collectionClasses

	^Collection withAllSubclasses asSet asSortedCollection: [:t2 :t3 | t2 name <= t3 name].!

initAllClasses

	AllClasses := Object withAllSubclasses asSet asSortedCollection: [:t1 :t2 | t1 name <= t2 name].
	^self!

testsWorkspace

	^self! !

!KRMuThReportWriterTestHarness class publicMethodsFor: 'examples'!

exampleOn: theReportName stream: theStream
	(KRMuThReportWriterTest reportName: theReportName executeOnStream: theStream reportObject: Collection) displayReport.!

examples
"
	KRMuThReportWriterTestHarness exampleOn: #testReport stream: self exampleStream.
	KRMuThReportWriterTestHarness exampleOn: #testReportProportionalX stream: self exampleStream.
	KRMuThReportWriterTestHarness exampleOn: #testReportWithBreakHeaderWithSummaryInfo stream: self exampleStream.
	KRMuThReportWriterTestHarness exampleOn: #testReportWithBreakHeaderWithSummaryInfoWithVariableHeight stream: self exampleStream.
	KRMuThReportWriterTestHarness exampleOn: #testReportWithBreaksWithVariableHeight stream: self exampleStream.
	KRMuThReportWriterTestHarness exampleOn: #testReportWithHeaderAndFooterBreaksWithVariableHeight stream: self exampleStream.
	KRMuThReportWriterTestHarness exampleOn: #testReportWithLongText stream: self exampleStream.

	KRMuThReportWriterTestHarness exampleOn: #testReportWithLongTextAllowSplit stream: self exampleStream.
	KRMuThReportWriterTestHarness exampleOn: #testReportWithMultipleBreaks stream: self exampleStream.
	KRMuThReportWriterTestHarness exampleOn: #testReportWithMultipleBreaksWithHeaderAndFooterWithVariableHeightWithSummaryInfo stream: self exampleStream.
	KRMuThReportWriterTestHarness exampleOn: #testReportWithMultipleBreaksWithSummaryInfo stream: self exampleStream.
	KRMuThReportWriterTestHarness exampleOn: #testReportWithMultipleBreaksWithVariableHeight stream: self exampleStream.
	KRMuThReportWriterTestHarness exampleOn: #testReportWithPageLayout stream: self exampleStream.
	KRMuThReportWriterTestHarness exampleOn: #testReportWithTopAndLeftMargins stream: self exampleStream.
	KRMuThReportWriterTestHarness exampleOn: #testReportWithVariableHeight stream: self exampleStream.


"!

exampleStream

	^self collectionClasses readStream! !

!KRMuThReportWriterTestHarness class publicMethodsFor: 'tests'!

allClassesDisplayReport

	(KRTestReportWriter
		reportName: #testReport
		executeOnStream: self allClasses readStream
		reportObject: Collection) displayReport.
	^self!

allClassesMuThDisplayReport

	(KRMuThReportWriterTest
		reportName: #testReport
		executeOnStream: self allClasses readStream
		reportObject: Collection) displayReport.
	^self!

someClassesDisplayReport

	(KRTestReportWriter
		reportName: #testReport
		executeOnStream: self collectionClasses readStream
		reportObject: Collection) displayReport.!

someClassesMuThDisplayReport

	(KRMuThReportWriterTest
		reportName: #testReport
		executeOnStream: self collectionClasses readStream
		reportObject: Collection) displayReport.! !

!KRRWMuThDynamicGenerationTest class publicMethodsFor: 'data'!

currentlyLoadedApplications

	^Application currentlyLoaded asSortedCollection: Class sortBlock!

fakeApplicationDataset

	| t1 t2 |
	t1 := Random new.
	t2 := OrderedCollection new.
	self currentlyLoadedApplications do: [:t3 | t3 subApplications do: [:t4 | t4 classes
				do: 
					[:t5 | 
					| t6 |
					(t6 := (1 to: 20)
								collect: [:t7 | (t1 next * 1000) rounded]) at: 1 put: t3 name asString.
					t6 at: 2 put: t4 name asString.
					t6 at: 3 put: t5 name asString.
					t2 add: t6]]].
	^t2!

smallApplicationDataset

	| t1 t2 |
	t1 := Random new.
	t2 := OrderedCollection new.
	(self currentlyLoadedApplications copyFrom: 1 to: 3)
		do: 
			[:t3 | 
			t3 classes
				do: 
					[:t4 | 
					| t5 |
					(t5 := (1 to: 20)
								collect: [:t6 | (t1 next * 1000) rounded]) at: 1 put: t3 name asString.
					t5 at: 2 put: nil.
					t5 at: 3 put: t4 name asString.
					t2 add: t5].
			t3 subApplications do: [:t7 | t7 classes
					do: 
						[:t8 | 
						| t9 |
						(t9 := (1 to: 20)
									collect: [:t10 | (t1 next * 1000) rounded]) at: 1 put: t3 name asString.
						t9 at: 2 put: t7 name asString.
						t9 at: 3 put: t8 name asString.
						t2 add: t9]]].
	^t2! !

!KRRWMuThDynamicGenerationTest class publicMethodsFor: 'examples'!

examples
"
	self randomWideLandscapeApplicationReport displayReport.
	self randomApplicationReportWithTwoRows displayReport.
	self randomApplicationReportWithBreakHeader displayReport.
	self randomLandscapeApplicationReport displayReport.
	self randomScaledLandscapeApplicationReport displayReport.
	self randomApplicationReport displayReport

"! !

!KRRWMuThDynamicGenerationTest class publicMethodsFor: 'reports'!

randomApplicationReport

	| t1 t2 t3 t4 |
	t1 := KRReportDefinition new.
	(t3 := Array new: 7) at: 1 put: ((KRTextCellSpec newString) right: 110; bottom: KRDynamicPosition new).
	t3 at: 2 put: ((KRTextCellSpec newString) left: (KRRelativePosition
				relativeToThe: #right
				of: 1
				offsetBy: 10); width: 150; bottom: KRDynamicPosition new).
	t3 at: 3 put: ((KRTextCellSpec newString) left: (KRRelativePosition
				relativeToThe: #right
				of: 2
				offsetBy: 10); width: 180; bottom: KRDynamicPosition new).
	t3 at: 4 put: ((KRTextCellSpec newNumber) alignment: #right; left: (KRRelativePosition
				relativeToThe: #right
				of: 3
				offsetBy: 10); width: 50; bottom: KRDynamicPosition new).
	t3 at: 5 put: ((KRTextCellSpec newNumber) alignment: #right; left: (KRRelativePosition
				relativeToThe: #right
				of: 4
				offsetBy: 10); width: 50; bottom: KRDynamicPosition new).
	t3 at: 6 put: ((KRTextCellSpec newNumber) alignment: #right; left: (KRRelativePosition
				relativeToThe: #right
				of: 5
				offsetBy: 10); width: 50; bottom: KRDynamicPosition new).
	t3 at: 7 put: ((KRTextCellSpec newNumber) alignment: #right; left: (KRRelativePosition
				relativeToThe: #right
				of: 6
				offsetBy: 10); width: 50; bottom: KRDynamicPosition new).
	t4 := KRRWBodyNodeCreator new cellSpecsAddingArrayAccess: t3.
	t1 bodyClass: t4.
	^(t2 := KRMuThReportWriter reportDefinition: t1) executeOnStream: self smallApplicationDataset readStream!

randomApplicationReportWithBreakHeader

	| t1 t2 t3 t4 t5 t6 t7 t8 t9 |
	t1 := KRReportDefinition new.
	(t3 := Array new: 7) at: 1 put: ((KRTextCellSpec newString) right: 100; bottom: KRDynamicPosition new).
	t3 at: 2 put: ((KRTextCellSpec newString) left: (KRRelativePosition
				relativeToThe: #right
				of: 1
				offsetBy: 10); width: 50; bottom: KRDynamicPosition new).
	t3 at: 3 put: ((KRTextCellSpec newString) left: (KRRelativePosition
				relativeToThe: #right
				of: 2
				offsetBy: 10); width: 50; bottom: KRDynamicPosition new).
	t3 at: 4 put: ((KRTextCellSpec newNumber) alignment: #right; left: (KRRelativePosition
				relativeToThe: #right
				of: 3
				offsetBy: 10); width: 50; bottom: KRDynamicPosition new).
	t3 at: 5 put: ((KRTextCellSpec newNumber) alignment: #right; left: (KRRelativePosition
				relativeToThe: #right
				of: 4
				offsetBy: 10); width: 50; bottom: KRDynamicPosition new).
	t3 at: 6 put: ((KRTextCellSpec newNumber) alignment: #right; left: (KRRelativePosition
				relativeToThe: #right
				of: 5
				offsetBy: 10); width: 50; bottom: KRDynamicPosition new).
	t3 at: 7 put: ((KRTextCellSpec newNumber) alignment: #right; left: (KRRelativePosition
				relativeToThe: #right
				of: 6
				offsetBy: 10); width: 50; bottom: KRDynamicPosition new).
	t4 := KRRWBodyNodeCreator new cellSpecsAddingArrayAccess: t3.
	t1 bodyClass: t4.
	t5 := OrderedCollection new.
	(t6 := KRMuThBreakDefinition new) selectorCollection: #(#first).
	t6 hasSummaryInfo: true.
	(t3 := Array new: 7) at: 1 put: ((KRTextCellSpec newString) right: 220; bottom: 16).
	t3 at: 2 put: ((KRTextCellSpec newString) left: (KRRelativePosition
				relativeToThe: #right
				of: 1
				offsetBy: 0); width: 0; bottom: 16).
	t3 at: 3 put: ((KRTextCellSpec newString) left: (KRRelativePosition
				relativeToThe: #right
				of: 2
				offsetBy: 0); width: 0; bottom: 16).
	t3 at: 4 put: ((KRTextCellSpec newNumber) alignment: #right; left: (KRRelativePosition
				relativeToThe: #right
				of: 3
				offsetBy: 10); width: 50; bottom: 16).
	t3 at: 5 put: ((KRTextCellSpec newNumber) alignment: #right; left: (KRRelativePosition
				relativeToThe: #right
				of: 4
				offsetBy: 10); width: 50; bottom: 16).
	t3 at: 6 put: ((KRTextCellSpec newNumber) alignment: #right; left: (KRRelativePosition
				relativeToThe: #right
				of: 5
				offsetBy: 10); width: 50; bottom: 16).
	t3 at: 7 put: ((KRTextCellSpec newNumber) alignment: #right; formatString: '[blue]#'; left: (KRRelativePosition
				relativeToThe: #right
				of: 6
				offsetBy: 10); width: 50; bottom: 16).
	t8 := 1.
	t9 := t3 size.
	[t8 <= t9]
		whileTrue: 
			[(t3 at: t8)
				getBlock: [:t10 | t10 at: t8].
			(t3 at: t8)
				extraEmphasis: #bold.
			t8 := t8 + 1].
	t7 := (KRRWAggregateNodeCreator new) accumulatorPrototype: KRArraySummingAccumulator new; cellSpecsAddingArrayAccess: t3.
	t6 headerClass: t7.
	t5 add: t6.
	t1 breakDefinitions: t5.
	^(t2 := KRMuThReportWriter reportDefinition: t1) executeOnStream: self fakeApplicationDataset readStream!

randomApplicationReportWithTwoRows

	| t1 t2 t3 t4 |
	t1 := KRReportDefinition new.
	(t3 := Array new: 7) at: 1 put: ((KRTextCellSpec newString) right: 100; bottom: KRDynamicPosition new).
	t3 at: 2 put: ((KRTextCellSpec newString) left: (KRRelativePosition
				relativeToThe: #right
				of: 1
				offsetBy: 10); width: 50; bottom: KRDynamicPosition new).
	t3 at: 3 put: ((KRTextCellSpec newString) left: (KRRelativePosition
				relativeToThe: #right
				of: 2
				offsetBy: 10); width: 50; bottom: KRDynamicPosition new).
	t3 at: 4 put: ((KRTextCellSpec newNumber) alignment: #right; left: (KRRelativePosition
				relativeToThe: #right
				of: 3
				offsetBy: 10); width: 50; bottom: KRDynamicPosition new).
	t3 at: 5 put: ((KRTextCellSpec newNumber) alignment: #right; left: (KRRelativePosition
				relativeToThe: #right
				of: 4
				offsetBy: 10); width: 50; bottom: KRDynamicPosition new).
	t3 at: 6 put: ((KRTextCellSpec newNumber) alignment: #right; left: (KRRelativePosition
				relativeToThe: #right
				of: 5
				offsetBy: 10); width: 50; bottom: KRDynamicPosition new).
	t3 at: 7 put: ((KRTextCellSpec newNumber) alignment: #right; left: (KRRelativePosition
				relativeToThe: #right
				of: 6
				offsetBy: 10); width: 50; top: (KRCompositeRelativePosition
				relativeToThe: #bottom
				of: (1 to: 6)
				composedBy: [:t5 :t6 | t5 max: t6]
				offsetBy: 2); height: KRDynamicPosition new).
	t4 := KRRWBodyNodeCreator new cellSpecsAddingArrayAccess: t3.
	t1 bodyClass: t4.
	^(t2 := KRMuThReportWriter reportDefinition: t1) executeOnStream: self fakeApplicationDataset readStream!

randomLandscapeApplicationReport

	| t1 t2 t3 t4 |
	t1 := KRReportDefinition new.
	(t3 := Array new: 7) at: 1 put: ((KRTextCellSpec newString) right: 150; bottom: 16).
	t3 at: 2 put: ((KRTextCellSpec newString) left: (KRRelativePosition
				relativeToThe: #right
				of: 1
				offsetBy: 10); width: 150; bottom: 16).
	t3 at: 3 put: ((KRTextCellSpec newString) left: (KRRelativePosition
				relativeToThe: #right
				of: 2
				offsetBy: 10); width: 150; bottom: 16).
	t3 at: 4 put: ((KRTextCellSpec newNumber) left: (KRRelativePosition
				relativeToThe: #right
				of: 3
				offsetBy: 10); width: 50; bottom: 16).
	t3 at: 5 put: ((KRTextCellSpec newNumber) left: (KRRelativePosition
				relativeToThe: #right
				of: 4
				offsetBy: 10); width: 50; bottom: 16).
	t3 at: 6 put: ((KRTextCellSpec newNumber) left: (KRRelativePosition
				relativeToThe: #right
				of: 5
				offsetBy: 10); width: 50; bottom: 16).
	t3 at: 7 put: ((KRTextCellSpec newNumber) left: (KRRelativePosition
				relativeToThe: #right
				of: 6
				offsetBy: 10); width: 50; bottom: 16).
	t4 := KRRWBodyNodeCreator new cellSpecsAddingArrayAccess: t3.
	t1 bodyClass: t4.
	t1 width: 11; height: 8.5.
	^(t2 := KRMuThReportWriter reportDefinition: t1) executeOnStream: self smallApplicationDataset readStream!

randomScaledLandscapeApplicationReport

	| t1 t2 t3 t4 t5 |
	t1 := KRReportDefinition new.
	(t3 := Array new: 12) at: 1 put: ((KRTextCellSpec newString) right: 150; bottom: 16).
	t3 at: 2 put: ((KRTextCellSpec newString) left: (KRRelativePosition
				relativeToThe: #right
				of: 1
				offsetBy: 10); width: 120; bottom: 16).
	t3 at: 3 put: ((KRTextCellSpec newString) left: (KRRelativePosition
				relativeToThe: #right
				of: 2
				offsetBy: 10); width: 120; bottom: 16).
	t3 at: 4 put: ((KRTextCellSpec newNumber) left: (KRRelativePosition
				relativeToThe: #right
				of: 3
				offsetBy: 10); width: 100; bottom: 16).
	t3 at: 5 put: ((KRTextCellSpec newNumber) left: (KRRelativePosition
				relativeToThe: #right
				of: 4
				offsetBy: 10); width: 100; bottom: 16).
	t3 at: 6 put: ((KRTextCellSpec newNumber) left: (KRRelativePosition
				relativeToThe: #right
				of: 5
				offsetBy: 10); width: 100; bottom: 16).
	t3 at: 7 put: ((KRTextCellSpec newNumber) left: (KRRelativePosition
				relativeToThe: #right
				of: 6
				offsetBy: 10); width: 100; bottom: 16).
	t3 at: 8 put: ((KRTextCellSpec newNumber) left: (KRRelativePosition
				relativeToThe: #right
				of: 7
				offsetBy: 10); width: 100; bottom: 16).
	t3 at: 9 put: ((KRTextCellSpec newNumber) left: (KRRelativePosition
				relativeToThe: #right
				of: 8
				offsetBy: 10); width: 100; bottom: 16).
	t3 at: 10 put: ((KRTextCellSpec newNumber) left: (KRRelativePosition
				relativeToThe: #right
				of: 9
				offsetBy: 10); width: 100; bottom: 16).
	t3 at: 11 put: ((KRTextCellSpec newNumber) left: (KRRelativePosition
				relativeToThe: #right
				of: 10
				offsetBy: 10); width: 100; bottom: 16).
	t3 at: 12 put: ((KRTextCellSpec newNumber) left: (KRRelativePosition
				relativeToThe: #right
				of: 11
				offsetBy: 10); width: 100; bottom: 16).
	t4 := KRRWBodyNodeCreator new cellSpecsAddingArrayAccess: t3.
	t1 bodyClass: t4.
	t1 width: 21; height: 16.
	(t5 := KRPrintPagerPreferences new) landscape: true.
	t5 fitToPage: true.
	t1 defaultPrintPagerPreferences: t5.
	^(t2 := KRMuThReportWriter reportDefinition: t1) executeOnStream: self smallApplicationDataset readStream!

randomWideLandscapeApplicationReport

	| t1 t2 t3 t4 |
	t1 := KRReportDefinition new.
	(t3 := Array new: 12) at: 1 put: ((KRTextCellSpec newString) right: 150; bottom: 16).
	t3 at: 2 put: ((KRTextCellSpec newString) left: (KRRelativePosition
				relativeToThe: #right
				of: 1
				offsetBy: 10); width: 120; bottom: 16).
	t3 at: 3 put: ((KRTextCellSpec newString) left: (KRRelativePosition
				relativeToThe: #right
				of: 2
				offsetBy: 10); width: 120; bottom: 16).
	t3 at: 4 put: ((KRTextCellSpec newNumber) left: (KRRelativePosition
				relativeToThe: #right
				of: 3
				offsetBy: 10); width: 100; bottom: 16).
	t3 at: 5 put: ((KRTextCellSpec newNumber) left: (KRRelativePosition
				relativeToThe: #right
				of: 4
				offsetBy: 10); width: 100; bottom: 16).
	t3 at: 6 put: ((KRTextCellSpec newNumber) left: (KRRelativePosition
				relativeToThe: #right
				of: 5
				offsetBy: 10); width: 100; bottom: 16).
	t3 at: 7 put: ((KRTextCellSpec newNumber) left: (KRRelativePosition
				relativeToThe: #right
				of: 6
				offsetBy: 10); width: 100; bottom: 16).
	t3 at: 8 put: ((KRTextCellSpec newNumber) left: (KRRelativePosition
				relativeToThe: #right
				of: 7
				offsetBy: 10); width: 100; bottom: 16).
	t3 at: 9 put: ((KRTextCellSpec newNumber) left: (KRRelativePosition
				relativeToThe: #right
				of: 8
				offsetBy: 10); width: 100; bottom: 16).
	t3 at: 10 put: ((KRTextCellSpec newNumber) left: (KRRelativePosition
				relativeToThe: #right
				of: 9
				offsetBy: 10); width: 100; bottom: 16).
	t3 at: 11 put: ((KRTextCellSpec newNumber) left: (KRRelativePosition
				relativeToThe: #right
				of: 10
				offsetBy: 10); width: 100; bottom: 16).
	t3 at: 12 put: ((KRTextCellSpec newNumber) left: (KRRelativePosition
				relativeToThe: #right
				of: 11
				offsetBy: 10); width: 100; bottom: 16).
	t4 := KRRWBodyNodeCreator new cellSpecsAddingArrayAccess: t3.
	t1 bodyClass: t4.
	t1 width: 21; height: 8.
	^(t2 := KRMuThReportWriter reportDefinition: t1) executeOnStream: self smallApplicationDataset readStream! !

KRMuThClassComplexityReportWriterTestHarness initializeAfterLoad!
KRMuThReportWriterTestHarness initializeAfterLoad!
KRMuThClassComplexityReportWriter initializeAfterLoad!
KRMuThReportWriterTest initializeAfterLoad!
KRRWMuThDynamicGenerationTest initializeAfterLoad!
KRDemoSummary initializeAfterLoad!
KRDemoSummaryProportional initializeAfterLoad!
KRDemoTitle initializeAfterLoad!
KRDemoTitleProportional initializeAfterLoad!
KRONO_Reporter_MultiThreaded_Test initializeAfterLoad!

KRONO_Reporter_MultiThreaded_Test loaded!
