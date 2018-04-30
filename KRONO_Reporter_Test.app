'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:38:03 pm'!



((KRONO_Reporter createSubApplication: #KRONO_Reporter_Test in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

KRONO_Reporter_Test becomeDefault!

KRReportWriter subclass: #KRTestReportWriter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

Object subclass: #KRRWClassNode
	instanceVariableNames: 'containedClass application '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

Object subclass: #KRRWComplexityReportTestHarness
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

Object subclass: #KRRWDynamicGenerationTester
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

Object subclass: #KRTestHarness
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

KRTestHarness subclass: #KRReportWriterTestHarness
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

KRAccumulatingReportSection subclass: #KRRWClassComplexityAccumulatingSections
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

KRReportSection subclass: #KRRWApplicationComplexityHeader
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

KRSimpleReportSection subclass: #KRRWApplicationComplexityBody
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

KRSimpleReportSection subclass: #KRRWClassComplexitySections
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

KRReportSection subclass: #TestBody
	instanceVariableNames: 'className instVarNames '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

TestBody subclass: #TestBodyProportional
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

TestBody subclass: #TestBodyWithLongText
	instanceVariableNames: 'comment '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

TestBody subclass: #TestBodyWithVariableHeight
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

KRReportSection subclass: #TestBreak
	instanceVariableNames: 'letter '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

TestBreak subclass: #TestBreakProportional
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

TestBreak subclass: #TestBreakSecondLevel
	instanceVariableNames: 'twoFirstLetters '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

TestBreakSecondLevel subclass: #TestBreakSecondLevelWithSummaryInfo
	instanceVariableNames: 'numberOfClasses '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

TestBreakSecondLevelWithSummaryInfo subclass: #TestBreakSecondLevelWithVariableHeightWithSummaryInfo
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

TestBreakSecondLevel subclass: #TestBreakSecondLevelWithVariableHeight
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

TestBreak subclass: #TestBreakWithSummaryInfo
	instanceVariableNames: 'numberOfClasses '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

TestBreakWithSummaryInfo subclass: #TestBreakWithSummaryInfoWithVariableHeight
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

TestBreak subclass: #TestBreakWithVariableHeight
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

KRReportSection subclass: #TestBreakFooter
	instanceVariableNames: 'count '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

TestBreakFooter subclass: #TestBreakFooterProportional
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

TestBreakFooter subclass: #TestBreakFooterSecondLevel
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

TestBreakFooter subclass: #TestBreakFooterSecondLevelWithVariableHeight
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

TestBreakFooter subclass: #TestBreakFooterWithVariableHeight
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

KRReportSection subclass: #TestFooter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

TestFooter subclass: #TestFooterProportional
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

KRReportSection subclass: #TestHeader
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

TestHeader subclass: #TestHeaderProportional
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

SubApplication subclass: #KRONO_Reporter_Test
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter_Test becomeDefault!

!KRReportWriterTestHarness class publicMethodsFor: 'examples'!

examples
"
KRReportWriterTestHarness new allClassesReport open.

KRReportWriterTestHarness new collectionClassesReport open.

"! !

!KRReportWriterTestHarness publicMethodsFor: 'tests'!

allClassesReport

	| t1 |
	t1 := Object withAllSubclasses asSet asSortedCollection: [:t2 :t3 | t2 name <= t3 name].
	^KRTestReportWriter
		reportName: #testReport
		executeOnStream: t1 readStream
		reportObject: Collection!

collectionClassesReport

	| t1 |
	t1 := Collection withAllSubclasses asSet asSortedCollection: [:t2 :t3 | t2 name <= t3 name].
	^KRTestReportWriter
		reportName: #testReport
		executeOnStream: t1 readStream
		reportObject: Collection! !

!KRRWApplicationComplexityBody class publicMethodsFor: 'interface specs'!

reportComponentSpec
	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 0 0 637.5 16)) #component: #(#SpecCollection #collection: #(#(#InputFieldSpec #layout: #(#LayoutFrame 12 0 0 0 229 0 0 1) #flags: 0 #model: #applicationName #tabable: false #style: #default #isReadOnly: true) #(#InputFieldSpec #layout: #(#LayoutFrame 271 0 0 0 403 0 0 1) #flags: 0 #model: #numberOfClasses #tabable: false #alignment: #right #style: #default #isReadOnly: true #type: #number) #(#InputFieldSpec #layout: #(#LayoutFrame 420 0 0 0 594 0 0 1) #flags: 0 #model: #numberOfMethods #tabable: false #alignment: #right #style: #default #isReadOnly: true #type: #number))))! !

!KRRWApplicationComplexityHeader class publicMethodsFor: 'interface specs'!

reportComponentSpec
	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 602 20) #max: #(#Point 602 20) #bounds: #(#Rectangle 0 0 602 20)) #component: #(#SpecCollection #collection: #(#(#LabelSpec #layout: #(#AlignmentOrigin 12 0 0 1 0 1) #label: 'Application Name' #style: #default) #(#LabelSpec #layout: #(#AlignmentOrigin 403 0 0 1 1 1) #label: 'Number of Classes' #style: #default) #(#LabelSpec #layout: #(#AlignmentOrigin 601 0 0 1 1 1) #label: 'Number of Methods' #style: #default))))! !

!KRRWClassComplexityAccumulatingSections class publicMethodsFor: 'interface specs'!

meanSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 0 0 602 100)) #component: #(#SpecCollection #collection: #(#(#RegionSpec #layout: #(#Rectangle 8 0 633 100) #colors: #(#LookPreferences #setBackgroundColor: #(#ColorValue 7700 7700 7700))) #(#InputFieldSpec #layout: #(#Rectangle 12 84 229 100) #flags: 0 #model: #'applicationName first' #tabable: false #style: #pixelSmall #isReadOnly: true) #(#InputFieldSpec #layout: #(#Rectangle 271 84 403 100) #flags: 0 #model: #count #tabable: false #alignment: #right #style: #pixelSmall #isReadOnly: true #type: #number) #(#InputFieldSpec #layout: #(#Rectangle 457 84 601 100) #flags: 0 #model: #'numberOfMethods mean' #tabable: false #alignment: #right #style: #pixelSmall #isReadOnly: true #type: #number) #(#ChartViewSpec #layout: #(#Rectangle 16 0 601 84) #model: #'numberOfMethods vector' #chartType: #BG_BarChartView #dataSeries: #(#ChartDataSpec #series: #(#(#ChartDataSetSpec #name: 'Methods' #look: #(#ChartElementLookSpec #lineWidth: 0)))) #options: #(#BarChartOptionsSpec #overlap: 0.0 #width: 1.0) #xAxis: #(#ChartingAxisSpec #scale: #(#ChartingScaleSpec #restrictions: #(#auto #zero #div #log)) #axisLine: false #borderLine: false #sectionMarkAt: #none) #yAxis: #(#ChartingValueAxisSpec #scale: #(#ChartingScaleSpec #invert: true) #tickPosition: #hideTick #axisLine: false #borderLine: false #sectionMarkAt: #none)))))!

summarySpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 0 0 602 100)) #component: #(#SpecCollection #collection: #(#(#RegionSpec #layout: #(#Rectangle 8 0 633 100) #colors: #(#LookPreferences #setBackgroundColor: #(#ColorValue 7700 7700 7700))) #(#InputFieldSpec #layout: #(#Rectangle 12 84 229 100) #flags: 0 #model: #'applicationName first' #tabable: false #style: #pixelSmall #isReadOnly: true) #(#InputFieldSpec #layout: #(#Rectangle 271 84 403 100) #flags: 0 #model: #count #tabable: false #alignment: #right #style: #pixelSmall #isReadOnly: true #type: #number) #(#InputFieldSpec #layout: #(#Rectangle 457 84 601 100) #flags: 0 #model: #'numberOfMethods sum' #tabable: false #alignment: #right #style: #pixelSmall #isReadOnly: true #type: #number) #(#ChartViewSpec #layout: #(#Rectangle 16 0 601 84) #model: #'numberOfMethods vector' #chartType: #BG_BarChartView #dataSeries: #(#ChartDataSpec #series: #(#(#ChartDataSetSpec #name: 'Methods' #look: #(#ChartElementLookSpec #lineWidth: 0)))) #options: #(#BarChartOptionsSpec #overlap: 0.0 #width: 1.0) #xAxis: #(#ChartingAxisSpec #scale: #(#ChartingScaleSpec #restrictions: #(#auto #zero #div #log)) #axisLine: false #borderLine: false #sectionMarkAt: #none) #yAxis: #(#ChartingValueAxisSpec #scale: #(#ChartingScaleSpec #invert: true) #tickPosition: #hideTick #axisLine: false #borderLine: false #sectionMarkAt: #none)))))! !

!KRRWClassComplexityAccumulatingSections publicMethodsFor: 'accessing'!

attributesToAccumulate

	^#(#applicationName #className #numberOfMethods)!

vectorAttributes

	^#(#numberOfMethods)! !

!KRRWClassComplexitySections class publicMethodsFor: 'interface specs'!

bodySpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 0 0) #bounds: #(#Rectangle 0 0 633 16)) #component: #(#SpecCollection #collection: #(#(#InputFieldSpec #layout: #(#Rectangle 62 0 229 16) #flags: 0 #model: #className #tabable: false #style: #pixelSmall #isReadOnly: true #type: #string) #(#InputFieldSpec #layout: #(#Rectangle 433 0 601 16) #flags: 0 #model: #numberOfMethods #tabable: false #alignment: #right #style: #pixelSmall #isReadOnly: true #type: #number))))! !

!KRRWClassNode class publicMethodsFor: 'instance creation'!

containedClass: t1 application: t2 

	| t3 |
	(t3 := self new) containedClass: t1.
	t3 application: t2.
	^t3! !

!KRRWClassNode publicMethodsFor: 'accessing'!

application

	^application!

application: t1 

	application := t1.
	^self!

containedClass

	^containedClass!

containedClass: t1 

	containedClass := t1.
	^self! !

!KRRWClassNode publicMethodsFor: 'report accessing'!

applicationName

	^self application rootApplication name!

className

	^self containedClass name!

numberOfMethods

	^(self containedClass methodNamesIn: self application) size + (self containedClass class methodNamesIn: self application) size! !

!KRRWComplexityReportTestHarness class publicMethodsFor: 'examples'!

examples
"
KRRWComplexityReportTestHarness new simpleClassTest  open.
KRRWComplexityReportTestHarness new applicationReportTest  open.
KRRWComplexityReportTestHarness new classAverageReport  open.
KRRWComplexityReportTestHarness new classReportTest  open.
KRRWComplexityReportTestHarness new programmatic1  open.
KRRWComplexityReportTestHarness new programmatic2  open.


"! !

!KRRWComplexityReportTestHarness publicMethodsFor: 'data generating'!

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

!KRRWComplexityReportTestHarness publicMethodsFor: 'tests'!

applicationReportTest

	^KRClassComplexityReportWriter
		reportName: #applicationReportSpec
		executeOnStream: self applicationsInUserSelectedConfigMap readStream
		reportObject: nil!

classAverageReport

	^KRClassComplexityReportWriter
		reportName: #classAverageReport
		executeOnStream: self classesInUserSelectedConfigMap readStream
		reportObject: nil!

classReportTest

	^KRClassComplexityReportWriter
		reportName: #classReportSpec
		executeOnStream: self classesInUserSelectedConfigMap readStream
		reportObject: nil!

programmatic1

	| t1 t2 |
	(t1 := KRReportDefinition new) bodyClass: KRRWApplicationComplexityBody.
	t1 headerClass: KRRWApplicationComplexityHeader.
	t1 footerClass: TestFooter.
	^(t2 := KRReportWriter reportDefinition: t1) executeOn: self applicationsInUserSelectedConfigMap!

programmatic2

	| t1 t2 |
	(t1 := KRReportDefinition new) bodyClass: KRRWApplicationComplexityBody.
	t1 headerClass: KRRWApplicationComplexityHeader.
	t1 footerClass: TestFooter.
	t1 titleClass: KRDemoTitle.
	t1 showSummaryHeader: false.
	^(t2 := KRReportWriter reportDefinition: t1) executeOnStream: self applicationsInUserSelectedConfigMap readStream!

simpleClassTest

	^KRClassComplexityReportWriter
		reportName: #simpleClassReportSpec
		executeOnStream: self classesInUserSelectedConfigMap readStream
		reportObject: nil! !

!KRRWDynamicGenerationTester class publicMethodsFor: 'examples'!

examples
"
KRRWDynamicGenerationTester new randomApplicationReport open.
KRRWDynamicGenerationTester new randomApplicationReportWithBreakHeader open.
KRRWDynamicGenerationTester new randomApplicationReportWithTwoRows open.
KRRWDynamicGenerationTester new randomLandscapeApplicationReport open.
KRRWDynamicGenerationTester new randomScaledLandscapeApplicationReport open.
KRRWDynamicGenerationTester new randomWideLandscapeApplicationReport open

"! !

!KRRWDynamicGenerationTester publicMethodsFor: 'data'!

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

!KRRWDynamicGenerationTester publicMethodsFor: 'reports'!

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
	^(t2 := KRReportWriter reportDefinition: t1) executeOnStream: self smallApplicationDataset readStream!

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
	(t6 := KRBreakDefinition new) selectorCollection: #(#first).
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
	^(t2 := KRReportWriter reportDefinition: t1) executeOnStream: self fakeApplicationDataset readStream!

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
	^(t2 := KRReportWriter reportDefinition: t1) executeOnStream: self fakeApplicationDataset readStream!

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
	^(t2 := KRReportWriter reportDefinition: t1) executeOnStream: self smallApplicationDataset readStream!

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
	^(t2 := KRReportWriter reportDefinition: t1) executeOnStream: self smallApplicationDataset readStream!

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
	^(t2 := KRReportWriter reportDefinition: t1) executeOnStream: self smallApplicationDataset readStream! !

!KRTestHarness class publicMethodsFor: 'examples'!

examples
"

KRReportWriterTestHarness new allClassesReport open.
KRReportWriterTestHarness new collectionClassesReport open.

KRRWDynamicGenerationTester new randomApplicationReport open.
KRRWDynamicGenerationTester new randomApplicationReportWithBreakHeader open.
KRRWDynamicGenerationTester new randomApplicationReportWithTwoRows open.
KRRWDynamicGenerationTester new randomLandscapeApplicationReport open.
KRRWDynamicGenerationTester new randomScaledLandscapeApplicationReport open.
KRRWDynamicGenerationTester new randomWideLandscapeApplicationReport open


KRRWComplexityReportTestHarness new simpleClassTest  open.
KRRWComplexityReportTestHarness new applicationReportTest  open.
KRRWComplexityReportTestHarness new classAverageReport  open.
KRRWComplexityReportTestHarness new classReportTest  open.
KRRWComplexityReportTestHarness new programmatic1  open.
KRRWComplexityReportTestHarness new programmatic2  open.


	KRMuThReportWriterTestHarness someClassesDisplayReport.
	KRMuThReportWriterTestHarness allClassesDisplayReport.
	KRMuThReportWriterTestHarness someClassesMuThDisplayReport.
	KRMuThReportWriterTestHarness allClassesMuThDisplayReport.


KRMuThClassComplexityReportWriterTestHarness  simpleClassTest  displayReport.
KRMuThClassComplexityReportWriterTestHarness  applicationReportTest  displayReport.
KRMuThClassComplexityReportWriterTestHarness  classAverageReport  displayReport.
KRMuThClassComplexityReportWriterTestHarness  classReportTest  displayReport.
KRMuThClassComplexityReportWriterTestHarness  programmatic1  displayReport.
KRMuThClassComplexityReportWriterTestHarness  programmatic2  displayReport.


"! !

!KRTestHarness publicMethodsFor: 'action'!

performTest: t1 

	self perform: t1.
	^self! !

!KRTestHarness publicMethodsFor: 'exceptions'!

fail

	^self error: 'A test in the test harness has failed.'! !

!KRTestHarness publicMethodsFor: 'test classification'!

automatedTests

	^#()!

observationTests

	^#()! !

!KRTestHarness publicMethodsFor: 'test harness'!

runAllAutomatedTests

	self automatedTests do: [:t1 | self performTest: t1].
	^self!

runAllObservationTests

	self observationsTests do: [:t1 | self performTest: t1].
	^self! !

!KRTestReportWriter class publicMethodsFor: 'report specs'!

testReport

	<resource: #jpmReport>
	^#(#KRReportDefinition #bodyClass: #TestBody #bodySelector: #reportComponentSpec #headerClass: #TestHeader #headerSelector: #reportComponentSpec #footerClass: #TestFooter #footerSelector: #reportComponentSpec #titleClass: #KRDemoTitle #titleSelector: #reportComponentSpec #summaryClass: #KRDemoSummary #summarySelector: #reportComponentSpec #height: 10.5 #width: 8 #leftMargin: 0.0 #topMargin: 0.0 #rightMargin: 0.0 #bottomMargin: 0.0 #separateSummaryPage: false #separateTitlePage: false #showSummaryHeader: true #showSummaryFooter: true #showTitleHeader: true #showTitleFooter: true #breakDefinitions: #(#(#KRBreakDefinition #headerClass: #TestBreak #headerSelector: #reportComponentSpec #footerClass: #TestBreakFooter #footerSelector: #reportComponentSpec #level: 1 #newPage: false #selectorCollection: #(#initialLetter))))! !

!TestBody class publicMethodsFor: 'interface specs'!

reportComponentSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 638 20) #max: #(#Point 638 20) #bounds: #(#Rectangle 0 0 638 20)) #component: #(#SpecCollection #collection: #(#(#InputFieldSpec #layout: #(#Rectangle 3 0 239 21) #model: #className #tabable: false #alignment: #center #style: #small #isReadOnly: true) #(#InputFieldSpec #layout: #(#Rectangle 237 0 594 21) #model: #instVarNames #tabable: true #style: #small #isReadOnly: true #type: #object))))! !

!TestBody publicMethodsFor: 'aspects'!

className

	className isNil ifTrue: [^(className := AspectAdaptor subject: self dataObject) forAspect: #name].
	^className!

instVarNames

	instVarNames isNil ifTrue: [^(instVarNames := AspectAdaptor subject: self dataObject) forAspect: #instVarNames].
	^instVarNames! !

!TestBodyProportional class publicMethodsFor: 'interface specs'!

reportComponentSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 638 20) #max: #(#Point 638 20) #bounds: #(#Rectangle 211 273 849 293)) #component: #(#SpecCollection #collection: #(#(#InputFieldSpec #layout: #(#LayoutFrame 3 0 0 0 0 0.416666 21 0) #model: #className #tabable: false #alignment: #center #style: #small #isReadOnly: true) #(#InputFieldSpec #layout: #(#LayoutFrame 0 0.416666 0 0 -3 1 21 0) #model: #instVarNames #tabable: true #style: #small #isReadOnly: true #type: #object))))! !

!TestBodyWithLongText class publicMethodsFor: 'interface specs'!

reportComponentSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 638 20) #max: #(#Point 638 20) #bounds: #(#Rectangle 496 181 1134 201)) #component: #(#SpecCollection #collection: #(#(#InputFieldSpec #layout: #(#LayoutFrame 3 0 0 0 239 0 0 1) #name: #classNameFieldID #model: #className #tabable: false #alignment: #center #style: #small #isReadOnly: true) #(#InputFieldSpec #layout: #(#LayoutFrame 237 0 0 0 394 0 0 1) #name: #instVarNamesFieldID #model: #instVarNames #tabable: true #style: #small #isReadOnly: true #type: #object) #(#TextEditorSpec #layout: #(#LayoutFrame 394 0 0 0 600 0 0 1) #name: #commentFieldID #flags: 12 #model: #comment))))! !

!TestBodyWithLongText publicMethodsFor: 'aspects'!

comment

	comment isNil ifTrue: [^(comment := AspectAdaptor subject: self dataObject) forAspect: #comment].
	^comment!

height

	^self className value size * 14 max: 22! !

!TestBodyWithVariableHeight class publicMethodsFor: 'interface specs'!

reportComponentSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 638 20) #max: #(#Point 638 20) #bounds: #(#Rectangle 372 341 1010 361)) #component: #(#SpecCollection #collection: #(#(#InputFieldSpec #layout: #(#LayoutFrame 3 0 0 0 239 0 0 1) #name: #classNameFieldID #model: #className #tabable: false #alignment: #center #style: #small #isReadOnly: true) #(#InputFieldSpec #layout: #(#LayoutFrame 237 0 0 0 594 0 0 1) #name: #instVarNamesFieldID #model: #instVarNames #tabable: true #style: #small #isReadOnly: true #type: #object))))! !

!TestBodyWithVariableHeight publicMethodsFor: 'aspects'!

height

	^self className value size * 4 max: 22! !

!TestBreak class publicMethodsFor: 'interface specs'!

reportComponentSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 0 0 637.5 32)) #component: #(#SpecCollection #collection: #(#(#InputFieldSpec #layout: #(#Rectangle 137 4 237 23) #flags: 0 #model: #initialLetter #tabable: false #style: #small #isReadOnly: true) #(#LabelSpec #layout: #(#Point 5 3) #label: 'Classes Starting with' #style: #small))))! !

!TestBreak publicMethodsFor: 'aspects'!

addDataPacket: t1 

	self initialLetter value: (t1 name printString copyFrom: 2 to: 2).
	^self!

initialLetter

	letter == nil ifTrue: [letter := 'A' asValue].
	^letter! !

!TestBreakFooter class publicMethodsFor: 'interface specs'!

reportComponentSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 0 0 637.5 32)) #component: #(#SpecCollection #collection: #(#(#InputFieldSpec #layout: #(#Rectangle 141 4 241 23) #model: #count #style: #small #type: #number) #(#LabelSpec #layout: #(#Point 41 2) #label: 'Total in group' #style: #small))))! !

!TestBreakFooter publicMethodsFor: 'aspects'!

count

	^(lastIndex - firstIndex + 1) asValue! !

!TestBreakFooterProportional class publicMethodsFor: 'interface specs'!

reportComponentSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 251 532 888.5 564)) #component: #(#SpecCollection #collection: #(#(#InputFieldSpec #layout: #(#LayoutFrame 5 0.25 3 0 0 0.5 22 0) #model: #count #style: #small #type: #number) #(#LabelSpec #layout: #(#LayoutFrame 5 0 3 0 0 0.25 22 0) #label: 'Total in group' #style: #small))))! !

!TestBreakFooterSecondLevel class publicMethodsFor: 'interface specs'!

reportComponentSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 0 0 637.5 32)) #component: #(#SpecCollection #collection: #(#(#InputFieldSpec #layout: #(#Rectangle 228 6 328 25) #model: #count #style: #small #type: #number) #(#LabelSpec #layout: #(#Point 68 6) #label: 'Total in two first letters group' #style: #small))))! !

!TestBreakFooterSecondLevelWithVariableHeight class publicMethodsFor: 'interface specs'!

reportComponentSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 344 306 981.5 338)) #component: #(#SpecCollection #collection: #(#(#InputFieldSpec #layout: #(#LayoutFrame 141 0 2 0 241 0 -2 1) #model: #count #style: #small #type: #number) #(#LabelSpec #layout: #(#AlignmentOrigin 41 0 0 0.5 0 0.5) #label: 'Total in group' #style: #small))))! !

!TestBreakFooterSecondLevelWithVariableHeight publicMethodsFor: 'aspects'!

height

	^self count value * 10 max: 20! !

!TestBreakFooterWithVariableHeight class publicMethodsFor: 'interface specs'!

reportComponentSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 375 177 1012.5 209)) #component: #(#SpecCollection #collection: #(#(#InputFieldSpec #layout: #(#LayoutFrame 141 0 2 0 241 0 -2 1) #model: #count #style: #small #type: #number) #(#LabelSpec #layout: #(#AlignmentOrigin 41 0 0 0.5 0 0.5) #label: 'Total in group' #style: #small))))! !

!TestBreakFooterWithVariableHeight publicMethodsFor: 'aspects'!

height

	^self count value * 10 max: 20! !

!TestBreakProportional class publicMethodsFor: 'interface specs'!

reportComponentSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 203 405 840.5 437)) #component: #(#SpecCollection #collection: #(#(#InputFieldSpec #layout: #(#LayoutFrame 5 0.25 3 0 0 0.5 22 0) #model: #initialLetter #tabable: false #style: #small #isReadOnly: true) #(#LabelSpec #layout: #(#LayoutFrame 5 0 3 0 0 0.25 22 0) #label: 'Classes Starting with' #style: #small))))! !

!TestBreakSecondLevel class publicMethodsFor: 'interface specs'!

reportComponentSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 0 0 637.5 32)) #component: #(#SpecCollection #collection: #(#(#LabelSpec #layout: #(#Point 68 7) #label: 'Break on two first letters' #style: #small) #(#InputFieldSpec #layout: #(#Rectangle 223 6 323 25) #flags: 0 #model: #twoFirstLetters #tabable: false #style: #small #isReadOnly: true))))! !

!TestBreakSecondLevel publicMethodsFor: 'aspects'!

addDataPacket: t1 

	super addDataPacket: t1.
	self twoFirstLetters value: (t1 name printString copyFrom: 2 to: 3).
	^self!

twoFirstLetters

	twoFirstLetters == nil ifTrue: [twoFirstLetters := 'Aa' asValue].
	^twoFirstLetters! !

!TestBreakSecondLevelWithSummaryInfo class publicMethodsFor: 'interface specs'!

reportComponentSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 0 0 637.5 32)) #component: #(#SpecCollection #collection: #(#(#LabelSpec #layout: #(#Point 68 7) #label: 'Break on two first letters' #style: #small) #(#InputFieldSpec #layout: #(#Rectangle 223 6 323 25) #flags: 0 #model: #twoFirstLetters #tabable: false #style: #small #isReadOnly: true) #(#InputFieldSpec #layout: #(#Rectangle 401 4 501 23) #flags: 0 #model: #numberOfClasses #tabable: false #style: #small #isReadOnly: true #type: #number))))! !

!TestBreakSecondLevelWithSummaryInfo publicMethodsFor: 'aspects'!

addDataPacket: t1 

	super addDataPacket: t1.
	self numberOfClasses: self numberOfClasses + 1.
	^self!

numberOfClasses

	numberOfClasses isNil ifTrue: [numberOfClasses := 0].
	^numberOfClasses!

numberOfClasses: t1 

	numberOfClasses := t1.
	^self! !

!TestBreakSecondLevelWithVariableHeight class publicMethodsFor: 'interface specs'!

reportComponentSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 229 209 866.5 241)) #component: #(#SpecCollection #collection: #(#(#LabelSpec #layout: #(#AlignmentOrigin 68 0 0 0.5 0 0.5) #label: 'Break on two first letters' #style: #small) #(#InputFieldSpec #layout: #(#LayoutFrame 223 0 6 0 323 0 -6 1) #model: #twoFirstLetters #tabable: false #style: #small #isReadOnly: true))))! !

!TestBreakSecondLevelWithVariableHeight publicMethodsFor: 'aspects'!

height

	^self twoFirstLetters value asUppercase first asInteger - $A asInteger + self twoFirstLetters value asUppercase last asInteger - $A asInteger + 1 * 7 max: 20! !

!TestBreakSecondLevelWithVariableHeightWithSummaryInfo class publicMethodsFor: 'interface specs'!

reportComponentSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 450 207 1087.5 239)) #component: #(#SpecCollection #collection: #(#(#LabelSpec #layout: #(#AlignmentOrigin 68 0 0 0.5 0 0.5) #label: 'Break on two first letters' #style: #small) #(#InputFieldSpec #layout: #(#LayoutFrame 223 0 2 0 323 0 -2 1) #model: #twoFirstLetters #tabable: false #style: #small #isReadOnly: true) #(#InputFieldSpec #layout: #(#LayoutFrame 401 0 2 0 501 0 -2 1) #model: #numberOfClasses #tabable: false #style: #small #isReadOnly: true #type: #number))))! !

!TestBreakSecondLevelWithVariableHeightWithSummaryInfo publicMethodsFor: 'aspects'!

height

	^self twoFirstLetters value asUppercase first asInteger - $A asInteger + self twoFirstLetters value asUppercase last asInteger - $A asInteger + 1 * 7 max: 20! !

!TestBreakWithSummaryInfo class publicMethodsFor: 'interface specs'!

reportComponentSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 219 296 856.5 328)) #component: #(#SpecCollection #collection: #(#(#InputFieldSpec #layout: #(#Rectangle 137 4 237 23) #flags: 0 #model: #initialLetter #tabable: false #style: #small #isReadOnly: true) #(#LabelSpec #layout: #(#Point 5 3) #label: 'Classes Starting with' #style: #small) #(#LabelSpec #layout: #(#Point 269 3) #label: 'Number of Classes' #style: #small) #(#InputFieldSpec #layout: #(#Rectangle 401 4 501 23) #flags: 0 #model: #numberOfClasses #tabable: false #style: #small #isReadOnly: true #type: #number))))! !

!TestBreakWithSummaryInfo publicMethodsFor: 'aspects'!

addDataPacket: t1 

	super addDataPacket: t1.
	self numberOfClasses: self numberOfClasses + 1.
	^self!

numberOfClasses

	numberOfClasses isNil ifTrue: [numberOfClasses := 0].
	^numberOfClasses!

numberOfClasses: t1 

	numberOfClasses := t1.
	^self! !

!TestBreakWithSummaryInfoWithVariableHeight class publicMethodsFor: 'interface specs'!

reportComponentSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 302 215 939.5 247)) #component: #(#SpecCollection #collection: #(#(#InputFieldSpec #layout: #(#LayoutFrame 137 0 2 0 237 0 -2 1) #model: #initialLetter #tabable: false #style: #small #isReadOnly: true) #(#LabelSpec #layout: #(#AlignmentOrigin 269 0 0 0.5 0 0.5) #label: 'Number of Classes' #style: #small) #(#InputFieldSpec #layout: #(#LayoutFrame 401 0 2 0 501 0 -2 1) #model: #numberOfClasses #tabable: false #style: #small #isReadOnly: true #type: #number) #(#LabelSpec #layout: #(#AlignmentOrigin 5 0 0 0.5 0 0.5) #label: 'Classes Starting with' #style: #small))))! !

!TestBreakWithSummaryInfoWithVariableHeight publicMethodsFor: 'aspects'!

height

	^self initialLetter value first asInteger - $A asInteger + 2 * 10 max: 20! !

!TestBreakWithVariableHeight class publicMethodsFor: 'interface specs'!

reportComponentSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 274 326 911.5 358)) #component: #(#SpecCollection #collection: #(#(#InputFieldSpec #layout: #(#LayoutFrame 137 0 4 0 237 0 0 1) #model: #initialLetter #tabable: false #style: #small #isReadOnly: true) #(#LabelSpec #layout: #(#LayoutFrame 5 0 3 0 117 0 0 1) #label: 'Classes Starting with' #style: #small))))! !

!TestBreakWithVariableHeight publicMethodsFor: 'aspects'!

height

	^self initialLetter value first asInteger - $A asInteger + 2 * 10 max: 20! !

!TestFooter class publicMethodsFor: 'interface specs'!

reportComponentSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 0 0 600 32)) #component: #(#SpecCollection #collection: #(#(#InputFieldSpec #layout: #(#Rectangle 590 4 600 25) #flags: 0 #model: #pageNumber #style: #small #isReadOnly: true #type: #number) #(#DividerSpec #layout: #(#Rectangle 1 1 600 5)) #(#DividerSpec #layout: #(#Rectangle 1 31 600 32)))))! !

!TestFooterProportional class publicMethodsFor: 'interface specs'!

reportComponentSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 313 322 913 354)) #component: #(#SpecCollection #collection: #(#(#InputFieldSpec #layout: #(#LayoutFrame -35 1 4 0 -5 1 25 0) #flags: 0 #model: #pageNumber #alignment: #right #style: #small #isReadOnly: true #type: #number) #(#DividerSpec #layout: #(#LayoutFrame 1 0 1 0 0 1 3 0)) #(#DividerSpec #layout: #(#LayoutFrame 1 0 -3 1 0 1 -1 1)))))! !

!TestHeader class publicMethodsFor: 'interface specs'!

reportComponentSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 0 0 637.5 32)) #component: #(#SpecCollection #collection: #(#(#LabelSpec #layout: #(#Point 166 1) #label: 'Class Report' #style: #small))))! !

!TestHeaderProportional class publicMethodsFor: 'interface specs'!

reportComponentSpec

	<resource: #canvas>
	^#(#FullSpec #window: #(#WindowSpec #label: 'Generated Canvas' #min: #(#Point 50 50) #bounds: #(#Rectangle 181 307 818.5 339)) #component: #(#SpecCollection #collection: #(#(#LabelSpec #layout: #(#AlignmentOrigin 0 0.5 1 0 0.5 0) #label: 'Class Report' #style: #small))))! !

KRTestReportWriter initializeAfterLoad!
KRRWClassNode initializeAfterLoad!
KRRWComplexityReportTestHarness initializeAfterLoad!
KRRWDynamicGenerationTester initializeAfterLoad!
KRTestHarness initializeAfterLoad!
KRReportWriterTestHarness initializeAfterLoad!
KRRWClassComplexityAccumulatingSections initializeAfterLoad!
KRRWApplicationComplexityHeader initializeAfterLoad!
KRRWApplicationComplexityBody initializeAfterLoad!
KRRWClassComplexitySections initializeAfterLoad!
TestBody initializeAfterLoad!
TestBodyProportional initializeAfterLoad!
TestBodyWithLongText initializeAfterLoad!
TestBodyWithVariableHeight initializeAfterLoad!
TestBreak initializeAfterLoad!
TestBreakProportional initializeAfterLoad!
TestBreakSecondLevel initializeAfterLoad!
TestBreakSecondLevelWithSummaryInfo initializeAfterLoad!
TestBreakSecondLevelWithVariableHeightWithSummaryInfo initializeAfterLoad!
TestBreakSecondLevelWithVariableHeight initializeAfterLoad!
TestBreakWithSummaryInfo initializeAfterLoad!
TestBreakWithSummaryInfoWithVariableHeight initializeAfterLoad!
TestBreakWithVariableHeight initializeAfterLoad!
TestBreakFooter initializeAfterLoad!
TestBreakFooterProportional initializeAfterLoad!
TestBreakFooterSecondLevel initializeAfterLoad!
TestBreakFooterSecondLevelWithVariableHeight initializeAfterLoad!
TestBreakFooterWithVariableHeight initializeAfterLoad!
TestFooter initializeAfterLoad!
TestFooterProportional initializeAfterLoad!
TestHeader initializeAfterLoad!
TestHeaderProportional initializeAfterLoad!
KRONO_Reporter_Test initializeAfterLoad!

KRONO_Reporter_Test loaded!
