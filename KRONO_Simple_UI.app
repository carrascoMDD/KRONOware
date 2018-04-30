'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:38:03 pm'!



((KRONO_Simple createSubApplication: #KRONO_Simple_UI in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

KRONO_Simple_UI becomeDefault!

CMDefinedUIPersistencyHolder subclass: #KRSimpleDefinedUIHolder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Simple_UI becomeDefault!

SubApplication subclass: #KRONO_Simple_UI
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Simple_UI becomeDefault!

!KRSimpleDefinedUIHolder class publicMethodsFor: 'cmDefinedParts persistence'!

exampleDefinedWindowStore

	"(CMDefinedPart newFromPersistenceAsCode: KRSimpleDefinedUIHolder exampleDefinedWindowStore) browse"

	self ojoCMDefinedParts.

	^   #( window 'Ejemplo CMDefinedPanelAppModel'
	nil
	nil
	nil
	nil
	nil
'Ejemplo CMDefinedPanleAppModel'
	exampleCMDefinedPanelAppModelDefinedWindowTitleNlsSymbol exampleCMDefinedPanelAppModelDefinedWindowTitleNlsGroup
	32 32 150 100 640 480 1400 1000
	KRSimpleDefinedUIHolder exampleDefinedWindowStore
	allAllowed
	nil
	'Clinica'
	KRSimpleTranslationHolder     kronoSimpleUITranslationStore
	( refToModel 'KronoSimple' kronoSimpleStore KRSimpleMetaInfoHolder )

	  (allowedRootTypes
		 ( refToType 'Clinica' 'Nucleo'  ) 
	   )
	  nil
	(parts
	  ( sep #sep
		nil
		nil
		nil
		nil
		nil
		cooperative sep nil
		( layoutRectangle 0.1 (0 12 ) (0 30 ) ( 0 1 ) true )
nil
	   )

	  ( title 'Title 1 ExampleCMDefinedPanelAppModelDefinedWindowName'
		nil
		nil
		true
		nil
		true
		cooperative title1LayoutSymbol exampleCMDefinedPanelAppModelDefinedPartTitleLayoutGroup
		( layoutRectangle 0.1 (0 30 ) (0 50 ) ( 0.05 0.3 ) false )
nil
		cif exampleCMDefinedPanelAppModelDefinedPartTitleNlsGroup
		'C.I.F.'
		yourself left
		0
	   )

	  ( field 'Field 1 ExampleCMDefinedPanelAppModelDefinedWindowName'
		nil
		nil
		true
		nil
		true
		cooperative field1LayoutSymbol exampleCMDefinedPanelAppModelDefinedPartFieldLayoutGroup
		( layoutRectangle 0.1 (0 30 ) (0 50 ) ( 0.4 0.55 ) true )
nil
		nil nil
		nil nil
		yourself left
		0
		nil
		 ( refToAttribute 'cif'  ( refToType 'PersonaJuridica' 'TiposGenerales'  )  ) 

	   )

	  ( sep #sep
		nil
		nil
		nil
		nil
		nil
		cooperative sep nil
		( layoutRectangle 0.1 (0 12 ) (0 30 ) ( 0 1 ) true )
nil
	   )

	  ( title 'Title 2 ExampleCMDefinedPanelAppModelDefinedWindowName'
		nil
		nil
		true
		nil
		true
		cooperative title2LayoutSymbol exampleCMDefinedPanelAppModelDefinedPartTitleLayoutGroup
		( layoutRectangle 0.1 (0 30 ) (0 50 ) ( 0.05 0.3 ) false )
nil
		razonSocial exampleCMDefinedPanelAppModelDefinedPartTitleNlsGroup
		'Razon Social'
		yourself left
		0
	   )

	  ( field 'Field 2 ExampleCMDefinedPanelAppModelDefinedWindowName'
		nil
		nil
		true
		nil
		true
		cooperative field2LayoutSymbol exampleCMDefinedPanelAppModelDefinedPartFieldLayoutGroup
		( layoutRectangle 0.1 (0 30 ) (0 50 ) ( 0.4 0.55 ) true )
nil
		nil nil
		nil nil
		yourself left
		0
		nil
		 ( refToAttribute 'razonSocial'  ( refToType 'PersonaJuridica' 'TiposGenerales'  )  ) 

	   )

	  ( sep #sep
		nil
		nil
		nil
		nil
		nil
		cooperative sep nil
		( layoutRectangle 0.1 (0 12 ) (0 30 ) ( 0 1 ) true )
nil
	   )

	  ( title 'Title 3 ExampleCMDefinedPanelAppModelDefinedWindowName'
		nil
		nil
		true
		nil
		true
		cooperative title3LayoutSymbol exampleCMDefinedPanelAppModelDefinedPartTitleLayoutGroup
		( layoutRectangle 0.1 (0 30 ) (0 50 ) ( 0.05 0.3 ) false )
nil
		nombreComercial exampleCMDefinedPanelAppModelDefinedPartTitleNlsGroup
		'Nombre Comercial'
		yourself left
		0
	   )

	  ( field 'Field 3 ExampleCMDefinedPanelAppModelDefinedWindowName'
		nil
		nil
		true
		nil
		true
		cooperative field3LayoutSymbol exampleCMDefinedPanelAppModelDefinedPartFieldLayoutGroup
		( layoutRectangle 0.1 (0 30 ) (0 50 ) ( 0.4 0.55 ) true )
nil
		nil nil
		nil nil
		yourself left
		0
		nil
		 ( refToAttribute 'nombreComercial'  ( refToType 'PersonaJuridica' 'TiposGenerales'  )  ) 

	   )

	 )

   )! !

!KRSimpleDefinedUIHolder class publicMethodsFor: 'current'!

currentMessages
	"self  currentDefinedUI browse"
	"self  currentDefinedUIBoundToModel browse"
	"self  resetCurrentDefinedUIs"
	"(self  currentDefinedUIStoreMethodSelector: self defaultCurrentDefinedUISelector) browsePath"
	"self  resetCurrentDefinedUIStoreMethodSelector: self defaultCurrentDefinedUISelector"! !

!KRSimpleDefinedUIHolder class publicMethodsFor: 'default'!

defaultCurrentDefinedUISelector
	"self  defaultCurrentDefinedUISelector "

	^#exampleDefinedWindowStore! !

KRSimpleDefinedUIHolder initializeAfterLoad!
KRONO_Simple_UI initializeAfterLoad!

KRONO_Simple_UI loaded!
