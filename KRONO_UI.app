'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:38:03 pm'!



Application create: #KRONO_UI with: 
    (#( CODEgenUI CODE_META)
        collect: [:each | Smalltalk at: each ifAbsent: [
        self error: 'Not all of the prerequisites are loaded']])!

KRONO_UI becomeDefault!

CMDefinitionsHolder subclass: #KRUIDefinitionsHolder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_UI becomeDefault!

CODEMModelEditorsOpener subclass: #KRUIEditorsOpener
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_UI becomeDefault!

METAVirtualConfiguration subclass: #KRUIDeveloperConfiguration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_UI becomeDefault!

CODEMModelMETAConfiguration subclass: #KRUIMETAConfiguration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_UI becomeDefault!

CODEMModelConfigurationsCollection subclass: #KRUIConfigurationsCollection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_UI becomeDefault!

CODEMModelPathFinderGenericBrowser subclass: #KRUIPathFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_UI becomeDefault!

CODEMModelConfigurationsBrowser subclass: #KRUIConfigurationsBrowser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_UI becomeDefault!

CODEMModelGenericBrowser subclass: #KRUIApplicationBrowser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_UI becomeDefault!

Application subclass: #KRONO_UI
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_UI becomeDefault!

!KRONO_UI class publicMethodsFor: 'ENVY'!

loaded
	KRUIMETAConfiguration initialize.
	KRUIDeveloperConfiguration initialize.
	KRUIConfigurationsCollection initialize.
	KRUIConfigurationsBrowser initialize.
	KRUIEditorsOpener initialize.
	KRUIPathFinder initialize.! !

!KRUIApplicationBrowser class publicMethodsFor: 'class initialization'!

initialize
	"KRUIApplicationBrowser initialize.
	KRUIApplicationBrowser allSubclasses do: [:aClass | aClass initialize]"

	super initialize! !

!KRUIApplicationBrowser class publicMethodsFor: 'instance creation'!

browserKind
	^#KRUIApplicationBrowser!

checkedBrowserParameters:	theBrowserParameters
	| aDict |

	aDict := theBrowserParameters isNil ifTrue: [ Dictionary new] ifFalse: [ theBrowserParameters].

	self in: aDict at: METABrowser showCanvasLabelParameterSymbol 			ifAbsentPut: true.

	aDict at: METABrowser editorsOpenerParameterSymbol put: KRUIEditorsOpener editorsOpener.
	
	^super checkedBrowserParameters:	theBrowserParameters.! !

!KRUIApplicationBrowser class publicMethodsFor: 'interface specs'!

aboutSpec
	^KRONOLauncherPanel autorCanvasSpec!

technicalSupportSpec

	^KRONOLauncherPanel soporteCanvasSpec!

windowSpec

	"UIPainter new openOnClass: self andSelector: #windowSpec"

	^#(#FullSpec #window: #(#WindowSpec #label: 'Browser' #min:#(#Point 400 300 ) #bounds: #(#Rectangle 32 32 932 732 ) #flags: 4 #menu: #metaApplicationBrowserMenu ) #component: #(#SpecCollection #collection: #( ) ) )! !

!KRUIApplicationBrowser class publicMethodsFor: 'menu'!

assistantDebugMenu
	"self assistantDebugMenu startUp"

	^super assistantDebugMenu!

assistantRuntimeMenu
	"self assistantRuntimeMenu startUp"

	^super assistantDebugMenu!

helpMenu
	"self helpMenu startUp"

	^#(#HWHelpPopUpMenu #('A_yuda' '_Autor' '_Soporte') #(1 2 ) #( #help #about #techSupport) #('Obtener Ayuda acerca de KRONOware' 'Informacion acerca del Autor, Derechos y Limitaciones de Uso de KRONOware' 'Obtener soporte acerda de KRONOware') ) decodeAsLiteralArray!

metaApplicationBrowserDebugMenu
	"self metaApplicationBrowserDebugMenu startUp"

	| aMenu aToolsMenu |

	aToolsMenu := self toolsMenu.
	aToolsMenu isNil
		ifTrue: [
			aMenu := HWHelpPopUpMenu 
				labelArray: #('_Navegador'  'Formato' '_Grabadora' '_Ventanas' '_Ayuda' 'Acceso Desarrollador') copy
				lines: #()
				values: (OrderedCollection new 
					add: self browserMenu;
					add: self viewsMenu;
					add: self assistantMenu;
					add: self windowsMenu;
					add: self helpMenu;
					add: self itselfMenu;
					yourself)
				helps: #(''  '' '' '' '' '' '')]
		ifFalse: [
			aMenu := HWHelpPopUpMenu 
				labelArray: #('_Navegador' 'Formato' '_Grabadora' '_Ventanas' 'Herramientas' '_Ayuda' 'Acceso Desarrollador') copy
				lines: #()
				values: (OrderedCollection new 
					add: self browserMenu;
					add: self viewsMenu;
					add: self assistantMenu;
					add: self windowsMenu;
					add: aToolsMenu;
					add: self helpMenu;
					add: self itselfMenu;
					yourself)
				helps: #(''  '' '' '' '' ''  '')].


	^aMenu!

metaApplicationBrowserMenu
	^DEBUGDvpt
		ifTrue: [ self metaApplicationBrowserDebugMenu]
		ifFalse: [ self metaApplicationBrowserRuntimeMenu]!

metaApplicationBrowserRuntimeMenu
	"self metaApplicationBrowserRuntimeMenu startUp"

	| aMenu aToolsMenu |

	aToolsMenu := self toolsMenu.
	aToolsMenu isNil
		ifTrue: [
			aMenu := HWHelpPopUpMenu 
				labelArray: #('_Navegador' 'Formato' '_Grabadora' '_Ventanas' '_Ayuda') copy
				lines: #()
				values: (OrderedCollection new 
					add:  self browserMenu;
					add: self viewsMenu;
					add: self assistantMenu;
					add: self windowsMenu;
					add: self helpMenu;
					yourself)
				helps: #(''  ''  '' '' '')]
		ifFalse: [
			aMenu := HWHelpPopUpMenu 
				labelArray: #('_Navegador' 'Formato' '_Grabadora' '_Ventanas' 'Herramientas' '_Ayuda') copy
				lines: #()
				values: (OrderedCollection new 
					add:  self browserMenu;
					add: self viewsMenu;
					add: self assistantMenu;
					add: self windowsMenu;
					add: aToolsMenu;
					add: self helpMenu;
					yourself)
				helps: #(''  '' '' '' '' ''  )].

	^aMenu!

pathMenu
	"self pathMenu startUp"

	^#(#HWHelpPopUpMenu #('_Desde el principio' '_Volver Atras' '_Abrir ventana de camino' '_Cerrar  ventana de camino' ) #(2 ) #(#goToFirstInHistory #backOneInHistory #showPathWindow #hidePathWindow ) #('Mostrar el primer objeto en el camino' 'Mostrar objeto anterior en el camino' 'Abrir ventana de camino' 'Cerrar ventana de camino' ))  decodeAsLiteralArray!

viewsMenu
	"self viewsMenu startUp"

	^#(#HWHelpPopUpMenu #('_Solo Navegador' 'Solo Editor' '_Navegador y Editor' '-- no usada --'  '_Formato Automatico' ) #(3 ) #(#onlyDisplayLists #onlyDisplayEditor #displayListAndEditor #showInheritedMode #autoLayoutMode) #( 'Mostrar solamente el panel de navegacion de caminos' 'Mostrar solamente el panel de edicion de datos' 'Mostrar los dos paneles : navegacio y editor' '-- no usado --' 'Modo Formato Automatico' ) )  decodeAsLiteralArray!

windowsMenu
	"self windowsMenu startUp"

	^#(#HWHelpPopUpMenu #('_Refrescar' '_Mover' '_Dimensiones' '_Delante' '_Detras' '_Colapsar' 'Lista de Ventanas') #(6) #( #refreshWindow #moveWindow #resizeWindow #frontWindow #backWindow #collapseWindow  #windowList ) #('Refrescar la visualizacion completa de la pantalla (no vuelve a acceder a los datos)' 'Mover la Pantalla' 'Cambiar las dimensiones de la pantalla' 'Traer la ventana delante de las demas' 'Enviar la ventana detras de todas las demas' 'Colapsar la ventana. Puede recuperarla desde su barra de herramientas windows'  'Mostrar una lista de todas las ventanas de KRONOware') )  decodeAsLiteralArray!

windowsRaiseHelpString
	^'Mostrar ventana ' copy! !

!KRUIApplicationBrowser class publicMethodsFor: 'menu indexed accessing'!

autoLayoutModeMenuItemHelp
	^'Cambia el modo de formato a automatico : el navegador ocupara toda la pantalla en ausencia de seleccion' copy!

autoLayoutModeMenuItemLabel
	^'Formato Automatico' copy!

fixedLayoutModeMenuItemHelp
	^'Cambia el modo de formato a fijo : el navegador mantiene sus dimensiones haya o no seleccion' copy!

fixedLayoutModeMenuItemLabel
	^'Formato Fijo' copy!

hideInheritedMenuItemHelp
	^'-- no usada --' copy!

hideInheritedMenuItemLabel
	^'-- no usada --' copy!

showInheritedMenuItemHelp
	^'-- no usada --' copy!

showInheritedMenuItemLabel
	^'-- no usada --' copy! !

!KRUIApplicationBrowser class publicMethodsFor: 'ref:preferences'!

preferredConfigurationsBrowserClass
	^KRUIConfigurationsBrowser!

preferredEditorsOpenerClass
	^KRUIEditorsOpener!

preferredMETAConfigurationClass

	^KRUIMETAConfiguration! !

!KRUIApplicationBrowser publicMethodsFor: 'label'!

browserLabelTitlePrefix
	^'KRONOware: ' copy! !

!KRUIApplicationBrowser publicMethodsFor: 'menu'!

about
	KRONOLauncherPanel openWithSpec: #autorWindowSpec!

help

	Dialog warn: 
		'Por favor, contacte Soporte tecnico\para realizar cualquier consulta\y recibir un manual de instrucciones'
		withCRs.
	self techSupport!

techSupport
	KRONOLauncherPanel openWithSpec: #soporteWindowSpec! !

!KRUIApplicationBrowser publicMethodsFor: 'updating'!

updateWindowLabel
	| aLabel aBrowserParameters aPrefix aPrefixString |
	aLabel :=  (self readOnly 
		ifTrue: ['Read Only ', self browserLabelTitlePrefix] 
		ifFalse: [self browserLabelTitlePrefix]),
		objectHolder classAndName.


	aBrowserParameters := objectHolder browserParameters.

	aPrefix := aBrowserParameters isNil
		ifTrue: ['']
		ifFalse: [ 
			aPrefixString := aBrowserParameters at: METABrowser windowLabelPrefixSymbol ifAbsent: [nil].
			aPrefixString isNil
				ifTrue: ['']
				ifFalse: [aPrefixString, ' ']].


	self builder window label: aPrefix ,  aLabel.
	self isDialog ifTrue: [ self dialogLabel value: 'Selection ',aPrefix , aLabel].!

updateWindowsList

	self updateWindowsListMenu!

updateWindowsListMenu

	| aMenu aWindowsListMenu |

	aMenu := self metaApplicationBrowserMenu.
	aMenu isNil ifTrue: [ ^self].

	aWindowsListMenu := aMenu valueAt: self class windowsSubMenuIndexInMenu.
	aWindowsListMenu buildMenu.! !

!KRUIConfigurationsBrowser class publicMethodsFor: 'class initialization'!

initialize
	"KRUIConfigurationsBrowser initialize.
	KRUIConfigurationsBrowser allSubclasses do: [:aClass | aClass initialize]"

	super initialize! !

!KRUIConfigurationsBrowser class publicMethodsFor: 'interface specs'!

windowSpec


	"UIPainter new openOnClass: self andSelector: #windowSpec"

	^#(#FullSpec #window: #(#WindowSpec #label: 'Browser' #min: #(#Point 400 300 ) #bounds: #(#Rectangle 32 32 740 680) #flags: 4 #menu: #metaApplicationBrowserMenu ) #component: #(#SpecCollection #collection: #( ) ) )! !

!KRUIConfigurationsBrowser class publicMethodsFor: 'ref:class accessing'!

browserKind
	^#KRUIConfigurationsBrowser! !

!KRUIConfigurationsBrowser class publicMethodsFor: 'ref:preferences'!

preferredConfigurationsCollectionClass
	^KRUIConfigurationsCollection!

preferredEditorsOpenerClass
	^KRUIEditorsOpener! !

!KRUIConfigurationsBrowser publicMethodsFor: 'updating'!

updateWindowsList! !

!KRUIConfigurationsCollection class publicMethodsFor: 'class initialization'!

initialize
	"KRUIConfigurationsCollection initialize"

	super initialize! !

!KRUIConfigurationsCollection class publicMethodsFor: 'ref:configurations'!

allCurrentConfigurations
	^Array 
		with: KRUIMETAConfiguration current
		with: KRUIDeveloperConfiguration current! !

!KRUIConfigurationsCollection publicMethodsFor: 'ref:accessing'!

name
	^'Todos los valores configurables' copy! !

!KRUIDefinitionsHolder class publicMethodsFor: 'configuration'!

allowModificationOfNonModifiableInitializedInCreationTerminalAttributesParmValue
	| aValue aConfig |

	aConfig := self developerConfiguration.
	aValue := aConfig isNil ifTrue: [ nil] ifFalse: [ aConfig getParameter: aConfig class allowModificationOfNonModifiableInitializedInCreationTerminalAttributesParameterName].
	aValue isNil ifFalse: [^aValue].
	^false!

browseIsAggregateRelationshipsConfigParmValue
	| aValue aConfig |

	aConfig := self developerConfiguration.
	aValue := aConfig isNil ifTrue: [ nil] ifFalse: [ aConfig getParameter: aConfig class browseIsAggregateRelationshipsParameterName].
	aValue isNil ifFalse: [^aValue].
	^false!

browseMetaInfoConfigParmValue
	| aValue aConfig |

	aConfig := self developerConfiguration.
	aValue := aConfig isNil ifTrue: [ nil] ifFalse: [ aConfig getParameter: aConfig class browseMetaInfoParameterName].
	aValue isNil ifFalse: [^aValue].
	^false!

browseRequiredFeaturesParentAttributeConfigParmValue
	| aValue aConfig |

	aConfig := self developerConfiguration.
	aValue := aConfig isNil ifTrue: [ nil] ifFalse: [ aConfig getParameter: aConfig class browseRequiredFeaturesParentAttributeParameterName].
	aValue isNil ifFalse: [^aValue].
	^false!

browseRequiredReferencedFeatureCandidatesConfigParmValue
	| aValue aConfig |

	aConfig := self developerConfiguration.
	aValue := aConfig isNil ifTrue: [ nil] ifFalse: [ aConfig getParameter: aConfig class browseRequiredReferencedFeatureCandidatesParameterName].
	aValue isNil ifFalse: [^aValue].
	^false!

browseSeparatePerspectivesForReferencedRequiredFeaturesConfigParmValue
	| aValue aConfig |

	aConfig := self developerConfiguration.
	aValue := aConfig isNil ifTrue: [ nil] ifFalse: [ aConfig getParameter: aConfig class browseSeparatePerspectivesForReferencedRequiredFeaturesParameterName].
	aValue isNil ifFalse: [^aValue].
	^false!

browseToDomainConfigParmValue
	| aValue aConfig |

	aConfig := self developerConfiguration.
	aValue := aConfig isNil ifTrue: [ nil] ifFalse: [ aConfig getParameter: aConfig class browseToDomainParameterName].
	aValue isNil ifFalse: [^aValue].
	^false!

developerConfiguration
	^KRUIDeveloperConfiguration current!

maxNumberOfSpecsInEachCreationDialogPerspectiveParmValue
	| aValue aConfig |

	aConfig := self developerConfiguration.
	aValue := aConfig isNil ifTrue: [ nil] ifFalse: [ aConfig getParameter: aConfig class maxNumberOfSpecsInEachCreationDialogPerspectiveParameterName].
	aValue isNil ifFalse: [^aValue].
	^10!

maxNumberOfSpecsInEachPerspectiveParmValue
	| aValue aConfig |

	aConfig := self developerConfiguration.
	aValue := aConfig isNil ifTrue: [ nil] ifFalse: [ aConfig getParameter: aConfig class maxNumberOfSpecsInEachPerspectiveParameterName].
	aValue isNil ifFalse: [^aValue].
	^10! !

!KRUIDefinitionsHolder class publicMethodsFor: 'preferences-refin'!

preferredApplicationBrowserClass
	^KRUIApplicationBrowser!

preferredPathFinderApplicationBrowserClass
	^KRUIPathFinder! !

!KRUIDefinitionsHolder class publicMethodsFor: 'specs building'!

buildMetaInfoSpec

	self browseMetaInfoConfigParmValue ifTrue: [ ^super buildMetaInfoSpec].
	
	^nil!

buildSpecFromAttribute: theAttribute

	theAttribute isNil ifTrue: [ ^nil].

	theAttribute name = CODEElement objectDomainCMGOAttributeName ifTrue: [ 
		 ^self browseToDomainConfigParmValue 
			ifFalse: [ nil]
			ifTrue: [ super buildSpecFromAttribute: theAttribute]
	].

	theAttribute name = CODEElement requiredFeaturesParentAttributeName ifTrue: [ 
		 ^self browseRequiredFeaturesParentAttributeConfigParmValue 
			ifFalse: [ nil]
			ifTrue: [ super buildSpecFromAttribute: theAttribute]
	].

	(theAttribute name endsWith: CODEElement candidatesFeatureNameForRequiredReferencedFeaturePostfix) ifTrue: [ 
		 ^self browseRequiredReferencedFeatureCandidatesConfigParmValue 
			ifFalse: [ nil]
			ifTrue: [ super buildSpecFromAttribute: theAttribute]
	].

	^super buildSpecFromAttribute: theAttribute!

buildSpecFromRelationship: theRelationship

	theRelationship isNil ifTrue: [ ^nil].

	self browseIsAggregateRelationshipsConfigParmValue ifTrue: [ 
		^super buildSpecFromRelationship: theRelationship
	].

	theRelationship isAggregated ifTrue: [ ^nil].

	^super buildSpecFromRelationship: theRelationship!

canChangeFeature: theFeature

	theFeature isNil ifTrue: [ ^false].

	^theFeature isChangeable or: [ 
		(theFeature referencedType nonVirtualType isPrimitive or: [ 
			theFeature referencedType nonVirtualType isEnumeration])  and: [ 
			theFeature computationKind = theFeature class computationKindInitializedInConstructor and: [ 
				self allowModificationOfNonModifiableInitializedInCreationTerminalAttributesParmValue
			]
		]
	]!

classifySpecs: theAllSpecs intoTerminals: theTerminalEditorSpecs separate: theSeparatePerspectiveSpecs

	theAllSpecs do: [ :aSpec |  
		aSpec isTerminalChildSpec ifTrue: [ 
			aSpec type = #Text ifTrue: [ theSeparatePerspectiveSpecs add: aSpec].
			 theTerminalEditorSpecs add: aSpec
		].
		aSpec isClassChildSpec ifTrue: [ 
			aSpec showInEditor ifTrue: [ theTerminalEditorSpecs add: aSpec].
			(aSpec isCMDriven not or: [ aSpec metaInfo type name endsWith:  CODEElement requiredFeaturesTypePostfix]) 
				ifFalse: [ theSeparatePerspectiveSpecs add: aSpec]
				ifTrue: [ 
					self browseSeparatePerspectivesForReferencedRequiredFeaturesConfigParmValue ifTrue: [  
						theSeparatePerspectiveSpecs add: aSpec
					]
				]
		].
		aSpec isCollectionChildSpec ifTrue: [ 
			theSeparatePerspectiveSpecs add: aSpec
		].
		aSpec isOperationChildSpec ifTrue: [ 
			 theTerminalEditorSpecs add: aSpec
		].
	].! !

!KRUIDeveloperConfiguration class publicMethodsFor: 'class initialization'!

initialize
	"KRUIDeveloperConfiguration initialize"

	super initialize! !

!KRUIDeveloperConfiguration class publicMethodsFor: 'ref:configuration'!

configurationDescription
 
	^('Configuracion.\', 
		'El Desarrollador utiliza parametros en esta configuracion para controlar el comportamiento de KRONOware') copy withCRs!

configurationName
	^'Parametros de Configuracion del Desarrollador' copy!

configurationParameters
 
	^(OrderedCollection new: 32)
		add: self browseIsAggregateRelationshipsParameter;
		add: self browseToDomainParameter;
		add: self browseMetaInfoParameter;
		add: self browseRequiredFeaturesParentAttributeParameter;
		add: self browseRequiredReferencedFeatureCandidatesParameter;
		add: self browseSeparatePerspectivesForReferencedRequiredFeaturesParameter;
		add: self discardPerspectivesInCreationDialogsParameter;
		add: self alwaysShowSeparateTextPerspectivesInCreationDialogsParameter;
		add: self createSomeInfoInNewProjectDomainParameter;
		add: self expandRanliteralInitializationExpressionsParameter;
		add: self allowModificationOfNonModifiableInitializedInCreationTerminalAttributesParameter;
		add: self maxNumberOfSpecsInEachCreationDialogPerspectiveParameter;
		add: self maxNumberOfSpecsInEachPerspectiveParameter;
		add: self softResetHealthCodesParameter;
		yourself! !

!KRUIDeveloperConfiguration class publicMethodsFor: 'ref:parameters'!

allowModificationOfNonModifiableInitializedInCreationTerminalAttributesParameter
	^self preferredParameterBooleanClass
			name: self allowModificationOfNonModifiableInitializedInCreationTerminalAttributesParameterName
			label: 'Permitir la Modificacion de Datos obligatoriamente introducidos en las pantallas de creacion.'
			value: self allowModificationOfNonModifiableInitializedInCreationTerminalAttributesParameterValue
			isEditable: true
			description: (
				'Cuando True, permite modificar los datos obligatoriamente introducidos en la creacion de cada objeto.\',
				'y que normalmente deberian quedar protegidos de modificacion despues de la creacion\',
				'	Es en el momento de crear la informacion, cuando el Usuario debe poner el mayor cuidado en introducir la informacion correcta.\',
				'	Es entonces cuando la fuente y motivo de la informacion esta presente y/o reciente, y es posible asegurar la mayor fidelidad.\',
				'    Dependiendo del grado de seguridad en la Clinica, y de proteccion de acceso al sistema por no Usuarios\',
				'	asi como el gradoresponsabilidad y confianza depositado en los usuarios,\',
				'	esta opcion permite controlar si estan permitidas modificaciones a posteriori, sin proceso de autorizacion y verificacion.\',
				'Cuando False, no es posible permite modificar los datos obligatoriamente introducidos en la creacion de cada objeto.',
				'    Esta opcion es mas segura, pues impide la modificacion a posteriori de datos producidos.\',
				'	Puede ser necesario activar esta opcion, en casos seleccionados, donde la modificacion de datos a posteriori\',
				'	esta justificada, autorizada, o adecuadamente supervisada')
				withCRs
			defaultValue: self allowModificationOfNonModifiableInitializedInCreationTerminalAttributesParameterValue
			verificationBlock: nil
			derivationBlock: nil!

alwaysShowSeparateTextPerspectivesInCreationDialogsParameter
	^self preferredParameterBooleanClass
			name: self alwaysShowSeparateTextPerspectivesInCreationDialogsParameterName
			label: 'Siempre mostrar Perspectiva separada para Textos en Pantallas de Creacion de Datos'
			value: self alwaysShowSeparateTextPerspectivesInCreationDialogsParameterValue
			isEditable: true
			description: (
				'Cuando True, las pantallas de creacion de datos mostraran un menu de perspectivas,\',
				'incluyendo perspectivas para todos los textos largos a introducir en la creacion.\',
				'	El Usuario dispone asi, en cada perspectiva separada para texto, del panel completo de la pantalla de creacion\',
				'	para introducir el texto desado\',
				'	Los navegadores resultan mas complejos por la perspectiva adicional de cada texto,\',
				'Cuando False, todos los datos de creacion aparecen en un mismo panel\',
				'reduciendo el espacio en pantalla disponible para que el usuario introduzca el texto.',
				'    Esta opcion es opcional, y para comodidad de entrada de textos.\',
				'	Los textos pueden tener cualquier longitud, independientemente del espacio disponible en pantalla para su introduccion')
				withCRs
			defaultValue: self alwaysShowSeparateTextPerspectivesInCreationDialogsParameterValue
			verificationBlock: nil
			derivationBlock: nil!

browseIsAggregateRelationshipsParameter
	^self preferredParameterBooleanClass
			name: self browseIsAggregateRelationshipsParameterName
			label: 'Navegar de contenidos a continente'
			value: self browseIsAggregateRelationshipsParameterValue
			isEditable: true
			description: (
				'Cuando True, permite navegar desde objetos contenidos a su objeto continente.\',
				'     Los navegadores resultan mas complejos por el aumentado numero de perspectivas navegables\',
				'     y la facilidad con que el usuario puede navegar ciclicamente sobre los mismos objetos.\',
				'Cuando False, no es posible navegar desde objetos contenidos a su continente.',
				'     Esta navegacion no es usualmente necesaria, pues el usuario suele estar al tanto del continente, pues acaba de navegar por el.\')
				withCRs
			defaultValue: self browseIsAggregateRelationshipsParameterValue
			verificationBlock: nil
			derivationBlock: nil!

browseMetaInfoParameter
	^self preferredParameterBooleanClass
			name: self browseMetaInfoParameterName
			label: 'Navegar la MetaInformacion de cada Objeto'
			value: self browseMetaInfoParameterValue
			isEditable: true
			description: (
				'Cuando True, permite navegar desde cada Objeto a su MetaInformacion.\',
				'     Los navegadores resultan mas complejos por la perspectiva adicional "MetaInfo",\',
				'     y por que el usuario se ve expuesto a la semantica subyacente del modelo de la aplicacion\',
				'Cuando False, no es posible navegar desde los objetos a su MetaInfo.',
				'     Esta navegacion no es necesaria, ya que el usuario se concentra en manejar informacion - no metainformacion.\')
				withCRs
			defaultValue: self browseMetaInfoParameterValue
			verificationBlock: nil
			derivationBlock: nil!

browseRequiredFeaturesParentAttributeParameter
	^self preferredParameterBooleanClass
			name: self browseRequiredFeaturesParentAttributeParameterName
			label: 'Navegar la RequiredFeaturesParentAttribute information en la creacion cada Objeto'
			value: self browseRequiredFeaturesParentAttributeParameterValue
			isEditable: true
			description: (
				'Cuando True, permite visualizar el RequiredFeaturesParentAttribute en los Dialogos de creacion de objetos.\',
				'     Los navegadores resultan mas complejos por el campo y perspectiva adicional llamada RequiredFeaturesParentAttribute,\',
				'Cuando False, no es posible permite visualizar el RequiredFeaturesParentAttribute en los Dialogos de creacion de objetos.',
				'     Esta visualizacion no es necesaria, ya que el objeto visualizado es temporal y de uso interno solamente durante la edicion de los parametros de creacion del nuevo objeto.\')
				withCRs
			defaultValue: self browseRequiredFeaturesParentAttributeParameterValue
			verificationBlock: nil
			derivationBlock: nil!

browseRequiredReferencedFeatureCandidatesParameter
	^self preferredParameterBooleanClass
			name: self browseRequiredReferencedFeatureCandidatesParameterName
			label: 'Navegar la RequiredReferencedFeatureCandidates informacion de cada Objeto'
			value: self browseRequiredReferencedFeatureCandidatesParameterValue
			isEditable: true
			description: (
				'Cuando True, permite visualizar los candidatos para nuevas referencias en los Dialogos de creacion de objetos.\',
				'     Los navegadores resultan mas complejos por el campo y perspectiva adicional de los candidatos de cada referencia",\',
				'Cuando False, no es posible visualizar los candidatos para nuevas referencias en los Dialogos de creacion de objetos.',
				'     Esta visualizacion no es necesaria, ya que los candidatos seran mostrados al Usuario cuando solicite la creacion de una referencia.\')
				withCRs
			defaultValue: self browseRequiredReferencedFeatureCandidatesParameterValue
			verificationBlock: nil
			derivationBlock: nil!

browseSeparatePerspectivesForReferencedRequiredFeaturesParameter
	^self preferredParameterBooleanClass
			name: self browseSeparatePerspectivesForReferencedRequiredFeaturesParameterName
			label: 'Navegar en perspectiva separada cada ReferencedRequiredFeatures para creacion de cada Objeto'
			value: self browseSeparatePerspectivesForReferencedRequiredFeaturesParameterValue
			isEditable: true
			description: (
				'Cuando True, permite visualizar en perspectiva separada las nuevas referencias en los Dialogos de creacion de objetos.\',
				'     Los navegadores resultan mas complejos por la perspectiva adicional de cada referencia.\',
				'Cuando False, no permite visualizar en perspectiva separada las nuevas referencias en los Dialogos de creacion de objetos.',
				'     Esta visualizacion no es necesaria, ya que el objeto seleccionado para nueva referencia muestra su nombre en el campo de editor.\')
				withCRs
			defaultValue: self browseSeparatePerspectivesForReferencedRequiredFeaturesParameterValue
			verificationBlock: nil
			derivationBlock: nil!

browseToDomainParameter
	^self preferredParameterBooleanClass
			name: self browseToDomainParameterName
			label: 'Navegar al Dominio global de la Clinica'
			value: self browseToDomainParameterValue
			isEditable: true
			description: (
				'Cuando True, permite navegar desde objetos raiz al Dominio de KRONOware para la Clinica bajo inspeccion.\',
				'     Los navegadores resultan mas complejos por la perspectiva adicional "Dominio",\',
				'     por que el usuario tiene acceso a los objetos Origen de todos los tipos raiz del sistema\',
				'     y la facilidad con que el usuario puede navegar ciclicamente de los objetos al Dominio, a los Origenes y otros objetos.\',
				'Cuando False, no es posible navegar desde los objetos al Dominio de KRONOware para la Clinica bajo inspeccion.',
				'     Esta navegacion no es necesaria, pues el Usuario accede a partes mas especificas de la aplicacion.\')
				withCRs
			defaultValue: self browseToDomainParameterValue
			verificationBlock: nil
			derivationBlock: nil!

createSomeInfoInNewProjectDomainParameter
	^self preferredParameterBooleanClass
			name: self createSomeInfoInNewProjectDomainParameterName
			label: 'Crear infomaciones de ejemplo cuando se cree una nueva Clinica'
			value: self createSomeInfoInNewProjectDomainParameterValue
			isEditable: true
			description: (
				'Cuando True, al crear una nueva Clinica, la nueva Clinica sera rellenada con informaciones de ejemplo y test.\',
				'     Usuarios potenciales o noveles, pueden observar esta informacion de ejemplo, como guia a los contenidos soportados por KRONOware,\',
				'     Esta informacion de ejemplo no es apropiada para el negocio real de la Clinica.\',
				'Cuando False, la nueva Clinica se creara sin informaciones de ejemplo.\',
				'	La nueva Clinica carecera de la informacion de ejemplo no apropiada para negocio real de la Clinica.\',
				'	El Usuario debera introducir la informacion real correspondiente a su negocio.')
				withCRs
			defaultValue: self createSomeInfoInNewProjectDomainParameterValue
			verificationBlock: nil
			derivationBlock: nil!

discardPerspectivesInCreationDialogsParameter
	^self preferredParameterBooleanClass
			name: self discardPerspectivesInCreationDialogsParameterName
			label: 'Descartar el Menu de Perspectivas en Pantallas de Creacion de Datos'
			value: self discardPerspectivesInCreationDialogsParameterValue
			isEditable: true
			description: (
				'Cuando True, las pantallas de creacion de datos mostraran un menu de perspectivas,\',
				'que permiten al usuario acceder en un panel separado a los textos y datos referenciados (si los hay) en la creacion del Dato.\',
				'     Los navegadores resultan mas complejos por la perspectiva adicional de cada texto,\',
				'Cuando False, todos los datos de creacion aparecen en un mismo panel\',
				'reduciendo el espacio en pantalla disponible para que el usuario introduzca el texto,\',
				'y sin la disponibilidad de panel separado para observar la informacion completa de datos referenciados\',
				'    Esta opcion es opcional, y para comodidad de entrada de textos.\',
				'	Los textos pueden tener cualquier longitud, independientemente del espacio disponible en pantalla para su introduccion')
				withCRs
			defaultValue: self discardPerspectivesInCreationDialogsParameterValue
			verificationBlock: nil
			derivationBlock: nil!

expandRanliteralInitializationExpressionsParameter
	^self preferredParameterBooleanClass
			name: self expandRanliteralInitializationExpressionsParameterName
			label: 'Rellenar datos con un numero aleatorio y texto de ejemplo'
			value: self expandRanliteralInitializationExpressionsParameterValue
			isEditable: true
			description: (
				'Cuando True, al crear nuevos datos, las informaciones requeridas seran rellenadas de antemano con una informacion arbitraria.\',
				'	La informacion se compone de un numero aleatorio de tres cifras, y un texto de ejemplo.\',
				'	Esta opcion permite la creacion rapida de informaciones, durante procesos de validacion o demostracion de KRONOware.\',
				'	Esta informacion de ejemplo no es apropiada para el negocio real de la Clinica.\',
				'	El Usuario puede seleccionar y borrar el numero y texto de ejemplo, e introducir la informacion deseada.\',
				'Cuando False, al crear nuevos datos, las informaciones requeridas apareceran en blanco,\',
				'listos para que el Usuario introduzca los datos apropiados.\',
				'	En general, los datos arbitrarios no son utiles para el Usuario en el funcionamiento normal de la aplicacion.\',
				'	Ademas, el Usuario se vera importunado por la tarea adicional, de eliminar los datos arbitrarios antes de introdcir los apropiados.')
				withCRs
			defaultValue: self expandRanliteralInitializationExpressionsParameterValue
			verificationBlock: nil
			derivationBlock: nil!

maxNumberOfSpecsInEachCreationDialogPerspectiveParameter
 
	^self preferredParameterNumberBoundedClass
			name: self maxNumberOfSpecsInEachCreationDialogPerspectiveParameterName
			label: 'Numero Maximo de Datos en cada panel de Perspectiva de ventana de Creacion'
			value: self maxNumberOfSpecsInEachCreationDialogPerspectiveParameterValue
			isEditable: true
			description:	'El numero maximo de datos mostrados en el panel de cada Perspectiva en ventanas de creacion'
			defaultValue: self maxNumberOfSpecsInEachCreationDialogPerspectiveParameterValue
			verificationBlock: nil
			derivationBlock: nil
			species: #SmallInteger
			lowerBound: 1
			upperBound: 100!

maxNumberOfSpecsInEachPerspectiveParameter
 
	^self preferredParameterNumberBoundedClass
			name: self maxNumberOfSpecsInEachPerspectiveParameterName
			label: 'Numero Maximo de Datos en cada panel de Perspectiva'
			value: self maxNumberOfSpecsInEachPerspectiveParameterValue
			isEditable: true
			description:	'El numero maximo de datos mostrados en el panel de cada Perspectiva'
			defaultValue: self maxNumberOfSpecsInEachPerspectiveParameterValue
			verificationBlock: nil
			derivationBlock: nil
			species: #SmallInteger
			lowerBound: 1
			upperBound: 100!

softResetHealthCodesParameter
	^self preferredParameterBooleanClass
			name: self softResetHealthCodesParameterName
			label: 'Reinicializar las codificaciones de Enfermedades y Procedimientos con el minimo coste'
			value: self softResetHealthCodesParameterValue
			isEditable: true
			description: (
				'Cuando True, la reinicializacion de codificaciones de Enfermedades y Procedimientos se realizara con minimo coste de tiempo.\',
				'Cuando False, la reinicializacion de codificaciones de Enfermedades y Procedimientos se realizara partiendo desde zero.\')
				withCRs
			defaultValue: self softResetHealthCodesParameterValue
			verificationBlock: nil
			derivationBlock: nil! !

!KRUIDeveloperConfiguration class publicMethodsFor: 'ref:parametersvalues'!

allowModificationOfNonModifiableInitializedInCreationTerminalAttributesParameterName
	^#allowModificationOfNonModifiableInitializedInCreationTerminalAttributes!

allowModificationOfNonModifiableInitializedInCreationTerminalAttributesParameterValue
	^false!

alwaysShowSeparateTextPerspectivesInCreationDialogsParameterName
	^#alwaysShowSeparateTextPerspectivesInCreationDialogs!

alwaysShowSeparateTextPerspectivesInCreationDialogsParameterValue
	^true!

browseIsAggregateRelationshipsParameterName
	^#browseIsAggregateRelationships!

browseIsAggregateRelationshipsParameterValue
	^false!

browseMetaInfoParameterName
	^#browseMetaInfo!

browseMetaInfoParameterValue
	^false!

browseRequiredFeaturesParentAttributeParameterName
	^#browseRequiredFeaturesParentAttribute!

browseRequiredFeaturesParentAttributeParameterValue
	^false!

browseRequiredReferencedFeatureCandidatesParameterName
	^#browseRequiredReferencedFeatureCandidates!

browseRequiredReferencedFeatureCandidatesParameterValue
	^false!

browseSeparatePerspectivesForReferencedRequiredFeaturesParameterName
	^#browseSeparatePerspectivesForReferencedRequiredFeatures!

browseSeparatePerspectivesForReferencedRequiredFeaturesParameterValue
	^false!

browseToDomainParameterName
	^#browseToDomain!

browseToDomainParameterValue
	^false!

createSomeInfoInNewProjectDomainParameterName
	^#createSomeInfoInNewProjectDomain!

createSomeInfoInNewProjectDomainParameterValue
	^true!

discardPerspectivesInCreationDialogsParameterName
	^#discardPerspectivesInCreationDialogs!

discardPerspectivesInCreationDialogsParameterValue
	^true!

expandRanliteralInitializationExpressionsParameterName
	^#expandRanliteralInitializationExpressions!

expandRanliteralInitializationExpressionsParameterValue
	^true!

maxNumberOfSpecsInEachCreationDialogPerspectiveParameterName
	^#maxNumberOfSpecsInEachCreationDialogPerspective!

maxNumberOfSpecsInEachCreationDialogPerspectiveParameterValue
	^10!

maxNumberOfSpecsInEachPerspectiveParameterName
	^#maxNumberOfSpecsInEachPerspective!

maxNumberOfSpecsInEachPerspectiveParameterValue
	^10!

softResetHealthCodesParameterName
	^#softResetHealthCodes!

softResetHealthCodesParameterValue
	^false! !

!KRUIDeveloperConfiguration class publicMethodsFor: 'ref:preferences'!

preferredConfigurationsCollectionClass
	^KRUIConfigurationsCollection! !

!KRUIEditorsOpener class publicMethodsFor: 'class accessing'!

editorsOpenerName
	^#KRUIGlobalEditorsOpener! !

!KRUIEditorsOpener class publicMethodsFor: 'class initialization'!

initialize
	"KRUIEditorsOpener initialize.
	KRUIEditorsOpener allSubclasses do: [:aClass | aClass initialize]"

	self installGlobalEditorsOpener! !

!KRUIEditorsOpener publicMethodsFor: 'editors'!

closeAllEditorsBut: theObject

	| aResult |
	aResult := super closeAllEditorsBut: theObject.

	self updateAllEditorsWindowsList.
	
	^aResult!

closeAllEditorsOfKind: theKind object: theObject  parameter: theParameter

	| aResult |
	aResult := super closeAllEditorsOfKind: theKind object: theObject  parameter: theParameter.

	self updateAllEditorsWindowsList.
	
	^aResult!

inconditionallyOpenEditorOfKind: theKind object: theObject parameter: theParameter 
	openBlock: theBlock

	| aResult |
	aResult := super inconditionallyOpenEditorOfKind: theKind object: theObject parameter: theParameter 
		openBlock: theBlock.

	self updateAllEditorsWindowsList.
	
	^aResult!

informEditorsHaveBeenClosed

	Cursor normal showWhile: [ Dialog warn: 'Sus ventanas han sido cerradas.']!

releaseEditor: theEditor

	| aResult |
	aResult := super releaseEditor: theEditor.

	self updateAllEditorsWindowsList.
	
	^aResult!

updateAllEditorsWindowsList

	| anEditor |

	self editors do: [:someEditors |
		someEditors do: [:anOPYE |
			anEditor := anOPYE at: 3.
			anEditor isNil ifFalse: [ anEditor updateWindowsList]]].! !

!KRUIMETAConfiguration class publicMethodsFor: 'class initialization'!

initialize
	"KRUIMETAConfiguration initialize"

	super initialize! !

!KRUIMETAConfiguration class publicMethodsFor: 'ref:configuration'!

configurationDescription
 
	^('Configuracion de Navegadores.\', 
		'Utilize los parametros en esta configuracion para controlar el aspecto de los navegadores de KRONOware') copy withCRs!

configurationName
	^'Navegadores KRONOware' copy! !

!KRUIMETAConfiguration class publicMethodsFor: 'ref:parametersvalues'!

numberOfEditorHoldersParameterValue
	^3!

preferredConfigurationsCollectionClass
	^KRUIConfigurationsCollection! !

!KRUIPathFinder class publicMethodsFor: 'class initialization'!

initialize
	"KRUIPathFinder initialize.
	KRUIPathFinder allSubclasses do: [:aClass | aClass initialize]"


	super initialize! !

!KRUIPathFinder class publicMethodsFor: 'generated resources'!

littleCloseIconAndLabel

	^(LabelAndIcon with: (self nlsIconLabel: 'Cerrar') asText allBold) icon: self littleCloseIcon! !

!KRUIPathFinder class publicMethodsFor: 'instance creation'!

browserKind
	^#KRUIPathFinder!

checkedBrowserParameters:	theBrowserParameters
	| aDict |

	aDict := theBrowserParameters isNil ifTrue: [ Dictionary new] ifFalse: [ theBrowserParameters].

	self in: aDict at: METABrowser showCanvasLabelParameterSymbol 			ifAbsentPut: true.

	aDict at: METABrowser editorsOpenerParameterSymbol put: KRUIEditorsOpener editorsOpener.
	self in: aDict at: METABrowser numberOfEditorHoldersParameterSymbol 	ifAbsentPut: 3.

	aDict at: METABrowser showClassNodePrefixesInPathFinderSymbol put:
		(self getMETAConfigurationParameterValue: #showClassNodePrefixesInPathFinder default: false).
	
	^super checkedBrowserParameters:	theBrowserParameters.! !

!KRUIPathFinder class publicMethodsFor: 'interface specs'!

aboutSpec
	^KRONOLauncherPanel autorCanvasSpec!

technicalSupportSpec

	^KRONOLauncherPanel soporteCanvasSpec!

windowSpec


	"UIPainter new openOnClass: self andSelector: #windowSpec"

	^#(#FullSpec #window: #(#WindowSpec #label: 'Browser' #min: #(#Point 400 300 ) #bounds: #(#Rectangle 32 32 932 732 ) #flags: 4 #menu: #metaApplicationBrowserMenu ) #component: #(#SpecCollection #collection: #( ) ) )! !

!KRUIPathFinder class publicMethodsFor: 'menu'!

assistantDebugMenu
	"self assistantDebugMenu startUp"

	^super assistantDebugMenu!

assistantRuntimeMenu
	"self assistantRuntimeMenu startUp"

	^super assistantDebugMenu!

helpMenu
	"self helpMenu startUp"

	^#(#HWHelpPopUpMenu #('A_yuda' '_Autor' '_Soporte') #(1 2 ) #( #help #about #techSupport) #('Obtener Ayuda acerca de KRONOware' 'Informacion acerca del Autor, Derechos y Limitaciones de Uso de KRONOware' 'Obtener soporte acerda de KRONOware') ) decodeAsLiteralArray!

listWindowsMenu
	"KRUIPathFinder listWindowsMenu startUp"

	^RTDynamicHelpPopUpMenu fromOptionsBlock: 
		(RTDynamicHelpPopUpMenu windowsRaiseBuildBlockGetWindowsFromBlock: self getWindowsBlock)
		helpString: self windowsRaiseHelpString!

metaApplicationBrowserDebugMenu
	"self metaApplicationBrowserDebugMenu startUp"

	| aMenu aToolsMenu |

	aToolsMenu := self toolsMenu.
	aToolsMenu isNil
		ifTrue: [
			aMenu := HWHelpPopUpMenu 
				labelArray: #('_Navegador' 'Camino' 'Formato' '_Grabadora' '_Ventanas' '_Ayuda' 'Acceso Desarrollador') copy
				lines: #()
				values: (OrderedCollection new 
					add: self browserMenu;
					add: self pathMenu;
					add: self viewsMenu;
					add: self assistantMenu;
					add: self windowsMenu;
					add: self helpMenu;
					add: self itselfMenu;
					yourself)
				helps: #(''  '' '' '' '' '' '')]
		ifFalse: [
			aMenu := HWHelpPopUpMenu 
				labelArray: #('_Navegador' 'Camino' 'Formato' '_Grabadora' '_Ventanas' 'Herramientas' '_Ayuda' 'Acceso Desarrollador') copy
				lines: #()
				values: (OrderedCollection new 
					add: self browserMenu;
					add: self pathMenu;
					add: self viewsMenu;
					add: self assistantMenu;
					add: self windowsMenu;
					add: aToolsMenu;
					add: self helpMenu;
					add: self itselfMenu;
					yourself)
				helps: #(''  '' '' '' '' '' ''  '')].


	^aMenu!

metaApplicationBrowserMenu
	^DEBUGDvpt
		ifTrue: [ self metaApplicationBrowserDebugMenu]
		ifFalse: [ self metaApplicationBrowserRuntimeMenu]!

metaApplicationBrowserRuntimeMenu
	"self metaApplicationBrowserRuntimeMenu startUp"

	| aMenu aToolsMenu |

	aToolsMenu := self toolsMenu.
	aToolsMenu isNil
		ifTrue: [
			aMenu := HWHelpPopUpMenu 
				labelArray: #('_Navegador'  'Camino' 'Formato' '_Grabadora' '_Ventanas' '_Ayuda') copy
				lines: #()
				values: (OrderedCollection new 
					add:  self browserMenu;
					add: self pathMenu;
					add: self viewsMenu;
					add: self assistantMenu;
					add: self windowsMenu;
					add: self helpMenu;
					yourself)
				helps: #('' '' ''  '' '' '')]
		ifFalse: [
			aMenu := HWHelpPopUpMenu 
				labelArray: #('_Navegador' 'Camino'  'Formato' '_Grabadora' '_Ventanas' 'Herramientas' '_Ayuda') copy
				lines: #()
				values: (OrderedCollection new 
					add:  self browserMenu;
					add: self pathMenu;
					add: self viewsMenu;
					add: self assistantMenu;
					add: self windowsMenu;
					add: aToolsMenu;
					add: self helpMenu;
					yourself)
				helps: #('' ''  '' '' '' '' ''  )].

	^aMenu!

pathMenu
	"self pathMenu startUp"

	^#(#HWHelpPopUpMenu #('_Desde el principio' '_Volver Atras' '_Abrir ventana de camino' '_Cerrar  ventana de camino' ) #(2 ) #(#goToFirstInHistory #backOneInHistory #showPathWindow #hidePathWindow ) #('Mostrar el primer objeto en el camino' 'Mostrar objeto anterior en el camino' 'Abrir ventana de camino' 'Cerrar ventana de camino' ))  decodeAsLiteralArray!

viewsMenu
	"self viewsMenu startUp"

	^#(#HWHelpPopUpMenu #('_Solo Navegador' 'Solo Editor' '_Navegador y Editor' '-- no usada --' ) #(3 ) #(#onlyDisplayLists #onlyDisplayEditor #displayListAndEditor #showInheritedMode ) #( 'Mostrar solamente el panel de navegacion de caminos' 'Mostrar solamente el panel de edicion de datos' 'Mostrar los dos paneles : navegacio y editor' '-- no usado --' ) )  decodeAsLiteralArray!

windowsMenu
	"self windowsMenu startUp"
	^self listWindowsMenu!

windowsRaiseHelpString
	^'Mostrar ventana ' copy! !

!KRUIPathFinder class publicMethodsFor: 'ref:preferences'!

preferredConfigurationsBrowserClass
	^KRUIConfigurationsBrowser!

preferredEditorsOpenerClass
	^KRUIEditorsOpener!

preferredMETAConfigurationClass

	^KRUIMETAConfiguration! !

!KRUIPathFinder publicMethodsFor: 'error handling'!

logControlLoopException: theException!

notifyControlLoopException: theException

	Dialog warn: 'Ha ocurrido un error.\Su ventana ha sido cerrada.' withCRs! !

!KRUIPathFinder publicMethodsFor: 'label'!

browserLabelTitlePrefix
	^'KRONOware: ' copy!

numberOfEditorHolders
	| aBrowserParameters aNumEditorHolders anObjectHolder |

	anObjectHolder := self objectHolder.
	anObjectHolder isNil ifFalse: [ 
		aBrowserParameters := anObjectHolder browserParameters.
		aBrowserParameters isNil ifFalse: [ 
			aNumEditorHolders := aBrowserParameters at: METABrowser numberOfEditorHoldersParameterSymbol ifAbsent: [ nil].
			aNumEditorHolders isNil ifFalse: [ ^aNumEditorHolders]]].


	 ^super numberOfEditorHolders "delegate on default way of finding it : through class method on configuration"! !

!KRUIPathFinder publicMethodsFor: 'menu'!

about
	KRONOLauncherPanel openWithSpec: #autorWindowSpec!

help

	Dialog warn: 
		'Por favor, contacte Soporte tecnico\para realizar cualquier consulta\y recibir un manual de instrucciones'
		withCRs.
	self techSupport!

techSupport
	KRONOLauncherPanel openWithSpec: #soporteWindowSpec! !

!KRUIPathFinder publicMethodsFor: 'updating'!

updateWindowLabel

	| aLabel aBrowserParameters aPrefix aPrefixString |
	aLabel :=  (self readOnly 
		ifTrue: ['Solo Lectura ', self browserLabelTitlePrefix] 
		ifFalse: [self browserLabelTitlePrefix]),
		objectHolder getValueClassName , ' ', objectHolder getValueName.

	aBrowserParameters := objectHolder browserParameters.

	aPrefix := aBrowserParameters isNil
		ifTrue: ['']
		ifFalse: [ 
			aPrefixString := aBrowserParameters at: METABrowser windowLabelPrefixSymbol ifAbsent: [nil].
			aPrefixString isNil
				ifTrue: ['']
				ifFalse: [aPrefixString, ' ']].

	
	self builder window label:  aPrefix , aLabel.
	self isDialog ifTrue: [ self dialogLabel value: 'Selection ', aPrefix , aLabel].

	self pathListApplication isNil ifFalse: [self pathListApplication updateWindowLabel].!

updateWindowsList
 
	self updateWindowsListMenu!

updateWindowsListMenu

	| aMenu aWindowsListMenu aMenuHolder |

	aMenuHolder := self metaApplicationBrowserMenu value.
	aMenuHolder isNil ifTrue: [ ^self].

	aMenu := aMenuHolder value.
	aMenu isNil ifTrue: [ ^self].

	aWindowsListMenu := aMenu valueAt: self class windowsSubMenuIndexInMenu.
	aWindowsListMenu buildMenu.! !

KRUIDefinitionsHolder initializeAfterLoad!
KRUIEditorsOpener initializeAfterLoad!
KRUIDeveloperConfiguration initializeAfterLoad!
KRUIMETAConfiguration initializeAfterLoad!
KRUIConfigurationsCollection initializeAfterLoad!
KRUIPathFinder initializeAfterLoad!
KRUIConfigurationsBrowser initializeAfterLoad!
KRUIApplicationBrowser initializeAfterLoad!
KRONO_UI initializeAfterLoad!

KRONO_UI loaded!
