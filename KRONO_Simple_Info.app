'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:38:03 pm'!



((KRONO_Simple createSubApplication: #KRONO_Simple_Info in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

KRONO_Simple_Info becomeDefault!

CMInfoPersistencyHolder subclass: #KRSimpleInfoHolder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Simple_Info becomeDefault!

SubApplication subclass: #KRONO_Simple_Info
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Simple_Info becomeDefault!

!KRSimpleInfoHolder class publicMethodsFor: 'browse'!

browseArchivoGeneral
	"KRSimpleInfoHolder browseArchivoGeneral "

	| aModel  aDefinitionsHolder aMetaInfo aBrowserClass aDomainCMGO aHomeArchivo anArchivoGeneralRoot |

	aDomainCMGO := KRSimpleInfoHolder  currentInfoStoreMethodSelector: #kronoSimpleInfoStore.
	aDomainCMGO isNil ifTrue: [ ^self].

	aHomeArchivo := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Archivo'].
	aHomeArchivo isNil ifTrue: [ ^self].

	anArchivoGeneralRoot := aHomeArchivo metaInfo getObject: aHomeArchivo featureNamedValue: 'homeRootsCMGO' 
		detect: 'nombre' test: [:anArchivoName | anArchivoName = 'General'] orCreate: 'General'.
	anArchivoGeneralRoot  isNil ifTrue: [ ^self].

	aMetaInfo := aHomeArchivo metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^self].

	aDefinitionsHolder := [ KRUIDefinitionsHolder fromModel: aModel].

	aBrowserClass := InputState default shiftDown 
		ifTrue: [ CODEMModelGenericBrowser ]
		ifFalse:  [ KRUIPathFinder].
	
	aBrowserClass 
		openForObject: 			anArchivoGeneralRoot 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 5;
				yourself)!

browseCodificacionEnfermedades
	"KRSimpleInfoHolder browseCodificacionEnfermedades "

	| aModel  aDefinitionsHolder aMetaInfo aBrowserClass aDomainCMGO aHomeArchivo anArchivoGeneralRoot aCodificacionEnfermedades |

	aDomainCMGO := KRSimpleInfoHolder  currentInfoStoreMethodSelector: #kronoSimpleInfoStore.
	aDomainCMGO isNil ifTrue: [ ^self].

	aHomeArchivo := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Archivo'].
	aHomeArchivo isNil ifTrue: [ ^self].

	anArchivoGeneralRoot := aHomeArchivo metaInfo getObject: aHomeArchivo featureNamedValue: 'homeRootsCMGO' 
		detect: 'nombre' test: [:anArchivoName | anArchivoName = 'General'] orCreate: 'General'.
	anArchivoGeneralRoot  isNil ifTrue: [ ^self].

	aCodificacionEnfermedades := anArchivoGeneralRoot metaInfo getObject: anArchivoGeneralRoot featureNamedValue: 'codificacionEnfermedades'.
	aCodificacionEnfermedades isNil ifTrue: [ ^self].

	aMetaInfo := aCodificacionEnfermedades metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^self].

	aDefinitionsHolder := [ KRUIDefinitionsHolder fromModel: aModel].

	aBrowserClass := InputState default shiftDown 
		ifTrue: [ CODEMModelGenericBrowser ]
		ifFalse:  [ KRUIPathFinder].
	
	aBrowserClass 
		openForObject: 			aCodificacionEnfermedades 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 4;
				yourself)!

browseCodificacionProcedimientos
	"KRSimpleInfoHolder browseCodificacionProcedimientos "

	| aModel  aDefinitionsHolder aMetaInfo aBrowserClass aDomainCMGO aHomeArchivo anArchivoGeneralRoot aCodificacionProcedimientos |

	aDomainCMGO := KRSimpleInfoHolder  currentInfoStoreMethodSelector: #kronoSimpleInfoStore.
	aDomainCMGO isNil ifTrue: [ ^self].

	aHomeArchivo := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Archivo'].
	aHomeArchivo isNil ifTrue: [ ^self].

	anArchivoGeneralRoot := aHomeArchivo metaInfo getObject: aHomeArchivo featureNamedValue: 'homeRootsCMGO' 
		detect: 'nombre' test: [:anArchivoName | anArchivoName = 'General'] orCreate: 'General'.
	anArchivoGeneralRoot  isNil ifTrue: [ ^self].

	aCodificacionProcedimientos := anArchivoGeneralRoot metaInfo getObject: anArchivoGeneralRoot featureNamedValue: 'codificacionProcedimientos'.
	aCodificacionProcedimientos isNil ifTrue: [ ^self].

	aMetaInfo := aCodificacionProcedimientos metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^self].

	aDefinitionsHolder := [ KRUIDefinitionsHolder fromModel: aModel].

	aBrowserClass := InputState default shiftDown 
		ifTrue: [ CODEMModelGenericBrowser ]
		ifFalse:  [ KRUIPathFinder].
	
	aBrowserClass 
		openForObject: 			aCodificacionProcedimientos 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 4;
				yourself)!

browseCodificacionSubstancias
	"KRSimpleInfoHolder browseCodificacionSubstancias "

	| aModel  aDefinitionsHolder aMetaInfo aBrowserClass aDomainCMGO aHomeArchivo anArchivoGeneralRoot aCodificacionSubstancias |

	aDomainCMGO := KRSimpleInfoHolder  currentInfoStoreMethodSelector: #kronoSimpleInfoStore.
	aDomainCMGO isNil ifTrue: [ ^self].

	aHomeArchivo := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Archivo'].
	aHomeArchivo isNil ifTrue: [ ^self].

	anArchivoGeneralRoot := aHomeArchivo metaInfo getObject: aHomeArchivo featureNamedValue: 'homeRootsCMGO' 
		detect: 'nombre' test: [:anArchivoName | anArchivoName = 'General'] orCreate: 'General'.
	anArchivoGeneralRoot  isNil ifTrue: [ ^self].

	aCodificacionSubstancias := anArchivoGeneralRoot metaInfo getObject: anArchivoGeneralRoot featureNamedValue: 'codificacionSubstancias'.
	aCodificacionSubstancias isNil ifTrue: [ ^self].

	aMetaInfo := aCodificacionSubstancias metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^self].

	aDefinitionsHolder := [ KRUIDefinitionsHolder fromModel: aModel].

	aBrowserClass := InputState default shiftDown 
		ifTrue: [ CODEMModelGenericBrowser ]
		ifFalse:  [ KRUIPathFinder].
	
	aBrowserClass 
		openForObject: 			aCodificacionSubstancias 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 4;
				yourself)!

browseCurrentWholeModel
	"KRSimpleInfoHolder browseCurrentWholeModel "

	| aModel  aDefinitionsHolder aMetaInfo aBrowserClass anArchivo |

	anArchivo := KRSimpleInfoHolder  currentInfoStoreMethodSelector: #kronoSimpleInfoStore.

	aMetaInfo := anArchivo metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^nil].

	aDefinitionsHolder := [ KRUIDefinitionsHolder fromModel: aModel].

	aBrowserClass := InputState default shiftDown 
		ifTrue: [ CODEMModelGenericBrowser ]
		ifFalse:  [ KRUIPathFinder].
	
	aBrowserClass 
		openForObject: 			anArchivo 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser editorsOpenerParameterSymbol put: CODEMModelEditorsOpener editorsOpener;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 5;
				at: METABrowser initialPathParameterSymbol put: 
					#(#('homesCMGO' 'Archivo') ('homeRootsCMGO' 'General'));
				yourself)!

browseDepartamentosClinica
	"KRSimpleInfoHolder browseDepartamentosClinica "

	| aModel  aDefinitionsHolder aMetaInfo aBrowserClass aDomainCMGO aHomeArchivo anArchivoGeneralRoot aClinica |

	aDomainCMGO := KRSimpleInfoHolder  currentInfoStoreMethodSelector: #kronoSimpleInfoStore.
	aDomainCMGO isNil ifTrue: [ ^self].

	aHomeArchivo := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Archivo'].
	aHomeArchivo isNil ifTrue: [ ^self].

	anArchivoGeneralRoot := aHomeArchivo metaInfo getObject: aHomeArchivo featureNamedValue: 'homeRootsCMGO' 
		detect: 'nombre' test: [:anArchivoName | anArchivoName = 'General'] orCreate: 'General'.
	anArchivoGeneralRoot  isNil ifTrue: [ ^self].

	aClinica := anArchivoGeneralRoot metaInfo getObject: anArchivoGeneralRoot featureNamedValue: 'clinica' .
	aClinica  isNil ifTrue: [ ^self].

	aMetaInfo := aHomeArchivo metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^self].

	aDefinitionsHolder := [ | aDefHldr |
		aDefHldr := KRUIDefinitionsHolder fromModel: aModel.
		aDefHldr redefine: (aModel typeNamed: 'Nucleo::Clinica')  pathSelectorNames:
			#( 'departamentos').
		aDefHldr redefineAll: (aModel typeNamed: 'Nucleo::DepartamentoClinica') withAllSubtypes 
			perspective: 'General' selectorNames:
			#('nombreDepartamento' 'tipoDepartamento' 
				'numeroTelecomunicacionTelefonoOficialUno' 'numeroTelecomunicacionTelefonoOficialDos'  				'numeroTelecomunicacionFaxOficial').
		aDefHldr redefine: (aModel typeNamed: 'Nucleo::Clinica') perspective: 'General' selectorNames:
			#('cif' 'razonSocial' 'nombreComercial' 'direccionOficial' 'direccionFacturaAlquiler' 
				'numeroTelecomunicacionTelefonoOficialUno' 'numeroTelecomunicacionTelefonoOficialDos'  				'numeroTelecomunicacionFaxOficial').
		aDefHldr redefineAll: (aModel typeNamed: 'Nucleo::PersonaClinica') withAllSubtypes pathSelectorNames:
			#('puestosClinica' 'numerosTelecomunicacion' 'direcciones' 
				'numeroTelecomunicacionTelefonoOficialUno' 'numeroTelecomunicacionTelefonoOficialDos'  				'numeroTelecomunicacionFaxOficial'
				).
		aDefHldr redefineAll: (aModel typeNamed: 'Nucleo::DepartamentoClinica') withAllSubtypes pathSelectorNames:
			#('puestosClinica' 'departamentos' 'numerosTelecomunicacion' 'direcciones' 
				'numeroTelecomunicacionTelefonoOficialUno' 'numeroTelecomunicacionTelefonoOficialDos'  				'numeroTelecomunicacionFaxOficial'
				).
		aDefHldr
	].

	aBrowserClass := InputState default shiftDown 
		ifTrue: [ CODEMModelGenericBrowser ]
		ifFalse:  [ KRUIPathFinder].
	
	aBrowserClass 
		openForObject: 			aClinica 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 4;
				at: METABrowser windowLabelPrefixSymbol put: 'Departamentos Clinica';
				at: METABrowser initialPathParameterSymbol put: #(#('departamentos' ) );
				yourself)!

browseFacturacionAlquiler
	"KRSimpleInfoHolder browseFacturacionAlquiler "

	| aModel  aDefinitionsHolder aMetaInfo aBrowserClass aDomainCMGO aHomeArchivo anArchivoGeneralRoot aFacturacionAlquiler |

	aDomainCMGO := KRSimpleInfoHolder  currentInfoStoreMethodSelector: #kronoSimpleInfoStore.
	aDomainCMGO isNil ifTrue: [ ^self].

	aHomeArchivo := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Archivo'].
	aHomeArchivo isNil ifTrue: [ ^self].

	anArchivoGeneralRoot := aHomeArchivo metaInfo getObject: aHomeArchivo featureNamedValue: 'homeRootsCMGO' 
		detect: 'nombre' test: [:anArchivoName | anArchivoName = 'General'] orCreate: 'General'.
	anArchivoGeneralRoot  isNil ifTrue: [ ^self].

	aFacturacionAlquiler := anArchivoGeneralRoot metaInfo getObject: anArchivoGeneralRoot featureNamedValue: 'facturacionAlquiler'.
	aFacturacionAlquiler isNil ifTrue: [ ^self].

	aMetaInfo := aHomeArchivo metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^self].

	aDefinitionsHolder := [ KRUIDefinitionsHolder fromModel: aModel].

	aBrowserClass := InputState default shiftDown 
		ifTrue: [ CODEMModelGenericBrowser ]
		ifFalse:  [ KRUIPathFinder].
	
	aBrowserClass 
		openForObject: 			aFacturacionAlquiler 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 3;
				yourself)!

browseFacturacionAlquilerCatalogosTarifas
	"KRSimpleInfoHolder browseFacturacionAlquilerCatalogosTarifas"

	| aModel  aDefinitionsHolder aMetaInfo aBrowserClass aDomainCMGO aHomeArchivo anArchivoGeneralRoot aFacturacionAlquiler aCatalogoFacturacionAlquiler |

	aDomainCMGO := KRSimpleInfoHolder  currentInfoStoreMethodSelector: #kronoSimpleInfoStore.
	aDomainCMGO isNil ifTrue: [ ^self].

	aHomeArchivo := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Archivo'].
	aHomeArchivo isNil ifTrue: [ ^self].

	anArchivoGeneralRoot := aHomeArchivo metaInfo getObject: aHomeArchivo featureNamedValue: 'homeRootsCMGO' 
		detect: 'nombre' test: [:anArchivoName | anArchivoName = 'General'] orCreate: 'General'.
	anArchivoGeneralRoot  isNil ifTrue: [ ^self].

	aFacturacionAlquiler := anArchivoGeneralRoot metaInfo getObject: anArchivoGeneralRoot featureNamedValue: 'facturacionAlquiler'.
	aFacturacionAlquiler isNil ifTrue: [ ^self].

	aCatalogoFacturacionAlquiler := aFacturacionAlquiler metaInfo getObject: aFacturacionAlquiler featureNamedValue: 'catalogoConceptosAlquiler'.
	aCatalogoFacturacionAlquiler isNil ifTrue: [ ^self].


	aMetaInfo := aHomeArchivo metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^self].

	aDefinitionsHolder := [ | aDefHldr |
		aDefHldr := KRUIDefinitionsHolder fromModel: aModel.
		aDefHldr redefine: (aModel typeNamed: 'Tarifas::CatalogoConceptosAlquiler')  pathSelectorNames:
			#( 'conceptosAlquiler' 'tarifasAlquiler' 'tarifaActualAlquiler').
		aDefHldr
	].

	aBrowserClass := InputState default shiftDown 
		ifTrue: [ CODEMModelGenericBrowser ]
		ifFalse:  [ KRUIPathFinder].
	
	aBrowserClass 
		openForObject: 			aCatalogoFacturacionAlquiler 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at: METABrowser showCanvasLabelParameterSymbol put: true;
				at: METABrowser numberOfEditorHoldersParameterSymbol put: 2;
				at: METABrowser initialPathParameterSymbol put: #(#('catalogosConceptosAlquiler' ) );
				at: METABrowser windowLabelPrefixSymbol put: 'Tarifas Alquiler';
				yourself)!

browseFacturacionAlquilerClientes
	"KRSimpleInfoHolder browseFacturacionAlquilerClientes "

	| aModel  aDefinitionsHolder aMetaInfo aBrowserClass aDomainCMGO aHomeArchivo anArchivoGeneralRoot aFacturacionAlquiler |

	aDomainCMGO := KRSimpleInfoHolder  currentInfoStoreMethodSelector: #kronoSimpleInfoStore.
	aDomainCMGO isNil ifTrue: [ ^self].

	aHomeArchivo := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Archivo'].
	aHomeArchivo isNil ifTrue: [ ^self].

	anArchivoGeneralRoot := aHomeArchivo metaInfo getObject: aHomeArchivo featureNamedValue: 'homeRootsCMGO' 
		detect: 'nombre' test: [:anArchivoName | anArchivoName = 'General'] orCreate: 'General'.
	anArchivoGeneralRoot  isNil ifTrue: [ ^self].

	aFacturacionAlquiler := anArchivoGeneralRoot metaInfo getObject: anArchivoGeneralRoot featureNamedValue: 'facturacionAlquiler'.
	aFacturacionAlquiler isNil ifTrue: [ ^self].

	aMetaInfo := aHomeArchivo metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^self].

	aDefinitionsHolder := [ | aDefHldr |
		aDefHldr := KRUIDefinitionsHolder fromModel: aModel.
		aDefHldr redefine: (aModel typeNamed: 'Facturacion::FacturacionAlquiler')  pathSelectorNames:
			#('clientesFacturasAlquiler' ).
		aDefHldr redefineAll: (aModel typeNamed: 'Facturacion::ClienteFacturaAlquiler') withAllSubtypes pathSelectorNames:
			#( 'direcciones' 'direccionFacturaAlquiler' 
				'numerosTelecomunicacion'
				'numeroTelecomunicacionTelefonoOficialUno' 'numeroTelecomunicacionTelefonoOficialDos'  'numeroTelecomunicacionFaxOficial'
				'facturas').
		aDefHldr
	].

	aBrowserClass := InputState default shiftDown 
		ifTrue: [ CODEMModelGenericBrowser ]
		ifFalse:  [ KRUIPathFinder].
	
	aBrowserClass 
		openForObject: 			aFacturacionAlquiler 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 3;
				at: METABrowser initialPathParameterSymbol put: #(#('clientesFacturasAlquiler' ) );
				at: METABrowser windowLabelPrefixSymbol put: 'Arrendatarios';
				at: METABrowser initialShowRootInEditorParameterSymbol put: false;
				yourself)!

browseFacturacionAlquilerFacturas
	"KRSimpleInfoHolder browseFacturacionAlquilerFacturas "

	| aModel  aDefinitionsHolder aMetaInfo aBrowserClass aDomainCMGO aHomeArchivo anArchivoGeneralRoot aFacturacionAlquiler |

	aDomainCMGO := KRSimpleInfoHolder  currentInfoStoreMethodSelector: #kronoSimpleInfoStore.
	aDomainCMGO isNil ifTrue: [ ^self].

	aHomeArchivo := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Archivo'].
	aHomeArchivo isNil ifTrue: [ ^self].

	anArchivoGeneralRoot := aHomeArchivo metaInfo getObject: aHomeArchivo featureNamedValue: 'homeRootsCMGO' 
		detect: 'nombre' test: [:anArchivoName | anArchivoName = 'General'] orCreate: 'General'.
	anArchivoGeneralRoot  isNil ifTrue: [ ^self].

	aFacturacionAlquiler := anArchivoGeneralRoot metaInfo getObject: anArchivoGeneralRoot featureNamedValue: 'facturacionAlquiler'.
	aFacturacionAlquiler isNil ifTrue: [ ^self].

	aMetaInfo := aHomeArchivo metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^self].

	aDefinitionsHolder := [ | aDefHldr |
		aDefHldr := KRUIDefinitionsHolder fromModel: aModel.
		aDefHldr redefine: (aModel typeNamed: 'Facturacion::FacturacionAlquiler')  pathSelectorNames:
			#( 'ejerciciosFacturacion' ).
		aDefHldr
	].


	aBrowserClass := InputState default shiftDown 
		ifTrue: [ CODEMModelGenericBrowser ]
		ifFalse:  [ KRUIPathFinder].
	
	aBrowserClass 
		openForObject: 			aFacturacionAlquiler 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 5;
				at: METABrowser initialPathParameterSymbol put: #(#('ejercicios' ) );
				at: METABrowser windowLabelPrefixSymbol put: 'Facturas Alquiler';
				at: METABrowser initialShowRootInEditorParameterSymbol put: true;
				yourself)!

browseFacturacionClinica
	"KRSimpleInfoHolder browseFacturacionClinica "

	| aModel  aDefinitionsHolder aMetaInfo aBrowserClass aDomainCMGO aHomeArchivo anArchivoGeneralRoot aFacturacionClinica |

	aDomainCMGO := KRSimpleInfoHolder  currentInfoStoreMethodSelector: #kronoSimpleInfoStore.
	aDomainCMGO isNil ifTrue: [ ^self].

	aHomeArchivo := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Archivo'].
	aHomeArchivo isNil ifTrue: [ ^self].

	anArchivoGeneralRoot := aHomeArchivo metaInfo getObject: aHomeArchivo featureNamedValue: 'homeRootsCMGO' 
		detect: 'nombre' test: [:anArchivoName | anArchivoName = 'General'] orCreate: 'General'.
	anArchivoGeneralRoot  isNil ifTrue: [ ^self].

	aFacturacionClinica := anArchivoGeneralRoot metaInfo getObject: anArchivoGeneralRoot featureNamedValue: 'facturacionClinica'.
	aFacturacionClinica isNil ifTrue: [ ^self].

	aMetaInfo := aHomeArchivo metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^self].

	aDefinitionsHolder := [ KRUIDefinitionsHolder fromModel: aModel].

	aBrowserClass := InputState default shiftDown 
		ifTrue: [ CODEMModelGenericBrowser ]
		ifFalse:  [ KRUIPathFinder].
	
	aBrowserClass 
		openForObject: 			aFacturacionClinica 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 3;
				yourself)!

browseFacturacionClinicaAseguradoras
	"KRSimpleInfoHolder browseFacturacionClinicaAseguradoras "

	| aModel  aDefinitionsHolder aMetaInfo aBrowserClass aDomainCMGO aHomeArchivo anArchivoGeneralRoot |

	aDomainCMGO := KRSimpleInfoHolder  currentInfoStoreMethodSelector: #kronoSimpleInfoStore.
	aDomainCMGO isNil ifTrue: [ ^self].

	aHomeArchivo := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Archivo'].
	aHomeArchivo isNil ifTrue: [ ^self].

	anArchivoGeneralRoot := aHomeArchivo metaInfo getObject: aHomeArchivo featureNamedValue: 'homeRootsCMGO' 
		detect: 'nombre' test: [:anArchivoName | anArchivoName = 'General'] orCreate: 'General'.
	anArchivoGeneralRoot  isNil ifTrue: [ ^self].

	aMetaInfo := aHomeArchivo metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^self].

	aDefinitionsHolder := [ | aDefHldr |
		aDefHldr := KRUIDefinitionsHolder fromModel: aModel.
		aDefHldr redefine: (aModel typeNamed: 'Nucleo::Archivo')  pathSelectorNames:
			#( 'organizacionesPagadorasPrestaciones' ).
		aDefHldr redefineAll: (aModel typeNamed: 'Organizaciones::Organizacion') withAllSubtypes pathSelectorNames:
			#( 'planesPagoPrestaciones' 'contratosPagoPrestaciones'  'intermediaciones' 
				'subOrganizaciones'
				'contactos' 'direcciones' 'direccionOficial' 'direccionFacturaClinica' 'direccionNotificacion'
				'numerosTelecomunicacion'
				'numeroTelecomunicacionTelefonoOficialUno' 'numeroTelecomunicacionTelefonoOficialDos'  'numeroTelecomunicacionFaxOficial'
				'origenDePacientes' 'responsableDePagoDe' 'intermediarioDe'
				'facturas').
		aDefHldr redefine: (aModel typeNamed: 'Organizaciones::PlanPagoPrestacionesConTarifaDeAseguradora')  pathSelectorNames:
			#( 'catalogoConceptosClinica'  'contratosPagoPrestaciones' ).
		aDefHldr redefine: (aModel typeNamed: 'Tarifas::CatalogoConceptosClinicaDeAseguradora')  pathSelectorNames:
			#( 'conceptos' 'tarifasClinica' 'tarifaActualClinica').
		aDefHldr
	].

	aBrowserClass := InputState default shiftDown 
		ifTrue: [ CODEMModelGenericBrowser ]
		ifFalse:  [ KRUIPathFinder].
	
	aBrowserClass 
		openForObject: 			anArchivoGeneralRoot 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 5;
				at: METABrowser initialPathParameterSymbol put: #(#('organizacionesPagadorasPrestaciones' ) );
				at: METABrowser windowLabelPrefixSymbol put: 'Pagadoras Prestaciones';
				at: METABrowser initialShowRootInEditorParameterSymbol put: false;
				yourself)!

browseFacturacionClinicaCatalogosTarifas
	"KRSimpleInfoHolder browseFacturacionAlquilerCatalogosTarifas"

	| aModel  aDefinitionsHolder aMetaInfo aBrowserClass aDomainCMGO aHomeArchivo anArchivoGeneralRoot aFacturacionClinica |

	aDomainCMGO := KRSimpleInfoHolder  currentInfoStoreMethodSelector: #kronoSimpleInfoStore.
	aDomainCMGO isNil ifTrue: [ ^self].

	aHomeArchivo := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Archivo'].
	aHomeArchivo isNil ifTrue: [ ^self].

	anArchivoGeneralRoot := aHomeArchivo metaInfo getObject: aHomeArchivo featureNamedValue: 'homeRootsCMGO' 
		detect: 'nombre' test: [:anArchivoName | anArchivoName = 'General'] orCreate: 'General'.
	anArchivoGeneralRoot  isNil ifTrue: [ ^self].

	aFacturacionClinica := anArchivoGeneralRoot metaInfo getObject: anArchivoGeneralRoot featureNamedValue: 'facturacionClinica'.
	aFacturacionClinica isNil ifTrue: [ ^self].

	aMetaInfo := aHomeArchivo metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^self].

	aDefinitionsHolder := [ | aDefHldr |
		aDefHldr := KRUIDefinitionsHolder fromModel: aModel.
		aDefHldr redefine: (aModel typeNamed: 'Facturacion::FacturacionClinica')  pathSelectorNames:
			#( 'catalogosConceptosClinica'  ).
		aDefHldr redefine: (aModel typeNamed: 'Tarifas::CatalogoConceptosClinicaGeneral')  pathSelectorNames:
			#( 'conceptos'  'todosConceptos' 'tarifasClinica' 'tarifaActualClinica' 'planesPagoPrestaciones' 'catalogosConceptosClinicaEspecificos').
		aDefHldr redefine: (aModel typeNamed: 'Tarifas::CatalogoConceptosClinicaEspecifico')  pathSelectorNames:
			#( 'conceptos'  'todosConceptos'  'tarifasClinica' 'tarifaActualClinica' 'planesPagoPrestaciones' 'catalogoConceptosClinicaEspecifico').
		aDefHldr
	].

	aBrowserClass := InputState default shiftDown 
		ifTrue: [ CODEMModelGenericBrowser ]
		ifFalse:  [ KRUIPathFinder].
	
	aBrowserClass 
		openForObject: 			aFacturacionClinica 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 4;
				at: METABrowser initialPathParameterSymbol put: #(#('catalogosConceptosClinica' ) );
				at: METABrowser windowLabelPrefixSymbol put: 'Catalogos y Tarifas';
				at: METABrowser initialShowRootInEditorParameterSymbol put: false;
			yourself)!

browseFacturacionClinicaFacturas
	"KRSimpleInfoHolder browseFacturacionClinicaFacturas "

	| aModel  aDefinitionsHolder aMetaInfo aBrowserClass aDomainCMGO aHomeArchivo anArchivoGeneralRoot aFacturacionClinica |

	aDomainCMGO := KRSimpleInfoHolder  currentInfoStoreMethodSelector: #kronoSimpleInfoStore.
	aDomainCMGO isNil ifTrue: [ ^self].

	aHomeArchivo := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Archivo'].
	aHomeArchivo isNil ifTrue: [ ^self].

	anArchivoGeneralRoot := aHomeArchivo metaInfo getObject: aHomeArchivo featureNamedValue: 'homeRootsCMGO' 
		detect: 'nombre' test: [:anArchivoName | anArchivoName = 'General'] orCreate: 'General'.
	anArchivoGeneralRoot  isNil ifTrue: [ ^self].

	aFacturacionClinica := anArchivoGeneralRoot metaInfo getObject: anArchivoGeneralRoot featureNamedValue: 'facturacionClinica'.
	aFacturacionClinica isNil ifTrue: [ ^self].

	aMetaInfo := aHomeArchivo metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^self].

	aDefinitionsHolder := [ | aDefHldr |
		aDefHldr := KRUIDefinitionsHolder fromModel: aModel.
		aDefHldr redefine: (aModel typeNamed: 'Facturacion::FacturacionClinica')  pathSelectorNames:
			#('ejerciciosFacturacion' ).
		aDefHldr redefine: (aModel typeNamed: 'Facturacion::FacturaClinicaParticular')  pathSelectorNames:
			#('visitasFacturadas' 'lineasFactura' 'clienteFactura').
		aDefHldr
	].

	aBrowserClass := InputState default shiftDown 
		ifTrue: [ CODEMModelGenericBrowser ]
		ifFalse:  [ KRUIPathFinder].
	
	aBrowserClass 
		openForObject: 			aFacturacionClinica 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 5;
				at: METABrowser initialPathParameterSymbol put: #(#('ejerciciosFacturacion' ) );
				at: METABrowser windowLabelPrefixSymbol put: 'Facturas Clinica';
				at: METABrowser initialShowRootInEditorParameterSymbol put: false;
				yourself)!

browseFacturacionClinicaIntermediarios
	"KRSimpleInfoHolder browseFacturacionClinicaIntermediarios "

	| aModel  aDefinitionsHolder aMetaInfo aBrowserClass aDomainCMGO aHomeArchivo anArchivoGeneralRoot |

	aDomainCMGO := KRSimpleInfoHolder  currentInfoStoreMethodSelector: #kronoSimpleInfoStore.
	aDomainCMGO isNil ifTrue: [ ^self].

	aHomeArchivo := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Archivo'].
	aHomeArchivo isNil ifTrue: [ ^self].

	anArchivoGeneralRoot := aHomeArchivo metaInfo getObject: aHomeArchivo featureNamedValue: 'homeRootsCMGO' 
		detect: 'nombre' test: [:anArchivoName | anArchivoName = 'General'] orCreate: 'General'.
	anArchivoGeneralRoot  isNil ifTrue: [ ^self].

	aMetaInfo := aHomeArchivo metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^self].

	aDefinitionsHolder := [ | aDefHldr |
		aDefHldr := KRUIDefinitionsHolder fromModel: aModel.
		aDefHldr redefine: (aModel typeNamed: 'Nucleo::Archivo')  pathSelectorNames:
			#('organizacionesGestorasPrestaciones' ).
		aDefHldr redefineAll: (aModel typeNamed: 'Organizaciones::OrganizacionGestoraPrestaciones') withAllSubtypes pathSelectorNames:
			#( 'gestionesPagoPrestaciones'
				'subOrganizaciones'
				'contactos' 'direcciones' 'direccionOficial' 'direccionFacturaClinica' 'direccionNotificacion'
				'numerosTelecomunicacion'
				'numeroTelecomunicacionTelefonoOficialUno' 'numeroTelecomunicacionTelefonoOficialDos'  'numeroTelecomunicacionFaxOficial'
				).
		aDefHldr redefineAll: (aModel typeNamed: 'Organizaciones::GestionPagoPrestaciones') withAllSubtypes pathSelectorNames:
			#( 'organizacionOrigenPacientesGestionada').
		aDefHldr
	].

	aBrowserClass := InputState default shiftDown 
		ifTrue: [ CODEMModelGenericBrowser ]
		ifFalse:  [ KRUIPathFinder].
	
	aBrowserClass 
		openForObject: 			anArchivoGeneralRoot 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 5;
				at: METABrowser initialPathParameterSymbol put: #(#('organizacionesGestorasPrestaciones' ) );
				at: METABrowser windowLabelPrefixSymbol put: 'Gestoras';
				at: METABrowser initialShowRootInEditorParameterSymbol put: false;
				yourself)!

browseFacturacionClinicaOrigenesIntermediariosAseguradoras
	"KRSimpleInfoHolder browseFacturacionClinicaOrigenesIntermediariosAseguradoras "

	| aModel  aDefinitionsHolder aMetaInfo aBrowserClass aDomainCMGO aHomeArchivo anArchivoGeneralRoot |

	aDomainCMGO := KRSimpleInfoHolder  currentInfoStoreMethodSelector: #kronoSimpleInfoStore.
	aDomainCMGO isNil ifTrue: [ ^self].

	aHomeArchivo := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Archivo'].
	aHomeArchivo isNil ifTrue: [ ^self].

	anArchivoGeneralRoot := aHomeArchivo metaInfo getObject: aHomeArchivo featureNamedValue: 'homeRootsCMGO' 
		detect: 'nombre' test: [:anArchivoName | anArchivoName = 'General'] orCreate: 'General'.
	anArchivoGeneralRoot  isNil ifTrue: [ ^self].

	aMetaInfo := aHomeArchivo metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^self].

	aDefinitionsHolder := [ | aDefHldr |
		aDefHldr := KRUIDefinitionsHolder fromModel: aModel.
		aDefHldr redefine: (aModel typeNamed: 'Nucleo::Archivo')  pathSelectorNames:
			#( 'organizacionesPagadorasPrestaciones'  'organizacionesOrigenPacientes' 'organizacionesIntermediariasPrestaciones' ).
		aDefHldr redefineAll: (aModel typeNamed: 'Organizaciones::Organizacion') withAllSubtypes pathSelectorNames:
			#( 'planesPagoPrestaciones' 'contratosPagoPrestaciones'  'intermediaciones' 
				'subOrganizaciones'
				'contactos' 'direcciones' 'direccionOficial' 'direccionFacturaClinica' 'direccionNotificacion'
				'numerosTelecomunicacion'
				'numeroTelecomunicacionTelefonoOficialUno' 'numeroTelecomunicacionTelefonoOficialDos'  'numeroTelecomunicacionFaxOficial'
				'origenDePacientes' 'responsableDePagoDe' 'intermediarioDe'
				'facturas').
		aDefHldr redefine: (aModel typeNamed: 'Organizaciones::PlanPagoPrestacionesConTarifaDeAseguradora')  pathSelectorNames:
			#( 'catalogoConceptosClinica'  'contratosPagoPrestaciones' ).
		aDefHldr
	].

	aBrowserClass := InputState default shiftDown 
		ifTrue: [ CODEMModelGenericBrowser ]
		ifFalse:  [ KRUIPathFinder].
	
	aBrowserClass 
		openForObject: 			anArchivoGeneralRoot 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 5;
				at: METABrowser initialPathParameterSymbol put: #(#('organizacionesPagadorasPrestaciones' ) );
				at: METABrowser windowLabelPrefixSymbol put: 'Origenes y Aseguradoras';
				at: METABrowser initialShowRootInEditorParameterSymbol put: false;
				yourself)!

browseFacturacionClinicaOrigenesPacientes
	"KRSimpleInfoHolder browseFacturacionClinicaOrigenesPacientes "

	| aModel  aDefinitionsHolder aMetaInfo aBrowserClass aDomainCMGO aHomeArchivo anArchivoGeneralRoot |

	aDomainCMGO := KRSimpleInfoHolder  currentInfoStoreMethodSelector: #kronoSimpleInfoStore.
	aDomainCMGO isNil ifTrue: [ ^self].

	aHomeArchivo := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Archivo'].
	aHomeArchivo isNil ifTrue: [ ^self].

	anArchivoGeneralRoot := aHomeArchivo metaInfo getObject: aHomeArchivo featureNamedValue: 'homeRootsCMGO' 
		detect: 'nombre' test: [:anArchivoName | anArchivoName = 'General'] orCreate: 'General'.
	anArchivoGeneralRoot  isNil ifTrue: [ ^self].

	aMetaInfo := aHomeArchivo metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^self].

	aDefinitionsHolder := [ | aDefHldr |
		aDefHldr := KRUIDefinitionsHolder fromModel: aModel.
		aDefHldr redefine: (aModel typeNamed: 'Nucleo::Archivo')  pathSelectorNames:
			#( 'organizacionesOrigenPacientes' ).
		aDefHldr redefineAll: (aModel typeNamed: 'Organizaciones::OrganizacionOrigenPacientes') withAllSubtypes pathSelectorNames:
			#(  'contratosPagoPrestaciones'  'gestionesPagoPrestaciones' 
				'subOrganizaciones'
				'contactos' 'direcciones' 'direccionOficial' 'direccionFacturaClinica' 'direccionNotificacion'
				'numerosTelecomunicacion'
				'numeroTelecomunicacionTelefonoOficialUno' 'numeroTelecomunicacionTelefonoOficialDos'  'numeroTelecomunicacionFaxOficial'
				'origenDePacientes' 
				'facturas').
		aDefHldr redefine: (aModel typeNamed: 'Organizaciones::PlanPagoPrestacionesConTarifaDeAseguradora')  pathSelectorNames:
			#( 'catalogoConceptosClinica'  'contratosPagoPrestaciones' ).
		aDefHldr redefineAll: (aModel typeNamed: 'Organizaciones::GestionPagoPrestaciones') withAllSubtypes pathSelectorNames:
			#('gestora').
		aDefHldr
	].

	aBrowserClass := InputState default shiftDown 
		ifTrue: [ CODEMModelGenericBrowser ]
		ifFalse:  [ KRUIPathFinder].
	
	aBrowserClass 
		openForObject: 			anArchivoGeneralRoot 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 5;
				at: METABrowser initialPathParameterSymbol put: #(#('organizacionesOrigenPacientes' ) );
				at: METABrowser windowLabelPrefixSymbol put: 'Origenes Pacientes';
				at: METABrowser initialShowRootInEditorParameterSymbol put: false;
				yourself)!

browseHistoriasClinicasHistorias
	"KRSimpleInfoHolder browseHistoriasClinicasHistorias "

	| aModel  aDefinitionsHolder aMetaInfo aBrowserClass aDomainCMGO aHomeArchivo anArchivoGeneralRoot |

	aDomainCMGO := KRSimpleInfoHolder  currentInfoStoreMethodSelector: #kronoSimpleInfoStore.
	aDomainCMGO isNil ifTrue: [ ^self].

	aHomeArchivo := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Archivo'].
	aHomeArchivo isNil ifTrue: [ ^self].

	anArchivoGeneralRoot := aHomeArchivo metaInfo getObject: aHomeArchivo featureNamedValue: 'homeRootsCMGO' 
		detect: 'nombre' test: [:anArchivoName | anArchivoName = 'General'] orCreate: 'General'.
	anArchivoGeneralRoot  isNil ifTrue: [ ^self].

	aMetaInfo := aHomeArchivo metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^self].

	aDefinitionsHolder := [ | aDefHldr |
		aDefHldr := KRUIDefinitionsHolder fromModel: aModel.
		aDefHldr redefine: (aModel typeNamed: 'Nucleo::Archivo')  pathSelectorNames:
			#( 'historias').
		aDefHldr redefine: (aModel typeNamed: 'Nucleo::Historia')  pathSelectorNames:
			#( 'paciente' 'visitas' 'cambiosEstadoAdministrativo' 'referenciasAHistorias'  'referenciadaPorHistorias').
		aDefHldr redefineAll: (aModel typeNamed: 'Nucleo::ActoClinico') withAllSubtypes  pathSelectorNames:
			#( 'prestacionesClinicaFacturables' 'personalParticipante' 'lineaFactura' ).
		aDefHldr redefine: (aModel typeNamed: 'Nucleo::VisitaParticular')  pathSelectorNames:
			#( 'actosClinicos' 'catalogoConceptosClinica' 'tarifaClinica' 'recepcionista' 'factura').
		aDefHldr redefine: (aModel typeNamed: 'Nucleo::VisitaRemitida')  pathSelectorNames:
			#( 'actosClinicos' 'organizacionOrigenPaciente' 'contratoPagoPrestaciones'  'tarifaClinica' 'recepcionista' 'factura').
		aDefHldr
	].


	aBrowserClass := InputState default shiftDown 
		ifTrue: [ CODEMModelGenericBrowser ]
		ifFalse:  [ KRUIPathFinder].
	
	aBrowserClass 
		openForObject: 			anArchivoGeneralRoot 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 5;
				at: METABrowser initialPathParameterSymbol put: #(#('historias' ) );
				at: METABrowser windowLabelPrefixSymbol put: 'Historias';
				at: METABrowser initialShowRootInEditorParameterSymbol put: false;
				yourself)!

browseOrganizacionYPersonalClinica
	"KRSimpleInfoHolder browseOrganizacionYPersonalClinica "

	| aModel  aDefinitionsHolder aMetaInfo aBrowserClass aDomainCMGO aHomeArchivo anArchivoGeneralRoot aClinica |

	aDomainCMGO := KRSimpleInfoHolder  currentInfoStoreMethodSelector: #kronoSimpleInfoStore.
	aDomainCMGO isNil ifTrue: [ ^self].

	aHomeArchivo := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Archivo'].
	aHomeArchivo isNil ifTrue: [ ^self].

	anArchivoGeneralRoot := aHomeArchivo metaInfo getObject: aHomeArchivo featureNamedValue: 'homeRootsCMGO' 
		detect: 'nombre' test: [:anArchivoName | anArchivoName = 'General'] orCreate: 'General'.
	anArchivoGeneralRoot  isNil ifTrue: [ ^self].

	aClinica := anArchivoGeneralRoot metaInfo getObject: anArchivoGeneralRoot featureNamedValue: 'clinica' .
	aClinica  isNil ifTrue: [ ^self].

	aMetaInfo := aHomeArchivo metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^self].

	aDefinitionsHolder := [ | aDefHldr |
		aDefHldr := KRUIDefinitionsHolder fromModel: aModel.
		aDefHldr redefine: (aModel typeNamed: 'Nucleo::Clinica')  pathSelectorNames:
			#( 	'personalClinica' 'departamentos').
		aDefHldr redefineAll: (aModel typeNamed: 'Nucleo::DepartamentoClinica') withAllSubtypes 
			perspective: 'General' selectorNames:
			#('nombreDepartamento' 'tipoDepartamento' 
				'numeroTelecomunicacionTelefonoOficialUno' 'numeroTelecomunicacionTelefonoOficialDos'  				'numeroTelecomunicacionFaxOficial').
		aDefHldr redefine: (aModel typeNamed: 'Nucleo::Clinica') perspective: 'General' selectorNames:
			#('cif' 'razonSocial' 'nombreComercial' 'direccionOficial' 'direccionFacturaAlquiler' 
				'numeroTelecomunicacionTelefonoOficialUno' 'numeroTelecomunicacionTelefonoOficialDos'  				'numeroTelecomunicacionFaxOficial').
		aDefHldr redefineAll: (aModel typeNamed: 'Nucleo::PersonaClinica') withAllSubtypes pathSelectorNames:
			#('puestosClinica' 'numerosTelecomunicacion' 'direcciones' 
				'numeroTelecomunicacionTelefonoOficialUno' 'numeroTelecomunicacionTelefonoOficialDos'  				'numeroTelecomunicacionFaxOficial'
				).
		aDefHldr redefineAll: (aModel typeNamed: 'Nucleo::DepartamentoClinica') withAllSubtypes pathSelectorNames:
			#('puestoClinicas' 'departamentos' 'numerosTelecomunicacion' 'direcciones' 
				'numeroTelecomunicacionTelefonoOficialUno' 'numeroTelecomunicacionTelefonoOficialDos'  				'numeroTelecomunicacionFaxOficial'
				).
		aDefHldr
	].

	aBrowserClass := InputState default shiftDown 
		ifTrue: [ CODEMModelGenericBrowser ]
		ifFalse:  [ KRUIPathFinder].
	
	aBrowserClass 
		openForObject: 			aClinica 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 3;
				at: METABrowser windowLabelPrefixSymbol put: 'Personal y Departamentos';
				at: METABrowser initialPathParameterSymbol put: #(#('personalClinica' ) );
				"at: METABrowser onlyFirstPathSelectorNamesParameterSymbol put:#(
					'personalClinica' 'departamentos' 
				);"
				yourself)!

browsePersonalClinica
	"KRSimpleInfoHolder browsePersonalClinica "

	| aModel  aDefinitionsHolder aMetaInfo aBrowserClass aDomainCMGO aHomeArchivo anArchivoGeneralRoot aClinica |

	aDomainCMGO := KRSimpleInfoHolder  currentInfoStoreMethodSelector: #kronoSimpleInfoStore.
	aDomainCMGO isNil ifTrue: [ ^self].

	aHomeArchivo := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Archivo'].
	aHomeArchivo isNil ifTrue: [ ^self].

	anArchivoGeneralRoot := aHomeArchivo metaInfo getObject: aHomeArchivo featureNamedValue: 'homeRootsCMGO' 
		detect: 'nombre' test: [:anArchivoName | anArchivoName = 'General'] orCreate: 'General'.
	anArchivoGeneralRoot  isNil ifTrue: [ ^self].

	aClinica := anArchivoGeneralRoot metaInfo getObject: anArchivoGeneralRoot featureNamedValue: 'clinica' .
	aClinica  isNil ifTrue: [ ^self].

	aMetaInfo := aHomeArchivo metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^self].

	aDefinitionsHolder := [ | aDefHldr |
		aDefHldr := KRUIDefinitionsHolder fromModel: aModel.
		aDefHldr redefine: (aModel typeNamed: 'Nucleo::Clinica')  pathSelectorNames:
			#( 	'personalClinica' ).
		aDefHldr redefine: (aModel typeNamed: 'Nucleo::Clinica') perspective: 'General' selectorNames:
			#('cif' 'razonSocial' 'nombreComercial' 'direccionOficial' 'direccionFacturaAlquiler' 
				'numeroTelecomunicacionTelefonoOficialUno' 'numeroTelecomunicacionTelefonoOficialDos'  				'numeroTelecomunicacionFaxOficial').
		aDefHldr redefineAll: (aModel typeNamed: 'Nucleo::PersonaClinica') withAllSubtypes pathSelectorNames:
			#('puestosClinica' 'numerosTelecomunicacion' 'direcciones' 
				'numeroTelecomunicacionTelefonoOficialUno' 'numeroTelecomunicacionTelefonoOficialDos'  				'numeroTelecomunicacionFaxOficial'
				).
		aDefHldr
	].

	aBrowserClass := InputState default shiftDown 
		ifTrue: [ CODEMModelGenericBrowser ]
		ifFalse:  [ KRUIPathFinder].
	
	aBrowserClass 
		openForObject: 			aClinica 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 2;
				at: METABrowser windowLabelPrefixSymbol put: 'Personal Clinica';
				at: METABrowser initialPathParameterSymbol put: #(#('personalClinica' ) );
					yourself)!

browseRazonSocialYDireccionClinica
	"KRSimpleInfoHolder browseRazonSocialYDireccionClinica "

	| aModel  aDefinitionsHolder aMetaInfo aBrowserClass aDomainCMGO aHomeArchivo anArchivoGeneralRoot aClinica |

	aDomainCMGO := KRSimpleInfoHolder  currentInfoStoreMethodSelector: #kronoSimpleInfoStore.
	aDomainCMGO isNil ifTrue: [ ^self].

	aHomeArchivo := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Archivo'].
	aHomeArchivo isNil ifTrue: [ ^self].

	anArchivoGeneralRoot := aHomeArchivo metaInfo getObject: aHomeArchivo featureNamedValue: 'homeRootsCMGO' 
		detect: 'nombre' test: [:anArchivoName | anArchivoName = 'General'] orCreate: 'General'.
	anArchivoGeneralRoot  isNil ifTrue: [ ^self].

	aClinica := anArchivoGeneralRoot metaInfo getObject: anArchivoGeneralRoot featureNamedValue: 'clinica' .
	aClinica  isNil ifTrue: [ ^self].

	aMetaInfo := aHomeArchivo metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^self].

	aDefinitionsHolder := [ | aDefHldr |
		aDefHldr := KRUIDefinitionsHolder fromModel: aModel.
		aDefHldr redefine: (aModel typeNamed: 'Nucleo::Clinica') perspective: 'General' selectorNames:
			#('cif' 'razonSocial' 'nombreComercial' 'direccionOficial' 'direccionFacturaAlquiler' 
				'numeroTelecomunicacionTelefonoOficialUno' 'numeroTelecomunicacionTelefonoOficialDos'  				'numeroTelecomunicacionFaxOficial').
		aDefHldr
	].

	aBrowserClass := InputState default shiftDown 
		ifTrue: [ CODEMModelGenericBrowser ]
		ifFalse:  [ KRUIPathFinder].
	
	aBrowserClass 
		openForObject: 			aClinica 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 1;
				at: METABrowser windowLabelPrefixSymbol put: 'Razon Social y direccion';
				at: METABrowser initialPathParameterSymbol put: #(#('direcciones' ) );
				at: METABrowser onlyFirstPathSelectorNamesParameterSymbol put:#(
					'direcciones' 'numerosTelecomunicacion' 
					'direccionFacturaAlquiler'  'direccionOficial' 'numeroTelecomunicacionTelefonoOficialUno' 
					'numeroTelecomunicacionTelefonoOficialDos'  'numeroTelecomunicacionFaxOficial');
				yourself)! !

!KRSimpleInfoHolder class publicMethodsFor: 'configuration'!

developerConfiguration
	^KRUIDeveloperConfiguration current!

getCreateSomeInfoInNewProjectDomainParameterValue
	| aValue aConfig |

	aConfig := self developerConfiguration.
	aValue := aConfig isNil ifTrue: [ nil] ifFalse: [ aConfig getParameter: aConfig class createSomeInfoInNewProjectDomainParameterName].
	^aValue == true! !

!KRSimpleInfoHolder class publicMethodsFor: 'current'!

currentInfoClinica
	"KRSimpleInfoHolder currentInfoClinica "

	| aDomainCMGO aHomeArchivo anArchivoGeneralRoot aClinica |

	aDomainCMGO := KRSimpleInfoHolder  currentInfoStoreMethodSelector: #kronoSimpleInfoStore.
	aDomainCMGO isNil ifTrue: [ ^self].

	aHomeArchivo := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Archivo'].
	aHomeArchivo isNil ifTrue: [ ^self].

	anArchivoGeneralRoot := aHomeArchivo metaInfo getObject: aHomeArchivo featureNamedValue: 'homeRootsCMGO' 
		detect: 'nombre' test: [:anArchivoName | anArchivoName = 'General'] orCreate: 'General'.
	anArchivoGeneralRoot  isNil ifTrue: [ ^self].

	aClinica := anArchivoGeneralRoot metaInfo getObject: anArchivoGeneralRoot featureNamedValue: 'clinica' orCreate: (Array with: (Array with: 'cif' with: 'A-11.222.333' copy) with: (Array with: 'razonSocial' with: 'Razon Social de la Clinica' copy) with: (Array with: 'nombreComercial' with: 'Nombre Comercial de la Clinica' copy)).

	^aClinica!

currentMessages
	"self  currentInfo browsePath"
	"self  resetCurrentInfos"
	"currentInfos"
	"(self  currentInfoStoreMethodSelector: self defaultCurrentInfoSelector) browsePath"
	"self  resetCurrentInfoStoreMethodSelector: self defaultCurrentInfoSelector"! !

!KRSimpleInfoHolder class publicMethodsFor: 'default'!

defaultCurrentInfoSelector
	"self  defaultCurrentInfoSelector "

	^#kronoSimpleInfoStore! !

!KRSimpleInfoHolder class publicMethodsFor: 'info creation'!

kronoSimpleInfoStore
	"KRSimpleInfoHolder  kronoSimpleInfoStore "

	| aModel aDomainCMGO aHomeArchivo anArchivoGeneralRoot  |

	aModel := KRSimpleMetaInfoHolder   currentModelStoreMethodSelector: #kronoSimpleStore. 
	aModel isNil ifTrue: [ ^nil].

	aDomainCMGO := aModel createDomainObject.
	aDomainCMGO isNil ifTrue: [ ^nil].

	aDomainCMGO metaInfo setObject: aDomainCMGO 
		featureNamed: aDomainCMGO metaInfo class domainNameCMGODomainAttributeName
		value:  'FerSalud' copy.

	aHomeArchivo := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Archivo'].
	aHomeArchivo isNil ifTrue: [ ^aDomainCMGO].


	anArchivoGeneralRoot := aHomeArchivo metaInfo getObject: aHomeArchivo featureNamedValue: 'homeRootsCMGO' 
		detect: 'nombre' test: [:anArchivoName | anArchivoName = 'General'] orCreate: 'General'.
	anArchivoGeneralRoot  isNil ifTrue: [ ^aDomainCMGO].

	self kronoSimpleInfoStoreCreateRootClinicaDataOnArchivo: anArchivoGeneralRoot.
	self getCreateSomeInfoInNewProjectDomainParameterValue ifTrue: [ 
		self kronoSimpleInfoStoreCreateSomeFacturacionAlquilerDataOnArchivo: anArchivoGeneralRoot.
		self kronoSimpleInfoStoreCreateSomePersonalDataOnArchivo: anArchivoGeneralRoot.
		self kronoSimpleInfoStoreCreateSomeCatalogosYTarifasDataOnArchivo: anArchivoGeneralRoot.
		self kronoSimpleInfoStoreCreateSomeOriginesYAseguradorasDataOnArchivo: anArchivoGeneralRoot.
		self kronoSimpleInfoStoreCreateSomeHistoriasDataOnArchivo: anArchivoGeneralRoot.
		self kronoSimpleInfoStoreCreateSomeFacturacionClinicaDataOnArchivo: anArchivoGeneralRoot.
	].

	self loadAndBindCodificacionesOnArchivo: anArchivoGeneralRoot.

	 ^aDomainCMGO!

kronoSimpleInfoStoreCreateRootClinicaDataOnArchivo: theArchivoGeneral

	| aClinica aDireccion aTelefonoUno aTelefonoDos aFax |

	aClinica := theArchivoGeneral metaInfo getObject: theArchivoGeneral featureNamedValue: 'clinica' orCreate: (Array with: (Array with: 'cif' with: 'B-123.456' copy) with: (Array with: 'razonSocial' with: 'Policlinica FerSalud' copy) with: (Array with: 'nombreComercial' with: 'FerSalud' copy)).

	aDireccion := aClinica metaInfo addToObject: aClinica featureNamedValue: 'direcciones' create: (Array 
		with: (Array with: 'calle' with: 'Ciudad de Barcelona' copy) 
		with: (Array with: 'numeroEnCalle' with: '29' copy) 
		with: (Array with: 'apartamento' with: 'bajo' copy)
		with: (Array with: 'distritoPostal' with: '46700' copy)
		with: (Array with: 'poblacion' with: 'Gandia' copy)
		with: (Array with: 'provincia' with: 'Valencia' copy)
		with: (Array with: 'pais' with: 'Planeta Tierra' copy)
		with: (Array with: 'fechaInicioValidez' with: SpanishDate today)
		with: (Array with: 'modoUso' with: (((aClinica metaInfo model typeNamed: 'TiposCompuestos::Direccion')
			effectiveFeatureNamed: 'modoUso')  
			referencedType createEnumerationObjectFromLiteral: 'Factura' ))).
	aClinica metaInfo setObject: aClinica featureNamed: 'direccionOficial' value: aDireccion.


	aTelefonoUno := aClinica metaInfo addToObject: aClinica featureNamedValue: 'numerosTelecomunicacion' create: (Array 
		with: (Array with: 'numero' with: '96 287 2649' copy) 
		with: (Array with: 'tipoNumeroTelecomunicacion' with: (((aClinica metaInfo effectiveFeatureNamed: 'numerosTelecomunicacion') 
			referencedType effectiveFeatureNamed: 'tipoNumeroTelecomunicacion')  
			referencedType createEnumerationObjectFromLiteral: 'TelefonoDirecto' copy))
		with: (Array with: 'fechaInicioValidez' with: SpanishDate today)
		with: (Array with: 'modoUso' with: (((aClinica metaInfo effectiveFeatureNamed: 'numerosTelecomunicacion') 
			referencedType effectiveFeatureNamed: 'modoUso')  
			referencedType createEnumerationObjectFromLiteral: 'Primario' copy))).
	aClinica metaInfo setObject: aClinica featureNamed: 'numeroTelecomunicacionTelefonoOficialUno' value: aTelefonoUno.
	aTelefonoDos := aClinica metaInfo addToObject: aClinica featureNamedValue: 'numerosTelecomunicacion' create: (Array 
		with: (Array with: 'numero' with: '96 287 5326' copy) 
		with: (Array with: 'tipoNumeroTelecomunicacion' with: (((aClinica metaInfo effectiveFeatureNamed: 'numerosTelecomunicacion') 
			referencedType effectiveFeatureNamed: 'tipoNumeroTelecomunicacion')  
			referencedType createEnumerationObjectFromLiteral: 'TelefonoDirecto' copy))
			with: (Array with: 'fechaInicioValidez' with: SpanishDate today)
		with: (Array with: 'modoUso' with: (((aClinica metaInfo effectiveFeatureNamed: 'numerosTelecomunicacion') 
			referencedType effectiveFeatureNamed: 'modoUso')  
			referencedType createEnumerationObjectFromLiteral: 'Secundario' copy))).
	aClinica metaInfo setObject: aClinica featureNamed: 'numeroTelecomunicacionTelefonoOficialDos' value: aTelefonoDos.
	aFax := aClinica metaInfo addToObject: aClinica featureNamedValue: 'numerosTelecomunicacion' create: (Array 
		with: (Array with: 'numero' with: '96 295 0046' copy) 
		with: (Array with: 'tipoNumeroTelecomunicacion' with: (((aClinica metaInfo effectiveFeatureNamed: 'numerosTelecomunicacion') 
			referencedType effectiveFeatureNamed: 'tipoNumeroTelecomunicacion')  
			referencedType createEnumerationObjectFromLiteral: 'Fax' copy))
			with: (Array with: 'fechaInicioValidez' with: SpanishDate today)
		with: (Array with: 'modoUso' with: (((aClinica metaInfo effectiveFeatureNamed: 'numerosTelecomunicacion') 
			referencedType effectiveFeatureNamed: 'modoUso')  
			referencedType createEnumerationObjectFromLiteral: 'Primario' copy))).
	aClinica metaInfo setObject: aClinica featureNamed: 'numeroTelecomunicacionFaxOficial' value: aFax.

	 ^theArchivoGeneral!

kronoSimpleInfoStoreCreateSomeCatalogosYTarifasDataOnArchivo: theArchivoGeneral

	| aFacturacionClinica unCatalogoClinicaParticulares unCatalogoClinicaEmpresas unCatalogoClinicaAseguradoras unaTarifaClinicaParticulares unCatalogoClinicaGeneral unaTarifaClinicaGeneral unConceptoClinica unPrecioTarifaClinica unaTarifaClinicaEmpresas unaTarifaClinicaAseguradoras |

	aFacturacionClinica := theArchivoGeneral metaInfo getObject: theArchivoGeneral featureNamedValue: 'facturacionClinica'.
	aFacturacionClinica isNil ifTrue: [ ^theArchivoGeneral].

	unCatalogoClinicaGeneral := aFacturacionClinica metaInfo addToObject: aFacturacionClinica featureNamedValue: 'catalogosConceptosClinica'
		factoryNamed: 'Tarifas::CatalogoConceptosClinicaGeneral' create: (Array 
		with: #( 'nombreCatalogo' 'Servicios Clinicos Generales' ) ).
	unaTarifaClinicaGeneral := unCatalogoClinicaGeneral metaInfo addToObject: unCatalogoClinicaGeneral featureNamedValue: 'tarifasClinica' 
		create: (Array 
		with: #( 'nombreTarifa' 'Tarifa de Servicios Clinicos Generales' )).
	unCatalogoClinicaGeneral metaInfo setObject: unCatalogoClinicaGeneral featureNamed: 'tarifaActualClinica' value: unaTarifaClinicaGeneral.

	unConceptoClinica := unCatalogoClinicaGeneral metaInfo 
		addToObject: unCatalogoClinicaGeneral featureNamedValue: 'conceptos' create: (Array 
		with: #( 'nombreConcepto' 'Visita Generalista' ) with: #( 'nombreUnidadFacturable' 'Visita' ) 
		with: (Array with: 'descripcionConcepto' with: 'Visita a Medico Generalista con duracion convencional' asText)).
	unPrecioTarifaClinica := unaTarifaClinicaGeneral metaInfo addToObject: unaTarifaClinicaGeneral featureNamedValue: 'preciosTarifaClinica' 
		create: (Array 
			with: ( Array with: 'nombrePrecio' with:  (unConceptoClinica metaInfo getObject: unConceptoClinica featureNamedValue: 'nombreConcepto' ))
			with: ( Array with: 'valorPrecio' with:  30)).
	unPrecioTarifaClinica metaInfo setObject: unPrecioTarifaClinica featureNamed: 'conceptoClinica' value: unConceptoClinica.

	unConceptoClinica := unCatalogoClinicaGeneral metaInfo 
		addToObject: unCatalogoClinicaGeneral featureNamedValue:  'conceptos' create: (Array  
		with: #( 'nombreConcepto' 'Visita Especialista' ) with: #( 'nombreUnidadFacturable' 'Visita' ) 
		with: (Array with: 'descripcionConcepto' with:  'Visita a Medico Especialista con duracion convencional' asText)).
	unPrecioTarifaClinica := unaTarifaClinicaGeneral metaInfo addToObject: unaTarifaClinicaGeneral featureNamedValue: 'preciosTarifaClinica' 
		create: (Array 
			with: ( Array with: 'nombrePrecio' with:  (unConceptoClinica metaInfo getObject: unConceptoClinica featureNamedValue: 'nombreConcepto' ))
			with: ( Array with: 'valorPrecio' with:  50)).
	unPrecioTarifaClinica metaInfo setObject: unPrecioTarifaClinica featureNamed: 'conceptoClinica' value: unConceptoClinica.

	unConceptoClinica := unCatalogoClinicaGeneral metaInfo 
		addToObject: unCatalogoClinicaGeneral featureNamedValue:  'conceptos' create: (Array  
		with: #( 'nombreConcepto' 'Rx Muy Grande' ) with: #( 'nombreUnidadFacturable' 'Placa' ) 
		with: (Array with: 'descripcionConcepto' with:  'Placa exposicion radiologica de muy grandes dimensiones' asText)).
	unPrecioTarifaClinica := unaTarifaClinicaGeneral metaInfo addToObject: unaTarifaClinicaGeneral featureNamedValue: 'preciosTarifaClinica' 
		create: (Array 
			with: ( Array with: 'nombrePrecio' with:  (unConceptoClinica metaInfo getObject: unConceptoClinica featureNamedValue: 'nombreConcepto' ))
			with: ( Array with: 'valorPrecio' with: 36)).
	unPrecioTarifaClinica metaInfo setObject: unPrecioTarifaClinica featureNamed: 'conceptoClinica' value: unConceptoClinica.

	unConceptoClinica := unCatalogoClinicaGeneral metaInfo 
		addToObject: unCatalogoClinicaGeneral featureNamedValue:  'conceptos' create: (Array  
		with: #( 'nombreConcepto' 'Rx Grande' ) with: #( 'nombreUnidadFacturable' 'Placa' ) 
		with: (Array with: 'descripcionConcepto' with:  'Placa exposicion radiologica de grandes dimensiones' asText)).
	unPrecioTarifaClinica := unaTarifaClinicaGeneral metaInfo addToObject: unaTarifaClinicaGeneral featureNamedValue: 'preciosTarifaClinica' 
		create: (Array 
			with: ( Array with: 'nombrePrecio' with:  (unConceptoClinica metaInfo getObject: unConceptoClinica featureNamedValue: 'nombreConcepto' ))
			with: ( Array with: 'valorPrecio' with:  36)).
	unPrecioTarifaClinica metaInfo setObject: unPrecioTarifaClinica featureNamed: 'conceptoClinica' value: unConceptoClinica.

	unConceptoClinica := unCatalogoClinicaGeneral metaInfo 
		addToObject: unCatalogoClinicaGeneral featureNamedValue:  'conceptos' create: (Array  
		with: #( 'nombreConcepto' 'Rx Mediana' ) with: #( 'nombreUnidadFacturable' 'Placa' ) 
		with: (Array with: 'descripcionConcepto' with:  'Placa exposicion radiologica de medias dimensiones' asText)).
	unPrecioTarifaClinica := unaTarifaClinicaGeneral metaInfo addToObject: unaTarifaClinicaGeneral featureNamedValue: 'preciosTarifaClinica' 
		create: (Array 
			with: ( Array with: 'nombrePrecio' with:  (unConceptoClinica metaInfo getObject: unConceptoClinica featureNamedValue: 'nombreConcepto' ))
			with: ( Array with: 'valorPrecio' with:  36)).
	unPrecioTarifaClinica metaInfo setObject: unPrecioTarifaClinica featureNamed: 'conceptoClinica' value: unConceptoClinica.

	unConceptoClinica := unCatalogoClinicaGeneral metaInfo 
		addToObject: unCatalogoClinicaGeneral featureNamedValue:  'conceptos' create: (Array  
		with: #( 'nombreConcepto' 'Rx Reducida') with: #( 'nombreUnidadFacturable' 'Placa' )
		with: (Array with: 'descripcionConcepto' with:  'Placa exposicion radiologica de reducidas dimensiones' asText)).
	unPrecioTarifaClinica := unaTarifaClinicaGeneral metaInfo addToObject: unaTarifaClinicaGeneral featureNamedValue: 'preciosTarifaClinica' 
		create: (Array 
			with: ( Array with: 'nombrePrecio' with:  (unConceptoClinica metaInfo getObject: unConceptoClinica featureNamedValue: 'nombreConcepto' ))
			with: ( Array with: 'valorPrecio' with:  30)).
	unPrecioTarifaClinica metaInfo setObject: unPrecioTarifaClinica featureNamed: 'conceptoClinica' value: unConceptoClinica.

	unConceptoClinica := unCatalogoClinicaGeneral metaInfo 
		addToObject: unCatalogoClinicaGeneral featureNamedValue:  'conceptos' create: (Array  
		with: #( 'nombreConcepto' 'Rx Muy reducida' ) with: #( 'nombreUnidadFacturable' 'Placa' ) 
		with: (Array with: 'descripcionConcepto' with:  'Placa exposicion radiologica de muy reducidas dimensiones' asText)).
	unPrecioTarifaClinica := unaTarifaClinicaGeneral metaInfo addToObject: unaTarifaClinicaGeneral featureNamedValue: 'preciosTarifaClinica' 
		create: (Array 
			with: ( Array with: 'nombrePrecio' with:  (unConceptoClinica metaInfo getObject: unConceptoClinica featureNamedValue: 'nombreConcepto' ))
			with: ( Array with: 'valorPrecio' with:  30)).
	unPrecioTarifaClinica metaInfo setObject: unPrecioTarifaClinica featureNamed: 'conceptoClinica' value: unConceptoClinica.


	unConceptoClinica := unCatalogoClinicaGeneral metaInfo 
		addToObject: unCatalogoClinicaGeneral featureNamedValue:  'conceptos' create: (Array  
		with: #( 'nombreConcepto' 'Reconocimiento Medico' ) with: #( 'nombreUnidadFacturable' 'Reconocimiento' ) ).
	unPrecioTarifaClinica := unaTarifaClinicaGeneral metaInfo addToObject: unaTarifaClinicaGeneral featureNamedValue: 'preciosTarifaClinica' 
		create: (Array 
			with: ( Array with: 'nombrePrecio' with:  (unConceptoClinica metaInfo getObject: unConceptoClinica featureNamedValue: 'nombreConcepto' ))
			with: ( Array with: 'valorPrecio' with:  24)).
	unPrecioTarifaClinica metaInfo setObject: unPrecioTarifaClinica featureNamed: 'conceptoClinica' value: unConceptoClinica.

	unConceptoClinica := unCatalogoClinicaGeneral metaInfo 
		addToObject: unCatalogoClinicaGeneral featureNamedValue:  'conceptos' create: (Array  
		with: #( 'nombreConcepto' 'ECG' ) with: #( 'nombreUnidadFacturable' 'ECG' ) ).
	unPrecioTarifaClinica := unaTarifaClinicaGeneral metaInfo addToObject: unaTarifaClinicaGeneral featureNamedValue: 'preciosTarifaClinica' 
		create: (Array 
			with: ( Array with: 'nombrePrecio' with:  (unConceptoClinica metaInfo getObject: unConceptoClinica featureNamedValue: 'nombreConcepto' ))
			with: ( Array with: 'valorPrecio' with:  12)).
	unPrecioTarifaClinica metaInfo setObject: unPrecioTarifaClinica featureNamed: 'conceptoClinica' value: unConceptoClinica.

	unConceptoClinica := unCatalogoClinicaGeneral metaInfo 
		addToObject: unCatalogoClinicaGeneral featureNamedValue:  'conceptos' create: (Array  
		with: #( 'nombreConcepto' 'Rehabilitacion' ) with: #( 'nombreUnidadFacturable' 'Sesion' ) ).
	unPrecioTarifaClinica := unaTarifaClinicaGeneral metaInfo addToObject: unaTarifaClinicaGeneral featureNamedValue: 'preciosTarifaClinica' 
		create: (Array 
			with: ( Array with: 'nombrePrecio' with:  (unConceptoClinica metaInfo getObject: unConceptoClinica featureNamedValue: 'nombreConcepto' ))
			with: ( Array with: 'valorPrecio' with: 15)).
	unPrecioTarifaClinica metaInfo setObject: unPrecioTarifaClinica featureNamed: 'conceptoClinica' value: unConceptoClinica.

	unCatalogoClinicaParticulares := aFacturacionClinica metaInfo addToObject: aFacturacionClinica featureNamedValue: 'catalogosConceptosClinica'
		factoryNamed: 'Tarifas::CatalogoConceptosClinicaEspecifico' create: (Array 
		with: #( 'nombreCatalogo' 'Servicios Clinicos para Pacientes Particulares' ) ).
	unCatalogoClinicaParticulares metaInfo setObject: unCatalogoClinicaParticulares featureNamed: 'catalogoConceptosClinicaGeneral' value: 
		unCatalogoClinicaGeneral.
	unaTarifaClinicaParticulares := unCatalogoClinicaParticulares metaInfo addToObject: unCatalogoClinicaParticulares featureNamedValue: 'tarifasClinica' 
		create: (Array 
		with: #( 'nombreTarifa' 'Tarifa de Servicios Clinicos para Pacientes Particulares' )).
	unCatalogoClinicaParticulares metaInfo setObject: unCatalogoClinicaParticulares featureNamed: 'tarifaActualClinica' value: unaTarifaClinicaParticulares.

	(unaTarifaClinicaGeneral metaInfo getObject: unaTarifaClinicaGeneral featureNamedValue: 'preciosTarifaClinica') do: [:aPrecioTarifaClinica |
		| unNuevoPrecioTarifa |
		unNuevoPrecioTarifa := unaTarifaClinicaParticulares metaInfo addToObject: unaTarifaClinicaParticulares featureNamedValue: 'preciosTarifaClinica' 
			create: (Array 
				with: ( Array with: 'nombrePrecio' with:  (aPrecioTarifaClinica metaInfo getObject: aPrecioTarifaClinica featureNamedValue: 'nombrePrecio' ))
				with: ( Array with: 'valorPrecio' with: 2 *  (aPrecioTarifaClinica metaInfo getObject: aPrecioTarifaClinica featureNamedValue: 'valorPrecio' ))).
		unNuevoPrecioTarifa metaInfo setObject: unNuevoPrecioTarifa featureNamed: 'conceptoClinica' value: 
			(aPrecioTarifaClinica metaInfo getObject: aPrecioTarifaClinica featureNamedValue: 'conceptoClinica' ).
	].

	unCatalogoClinicaEmpresas := aFacturacionClinica metaInfo addToObject: aFacturacionClinica featureNamedValue: 'catalogosConceptosClinica' 		factoryNamed: 'Tarifas::CatalogoConceptosClinicaEspecifico' create: (Array 
		with: #( 'nombreCatalogo' 'Servicios Clinicos Generales para Empresas' )).
	unCatalogoClinicaEmpresas metaInfo setObject: unCatalogoClinicaEmpresas featureNamed: 'catalogoConceptosClinicaGeneral' value: 
		unCatalogoClinicaGeneral.
	unaTarifaClinicaEmpresas := unCatalogoClinicaEmpresas metaInfo addToObject: unCatalogoClinicaEmpresas featureNamedValue: 'tarifasClinica' 
		create: (Array 
		with: #( 'nombreTarifa' 'Tarifa de Servicios Clinicos para Pacientes de Empresa' )).
	unCatalogoClinicaEmpresas metaInfo setObject: unCatalogoClinicaEmpresas featureNamed: 'tarifaActualClinica' value: unaTarifaClinicaEmpresas.

	(unaTarifaClinicaGeneral metaInfo getObject: unaTarifaClinicaGeneral featureNamedValue: 'preciosTarifaClinica') do: [:aPrecioTarifaClinica |
		| unNuevoPrecioTarifa |
		unNuevoPrecioTarifa := unaTarifaClinicaEmpresas metaInfo addToObject: unaTarifaClinicaEmpresas featureNamedValue: 'preciosTarifaClinica' 
			create: (Array 
				with: ( Array with: 'nombrePrecio' with:  (aPrecioTarifaClinica metaInfo getObject: aPrecioTarifaClinica featureNamedValue: 'nombrePrecio' ))
				with: ( Array with: 'valorPrecio' with: 2 *  (aPrecioTarifaClinica metaInfo getObject: aPrecioTarifaClinica featureNamedValue: 'valorPrecio' ))).
		unNuevoPrecioTarifa metaInfo setObject: unNuevoPrecioTarifa featureNamed: 'conceptoClinica' value: 
			(aPrecioTarifaClinica metaInfo getObject: aPrecioTarifaClinica featureNamedValue: 'conceptoClinica' ).
	].

	unCatalogoClinicaAseguradoras := aFacturacionClinica metaInfo addToObject: aFacturacionClinica featureNamedValue: 'catalogosConceptosClinica'
		factoryNamed: 'Tarifas::CatalogoConceptosClinicaEspecifico' create: (Array 
		with: #( 'nombreCatalogo' 'Servicios Clinicos Generales para Aseguradoras sin Catalogo propio' ) ).
	unCatalogoClinicaAseguradoras metaInfo setObject: unCatalogoClinicaAseguradoras featureNamed: 'catalogoConceptosClinicaGeneral' value: 
		unCatalogoClinicaGeneral.
	unaTarifaClinicaAseguradoras := unCatalogoClinicaAseguradoras metaInfo addToObject: unCatalogoClinicaAseguradoras featureNamedValue: 'tarifasClinica' 
		create: (Array 
		with: #( 'nombreTarifa' 'Tarifa de Servicios Clinicos para Aseguradoras sin Catalogo propio' )).
	unCatalogoClinicaAseguradoras metaInfo setObject: unCatalogoClinicaAseguradoras featureNamed: 'tarifaActualClinica' value: unaTarifaClinicaGeneral.
	(unaTarifaClinicaGeneral metaInfo getObject: unaTarifaClinicaGeneral featureNamedValue: 'preciosTarifaClinica') do: [:aPrecioTarifaClinica |
		| unNuevoPrecioTarifa |
		unNuevoPrecioTarifa := unaTarifaClinicaAseguradoras metaInfo addToObject: unaTarifaClinicaAseguradoras featureNamedValue: 'preciosTarifaClinica' 
			create: (Array 
				with: ( Array with: 'nombrePrecio' with:  (aPrecioTarifaClinica metaInfo getObject: aPrecioTarifaClinica featureNamedValue: 'nombrePrecio' ))
				with: ( Array with: 'valorPrecio' with: 3 *  (aPrecioTarifaClinica metaInfo getObject: aPrecioTarifaClinica featureNamedValue: 'valorPrecio' ))).
		unNuevoPrecioTarifa metaInfo setObject: unNuevoPrecioTarifa featureNamed: 'conceptoClinica' value: 
			(aPrecioTarifaClinica metaInfo getObject: aPrecioTarifaClinica featureNamedValue: 'conceptoClinica' ).
	].

	 ^theArchivoGeneral!

kronoSimpleInfoStoreCreateSomeFacturacionAlquilerDataOnArchivo: theArchivoGeneral

	| aClinica aFacturacionAlquiler aClienteFacturacionAlquiler aDireccionClienteFacturacionAlquiler aCatalogoFacturacionAlquiler aConceptoFacturacionAlquiler aTarifaFacturacionAlquiler aPrecioTarifaFacturacionAlquiler aEjercicioFacturacionAlquiler aPeriodoFacturacionAlquiler aFacturaAlquiler aLineaFacturaAlquiler  |

	aClinica := theArchivoGeneral metaInfo getObject: theArchivoGeneral featureNamedValue: 'clinica' .
	aClinica isNil ifTrue: [ ^nil].

	aFacturacionAlquiler := theArchivoGeneral metaInfo getObject: theArchivoGeneral featureNamedValue: 'facturacionAlquiler'.
	aFacturacionAlquiler isNil ifTrue: [ ^theArchivoGeneral].

	aClienteFacturacionAlquiler := aFacturacionAlquiler metaInfo addToObject: aFacturacionAlquiler featureNamedValue: 'clientesFacturasAlquiler'
		factoryNamed: 'Facturacion::ClienteFacturaAlquilerPersonaFisica' create: (Array 
		with: (Array with: 'dninif' with: '22.671.935-F' copy) 
		with: (Array with: 'nombre' with: 'FCO JOSE' copy) 
		with: (Array with: 'apellidos' with: 'NOHALES ALFONSO' copy)
		with: (Array with: 'sexo' with: (((aFacturacionAlquiler metaInfo model typeNamed: 'Facturacion::ClienteFacturaAlquilerPersonaFisica')
			effectiveFeatureNamed: 'sexo')  
			referencedType createEnumerationObjectFromLiteral: 'Masculino' copy))).

	aDireccionClienteFacturacionAlquiler := aClienteFacturacionAlquiler metaInfo 
		addToObject: aClienteFacturacionAlquiler featureNamedValue: 'direcciones' create: (Array 
			with: (Array with: 'calle' with: 'Ciudad de Barcelona' copy) 
			with: (Array with: 'numeroEnCalle' with: '29' copy) 
			with: (Array with: 'apartamento' with: 'B' copy)
			with: (Array with: 'distritoPostal' with: '46700' copy)
			with: (Array with: 'poblacion' with: 'Gandia' copy)
			with: (Array with: 'provincia' with: 'Valencia' copy)
			with: (Array with: 'pais' with: 'Planeta Tierra' copy)
			with: (Array with: 'fechaInicioValidez' with: SpanishDate today)
		with: (Array with: 'modoUso' with: (((aClienteFacturacionAlquiler metaInfo model typeNamed: 'TiposCompuestos::Direccion')
			effectiveFeatureNamed: 'modoUso')  
			referencedType createEnumerationObjectFromLiteral: 'Factura' ))).
	aClienteFacturacionAlquiler metaInfo setObject: aClienteFacturacionAlquiler 
		featureNamed: 'direccionOficial' value: aDireccionClienteFacturacionAlquiler.

	aCatalogoFacturacionAlquiler := aFacturacionAlquiler metaInfo getObject: aFacturacionAlquiler featureNamedValue: 'catalogoConceptosAlquiler'.
	aCatalogoFacturacionAlquiler isNil ifTrue: [ ^theArchivoGeneral].

	aConceptoFacturacionAlquiler := aCatalogoFacturacionAlquiler metaInfo 
		addToObject: aCatalogoFacturacionAlquiler featureNamedValue: 'conceptosAlquiler' create: (Array 
			with: (Array with: 'nombreConcepto' with: 'Despacho Consultas' copy)
			with: (Array with: 'descripcionConcepto' with: 'Cuota Mensual por Alquiler de un Despacho para Consultas medicas' copy)
			with: (Array with: 'nombreUnidadFacturable' with: 'MES' copy)).

	aTarifaFacturacionAlquiler := aCatalogoFacturacionAlquiler metaInfo 
		addToObject: aCatalogoFacturacionAlquiler featureNamedValue: 'tarifasAlquiler' create: (Array 
			with: (Array with: 'nombreTarifa' with: 'Tarifa Conceptos de Facturacion Alquiler' copy)
			with: (Array with: 'fechaInicioValidez' with: SpanishDate today)).
	aTarifaFacturacionAlquiler metaInfo setObject: aTarifaFacturacionAlquiler 
		featureNamed: 'catalogoConceptosAlquiler' value: aCatalogoFacturacionAlquiler.

	aPrecioTarifaFacturacionAlquiler := aTarifaFacturacionAlquiler metaInfo 
		addToObject: aTarifaFacturacionAlquiler featureNamedValue: 'preciosTarifaAlquiler' create: (Array 
			with: (Array with: 'nombrePrecio' with: 'Despacho Consultas' copy)
			with: (Array with: 'valorPrecio' with: 500)).
	aPrecioTarifaFacturacionAlquiler metaInfo setObject: aPrecioTarifaFacturacionAlquiler 
		featureNamed: 'conceptoAlquiler' value: aConceptoFacturacionAlquiler.

	aCatalogoFacturacionAlquiler metaInfo setObject: aCatalogoFacturacionAlquiler 
		featureNamed: 'tarifaActualAlquiler' value: aTarifaFacturacionAlquiler.


	aEjercicioFacturacionAlquiler := aFacturacionAlquiler metaInfo addToObject: aFacturacionAlquiler featureNamedValue: 'ejerciciosFacturacion' 
		create: (Array with: (Array with: 'ejercicioFiscal' with: '2002' copy) ).
	aPeriodoFacturacionAlquiler := aEjercicioFacturacionAlquiler metaInfo addToObject: aEjercicioFacturacionAlquiler 
		featureNamedValue: 'periodosFacturacion' 
		create: (Array with: (Array with: 'periodo' with: '1' copy) ).

	aFacturaAlquiler := aPeriodoFacturacionAlquiler metaInfo addToObject: aPeriodoFacturacionAlquiler 
		featureNamedValue: 'facturas' 
		create: (Array 
			with: (Array with: 'numeroFactura' with: 'A/0016/01' copy)
			with: (Array with: 'fechaFactura' with: SpanishDate today)
		).
	aFacturaAlquiler metaInfo setObject: aFacturaAlquiler 
		featureNamed: 'clienteFactura' value: aClienteFacturacionAlquiler.
	aFacturaAlquiler metaInfo setObject: aFacturaAlquiler 
		featureNamed: 'clienteFactura' value: aClienteFacturacionAlquiler.

	aLineaFacturaAlquiler := aFacturaAlquiler metaInfo addToObject: aFacturaAlquiler 
		featureNamedValue: 'lineasFactura' 
		create: (Array 
			with: (Array with: 'numeroUnidades' with: 1 copy)
			with: (Array with: 'precioUnitario' with: 500 copy)
			with: (Array with: 'concepto' with: 'Cuota Mensual por Alquiler de un Despacho para Consultas medicas' copy)
		).
	aLineaFacturaAlquiler metaInfo setObject: aLineaFacturaAlquiler 
		featureNamed: 'precioTarifaAlquiler' value: aPrecioTarifaFacturacionAlquiler.


	 ^theArchivoGeneral!

kronoSimpleInfoStoreCreateSomeFacturacionClinicaDataOnArchivo: theArchivoGeneral

	| aClinica aFacturacionClinica aEjercicioFacturacionClinica |

	aClinica := theArchivoGeneral metaInfo getObject: theArchivoGeneral featureNamedValue: 'clinica' .
	aClinica isNil ifTrue: [ ^nil].

	aFacturacionClinica := theArchivoGeneral metaInfo getObject: theArchivoGeneral featureNamedValue: 'facturacionClinica'.
	aFacturacionClinica isNil ifTrue: [ ^theArchivoGeneral].

	aEjercicioFacturacionClinica := aFacturacionClinica metaInfo addToObject: aFacturacionClinica featureNamedValue: 'ejerciciosFacturacion' 
		create: (Array with: (Array with: 'ejercicioFiscal' with: '2002' copy) ).
	aEjercicioFacturacionClinica metaInfo addToObject: aEjercicioFacturacionClinica 
		featureNamedValue: 'periodosFacturacion' 
		create: (Array with: (Array with: 'periodo' with: '1' copy) ).



	 ^theArchivoGeneral!

kronoSimpleInfoStoreCreateSomeHistoriasDataOnArchivo: theArchivoGeneral


	|  aHistoria aPaciente aDireccionPaciente aTelefonoUno |

	aHistoria := theArchivoGeneral metaInfo addToObject: theArchivoGeneral featureNamedValue: 'historias'
		create: (Array 
		with: #( 'codigoHistoria' '55.555.555-E' ) 
		with: #( 'numeroHistoria' '1234' ) 
		with: (Array with: 'fechaApertura' with: SpanishDate today)
		with: (Array with: 'comentario' with: (Text string: 'Historia clinica' emphasis: #serif) )
		with: (Array with: 'tipoHistoria' with: (((theArchivoGeneral metaInfo model typeNamed: 'Nucleo::Historia')
			effectiveFeatureNamed: 'tipoHistoria')  
			referencedType createEnumerationObjectFromLiteral: 'Actual' ))).

	aPaciente := aHistoria metaInfo addToObject: aHistoria featureNamedValue: 'paciente' create: (Array 
		with: #( 'dninif'  '55.555.555-E' ) 
		with: #( 'nombre' 'NOMBRE del PACIENTE' ) 
		with: #( 'apellidos' 'APELLIDOS DEL PACIENTE' )
		with: (Array with: 'fechaNacimiento' with: (SpanishDate newDay: 10 monthNumber: 10 year: 1955))
		with: #( 'poblacionNacimiento' 'Valencia')
		with: #( 'provinciaNacimiento' 'Valencia')
		with: #( 'paisNacimiento' 'Planeta Tierra')
		with: (Array with: 'sexo' with: (((aHistoria metaInfo model typeNamed: 'Nucleo::Paciente')
			effectiveFeatureNamed: 'sexo')  
			referencedType createEnumerationObjectFromLiteral: 'Masculino' ))).

	aDireccionPaciente := aPaciente metaInfo 
		addToObject: aPaciente featureNamedValue: 'direcciones' create: (Array 
			with: #( 'calle' 'CALLE DEL PACIENTE' ) 
			with: #( 'numeroEnCalle' '16' ) 
			with: #( 'apartamento' '5' )
			with: #( 'distritoPostal' '46700' )
			with: #( 'poblacion' 'Gandia' )
			with: #( 'provincia' 'Valencia' )
			with: #( 'pais' 'Planeta Tierra' )
			with: (Array with: 'fechaInicioValidez' with: SpanishDate today)
			with: (Array with: 'modoUso' with: ((aPaciente metaInfo model typeNamed: 'TiposCompuestos::ModoUsoDireccion') 
				createEnumerationObjectFromLiteral: 'Oficial' ))).

	aTelefonoUno := aPaciente metaInfo addToObject: aPaciente featureNamedValue: 'numerosTelecomunicacion' create: (Array 
		with: (Array with: 'numero' with: '96 286 1234' copy) 
		with: (Array with: 'tipoNumeroTelecomunicacion' with: (((aPaciente metaInfo effectiveFeatureNamed: 'numerosTelecomunicacion') 
			referencedType effectiveFeatureNamed: 'tipoNumeroTelecomunicacion')  
			referencedType createEnumerationObjectFromLiteral: 'TelefonoDirecto' copy))
		with: (Array with: 'fechaInicioValidez' with: SpanishDate today)
		with: (Array with: 'modoUso' with: (((aPaciente metaInfo effectiveFeatureNamed: 'numerosTelecomunicacion') 
			referencedType effectiveFeatureNamed: 'modoUso')  
			referencedType createEnumerationObjectFromLiteral: 'Primario' copy))).


	 ^theArchivoGeneral!

kronoSimpleInfoStoreCreateSomeOriginesYAseguradorasDataOnArchivo: theArchivoGeneral


	|  aAseguradora aDireccionAseguradora aEmpresa aDireccionEmpresa |


	aAseguradora := theArchivoGeneral metaInfo addToObject: theArchivoGeneral featureNamedValue: 'organizacionesPagadorasPrestaciones'
		factoryNamed: 'Organizaciones::Aseguradora' create: (Array 
		with: #( 'cif' 'A-4.555.999' ) 
		with: #( 'razonSocial' 'Aseguradora Riesgo Zero' ) 
		with: #( 'nombreComercial' 'R.Z.' )).

	aDireccionAseguradora := aAseguradora metaInfo 
		addToObject: aAseguradora featureNamedValue: 'direcciones' create: (Array 
			with: #( 'calle' 'Calle El Tropezon' ) 
			with: #( 'numeroEnCalle' '13' ) 
			with: #( 'apartamento' '13' )
			with: #( 'distritoPostal' '46013' )
			with: #( 'poblacion' 'Valencia' )
			with: #( 'provincia' 'Valencia' )
			with: #( 'pais' 'Planeta Tierra' )
			with: (Array with: 'fechaInicioValidez' with: SpanishDate today)
			with: (Array with: 'modoUso' with: ((aAseguradora metaInfo model typeNamed: 'TiposCompuestos::ModoUsoDireccion')
				createEnumerationObjectFromLiteral: 'Factura' ))).



	aEmpresa := theArchivoGeneral metaInfo addToObject: theArchivoGeneral featureNamedValue: 'organizacionesOrigenPacientes'
		factoryNamed: 'Organizaciones::Empresa' create: (Array 
			with: #( 'cif' 'A-4.555.999' ) 
			with: #( 'razonSocial' 'Empresa Industrial' ) 
			with: #( 'nombreComercial' 'EmIn' )).

	aDireccionEmpresa := aEmpresa metaInfo 
		addToObject: aEmpresa featureNamedValue: 'direcciones' create: (Array 
			with: #( 'calle' 'Calle Industria' ) 
			with: #( 'numeroEnCalle' '211' ) 
			with: #( 'apartamento' 'bajo' )
			with: #( 'distritoPostal' '46700' )
			with: #( 'poblacion' 'Gandia' )
			with: #( 'provincia' 'Valencia' )
			with: #( 'pais' 'Planeta Tierra' )
			with: (Array with: 'fechaInicioValidez' with: SpanishDate today)
			with: (Array with: 'modoUso' with: ((aEmpresa metaInfo model typeNamed: 'TiposCompuestos::ModoUsoDireccion')
				createEnumerationObjectFromLiteral: 'Factura' ))).

	 ^theArchivoGeneral!

kronoSimpleInfoStoreCreateSomePersonalDataOnArchivo: theArchivoGeneral


	| aClinica  aDoctor aDireccionDoctor aRecepcionista aDireccionRecepcionista aDepartamentoDireccion aTelefonoUnoDepartamentoDireccion aDirector aDireccionDirector aDepartamentoConsultasExternas aTelefonoUnoDepartamentoConsultasExternas aDepartamentoAdmision aTelefonoUnoDepartamentoAdmision aDireccionDepartamentoDireccion aDireccionDepartamentoConsultasExternas aDireccionDepartamentoAdmision aPuestoDirector aPuestoDoctor aJefeAdmision aDireccionJefeAdmision aPuestoJefeAdmision  aPuestoRecepcionista |

	aClinica := theArchivoGeneral metaInfo getObject: theArchivoGeneral featureNamedValue: 'clinica'.
	aClinica isNil ifTrue: [ ^self].

	aDirector := aClinica metaInfo addToObject: aClinica featureNamedValue: 'personalClinica'
		factoryNamed: 'Nucleo::Doctor' create: (Array 
		with: #( 'dninif' '11.111.111-A' ) 
		with: #( 'nombre' 'SALVADOR' ) 
		with: #( 'apellidos' 'BUCHO SASTRE' )
		with: (Array with: 'sexo' with: (((aClinica metaInfo model typeNamed: 'Nucleo::Doctor')
			effectiveFeatureNamed: 'sexo')  
			referencedType createEnumerationObjectFromLiteral: 'Masculino' ))).

	aDireccionDirector := aDirector metaInfo 
		addToObject: aDirector featureNamedValue: 'direcciones' create: (Array 
			with: #( 'calle' 'Ciudad de Barcelona' ) 
			with: #( 'numeroEnCalle' '29' ) 
			with: #( 'apartamento' 'B' )
			with: #( 'distritoPostal' '46700' )
			with: #( 'poblacion' 'Gandia' )
			with: #( 'provincia' 'Valencia' )
			with: #( 'pais' 'Planeta Tierra' )
			with: (Array with: 'fechaInicioValidez' with: SpanishDate today)
			with: (Array with: 'modoUso' with: ((aClinica metaInfo model typeNamed: 'TiposCompuestos::ModoUsoDireccion')
				createEnumerationObjectFromLiteral: 'Oficial' ))).

	aDepartamentoDireccion := aClinica metaInfo addToObject: aClinica featureNamedValue: 'departamentos'
		factoryNamed: 'Nucleo::DepartamentoClinica' create: (Array 
		with: #( 'nombreDepartamento' 'Direccion de PoliClinica FerSalud' ) 
		with: (Array with: 'tipoDepartamento' with: (((aClinica metaInfo model typeNamed: 'Nucleo::DepartamentoClinica')
			effectiveFeatureNamed: 'tipoDepartamento')  
			referencedType createEnumerationObjectFromLiteral: 'DireccionClinica' ))).
	aDireccionDepartamentoDireccion := aDepartamentoDireccion metaInfo 
		addToObject: aDepartamentoDireccion featureNamedValue: 'direcciones' create: (Array 
			with: #( 'calle' 'Ciudad de Barcelona' ) 
			with: #( 'numeroEnCalle' '29' ) 
			with: #( 'apartamento' 'B' )
			with: #( 'distritoPostal' '46700' )
			with: #( 'poblacion' 'Gandia' )
			with: #( 'provincia' 'Valencia' )
			with: #( 'pais' 'Planeta Tierra' )
			with: #( 'fechaInicioValidez' with: SpanishDate today)
			with: (Array with: 'modoUso' with: ((aClinica metaInfo model typeNamed: 'TiposCompuestos::ModoUsoDireccion')
				createEnumerationObjectFromLiteral: 'Oficial' ))).

	aTelefonoUnoDepartamentoDireccion := aDepartamentoDireccion metaInfo addToObject: aDepartamentoDireccion 
		featureNamedValue: 'numerosTelecomunicacion' create: (Array 
		with: #( 'numero' '96 287 2650' ) 
		with: (Array with: 'tipoNumeroTelecomunicacion' with: (((aDepartamentoDireccion metaInfo effectiveFeatureNamed: 'numerosTelecomunicacion') 
			referencedType effectiveFeatureNamed: 'tipoNumeroTelecomunicacion')  
			referencedType createEnumerationObjectFromLiteral: 'TelefonoDirecto' ))
		with: (Array with: 'fechaInicioValidez' with: SpanishDate today)
		with: (Array with: 'modoUso' with: (((aDepartamentoDireccion metaInfo effectiveFeatureNamed: 'numerosTelecomunicacion') 
			referencedType effectiveFeatureNamed: 'modoUso')  
			referencedType createEnumerationObjectFromLiteral: 'Primario' ))).

	aPuestoDirector := aDepartamentoDireccion metaInfo addToObject: aDepartamentoDireccion featureNamedValue: 'puestosClinica'
		create: (Array 
		with: #( 'nombrePuestoClinica' 'Director General' ) 
		with: (Array with: 'fechaInicioValidez' with: SpanishDate today)
		with: (Array with: 'tipoPuestoClinica' with: (((aClinica metaInfo model typeNamed: 'TiposRelacionales::PuestoClinica')
			effectiveFeatureNamed: 'tipoPuestoClinica')  
			referencedType createEnumerationObjectFromLiteral: 'DirectorClinica' ))).
	aPuestoDirector metaInfo setObject: aPuestoDirector  featureNamed: 'personaClinica' value: aDirector.

	aDoctor := aClinica metaInfo addToObject: aClinica featureNamedValue: 'personalClinica'
		factoryNamed: 'Nucleo::Doctor' create: (Array 
		with: #( 'dninif' '22.671.935-F' ) 
		with: #( 'nombre' 'FCO JOSE' ) 
		with: #( 'apellidos' 'NOHALES ALFONSO' )
		with: (Array with: 'sexo' with: (((aClinica metaInfo model typeNamed: 'Nucleo::Doctor')
			effectiveFeatureNamed: 'sexo')  
			referencedType createEnumerationObjectFromLiteral: 'Masculino' ))).

	aDireccionDoctor := aDoctor metaInfo 
		addToObject: aDoctor featureNamedValue: 'direcciones' create: (Array 
			with: #( 'calle' 'Ciudad de Barcelona' ) 
			with: #( 'numeroEnCalle' '29' ) 
			with: #( 'apartamento' 'B' )
			with: #( 'distritoPostal' '46700' )
			with: #( 'poblacion' 'Gandia' )
			with: #( 'provincia' 'Valencia' )
			with: #( 'pais' 'Planeta Tierra' )
			with: #( 'fechaInicioValidez' with: SpanishDate today)
			with: (Array with: 'modoUso' with: ((aClinica metaInfo model typeNamed: 'TiposCompuestos::ModoUsoDireccion')
				createEnumerationObjectFromLiteral: 'Oficial' ))).


	aDepartamentoConsultasExternas := aClinica metaInfo addToObject: aClinica featureNamedValue: 'departamentos'
		factoryNamed: 'Nucleo::DepartamentoClinica' create: (Array 
		with: #( 'nombreDepartamento' 'ConsultasExternas' ) 
		with: (Array with: 'tipoDepartamento' with: (((aClinica metaInfo model typeNamed: 'Nucleo::DepartamentoClinica')
			effectiveFeatureNamed: 'tipoDepartamento')  
			referencedType createEnumerationObjectFromLiteral: 'ServicioClinica' ))).

	aTelefonoUnoDepartamentoConsultasExternas := aDepartamentoConsultasExternas metaInfo addToObject: aDepartamentoConsultasExternas 
		featureNamedValue: 'numerosTelecomunicacion' create: (Array 
		with: #( 'numero' '96 287 2651' ) 
		with: (Array with: 'tipoNumeroTelecomunicacion' with: (((aDepartamentoConsultasExternas metaInfo effectiveFeatureNamed: 'numerosTelecomunicacion') 
			referencedType effectiveFeatureNamed: 'tipoNumeroTelecomunicacion')  
			referencedType createEnumerationObjectFromLiteral: 'TelefonoDirecto' ))
		with: (Array with: 'fechaInicioValidez' with: SpanishDate today)
		with: (Array with: 'modoUso' with: (((aDepartamentoConsultasExternas metaInfo effectiveFeatureNamed: 'numerosTelecomunicacion') 
			referencedType effectiveFeatureNamed: 'modoUso')  
			referencedType createEnumerationObjectFromLiteral: 'Primario' ))).

	aDireccionDepartamentoConsultasExternas := aDepartamentoConsultasExternas metaInfo 
		addToObject: aDepartamentoConsultasExternas featureNamedValue: 'direcciones' create: (Array 
			with: #( 'calle' 'Ciudad de Barcelona' ) 
			with: #( 'numeroEnCalle' '29' ) 
			with: #( 'apartamento' 'B' )
			with: #( 'distritoPostal' '46700' )
			with: #( 'poblacion' 'Gandia' )
			with: #( 'provincia' 'Valencia' )
			with: #( 'pais' 'Planeta Tierra' )
			with: #( 'fechaInicioValidez' with: SpanishDate today)
			with: (Array with: 'modoUso' with: ((aClinica metaInfo model typeNamed: 'TiposCompuestos::ModoUsoDireccion')
				createEnumerationObjectFromLiteral: 'Oficial' ))).

	aPuestoDoctor := aDepartamentoConsultasExternas metaInfo addToObject: aDepartamentoConsultasExternas featureNamedValue: 'puestosClinica'
		create: (Array 
		with: #( 'nombrePuestoClinica' 'Especialista Medicina Interna' ) 
		with: (Array with: 'fechaInicioValidez' with: SpanishDate today)
		with: (Array with: 'tipoPuestoClinica' with: (((aClinica metaInfo model typeNamed: 'TiposRelacionales::PuestoClinica')
			effectiveFeatureNamed: 'tipoPuestoClinica')  
			referencedType createEnumerationObjectFromLiteral: 'DoctorTitular' ))).
	aPuestoDoctor metaInfo setObject: aPuestoDoctor  featureNamed: 'personaClinica' value: aDoctor.


	aJefeAdmision := aClinica metaInfo addToObject: aClinica featureNamedValue: 'personalClinica'
		factoryNamed: 'Nucleo::Administrativo' create: (Array 
		with: #( 'dninif' '9.876.543-Z' ) 
		with: #( 'nombre' 'NOMBRE del JEFE de ADMISION' ) 
		with: #( 'apellidos' 'APELLIDOS del JEFE de ADMISION' )
		with: (Array with: 'sexo' with: (((aClinica metaInfo model typeNamed: 'Nucleo::Administrativo')
			effectiveFeatureNamed: 'sexo')  
			referencedType createEnumerationObjectFromLiteral: 'Femenino' ))).

	aDireccionJefeAdmision := aJefeAdmision metaInfo 
		addToObject: aJefeAdmision featureNamedValue: 'direcciones' create: (Array 
			with: #( 'calle' 'Ciudad de Barcelona' ) 
			with: #( 'numeroEnCalle' '29' ) 
			with: #( 'apartamento' 'B' )
			with: #( 'distritoPostal' '46700' )
			with: #( 'poblacion' 'Gandia' )
			with: #( 'provincia' 'Valencia' )
			with: #( 'pais' 'Planeta Tierra' )
			with: #( 'fechaInicioValidez' with: SpanishDate today)
			with: (Array with: 'modoUso' with: ((aClinica metaInfo model typeNamed: 'TiposCompuestos::ModoUsoDireccion')
				createEnumerationObjectFromLiteral: 'Oficial' ))).


	aRecepcionista := aClinica metaInfo addToObject: aClinica featureNamedValue: 'personalClinica'
		factoryNamed: 'Nucleo::Recepcionista' create: (Array 
		with: #( 'dninif' '33.555.777-W' ) 
		with: #( 'nombre' 'NOMBRE del RECEPCIONISTA' ) 
		with: #( 'apellidos' 'APELLIDOS del RECEPCIONISTA' )
		with: (Array with: 'sexo' with: (((aClinica metaInfo model typeNamed: 'Nucleo::Recepcionista')
			effectiveFeatureNamed: 'sexo')  
			referencedType createEnumerationObjectFromLiteral: 'Femenino' ))).

	aDireccionRecepcionista := aRecepcionista metaInfo 
		addToObject: aRecepcionista featureNamedValue: 'direcciones' create: (Array 
			with: #( 'calle' 'Ciudad de Barcelona' ) 
			with: #( 'numeroEnCalle' '29' ) 
			with: #( 'apartamento' 'B' )
			with: #( 'distritoPostal' '46700' )
			with: #( 'poblacion' 'Gandia' )
			with: #( 'provincia' 'Valencia' )
			with: #( 'pais' 'Planeta Tierra' )
			with: #( 'fechaInicioValidez' with: SpanishDate today)
			with: (Array with: 'modoUso' with: ((aClinica metaInfo model typeNamed: 'TiposCompuestos::ModoUsoDireccion')
				createEnumerationObjectFromLiteral: 'Oficial' ))).


	aDepartamentoAdmision := aClinica metaInfo addToObject: aClinica featureNamedValue: 'departamentos'
		factoryNamed: 'Nucleo::DepartamentoClinica' create: (Array 
		with: #( 'nombreDepartamento' 'Admision' ) 
		with: (Array with: 'tipoDepartamento' with: (((aClinica metaInfo model typeNamed: 'Nucleo::DepartamentoClinica')
			effectiveFeatureNamed: 'tipoDepartamento')  
			referencedType createEnumerationObjectFromLiteral: 'UnidadAdministrativa' ))).

	aTelefonoUnoDepartamentoAdmision := aDepartamentoAdmision metaInfo addToObject: aDepartamentoAdmision 
		featureNamedValue: 'numerosTelecomunicacion' create: (Array 
		with: #( 'numero' '96 287 2650' ) 
		with: (Array with: 'tipoNumeroTelecomunicacion' with: (((aDepartamentoAdmision metaInfo effectiveFeatureNamed: 'numerosTelecomunicacion') 
			referencedType effectiveFeatureNamed: 'tipoNumeroTelecomunicacion')  
			referencedType createEnumerationObjectFromLiteral: 'TelefonoDirecto' ))
		with: (Array with: 'fechaInicioValidez' with: SpanishDate today)
		with: (Array with: 'modoUso' with: (((aDepartamentoAdmision metaInfo effectiveFeatureNamed: 'numerosTelecomunicacion') 
			referencedType effectiveFeatureNamed: 'modoUso')  
			referencedType createEnumerationObjectFromLiteral: 'Primario' ))).

	aDireccionDepartamentoAdmision := aDepartamentoAdmision metaInfo 
		addToObject: aDepartamentoAdmision featureNamedValue: 'direcciones' create: (Array 
			with: #( 'calle' 'Ciudad de Barcelona' ) 
			with: #( 'numeroEnCalle' '29' ) 
			with: #( 'apartamento' 'B' )
			with: #( 'distritoPostal' '46700' )
			with: #( 'poblacion' 'Gandia' )
			with: #( 'provincia' 'Valencia' )
			with: #( 'pais' 'Planeta Tierra' )
			with: #( 'fechaInicioValidez' with: SpanishDate today)
			with: (Array with: 'modoUso' with: ((aClinica metaInfo model typeNamed: 'TiposCompuestos::ModoUsoDireccion')
				createEnumerationObjectFromLiteral: 'Oficial' ))).


	aPuestoJefeAdmision:= aDepartamentoAdmision metaInfo addToObject: aDepartamentoAdmision featureNamedValue: 'puestosClinica'
		create: (Array 
		with: #( 'nombrePuestoClinica' 'Jefe Admision' ) 
		with: (Array with: 'fechaInicioValidez' with: SpanishDate today)
		with: (Array with: 'tipoPuestoClinica' with: (((aClinica metaInfo model typeNamed: 'TiposRelacionales::PuestoClinica')
			effectiveFeatureNamed: 'tipoPuestoClinica')  
			referencedType createEnumerationObjectFromLiteral: 'JefeUnidadAdministrativa' ))).
	aPuestoJefeAdmision metaInfo setObject: aPuestoJefeAdmision featureNamed: 'personaClinica' value: aJefeAdmision.

	aPuestoRecepcionista := aDepartamentoAdmision metaInfo addToObject: aDepartamentoAdmision featureNamedValue: 'puestosClinica'
		create: (Array 
		with: #( 'nombrePuestoClinica' 'Ventanilla Admision' ) 
		with: (Array with: 'fechaInicioValidez' with: SpanishDate today)
		with: (Array with: 'tipoPuestoClinica' with: (((aClinica metaInfo model typeNamed: 'TiposRelacionales::PuestoClinica')
			effectiveFeatureNamed: 'tipoPuestoClinica')  
			referencedType createEnumerationObjectFromLiteral: 'Recepcionista' ))).
	aPuestoRecepcionista metaInfo setObject: aPuestoRecepcionista  featureNamed: 'personaClinica' value: aRecepcionista.



	 ^theArchivoGeneral!

loadAndBindCodificacionesOnArchivo: theArchivoGeneral

	| aModel aCodificacionEnfermedades aCodificacionProcedimientos |

	aModel := theArchivoGeneral metaInfo model.
	aModel isNil ifTrue: [ ^nil].
(Dialog confirm: 'Load CODIFICACIONES ?' initialAnswer: true) ifFalse: [ ^theArchivoGeneral].

	aCodificacionEnfermedades := KRIcd9CMCodesInfoHolder currentInfoStoreMethodSelector: #codificacionEnfermedadesStore.
	aCodificacionEnfermedades isNil ifFalse: [
		aCodificacionEnfermedades  recursiveDetachFromMetaInfo.
		aCodificacionEnfermedades  recursiveAttachToMetaInfoInModel: aModel.
		aCodificacionEnfermedades metaInfo setObject: aCodificacionEnfermedades featureNamed: 'archivo'
			value: theArchivoGeneral
	].

	aCodificacionProcedimientos :=  KRIcd9CMCodesInfoHolder currentInfoStoreMethodSelector: #codificacionProcedimientosStore.
	aCodificacionProcedimientos isNil ifFalse: [
		aCodificacionProcedimientos  recursiveDetachFromMetaInfo.
		aCodificacionProcedimientos  recursiveAttachToMetaInfoInModel: aModel.
		aCodificacionProcedimientos metaInfo setObject: aCodificacionProcedimientos featureNamed: 'archivo'
			value: theArchivoGeneral.
	].

	^theArchivoGeneral! !

!KRSimpleInfoHolder class publicMethodsFor: 'persistence'!

loadCurrentInfoClinicaFrom: theFileName

	| aDomainCMGO aModel aHomeArchivo anArchivoGeneralRoot aDefaultCurrentInfoSelector someCurrentInfos |
	
	(theFileName isNil  or: [ theFileName isEmpty]) ifTrue: [ ^nil].

	aDefaultCurrentInfoSelector := self defaultCurrentInfoSelector.
	aDefaultCurrentInfoSelector isNil ifTrue: [ ^nil].

	someCurrentInfos := self currentInfos.
	someCurrentInfos isNil ifTrue: [ ^nil].

	aModel := KRSimpleMetaInfoHolder   currentModelStoreMethodSelector: #kronoSimpleStore. 
	aModel isNil ifTrue: [ ^nil].

	aDomainCMGO := CMGenericObject bossInFromFileNamed: theFileName.
	aDomainCMGO isNil ifTrue: [ ^nil].

	aDomainCMGO recursiveAttachToMetaInfoInModel: aModel.

	aHomeArchivo := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Archivo'].
	aHomeArchivo isNil ifTrue: [ ^nil].

	anArchivoGeneralRoot := aHomeArchivo metaInfo getObject: aHomeArchivo featureNamedValue: 'homeRootsCMGO' 
		detect: 'nombre' test: [:anArchivoName | anArchivoName = 'General'] orCreate: 'General'.
	anArchivoGeneralRoot  isNil ifTrue: [ ^nil].

	someCurrentInfos at:  aDefaultCurrentInfoSelector  put: aDomainCMGO.

	self loadAndBindCodificacionesOnArchivo: anArchivoGeneralRoot.


	^aDomainCMGO!

saveCurrentInfoClinicaAs: theFileName

	| aDomainCMGO aHomeArchivo anArchivoGeneralRoot aModel aCodificacionEnfermedades aCodificacionProcedimientos aCodificacionSubstancias |
	
	(theFileName isNil  or: [ theFileName isEmpty]) ifTrue: [ ^nil].

	self hasCurrentInfo ifFalse: [ ^nil].

	aDomainCMGO := self currentInfo.
	aDomainCMGO isNil ifTrue: [ ^nil].

	aHomeArchivo := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Archivo'].
	aHomeArchivo isNil ifTrue: [ ^nil].

	anArchivoGeneralRoot := aHomeArchivo metaInfo getObject: aHomeArchivo featureNamedValue: 'homeRootsCMGO' 
		detect: 'nombre' test: [:anArchivoName | anArchivoName = 'General'].
	anArchivoGeneralRoot  isNil ifTrue: [ ^nil].

	aModel := aDomainCMGO metaInfo model.
	aModel isNil ifTrue: [ ^nil].


	CMTransaction firstTransactionDo:	[ 

		[
			aCodificacionEnfermedades := anArchivoGeneralRoot metaInfo getObject: anArchivoGeneralRoot 
				featureNamedValue: 'codificacionEnfermedades'.
			aCodificacionEnfermedades isNil ifFalse: [ 
				anArchivoGeneralRoot metaInfo unsetObject: anArchivoGeneralRoot 
					featureNamed: 'codificacionEnfermedades' value: aCodificacionEnfermedades
			].
			aCodificacionProcedimientos := anArchivoGeneralRoot metaInfo getObject: anArchivoGeneralRoot 
				featureNamedValue: 'codificacionProcedimientos'.
			aCodificacionProcedimientos isNil ifFalse: [ 
				anArchivoGeneralRoot metaInfo unsetObject: anArchivoGeneralRoot 
					featureNamed: 'codificacionProcedimientos' value: aCodificacionProcedimientos
			].
			aCodificacionSubstancias := anArchivoGeneralRoot metaInfo getObject: anArchivoGeneralRoot 
				featureNamedValue: 'codificacionSubstancias'.
			aCodificacionSubstancias isNil ifFalse: [ 
				anArchivoGeneralRoot metaInfo unsetObject: anArchivoGeneralRoot 
					featureNamed: 'codificacionSubstancias' value: aCodificacionSubstancias
			].

			aDomainCMGO recursiveDetachFromMetaInfo.

			aDomainCMGO bossOutToFileNamed: theFileName

		]
			valueNowOrOnUnwindDo: 
		[
			aDomainCMGO recursiveAttachToMetaInfoInModel: aModel.
		].

		CMTransaction undoLastTransaction

	]! !

KRSimpleInfoHolder initializeAfterLoad!
KRONO_Simple_Info initializeAfterLoad!

KRONO_Simple_Info loaded!
