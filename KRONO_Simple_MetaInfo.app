'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:38:03 pm'!



((KRONO_Simple createSubApplication: #KRONO_Simple_MetaInfo in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

KRONO_Simple_MetaInfo becomeDefault!

CMMetaInfoPersistencyHolder subclass: #KRSimpleMetaInfoHolder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Simple_MetaInfo becomeDefault!

SubApplication subclass: #KRONO_Simple_MetaInfo
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Simple_MetaInfo becomeDefault!

!KRSimpleMetaInfoHolder class publicMethodsFor: 'current'!

currentMessages
	"self  currentModel browsePath"
	"self  resetCurrentModels"
	"(self  currentModelStoreMethodSelector: self defaultCurrentModelSelector) browsePath"
	"self  resetCurrentModelStoreMethodSelector: self defaultCurrentModelSelector"!

retrieveCurrentModelStoreMethodSelector: theStoreMethodSelector
	| aModel |
	aModel := super retrieveCurrentModelStoreMethodSelector: theStoreMethodSelector.
	aModel isNil ifTrue: [ ^nil].

	"aModel definitionsHolderClassNameForInstances: KRUIDefinitionsHolder name.
	aModel configurationClassNameForInstances: KRUIDeveloperConfiguration name."
	^aModel! !

!KRSimpleMetaInfoHolder class publicMethodsFor: 'default'!

defaultCurrentModelSelector
	"self  defaultCurrentTranslationSelector "

	^#kronoSimpleStore! !

!KRSimpleMetaInfoHolder class publicMethodsFor: 'modelElements persistence'!

kronoSimpleStore

	"(CODEElement newFromPersistenceAsCode: KRSimpleMetaInfoHolder kronoSimpleStore) browsePath"

	self ojoModel.

	^   #( model 'KronoSimple'
	nil nil
	nil
	nil
	KRSimpleMetaInfoHolder kronoSimpleStore
	nil
	(submodules
	  ( module 'DomainRootElements'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'Domain'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'domainModelCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'CODEModel' 'PrimitiveTypes'  ) 

				false
			   )

			  ( attribute 'domainNameCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false true false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'homesCMGO'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'domainCMGO'  ( refToType 'Home' 'DomainRootElements'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Home'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'homeIDCounterCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'Integer' 'PrimitiveTypes'  ) 

				false
			   )

			  ( attribute 'homeRootsCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'CMGO' 'PrimitiveTypes'  ) 

				false
			   )

			  ( attribute 'homedElementsTypeMetaInfoCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'CODEType' 'PrimitiveTypes'  ) 

				false
			   )

			  ( attribute 'homeNameCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false true false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'domainCMGO'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'homesCMGO'  ( refToType 'Domain' 'DomainRootElements'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		 )
		nil
	   )

	  ( module 'PrimitiveTypes'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'CODEModel'
			nil nil
			nil
			nil
			false false false
			CODEModel nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'Integer'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			'1'
			nil
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'CMGO'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'objectDomainCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'Domain' 'DomainRootElements'  ) 

				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'CODEType'
			nil nil
			nil
			nil
			false false false
			CODEElement nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'String'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'Date'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'Time'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'Number'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'Boolean'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'Text'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'SpanishDate'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
			nil
			false
		   )

		 )
		nil
	   )

	  ( module 'Nucleo'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'Archivo'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false true false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'nombre'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral Archivo'
				''
				 ( refToType 'NombreArchivo' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'organizacionesNotificables'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false false false false false
				ALWAYS
				''
				''
				''
				' calc organizacionesOrigenPacientes  union:   organizacionesIntermediariasPrestaciones union:   organizacionesPagadorasPrestaciones'
				''
				 ( refToType 'Organizacion' 'Organizaciones'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'historias'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				'codigoHistoria'
				''
				''
				''
				( refToInverseRelationship 'archivo'  ( refToType 'Historia' 'Nucleo'  )  ) 
				false
			   )

			  ( relationship 'facturacionClinica'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'archivo'  ( refToType 'FacturacionClinica' 'Facturacion'  )  ) 
				false
			   )

			  ( relationship 'facturacionAlquiler'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'1' #'1'
				false false false false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'archivo'  ( refToType 'FacturacionAlquiler' 'Facturacion'  )  ) 
				false
			   )

			  ( relationship 'organizacionesOrigenPacientes'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'archivo'  ( refToType 'OrganizacionOrigenPacientes' 'Organizaciones'  )  ) 
				false
			   )

			  ( relationship 'organizacionesGestorasPrestaciones'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'archivo'  ( refToType 'OrganizacionGestoraPrestaciones' 'Organizaciones'  )  ) 
				false
			   )

			  ( relationship 'organizacionesPagadorasPrestaciones'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'archivo'  ( refToType 'OrganizacionPagadoraPrestaciones' 'Organizaciones'  )  ) 
				false
			   )

			  ( relationship 'clinica'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'1' #'1'
				false false true false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				( refToInverseRelationship 'archivo'  ( refToType 'Clinica' 'Nucleo'  )  ) 
				false
			   )

			  ( relationship 'codificacionEnfermedades'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'1' #'1'
				false false false false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'archivo'  ( refToType 'CodificacionEnfermedadesIDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				false
			   )

			  ( relationship 'codificacionProcedimientos'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'archivo'  ( refToType 'CodificacionProcedimientosIDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				false
			   )

			  ( relationship 'codificacionSubstancias'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'archivo'  ( refToType 'CodificacionSubstanciasIDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				false
			   )

			  ( relationship 'calendarioVisitas'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				( refToInverseRelationship 'archivo'  ( refToType 'CalendarioVisitas' 'Calendario'  )  ) 
				false
			   )

			  ( relationship 'calendarioGuardias'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'1' #'1'
				false false false false false true false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				( refToInverseRelationship 'archivo'  ( refToType 'CalendarioGuardias' 'Calendario'  )  ) 
				false
			   )

			  ( relationship 'cis core root'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'archivo'  ( refToType 'CISR CORE Root' 'CISR' 'CISR CORE'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Historia'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false true
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'codigoHistoria'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral nif paciente'
				''
				 ( refToType 'CodigoHistoria' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'numeroHistoria'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false true false false true true
				FROMHOMEIDCOUNTER
				''
				''
				''
				''
				''
				 ( refToType 'NumeroHistoria' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'fechaApertura'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'FechaAperturaHistoria' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'fechaCierre'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'FechaCierreHistoria' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'comentario'
				nil nil
				nil
				(aspects
				   ( refToAspect 'PresentationText' 'Aspectos'  ) 
				 )
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				'ranliteral comentario a la historia del paciente'
				''
				 ( refToType 'ComentarioHistoria' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'tipoHistoria'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal Actual'
				''
				 ( refToType 'TipoHistoria' 'Enumeraciones'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'cambiosEstadoAdministrativo'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				'fechaCambioEstadoAdministrativo'
				''
				''
				''
				( refToInverseRelationship 'historia'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				false
			   )

			  ( relationship 'archivo'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'historias'  ( refToType 'Archivo' 'Nucleo'  )  ) 
				false
			   )

			  ( relationship 'paciente'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'1' #'1'
				false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				( refToInverseRelationship 'historia'  ( refToType 'Paciente' 'Nucleo'  )  ) 
				false
			   )

			  ( relationship 'referenciasAHistorias'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'historiaReferenciante'  ( refToType 'ReferenciaAHistoria' 'TiposRelacionales'  )  ) 
				false
			   )

			  ( relationship 'referenciadaPorHistorias'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'historiaReferenciada'  ( refToType 'ReferenciaAHistoria' 'TiposRelacionales'  )  ) 
				false
			   )

			  ( relationship 'visitas'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				'fechaVisita'
				''
				''
				''
				( refToInverseRelationship 'historia'  ( refToType 'Visita' 'Nucleo'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Paciente'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false true
			nil
			(supertypes
			   ( refToType 'PersonaFisica' 'TiposGenerales'  ) 
			   ( refToType 'ConDirecciones' 'ContenedoresDeTipos'  ) 
			   ( refToType 'ConNumerosTelecomunicacion' 'ContenedoresDeTipos'  ) 
			   ( refToType 'ConPersonasContacto' 'ContenedoresDeTipos'  ) 
			   ( refToType 'ClienteFacturaClinica' 'Facturacion'  ) 
			 )
			(attributes
			  ( attributeRefinement 'dninif'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				AFTERCONNECTION
				''
				''
				''
				'clone historia codigoHistoria'
				''
				 ( refToType 'CodigoHistoria' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'dninif'  ( refToType 'PersonaFisica' 'TiposGenerales'  )  ) 
				 )
				( isNameAttribute: computationKind: isInitializationPropagationAllowed: isInitializationPropagationOnConnectAllowed: initializationExpression:  )
			   )

			  ( attributeRefinement 'nombre'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral nombre paciente'
				''
				 ( refToType 'NombrePersona' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'nombre'  ( refToType 'PersonaFisica' 'TiposGenerales'  )  ) 
				 )
				( isChangeable: isInitializationPropagationAllowed: isInitializationPropagationOnConnectAllowed: initializationExpression:  )
			   )

			  ( attributeRefinement 'apellidos'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral apellidos paciente'
				''
				 ( refToType 'ApellidosPersona' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'apellidos'  ( refToType 'PersonaFisica' 'TiposGenerales'  )  ) 
				 )
				( initializationExpression: isInitializationPropagationOnConnectAllowed: isChangeable:  )
			   )

			  ( attributeRefinement 'sexo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'Sexo' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'sexo'  ( refToType 'PersonaFisica' 'TiposGenerales'  )  ) 
				 )
				( isInitializationPropagationAllowed: isInitializationPropagationOnConnectAllowed:  )
			   )

			  ( attributeRefinement 'fechaNacimiento'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'FechaNacimiento' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'fechaNacimiento'  ( refToType 'PersonaFisica' 'TiposGenerales'  )  ) 
				 )
				( isInitializationPropagationAllowed: isInitializationPropagationOnConnectAllowed: isChangeable:  )
			   )

			  ( attribute 'poblacionNacimiento'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'Poblacion' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'provinciaNacimiento'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'Provincia' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'paisNacimiento'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'Pais' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'direccionOficial'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'noclone direcciones'
				'calc modoUso same: "Oficial"'
				 ( refToType 'Direccion' 'TiposCompuestos'  ) 

				false
			   )

			  ( attributeRefinement 'nifApellidosYNombre'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				ALWAYS
				''
				''
				''
				'calc clonenot dninif , "_" , apellidos , "," , nombre'
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'nifApellidosYNombre'  ( refToType 'PersonaFisica' 'TiposGenerales'  )  ) 
				 )
				( isNameAttribute:  )
			   )

			 )
			(relationships
			  ( relationship 'historia'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'paciente'  ( refToType 'Historia' 'Nucleo'  )  ) 
				false
			   )

			  ( relationshipRefinement 'facturas'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'clienteFactura'  ( refToType 'FacturaClinicaParticular' 'Facturacion'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'facturas'  ( refToType 'ClienteFacturaClinica' 'Facturacion'  )  ) 
				 )
				( isInitializationPropagationAllowed: isInitializationPropagationOnConnectAllowed:  )
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Doctor'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'PersonaClinica' 'Nucleo'  ) 
			   ( refToType 'ParteInteraccionCambioEstadoAdministrativo' 'EstadosAdministrativos'  ) 
			   ( refToType 'RecomendanteCambioEstadoAdministrativo' 'EstadosAdministrativos'  ) 
			   ( refToType 'AutorizanteCambioEstadoAdministrativo' 'EstadosAdministrativos'  ) 
			   ( refToType 'ParticipanteActoClinico' 'Nucleo'  ) 
			   ( refToType 'Atestatario' 'TiposGenerales'  ) 
			   ( refToType 'Diplomado' 'Nucleo'  ) 
			   ( refToType 'Recepcionista' 'Nucleo'  ) 
			   ( refToType 'AtestatarioRepresentado' 'TiposGenerales'  ) 
			   ( refToType 'NotificanteDeCambioEstadoAdministativo' 'EstadosAdministrativos'  ) 
			   ( refToType 'Colegiado' 'Nucleo'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'PersonaClinica'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'PersonaFisica' 'TiposGenerales'  ) 
			   ( refToType 'ConNumerosTelecomunicacion' 'ContenedoresDeTipos'  ) 
			 )
			(attributes
			  ( attributeRefinement 'apellidosYNombre'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				ALWAYS
				''
				''
				''
				'calc clonenot apellidos , "," , nombre'
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'apellidosYNombre'  ( refToType 'PersonaFisica' 'TiposGenerales'  )  ) 
				 )
				( isNameAttribute:  )
			   )

			  ( attributeRefinement 'dninif'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral nif '
				''
				 ( refToType 'DNINIF' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'dninif'  ( refToType 'PersonaFisica' 'TiposGenerales'  )  ) 
				 )
				( isNameAttribute:  )
			   )

			 )
			(relationships
			  ( relationship 'conPersonalClinica'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'personalClinica'  ( refToType 'ConPersonalClinica' 'ContenedoresDeTipos'  )  ) 
				false
			   )

			  ( relationship 'puestosClinica'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'personaClinica'  ( refToType 'PuestoClinica' 'TiposRelacionales'  )  ) 
				false
			   )

			  ( relationship 'entradaCalendarioGuardia'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'personasClinica'  ( refToType 'EntradaCalendarioGuardia' 'Calendario'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Visita'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'fechaVisita'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'SpanishDate' 'PrimitiveTypes'  ) 

				false
			   )

			  ( attribute 'horaVisita'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'Time' 'PrimitiveTypes'  ) 

				false
			   )

			  ( attribute 'tipoOrigenPaciente'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal Empresa'
				''
				 ( refToType 'TipoOrigenPaciente' 'Enumeraciones'  ) 

				false
			   )

			  ( attribute 'estadoVisita'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALVALUE
				''
				''
				''
				'literal EnCurso'
				''
				 ( refToType 'EstadoVisita' 'Enumeraciones'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'actosClinicos'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'visita'  ( refToType 'ActoClinico' 'Nucleo'  )  ) 
				false
			   )

			  ( relationship 'recepcionista'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'noclone historia archivo clinica personalClinica'
				''
				''
				( refToInverseRelationship 'visitasRecibidas'  ( refToType 'Recepcionista' 'Nucleo'  )  ) 
				false
			   )

			  ( relationship 'historia'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'visitas'  ( refToType 'Historia' 'Nucleo'  )  ) 
				false
			   )

			  ( relationship 'factura'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'visitasFacturadas'  ( refToType 'FacturaClinica' 'Facturacion'  )  ) 
				false
			   )

			  ( relationship 'entradaCalendarioVisita'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false false false true false false
				NOCOMPUTATION
				''
				'nocreate historia archivo calendarioVisitas ejercicios periodos jornadas entradasCalendario'
				''
				''
				( refToInverseRelationship 'visita'  ( refToType 'EntradaCalendarioVisita' 'Calendario'  )  ) 
				false
			   )

			  ( relationship 'tarifaClinica'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'visitas'  ( refToType 'TarifaClinicaAbstracta' 'Tarifas'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'ActoClinico'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'claseActo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'ClaseActoClinico' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'comentario'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral comentario al acto clinico en texto libre, recomendado utilizar codificaciones compatibles prEnv13606'
				''
				 ( refToType 'ComentarioActoClinico' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'totalFacturable'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'calc prestacionesClinicaFacturables precio :sum'
				''
				 ( refToType 'PrecioLineaFactura' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'visita'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'actosClinicos'  ( refToType 'Visita' 'Nucleo'  )  ) 
				false
			   )

			  ( relationship 'personalParticipante'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'1' #*
				false false true false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				( refToInverseRelationship 'actoClinico'  ( refToType 'ParticipacionActoClinico' 'TiposRelacionales'  )  ) 
				false
			   )

			  ( relationship 'lineaFactura'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'actoClinicoFacturado'  ( refToType 'LineaFacturaClinicaPorActos' 'Facturacion'  )  ) 
				false
			   )

			  ( relationship 'prestacionesClinicaFacturables'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'actoClinico'  ( refToType 'PrestacionClinicaFacturable' 'Facturacion'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Administrativo'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'PersonaClinica' 'Nucleo'  ) 
			   ( refToType 'Atestatario' 'TiposGenerales'  ) 
			   ( refToType 'AtestatarioRepresentado' 'TiposGenerales'  ) 
			   ( refToType 'NotificanteDeCambioEstadoAdministativo' 'EstadosAdministrativos'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'Diplomado'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'ParticipanteActoClinico' 'Nucleo'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'Tecnico'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'PersonaClinica' 'Nucleo'  ) 
			   ( refToType 'ParticipanteActoClinico' 'Nucleo'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'PersonaAuxiliar'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'PersonaClinica' 'Nucleo'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'Recepcionista'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'PersonaClinica' 'Nucleo'  ) 
			   ( refToType 'Atestatario' 'TiposGenerales'  ) 
			   ( refToType 'AtestatarioRepresentado' 'TiposGenerales'  ) 
			   ( refToType 'NotificanteDeCambioEstadoAdministativo' 'EstadosAdministrativos'  ) 
			 )
			nil
			(relationships
			  ( relationship 'visitasRecibidas'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'recepcionista'  ( refToType 'Visita' 'Nucleo'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'ParticipanteActoClinico'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'PersonaClinica' 'Nucleo'  ) 
			 )
			nil
			(relationships
			  ( relationship 'participacionesActoClinico'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'participanteActoClinicos'  ( refToType 'ParticipacionActoClinico' 'TiposRelacionales'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Clinica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'ConPersonalClinica' 'ContenedoresDeTipos'  ) 
			   ( refToType 'ConDepartamentosClinica' 'ContenedoresDeTipos'  ) 
			   ( refToType 'PersonaJuridica' 'TiposGenerales'  ) 
			   ( refToType 'ConNumerosTelecomunicacion' 'ContenedoresDeTipos'  ) 
			 )
			(attributes
			  ( attributeRefinement 'cif'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal B-123.456'
				''
				 ( refToType 'CIF' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'cif'  ( refToType 'PersonaJuridica' 'TiposGenerales'  )  ) 
				 )
				( isChangeable: isInitializationPropagationAllowed: isInitializationPropagationOnConnectAllowed: initializationExpression:  )
			   )

			  ( attributeRefinement 'nombreComercial'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal FerSalud'
				''
				 ( refToType 'NombreComercial' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'nombreComercial'  ( refToType 'PersonaJuridica' 'TiposGenerales'  )  ) 
				 )
				( isChangeable: isInitializationPropagationAllowed: isInitializationPropagationOnConnectAllowed: initializationExpression:  )
			   )

			  ( attributeRefinement 'razonSocial'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal Policlinica FerSalud'
				''
				 ( refToType 'RazonSocial' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'razonSocial'  ( refToType 'PersonaJuridica' 'TiposGenerales'  )  ) 
				 )
				( initializationExpression:  )
			   )

			  ( attribute 'direccionOficial'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'noclone direcciones'
				'calc modoUso same: "Oficial"'
				 ( refToType 'Direccion' 'TiposCompuestos'  ) 

				false
			   )

			  ( attribute 'direccionFacturaAlquiler'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'noclone direcciones'
				'calc modoUso same: "Factura"'
				 ( refToType 'Direccion' 'TiposCompuestos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'archivo'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'clinica'  ( refToType 'Archivo' 'Nucleo'  )  ) 
				false
			   )

			  ( relationship 'clinicaDeDepartamento'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'0'
				false false true false false true true
				ALWAYS
				''
				''
				''
				'literal nil'
				( refToInverseRelationship 'clinica'  ( refToType 'DepartamentoClinica' 'Nucleo'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'DepartamentoClinica'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			   ( refToType 'ConPuestosClinica' 'ContenedoresDeTipos'  ) 
			   ( refToType 'ConDepartamentosClinica' 'ContenedoresDeTipos'  ) 
			   ( refToType 'ConNumerosTelecomunicacion' 'ContenedoresDeTipos'  ) 
			   ( refToType 'ConDirecciones' 'ContenedoresDeTipos'  ) 
			 )
			(attributes
			  ( attribute 'nombreDepartamento'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral Unidad o Servicio Clinica'
				''
				 ( refToType 'NombreDepartamento' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'tipoDepartamento'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'TipoDepartamentoClinica' 'Enumeraciones'  ) 

				false
			   )

			  ( attribute 'nombreYTipoDepartamento'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				ALWAYS
				''
				''
				''
				'calc clonenot nombreDepartamento , "_(" , tipoDepartamento , ")"'
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'departamentoDe'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'departamentos'  ( refToType 'ConDepartamentosClinica' 'ContenedoresDeTipos'  )  ) 
				false
			   )

			  ( relationship 'clinica'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false true false false true true
				ALWAYS
				''
				''
				''
				'calc clonenot departamentoDe :recurse'
				( refToInverseRelationship 'clinicaDeDepartamento'  ( refToType 'Clinica' 'Nucleo'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'ServicioClinica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'DepartamentoClinica' 'Nucleo'  ) 
			 )
			(attributes
			  ( attributeRefinement 'tipoDepartamento'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALVALUE
				''
				''
				''
				'literal ServicioClinica'
				''
				 ( refToType 'TipoDepartamentoClinica' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'tipoDepartamento'  ( refToType 'DepartamentoClinica' 'Nucleo'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'UnidadAdministrativa'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'DepartamentoClinica' 'Nucleo'  ) 
			 )
			(attributes
			  ( attributeRefinement 'tipoDepartamento'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal UnidadAdministrativa'
				''
				 ( refToType 'TipoDepartamentoClinica' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'tipoDepartamento'  ( refToType 'DepartamentoClinica' 'Nucleo'  )  ) 
				 )
				( computationKind: isInitializationPropagationAllowed: isInitializationPropagationOnConnectAllowed: initializationExpression:  )
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'UnidadAuxiliar'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'DepartamentoClinica' 'Nucleo'  ) 
			 )
			(attributes
			  ( attributeRefinement 'tipoDepartamento'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALVALUE
				''
				''
				''
				'literal UnidadAuxiliar'
				''
				 ( refToType 'TipoDepartamentoClinica' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'tipoDepartamento'  ( refToType 'DepartamentoClinica' 'Nucleo'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'DireccionGestionClinica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'DepartamentoClinica' 'Nucleo'  ) 
			 )
			(attributes
			  ( attributeRefinement 'tipoDepartamento'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal DireccionClinica'
				''
				 ( refToType 'TipoDepartamentoClinica' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'tipoDepartamento'  ( refToType 'DepartamentoClinica' 'Nucleo'  )  ) 
				 )
				( computationKind: isInitializationPropagationAllowed: isInitializationPropagationOnConnectAllowed: initializationExpression:  )
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'OtroDepartamento'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'DepartamentoClinica' 'Nucleo'  ) 
			 )
			(attributes
			  ( attributeRefinement 'tipoDepartamento'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALVALUE
				''
				''
				''
				'literal OtrosDepartamentos'
				''
				 ( refToType 'TipoDepartamentoClinica' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'tipoDepartamento'  ( refToType 'DepartamentoClinica' 'Nucleo'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'VisitaParticular'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'Visita' 'Nucleo'  ) 
			 )
			(attributes
			  ( attributeRefinement 'tipoOrigenPaciente'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALVALUE
				''
				''
				''
				'literal Particular'
				''
				 ( refToType 'TipoOrigenPaciente' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'tipoOrigenPaciente'  ( refToType 'Visita' 'Nucleo'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			 )
			(relationships
			  ( relationshipRefinement 'tarifaClinica'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				AFTERCONNECTION
				''
				''
				''
				'clonenot catalogoConceptosClinica tarifaActualClinica'
				( refToInverseRelationship 'visitas'  ( refToType 'TarifaClinica' 'Tarifas'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'tarifaClinica'  ( refToType 'Visita' 'Nucleo'  )  ) 
				 )
				( computationKind: kind: initializationExpression:  )
			   )

			  ( relationshipRefinement 'factura'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'visitasFacturadas'  ( refToType 'FacturaClinicaParticular' 'Facturacion'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'factura'  ( refToType 'Visita' 'Nucleo'  )  ) 
				 )
				( kind:  )
			   )

			  ( relationship 'catalogoConceptosClinica'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate historia archivo facturacionClinica catalogosConceptosClinica'
				''
				''
				( refToInverseRelationship 'visitasParticulares'  ( refToType 'CatalogoConceptosClinicaDeClinica' 'Tarifas'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'VisitaRemitida'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'Visita' 'Nucleo'  ) 
			 )
			nil
			(relationships
			  ( relationship 'organizacionOrigenPaciente'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate historia archivo organizacionesOrigenPacientes'
				''
				''
				( refToInverseRelationship 'visitasRemitidas'  ( refToType 'OrganizacionOrigenPacientes' 'Organizaciones'  )  ) 
				false
			   )

			  ( relationship 'contratoPagoPrestaciones'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'$self nocreate organizacionOrigenPaciente contratosPagoPrestaciones'
				''
				''
				( refToInverseRelationship 'visitasRemitidas'  ( refToType 'ContratoPagoPrestaciones' 'Organizaciones'  )  ) 
				false
			   )

			  ( relationshipRefinement 'tarifaClinica'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false true false false false false
				AFTERCONNECTION
				''
				''
				''
				'clonenot contratoPagoPrestaciones planPagoPrestaciones catalogoConceptosClinica tarifaActualClinica'
				( refToInverseRelationship 'visitas'  ( refToType 'TarifaClinicaAbstracta' 'Tarifas'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'tarifaClinica'  ( refToType 'Visita' 'Nucleo'  )  ) 
				 )
				( computationKind: isChangeable: kind: initializationExpression:  )
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Colegiado'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'numeroColegiado'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral'
				''
				 ( refToType 'NumeroColegiado' 'TiposBasicos'  ) 

				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		 )
		nil
	   )

	  ( module 'EstadosAdministrativos'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'BajaLaboral'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  ) 
			 )
			(attributes
			  ( attributeRefinement 'siguienteEstadoAdministrativo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Baja'
				''
				 ( refToType 'EstadoAdministrativo' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'siguienteEstadoAdministrativo'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attributeRefinement 'ambitoAdministrativo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Laboral'
				''
				 ( refToType 'AmbitoAdministrativo' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'ambitoAdministrativo'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attributeRefinement 'temporalidadCambio'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Actual'
				''
				 ( refToType 'TemporalidadCambio' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'temporalidadCambio'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attributeRefinement 'gradoCertezaCambio'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Cierto'
				''
				 ( refToType 'GradoCertezaCambio' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'gradoCertezaCambio'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attributeRefinement 'anteriorEstadoAdministrativo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Alta'
				''
				 ( refToType 'EstadoAdministrativo' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'anteriorEstadoAdministrativo'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( initializationExpression: computationKind: isChangeable:  )
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'AltaLaboral'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  ) 
			 )
			(attributes
			  ( attributeRefinement 'siguienteEstadoAdministrativo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Alta'
				''
				 ( refToType 'EstadoAdministrativo' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'siguienteEstadoAdministrativo'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attributeRefinement 'ambitoAdministrativo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Laboral'
				''
				 ( refToType 'AmbitoAdministrativo' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'ambitoAdministrativo'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attributeRefinement 'temporalidadCambio'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Actual'
				''
				 ( refToType 'TemporalidadCambio' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'temporalidadCambio'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attributeRefinement 'gradoCertezaCambio'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Cierto'
				''
				 ( refToType 'GradoCertezaCambio' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'gradoCertezaCambio'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attributeRefinement 'anteriorEstadoAdministrativo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Baja'
				''
				 ( refToType 'EstadoAdministrativo' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'anteriorEstadoAdministrativo'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( initializationExpression: computationKind: isChangeable:  )
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'BajaEscolar'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  ) 
			 )
			(attributes
			  ( attributeRefinement 'siguienteEstadoAdministrativo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Baja'
				''
				 ( refToType 'EstadoAdministrativo' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'siguienteEstadoAdministrativo'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attributeRefinement 'ambitoAdministrativo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Escolar'
				''
				 ( refToType 'AmbitoAdministrativo' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'ambitoAdministrativo'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attributeRefinement 'temporalidadCambio'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Actual'
				''
				 ( refToType 'TemporalidadCambio' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'temporalidadCambio'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attributeRefinement 'gradoCertezaCambio'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Cierto'
				''
				 ( refToType 'GradoCertezaCambio' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'gradoCertezaCambio'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attributeRefinement 'anteriorEstadoAdministrativo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Alta'
				''
				 ( refToType 'EstadoAdministrativo' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'anteriorEstadoAdministrativo'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( initializationExpression: computationKind: isChangeable:  )
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'AltaEscolar'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  ) 
			 )
			(attributes
			  ( attributeRefinement 'siguienteEstadoAdministrativo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Alta'
				''
				 ( refToType 'EstadoAdministrativo' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'siguienteEstadoAdministrativo'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attributeRefinement 'ambitoAdministrativo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Escolar'
				''
				 ( refToType 'AmbitoAdministrativo' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'ambitoAdministrativo'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attributeRefinement 'temporalidadCambio'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Actual'
				''
				 ( refToType 'TemporalidadCambio' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'temporalidadCambio'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attributeRefinement 'gradoCertezaCambio'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Cierto'
				''
				 ( refToType 'GradoCertezaCambio' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'gradoCertezaCambio'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attributeRefinement 'anteriorEstadoAdministrativo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Baja'
				''
				 ( refToType 'EstadoAdministrativo' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'anteriorEstadoAdministrativo'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( initializationExpression: computationKind: isChangeable:  )
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'PrevisionAltaLaboral'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  ) 
			 )
			(attributes
			  ( attributeRefinement 'siguienteEstadoAdministrativo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Alta'
				''
				 ( refToType 'EstadoAdministrativo' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'siguienteEstadoAdministrativo'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attributeRefinement 'ambitoAdministrativo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Laboral'
				''
				 ( refToType 'AmbitoAdministrativo' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'ambitoAdministrativo'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attributeRefinement 'temporalidadCambio'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Previsto'
				''
				 ( refToType 'TemporalidadCambio' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'temporalidadCambio'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attributeRefinement 'gradoCertezaCambio'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'GradoCertezaCambio' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'gradoCertezaCambio'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				nil
			   )

			  ( attributeRefinement 'anteriorEstadoAdministrativo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Baja'
				''
				 ( refToType 'EstadoAdministrativo' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'anteriorEstadoAdministrativo'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( initializationExpression: computationKind: isChangeable:  )
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'PrevisionAltaEscolar'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  ) 
			 )
			(attributes
			  ( attributeRefinement 'siguienteEstadoAdministrativo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Alta'
				''
				 ( refToType 'EstadoAdministrativo' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'siguienteEstadoAdministrativo'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attributeRefinement 'ambitoAdministrativo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Escolar'
				''
				 ( refToType 'AmbitoAdministrativo' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'ambitoAdministrativo'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attributeRefinement 'temporalidadCambio'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Previsto'
				''
				 ( refToType 'TemporalidadCambio' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'temporalidadCambio'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attributeRefinement 'gradoCertezaCambio'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'GradoCertezaCambio' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'gradoCertezaCambio'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				nil
			   )

			  ( attributeRefinement 'anteriorEstadoAdministrativo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Baja'
				''
				 ( refToType 'EstadoAdministrativo' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'anteriorEstadoAdministrativo'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				 )
				( initializationExpression: computationKind: isChangeable:  )
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'CambioEstadoAdministrativoAbstracto'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'fechaCambioEstadoAdministrativo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'FechaCambioEstadoAdministrativo' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'ambitoAdministrativo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'AmbitoAdministrativo' 'Enumeraciones'  ) 

				false
			   )

			  ( attribute 'anteriorEstadoAdministrativo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'EstadoAdministrativo' 'Enumeraciones'  ) 

				false
			   )

			  ( attribute 'siguienteEstadoAdministrativo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'EstadoAdministrativo' 'Enumeraciones'  ) 

				false
			   )

			  ( attribute 'gradoCertezaCambio'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'GradoCertezaCambio' 'Enumeraciones'  ) 

				false
			   )

			  ( attribute 'temporalidadCambio'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'TemporalidadCambio' 'Enumeraciones'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'recomendacionCambioEstadoAdministrativos'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'1' #*
				false false true false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				( refToInverseRelationship 'cambioEstadoAdministrativo'  ( refToType 'RecomendacionCambioEstadoAdministrativo' 'EstadosAdministrativos'  )  ) 
				false
			   )

			  ( relationship 'autorizacionCambioEstadoAdministrativos'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'cambioEstadoAdministrativo'  ( refToType 'AutorizacionCambioEstadoAdministrativo' 'EstadosAdministrativos'  )  ) 
				false
			   )

			  ( relationship 'notificacionesCambiosEstadoAdministrativo'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'cambioEstadoAdministrativo'  ( refToType 'NotificacionCambioEstadoAdministrativo' 'EstadosAdministrativos'  )  ) 
				false
			   )

			  ( relationship 'historia'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'cambiosEstadoAdministrativo'  ( refToType 'Historia' 'Nucleo'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'RecomendanteCambioEstadoAdministrativo'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'DoctorCapacitadoCambioEstadoAdministrativo' 'EstadosAdministrativos'  ) 
			 )
			nil
			(relationships
			  ( relationship 'recomiendaCambiosEstadoAdministrativos'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				nil
				false
			   )

			  ( relationshipRefinement 'interaccionesCambioEstadoAdministrativo'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'iniciadorInteraccion'  ( refToType 'RecomendacionCambioEstadoAdministrativo' 'EstadosAdministrativos'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'interaccionesCambioEstadoAdministrativo'  ( refToType 'ParteInteraccionCambioEstadoAdministrativo' 'EstadosAdministrativos'  )  ) 
				 )
				( kind:  )
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'AutorizanteCambioEstadoAdministrativo'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'DoctorCapacitadoCambioEstadoAdministrativo' 'EstadosAdministrativos'  ) 
			 )
			nil
			(relationships
			  ( relationshipRefinement 'interaccionesCambioEstadoAdministrativo'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'iniciadorInteraccion'  ( refToType 'AutorizacionCambioEstadoAdministrativo' 'EstadosAdministrativos'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'interaccionesCambioEstadoAdministrativo'  ( refToType 'ParteInteraccionCambioEstadoAdministrativo' 'EstadosAdministrativos'  )  ) 
				 )
				( kind:  )
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'NotificanteDeCambioEstadoAdministativo'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'ParteInteraccionCambioEstadoAdministrativo' 'EstadosAdministrativos'  ) 
			 )
			nil
			(relationships
			  ( relationship 'notificanteDeCambioEstadosAdministrativo'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				nil
				false
			   )

			  ( relationshipRefinement 'interaccionesCambioEstadoAdministrativo'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'iniciadorInteraccion'  ( refToType 'NotificacionCambioEstadoAdministrativo' 'EstadosAdministrativos'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'interaccionesCambioEstadoAdministrativo'  ( refToType 'ParteInteraccionCambioEstadoAdministrativo' 'EstadosAdministrativos'  )  ) 
				 )
				( kind:  )
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'AutorizacionCambioEstadoAdministrativo'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'InteraccionCambioEstadoAdministrativo' 'EstadosAdministrativos'  ) 
			 )
			(attributes
			  ( attribute 'cambioAutorizado'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

				false
			   )

			  ( attribute 'respuestaAutorizacion'
				nil nil
				nil
				(aspects
				   ( refToAspect 'PresentationText' 'Aspectos'  ) 
				 )
				'' nil
				#'1' #'1'
				false false true false true false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral respuesta detallada de la autorizacion del cambio de estado administrativo'
				''
				 ( refToType 'RespuestaAutorizacionCambioEstadoAdministrativo' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'cambioEstadoAdministrativo'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'autorizacionCambioEstadoAdministrativos'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				false
			   )

			  ( relationshipRefinement 'iniciadorInteraccion'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate cambioEstadoAdministrativoAbstracto historia archivo clinica personalClinica'
				''
				''
				( refToInverseRelationship 'interaccionesCambioEstadoAdministrativo'  ( refToType 'AutorizanteCambioEstadoAdministrativo' 'EstadosAdministrativos'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'iniciadorInteraccion'  ( refToType 'InteraccionCambioEstadoAdministrativo' 'EstadosAdministrativos'  )  ) 
				 )
				( kind: isChangeable:  )
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'RecomendacionCambioEstadoAdministrativo'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'InteraccionCambioEstadoAdministrativo' 'EstadosAdministrativos'  ) 
			 )
			(attributes
			  ( attribute 'motivoRecomendacion'
				nil nil
				nil
				(aspects
				   ( refToAspect 'PresentationText' 'Aspectos'  ) 
				 )
				'' nil
				#'1' #'1'
				false false true false true false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral descripcion detallada del motivo de la recomendacion del cambio de estado administrativo'
				''
				 ( refToType 'MotivoRecomendacionCambioEstadoAdministrativo' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'cambioEstadoAdministrativo'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'recomendacionCambioEstadoAdministrativos'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				false
			   )

			  ( relationshipRefinement 'iniciadorInteraccion'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate cambioEstadoAdministrativoAbstracto historia archivo clinica personalClinica'
				''
				''
				( refToInverseRelationship 'interaccionesCambioEstadoAdministrativo'  ( refToType 'RecomendanteCambioEstadoAdministrativo' 'EstadosAdministrativos'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'iniciadorInteraccion'  ( refToType 'InteraccionCambioEstadoAdministrativo' 'EstadosAdministrativos'  )  ) 
				 )
				( kind: isChangeable:  )
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'NotificacionCambioEstadoAdministrativo'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'InteraccionCambioEstadoAdministrativo' 'EstadosAdministrativos'  ) 
			 )
			(attributes
			  ( attribute 'comentarioNotificacion'
				nil nil
				nil
				(aspects
				   ( refToAspect 'PresentationText' 'Aspectos'  ) 
				 )
				'' nil
				#'1' #'1'
				false false true false true false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral comentario detallado de la notificacion del cambio de estado administrativo'
				''
				 ( refToType 'ComentarioNotificacionCambioEstadoAdministrativo' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'cambioEstadoAdministrativo'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'notificacionesCambiosEstadoAdministrativo'  ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  )  ) 
				false
			   )

			  ( relationship 'notificados'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false true false false false false
				NOCOMPUTATION
				''
				'nocreate cambioEstadoAdministrativo historia archivo organizacionesNotificables'
				''
				''
				( refToInverseRelationship 'notificadoDeCambiosEstadoAdministrativos'  ( refToType 'NotificadoCambioEstadoAdministrativo' 'EstadosAdministrativos'  )  ) 
				false
			   )

			  ( relationshipRefinement 'iniciadorInteraccion'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate cambioEstadoAdministrativoAbstracto historia archivo clinica personalClinica'
				''
				''
				( refToInverseRelationship 'interaccionesCambioEstadoAdministrativo'  ( refToType 'NotificanteDeCambioEstadoAdministativo' 'EstadosAdministrativos'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'iniciadorInteraccion'  ( refToType 'InteraccionCambioEstadoAdministrativo' 'EstadosAdministrativos'  )  ) 
				 )
				( kind: isChangeable:  )
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'InteraccionCambioEstadoAdministrativo'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'fechaCreacion'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'FechaCreacionInteraccionCambioEstadoAdministrativo' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'iniciadorInteraccion'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false true false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate cambioEstadoAdministrativoAbstracto historia archivo clinica personalClinica'
				''
				''
				( refToInverseRelationship 'interaccionesCambioEstadoAdministrativo'  ( refToType 'ParteInteraccionCambioEstadoAdministrativo' 'EstadosAdministrativos'  )  ) 
				false
			   )

			  ( relationship 'atestados'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				'fechaAtestado'
				''
				''
				''
				( refToInverseRelationship 'interaccionCambioEstadoAdministrativo'  ( refToType 'Atestado' 'TiposCompuestos'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'CambioEstadoAdministrativo'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			true false false false
			nil
			(supertypes
			   ( refToType 'CambioEstadoAdministrativoAbstracto' 'EstadosAdministrativos'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'ParteInteraccionCambioEstadoAdministrativo'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'PersonaFisicaOJuridica' 'TiposGenerales'  ) 
			 )
			nil
			(relationships
			  ( relationship 'interaccionesCambioEstadoAdministrativo'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'iniciadorInteraccion'  ( refToType 'InteraccionCambioEstadoAdministrativo' 'EstadosAdministrativos'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'DoctorCapacitadoCambioEstadoAdministrativo'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'ParteInteraccionCambioEstadoAdministrativo' 'EstadosAdministrativos'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NotificadoCambioEstadoAdministrativo'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'PersonaFisicaOJuridica' 'TiposGenerales'  ) 
			 )
			nil
			(relationships
			  ( relationship 'notificadoDeCambiosEstadoAdministrativos'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'notificados'  ( refToType 'NotificacionCambioEstadoAdministrativo' 'EstadosAdministrativos'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		 )
		nil
	   )

	  ( module 'ElementosDependientes'
		nil nil
		nil
		nil
		nil nil
		nil
		nil
	   )

	  ( module 'TiposBasicos'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'DNINIF'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'NIFoCIF' 'TiposBasicos'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NombrePersona'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'ApellidosPersona'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'Calle'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NumeroEnCalle'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NumeroDeApartamento'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'DistritoPostal'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'Poblacion'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'Provincia'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'Pais'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'Sexo'
			nil nil
			nil
			nil
			false true false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'Masculino'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Femenino'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'NombreArchivo'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'CodigoHistoria'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'DNINIF' 'TiposBasicos'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NumeroHistoria'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'Telefono'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'NumeroTelefonico' 'TiposBasicos'  ) 
			 )
			nil
			(relationships
			  ( relationship 'lugar'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'telefonos'  ( refToType 'Lugar' 'BolsaCarga'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'CorreoElectronico'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NumeroTelefonico'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'Fax'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Telefono' 'TiposBasicos'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'PaginaWeb'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'URL' 'TiposBasicos'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'URL'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'FechaAperturaHistoria'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'SpanishDate' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NombreOrganizacion'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'ClaseActoClinico'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NumeroPeriodoFacturacion'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			'1'
			(supertypes
			   ( refToType 'Integer' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NumeroEjercicioFiscal'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			'2002'
			(supertypes
			   ( refToType 'Integer' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'SerieFacturacion'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			'A'
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NumeroFactura'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			'1'
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'ConceptoLineaFactura'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'FechaFactura'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'SpanishDate' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NIFoCIF'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'CIF'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'NIFoCIF' 'TiposBasicos'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NombreUnidadFacturableAlquiler'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'PrecioUnidadFacturable'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Number' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'FechaInicioValidez'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'SpanishDate' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'FechaFinValidez'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'SpanishDate' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'FechaNacimiento'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'SpanishDate' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'FechaCambioEstadoAdministrativo'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'SpanishDate' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'MotivoRecomendacionCambioEstadoAdministrativo'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Text' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'RespuestaAutorizacionCambioEstadoAdministrativo'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Text' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'ComentarioNotificacionCambioEstadoAdministrativo'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Text' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'FechaAtestado'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'SpanishDate' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'HoraAtestado'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Time' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'LugarAtestado'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'FirmaDigital'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Text' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'ComentarioHistoria'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Text' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'CodigoProtocoloAtestatario'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'FechaCreacionInteraccionCambioEstadoAdministrativo'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'SpanishDate' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'ComentarioActoClinico'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Text' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'TiempoParticipacionActoClinico'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Number' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'RazonSocial'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NombreComercial'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'OtrosTiposContacto'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NombreConceptoAlquiler'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NombreCatalogoConceptosAlquiler'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NombreTarifaAlquiler'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'ValorPrecioAlquiler'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Number' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NombrePrecioTarifaAlquiler'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'DescripcionConceptoAlquiler'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Text' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NumeroUnidadesLineaFacturaAlquiler'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Number' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'PrecioUnitarioLineaFactura'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Number' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'PrecioLineaFactura'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Number' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'PrecioTotalFactura'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Number' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NombreTarifaClinica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NombreDepartamento'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NombrePuestoClinica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'Porcentaje'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Number' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'PorcentajeIVA'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Porcentaje' 'TiposBasicos'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'PorcentajeIRPF'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Porcentaje' 'TiposBasicos'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'IVA'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Number' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'IRPF'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Number' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'TotalFacturaConIVAeIRPF'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Number' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'FechaCierreHistoria'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'SpanishDate' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NombreConceptoClinica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'DescripcionConceptoClinica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Text' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NombreCatalogoConceptosClinica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NombrePrecioTarifaClinica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'ValorPrecioClinica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Number' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NombreUnidadFacturableClinica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NombrePlanPagoPrestaciones'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NombreAcuerdoTarifaClinica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NombreContratoPagoPrestaciones'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'CodigoConceptoClinica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'CodigoConceptoPagadorPrestaciones'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NumeroEjercicioCalendario'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Integer' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NumeroPeriodoCalendario'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Integer' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NumeroJornadaCalendario'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Integer' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NombreMesCalendario'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NombreDiaSemanaCalendario'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NombreCalendario'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NombreEntradaCalendario'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'HoraComienzo'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Time' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'HoraFin'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Time' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NumeroUnidadesLineaFacturaClinica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Number' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NumeroColegiado'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		 )
		nil
	   )

	  ( module 'TiposCompuestos'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'Direccion'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'calle'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral nombre de calle '
				''
				 ( refToType 'Calle' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'numeroEnCalle'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral numero'
				''
				 ( refToType 'NumeroEnCalle' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'apartamento'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral apartamento'
				''
				 ( refToType 'NumeroDeApartamento' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'distritoPostal'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral DP'
				''
				 ( refToType 'DistritoPostal' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'poblacion'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral  Poblacion'
				''
				 ( refToType 'Poblacion' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'provincia'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral provincia'
				''
				 ( refToType 'Provincia' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'pais'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal Planeta Tierra'
				''
				 ( refToType 'Pais' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'fechaInicioValidez'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'FechaInicioValidez' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'fechaFinValidez'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'FechaFinValidez' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'modoUso'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal Factura'
				''
				 ( refToType 'ModoUsoDireccion' 'TiposCompuestos'  ) 

				false
			   )

			  ( attribute 'distritoPostalPoblacionProvincia'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				ALWAYS
				''
				''
				''
				'calc clonenot distritoPostal , "-" , poblacion , "_(" , provincia , ")_" , calle , "," , numeroEnCalle , "-" , apartamento'
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'direccionDe'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'direcciones'  ( refToType 'ConDirecciones' 'ContenedoresDeTipos'  )  ) 
				false
			   )

			  ( relationship 'lugar'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'direccion'  ( refToType 'Lugar' 'BolsaCarga'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'NumeroTelecomunicacion'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'numero'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral numero de telecomunicacion'
				''
				 ( refToType 'Telefono' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'tipoNumeroTelecomunicacion'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal TelefonoDirecto'
				''
				 ( refToType 'TipoNumeroTelecomunicacion' 'Enumeraciones'  ) 

				false
			   )

			  ( attribute 'fechaInicioValidez'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'FechaInicioValidez' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'fechaFinValidez'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false false false
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'FechaFinValidez' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'modoUso'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal Primario'
				''
				 ( refToType 'ModoUsoNumeroTelecomunicacion' 'TiposCompuestos'  ) 

				false
			   )

			  ( attribute 'esTipoNumeroTelecomunicacionTelefonoDirecto'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'calc tipoNumeroTelecomunicacion same: "TelefonoDirecto"'
				''
				 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

				false
			   )

			  ( attribute 'esModoUsoPrimario'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'calc modoUso same: "Primario"'
				''
				 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

				false
			   )

			  ( attribute 'esTipoNumeroTelecomunicacionFax'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'calc tipoNumeroTelecomunicacion same: "Fax"'
				''
				 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

				false
			   )

			  ( attribute 'esTipoNumeroTelecomunicacionTelefono'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'calc esTipoNumeroTelecomunicacionTelefonoDirecto or: esTipoNumeroTelecomunicacionTelefonoCobroRevertido or: esTipoNumeroTelecomunicacionTelefonoMovil'
				''
				 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

				false
			   )

			  ( attribute 'esModoUsoSecundario'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'calc modoUso same: "Secundario"'
				''
				 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

				false
			   )

			  ( attribute 'esTipoNumeroTelecomunicacionTelefonoCobroRevertido'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'calc tipoNumeroTelecomunicacion same: "TelefonoCobroRevertido"'
				''
				 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

				false
			   )

			  ( attribute 'esTipoNumeroTelecomunicacionTelefonoMovil'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'calc tipoNumeroTelecomunicacion same: "TelefonoMovil"'
				''
				 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

				false
			   )

			  ( attribute 'numeroTipoYUso'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				ALWAYS
				''
				''
				''
				'calc clonenot numero , "_" , tipoNumeroTelecomunicacion , "-" , modoUso'
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'conNumerosTelecomunicacion'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'numerosTelecomunicacion'  ( refToType 'ConNumerosTelecomunicacion' 'ContenedoresDeTipos'  )  ) 
				false
			   )

			  ( relationship 'telefonoOficialDeClinicaUno'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Atestado'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'poderAtestado'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal NombrePropio'
				''
				 ( refToType 'PoderAtestado' 'Enumeraciones'  ) 

				false
			   )

			  ( attribute 'fechaAtestado'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'FechaAtestado' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'horaAtestado'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'HoraAtestado' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'codigoProtocoloAtestatario'
				nil nil
				nil
				nil
				'ranliteral numero de protocolo o serie atestado' nil
				#'0' #'1'
				false false false false true false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'CodigoProtocoloAtestatario' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'firma'
				nil nil
				nil
				(aspects
				   ( refToAspect 'PresentationText' 'Aspectos'  ) 
				 )
				'ranliteral una firma digital ' nil
				#'1' #'1'
				false false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'FirmaDigital' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'poderYNombreYApellidosAtestatario'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				ALWAYS
				''
				''
				''
				'calc clonenot poderAtestado , "_" , atestatario apellidosYNombre'
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'atestatario'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate interaccionCambioEstadoAdministrativo cambioEstadoAdministrativo historia archivo clinica personalClinica'
				''
				'clonenot interaccionCambioEstadoAdministrativo iniciadorInteraccion'
				( refToInverseRelationship 'atestados'  ( refToType 'Atestatario' 'TiposGenerales'  )  ) 
				false
			   )

			  ( relationship 'enRepresentacionDe'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate interaccionCambioEstadoAdministrativo cambioEstadoAdministrativo historia archivo clinica personalClinica'
				''
				'clonenot interaccionCambioEstadoAdministrativo iniciadorInteraccion'
				( refToInverseRelationship 'representadoEnAtestados'  ( refToType 'AtestatarioRepresentado' 'TiposGenerales'  )  ) 
				false
			   )

			  ( relationship 'interaccionCambioEstadoAdministrativo'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'atestados'  ( refToType 'InteraccionCambioEstadoAdministrativo' 'EstadosAdministrativos'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'ModoUsoDireccion'
			nil nil
			nil
			nil
			false true false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'Oficial'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Notificacion'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Factura'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Fisica'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'AtencionAlPublico'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Virtual'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Correo'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'ModoUsoNumeroTelecomunicacion'
			nil nil
			nil
			nil
			false true false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'Primario'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Secundario'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Interno'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		 )
		nil
	   )

	  ( module 'Enumeraciones'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'TipoOrigenPaciente'
			nil nil
			nil
			nil
			false true false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'Empresa'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Colegio'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Extranjero'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'SanidadPublica'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Particular'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'TipoNumeroTelecomunicacion'
			nil nil
			nil
			nil
			false true false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'TelefonoCobroRevertido'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'TelefonoDirecto'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'TelefonoMovil'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'BuscaPersonas'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Fax'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'ModoRelacionHistoria'
			nil nil
			nil
			nil
			false true false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'HistoriaPrevia'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'HistoriaExterna'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Familiar'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Padre'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Madre'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Hermano'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Abuelo'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Consanguineo'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Vecindad'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'EstadoAdministrativo'
			nil nil
			nil
			nil
			false true false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'Alta'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Baja'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'GradoCertezaCambio'
			nil nil
			nil
			nil
			false true false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'Cierto'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Probable'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Posible'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Improbable'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Indefinido'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'TemporalidadCambio'
			nil nil
			nil
			nil
			false true false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'Actual'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Previsto'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Anterior'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'AmbitoAdministrativo'
			nil nil
			nil
			nil
			false true false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'Escolar'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Laboral'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Privado'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'SanidadPublica'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Extranjero'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'PoderAtestado'
			nil nil
			nil
			nil
			false true false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'NombrePropio'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'EnRepresentacion'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'ModoParticipacionActoClinico'
			nil nil
			nil
			nil
			false true false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'Responsable'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Supervisor'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Realizador'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Ayudante'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'UnidadTiempoParticipacionActoClinico'
			nil nil
			nil
			nil
			false true false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'ActoCompleto'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Horas'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Minutos'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Dias'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'TipoGestoraPrestaciones'
			nil nil
			nil
			nil
			false true false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'Gestoria'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Mutua'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'TipoPagadorPrestaciones'
			nil nil
			nil
			nil
			false true false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'Aseguradora'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Mutua'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'TipoContacto'
			nil nil
			nil
			nil
			false true false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'Primario'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Emergencia'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Vacaciones'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Responsable'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Familiar'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Patron'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Superior'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Alternativo'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Inferior'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Otros'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'TipoDepartamentoClinica'
			nil nil
			nil
			nil
			false true false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'DireccionClinica'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'ServicioClinica'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'UnidadAdministrativa'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'UnidadAuxiliar'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'OtrosDepartamentos'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'TipoPuestoClinica'
			nil nil
			nil
			nil
			false true false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'DirectorClinica'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'JefeServicio'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'DoctorTitular'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'DoctorSuplente'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Enfermeria'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Tecnico'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'JefeUnidadAdministrativa'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Administrativo'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Mantenimiento'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Auxiliar'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Recepcionista'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'TipoHistoria'
			nil nil
			nil
			nil
			false true false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'Actual'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Pasada'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'TipoCalendario'
			nil nil
			nil
			nil
			false true false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'Visitas'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Guardias'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Comunicaciones'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Otros'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'TipoEntradaCalendario'
			nil nil
			nil
			nil
			false true false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'Visita'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'TomaMuestras'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'RecepcionResultados'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'TemporalidadEntradaCalendario'
			nil nil
			nil
			nil
			false true false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'Planeada'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Actual'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Esperada'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Limite'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'EstadoVisita'
			nil nil
			nil
			nil
			false true false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'EnCurso'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Planeada'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Realizada'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Cancelada'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Descartada'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		 )
		nil
	   )

	  ( module 'ContenedoresDeTipos'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'ConDirecciones'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			(relationships
			  ( relationship 'direcciones'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'direccionDe'  ( refToType 'Direccion' 'TiposCompuestos'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'ConPersonasContacto'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			(relationships
			  ( relationship 'contactos'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'conocedorContacto'  ( refToType 'Contacto' 'TiposGenerales'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'ConNumerosTelecomunicacion'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'numeroTelecomunicacionTelefonoOficialUno'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'noclone numerosTelecomunicacion'
				'calc esModoUsoPrimario  and: esTipoNumeroTelecomunicacionTelefono'
				 ( refToType 'NumeroTelecomunicacion' 'TiposCompuestos'  ) 

				false
			   )

			  ( attribute 'numeroTelecomunicacionTelefonoOficialDos'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'noclone numerosTelecomunicacion'
				'calc esModoUsoSecundario  and: esTipoNumeroTelecomunicacionTelefono'
				 ( refToType 'NumeroTelecomunicacion' 'TiposCompuestos'  ) 

				false
			   )

			  ( attribute 'numeroTelecomunicacionFaxOficial'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'noclone numerosTelecomunicacion'
				'calc esModoUsoPrimario  and: esTipoNumeroTelecomunicacionFax'
				 ( refToType 'NumeroTelecomunicacion' 'TiposCompuestos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'numerosTelecomunicacion'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'conNumerosTelecomunicacion'  ( refToType 'NumeroTelecomunicacion' 'TiposCompuestos'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'ConSubOrganizaciones'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			(relationships
			  ( relationship 'subOrganizaciones'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'superOrganizacion'  ( refToType 'Organizacion' 'Organizaciones'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'ConDepartamentosClinica'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			(relationships
			  ( relationship 'departamentos'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'departamentoDe'  ( refToType 'DepartamentoClinica' 'Nucleo'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'ConPersonalClinica'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			(relationships
			  ( relationship 'personalClinica'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'conPersonalClinica'  ( refToType 'PersonaClinica' 'Nucleo'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'ConPuestosClinica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			(relationships
			  ( relationship 'puestosClinica'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'puestoEn'  ( refToType 'PuestoClinica' 'TiposRelacionales'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		 )
		nil
	   )

	  ( module 'ActosClinicos'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'Exploracion'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'ActoClinico' 'Nucleo'  ) 
			 )
			(attributes
			  ( attributeRefinement 'claseActo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALVALUE
				''
				''
				''
				'literal Exploracion'
				''
				 ( refToType 'ClaseActoClinico' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'claseActo'  ( refToType 'ActoClinico' 'Nucleo'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'Diagnostico'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'ActoClinico' 'Nucleo'  ) 
			 )
			(attributes
			  ( attributeRefinement 'claseActo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALVALUE
				''
				''
				''
				'literal Diagnostico'
				''
				 ( refToType 'ClaseActoClinico' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'claseActo'  ( refToType 'ActoClinico' 'Nucleo'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'Tratamiento'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'ActoClinico' 'Nucleo'  ) 
			 )
			(attributes
			  ( attributeRefinement 'claseActo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALVALUE
				''
				''
				''
				'literal Tratamiento'
				''
				 ( refToType 'ClaseActoClinico' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'claseActo'  ( refToType 'ActoClinico' 'Nucleo'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'SolicitudAnalisis'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'ActoClinico' 'Nucleo'  ) 
			 )
			(attributes
			  ( attributeRefinement 'claseActo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALVALUE
				''
				''
				''
				'literal SolicitudAnalisis'
				''
				 ( refToType 'ClaseActoClinico' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'claseActo'  ( refToType 'ActoClinico' 'Nucleo'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'ResultadosAnalisis'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'ActoClinico' 'Nucleo'  ) 
			 )
			(attributes
			  ( attributeRefinement 'claseActo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALVALUE
				''
				''
				''
				'literal ResultadosAnalisis'
				''
				 ( refToType 'ClaseActoClinico' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'claseActo'  ( refToType 'ActoClinico' 'Nucleo'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'OtrosActosClinicos'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'ActoClinico' 'Nucleo'  ) 
			 )
			(attributes
			  ( attributeRefinement 'claseActo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal Otros'
				''
				 ( refToType 'ClaseActoClinico' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'claseActo'  ( refToType 'ActoClinico' 'Nucleo'  )  ) 
				 )
				( isChangeable: initializationExpression:  )
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'SoloParaFactura'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'ActoClinico' 'Nucleo'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		 )
		nil
	   )

	  ( module 'Facturacion'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'EjercicioFacturacion'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'ejercicioFiscal'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal 2002'
				''
				 ( refToType 'NumeroEjercicioFiscal' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'periodosFacturacion'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				'periodo'
				''
				''
				''
				( refToInverseRelationship 'ejercicioFacturacion'  ( refToType 'PeriodoFacturacion' 'Facturacion'  )  ) 
				false
			   )

			  ( relationship 'facturacion'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'ejerciciosFacturacion'  ( refToType 'Facturacion' 'Facturacion'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'PeriodoFacturacion'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'periodo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal 1'
				''
				 ( refToType 'NumeroPeriodoFacturacion' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'ejercicioFacturacion'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'periodosFacturacion'  ( refToType 'EjercicioFacturacion' 'Facturacion'  )  ) 
				false
			   )

			  ( relationship 'facturas'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				'numeroFactura'
				''
				''
				''
				( refToInverseRelationship 'periodoFacturacion'  ( refToType 'Factura' 'Facturacion'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Facturacion'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'serie'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal A'
				''
				 ( refToType 'SerieFacturacion' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'ejerciciosFacturacion'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				'ejercicioFiscal'
				''
				''
				''
				( refToInverseRelationship 'facturacion'  ( refToType 'EjercicioFacturacion' 'Facturacion'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Factura'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'numeroFactura'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral  '
				''
				 ( refToType 'NumeroFactura' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'fechaFactura'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'FechaFactura' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'totalFactura'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'calc lineasFactura precio :sum'
				''
				 ( refToType 'PrecioTotalFactura' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'periodoFacturacion'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'facturas'  ( refToType 'PeriodoFacturacion' 'Facturacion'  )  ) 
				false
			   )

			  ( relationship 'lineasFactura'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'factura'  ( refToType 'LineaFactura' 'Facturacion'  )  ) 
				false
			   )

			  ( relationship 'clienteFactura'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate periodoFacturacion ejercicioFacturacion facturacion clientesFacturacion'
				''
				''
				( refToInverseRelationship 'facturas'  ( refToType 'ClienteFactura' 'Facturacion'  )  ) 
				false
			   )

			 )
			(operations
			  ( operation 'imprimirFactura'
				nil nil
				nil
				nil
				#'0' #'0'
				false false                 ''
				''
				'smalltalk KRFacturaAlquilerReportWriter facturaAlquilerReport: $argument'
				nil
				nil
			   )

			 )
			nil
			false
		   )

		  ( type 'FacturaClinica'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'Factura' 'Facturacion'  ) 
			 )
			nil
			(relationships
			  ( relationshipRefinement 'clienteFactura'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate periodoFacturacion ejercicioFacturacion facturacion archivo organizacionesPagadorasPrestaciones'
				''
				''
				( refToInverseRelationship 'facturas'  ( refToType 'ClienteFacturaClinica' 'Facturacion'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'clienteFactura'  ( refToType 'Factura' 'Facturacion'  )  ) 
				 )
				( kind: candidatesPath:  )
			   )

			  ( relationshipRefinement 'lineasFactura'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'factura'  ( refToType 'LineaFacturaClinica' 'Facturacion'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'lineasFactura'  ( refToType 'Factura' 'Facturacion'  )  ) 
				 )
				( kind:  )
			   )

			  ( relationshipRefinement 'periodoFacturacion'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'facturas'  ( refToType 'PeriodoFacturacionClinica' 'Facturacion'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'periodoFacturacion'  ( refToType 'Factura' 'Facturacion'  )  ) 
				 )
				( kind:  )
			   )

			  ( relationship 'visitasFacturadas'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				NOCOMPUTATION
				''
				''
				'factura :isnil'
				''
				( refToInverseRelationship 'factura'  ( refToType 'Visita' 'Nucleo'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'LineaFactura'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'concepto'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'ConceptoLineaFactura' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'factura'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'lineasFactura'  ( refToType 'Factura' 'Facturacion'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'ClienteFactura'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			nil
			(relationships
			  ( relationship 'facturas'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'clienteFactura'  ( refToType 'Factura' 'Facturacion'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'InteresadoFactura'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'PersonaFisicaOJuridica' 'TiposGenerales'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'IntermediarioFactura'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'PersonaFisicaOJuridica' 'TiposGenerales'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'LineaFacturaClinica'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'LineaFactura' 'Facturacion'  ) 
			 )
			(attributes
			  ( attribute 'numeroUnidades'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral '
				''
				 ( refToType 'NumeroUnidadesLineaFacturaClinica' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'precio'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'calc precioUnitario * numeroUnidades'
				''
				 ( refToType 'PrecioLineaFactura' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'precioUnitario'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral'
				''
				 ( refToType 'PrecioUnitarioLineaFactura' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationshipRefinement 'factura'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'lineasFactura'  ( refToType 'FacturaClinica' 'Facturacion'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'factura'  ( refToType 'LineaFactura' 'Facturacion'  )  ) 
				 )
				( kind:  )
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'FacturaAlquiler'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'Factura' 'Facturacion'  ) 
			 )
			(attributes
			  ( attribute 'porcentajeIVA'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				AFTERCONNECTION
				''
				''
				''
				'noclone periodoFacturacion ejercicioFacturacion facturacion porcentajeIVA'
				''
				 ( refToType 'PorcentajeIVA' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'porcentajeIRPF'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false true false false
				AFTERCONNECTION
				''
				''
				''
				'noclone periodoFacturacion ejercicioFacturacion facturacion porcentajeIRPF'
				''
				 ( refToType 'PorcentajeIRPF' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'iva'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'calc totalFactura * porcentajeIVA / 100'
				''
				 ( refToType 'IVA' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'irpf'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'calc totalFactura * porcentajeIRPF / 100'
				''
				 ( refToType 'IRPF' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'totalFacturaConIVAeIRPF'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'calc totalFactura + iva - irpf'
				''
				 ( refToType 'TotalFacturaConIVAeIRPF' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationshipRefinement 'lineasFactura'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'factura'  ( refToType 'LineaFacturaAlquilerAbstracta' 'Facturacion'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'lineasFactura'  ( refToType 'Factura' 'Facturacion'  )  ) 
				 )
				( kind:  )
			   )

			  ( relationshipRefinement 'clienteFactura'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate periodoFacturacion ejercicioFacturacion facturacion clientesFacturasAlquiler'
				''
				''
				( refToInverseRelationship 'facturas'  ( refToType 'ClienteFacturaAlquiler' 'Facturacion'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'clienteFactura'  ( refToType 'Factura' 'Facturacion'  )  ) 
				 )
				( kind: candidatesPath:  )
			   )

			  ( relationshipRefinement 'periodoFacturacion'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'facturas'  ( refToType 'PeriodoFacturacionAlquiler' 'Facturacion'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'periodoFacturacion'  ( refToType 'Factura' 'Facturacion'  )  ) 
				 )
				( kind:  )
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'ClienteFacturaAlquiler'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'ClienteFactura' 'Facturacion'  ) 
			   ( refToType 'ConNumerosTelecomunicacion' 'ContenedoresDeTipos'  ) 
			 )
			(attributes
			  ( attribute 'direccionFacturaAlquiler'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'noclone direcciones'
				'calc modoUso same: "Factura"'
				 ( refToType 'Direccion' 'TiposCompuestos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationshipRefinement 'facturas'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'clienteFactura'  ( refToType 'FacturaAlquiler' 'Facturacion'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'facturas'  ( refToType 'ClienteFactura' 'Facturacion'  )  ) 
				 )
				( kind:  )
			   )

			  ( relationship 'facturacionAlquiler'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'clientesFacturasAlquiler'  ( refToType 'FacturacionAlquiler' 'Facturacion'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'ClienteFacturaClinica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true true false
			nil
			(supertypes
			   ( refToType 'ClienteFactura' 'Facturacion'  ) 
			 )
			nil
			(relationships
			  ( relationshipRefinement 'facturas'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'clienteFactura'  ( refToType 'FacturaClinica' 'Facturacion'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'facturas'  ( refToType 'ClienteFactura' 'Facturacion'  )  ) 
				 )
				( kind:  )
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'FacturacionAlquiler'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false true false
			nil
			(supertypes
			   ( refToType 'Facturacion' 'Facturacion'  ) 
			 )
			(attributes
			  ( attributeRefinement 'serie'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALVALUE
				''
				''
				''
				'literal B'
				''
				 ( refToType 'SerieFacturacion' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'serie'  ( refToType 'Facturacion' 'Facturacion'  )  ) 
				 )
				( computationKind: isInitializationPropagationAllowed: isInitializationPropagationOnConnectAllowed: initializationExpression:  )
			   )

			  ( attribute 'porcentajeIVA'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal 16'
				''
				 ( refToType 'PorcentajeIVA' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'porcentajeIRPF'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal 15'
				''
				 ( refToType 'PorcentajeIRPF' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationshipRefinement 'ejerciciosFacturacion'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				'ejercicioFiscal'
				''
				''
				''
				( refToInverseRelationship 'facturacion'  ( refToType 'EjercicioFacturacionAlquiler' 'Facturacion'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'ejerciciosFacturacion'  ( refToType 'Facturacion' 'Facturacion'  )  ) 
				 )
				( kind:  )
			   )

			  ( relationship 'archivo'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'facturacionAlquiler'  ( refToType 'Archivo' 'Nucleo'  )  ) 
				false
			   )

			  ( relationship 'clientesFacturasAlquiler'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'facturacionAlquiler'  ( refToType 'ClienteFacturaAlquiler' 'Facturacion'  )  ) 
				false
			   )

			  ( relationship 'catalogoConceptosAlquiler'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'1' #'1'
				false false false false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'facturacionAlquiler'  ( refToType 'CatalogoConceptosAlquiler' 'Tarifas'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'FacturacionClinica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false true false
			nil
			(supertypes
			   ( refToType 'Facturacion' 'Facturacion'  ) 
			 )
			(attributes
			  ( attributeRefinement 'serie'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALVALUE
				''
				''
				''
				'literal A'
				''
				 ( refToType 'SerieFacturacion' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'serie'  ( refToType 'Facturacion' 'Facturacion'  )  ) 
				 )
				( isInitializationPropagationAllowed: computationKind: isInitializationPropagationOnConnectAllowed:  )
			   )

			 )
			(relationships
			  ( relationshipRefinement 'ejerciciosFacturacion'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				'ejercicioFiscal'
				''
				''
				''
				( refToInverseRelationship 'facturacion'  ( refToType 'EjercicioFacturacionClinica' 'Facturacion'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'ejerciciosFacturacion'  ( refToType 'Facturacion' 'Facturacion'  )  ) 
				 )
				( kind:  )
			   )

			  ( relationship 'catalogosConceptosClinica'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'facturacionClinica'  ( refToType 'CatalogoConceptosClinicaDeClinica' 'Tarifas'  )  ) 
				false
			   )

			  ( relationship 'archivo'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'facturacionClinica'  ( refToType 'Archivo' 'Nucleo'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'EjercicioFacturacionAlquiler'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'EjercicioFacturacion' 'Facturacion'  ) 
			 )
			nil
			(relationships
			  ( relationshipRefinement 'periodosFacturacion'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				'periodo'
				''
				''
				''
				( refToInverseRelationship 'ejercicioFacturacion'  ( refToType 'PeriodoFacturacionAlquiler' 'Facturacion'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'periodosFacturacion'  ( refToType 'EjercicioFacturacion' 'Facturacion'  )  ) 
				 )
				( kind:  )
			   )

			  ( relationshipRefinement 'facturacion'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'ejerciciosFacturacion'  ( refToType 'FacturacionAlquiler' 'Facturacion'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'facturacion'  ( refToType 'EjercicioFacturacion' 'Facturacion'  )  ) 
				 )
				( kind:  )
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'EjercicioFacturacionClinica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'EjercicioFacturacion' 'Facturacion'  ) 
			 )
			nil
			(relationships
			  ( relationshipRefinement 'periodosFacturacion'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				'periodo'
				''
				''
				''
				( refToInverseRelationship 'ejercicioFacturacion'  ( refToType 'PeriodoFacturacionClinica' 'Facturacion'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'periodosFacturacion'  ( refToType 'EjercicioFacturacion' 'Facturacion'  )  ) 
				 )
				( kind:  )
			   )

			  ( relationshipRefinement 'facturacion'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'ejerciciosFacturacion'  ( refToType 'FacturacionClinica' 'Facturacion'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'facturacion'  ( refToType 'EjercicioFacturacion' 'Facturacion'  )  ) 
				 )
				( kind:  )
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'PeriodoFacturacionAlquiler'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'PeriodoFacturacion' 'Facturacion'  ) 
			 )
			nil
			(relationships
			  ( relationshipRefinement 'facturas'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				'numeroFactura'
				''
				''
				''
				( refToInverseRelationship 'periodoFacturacion'  ( refToType 'FacturaAlquiler' 'Facturacion'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'facturas'  ( refToType 'PeriodoFacturacion' 'Facturacion'  )  ) 
				 )
				( kind:  )
			   )

			  ( relationshipRefinement 'ejercicioFacturacion'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'periodosFacturacion'  ( refToType 'EjercicioFacturacionAlquiler' 'Facturacion'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'ejercicioFacturacion'  ( refToType 'PeriodoFacturacion' 'Facturacion'  )  ) 
				 )
				( kind:  )
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'PeriodoFacturacionClinica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'PeriodoFacturacion' 'Facturacion'  ) 
			 )
			nil
			(relationships
			  ( relationshipRefinement 'facturas'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				'numeroFactura'
				''
				''
				''
				( refToInverseRelationship 'periodoFacturacion'  ( refToType 'FacturaClinica' 'Facturacion'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'facturas'  ( refToType 'PeriodoFacturacion' 'Facturacion'  )  ) 
				 )
				( kind:  )
			   )

			  ( relationshipRefinement 'ejercicioFacturacion'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'periodosFacturacion'  ( refToType 'EjercicioFacturacionClinica' 'Facturacion'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'ejercicioFacturacion'  ( refToType 'PeriodoFacturacion' 'Facturacion'  )  ) 
				 )
				( kind:  )
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'LineaFacturaAlquilerAbstracta'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'LineaFactura' 'Facturacion'  ) 
			 )
			(attributes
			  ( attributeRefinement 'concepto'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral Concepto Linea Factura Alquiler'
				''
				 ( refToType 'ConceptoLineaFactura' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'concepto'  ( refToType 'LineaFactura' 'Facturacion'  )  ) 
				 )
				( initializationExpression:  )
			   )

			  ( attribute 'precioUnitario'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral'
				''
				 ( refToType 'PrecioUnitarioLineaFactura' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'numeroUnidades'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral '
				''
				 ( refToType 'NumeroUnidadesLineaFacturaAlquiler' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'precio'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'calc precioUnitario * numeroUnidades'
				''
				 ( refToType 'PrecioLineaFactura' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'nombreUnidadFacturable'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral Unidad'
				''
				 ( refToType 'NombreUnidadFacturableAlquiler' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationshipRefinement 'factura'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'lineasFactura'  ( refToType 'FacturaAlquiler' 'Facturacion'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'factura'  ( refToType 'LineaFactura' 'Facturacion'  )  ) 
				 )
				( kind:  )
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'ClienteFacturaAlquilerPersonaFisica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'ClienteFacturaAlquiler' 'Facturacion'  ) 
			   ( refToType 'PersonaFisica' 'TiposGenerales'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'ClienteFacturaAlquilerPersonaJuridica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'ClienteFacturaAlquiler' 'Facturacion'  ) 
			   ( refToType 'PersonaJuridica' 'TiposGenerales'  ) 
			   ( refToType 'ConPersonasContacto' 'ContenedoresDeTipos'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'FacturaClinicaTrabajador'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'FacturaClinica' 'Facturacion'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'FacturaClinicaColegio'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'FacturaClinica' 'Facturacion'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'FacturaClinicaAseguradoTotal'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'FacturaClinica' 'Facturacion'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'FacturaClinicaAseguradoDetallada'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'FacturaClinica' 'Facturacion'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'FacturaClinicaParticular'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'FacturaClinica' 'Facturacion'  ) 
			 )
			nil
			(relationships
			  ( relationshipRefinement 'clienteFactura'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate periodoFacturacion ejercicioFacturacion facturacion archivo historias paciente'
				''
				''
				( refToInverseRelationship 'facturas'  ( refToType 'Paciente' 'Nucleo'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'clienteFactura'  ( refToType 'FacturaClinica' 'Facturacion'  )  ) 
				 )
				( candidatesPath:  )
			   )

			  ( relationshipRefinement 'visitasFacturadas'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false true false false false false
				NOCOMPUTATION
				''
				'nocreate clienteFactura historia visitas'
				'calc clonenot factura :isnil'
				''
				( refToInverseRelationship 'factura'  ( refToType 'VisitaParticular' 'Nucleo'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'visitasFacturadas'  ( refToType 'FacturaClinica' 'Facturacion'  )  ) 
				 )
				( isChangeable: kind: candidatesConstraint: candidatesPath:  )
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'LineaFacturaClinicaArbitraria'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'LineaFacturaClinica' 'Facturacion'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'LineaFacturaClinicaPorTarifa'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'LineaFacturaClinica' 'Facturacion'  ) 
			 )
			(attributes
			  ( attributeRefinement 'concepto'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				AFTERCONNECTION
				''
				''
				''
				'clone precioTarifaClinica conceptoClinica nombreConcepto'
				''
				 ( refToType 'ConceptoLineaFactura' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'concepto'  ( refToType 'LineaFactura' 'Facturacion'  )  ) 
				 )
				( initializationExpression: computationKind: isChangeable:  )
			   )

			  ( attributeRefinement 'precioUnitario'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				AFTERCONNECTION
				''
				''
				''
				'clone precioTarifaClinica valorPrecio'
				''
				 ( refToType 'PrecioUnitarioLineaFactura' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'precioUnitario'  ( refToType 'LineaFacturaClinica' 'Facturacion'  )  ) 
				 )
				( initializationExpression: computationKind: isChangeable:  )
			   )

			 )
			(relationships
			  ( relationship 'precioTarifaClinica'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate factura visitasFacturadas tarifaClinica preciosTarifaClinica'
				''
				''
				( refToInverseRelationship 'lineasFactura'  ( refToType 'PrecioTarifaClinica' 'Tarifas'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'LineaFacturaClinicaPorActos'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'LineaFacturaClinica' 'Facturacion'  ) 
			 )
			(attributes
			  ( attributeRefinement 'concepto'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				AFTERCONNECTION
				''
				''
				''
				'clonenot actoClinicoFacturado claseActo'
				''
				 ( refToType 'ConceptoLineaFactura' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'concepto'  ( refToType 'LineaFactura' 'Facturacion'  )  ) 
				 )
				( initializationExpression: computationKind: isChangeable:  )
			   )

			  ( attributeRefinement 'numeroUnidades'
				nil nil
				nil
				nil
				'' nil
				#'0' #'0'
				false true false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'NumeroUnidadesLineaFacturaClinica' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'numeroUnidades'  ( refToType 'LineaFacturaClinica' 'Facturacion'  )  ) 
				 )
				nil
			   )

			  ( attributeRefinement 'precioUnitario'
				nil nil
				nil
				nil
				'' nil
				#'0' #'0'
				false true false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'PrecioUnitarioLineaFactura' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'precioUnitario'  ( refToType 'LineaFacturaClinica' 'Facturacion'  )  ) 
				 )
				nil
			   )

			  ( attributeRefinement 'precio'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				AFTERCONNECTION
				''
				''
				''
				'clonenot actoClinicoFacturado totalFacturable'
				''
				 ( refToType 'PrecioLineaFactura' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'precio'  ( refToType 'LineaFacturaClinica' 'Facturacion'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			 )
			(relationships
			  ( relationship 'actoClinicoFacturado'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate factura visitasFacturadas actosClinicos'
				'calc clonenot lineaFactura :isnil'
				''
				( refToInverseRelationship 'lineaFactura'  ( refToType 'ActoClinico' 'Nucleo'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'PrestacionClinicaFacturable'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'numeroUnidades'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal 1'
				''
				 ( refToType 'NumeroUnidadesLineaFacturaClinica' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'nombrePrecio'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				AFTERCONNECTION
				''
				''
				''
				'clonenot precioTarifaClinicas nombrePrecio'
				''
				 ( refToType 'NombrePrecioTarifaClinica' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'precioUnitario'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				AFTERCONNECTION
				''
				''
				''
				'clonenot precioTarifaClinicas valorPrecio'
				''
				 ( refToType 'PrecioUnitarioLineaFactura' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'precio'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'calc clonenot numeroUnidades * precioUnitario'
				''
				 ( refToType 'PrecioLineaFactura' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'precioTarifaClinicas'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate actoClinico visita tarifaClinica preciosTarifaClinica'
				''
				''
				( refToInverseRelationship 'prestacionesClinicaFacturables'  ( refToType 'PrecioTarifaClinica' 'Tarifas'  )  ) 
				false
			   )

			  ( relationship 'actoClinico'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'prestacionesClinicaFacturables'  ( refToType 'ActoClinico' 'Nucleo'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'LineaFacturaAlquilerNoPorTarifa'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'LineaFacturaAlquilerAbstracta' 'Facturacion'  ) 
			 )
			(attributes
			  ( attributeRefinement 'concepto'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral Concepto Linea Factura de Alquiler no por Tarifa'
				''
				 ( refToType 'ConceptoLineaFactura' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'concepto'  ( refToType 'LineaFacturaAlquilerAbstracta' 'Facturacion'  )  ) 
				 )
				( isChangeable: initializationExpression:  )
			   )

			  ( attributeRefinement 'precioUnitario'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral'
				''
				 ( refToType 'PrecioUnitarioLineaFactura' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'precioUnitario'  ( refToType 'LineaFacturaAlquilerAbstracta' 'Facturacion'  )  ) 
				 )
				( isChangeable:  )
			   )

			  ( attributeRefinement 'nombreUnidadFacturable'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral Unidad'
				''
				 ( refToType 'NombreUnidadFacturableAlquiler' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'nombreUnidadFacturable'  ( refToType 'LineaFacturaAlquilerAbstracta' 'Facturacion'  )  ) 
				 )
				( isChangeable:  )
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'LineaFacturaAlquilerPorTarifa'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'LineaFacturaAlquilerAbstracta' 'Facturacion'  ) 
			 )
			(attributes
			  ( attributeRefinement 'concepto'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				AFTERCONNECTION
				''
				''
				''
				'clone precioTarifaAlquiler conceptoAlquiler nombreConcepto'
				''
				 ( refToType 'ConceptoLineaFactura' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'concepto'  ( refToType 'LineaFacturaAlquilerAbstracta' 'Facturacion'  )  ) 
				 )
				( initializationExpression: computationKind: isChangeable:  )
			   )

			  ( attributeRefinement 'precioUnitario'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				AFTERCONNECTION
				''
				''
				''
				'clone precioTarifaAlquiler valorPrecio'
				''
				 ( refToType 'PrecioUnitarioLineaFactura' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'precioUnitario'  ( refToType 'LineaFacturaAlquilerAbstracta' 'Facturacion'  )  ) 
				 )
				( initializationExpression: computationKind: isChangeable:  )
			   )

			  ( attributeRefinement 'nombreUnidadFacturable'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				AFTERCONNECTION
				''
				''
				''
				'clone precioTarifaAlquiler conceptoAlquiler nombreUnidadFacturable'
				''
				 ( refToType 'NombreUnidadFacturableAlquiler' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'nombreUnidadFacturable'  ( refToType 'LineaFacturaAlquilerAbstracta' 'Facturacion'  )  ) 
				 )
				( initializationExpression: computationKind: isChangeable:  )
			   )

			 )
			(relationships
			  ( relationship 'precioTarifaAlquiler'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate factura periodoFacturacion ejercicioFacturacion facturacion catalogoConceptosAlquiler tarifaActualAlquiler preciosTarifaAlquiler'
				''
				''
				( refToInverseRelationship 'lineasFacturaAlquiler'  ( refToType 'PrecioTarifaAlquiler' 'Tarifas'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		 )
		nil
	   )

	  ( module 'TiposGenerales'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'PersonaFisicaOJuridica'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			   ( refToType 'ConDirecciones' 'ContenedoresDeTipos'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'PersonaFisica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'PersonaFisicaOJuridica' 'TiposGenerales'  ) 
			 )
			(attributes
			  ( attribute 'dninif'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral nif '
				''
				 ( refToType 'DNINIF' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'nombre'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral nombre'
				''
				 ( refToType 'NombrePersona' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'apellidos'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral apellidos apellidos'
				''
				 ( refToType 'ApellidosPersona' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'fechaNacimiento'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'FechaNacimiento' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'sexo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'Sexo' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'apellidosYNombre'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'calc clonenot apellidos , "," , nombre'
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

				false
			   )

			  ( attribute 'nifApellidosYNombre'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'calc clonenot dninif , "_" , apellidos , "," , nombre'
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'personaContacto'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'PersonaJuridica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'PersonaFisicaOJuridica' 'TiposGenerales'  ) 
			 )
			(attributes
			  ( attribute 'razonSocial'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral razon social '
				''
				 ( refToType 'RazonSocial' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'nombreComercial'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral nombre comercial '
				''
				 ( refToType 'NombreComercial' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'cif'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral cif '
				''
				 ( refToType 'CIF' 'TiposBasicos'  ) 

				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'Contacto'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'tipoContacto'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal Primario'
				''
				 ( refToType 'TipoContacto' 'Enumeraciones'  ) 

				false
			   )

			  ( attribute 'otroTipoContacto'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'OtrosTiposContacto' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'fechaInicioValidez'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'FechaInicioValidez' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'fechaFinValidez'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'FechaFinValidez' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'conocedorContacto'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'contactos'  ( refToType 'ConPersonasContacto' 'ContenedoresDeTipos'  )  ) 
				false
			   )

			  ( relationship 'personaContacto'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				( refToInverseRelationship 'contacto'  ( refToType 'PersonaContacto' 'TiposGenerales'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Atestatario'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'PersonaFisica' 'TiposGenerales'  ) 
			 )
			(attributes
			  ( attributeRefinement 'apellidosYNombre'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				ALWAYS
				''
				''
				''
				'calc clonenot apellidos , "," , nombre'
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'apellidosYNombre'  ( refToType 'PersonaFisica' 'TiposGenerales'  )  ) 
				 )
				( isNameAttribute:  )
			   )

			  ( attributeRefinement 'dninif'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral nif '
				''
				 ( refToType 'DNINIF' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'dninif'  ( refToType 'PersonaFisica' 'TiposGenerales'  )  ) 
				 )
				( isNameAttribute:  )
			   )

			 )
			(relationships
			  ( relationship 'atestados'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'atestatario'  ( refToType 'Atestado' 'TiposCompuestos'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'AtestatarioRepresentado'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'PersonaFisica' 'TiposGenerales'  ) 
			 )
			(attributes
			  ( attributeRefinement 'apellidosYNombre'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				ALWAYS
				''
				''
				''
				'calc clonenot apellidos , "," , nombre'
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'apellidosYNombre'  ( refToType 'PersonaFisica' 'TiposGenerales'  )  ) 
				 )
				( isNameAttribute:  )
			   )

			  ( attributeRefinement 'dninif'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral nif '
				''
				 ( refToType 'DNINIF' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'dninif'  ( refToType 'PersonaFisica' 'TiposGenerales'  )  ) 
				 )
				( isNameAttribute: isInitializationPropagationAllowed: isInitializationPropagationOnConnectAllowed:  )
			   )

			 )
			(relationships
			  ( relationship 'representadoEnAtestados'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'enRepresentacionDe'  ( refToType 'Atestado' 'TiposCompuestos'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Inspector'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'PersonaFisica' 'TiposGenerales'  ) 
			   ( refToType 'Atestatario' 'TiposGenerales'  ) 
			   ( refToType 'AtestatarioRepresentado' 'TiposGenerales'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'PersonaContacto'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'PersonaFisica' 'TiposGenerales'  ) 
			   ( refToType 'ConNumerosTelecomunicacion' 'ContenedoresDeTipos'  ) 
			 )
			nil
			(relationships
			  ( relationship 'contacto'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'personaContacto'  ( refToType 'Contacto' 'TiposGenerales'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		 )
		nil
	   )

	  ( module 'Tarifas'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'ConceptoClinica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'nombreConcepto'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral concepto clinica'
				''
				 ( refToType 'NombreConceptoClinica' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'descripcionConcepto'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral descripcion del concepto de clinica'
				''
				 ( refToType 'DescripcionConceptoClinica' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'nombreUnidadFacturable'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral unidadFacturable'
				''
				 ( refToType 'NombreUnidadFacturableClinica' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'codigoConceptoClinica'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral concepto clinica'
				''
				 ( refToType 'CodigoConceptoClinica' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'codigoConceptoPagadorPrestaciones'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral codigo concepto clinica por el pagador de prestaciones'
				''
				 ( refToType 'CodigoConceptoPagadorPrestaciones' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'nombreCatalogo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'clonenot catalogoConceptos nombreCatalogo'
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'preciosTarifaClinica'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'conceptoClinica'  ( refToType 'PrecioTarifaClinica' 'Tarifas'  )  ) 
				false
			   )

			  ( relationship 'especificoDeCatalogo'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				nil
				false
			   )

			  ( relationship 'enTodosConceptosDeCatalogo'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'todosConceptos'  ( refToType 'CatalogoConceptosClinicaEspecifico' 'Tarifas'  )  ) 
				false
			   )

			  ( relationship 'catalogoConceptos'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'conceptos'  ( refToType 'CatalogoConceptosClinicaAbstracto' 'Tarifas'  )  ) 
				false
			   )

			  ( relationshipRefinement 'enTodosConceptosDeCatalogo'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'todosConceptos'  ( refToType 'CatalogoConceptosClinicaEspecifico' 'Tarifas'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'enTodosConceptosDeCatalogo'  ( refToType 'ConceptoClinica' 'Tarifas'  )  ) 
				 )
				( kind:  )
			   )

			  ( relationship 'enTodosConceptosDeCatalogo'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'todosConceptos'  ( refToType 'CatalogoConceptosClinicaAbstracto' 'Tarifas'  )  ) 
				false
			   )

			  ( relationship 'enTodosConceptosDeCatalogo'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'todosConceptos'  ( refToType 'CatalogoConceptosClinicaEspecifico' 'Tarifas'  )  ) 
				false
			   )

			  ( relationship 'enTodosConceptosDeCatalogo'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'todosConceptos'  ( refToType 'CatalogoConceptosClinicaAbstracto' 'Tarifas'  )  ) 
				false
			   )

			  ( relationship 'enTodosConceptosDeCatalogo'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'todosConceptos'  ( refToType 'CatalogoConceptosClinicaEspecifico' 'Tarifas'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'TarifaClinicaAbstracta'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'nombreTarifa'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'$self clonenot catalogoConceptosClinica nombreCatalogo'
				''
				 ( refToType 'NombreTarifaClinica' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'fechaInicioValidez'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'FechaInicioValidez' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'fechaFinValidez'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'FechaFinValidez' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'nombreCatalogo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'clonenot catalogoConceptosClinica nombreCatalogo'
				''
				 ( refToType 'NombreCatalogoConceptosClinica' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'preciosTarifaClinica'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'tarifaClinica'  ( refToType 'PrecioTarifaClinica' 'Tarifas'  )  ) 
				false
			   )

			  ( relationship 'catalogoConceptosClinica'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate facturacionClinica catalogosConceptosClinica'
				''
				''
				( refToInverseRelationship 'tarifasClinica'  ( refToType 'CatalogoConceptosClinicaAbstracto' 'Tarifas'  )  ) 
				false
			   )

			  ( relationship 'visitasParticulares'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				nil
				false
			   )

			  ( relationship 'tarifaActualDeCatalogoConceptosClinica'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'tarifaActualClinica'  ( refToType 'CatalogoConceptosClinicaAbstracto' 'Tarifas'  )  ) 
				false
			   )

			  ( relationship 'visitas'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'tarifaClinica'  ( refToType 'Visita' 'Nucleo'  )  ) 
				false
			   )

			  ( relationshipRefinement 'visitas'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'tarifaClinica'  ( refToType 'VisitaRemitida' 'Nucleo'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'visitas'  ( refToType 'TarifaClinicaAbstracta' 'Tarifas'  )  ) 
				 )
				( kind:  )
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'CatalogoConceptosClinicaAbstracto'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'nombreCatalogo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false true false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral catalogo conceptos clinica'
				''
				 ( refToType 'NombreCatalogoConceptosClinica' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'tarifasClinica'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'catalogoConceptosClinica'  ( refToType 'TarifaClinicaAbstracta' 'Tarifas'  )  ) 
				false
			   )

			  ( relationship 'tarifaActualClinica'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false true false false false false
				NOCOMPUTATION
				''
				'noclone tarifasClinica'
				''
				''
				( refToInverseRelationship 'tarifaActualDeCatalogoConceptosClinica'  ( refToType 'TarifaClinicaAbstracta' 'Tarifas'  )  ) 
				false
			   )

			  ( relationship 'todosConceptos'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false true false false false false
				ALWAYS
				'nombreConcepto'
				''
				''
				'nocreate conceptos'
				( refToInverseRelationship 'enTodosConceptosDeCatalogo'  ( refToType 'ConceptoClinica' 'Tarifas'  )  ) 
				false
			   )

			  ( relationship 'conceptos'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				'nombreConcepto'
				''
				''
				''
				( refToInverseRelationship 'catalogoConceptos'  ( refToType 'ConceptoClinica' 'Tarifas'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'PrecioTarifaClinica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'nombrePrecio'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false true
				AFTERCONNECTION
				''
				''
				''
				'calc clonenot "precio_" , conceptoClinica nombreConcepto'
				''
				 ( refToType 'NombrePrecioTarifaClinica' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'valorPrecio'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral '
				''
				 ( refToType 'ValorPrecioClinica' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'nombreTarifa'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'clonenot tarifaClinica nombreTarifa'
				''
				 ( refToType 'NombreTarifaClinica' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'nombreCatalogo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'clonenot tarifaClinica catalogoConceptosClinica nombreCatalogo'
				''
				 ( refToType 'NombreCatalogoConceptosClinica' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'conceptoClinica'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate tarifaClinica catalogoConceptosClinica todosConceptos'
				''
				''
				( refToInverseRelationship 'preciosTarifaClinica'  ( refToType 'ConceptoClinica' 'Tarifas'  )  ) 
				false
			   )

			  ( relationship 'tarifaClinica'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'preciosTarifaClinica'  ( refToType 'TarifaClinicaAbstracta' 'Tarifas'  )  ) 
				false
			   )

			  ( relationship 'lineasFactura'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'precioTarifaClinica'  ( refToType 'LineaFacturaClinicaPorTarifa' 'Facturacion'  )  ) 
				false
			   )

			  ( relationship 'prestacionesClinicaFacturables'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'precioTarifaClinicas'  ( refToType 'PrestacionClinicaFacturable' 'Facturacion'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'ConceptoAlquiler'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'nombreConcepto'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral concepto alquiler'
				''
				 ( refToType 'NombreConceptoAlquiler' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'descripcionConcepto'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral descripcion del concepto de alquiler clinicas'
				''
				 ( refToType 'DescripcionConceptoAlquiler' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'nombreUnidadFacturable'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral unidadFacturable'
				''
				 ( refToType 'NombreUnidadFacturableAlquiler' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'catalogoConceptosAlquiler'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'conceptosAlquiler'  ( refToType 'CatalogoConceptosAlquiler' 'Tarifas'  )  ) 
				false
			   )

			  ( relationship 'preciosTarifaAlquiler'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'conceptoAlquiler'  ( refToType 'PrecioTarifaAlquiler' 'Tarifas'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'TarifaAlquiler'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'nombreTarifa'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'clone catalogoConceptosAlquiler nombreCatalogo'
				''
				 ( refToType 'NombreTarifaAlquiler' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'fechaInicioValidez'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'FechaInicioValidez' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'fechaFinValidez'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'FechaFinValidez' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'preciosTarifaAlquiler'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'tarifaAlquiler'  ( refToType 'PrecioTarifaAlquiler' 'Tarifas'  )  ) 
				false
			   )

			  ( relationship 'catalogoConceptosAlquiler'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate facturacionAlquiler catalogosConceptosAlquiler'
				''
				''
				( refToInverseRelationship 'tarifasAlquiler'  ( refToType 'CatalogoConceptosAlquiler' 'Tarifas'  )  ) 
				false
			   )

			  ( relationship 'tarifaActualDecatalogoConceptosAlquiler'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'tarifaActualAlquiler'  ( refToType 'CatalogoConceptosAlquiler' 'Tarifas'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'CatalogoConceptosAlquiler'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'nombreCatalogo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALVALUE
				''
				''
				''
				'literal Catalogo Conceptos de Alquiler'
				''
				 ( refToType 'NombreCatalogoConceptosAlquiler' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'conceptosAlquiler'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false false false
				NOCOMPUTATION
				'nombreConcepto'
				''
				''
				''
				( refToInverseRelationship 'catalogoConceptosAlquiler'  ( refToType 'ConceptoAlquiler' 'Tarifas'  )  ) 
				false
			   )

			  ( relationship 'tarifasAlquiler'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'catalogoConceptosAlquiler'  ( refToType 'TarifaAlquiler' 'Tarifas'  )  ) 
				false
			   )

			  ( relationship 'facturacionAlquiler'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'catalogoConceptosAlquiler'  ( refToType 'FacturacionAlquiler' 'Facturacion'  )  ) 
				false
			   )

			  ( relationship 'tarifaActualAlquiler'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false true false false false false
				NOCOMPUTATION
				''
				'nocreate tarifasAlquiler'
				''
				''
				( refToInverseRelationship 'tarifaActualDecatalogoConceptosAlquiler'  ( refToType 'TarifaAlquiler' 'Tarifas'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'PrecioTarifaAlquiler'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'nombrePrecio'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false true
				AFTERCONNECTION
				''
				''
				''
				'clone conceptoAlquiler nombreConcepto'
				''
				 ( refToType 'NombrePrecioTarifaAlquiler' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'valorPrecio'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral '
				''
				 ( refToType 'ValorPrecioAlquiler' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'conceptoAlquiler'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false true false false
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate tarifaAlquiler catalogoConceptosAlquiler conceptosAlquiler'
				''
				''
				( refToInverseRelationship 'preciosTarifaAlquiler'  ( refToType 'ConceptoAlquiler' 'Tarifas'  )  ) 
				false
			   )

			  ( relationship 'tarifaAlquiler'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'preciosTarifaAlquiler'  ( refToType 'TarifaAlquiler' 'Tarifas'  )  ) 
				false
			   )

			  ( relationship 'lineasFacturaAlquiler'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'precioTarifaAlquiler'  ( refToType 'LineaFacturaAlquilerPorTarifa' 'Facturacion'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'TarifaClinicaDeAseguradora'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'TarifaClinicaAbstracta' 'Tarifas'  ) 
			 )
			nil
			(relationships
			  ( relationshipRefinement 'catalogoConceptosClinica'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate facturacionClinica catalogosConceptosClinica'
				''
				''
				( refToInverseRelationship 'tarifasClinica'  ( refToType 'CatalogoConceptosClinicaDeAseguradora' 'Tarifas'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'catalogoConceptosClinica'  ( refToType 'TarifaClinicaAbstracta' 'Tarifas'  )  ) 
				 )
				( isInitializationPropagationAllowed: kind: isInitializationPropagationOnConnectAllowed:  )
			   )

			  ( relationshipRefinement 'tarifaActualDeCatalogoConceptosClinica'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false false false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'tarifaActualClinica'  ( refToType 'CatalogoConceptosClinicaDeAseguradora' 'Tarifas'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'tarifaActualDeCatalogoConceptosClinica'  ( refToType 'TarifaClinicaAbstracta' 'Tarifas'  )  ) 
				 )
				( isInitializationPropagationAllowed: kind: isInitializationPropagationOnConnectAllowed:  )
			   )

			  ( relationshipRefinement 'tarifaActualDeCatalogoConceptosClinica'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'tarifaActualClinica'  ( refToType 'CatalogoConceptosClinicaDeAseguradora' 'Tarifas'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'tarifaActualDeCatalogoConceptosClinica'  ( refToType 'TarifaClinicaAbstracta' 'Tarifas'  )  ) 
				 )
				( kind:  )
			   )

			  ( relationship 'tarifaActualDeCatalogoConceptosClinica'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'tarifaActualClinica'  ( refToType 'CatalogoConceptosClinicaDeAseguradora' 'Tarifas'  )  ) 
				false
			   )

			  ( relationship 'tarifaActualDeCatalogoConceptosClinica'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'tarifaActualClinica'  ( refToType 'CatalogoConceptosClinicaDeAseguradora' 'Tarifas'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'CatalogoConceptosClinicaDeAseguradora'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CatalogoConceptosClinicaAbstracto' 'Tarifas'  ) 
			 )
			(attributes
			  ( attributeRefinement 'nombreCatalogo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				AFTERCONNECTION
				''
				''
				''
				'calc clonenot  "Catalogo_del_Plan_" , planPagoPrestaciones nombrePlanPagoPrestaciones'
				''
				 ( refToType 'NombreCatalogoConceptosClinica' 'TiposBasicos'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'nombreCatalogo'  ( refToType 'CatalogoConceptosClinicaAbstracto' 'Tarifas'  )  ) 
				 )
				( computationKind: isDuplicatesAllowed: initializationExpression:  )
			   )

			 )
			(relationships
			  ( relationshipRefinement 'tarifasClinica'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'catalogoConceptosClinica'  ( refToType 'TarifaClinicaDeAseguradora' 'Tarifas'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'tarifasClinica'  ( refToType 'CatalogoConceptosClinicaAbstracto' 'Tarifas'  )  ) 
				 )
				( kind:  )
			   )

			  ( relationship 'planPagoPrestaciones'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				nil
				false
			   )

			  ( relationshipRefinement 'tarifaActualClinica'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false true false false false false
				NOCOMPUTATION
				''
				'noclone tarifasClinica'
				''
				''
				( refToInverseRelationship 'tarifaActualDeCatalogoConceptosClinica'  ( refToType 'TarifaClinicaDeAseguradora' 'Tarifas'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'tarifaActualClinica'  ( refToType 'CatalogoConceptosClinicaAbstracto' 'Tarifas'  )  ) 
				 )
				( kind:  )
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'CatalogoConceptosClinicaDeClinica'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CatalogoConceptosClinicaAbstracto' 'Tarifas'  ) 
			 )
			nil
			(relationships
			  ( relationship 'facturacionClinica'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'catalogosConceptosClinica'  ( refToType 'FacturacionClinica' 'Facturacion'  )  ) 
				false
			   )

			  ( relationshipRefinement 'tarifasClinica'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'catalogoConceptosClinica'  ( refToType 'TarifaClinica' 'Tarifas'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'tarifasClinica'  ( refToType 'CatalogoConceptosClinicaAbstracto' 'Tarifas'  )  ) 
				 )
				( kind:  )
			   )

			  ( relationship 'planesPagoPrestaciones'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'catalogoConceptosClinica'  ( refToType 'PlanPagoPrestacionesConTarifaDeClinica' 'Organizaciones'  )  ) 
				false
			   )

			  ( relationship 'visitasParticulares'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'catalogoConceptosClinica'  ( refToType 'VisitaParticular' 'Nucleo'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'TarifaClinica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'TarifaClinicaAbstracta' 'Tarifas'  ) 
			 )
			nil
			(relationships
			  ( relationshipRefinement 'catalogoConceptosClinica'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate facturacionClinica catalogosConceptosClinica'
				''
				''
				( refToInverseRelationship 'tarifasClinica'  ( refToType 'CatalogoConceptosClinicaDeClinica' 'Tarifas'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'catalogoConceptosClinica'  ( refToType 'TarifaClinicaAbstracta' 'Tarifas'  )  ) 
				 )
				( isInitializationPropagationAllowed: kind: isInitializationPropagationOnConnectAllowed:  )
			   )

			  ( relationshipRefinement 'visitas'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'tarifaClinica'  ( refToType 'VisitaParticular' 'Nucleo'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'visitas'  ( refToType 'TarifaClinicaAbstracta' 'Tarifas'  )  ) 
				 )
				( kind:  )
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'CatalogoConceptosClinicaGeneral'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CatalogoConceptosClinicaDeClinica' 'Tarifas'  ) 
			 )
			nil
			(relationships
			  ( relationship 'catalogosConceptosClinicaEspecificos'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'catalogoConceptosClinicaGeneral'  ( refToType 'CatalogoConceptosClinicaEspecifico' 'Tarifas'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'CatalogoConceptosClinicaEspecifico'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CatalogoConceptosClinicaDeClinica' 'Tarifas'  ) 
			 )
			nil
			(relationships
			  ( relationship 'catalogoConceptosClinicaGeneral'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate facturacionClinica catalogosConceptosClinica'
				''
				''
				( refToInverseRelationship 'catalogosConceptosClinicaEspecificos'  ( refToType 'CatalogoConceptosClinicaGeneral' 'Tarifas'  )  ) 
				false
			   )

			  ( relationshipRefinement 'todosConceptos'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false true false false false false
				ALWAYS
				'nombreConcepto'
				''
				''
				'calc clonenot conceptos union: catalogoConceptosClinicaGeneral conceptos'
				( refToInverseRelationship 'enTodosConceptosDeCatalogo'  ( refToType 'ConceptoClinica' 'Tarifas'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'todosConceptos'  ( refToType 'CatalogoConceptosClinicaAbstracto' 'Tarifas'  )  ) 
				 )
				( kind: initializationExpression:  )
			   )

			 )
			nil
			nil
			false
		   )

		 )
		nil
	   )

	  ( module 'TiposRelacionales'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'ReferenciaAHistoria'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'modoRelacionHistoria'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false true false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'ModoRelacionHistoria' 'Enumeraciones'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'historiaReferenciada'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				'noclone historiaReferenciante archivo historias'
				''
				''
				( refToInverseRelationship 'referenciadaPorHistorias'  ( refToType 'Historia' 'Nucleo'  )  ) 
				false
			   )

			  ( relationship 'historiaReferenciante'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'referenciasAHistorias'  ( refToType 'Historia' 'Nucleo'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'ParticipacionActoClinico'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'modoParticipacion'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal Realizador'
				''
				 ( refToType 'ModoParticipacionActoClinico' 'Enumeraciones'  ) 

				false
			   )

			  ( attribute 'tiempoParticipacion'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal 1'
				''
				 ( refToType 'TiempoParticipacionActoClinico' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'unidadTiempoParticipacion'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal ActoCompleto'
				''
				 ( refToType 'UnidadTiempoParticipacionActoClinico' 'Enumeraciones'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'participanteActoClinicos'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate actoClinico visita historia archivo clinica personalClinica'
				''
				''
				( refToInverseRelationship 'participacionesActoClinico'  ( refToType 'ParticipanteActoClinico' 'Nucleo'  )  ) 
				false
			   )

			  ( relationship 'actoClinico'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'personalParticipante'  ( refToType 'ActoClinico' 'Nucleo'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'PuestoClinica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'tipoPuestoClinica'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'TipoPuestoClinica' 'Enumeraciones'  ) 

				false
			   )

			  ( attribute 'nombrePuestoClinica'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral PuestoEnClinica'
				''
				 ( refToType 'NombrePuestoClinica' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'fechaInicioValidez'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'FechaInicioValidez' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'fechaFinValidez'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'FechaFinValidez' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'nombreYTipoPuestoClinica'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				ALWAYS
				''
				''
				''
				'calc clonenot nombrePuestoClinica , "_(" , tipoPuestoClinica , ")"'
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'personaClinica'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate conPuestosClinica clinica personalClinica'
				''
				''
				( refToInverseRelationship 'puestosClinica'  ( refToType 'PersonaClinica' 'Nucleo'  )  ) 
				false
			   )

			  ( relationship 'puestoEn'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'puestosClinica'  ( refToType 'ConPuestosClinica' 'ContenedoresDeTipos'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		 )
		nil
	   )

	  ( module 'Aspectos'
		nil nil
		(definedAspects
		  ( aspect 'PresentationText'
			nil nil
		   )

		 )
		nil
		nil nil
		nil
		nil
	   )

	  ( module 'Organizaciones'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'Organizacion'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'PersonaJuridica' 'TiposGenerales'  ) 
			   ( refToType 'ConNumerosTelecomunicacion' 'ContenedoresDeTipos'  ) 
			   ( refToType 'ConPersonasContacto' 'ContenedoresDeTipos'  ) 
			   ( refToType 'ConSubOrganizaciones' 'ContenedoresDeTipos'  ) 
			 )
			(attributes
			  ( attribute 'direccionOficial'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'noclone direcciones'
				'calc modoUso same: "Oficial"'
				 ( refToType 'Direccion' 'TiposCompuestos'  ) 

				false
			   )

			  ( attribute 'direccionFacturaClinica'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'noclone direcciones'
				'calc modoUso same: "Factura"'
				 ( refToType 'Direccion' 'TiposCompuestos'  ) 

				false
			   )

			  ( attribute 'direccionNotificacion'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'noclone direcciones'
				'calc modoUso same: "Notificacion"'
				 ( refToType 'Direccion' 'TiposCompuestos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'superOrganizacion'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'subOrganizaciones'  ( refToType 'ConSubOrganizaciones' 'ContenedoresDeTipos'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'OrganizacionOrigenPacientes'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Organizacion' 'Organizaciones'  ) 
			 )
			(attributes
			  ( attribute 'tipoOrigenPaciente'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'TipoOrigenPaciente' 'Enumeraciones'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'archivo'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'organizacionesOrigenPacientes'  ( refToType 'Archivo' 'Nucleo'  )  ) 
				false
			   )

			  ( relationship 'gestionesPagoPrestaciones'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'organizacionOrigenPacientes'  ( refToType 'GestionPagoPrestaciones' 'Organizaciones'  )  ) 
				false
			   )

			  ( relationship 'contratosPagoPrestaciones'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'organizacionOrigenPacientes'  ( refToType 'ContratoPagoPrestaciones' 'Organizaciones'  )  ) 
				false
			   )

			  ( relationship 'origenesPacienteReferido'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				nil
				false
			   )

			  ( relationship 'visitasRemitidas'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'organizacionOrigenPaciente'  ( refToType 'VisitaRemitida' 'Nucleo'  )  ) 
				false
			   )

			  ( relationship 'redundante_gestionesPagoPrestaciones'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				ALWAYS
				''
				''
				''
				'clonenot gestionesPagoPrestaciones'
				( refToInverseRelationship 'organizacionOrigenPacientesGestionada'  ( refToType 'GestionPagoPrestaciones' 'Organizaciones'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Colegio'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'OrganizacionOrigenPacientes' 'Organizaciones'  ) 
			   ( refToType 'NotificadoCambioEstadoAdministrativo' 'EstadosAdministrativos'  ) 
			 )
			(attributes
			  ( attributeRefinement 'tipoOrigenPaciente'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALVALUE
				''
				''
				''
				'literal Colegio'
				''
				 ( refToType 'TipoOrigenPaciente' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'tipoOrigenPaciente'  ( refToType 'OrganizacionOrigenPacientes' 'Organizaciones'  )  ) 
				 )
				( isNameAttribute: computationKind: initializationExpression:  )
			   )

			 )
			(relationships
			  ( relationshipRefinement 'subOrganizaciones'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'superOrganizacion'  ( refToType 'Colegio' 'Organizaciones'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'subOrganizaciones'  ( refToType 'ConSubOrganizaciones' 'ContenedoresDeTipos'  )  ) 
				 )
				( kind:  )
			   )

			  ( relationshipRefinement 'superOrganizacion'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'subOrganizaciones'  ( refToType 'Colegio' 'Organizaciones'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'superOrganizacion'  ( refToType 'Organizacion' 'Organizaciones'  )  ) 
				 )
				( isChangeable: isInitializationPropagationAllowed: kind: isInitializationPropagationOnConnectAllowed:  )
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Empresa'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'OrganizacionOrigenPacientes' 'Organizaciones'  ) 
			   ( refToType 'NotificadoCambioEstadoAdministrativo' 'EstadosAdministrativos'  ) 
			 )
			(attributes
			  ( attributeRefinement 'tipoOrigenPaciente'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALVALUE
				''
				''
				''
				'literal Empresa'
				''
				 ( refToType 'TipoOrigenPaciente' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'tipoOrigenPaciente'  ( refToType 'OrganizacionOrigenPacientes' 'Organizaciones'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			 )
			(relationships
			  ( relationshipRefinement 'subOrganizaciones'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'superOrganizacion'  ( refToType 'Empresa' 'Organizaciones'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'subOrganizaciones'  ( refToType 'ConSubOrganizaciones' 'ContenedoresDeTipos'  )  ) 
				 )
				( kind:  )
			   )

			  ( relationshipRefinement 'superOrganizacion'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'subOrganizaciones'  ( refToType 'Empresa' 'Organizaciones'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'superOrganizacion'  ( refToType 'Organizacion' 'Organizaciones'  )  ) 
				 )
				( isChangeable: isInitializationPropagationAllowed: kind: isInitializationPropagationOnConnectAllowed:  )
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Gestoria'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'OrganizacionGestoraPrestaciones' 'Organizaciones'  ) 
			   ( refToType 'NotificadoCambioEstadoAdministrativo' 'EstadosAdministrativos'  ) 
			 )
			(attributes
			  ( attributeRefinement 'tipoGestoraPrestaciones'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALVALUE
				''
				''
				''
				'literal Gestoria'
				''
				 ( refToType 'TipoGestoraPrestaciones' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'tipoGestoraPrestaciones'  ( refToType 'OrganizacionGestoraPrestaciones' 'Organizaciones'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			 )
			(relationships
			  ( relationshipRefinement 'subOrganizaciones'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'superOrganizacion'  ( refToType 'Gestoria' 'Organizaciones'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'subOrganizaciones'  ( refToType 'ConSubOrganizaciones' 'ContenedoresDeTipos'  )  ) 
				 )
				( kind:  )
			   )

			  ( relationshipRefinement 'superOrganizacion'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'subOrganizaciones'  ( refToType 'Gestoria' 'Organizaciones'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'superOrganizacion'  ( refToType 'Organizacion' 'Organizaciones'  )  ) 
				 )
				( isChangeable: isInitializationPropagationAllowed: kind: isInitializationPropagationOnConnectAllowed:  )
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'OrganizacionGestoraPrestaciones'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'Organizacion' 'Organizaciones'  ) 
			 )
			(attributes
			  ( attribute 'tipoGestoraPrestaciones'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'TipoGestoraPrestaciones' 'Enumeraciones'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'archivo'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'organizacionesGestorasPrestaciones'  ( refToType 'Archivo' 'Nucleo'  )  ) 
				false
			   )

			  ( relationship 'gestionesPagoPrestaciones'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'gestora'  ( refToType 'GestionPagoPrestaciones' 'Organizaciones'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'OrganizacionPagadoraPrestaciones'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'Organizacion' 'Organizaciones'  ) 
			   ( refToType 'ClienteFacturaClinica' 'Facturacion'  ) 
			 )
			(attributes
			  ( attribute 'tipoPagadorPrestaciones'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'TipoPagadorPrestaciones' 'Enumeraciones'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'archivo'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'organizacionesPagadorasPrestaciones'  ( refToType 'Archivo' 'Nucleo'  )  ) 
				false
			   )

			  ( relationship 'planesPagoPrestaciones'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'organizacionPagadoraPrestaciones'  ( refToType 'PlanPagoPrestaciones' 'Organizaciones'  )  ) 
				false
			   )

			  ( relationship 'contratosPagoPrestaciones'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'organizacionPagadoraPrestaciones'  ( refToType 'ContratoPagoPrestaciones' 'Organizaciones'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Aseguradora'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'OrganizacionPagadoraPrestaciones' 'Organizaciones'  ) 
			   ( refToType 'NotificadoCambioEstadoAdministrativo' 'EstadosAdministrativos'  ) 
			 )
			(attributes
			  ( attributeRefinement 'tipoPagadorPrestaciones'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALVALUE
				''
				''
				''
				'literal Aseguradora'
				''
				 ( refToType 'TipoPagadorPrestaciones' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'tipoPagadorPrestaciones'  ( refToType 'OrganizacionPagadoraPrestaciones' 'Organizaciones'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			 )
			(relationships
			  ( relationshipRefinement 'subOrganizaciones'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'superOrganizacion'  ( refToType 'Aseguradora' 'Organizaciones'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'subOrganizaciones'  ( refToType 'ConSubOrganizaciones' 'ContenedoresDeTipos'  )  ) 
				 )
				( kind:  )
			   )

			  ( relationshipRefinement 'superOrganizacion'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'subOrganizaciones'  ( refToType 'Aseguradora' 'Organizaciones'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'superOrganizacion'  ( refToType 'Organizacion' 'Organizaciones'  )  ) 
				 )
				( kind:  )
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Mutua'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'OrganizacionPagadoraPrestaciones' 'Organizaciones'  ) 
			   ( refToType 'OrganizacionGestoraPrestaciones' 'Organizaciones'  ) 
			   ( refToType 'NotificadoCambioEstadoAdministrativo' 'EstadosAdministrativos'  ) 
			 )
			(attributes
			  ( attributeRefinement 'tipoPagadorPrestaciones'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALVALUE
				''
				''
				''
				'literal Mutua'
				''
				 ( refToType 'TipoPagadorPrestaciones' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'tipoPagadorPrestaciones'  ( refToType 'OrganizacionPagadoraPrestaciones' 'Organizaciones'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attributeRefinement 'tipoGestoraPrestaciones'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALVALUE
				''
				''
				''
				'literal Mutua'
				''
				 ( refToType 'TipoGestoraPrestaciones' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'tipoGestoraPrestaciones'  ( refToType 'OrganizacionGestoraPrestaciones' 'Organizaciones'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			 )
			(relationships
			  ( relationshipRefinement 'subOrganizaciones'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'superOrganizacion'  ( refToType 'Mutua' 'Organizaciones'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'subOrganizaciones'  ( refToType 'ConSubOrganizaciones' 'ContenedoresDeTipos'  )  ) 
				 )
				( kind:  )
			   )

			  ( relationshipRefinement 'superOrganizacion'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'subOrganizaciones'  ( refToType 'Mutua' 'Organizaciones'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'superOrganizacion'  ( refToType 'Organizacion' 'Organizaciones'  )  ) 
				 )
				( isChangeable: isInitializationPropagationAllowed: kind: isInitializationPropagationOnConnectAllowed:  )
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'AdministracionSanidadPublica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'OrganizacionPagadoraPrestaciones' 'Organizaciones'  ) 
			   ( refToType 'OrganizacionOrigenPacientes' 'Organizaciones'  ) 
			   ( refToType 'NotificadoCambioEstadoAdministrativo' 'EstadosAdministrativos'  ) 
			 )
			(attributes
			  ( attributeRefinement 'tipoOrigenPaciente'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALVALUE
				''
				''
				''
				'literal SanidadPublica'
				''
				 ( refToType 'TipoOrigenPaciente' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'tipoOrigenPaciente'  ( refToType 'OrganizacionOrigenPacientes' 'Organizaciones'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attributeRefinement 'tipoPagadorPrestaciones'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Aseguradora'
				''
				 ( refToType 'TipoPagadorPrestaciones' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'tipoPagadorPrestaciones'  ( refToType 'OrganizacionPagadoraPrestaciones' 'Organizaciones'  )  ) 
				 )
				( computationKind: isInitializationPropagationAllowed: isInitializationPropagationOnConnectAllowed: initializationExpression:  )
			   )

			 )
			(relationships
			  ( relationshipRefinement 'subOrganizaciones'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'superOrganizacion'  ( refToType 'AdministracionSanidadPublica' 'Organizaciones'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'subOrganizaciones'  ( refToType 'ConSubOrganizaciones' 'ContenedoresDeTipos'  )  ) 
				 )
				( kind:  )
			   )

			  ( relationshipRefinement 'superOrganizacion'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'subOrganizaciones'  ( refToType 'AdministracionSanidadPublica' 'Organizaciones'  )  ) 
				false
				(refinedRelationships
				  ( refToRefinedRelationship 'superOrganizacion'  ( refToType 'Organizacion' 'Organizaciones'  )  ) 
				 )
				( kind:  )
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'GestionPagoPrestaciones'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'fechaInicioValidez'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'FechaInicioValidez' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'fechaFinValidez'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false false false
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'FechaFinValidez' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'razonSocialGestora'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				AFTERCONNECTION
				''
				''
				''
				'clonenot gestora razonSocial'
				''
				 ( refToType 'RazonSocial' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'organizacionOrigenPacientes'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'gestionesPagoPrestaciones'  ( refToType 'OrganizacionOrigenPacientes' 'Organizaciones'  )  ) 
				false
			   )

			  ( relationship 'gestora'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate organizacionOrigenPacientes archivo organizacionesGestorasPrestaciones'
				''
				''
				( refToInverseRelationship 'gestionesPagoPrestaciones'  ( refToType 'OrganizacionGestoraPrestaciones' 'Organizaciones'  )  ) 
				false
			   )

			  ( relationship 'organizacionOrigenPacientesGestionada'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				ALWAYS
				''
				''
				''
				'clonenot organizacionOrigenPacientes'
				( refToInverseRelationship 'redundante_gestionesPagoPrestaciones'  ( refToType 'OrganizacionOrigenPacientes' 'Organizaciones'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'ContratoPagoPrestaciones'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'nombreContratoPagoPrestaciones'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'NombreContratoPagoPrestaciones' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'fechaInicioValidez'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'FechaInicioValidez' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'fechaFinValidez'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false false false
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'FechaFinValidez' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'organizacionPagadoraPrestaciones'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate organizacionOrigenPacientes archivo organizacionesPagadorasPrestaciones'
				''
				''
				( refToInverseRelationship 'contratosPagoPrestaciones'  ( refToType 'OrganizacionPagadoraPrestaciones' 'Organizaciones'  )  ) 
				false
			   )

			  ( relationship 'planPagoPrestaciones'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'$self nocreate organizacionPagadoraPrestaciones planesPagoPrestaciones'
				''
				''
				( refToInverseRelationship 'contratosPagoPrestaciones'  ( refToType 'PlanPagoPrestaciones' 'Organizaciones'  )  ) 
				false
			   )

			  ( relationship 'organizacionOrigenPacientes'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'contratosPagoPrestaciones'  ( refToType 'OrganizacionOrigenPacientes' 'Organizaciones'  )  ) 
				false
			   )

			  ( relationship 'visitasRemitidas'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'contratoPagoPrestaciones'  ( refToType 'VisitaRemitida' 'Nucleo'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'PersonaAdministradora'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'PersonaFisica' 'TiposGenerales'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'PlanPagoPrestaciones'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'nombrePlanPagoPrestaciones'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral Plan X'
				''
				 ( refToType 'NombrePlanPagoPrestaciones' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'contratosPagoPrestaciones'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'planPagoPrestaciones'  ( refToType 'ContratoPagoPrestaciones' 'Organizaciones'  )  ) 
				false
			   )

			  ( relationship 'organizacionPagadoraPrestaciones'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'planesPagoPrestaciones'  ( refToType 'OrganizacionPagadoraPrestaciones' 'Organizaciones'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'PlanPagoPrestacionesConTarifaDeClinica'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'PlanPagoPrestaciones' 'Organizaciones'  ) 
			 )
			nil
			(relationships
			  ( relationship 'catalogoConceptosClinica'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate organizacionPagadoraPrestaciones archivo facturacionClinica catalogosConceptosClinica'
				''
				''
				( refToInverseRelationship 'planesPagoPrestaciones'  ( refToType 'CatalogoConceptosClinicaDeClinica' 'Tarifas'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'PlanPagoPrestacionesConTarifaDeAseguradora'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'PlanPagoPrestaciones' 'Organizaciones'  ) 
			 )
			nil
			(relationships
			  ( relationship 'planPagoPrestaciones'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'1' #'1'
				false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			false
		   )

		 )
		nil
	   )

	  ( module 'Codificacion_IDC_9_CM'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'EnfermedadAbstractaIDC9CM'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'codigoYNombreEnfermedad'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				ALWAYS
				''
				''
				''
				'calc codigoCompletoEnfermedad , ":" , nombreEnfermedad'
				''
				 ( refToType 'CodigoYNombreEnfermedadIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			  ( attribute 'codigoCompletoEnfermedad'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'noclone codigoEnfermedad'
				''
				 ( refToType 'CodigoEnfermedadIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			  ( attribute 'codigoEnfermedad'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'CodigoEnfermedadIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			  ( attribute 'nombreEnfermedad'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'NombreEnfermedadIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			  ( attribute 'notaEnfermedad'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'NotaIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			  ( attribute 'comentarioEnfermedad'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'ComentarioIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			  ( attribute 'inclusionEnfermedades'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'InclusionIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			  ( attribute 'exclusionEnfermedades'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'ExclusionIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'NombreEnfermedadIDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'CodigoEnfermedadIDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NotaIDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Text' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'InclusionIDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Text' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'ExclusionIDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Text' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'EnfermedadNivel1IDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'EnfermedadAbstractaIDC9CM' 'Codificacion_IDC_9_CM'  ) 
			 )
			(attributes
			  ( attributeRefinement 'codigoCompletoEnfermedad'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'noclone codigoEnfermedad'
				''
				 ( refToType 'CodigoEnfermedadIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'codigoCompletoEnfermedad'  ( refToType 'EnfermedadAbstractaIDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				 )
				nil
			   )

			 )
			(relationships
			  ( relationship 'enfermedadesNivel2'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				'nombreEnfermedad'
				''
				''
				''
				( refToInverseRelationship 'enfermedadNivel1'  ( refToType 'EnfermedadNivel2IDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				false
			   )

			  ( relationship 'categoriaEnfermedades'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'enfermedadesNivel1'  ( refToType 'CategoriaEnfermedadesIDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'EnfermedadNivel2IDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'EnfermedadAbstractaIDC9CM' 'Codificacion_IDC_9_CM'  ) 
			 )
			(attributes
			  ( attributeRefinement 'codigoCompletoEnfermedad'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'calc enfermedadNivel1 codigoEnfermedad , "." , codigoEnfermedad'
				''
				 ( refToType 'CodigoEnfermedadIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'codigoCompletoEnfermedad'  ( refToType 'EnfermedadAbstractaIDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				 )
				( initializationExpression:  )
			   )

			 )
			(relationships
			  ( relationship 'enfermedadNivel1'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'enfermedadesNivel2'  ( refToType 'EnfermedadNivel1IDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				false
			   )

			  ( relationship 'enfermedadesNivel3'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				'nombreEnfermedad'
				''
				''
				''
				( refToInverseRelationship 'enfermedadNivel2'  ( refToType 'EnfermedadNivel3IDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'EnfermedadNivel3IDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'EnfermedadAbstractaIDC9CM' 'Codificacion_IDC_9_CM'  ) 
			 )
			(attributes
			  ( attributeRefinement 'codigoCompletoEnfermedad'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'calc enfermedadNivel2 enfermedadNivel1 codigoCompletoEnfermedad , "." , codigoEnfermedad'
				''
				 ( refToType 'CodigoEnfermedadIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'codigoCompletoEnfermedad'  ( refToType 'EnfermedadAbstractaIDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				 )
				( initializationExpression:  )
			   )

			 )
			(relationships
			  ( relationship 'enfermedadNivel2'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'enfermedadesNivel3'  ( refToType 'EnfermedadNivel2IDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'ComentarioIDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'Text' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'CodificacionEnfermedadesIDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CodificacionAbstractaIDC9CM' 'Codificacion_IDC_9_CM'  ) 
			 )
			(attributes
			  ( attributeRefinement 'tipoCodificacion'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALVALUE
				''
				''
				''
				'literal Enfermedades'
				''
				 ( refToType 'TipoCodificacionIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'tipoCodificacion'  ( refToType 'CodificacionAbstractaIDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attributeRefinement 'nombreCodificacion'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal Enfermedades IDC9CM'
				''
				 ( refToType 'NombreCodificacionIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'nombreCodificacion'  ( refToType 'CodificacionAbstractaIDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				 )
				( initializationExpression:  )
			   )

			 )
			(relationships
			  ( relationship 'archivo'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'codificacionEnfermedades'  ( refToType 'Archivo' 'Nucleo'  )  ) 
				false
			   )

			  ( relationship 'categoriasEnfermedades'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				'nombreCategoria'
				''
				''
				''
				( refToInverseRelationship 'codificacionEnfermedadesIDC9CM'  ( refToType 'CategoriaEnfermedadesIDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'CodificacionProcedimientosIDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CodificacionAbstractaIDC9CM' 'Codificacion_IDC_9_CM'  ) 
			 )
			(attributes
			  ( attributeRefinement 'tipoCodificacion'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALVALUE
				''
				''
				''
				'literal Procedimientos'
				''
				 ( refToType 'TipoCodificacionIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'tipoCodificacion'  ( refToType 'CodificacionAbstractaIDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attributeRefinement 'nombreCodificacion'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal Procedimientos IDC9CM'
				''
				 ( refToType 'NombreCodificacionIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'nombreCodificacion'  ( refToType 'CodificacionAbstractaIDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				 )
				( initializationExpression:  )
			   )

			 )
			(relationships
			  ( relationship 'archivo'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'codificacionProcedimientos'  ( refToType 'Archivo' 'Nucleo'  )  ) 
				false
			   )

			  ( relationship 'categoriasProcedimientos'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'codificacionProcedimientosIDC9CM'  ( refToType 'CategoriaProcedimientosIDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'NombreCodificacionIDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'TipoCodificacionIDC9CM'
			nil nil
			nil
			nil
			false true false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'Enfermedades'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Procedimientos'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'CausasTraumatismo'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'EnfermedadesMentales'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'MorfologiasNeoplasmas'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			  ( attribute 'Substancias'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				nil
				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'CodificacionAbstractaIDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'tipoCodificacion'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'TipoCodificacionIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			  ( attribute 'nombreCodificacion'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'NombreCodificacionIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'CategoriaEnfermedadesIDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'nombreCategoria'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'NombreCategoriaEnfermedadIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			  ( attribute 'codigoCategoria'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'CodigoCategoriaEnfermedadesIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			  ( attribute 'notaCategoria'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'NotaIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			  ( attribute 'comentarioCategoria'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'ComentarioIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			  ( attribute 'inclusionCategoria'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'InclusionIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			  ( attribute 'exclusionCategoria'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'ExclusionIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'enfermedadesNivel1'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				'nombreEnfermedad'
				''
				''
				''
				( refToInverseRelationship 'categoriaEnfermedades'  ( refToType 'EnfermedadNivel1IDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				false
			   )

			  ( relationship 'codificacionEnfermedadesIDC9CM'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'categoriasEnfermedades'  ( refToType 'CodificacionEnfermedadesIDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'NombreCategoriaEnfermedadIDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'CodigoCategoriaEnfermedadesIDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'CodigoYNombreEnfermedadIDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'CodificacionSubstanciasIDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CodificacionAbstractaIDC9CM' 'Codificacion_IDC_9_CM'  ) 
			 )
			(attributes
			  ( attributeRefinement 'tipoCodificacion'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALVALUE
				''
				''
				''
				'literal Substancias'
				''
				 ( refToType 'TipoCodificacionIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'tipoCodificacion'  ( refToType 'CodificacionAbstractaIDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				 )
				( computationKind: isInitializationPropagationAllowed: isInitializationPropagationOnConnectAllowed: initializationExpression:  )
			   )

			  ( attributeRefinement 'nombreCodificacion'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal Substancias IDC9CM'
				''
				 ( refToType 'NombreCodificacionIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'nombreCodificacion'  ( refToType 'CodificacionAbstractaIDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				 )
				( isInitializationPropagationAllowed: isInitializationPropagationOnConnectAllowed: initializationExpression:  )
			   )

			 )
			(relationships
			  ( relationship 'archivo'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'codificacionSubstancias'  ( refToType 'Archivo' 'Nucleo'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'ProcedimientoAbstractoIDC9CM'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'codigoYNombreProcedimento'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				ALWAYS
				''
				''
				''
				'calc codigoCompletoProcedimiento , ":" , nombreProcedimiento'
				''
				 ( refToType 'CodigoYNombreProcedimientoIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			  ( attribute 'codigoCompletoProcedimiento'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'noclone codigoProcedimiento'
				''
				 ( refToType 'CodigoProcedimientoIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			  ( attribute 'codigoProcedimiento'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'CodigoProcedimientoIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			  ( attribute 'nombreProcedimiento'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'NombreProcedimientoIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			  ( attribute 'notaProcedimiento'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'NotaIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			  ( attribute 'comentarioProcedimiento'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'ComentarioIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			  ( attribute 'inclusionProcedimiento'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'InclusionIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			  ( attribute 'exclusionProcedimiento'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'ExclusionIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'ProcedimientoNivel1IDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'ProcedimientoAbstractoIDC9CM' 'Codificacion_IDC_9_CM'  ) 
			 )
			(attributes
			  ( attributeRefinement 'codigoCompletoProcedimiento'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'noclone codigoProcedimiento'
				''
				 ( refToType 'CodigoProcedimientoIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'codigoCompletoProcedimiento'  ( refToType 'ProcedimientoAbstractoIDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				 )
				nil
			   )

			 )
			(relationships
			  ( relationship 'categoriaProcedimientos'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'procedimientosNivel1'  ( refToType 'CategoriaProcedimientosIDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				false
			   )

			  ( relationship 'procedimientosNivel2'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'procedimientoNivel1'  ( refToType 'ProcedimientoNivel2IDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'ProcedimientoNivel2IDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'ProcedimientoAbstractoIDC9CM' 'Codificacion_IDC_9_CM'  ) 
			 )
			(attributes
			  ( attributeRefinement 'codigoCompletoProcedimiento'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'calc procedimientoNivel1 codigoProcedimiento , "." , codigoProcedimiento'
				''
				 ( refToType 'CodigoProcedimientoIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'codigoCompletoProcedimiento'  ( refToType 'ProcedimientoAbstractoIDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				 )
				( initializationExpression:  )
			   )

			 )
			(relationships
			  ( relationship 'procedimientoNivel1'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'procedimientosNivel2'  ( refToType 'ProcedimientoNivel1IDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				false
			   )

			  ( relationship 'procedimientosNivel3'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'procedimientoNivel2'  ( refToType 'ProcedimientoNivel3IDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'ProcedimientoNivel3IDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'ProcedimientoAbstractoIDC9CM' 'Codificacion_IDC_9_CM'  ) 
			 )
			(attributes
			  ( attributeRefinement 'codigoCompletoProcedimiento'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'calc procedimientoNivel2 procedimientoNivel1 codigoCompletoProcedimiento , "." , codigoProcedimiento'
				''
				 ( refToType 'CodigoProcedimientoIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'codigoCompletoProcedimiento'  ( refToType 'ProcedimientoAbstractoIDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				 )
				( initializationExpression:  )
			   )

			 )
			(relationships
			  ( relationship 'procedimientoNivel2'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'procedimientosNivel3'  ( refToType 'ProcedimientoNivel2IDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'CategoriaProcedimientosIDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'nombreCategoria'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'NombreCategoriaProcedimientoIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			  ( attribute 'codigoCategoria'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'CodigoCategoriaProcedimientosIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			  ( attribute 'notaCategoria'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'NotaIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			  ( attribute 'comentarioCategoria'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'ComentarioIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			  ( attribute 'inclusionCategoria'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'InclusionIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			  ( attribute 'exclusionCategoria'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'ExclusionIDC9CM' 'Codificacion_IDC_9_CM'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'procedimientosNivel1'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false false true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'categoriaProcedimientos'  ( refToType 'ProcedimientoNivel1IDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				false
			   )

			  ( relationship 'codificacionProcedimientosIDC9CM'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'categoriasProcedimientos'  ( refToType 'CodificacionProcedimientosIDC9CM' 'Codificacion_IDC_9_CM'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'NombreCategoriaProcedimientoIDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'NombreProcedimientoIDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'CodigoProcedimientoIDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'CodigoYNombreProcedimientoIDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		  ( type 'CodigoCategoriaProcedimientosIDC9CM'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false true false false
			nil
			(supertypes
			   ( refToType 'String' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		 )
		nil
	   )

	  ( module 'Calendario'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'Ejercicio'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'numeroEjercicio'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal 2002'
				''
				 ( refToType 'NumeroEjercicioCalendario' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'calendario'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'ejercicios'  ( refToType 'CalendarioAbstracto' 'Calendario'  )  ) 
				false
			   )

			  ( relationship 'periodos'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				'numeroPeriodo'
				''
				''
				''
				( refToInverseRelationship 'ejercicio'  ( refToType 'Periodo' 'Calendario'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Periodo'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'numeroPeriodo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal 1'
				''
				 ( refToType 'NumeroPeriodoCalendario' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'nombreMes'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'literal nombreMes'
				''
				 ( refToType 'NombreMesCalendario' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'ejercicio'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'periodos'  ( refToType 'Ejercicio' 'Calendario'  )  ) 
				false
			   )

			  ( relationship 'jornadas'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				'numeroJornada'
				''
				''
				''
				( refToInverseRelationship 'periodo'  ( refToType 'Jornada' 'Calendario'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Jornada'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'numeroJornada'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'NumeroJornadaCalendario' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'periodo'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'jornadas'  ( refToType 'Periodo' 'Calendario'  )  ) 
				false
			   )

			  ( relationship 'entradasCalendario'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'jornada'  ( refToType 'EntradaCalendario' 'Calendario'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'CalendarioAbstracto'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'nombreCalendario'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false true false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'NombreCalendario' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'tipoCalendario'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal Visitas'
				''
				 ( refToType 'TipoCalendario' 'Enumeraciones'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'ejercicios'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				'numeroEjercicio'
				''
				''
				''
				( refToInverseRelationship 'calendario'  ( refToType 'Ejercicio' 'Calendario'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'CalendarioVisitas'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CalendarioAbstracto' 'Calendario'  ) 
			 )
			(attributes
			  ( attributeRefinement 'tipoCalendario'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal Visitas'
				''
				 ( refToType 'TipoCalendario' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'tipoCalendario'  ( refToType 'CalendarioAbstracto' 'Calendario'  )  ) 
				 )
				( isInitializationPropagationAllowed: isInitializationPropagationOnConnectAllowed:  )
			   )

			 )
			(relationships
			  ( relationship 'archivo'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'calendarioVisitas'  ( refToType 'Archivo' 'Nucleo'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'CalendarioGuardias'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CalendarioAbstracto' 'Calendario'  ) 
			 )
			(attributes
			  ( attributeRefinement 'tipoCalendario'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'literal Guardias'
				''
				 ( refToType 'TipoCalendario' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'tipoCalendario'  ( refToType 'CalendarioAbstracto' 'Calendario'  )  ) 
				 )
				( initializationExpression:  )
			   )

			 )
			(relationships
			  ( relationship 'archivo'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'calendarioGuardias'  ( refToType 'Archivo' 'Nucleo'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'EntradaCalendario'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'nombreEntradaCalendario'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'NombreEntradaCalendario' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'tipoEntradaCalendario'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'TipoEntradaCalendario' 'Enumeraciones'  ) 

				false
			   )

			  ( attribute 'temporalidadEntradaCalendario'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'TemporalidadEntradaCalendario' 'Enumeraciones'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'jornada'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'entradasCalendario'  ( refToType 'Jornada' 'Calendario'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'EntradaCalendarioVisita'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'EntradaCalendario' 'Calendario'  ) 
			 )
			(attributes
			  ( attributeRefinement 'tipoEntradaCalendario'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALVALUE
				''
				''
				''
				'literal Visita'
				''
				 ( refToType 'TipoEntradaCalendario' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'tipoEntradaCalendario'  ( refToType 'EntradaCalendario' 'Calendario'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			 )
			(relationships
			  ( relationship 'visita'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'entradaCalendarioVisita'  ( refToType 'Visita' 'Nucleo'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'EntradaCalendarioGuardia'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'EntradaCalendario' 'Calendario'  ) 
			 )
			(attributes
			  ( attributeRefinement 'tipoEntradaCalendario'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALVALUE
				''
				''
				''
				'literal Guardia'
				''
				 ( refToType 'TipoEntradaCalendario' 'Enumeraciones'  ) 

				false
				(refinedAttributes
				   ( refToAttribute 'tipoEntradaCalendario'  ( refToType 'EntradaCalendario' 'Calendario'  )  ) 
				 )
				( computationKind: initializationExpression:  )
			   )

			  ( attribute 'horaComienzo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'HoraComienzo' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'horaFin'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'HoraFin' 'TiposBasicos'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'personasClinica'
				nil nil
				nil
				nil
				REFERENCES nil
				#'1' #*
				false false true false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				'nocreate jornada periodo ejercicio calendario archivo personalClinica'
				''
				''
				( refToInverseRelationship 'entradaCalendarioGuardia'  ( refToType 'PersonaClinica' 'Nucleo'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		 )
		nil
	   )

	  ( module 'CISR'
		nil nil
		nil
		nil
		nil nil
		nil
		(submodules
		  ( module 'CISR CORE'
			nil nil
			nil
			nil
			nil nil
			(types
			  ( type 'CISR CORE Root'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false true false
				nil
				(supertypes
				   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
				 )
				(attributes
				  ( attribute 'name'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false true false false false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal CIS'
					''
					 ( refToType 'String' 'PrimitiveTypes'  ) 

					false
				   )

				 )
				(relationships
				  ( relationship 'customers'
					nil nil
					nil
					nil
					AGGREGATES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'root'  ( refToType 'Customer' 'CISR' 'CISR CORE' 'Customers'  )  ) 
					false
				   )

				  ( relationship 'archivo'
					nil nil
					nil
					nil
					isAGGREGATED nil
					#'1' #'1'
					false false false false false false false
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'cis core root'  ( refToType 'Archivo' 'Nucleo'  )  ) 
					false
				   )

				  ( relationship 'unmatchedReturns'
					nil nil
					nil
					nil
					AGGREGATES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'cISR CORE Root'  ( refToType 'Unmatched Return' 'CISR' 'CISR CORE' 'Returns'  )  ) 
					false
				   )

				 )
				nil
				nil
				false
			   )

			 )
			(submodules
			  ( module 'Customers'
				nil nil
				nil
				nil
				nil nil
				(types
				  ( type 'Customer'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'UTR'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					 )
					(relationships
					  ( relationship 'root'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'customers'  ( refToType 'CISR CORE Root' 'CISR' 'CISR CORE'  )  ) 
						false
					   )

					  ( relationship 'contractor'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #'1'
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'customer'  ( refToType 'Contractor' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						false
					   )

					  ( relationship 'subcontractorRegistrations'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						ALWAYS
						''
						''
						''
						'calc clonenot subcontractorRegistrationOnHisOwnRight'
						( refToInverseRelationship 'customer'  ( refToType 'Abstract Subcontractor' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Non Partnership'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Customer' 'CISR' 'CISR CORE' 'Customers'  ) 
					 )
					nil
					(relationships
					  ( relationship 'subcontractorRegistrationOnHisOwnRight'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'customer'  ( refToType 'Individual Subcontractor Registration' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Abstract Partnership'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Customer' 'CISR' 'CISR CORE' 'Customers'  ) 
					 )
					nil
					(relationships
					  ( relationship 'partners'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'partnership'  ( refToType 'Partner' 'CISR' 'CISR CORE' 'Customers'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'CID Sole Trader Details'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'NINO'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'NINOStatus'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'NINO Status Kind' 'CISR' 'CISR CORE' 'Customers' 'Enumerations'  ) 

						false
					   )

					  ( attribute 'IsDeceased'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal False'
						''
						 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'DateOfBirth'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'Date' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'DateStartedSelfEmployment'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'Date' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'DistrictNumber'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						nil
						false
					   )

					  ( attribute 'ProcessingOffice'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						nil
						false
					   )

					  ( attribute 'Title'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'TitleKind' 'CISR' 'CISR CORE' 'Non Identifiable' 'Enumerations'  ) 

						false
					   )

					  ( attribute 'FirstName'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'SecondName'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'Surname'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'DaytimeTelephoneNumber'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					 )
					nil
					nil
					nil
					false
				   )

				  ( type 'Trust'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Non Partnership' 'CISR' 'CISR CORE' 'Customers'  ) 
					 )
					nil
					nil
					nil
					nil
					false
				   )

				  ( type 'Company'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Non Partnership' 'CISR' 'CISR CORE' 'Customers'  ) 
					   ( refToType 'Partnereable' 'CISR' 'CISR CORE' 'Customers'  ) 
					 )
					(attributes
					  ( attribute 'CRN'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					 )
					(relationships
					  ( relationshipRefinement 'subcontractorRegistrations'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						ALWAYS
						''
						''
						''
						'calc clonenot partnerInPartnerships subcontractorRegistration union: subcontractorRegistrationOnHisOwnRight'
						( refToInverseRelationship 'customer'  ( refToType 'Abstract Subcontractor' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						false
						(refinedRelationships
						  ( refToRefinedRelationship 'subcontractorRegistrations'  ( refToType 'Customer' 'CISR' 'CISR CORE' 'Customers'  )  ) 
						 )
						( initializationExpression:  )
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Unincorporated Body'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Non Partnership' 'CISR' 'CISR CORE' 'Customers'  ) 
					 )
					nil
					nil
					nil
					nil
					false
				   )

				  ( type 'Limited Partnership'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Abstract Partnership' 'CISR' 'CISR CORE' 'Customers'  ) 
					 )
					nil
					(relationships
					  ( relationshipRefinement 'subcontractorRegistrations'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						ALWAYS
						''
						''
						''
						'calc clonenot partners subcontractorRegistration union: subcontractorRegistrationOnHisOwnRight '
						( refToInverseRelationship 'customer'  ( refToType 'Abstract Subcontractor' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						false
						(refinedRelationships
						  ( refToRefinedRelationship 'subcontractorRegistrations'  ( refToType 'Customer' 'CISR' 'CISR CORE' 'Customers'  )  ) 
						 )
						( initializationExpression:  )
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Partnership'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Abstract Partnership' 'CISR' 'CISR CORE' 'Customers'  ) 
					 )
					nil
					(relationships
					  ( relationshipRefinement 'subcontractorRegistrations'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						ALWAYS
						''
						''
						''
						'calc clonenot partners subcontractorRegistration union: subcontractorRegistrationOnHisOwnRight '
						( refToInverseRelationship 'customer'  ( refToType 'Abstract Subcontractor' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						false
						(refinedRelationships
						  ( refToRefinedRelationship 'subcontractorRegistrations'  ( refToType 'Customer' 'CISR' 'CISR CORE' 'Customers'  )  ) 
						 )
						( initializationExpression:  )
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Scottish Partnership'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Abstract Partnership' 'CISR' 'CISR CORE' 'Customers'  ) 
					   ( refToType 'Partnereable' 'CISR' 'CISR CORE' 'Customers'  ) 
					 )
					nil
					(relationships
					  ( relationshipRefinement 'subcontractorRegistrations'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						ALWAYS
						''
						''
						''
						'calc clonenot partnerInPartnerships subcontractorRegistration union: subcontractorRegistrationOnHisOwnRight union: partners subcontractorRegistration'
						( refToInverseRelationship 'customer'  ( refToType 'Abstract Subcontractor' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						false
						(refinedRelationships
						  ( refToRefinedRelationship 'subcontractorRegistrations'  ( refToType 'Customer' 'CISR' 'CISR CORE' 'Customers'  )  ) 
						 )
						( initializationExpression:  )
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Limited Liability Partnership'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Abstract Partnership' 'CISR' 'CISR CORE' 'Customers'  ) 
					   ( refToType 'Partnereable' 'CISR' 'CISR CORE' 'Customers'  ) 
					 )
					nil
					(relationships
					  ( relationshipRefinement 'subcontractorRegistrations'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						ALWAYS
						''
						''
						''
						'calc clonenot partnerInPartnerships subcontractor union: subcontractorRegistrationOnHisOwnRight union: partners subcontractorRegistration'
						( refToInverseRelationship 'customer'  ( refToType 'Abstract Subcontractor' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						false
						(refinedRelationships
						  ( refToRefinedRelationship 'subcontractorRegistrations'  ( refToType 'Customer' 'CISR' 'CISR CORE' 'Customers'  )  ) 
						 )
						( initializationExpression:  )
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Partnereable'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					nil
					nil
					(relationships
					  ( relationship 'partnerInPartnerships'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'customer'  ( refToType 'Partner' 'CISR' 'CISR CORE' 'Customers'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Partner'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'UTRsPair'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						ALWAYS
						''
						''
						''
						'calc clonenot partnership UTR , "_&_" , customer UTR'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'Is Responsible Partner'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal True'
						''
						 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

						false
					   )

					 )
					(relationships
					  ( relationship 'partnership'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'partners'  ( refToType 'Abstract Partnership' 'CISR' 'CISR CORE' 'Customers'  )  ) 
						false
					   )

					  ( relationship 'customer'
						nil nil
						nil
						nil
						REFERENCES nil
						#'1' #'1'
						false false true false false true true
						INITIALIZEDINCONSTRUCTOR
						''
						'nocreate partnership root customers'
						''
						''
						( refToInverseRelationship 'partnerInPartnerships'  ( refToType 'Partnereable' 'CISR' 'CISR CORE' 'Customers'  )  ) 
						false
					   )

					  ( relationship 'subcontractorRegistration'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'1' #'1'
						false false false false false true true
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						( refToInverseRelationship 'partner'  ( refToType 'Partnership Partner Subcontractor Registration' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'CIS Sole Trader Details'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'CISTradingName'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'VATRegistrationNumber'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'EveningTelephoneNumber'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'MobilePhoneNumber'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'FaxNumber'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'EmailAddress'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'HasBeenPaidByContractor'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal True'
						''
						 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'DateStartedOrPlanToStartConstruction'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'Date' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'UseExistingAgent'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal False'
						''
						 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'SendContractorOutputToAgent'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal False'
						''
						 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'TypeOfWork'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'Type Of Work Kind' 'CISR' 'CISR CORE' 'Customers' 'Enumerations'  ) 

						false
					   )

					  ( attribute 'IDDocumentsProvingID'
						nil nil
						nil
						nil
						'' nil
						#'0' #*
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'ID Document Kind' 'CISR' 'CISR CORE' 'Customers' 'Enumerations'  ) 

						false
					   )

					 )
					nil
					nil
					nil
					false
				   )

				  ( type 'Sole Trader'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Non Partnership' 'CISR' 'CISR CORE' 'Customers'  ) 
					   ( refToType 'Partnereable' 'CISR' 'CISR CORE' 'Customers'  ) 
					   ( refToType 'CIS Sole Trader Details' 'CISR' 'CISR CORE' 'Customers'  ) 
					   ( refToType 'CID Sole Trader Details' 'CISR' 'CISR CORE' 'Customers'  ) 
					   ( refToType 'Address Sole Trader Details' 'CISR' 'CISR CORE' 'Customers'  ) 
					 )
					(attributes
					  ( attribute 'Name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'clonenot CISTradingName'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					 )
					(relationships
					  ( relationship 'agent'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #'1'
						false false true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						( refToInverseRelationship 'soleTraders'  ( refToType 'Sole Trader Agent' 'CISR' 'CISR CORE' 'Agents'  )  ) 
						false
					   )

					  ( relationshipRefinement 'subcontractorRegistrations'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						ALWAYS
						''
						''
						''
						'calc clonenot partnerInPartnerships subcontractorRegistration union: subcontractorRegistrationOnHisOwnRight'
						( refToInverseRelationship 'customer'  ( refToType 'Abstract Subcontractor' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						false
						(refinedRelationships
						  ( refToRefinedRelationship 'subcontractorRegistrations'  ( refToType 'Customer' 'CISR' 'CISR CORE' 'Customers'  )  ) 
						 )
						( initializationExpression:  )
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Address Sole Trader Details'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'Home Address'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'Address' 'CISR' 'CISR CORE' 'Non Identifiable'  ) 

						false
					   )

					  ( attribute 'CISCorrAddress'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'Address' 'CISR' 'CISR CORE' 'Non Identifiable'  ) 

						false
					   )

					 )
					nil
					nil
					nil
					false
				   )

				 )
				(submodules
				  ( module 'Enumerations'
					nil nil
					nil
					nil
					nil nil
					(types
					  ( type 'NINO Status Kind'
						nil nil
						nil
						nil
						false true false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
						 )
						(attributes
						  ( attribute 'Verified'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false false false
							NOCOMPUTATION
							''
							''
							''
							''
							''
							nil
							false
						   )

						  ( attribute 'Not Verified'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false false false
							NOCOMPUTATION
							''
							''
							''
							''
							''
							nil
							false
						   )

						 )
						nil
						nil
						nil
						false
					   )

					  ( type 'Type Of Work Kind'
						nil nil
						nil
						nil
						false true false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
						 )
						(attributes
						  ( attribute 'Foundations'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false false false
							NOCOMPUTATION
							''
							''
							''
							''
							''
							nil
							false
						   )

						  ( attribute 'Landfill'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false false false
							NOCOMPUTATION
							''
							''
							''
							''
							''
							nil
							false
						   )

						  ( attribute 'Metallic Structure'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false false false
							NOCOMPUTATION
							''
							''
							''
							''
							''
							nil
							false
						   )

						  ( attribute 'Coatings and Paintings'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false false false
							NOCOMPUTATION
							''
							''
							''
							''
							''
							nil
							false
						   )

						  ( attribute 'Bricklayers'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false false false
							NOCOMPUTATION
							''
							''
							''
							''
							''
							nil
							false
						   )

						  ( attribute 'Quality Management'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false false false
							NOCOMPUTATION
							''
							''
							''
							''
							''
							nil
							false
						   )

						  ( attribute 'Construction Management'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false false false
							NOCOMPUTATION
							''
							''
							''
							''
							''
							nil
							false
						   )

						  ( attribute 'Electrical and Electronics'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false false false
							NOCOMPUTATION
							''
							''
							''
							''
							''
							nil
							false
						   )

						  ( attribute 'Elevators'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false false false
							NOCOMPUTATION
							''
							''
							''
							''
							''
							nil
							false
						   )

						 )
						nil
						nil
						nil
						false
					   )

					  ( type 'ID Document Kind'
						nil nil
						nil
						nil
						false true false
						nil nil
						nil
						false false false false
						nil
						nil
						(attributes
						  ( attribute 'Passport'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false false false
							NOCOMPUTATION
							''
							''
							''
							''
							''
							nil
							false
						   )

						  ( attribute 'Driver License'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false false false
							NOCOMPUTATION
							''
							''
							''
							''
							''
							nil
							false
						   )

						  ( attribute 'Birth Certificate'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false false false
							NOCOMPUTATION
							''
							''
							''
							''
							''
							nil
							false
						   )

						  ( attribute 'Marriage Certificate'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false false false
							NOCOMPUTATION
							''
							''
							''
							''
							''
							nil
							false
						   )

						  ( attribute 'Home Office Certificate'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false false false
							NOCOMPUTATION
							''
							''
							''
							''
							''
							nil
							false
						   )

						  ( attribute 'Foreign Office Certificate'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false false false
							NOCOMPUTATION
							''
							''
							''
							''
							''
							nil
							false
						   )

						  ( attribute 'Biometric Level1'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false false false
							NOCOMPUTATION
							''
							''
							''
							''
							''
							nil
							false
						   )

						  ( attribute 'Biometric Level2'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false false false
							NOCOMPUTATION
							''
							''
							''
							''
							''
							nil
							false
						   )

						  ( attribute 'DNA Match Level 1'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false false false
							NOCOMPUTATION
							''
							''
							''
							''
							''
							nil
							false
						   )

						  ( attribute 'DNA Match Level 2'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false false false
							NOCOMPUTATION
							''
							''
							''
							''
							''
							nil
							false
						   )

						 )
						nil
						nil
						nil
						false
					   )

					 )
					nil
				   )

				 )
			   )

			  ( module 'Registrations'
				nil nil
				nil
				nil
				nil nil
				(types
				  ( type 'Contractor'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'UTR'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						ALWAYS
						''
						''
						''
						'clonenot customer UTR'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					 )
					(relationships
					  ( relationship 'customer'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'contractor'  ( refToType 'Customer' 'CISR' 'CISR CORE' 'Customers'  )  ) 
						false
					   )

					  ( relationship 'contractorSchemes'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'contractor'  ( refToType 'Contractor Scheme' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Contractor Scheme'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'aoRef'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					 )
					(relationships
					  ( relationship 'contractor'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'contractorSchemes'  ( refToType 'Contractor' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						false
					   )

					  ( relationship 'verifications'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'contractorScheme'  ( refToType 'Verification' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						false
					   )

					  ( relationship 'liabilityPeriods'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'contractorScheme'  ( refToType 'Liability Period' 'CISR' 'CISR CORE' 'Returns'  )  ) 
						false
					   )

					  ( relationship 'unmatchedReturns'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'contractorScheme'  ( refToType 'Unmatched Return' 'CISR' 'CISR CORE' 'Returns'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Abstract Subcontractor'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'Name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'clonenot customer name'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'DisplayUTRs'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						ALWAYS
						''
						''
						''
						'clonenot customer UTR'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					 )
					(relationships
					  ( relationship 'individualVerifications'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'subcontractor'  ( refToType 'Individual Verification' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						false
					   )

					  ( relationship 'subcontractorPayment'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'subcontractor'  ( refToType 'Subcontractor Payment' 'CISR' 'CISR CORE' 'Returns'  )  ) 
						false
					   )

					  ( relationship 'customer'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'subcontractorRegistrations'  ( refToType 'Customer' 'CISR' 'CISR CORE' 'Customers'  )  ) 
						false
					   )

					  ( relationship 'taxTreatments'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						true false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'abstract Subcontractor'  ( refToType 'Tax Treatment' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						false
					   )

					  ( relationshipRefinement 'customer'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'subcontractorRegistrations'  ( refToType 'Limited Liability Partnership' 'CISR' 'CISR CORE' 'Customers'  )  ) 
						false
						(refinedRelationships
						  ( refToRefinedRelationship 'customer'  ( refToType 'Abstract Subcontractor' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						 )
						nil
					   )

					  ( relationshipRefinement 'customer'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'subcontractorRegistrations'  ( refToType 'Scottish Partnership' 'CISR' 'CISR CORE' 'Customers'  )  ) 
						false
						(refinedRelationships
						  ( refToRefinedRelationship 'customer'  ( refToType 'Abstract Subcontractor' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						 )
						nil
					   )

					  ( relationshipRefinement 'customer'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'subcontractorRegistrations'  ( refToType 'Company' 'CISR' 'CISR CORE' 'Customers'  )  ) 
						false
						(refinedRelationships
						  ( refToRefinedRelationship 'customer'  ( refToType 'Abstract Subcontractor' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						 )
						nil
					   )

					  ( relationshipRefinement 'customer'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'subcontractorRegistrations'  ( refToType 'Sole Trader' 'CISR' 'CISR CORE' 'Customers'  )  ) 
						false
						(refinedRelationships
						  ( refToRefinedRelationship 'customer'  ( refToType 'Abstract Subcontractor' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						 )
						nil
					   )

					  ( relationshipRefinement 'customer'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'subcontractorRegistrations'  ( refToType 'Partnership' 'CISR' 'CISR CORE' 'Customers'  )  ) 
						false
						(refinedRelationships
						  ( refToRefinedRelationship 'customer'  ( refToType 'Abstract Subcontractor' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						 )
						nil
					   )

					  ( relationshipRefinement 'customer'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'subcontractorRegistrations'  ( refToType 'Limited Partnership' 'CISR' 'CISR CORE' 'Customers'  )  ) 
						false
						(refinedRelationships
						  ( refToRefinedRelationship 'customer'  ( refToType 'Abstract Subcontractor' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						 )
						nil
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Individual Subcontractor Registration'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Abstract Subcontractor' 'CISR' 'CISR CORE' 'Registrations'  ) 
					 )
					(attributes
					  ( attribute 'UTR'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						ALWAYS
						''
						''
						''
						'clonenot customer UTR'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attributeRefinement 'DisplayUTRs'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'clonenot UTR'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
						(refinedAttributes
						   ( refToAttribute 'DisplayUTRs'  ( refToType 'Abstract Subcontractor' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						 )
						( isChangeable: initializationExpression:  )
					   )

					 )
					(relationships
					  ( relationship 'customer'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'subcontractorRegistrationOnHisOwnRight'  ( refToType 'Non Partnership' 'CISR' 'CISR CORE' 'Customers'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Partnership Partner Subcontractor Registration'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Abstract Subcontractor' 'CISR' 'CISR CORE' 'Registrations'  ) 
					 )
					(attributes
					  ( attribute 'UTRsPair'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						ALWAYS
						''
						''
						''
						'clonenot partner UTRsPair'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attributeRefinement 'Name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'calc clonenot partner name'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
						(refinedAttributes
						   ( refToAttribute 'Name'  ( refToType 'Abstract Subcontractor' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						 )
						( isNameAttribute: initializationExpression:  )
					   )

					  ( attributeRefinement 'DisplayUTRs'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'clonenot UTRsPair'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
						(refinedAttributes
						   ( refToAttribute 'DisplayUTRs'  ( refToType 'Abstract Subcontractor' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						 )
						( isChangeable: initializationExpression:  )
					   )

					 )
					(relationships
					  ( relationship 'partner'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'subcontractorRegistration'  ( refToType 'Partner' 'CISR' 'CISR CORE' 'Customers'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Verification'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false true true
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'VerificationNumber'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'calc clonenot ( contractorScheme  contractor customer root customers contractor contractorSchemes verifications VerificationNumber :max ) + 1'
						''
						 ( refToType 'Integer' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'VerificationDate'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'Date' 'PrimitiveTypes'  ) 

						false
					   )

					 )
					(relationships
					  ( relationship 'contractorScheme'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'verifications'  ( refToType 'Contractor Scheme' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						false
					   )

					  ( relationship 'individualVerifications'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						true false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'verification'  ( refToType 'Individual Verification' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						true
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Individual Verification'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'VerificationSuffix'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'calc clonenot ( verification individualVerifications VerificationSuffix :max ) + 1'
						''
						 ( refToType 'Integer' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'VerificationNumberAndSuffix'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						ALWAYS
						''
						''
						''
						'calc clonenot verification VerificationNumber , "-" , VerificationSuffix'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					 )
					(relationships
					  ( relationship 'verification'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'individualVerifications'  ( refToType 'Verification' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						false
					   )

					  ( relationship 'subcontractor'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #'1'
						false false true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						'nocreate verification contractorScheme contractor customer root customers subcontractorRegistrations'
						''
						''
						( refToInverseRelationship 'individualVerifications'  ( refToType 'Abstract Subcontractor' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Tax Treatment'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					nil
					(attributes
					  ( attribute 'Tax Treatment'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'Tax Treatment Kind' 'CISR' 'CISR CORE' 'Registrations'  ) 

						false
					   )

					  ( attribute 'Date'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'Date' 'PrimitiveTypes'  ) 

						false
					   )

					 )
					(relationships
					  ( relationship 'abstract Subcontractor'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'taxTreatments'  ( refToType 'Abstract Subcontractor' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Tax Treatment Kind'
					nil nil
					nil
					nil
					false true false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'Net'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
						false
					   )

					  ( attribute 'Gross'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
						false
					   )

					  ( attribute 'Maximum'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
						false
					   )

					 )
					nil
					nil
					nil
					false
				   )

				 )
				nil
			   )

			  ( module 'Returns'
				nil nil
				nil
				nil
				nil nil
				(types
				  ( type 'Abstract Return'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'LoggedDate'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'Date' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'AmendedDate'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						NOCOMPUTATION
						''
						''
						''
						''
						''
						 ( refToType 'Date' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'TraceFromAction'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						NOCOMPUTATION
						''
						''
						''
						''
						''
						 ( refToType 'Trace Action Kind' 'CISR' 'CISR CORE' 'Returns'  ) 

						false
					   )

					  ( attribute 'TraceToAction'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						NOCOMPUTATION
						''
						''
						''
						''
						''
						 ( refToType 'Trace Action Kind' 'CISR' 'CISR CORE' 'Returns'  ) 

						false
					   )

					 )
					(relationships
					  ( relationship 'traceTo'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #'1'
						false false true false false false false
						NOCOMPUTATION
						''
						'nocreate calc liabilityPeriod contractorScheme liabilityPeriods processedReturns , liabilityPeriod contractorScheme liabilityPeriods multipleReturns , liabilityPeriod contractorScheme contractor contractorSchemes liabilityPeriods processedReturns , liabilityPeriod contractorScheme  contractor contractorSchemes liabilityPeriods multipleReturns , liabilityPeriod contractorScheme unmatchedReturns , liabilityPeriod contractorScheme contractor customer root unmatchedReturns'
						''
						''
						( refToInverseRelationship 'traceFrom'  ( refToType 'Abstract Return' 'CISR' 'CISR CORE' 'Returns'  )  ) 
						false
					   )

					  ( relationship 'traceFrom'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'traceTo'  ( refToType 'Abstract Return' 'CISR' 'CISR CORE' 'Returns'  )  ) 
						false
					   )

					  ( relationship 'subcontractorPayments'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'return'  ( refToType 'Subcontractor Payment' 'CISR' 'CISR CORE' 'Returns'  )  ) 
						false
					   )

					  ( relationship 'liabilityPeriod'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'allReturns'  ( refToType 'Liability Period' 'CISR' 'CISR CORE' 'Returns'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Unmatched Return'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Abstract Return' 'CISR' 'CISR CORE' 'Returns'  ) 
					 )
					(attributes
					  ( attribute 'IsResolved'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALVALUE
						''
						''
						''
						'literal false'
						''
						 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'Name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						ALWAYS
						''
						''
						''
						'calc clonenot "UTR:" , UTR , " AOref:" , aoRef, " P" , period , "Unmatched"'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'UTR'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'AORef'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'Year'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'Month'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'period'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						ALWAYS
						''
						''
						''
						'calc clonenot year , "-" , month'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					 )
					(relationships
					  ( relationship 'contractorScheme'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'unmatchedReturns'  ( refToType 'Contractor Scheme' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						false
					   )

					  ( relationship 'cISR CORE Root'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'unmatchedReturns'  ( refToType 'CISR CORE Root' 'CISR' 'CISR CORE'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Multiple Return'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Abstract Return' 'CISR' 'CISR CORE' 'Returns'  ) 
					 )
					(attributes
					  ( attribute 'IsResolved'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALVALUE
						''
						''
						''
						'literal false'
						''
						 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'DisplayName'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						ALWAYS
						''
						''
						''
						'calc clonenot liabilityPeriod contractorScheme aoRef , ":P" , liabilityPeriod Period , "-Multiple"'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					 )
					(relationships
					  ( relationship 'liabilityPeriod'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'multipleReturns'  ( refToType 'Liability Period' 'CISR' 'CISR CORE' 'Returns'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Processed Return'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Abstract Return' 'CISR' 'CISR CORE' 'Returns'  ) 
					 )
					(attributes
					  ( attribute 'IsEffective'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALVALUE
						''
						''
						''
						'literal True'
						''
						 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'Version'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'calc clonenot  ( liabilityPeriod processedReturns Version :max ) + 1'
						''
						 ( refToType 'Integer' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'DisplayName'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						ALWAYS
						''
						''
						''
						'calc clonenot liabilityPeriod contractorScheme aoRef , ":P" , liabilityPeriod Period , "-V" , Version'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					 )
					(relationships
					  ( relationship 'liabilityPeriod'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'processedReturns'  ( refToType 'Liability Period' 'CISR' 'CISR CORE' 'Returns'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Liability Period'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					nil
					(attributes
					  ( attribute 'Year'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'Month'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'Period'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						ALWAYS
						''
						''
						''
						'calc clonenot Year , "/" , Month'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					 )
					(relationships
					  ( relationship 'contractorScheme'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'liabilityPeriods'  ( refToType 'Contractor Scheme' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						false
					   )

					  ( relationship 'processedReturns'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'liabilityPeriod'  ( refToType 'Processed Return' 'CISR' 'CISR CORE' 'Returns'  )  ) 
						false
					   )

					  ( relationship 'multipleReturns'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'liabilityPeriod'  ( refToType 'Multiple Return' 'CISR' 'CISR CORE' 'Returns'  )  ) 
						false
					   )

					  ( relationship 'allReturns'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false false false false false false
						ALWAYS
						''
						''
						''
						'calc clonenot processedReturns , multipleReturns'
						( refToInverseRelationship 'liabilityPeriod'  ( refToType 'Abstract Return' 'CISR' 'CISR CORE' 'Returns'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Subcontractor Payment'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					nil
					(attributes
					  ( attribute 'DisplayUTRs'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'clonenot subcontractor DisplayUTRs'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					 )
					(relationships
					  ( relationship 'return'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'subcontractorPayments'  ( refToType 'Abstract Return' 'CISR' 'CISR CORE' 'Returns'  )  ) 
						false
					   )

					  ( relationship 'subcontractor'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #'1'
						false true true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						'calc nocreate return liabilityPeriod contractorScheme verifications individualVerifications subcontractor union: return liabilityPeriod contractorScheme contractor customer root customers subcontractorRegistrations'
						''
						''
						( refToInverseRelationship 'subcontractorPayment'  ( refToType 'Abstract Subcontractor' 'CISR' 'CISR CORE' 'Registrations'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Trace Action Kind'
					nil nil
					nil
					nil
					false true false
					nil nil
					nil
					false false false false
					nil
					nil
					(attributes
					  ( attribute 'Amend Items Or Payments'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
						false
					   )

					  ( attribute 'Amend Period'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
						false
					   )

					  ( attribute 'Amend AORef'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
						false
					   )

					  ( attribute 'Replace by Multiple'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
						false
					   )

					  ( attribute 'Replace by Unmatched'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
						false
					   )

					  ( attribute 'Merge with Multiple'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
						false
					   )

					  ( attribute 'Merge with Unmatched'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
						false
					   )

					  ( attribute 'Unmatch'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
						false
					   )

					 )
					nil
					nil
					nil
					false
				   )

				 )
				nil
			   )

			  ( module 'Agents'
				nil nil
				nil
				nil
				nil nil
				(types
				  ( type 'Abstract Agent'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					nil
					nil
					nil
					nil
					false
				   )

				  ( type 'Sole Trader Agent'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					nil
					(attributes
					  ( attribute 'Name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'Address'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
						false
					   )

					 )
					(relationships
					  ( relationship 'soleTraders'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'agent'  ( refToType 'Sole Trader' 'CISR' 'CISR CORE' 'Customers'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				 )
				nil
			   )

			  ( module 'Non Identifiable'
				nil nil
				nil
				nil
				nil nil
				(types
				  ( type 'Address'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'Line1'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'Line2'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'Line3'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'Line4'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'PostCode'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal TF1 1PP'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'Town'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal Telford'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'County'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal Shropshire'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'Country'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal United Kingdom'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'DisplayName'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						ALWAYS
						''
						''
						''
						'calc clonenot Line1 , "-" , Town , "-" , PostCode'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					 )
					nil
					nil
					nil
					false
				   )

				 )
				(submodules
				  ( module 'Enumerations'
					nil nil
					nil
					nil
					nil nil
					(types
					  ( type 'TitleKind'
						nil nil
						nil
						nil
						false true false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
						 )
						(attributes
						  ( attribute 'Mr'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false false false
							NOCOMPUTATION
							''
							''
							''
							''
							''
							nil
							false
						   )

						  ( attribute 'Miss'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false false false
							NOCOMPUTATION
							''
							''
							''
							''
							''
							nil
							false
						   )

						  ( attribute 'Mrs'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false false false
							NOCOMPUTATION
							''
							''
							''
							''
							''
							nil
							false
						   )

						  ( attribute 'Sir'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false false false
							NOCOMPUTATION
							''
							''
							''
							''
							''
							nil
							false
						   )

						 )
						nil
						nil
						nil
						false
					   )

					 )
					nil
				   )

				 )
			   )

			  ( module 'Contacts'
				nil nil
				nil
				nil
				nil nil
				(types
				  ( type 'Contact'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					nil
					nil
					nil
					nil
					false
				   )

				  ( type 'Incoming Contact'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					nil
					nil
					nil
					nil
					false
				   )

				  ( type 'Outgoing Contact'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					nil
					nil
					nil
					nil
					false
				   )

				 )
				nil
			   )

			 )
		   )

		 )
	   )

	  ( module 'BolsaCarga'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'Transportista'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			   ( refToType 'ConDirecciones' 'ContenedoresDeTipos'  ) 
			   ( refToType 'ConNumerosTelecomunicacion' 'ContenedoresDeTipos'  ) 
			 )
			nil
			(relationships
			  ( relationship 'viajes'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'transportista'  ( refToType 'Viaje' 'BolsaCarga'  )  ) 
				false
			   )

			  ( relationship 'bolsa'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'transportistas'  ( refToType 'Bolsa' 'BolsaCarga'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'TransportistaFisico'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'Transportista' 'BolsaCarga'  ) 
			 )
			(attributes
			  ( attribute 'dninif'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral nif '
				''
				 ( refToType 'DNINIF' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'nombre'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral nombre'
				''
				 ( refToType 'NombrePersona' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'apellidos'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral apellidos apellidos'
				''
				 ( refToType 'ApellidosPersona' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'fechaNacimiento'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'FechaNacimiento' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'sexo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'Sexo' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'apellidosYNombre'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'calc clonenot apellidos , "," , nombre'
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

				false
			   )

			  ( attribute 'nifApellidosYNombre'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'calc clonenot dninif , "_" , apellidos , "," , nombre'
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'TransportistaJuridico'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'Transportista' 'BolsaCarga'  ) 
			 )
			(attributes
			  ( attribute 'cif'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral cif '
				''
				 ( refToType 'CIF' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'razonSocial'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral razon social '
				''
				 ( refToType 'RazonSocial' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'nombreComercial'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral nombre comercial '
				''
				 ( refToType 'NombreComercial' 'TiposBasicos'  ) 

				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'ExpendedorMercancias'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			   ( refToType 'ConDirecciones' 'ContenedoresDeTipos'  ) 
			   ( refToType 'ConNumerosTelecomunicacion' 'ContenedoresDeTipos'  ) 
			 )
			nil
			(relationships
			  ( relationship 'cargas'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'expendedorMercancias'  ( refToType 'Carga' 'BolsaCarga'  )  ) 
				false
			   )

			  ( relationship 'bolsa'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'expendedoresMercancias'  ( refToType 'Bolsa' 'BolsaCarga'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'ExpendedorMercanciasFisico'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'ExpendedorMercancias' 'BolsaCarga'  ) 
			 )
			(attributes
			  ( attribute 'dninif'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral nif '
				''
				 ( refToType 'DNINIF' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'nombre'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral nombre'
				''
				 ( refToType 'NombrePersona' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'apellidos'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral apellidos apellidos'
				''
				 ( refToType 'ApellidosPersona' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'fechaNacimiento'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'FechaNacimiento' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'sexo'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'Sexo' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'apellidosYNombre'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'calc clonenot apellidos , "," , nombre'
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

				false
			   )

			  ( attribute 'nifApellidosYNombre'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false false false false false
				ALWAYS
				''
				''
				''
				'calc clonenot dninif , "_" , apellidos , "," , nombre'
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'ExpendedorMercanciasJuridico'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'ExpendedorMercancias' 'BolsaCarga'  ) 
			 )
			(attributes
			  ( attribute 'cif'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral cif '
				''
				 ( refToType 'CIF' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'razonSocial'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral razon social '
				''
				 ( refToType 'RazonSocial' 'TiposBasicos'  ) 

				false
			   )

			  ( attribute 'nombreComercial'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				'ranliteral nombre comercial '
				''
				 ( refToType 'NombreComercial' 'TiposBasicos'  ) 

				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		  ( type 'Carga'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			nil
			(relationships
			  ( relationship 'expendedorMercancias'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'cargas'  ( refToType 'ExpendedorMercancias' 'BolsaCarga'  )  ) 
				false
			   )

			  ( relationship 'entrega'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'1' #'1'
				false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				( refToInverseRelationship 'carga'  ( refToType 'Entrega' 'BolsaCarga'  )  ) 
				false
			   )

			  ( relationship 'recojida'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'1' #'1'
				false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				( refToInverseRelationship 'carga'  ( refToType 'Recojida' 'BolsaCarga'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Viaje'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			nil
			(relationships
			  ( relationship 'transportista'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'viajes'  ( refToType 'Transportista' 'BolsaCarga'  )  ) 
				false
			   )

			  ( relationship 'salida'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'1' #'1'
				false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				( refToInverseRelationship 'viaje'  ( refToType 'Salida' 'BolsaCarga'  )  ) 
				false
			   )

			  ( relationship 'llegada'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'1' #'1'
				false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				( refToInverseRelationship 'viaje'  ( refToType 'Llegada' 'BolsaCarga'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Bolsa'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'nombre'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

				false
			   )

			  ( attribute 'descripcion'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false false false
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'Text' 'PrimitiveTypes'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'transportistas'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'bolsa'  ( refToType 'Transportista' 'BolsaCarga'  )  ) 
				false
			   )

			  ( relationship 'expendedoresMercancias'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'bolsa'  ( refToType 'ExpendedorMercancias' 'BolsaCarga'  )  ) 
				false
			   )

			  ( relationship 'mercado'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'bolsas'  ( refToType 'Mercado' 'BolsaCarga'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Recojida'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			   ( refToType 'Suceso' 'BolsaCarga'  ) 
			 )
			nil
			(relationships
			  ( relationship 'carga'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'recojida'  ( refToType 'Carga' 'BolsaCarga'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Entrega'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			   ( refToType 'Suceso' 'BolsaCarga'  ) 
			 )
			nil
			(relationships
			  ( relationship 'carga'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'entrega'  ( refToType 'Carga' 'BolsaCarga'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Lugar'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'nombre'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

				false
			   )

			  ( attribute 'lugarYDireccion'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				ALWAYS
				''
				''
				''
				'calc clonenot direccion distritoPostalPoblacionProvincia'
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

				false
			   )

			  ( attribute 'personaContacto'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'direccion'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'1' #'1'
				false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				( refToInverseRelationship 'lugar'  ( refToType 'Direccion' 'TiposCompuestos'  )  ) 
				false
			   )

			  ( relationship 'conLugar'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'lugar'  ( refToType 'ConLugar' 'BolsaCarga'  )  ) 
				false
			   )

			  ( relationship 'telefonos'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'lugar'  ( refToType 'Telefono' 'TiposBasicos'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Momento'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'fecha'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false false false true false false false
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'Date' 'PrimitiveTypes'  ) 

				false
			   )

			  ( attribute 'hora'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false false false
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'Time' 'PrimitiveTypes'  ) 

				false
			   )

			  ( attribute 'fechaYHora'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false true false false false false false
				ALWAYS
				''
				''
				''
				''
				'calc clonenot fecha , hora'
				 ( refToType 'String' 'PrimitiveTypes'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'horario'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'comienzo'  ( refToType 'Horario' 'BolsaCarga'  )  ) 
				false
			   )

			  ( relationship 'horario'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'final'  ( refToType 'Horario' 'BolsaCarga'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Horario'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			nil
			(relationships
			  ( relationship 'comienzo'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'1' #'1'
				false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				( refToInverseRelationship 'horario'  ( refToType 'Momento' 'BolsaCarga'  )  ) 
				false
			   )

			  ( relationship 'final'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'1' #'1'
				false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				( refToInverseRelationship 'horario'  ( refToType 'Momento' 'BolsaCarga'  )  ) 
				false
			   )

			  ( relationship 'conHorario'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'horario'  ( refToType 'ConHorario' 'BolsaCarga'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Mercado'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false true false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			(attributes
			  ( attribute 'nombre'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

				false
			   )

			 )
			(relationships
			  ( relationship 'bolsas'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'mercado'  ( refToType 'Bolsa' 'BolsaCarga'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'ConLugar'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			nil
			(relationships
			  ( relationship 'lugar'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'1' #'1'
				false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				( refToInverseRelationship 'conLugar'  ( refToType 'Lugar' 'BolsaCarga'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'ConHorario'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			nil
			(relationships
			  ( relationship 'horario'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'1' #'1'
				false false false false false true true
				INITIALIZEDINCONSTRUCTOR
				''
				''
				''
				''
				( refToInverseRelationship 'conHorario'  ( refToType 'Horario' 'BolsaCarga'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Salida'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			   ( refToType 'Suceso' 'BolsaCarga'  ) 
			 )
			nil
			(relationships
			  ( relationship 'viaje'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'salida'  ( refToType 'Viaje' 'BolsaCarga'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Llegada'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			   ( refToType 'Suceso' 'BolsaCarga'  ) 
			 )
			nil
			(relationships
			  ( relationship 'viaje'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'llegada'  ( refToType 'Viaje' 'BolsaCarga'  )  ) 
				false
			   )

			 )
			nil
			nil
			false
		   )

		  ( type 'Suceso'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			   ( refToType 'ConLugar' 'BolsaCarga'  ) 
			   ( refToType 'ConHorario' 'BolsaCarga'  ) 
			 )
			(attributes
			  ( attribute 'lugarYHorario'
				nil nil
				nil
				nil
				'' nil
				#'1' #'1'
				false false true false false false false false
				ALWAYS
				''
				''
				''
				'calc clonenot lugar nombreLugar , horario comienzo fechaYHora'
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

				false
			   )

			 )
			nil
			nil
			nil
			false
		   )

		 )
		nil
	   )

	  ( module 'matrizero'
		nil nil
		nil
		nil
		nil nil
		nil
		(submodules
		  ( module 'core'
			nil nil
			nil
			nil
			nil nil
			nil
			(submodules
			  ( module 'cmps'
				nil nil
				nil
				nil
				nil nil
				(types
				  ( type 'Component'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Mx0object' 'matrizero' 'root'  ) 
					 )
					(attributes
					  ( attribute 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					 )
					(relationships
					  ( relationship 'uiSpeciication'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'components'  ( refToType 'UIspecification' 'matrizero' 'core' 'cmps'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'DataStore'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Component' 'matrizero' 'core' 'cmps'  ) 
					 )
					nil
					(relationships
					  ( relationship 'sourceOf'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false true false false false false
						NOCOMPUTATION
						''
						'nocreate uiSpecification dataProducers'
						''
						''
						( refToInverseRelationship 'sources'  ( refToType 'DataProducer' 'matrizero' 'core' 'cmps'  )  ) 
						false
					   )

					  ( relationship 'targetOf'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false true false false false false
						NOCOMPUTATION
						''
						'nocreate uiSpecification dataProducers'
						''
						''
						( refToInverseRelationship 'targets'  ( refToType 'DataProducer' 'matrizero' 'core' 'cmps'  )  ) 
						false
					   )

					  ( relationship 'uispec'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'dataStores'  ( refToType 'UIspecification' 'matrizero' 'core' 'cmps'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'DataProducer'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Component' 'matrizero' 'core' 'cmps'  ) 
					 )
					(attributes
					  ( attribute 'isMulti'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'lowerMultiplicity'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'1'
						''
						 ( refToType 'Integer' 'PrimitiveTypes'  ) 

						false
					   )

					  ( attribute 'higerMultiplicity'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'1'
						''
						 ( refToType 'Integer' 'PrimitiveTypes'  ) 

						false
					   )

					 )
					(relationships
					  ( relationship 'sources'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false true false false false false
						NOCOMPUTATION
						''
						'nocreate uiSpecification dataStores'
						''
						''
						( refToInverseRelationship 'sourceOf'  ( refToType 'DataStore' 'matrizero' 'core' 'cmps'  )  ) 
						false
					   )

					  ( relationship 'targets'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false true false false false false
						NOCOMPUTATION
						''
						'nocreate uiSpecification dataStores'
						''
						''
						( refToInverseRelationship 'targetOf'  ( refToType 'DataStore' 'matrizero' 'core' 'cmps'  )  ) 
						false
					   )

					  ( relationship 'uispec'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'dataProducers'  ( refToType 'UIspecification' 'matrizero' 'core' 'cmps'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Editor'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'DataStore' 'matrizero' 'core' 'cmps'  ) 
					   ( refToType 'FieldContainer' 'matrizero' 'core' 'cmps'  ) 
					 )
					nil
					nil
					nil
					nil
					false
				   )

				  ( type 'Query'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'DataProducer' 'matrizero' 'core' 'cmps'  ) 
					 )
					(attributes
					  ( attribute 'queryConstraint'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'Constraint' 'matrizero' 'core' 'constraints'  ) 

						false
					   )

					 )
					nil
					nil
					nil
					false
				   )

				  ( type 'Navigator'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'DataProducer' 'matrizero' 'core' 'cmps'  ) 
					 )
					(attributes
					  ( attribute 'traversalExpression'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'Constraint' 'matrizero' 'core' 'constraints'  ) 

						false
					   )

					 )
					nil
					nil
					nil
					false
				   )

				  ( type 'Creator'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'DataProducer' 'matrizero' 'core' 'cmps'  ) 
					 )
					nil
					nil
					nil
					nil
					false
				   )

				  ( type 'Linker'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'DataProducer' 'matrizero' 'core' 'cmps'  ) 
					 )
					nil
					nil
					nil
					nil
					false
				   )

				  ( type 'ArgumentsEditor'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'DataStore' 'matrizero' 'core' 'cmps'  ) 
					   ( refToType 'FieldContainer' 'matrizero' 'core' 'cmps'  ) 
					 )
					nil
					nil
					nil
					nil
					false
				   )

				  ( type 'Selector'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'DataProducer' 'matrizero' 'core' 'cmps'  ) 
					 )
					(attributes
					  ( attribute 'selectionConstraint'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'Constraint' 'matrizero' 'core' 'constraints'  ) 

						false
					   )

					  ( attribute 'multiSelectionConstraint'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'Constraint' 'matrizero' 'core' 'constraints'  ) 

						false
					   )

					 )
					nil
					nil
					nil
					false
				   )

				  ( type 'UIspecification'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Component' 'matrizero' 'core' 'cmps'  ) 
					 )
					nil
					(relationships
					  ( relationship 'components'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'uiSpeciication'  ( refToType 'Component' 'matrizero' 'core' 'cmps'  )  ) 
						false
					   )

					  ( relationship 'dataStores'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						ALWAYS
						''
						''
						''
						'clonenot components'
						( refToInverseRelationship 'uispec'  ( refToType 'DataStore' 'matrizero' 'core' 'cmps'  )  ) 
						false
					   )

					  ( relationship 'dataProducers'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						ALWAYS
						''
						''
						''
						'clonenot components'
						( refToInverseRelationship 'uispec'  ( refToType 'DataProducer' 'matrizero' 'core' 'cmps'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Field'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'InfoItem' 'matrizero' 'core' 'cmps'  ) 
					 )
					nil
					(relationships
					  ( relationship 'fieldContainer'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'fields'  ( refToType 'FieldContainer' 'matrizero' 'core' 'cmps'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Grid'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'DataStore' 'matrizero' 'core' 'cmps'  ) 
					 )
					nil
					(relationships
					  ( relationship 'columns'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'grid'  ( refToType 'Column' 'matrizero' 'core' 'cmps'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Column'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'InfoItem' 'matrizero' 'core' 'cmps'  ) 
					 )
					nil
					(relationships
					  ( relationship 'grid'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'columns'  ( refToType 'Grid' 'matrizero' 'core' 'cmps'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'InfoItem'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Mx0object' 'matrizero' 'root'  ) 
					 )
					(attributes
					  ( attribute 'title'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'Title' 'matrizero' 'support'  ) 

						false
					   )

					  ( attribute 'valueExpression'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'Constraint' 'matrizero' 'core' 'constraints'  ) 

						false
					   )

					  ( attribute 'isAvailableExpression'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'Constraint' 'matrizero' 'core' 'constraints'  ) 

						false
					   )

					  ( attribute 'isEditableExpression'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'Constraint' 'matrizero' 'core' 'constraints'  ) 

						false
					   )

					  ( attribute 'isNewValueValidExpression'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'Constraint' 'matrizero' 'core' 'constraints'  ) 

						false
					   )

					 )
					nil
					nil
					nil
					false
				   )

				  ( type 'FieldContainer'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Mx0object' 'matrizero' 'root'  ) 
					 )
					nil
					(relationships
					  ( relationship 'fields'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'fieldContainer'  ( refToType 'Field' 'matrizero' 'core' 'cmps'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				 )
				nil
			   )

			  ( module 'traversals'
				nil nil
				nil
				nil
				nil nil
				nil
				nil
			   )

			  ( module 'constraints'
				nil nil
				nil
				nil
				nil nil
				(types
				  ( type 'Constraint'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					nil
					(attributes
					  ( attribute 'constraintCode'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'Text' 'PrimitiveTypes'  ) 

						false
					   )

					 )
					(relationships
					  ( relationship 'constraintLanguage'
						nil nil
						nil
						nil
						REFERENCES nil
						#'1' #'1'
						false false true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						( refToInverseRelationship 'constraint'  ( refToType 'ConstraintLanguage' 'matrizero' 'core' 'constraints'  )  ) 
						false
					   )

					  ( relationship 'parseTree'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #'1'
						false false true false false true true
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						( refToInverseRelationship 'constraint'  ( refToType 'Node' 'matrizero' 'core' 'constraints'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'ConstraintLanguage'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					nil
					nil
					(relationships
					  ( relationship 'constraint'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'constraintLanguage'  ( refToType 'Constraint' 'matrizero' 'core' 'constraints'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'Node'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Mx0object' 'matrizero' 'root'  ) 
					 )
					nil
					(relationships
					  ( relationship 'unaryOperatorNode'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'operand'  ( refToType 'UnaryOperatorNode' 'matrizero' 'core' 'constraints'  )  ) 
						false
					   )

					  ( relationship 'binaryOperatorNode'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'firstOperand'  ( refToType 'BinaryOperatorNode' 'matrizero' 'core' 'constraints'  )  ) 
						false
					   )

					  ( relationship 'binaryOperatorNode'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'secondOperand'  ( refToType 'BinaryOperatorNode' 'matrizero' 'core' 'constraints'  )  ) 
						false
					   )

					  ( relationship 'constraint'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'parseTree'  ( refToType 'Constraint' 'matrizero' 'core' 'constraints'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'OperatorNode'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Node' 'matrizero' 'core' 'constraints'  ) 
					 )
					(attributes
					  ( attribute 'operatorSelector'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					 )
					nil
					nil
					nil
					false
				   )

				  ( type 'UnaryOperatorNode'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'OperatorNode' 'matrizero' 'core' 'constraints'  ) 
					 )
					nil
					(relationships
					  ( relationship 'operand'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'1' #'1'
						false false true false false true true
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						( refToInverseRelationship 'unaryOperatorNode'  ( refToType 'Node' 'matrizero' 'core' 'constraints'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'BinaryOperatorNode'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'OperatorNode' 'matrizero' 'core' 'constraints'  ) 
					 )
					nil
					(relationships
					  ( relationship 'firstOperand'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'1' #'1'
						false false true false false true true
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						( refToInverseRelationship 'binaryOperatorNode'  ( refToType 'Node' 'matrizero' 'core' 'constraints'  )  ) 
						false
					   )

					  ( relationship 'secondOperand'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'1' #'1'
						false false true false false true true
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						( refToInverseRelationship 'binaryOperatorNode'  ( refToType 'Node' 'matrizero' 'core' 'constraints'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'MessageNode'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Node' 'matrizero' 'core' 'constraints'  ) 
					 )
					(attributes
					  ( attribute 'messageSelector'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					 )
					nil
					nil
					nil
					false
				   )

				  ( type 'MessageParameterNode'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					nil
					nil
					nil
					nil
					nil
					false
				   )

				  ( type 'FeatureNode'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Node' 'matrizero' 'core' 'constraints'  ) 
					 )
					nil
					(relationships
					  ( relationship 'feature'
						nil nil
						nil
						nil
						REFERENCES nil
						#'1' #'1'
						false false true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						( refToInverseRelationship 'usedInConstraintsFeatureNodes'  ( refToType 'Feature' 'matrizero' 'meta'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				  ( type 'LiteralNode'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					nil
					(attributes
					  ( attribute 'dataType'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'DataType' 'matrizero' 'meta'  ) 

						false
					   )

					  ( attribute 'value'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						false
					   )

					 )
					nil
					nil
					nil
					false
				   )

				 )
				nil
			   )

			 )
		   )

		  ( module 'root'
			nil nil
			nil
			nil
			nil nil
			(types
			  ( type 'Mx0object'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
				 )
				nil
				nil
				nil
				nil
				false
			   )

			 )
			nil
		   )

		  ( module 'meta'
			nil nil
			nil
			nil
			nil nil
			(types
			  ( type 'Class'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'Metaclass' 'matrizero' 'meta'  ) 
				 )
				nil
				nil
				nil
				nil
				false
			   )

			  ( type 'Attribute'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'Feature' 'matrizero' 'meta'  ) 
				 )
				nil
				nil
				nil
				nil
				false
			   )

			  ( type 'Relationship'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'Feature' 'matrizero' 'meta'  ) 
				 )
				nil
				nil
				nil
				nil
				false
			   )

			  ( type 'Feature'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'Metaclass' 'matrizero' 'meta'  ) 
				 )
				nil
				(relationships
				  ( relationship 'usedInConstraintsFeatureNodes'
					nil nil
					nil
					nil
					REFERENCES nil
					#'0' #*
					false false false false false false false
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'feature'  ( refToType 'FeatureNode' 'matrizero' 'core' 'constraints'  )  ) 
					false
				   )

				 )
				nil
				nil
				false
			   )

			  ( type 'Metaclass'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'Mx0object' 'matrizero' 'root'  ) 
				 )
				nil
				nil
				nil
				nil
				false
			   )

			  ( type 'DataType'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'Metaclass' 'matrizero' 'meta'  ) 
				 )
				nil
				nil
				nil
				nil
				false
			   )

			 )
			nil
		   )

		  ( module 'support'
			nil nil
			nil
			nil
			nil nil
			(types
			  ( type 'Title'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'Mx0object' 'matrizero' 'root'  ) 
				 )
				(attributes
				  ( attribute 'textExpression'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false false false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					''
					''
					 ( refToType 'Constraint' 'matrizero' 'core' 'constraints'  ) 

					false
				   )

				  ( attribute 'imageExpression'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false false false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					''
					''
					 ( refToType 'Constraint' 'matrizero' 'core' 'constraints'  ) 

					false
				   )

				  ( attribute 'styleExpression'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false false false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					''
					''
					 ( refToType 'Style' 'matrizero' 'support'  ) 

					false
				   )

				 )
				nil
				nil
				nil
				false
			   )

			  ( type 'Style'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'Mx0object' 'matrizero' 'root'  ) 
				 )
				(attributes
				  ( attribute 'name'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false true false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					''
					''
					 ( refToType 'String' 'PrimitiveTypes'  ) 

					false
				   )

				  ( attribute 'styleSpec'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					''
					''
					 ( refToType 'String' 'PrimitiveTypes'  ) 

					false
				   )

				 )
				nil
				nil
				nil
				false
			   )

			 )
			nil
		   )

		 )
	   )

	  ( module 'gvMetrica'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'MetricaElement'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
			nil
			false
		   )

		 )
		(submodules
		  ( module 'CASI'
			nil nil
			nil
			nil
			nil nil
			(types
			  ( type 'CASI'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				nil
				nil
				(relationships
				  ( relationship 'inicio de Proyecto'
					nil nil
					nil
					nil
					AGGREGATES nil
					#'1' #'1'
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'casi'  ( refToType '010101 Inicio de Proyecto' 'gvMetrica' 'CASI' '010101 Inicio de Proyecto'  )  ) 
					false
				   )

				 )
				nil
				nil
				false
			   )

			 )
			(submodules
			  ( module '010101 Inicio de Proyecto'
				nil nil
				nil
				nil
				nil nil
				(types
				  ( type '010101 Inicio de Proyecto'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					nil
					nil
					(relationships
					  ( relationship 'objetivos'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'inicio de Proyecto'  ( refToType '010101 Objetivo' 'gvMetrica' 'CASI' '010101 Inicio de Proyecto' 'elementos'  )  ) 
						false
					   )

					  ( relationship 'casi'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'inicio de Proyecto'  ( refToType 'CASI' 'gvMetrica' 'CASI'  )  ) 
						false
					   )

					  ( relationship 'exclusiones'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship '010101 Inicio de Proyecto'  ( refToType '010101 Exclusion' 'gvMetrica' 'CASI' '010101 Inicio de Proyecto' 'elementos'  )  ) 
						false
					   )

					 )
					nil
					nil
					false
				   )

				 )
				(submodules
				  ( module 'elementos'
					nil nil
					nil
					nil
					nil nil
					(types
					  ( type '010101 Objetivo'
						(comments

						  ( comment 'Description'
							'Indicar brevemente el objetivo principal del proyecto.
'
						   )

						  ( comment 'Observaciones'
							'2 líneas apx.'
						   )

						 )

						nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'Con Texto' 'gvMetrica' 'auxiliares'  ) 
						 )
						nil
						(relationships
						  ( relationship 'inicio de Proyecto'
							nil nil
							nil
							nil
							isAGGREGATED nil
							#'1' #'1'
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'objetivos'  ( refToType '010101 Inicio de Proyecto' 'gvMetrica' 'CASI' '010101 Inicio de Proyecto'  )  ) 
							false
						   )

						 )
						nil
						nil
						false
					   )

					  ( type '010101 Exclusion'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'Con Texto' 'gvMetrica' 'auxiliares'  ) 
						 )
						nil
						(relationships
						  ( relationship '010101 Inicio de Proyecto'
							nil nil
							nil
							nil
							isAGGREGATED nil
							#'1' #'1'
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'exclusiones'  ( refToType '010101 Inicio de Proyecto' 'gvMetrica' 'CASI' '010101 Inicio de Proyecto'  )  ) 
							false
						   )

						 )
						nil
						nil
						false
					   )

					 )
					nil
				   )

				 )
			   )

			 )
		   )

		  ( module 'auxiliares'
			nil nil
			nil
			nil
			nil nil
			(types
			  ( type 'Con Texto'
				nil nil
				nil
				nil
				true false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
				 )
				nil
				nil
				nil
				nil
				false
			   )

			 )
			nil
		   )

		 )
	   )

	  ( module 'EMppReqs'
		nil nil
		nil
		nil
		nil nil
		nil
		(submodules
		  ( module 'EMppGlossaries'
			nil nil
			nil
			nil
			nil nil
			(types
			  ( type 'GlossaryTerm'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'EMppNamedObject' 'EMppReqs' 'EMppCommon'  ) 
				 )
				(attributes
				  ( attribute 'definition'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false true false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					''
					''
					 ( refToType 'Text' 'PrimitiveTypes'  ) 

					false
				   )

				 )
				(relationships
				  ( relationship 'glossary'
					nil nil
					nil
					nil
					isAGGREGATED nil
					#'1' #'1'
					false false false false false false false
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'terms'  ( refToType 'Glossary' 'EMppReqs' 'EMppGlossaries'  )  ) 
					false
				   )

				 )
				nil
				nil
				false
			   )

			  ( type 'Glossary'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'EMppNamedObject' 'EMppReqs' 'EMppCommon'  ) 
				 )
				nil
				(relationships
				  ( relationship 'terms'
					nil nil
					nil
					nil
					AGGREGATES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'glossary'  ( refToType 'GlossaryTerm' 'EMppReqs' 'EMppGlossaries'  )  ) 
					false
				   )

				 )
				nil
				nil
				false
			   )

			 )
			nil
		   )

		  ( module 'EMppCommon'
			nil nil
			nil
			nil
			nil nil
			(types
			  ( type 'EMppObject'
				nil nil
				nil
				nil
				true false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
				 )
				nil
				nil
				nil
				nil
				false
			   )

			  ( type 'EMppNamedObject'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'EMppObject' 'EMppReqs' 'EMppCommon'  ) 
				 )
				(attributes
				  ( attribute 'name'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false true false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					''
					''
					 ( refToType 'String' 'PrimitiveTypes'  ) 

					false
				   )

				 )
				nil
				nil
				nil
				false
			   )

			 )
			nil
		   )

		  ( module 'EMppRegulations'
			nil nil
			nil
			nil
			nil nil
			(types
			  ( type 'EMppLaw'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'EMppNamedObject' 'EMppReqs' 'EMppCommon'  ) 
				 )
				nil
				(relationships
				  ( relationship 'sections'
					nil nil
					nil
					nil
					AGGREGATES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'law'  ( refToType 'EMppSection' 'EMppReqs' 'EMppRegulations'  )  ) 
					false
				   )

				  ( relationship 'ammendments'
					nil nil
					nil
					nil
					REFERENCES nil
					#'0' #*
					false false false false false false false
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'ammendedLaw'  ( refToType 'EMppLawAmmendment' 'EMppReqs' 'EMppRegulations'  )  ) 
					false
				   )

				  ( relationship 'root'
					nil nil
					nil
					nil
					isAGGREGATED nil
					#'1' #'1'
					false false false false false false false
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'laws'  ( refToType 'EMppRequirements' 'EMppReqs' 'EMppRoots'  )  ) 
					false
				   )

				 )
				nil
				nil
				false
			   )

			  ( type 'EMppArticle'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'EMppNamedObject' 'EMppReqs' 'EMppCommon'  ) 
				 )
				nil
				(relationships
				  ( relationship 'section'
					nil nil
					nil
					nil
					isAGGREGATED nil
					#'1' #'1'
					false false false false false false false
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'articles'  ( refToType 'EMppSection' 'EMppReqs' 'EMppRegulations'  )  ) 
					false
				   )

				 )
				nil
				nil
				false
			   )

			  ( type 'EMppSection'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'EMppNamedObject' 'EMppReqs' 'EMppCommon'  ) 
				 )
				nil
				(relationships
				  ( relationship 'law'
					nil nil
					nil
					nil
					isAGGREGATED nil
					#'1' #'1'
					false false false false false false false
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'sections'  ( refToType 'EMppLaw' 'EMppReqs' 'EMppRegulations'  )  ) 
					false
				   )

				  ( relationship 'articles'
					nil nil
					nil
					nil
					AGGREGATES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'section'  ( refToType 'EMppArticle' 'EMppReqs' 'EMppRegulations'  )  ) 
					false
				   )

				 )
				nil
				nil
				false
			   )

			  ( type 'EMppLawAmmendment'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'EMppLaw' 'EMppReqs' 'EMppRegulations'  ) 
				 )
				nil
				(relationships
				  ( relationship 'ammendedLaw'
					nil nil
					nil
					nil
					REFERENCES nil
					#'1' #'1'
					false false true false false false true
					NOCOMPUTATION
					''
					'clonenot root laws'
					''
					''
					( refToInverseRelationship 'ammendments'  ( refToType 'EMppLaw' 'EMppReqs' 'EMppRegulations'  )  ) 
					false
				   )

				 )
				nil
				nil
				false
			   )

			 )
			nil
		   )

		  ( module 'EMppRoots'
			nil nil
			nil
			nil
			nil nil
			(types
			  ( type 'EMppRequirements'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false true false
				nil
				(supertypes
				   ( refToType 'EMppNamedObject' 'EMppReqs' 'EMppCommon'  ) 
				 )
				nil
				(relationships
				  ( relationship 'laws'
					nil nil
					nil
					nil
					AGGREGATES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'root'  ( refToType 'EMppLaw' 'EMppReqs' 'EMppRegulations'  )  ) 
					false
				   )

				 )
				nil
				nil
				false
			   )

			 )
			nil
		   )

		 )
	   )

	 )
	KRSimpleTranslationHolder kronoSimpleTranslationStore
   )! !

KRSimpleMetaInfoHolder initializeAfterLoad!
KRONO_Simple_MetaInfo initializeAfterLoad!

KRONO_Simple_MetaInfo loaded!
