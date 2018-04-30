'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:38:03 pm'!



((KRONO_Simple createSubApplication: #KRONO_Simple_Translation in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

KRONO_Simple_Translation becomeDefault!

SubApplication subclass: #KRONO_Simple_Translation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Simple_Translation becomeDefault!

TranslationsPersistencyHolder subclass: #KRSimpleTranslationHolder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Simple_Translation becomeDefault!

!KRSimpleTranslationHolder class publicMethodsFor: 'current'!

currentMessages
	"self  currentTranslation browsePath"
	"self  resetCurrentTranslations"
	"(self  currentTranslationStoreMethodSelector: self defaultCurrentTranslationSelector) browsePath"
	"self  resetTranslationSelectorsToIgnore"
	"self  translationSelectorsToIgnore"
	"self  translationSelectorsToIgnoreAdd: self defaultCurrentTranslationSelector"! !

!KRSimpleTranslationHolder class publicMethodsFor: 'default'!

defaultCurrentTranslationSelector
	"KRSimpleTranslationHolder  defaultCurrentTranslationSelector "

	^#kronoSimpleTranslationStore! !

!KRSimpleTranslationHolder class publicMethodsFor: 'translations persistence'!

kronoSimpleTranslationStore

	"(TranslationModelBase newFromPersistenceAsCode: KRSimpleTranslationHolder kronoSimpleTranslationStore) browsePath"

	self ojoTranslation.

	^   #( application 'KronoSimple_ApplicationTranslation'
	KRSimpleTranslationHolder kronoSimpleTranslationStore
	nil
	(groups
	  ( group 'Domain_Module'
		nil
		(items
		  ( item 'Domain_Module'
			'Dominio'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'DomainRootElements_Module'
		nil
		(items
		  ( item 'DomainRootElements_Module'
			'Elementos Raiz del Dominio'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'domainRoots_Module'
		nil
		(items
		  ( item 'domainRoots_Module'
			'Modulo de Dominio (Modelo)'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'domainRoots_Model'
		nil
		(items
		  ( item 'domainRoots_Model'
			'Modelo de Dominio'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'DomainRootElements::Domain_Type'
		nil
		(items
		  ( item 'Domain_Type'
			'Dominio'
			nil nil
			nil
		   )

		  ( item 'homes_Relationship'
			'Origenes'
			nil nil
			nil
		   )

		  ( item 'homes_Relationship_NameOne'
			'Origen'
			nil nil
			nil
		   )

		  ( item 'domainModelCMGO_Attribute'
			'ModeloDominio'
			nil nil
			nil
		   )

		  ( item 'domainModelCMGO_Attribute_NameOne'
			'domainModelCMGO'
			nil nil
			nil
		   )

		  ( item 'homesCMGO_Relationship'
			'Origenes'
			nil nil
			nil
		   )

		  ( item 'homesCMGO_Relationship_NameOne'
			'Origen'
			nil nil
			nil
		   )

		  ( item 'domainNameCMGO_Attribute'
			'NombreDominio'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'DomainRootElements::Home_Type'
		nil
		(items
		  ( item 'Home_Type'
			'Origen'
			nil nil
			nil
		   )

		  ( item 'domain_Relationship'
			'Dominio'
			nil nil
			nil
		   )

		  ( item 'homeIDCounterCMGO_Attribute'
			'ContadorIdentificadores'
			nil nil
			nil
		   )

		  ( item 'homeIDCounterCMGO_Attribute_NameOne'
			'homeIDCounterCMGO'
			nil nil
			nil
		   )

		  ( item 'homeRootsCMGO_Attribute'
			'Raices'
			nil nil
			nil
		   )

		  ( item 'homeRootsCMGO_Attribute_NameOne'
			'Raiz'
			nil nil
			nil
		   )

		  ( item 'metaInfoCMGO_Attribute'
			'MetaInfo'
			nil nil
			nil
		   )

		  ( item 'metaInfoCMGO_Attribute_NameOne'
			'metaInfoCMGO'
			nil nil
			nil
		   )

		  ( item 'homedElementsTypeMetaInfoCMGO_Attribute'
			'TipoRaizMetaInfo'
			nil nil
			nil
		   )

		  ( item 'homedElementsTypeMetaInfoCMGO_Attribute_NameOne'
			'homedElementsTypeMetaInfoCMGO'
			nil nil
			nil
		   )

		  ( item 'domainCMGO_Relationship'
			'Dominio'
			nil nil
			nil
		   )

		  ( item 'homeNameCMGO_Attribute'
			'NombreOrigen'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::Integer_Type'
		nil
		(items
		  ( item 'Integer_Type'
			'Entero'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::CODEElement_Type'
		nil
		(items
		  ( item 'CODEElement_Type'
			'MetaInfo'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::CODEType_Type'
		nil
		(items
		  ( item 'CODEType_Type'
			'Tipo%1'
			nil nil
			(usedItems
			   ( refToItemTranslation 'CODEElement_Type' 'PrimitiveTypes::CODEElement_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::CMGO_Type'
		nil
		(items
		  ( item 'CMGO_Type'
			'ObjetoGenerico'
			nil nil
			nil
		   )

		  ( item 'objectDomainCMGO_Attribute'
			'Dominio'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::CODEModel_Type'
		nil
		(items
		  ( item 'CODEModel_Type'
			'Modelo%1'
			nil nil
			(usedItems
			   ( refToItemTranslation 'CODEElement_Type' 'PrimitiveTypes::CODEElement_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'PrimitiveTypes_Module'
		nil
		(items
		  ( item 'PrimitiveTypes_Module'
			'Tipos Primitivos'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::String_Type'
		nil
		(items
		  ( item 'String_Type'
			'Letras'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'Aux_Module'
		nil
		(items
		  ( item 'Aux_Module'
			'AuxiliarUsoInterno'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'Nucleo_Module'
		nil
		(items
		  ( item 'Nucleo_Module'
			'Nucleo'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'Nucleo::Archivo_Type'
		nil
		(items
		  ( item 'Archivo_Type'
			'Archivo'
			nil nil
			nil
		   )

		  ( item 'historias_Relationship'
			'historias'
			nil nil
			nil
		   )

		  ( item 'historias_Relationship_NameOne'
			'Historia'
			nil nil
			nil
		   )

		  ( item 'personaClinicas_Relationship'
			'personasClinica'
			nil nil
			nil
		   )

		  ( item 'personaClinicas_Relationship_NameOne'
			'personaClinica'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'Nucleo::Historia_Type'
		nil
		(items
		  ( item 'Historia_Type'
			'Historia'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'Nucleo::Paciente_Type'
		nil
		(items
		  ( item 'Paciente_Type'
			'Paciente'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Persona_Type' 'Nucleo::Persona_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		  ( item 'direccions_Relationship'
			'direcciones'
			nil nil
			nil
		   )

		  ( item 'direccions_Relationship_NameOne'
			'direccion'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'TiposBasicos::DNINIF_Type'
		nil
		(items
		  ( item 'DNINIF_Type'
			'DNINIF'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::Date_Type'
		nil
		(items
		  ( item 'Date_Type'
			'Fecha'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::Time_Type'
		nil
		(items
		  ( item 'Time_Type'
			'Hora'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::Number_Type'
		nil
		(items
		  ( item 'Number_Type'
			'Numero'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'TiposBasicos::CodigoHistoria_Type'
		nil
		(items
		  ( item 'CodigoHistoria_Type'
			'CodigoHistoria (%1)'
			nil nil
			(usedItems
			   ( refToItemTranslation 'DNINIF_Type' 'TiposBasicos::DNINIF_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Nucleo::Persona_Type'
		nil
		(items
		  ( item 'Persona_Type'
			'Persona'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'TiposBasicos::NumeroTelefonico_Type'
		nil
		(items
		  ( item 'NumeroTelefonico_Type'
			'NumeroTelefonico'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'TiposBasicos::Telefono_Type'
		nil
		(items
		  ( item 'Telefono_Type'
			'TEL (%1)'
			nil nil
			(usedItems
			   ( refToItemTranslation 'NumeroTelefonico_Type' 'TiposBasicos::NumeroTelefonico_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::Fax_Type'
		nil
		(items
		  ( item 'Fax_Type'
			'FAX (%1)'
			nil nil
			(usedItems
			   ( refToItemTranslation 'NumeroTelefonico_Type' 'TiposBasicos::NumeroTelefonico_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::URL_Type'
		nil
		(items
		  ( item 'URL_Type'
			'URL'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'TiposBasicos::PaginaWeb_Type'
		nil
		(items
		  ( item 'PaginaWeb_Type'
			'PaginaWeb (%1)'
			nil nil
			(usedItems
			   ( refToItemTranslation 'URL_Type' 'TiposBasicos::URL_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::FechaCreacion_Type'
		nil
		(items
		  ( item 'FechaCreacion_Type'
			'%1 Creacion'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Date_Type' 'PrimitiveTypes::Date_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Nucleo::PersonaClinica_Type'
		nil
		(items
		  ( item 'PersonaClinica_Type'
			'PersonaClinica'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Persona_Type' 'Nucleo::Persona_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Conceptos::Exploracion_Type'
		nil
		(items
		  ( item 'Exploracion_Type'
			'Exploracion'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Concepto_Type' 'Nucleo::Concepto_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Nucleo::Concepto_Type'
		nil
		(items
		  ( item 'Concepto_Type'
			'Concepto'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'Conceptos::Diagnostico_Type'
		nil
		(items
		  ( item 'Diagnostico_Type'
			'Diagnostico'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Concepto_Type' 'Nucleo::Concepto_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Conceptos::Tratamiento_Type'
		nil
		(items
		  ( item 'Tratamiento_Type'
			'Tratamiento'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Concepto_Type' 'Nucleo::Concepto_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Conceptos::ResultadosAnalisis_Type'
		nil
		(items
		  ( item 'ResultadosAnalisis_Type'
			'ResultadosAnalisis'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Concepto_Type' 'Nucleo::Concepto_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Conceptos::SolicitudAnalisis_Type'
		nil
		(items
		  ( item 'SolicitudAnalisis_Type'
			'SolicitudAnalisis'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Concepto_Type' 'Nucleo::Concepto_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Nucleo::ActoClinico_Type'
		nil
		(items
		  ( item 'ActoClinico_Type'
			'ActoClinico'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'ActosClinicos::Diagnostico_Type'
		nil
		(items
		  ( item 'Diagnostico_Type'
			'Diagnostico'
			nil nil
			(usedItems
			   ( refToItemTranslation 'ActoClinico_Type' 'Nucleo::ActoClinico_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'ActosClinicos::Exploracion_Type'
		nil
		(items
		  ( item 'Exploracion_Type'
			'Exploracion'
			nil nil
			(usedItems
			   ( refToItemTranslation 'ActoClinico_Type' 'Nucleo::ActoClinico_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'ActosClinicos::ResultadosAnalisis_Type'
		nil
		(items
		  ( item 'ResultadosAnalisis_Type'
			'ResultadosAnalisis'
			nil nil
			(usedItems
			   ( refToItemTranslation 'ActoClinico_Type' 'Nucleo::ActoClinico_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'ActosClinicos::SolicitudAnalisis_Type'
		nil
		(items
		  ( item 'SolicitudAnalisis_Type'
			'SolicitudAnalisis'
			nil nil
			(usedItems
			   ( refToItemTranslation 'ActoClinico_Type' 'Nucleo::ActoClinico_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'ActosClinicos::Tratamiento_Type'
		nil
		(items
		  ( item 'Tratamiento_Type'
			'Tratamiento'
			nil nil
			(usedItems
			   ( refToItemTranslation 'ActoClinico_Type' 'Nucleo::ActoClinico_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::FechaFactura_Type'
		nil
		(items
		  ( item 'FechaFactura_Type'
			'%1 Factura'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Date_Type' 'PrimitiveTypes::Date_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::FechaInicioValidez_Type'
		nil
		(items
		  ( item 'FechaInicioValidez_Type'
			'FechaInicioValidez'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Date_Type' 'PrimitiveTypes::Date_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::FechaFinValidez_Type'
		nil
		(items
		  ( item 'FechaFinValidez_Type'
			'FechaFinValidez'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Date_Type' 'PrimitiveTypes::Date_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::FechaNacimiento_Type'
		nil
		(items
		  ( item 'FechaNacimiento_Type'
			'FechaNacimiento'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Date_Type' 'PrimitiveTypes::Date_Type' 'KronoSimple_ApplicationTranslation' ) 
			   ( refToItemTranslation 'SpanishDate_Type' 'PrimitiveTypes::SpanishDate_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::FechaCambioEstado_Type'
		nil
		(items
		  ( item 'FechaCambioEstado_Type'
			'FechaCambioEstado'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Date_Type' 'PrimitiveTypes::Date_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::MotivoRecomendacionCambioEstadoAdministrativo_Type'
		nil
		(items
		  ( item 'MotivoRecomendacionCambioEstadoAdministrativo_Type'
			'MotivoRecomendacionCambioEstadoAdministrativo'
			nil nil
			(usedItems
			   ( refToItemTranslation 'String_Type' 'PrimitiveTypes::String_Type' 'KronoSimple_ApplicationTranslation' ) 
			   ( refToItemTranslation 'Text_Type' 'PrimitiveTypes::Text_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::RespuestaAutorizacionCambioEstadoAdministrativo_Type'
		nil
		(items
		  ( item 'RespuestaAutorizacionCambioEstadoAdministrativo_Type'
			'RespuestaAutorizacionCambioEstadoAdministrativo'
			nil nil
			(usedItems
			   ( refToItemTranslation 'String_Type' 'PrimitiveTypes::String_Type' 'KronoSimple_ApplicationTranslation' ) 
			   ( refToItemTranslation 'Text_Type' 'PrimitiveTypes::Text_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::Boolean_Type'
		nil
		(items
		  ( item 'Boolean_Type'
			'Logico'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'TiposBasicos::FechaAtestado_Type'
		nil
		(items
		  ( item 'FechaAtestado_Type'
			'FechaAtestado'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Date_Type' 'PrimitiveTypes::Date_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::HoraAtestado_Type'
		nil
		(items
		  ( item 'HoraAtestado_Type'
			'HoraAtestado'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Time_Type' 'PrimitiveTypes::Time_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::FirmaDigital_Type'
		nil
		(items
		  ( item 'FirmaDigital_Type'
			'FirmaDigital'
			nil nil
			(usedItems
			   ( refToItemTranslation 'String_Type' 'PrimitiveTypes::String_Type' 'KronoSimple_ApplicationTranslation' ) 
			   ( refToItemTranslation 'Text_Type' 'PrimitiveTypes::Text_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::ComentarioHistoria_Type'
		nil
		(items
		  ( item 'ComentarioHistoria_Type'
			'ComentarioHistoria'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Text_Type' 'PrimitiveTypes::Text_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::Text_Type'
		nil
		(items
		  ( item 'Text_Type'
			'Texto'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'TiposBasicos::ComentarioNotificacionCambioEstadoAdministrativo_Type'
		nil
		(items
		  ( item 'ComentarioNotificacionCambioEstadoAdministrativo_Type'
			'ComentarioNotificacionCambioEstadoAdministrativo'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Text_Type' 'PrimitiveTypes::Text_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::FechaCreacionInteraccionCambioEstadoAdministrativo_Type'
		nil
		(items
		  ( item 'FechaCreacionInteraccionCambioEstadoAdministrativo_Type'
			'FechaCreacionInteraccionCambioEstadoAdministrativo'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Date_Type' 'PrimitiveTypes::Date_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Nucleo::Doctor_Type'
		nil
		(items
		  ( item 'Doctor_Type'
			'Doctor'
			nil nil
			(usedItems
			   ( refToItemTranslation 'PersonaClinica_Type' 'Nucleo::PersonaClinica_Type' 'KronoSimple_ApplicationTranslation' ) 
			   ( refToItemTranslation 'ParticipanteActoClinico_Type' 'Nucleo::ParticipanteActoClinico_Type' 'KronoSimple_ApplicationTranslation' ) 
			   ( refToItemTranslation 'Diplomado_Type' 'Nucleo::Diplomado_Type' 'KronoSimple_ApplicationTranslation' ) 
			   ( refToItemTranslation 'Recepcionista_Type' 'Nucleo::Recepcionista_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Nucleo::Tecnico_Type'
		nil
		(items
		  ( item 'Tecnico_Type'
			'Tecnico'
			nil nil
			(usedItems
			   ( refToItemTranslation 'PersonaClinica_Type' 'Nucleo::PersonaClinica_Type' 'KronoSimple_ApplicationTranslation' ) 
			   ( refToItemTranslation 'ParticipanteActoClinico_Type' 'Nucleo::ParticipanteActoClinico_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Nucleo::Diplomado_Type'
		nil
		(items
		  ( item 'Diplomado_Type'
			'Diplomado'
			nil nil
			(usedItems
			   ( refToItemTranslation 'PersonaClinica_Type' 'Nucleo::PersonaClinica_Type' 'KronoSimple_ApplicationTranslation' ) 
			   ( refToItemTranslation 'ParticipanteActoClinico_Type' 'Nucleo::ParticipanteActoClinico_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Nucleo::PersonaAuxiliar_Type'
		nil
		(items
		  ( item 'PersonaAuxiliar_Type'
			'PersonaAuxiliar'
			nil nil
			(usedItems
			   ( refToItemTranslation 'PersonaClinica_Type' 'Nucleo::PersonaClinica_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Nucleo::Administrativo_Type'
		nil
		(items
		  ( item 'Administrativo_Type'
			'Administrativo'
			nil nil
			(usedItems
			   ( refToItemTranslation 'PersonaClinica_Type' 'Nucleo::PersonaClinica_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Facturacion::Facturacion_Type'
		nil
		(items
		  ( item 'Facturacion_Type'
			'Facturacion'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'Facturacion::FacturacionAlquiler_Type'
		nil
		(items
		  ( item 'FacturacionAlquiler_Type'
			'FacturacionAlquiler'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Facturacion_Type' 'Facturacion::Facturacion_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Facturacion::FacturacionClinica_Type'
		nil
		(items
		  ( item 'FacturacionClinica_Type'
			'FacturacionClinica'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Facturacion_Type' 'Facturacion::Facturacion_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Nucleo::Recepcionista_Type'
		nil
		(items
		  ( item 'Recepcionista_Type'
			'Recepcionista'
			nil nil
			(usedItems
			   ( refToItemTranslation 'PersonaClinica_Type' 'Nucleo::PersonaClinica_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'ActosClinicos::OtrosActosClinicos_Type'
		nil
		(items
		  ( item 'OtrosActosClinicos_Type'
			'OtrosActosClinicos'
			nil nil
			(usedItems
			   ( refToItemTranslation 'ActoClinico_Type' 'Nucleo::ActoClinico_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::ComentarioActoClinico_Type'
		nil
		(items
		  ( item 'ComentarioActoClinico_Type'
			'ComentarioActoClinico'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Text_Type' 'PrimitiveTypes::Text_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Nucleo::ParticipanteActoClinico_Type'
		nil
		(items
		  ( item 'ParticipanteActoClinico_Type'
			'ParticipanteActoClinico'
			nil nil
			(usedItems
			   ( refToItemTranslation 'PersonaClinica_Type' 'Nucleo::PersonaClinica_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::TiempoParticipacionActoClinico_Type'
		nil
		(items
		  ( item 'TiempoParticipacionActoClinico_Type'
			'TiempoParticipacionActoClinico'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Number_Type' 'PrimitiveTypes::Number_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'EstadosAdministrativos::InteraccionCambioEstadoAdministrativo_Type'
		nil
		(items
		  ( item 'InteraccionCambioEstadoAdministrativo_Type'
			'InteraccionCambioEstadoAdministrativo'
			nil nil
			(usedItems
			   ( refToItemTranslation 'CMGO_Type' 'PrimitiveTypes::CMGO_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Organizaciones::OrigenPaciente_Type'
		nil
		(items
		  ( item 'OrigenPaciente_Type'
			'OrigenPaciente'
			nil nil
			(usedItems
			   ( refToItemTranslation 'CMGO_Type' 'PrimitiveTypes::CMGO_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Organizaciones::CoverturaPagoPrestaciones_Type'
		nil
		(items
		  ( item 'CoverturaPagoPrestaciones_Type'
			'CoverturaPagoPrestaciones'
			nil nil
			(usedItems
			   ( refToItemTranslation 'CMGO_Type' 'PrimitiveTypes::CMGO_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Organizaciones::IntermediacionPrestaciones_Type'
		nil
		(items
		  ( item 'IntermediacionPrestaciones_Type'
			'IntermediacionPrestaciones'
			nil nil
			(usedItems
			   ( refToItemTranslation 'CMGO_Type' 'PrimitiveTypes::CMGO_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::RazonSocial_Type'
		nil
		(items
		  ( item 'RazonSocial_Type'
			'RazonSocial'
			nil nil
			(usedItems
			   ( refToItemTranslation 'String_Type' 'PrimitiveTypes::String_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::NombreComercial_Type'
		nil
		(items
		  ( item 'NombreComercial_Type'
			'NombreComercial'
			nil nil
			(usedItems
			   ( refToItemTranslation 'String_Type' 'PrimitiveTypes::String_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::OtrosTiposContacto_Type'
		nil
		(items
		  ( item 'OtrosTiposContacto_Type'
			'OtrosTiposContacto'
			nil nil
			(usedItems
			   ( refToItemTranslation 'String_Type' 'PrimitiveTypes::String_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Tarifas::PrecioTarifaAlquiler_Type'
		nil
		(items
		  ( item 'PrecioTarifaAlquiler_Type'
			'PrecioTarifaAlquiler'
			nil nil
			(usedItems
			   ( refToItemTranslation 'CMGO_Type' 'PrimitiveTypes::CMGO_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Tarifas::TarifaAlquiler_Type'
		nil
		(items
		  ( item 'TarifaAlquiler_Type'
			'TarifaAlquiler'
			nil nil
			(usedItems
			   ( refToItemTranslation 'PrecioTarifaAlquiler_Type' 'Tarifas::PrecioTarifaAlquiler_Type' 'KronoSimple_ApplicationTranslation' ) 
			   ( refToItemTranslation 'CMGO_Type' 'PrimitiveTypes::CMGO_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::PorcentajeIVA_Type'
		nil
		(items
		  ( item 'PorcentajeIVA_Type'
			'PorcentajeIVA'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Number_Type' 'PrimitiveTypes::Number_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::NumeroFactura_Type'
		nil
		(items
		  ( item 'NumeroFactura_Type'
			'NumeroFactura'
			nil nil
			(usedItems
			   ( refToItemTranslation 'String_Type' 'PrimitiveTypes::String_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::SpanishDate_Type'
		nil
		(items
		  ( item 'SpanishDate_Type'
			'FechaEuropea'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'TiposBasicos::FechaCierreHistoria_Type'
		nil
		(items
		  ( item 'FechaCierreHistoria_Type'
			'FechaCierreHistoria'
			nil nil
			(usedItems
			   ( refToItemTranslation 'SpanishDate_Type' 'PrimitiveTypes::SpanishDate_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::ValorPrecioClinica_Type'
		nil
		(items
		  ( item 'ValorPrecioClinica_Type'
			'ValorPrecioClinica'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Number_Type' 'PrimitiveTypes::Number_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::DescripcionConceptoAlquiler_Type'
		nil
		(items
		  ( item 'DescripcionConceptoAlquiler_Type'
			'DescripcionConceptoAlquiler'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Text_Type' 'PrimitiveTypes::Text_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::DescripcionConceptoClinica_Type'
		nil
		(items
		  ( item 'DescripcionConceptoClinica_Type'
			'DescripcionConceptoClinica'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Text_Type' 'PrimitiveTypes::Text_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Organizaciones::OrigenPacienteReferido_Type'
		nil
		(items
		  ( item 'OrigenPacienteReferido_Type'
			'OrigenPacienteReferido'
			nil nil
			(usedItems
			   ( refToItemTranslation 'OrigenPaciente_Type' 'Organizaciones::OrigenPaciente_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Organizaciones::OrigenPacienteParticular_Type'
		nil
		(items
		  ( item 'OrigenPacienteParticular_Type'
			'OrigenPacienteParticular'
			nil nil
			(usedItems
			   ( refToItemTranslation 'OrigenPaciente_Type' 'Organizaciones::OrigenPaciente_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Facturacion::FacturaClinicaPorPaciente_Type'
		nil
		(items
		  ( item 'FacturaClinicaPorPaciente_Type'
			'FacturaClinicaPorPaciente'
			nil nil
			(usedItems
			   ( refToItemTranslation 'FacturacionClinica_Type' 'Facturacion::FacturacionClinica_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::NumeroHistoria_Type'
		nil
		(items
		  ( item 'NumeroHistoria_Type'
			'NumeroHistoria'
			nil nil
			(usedItems
			   ( refToItemTranslation 'String_Type' 'PrimitiveTypes::String_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::NombreEntradaCalendario_Type'
		nil
		(items
		  ( item 'NombreEntradaCalendario_Type'
			'NombreEntradaCalendario'
			nil nil
			(usedItems
			   ( refToItemTranslation 'String_Type' 'PrimitiveTypes::String_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::HoraComienzo_Type'
		nil
		(items
		  ( item 'HoraComienzo_Type'
			'HoraComienzo'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Time_Type' 'PrimitiveTypes::Time_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::HoraFin_Type'
		nil
		(items
		  ( item 'HoraFin_Type'
			'HoraFin'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Time_Type' 'PrimitiveTypes::Time_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'ActosClinicos::TotalizadoParaFactura_Type'
		nil
		(items
		  ( item 'TotalizadoParaFactura_Type'
			'TotalizadoParaFactura'
			nil nil
			(usedItems
			   ( refToItemTranslation 'ActoClinico_Type' 'Nucleo::ActoClinico_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Facturacion::FacturaClinicaAseguradoDetallada_Type'
		nil
		(items
		  ( item 'FacturaClinicaAseguradoDetallada_Type'
			'FacturaClinicaAseguradoDetallada'
			nil nil
			(usedItems
			   ( refToItemTranslation 'FacturacionClinica_Type' 'Facturacion::FacturacionClinica_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Facturacion::FacturaClinicaParticular_Type'
		nil
		(items
		  ( item 'FacturaClinicaParticular_Type'
			'FacturaClinicaParticular'
			nil nil
			(usedItems
			   ( refToItemTranslation 'FacturacionClinica_Type' 'Facturacion::FacturacionClinica_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Calendario::Ejercicio_Type'
		nil
		(items
		  ( item 'periodos_Relationship'
			'PPPerrrriodddoosss'
			nil nil
			nil
		   )

		  ( item 'periodos_Relationship_NameOne'
			'PPeeRRiiOOddOOss'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Customers::Sole Trader_Type'
		nil
		(items
		  ( item 'Date Started Self Employment_Attribute'
			'Date Started Self Employment'
			nil nil
			nil
		   )

		  ( item 'DistrictNumber_Attribute'
			'District Number'
			nil nil
			nil
		   )

		  ( item 'ProcessingOffice_Attribute'
			'Processing Office'
			nil nil
			nil
		   )

		  ( item 'DateOfBirth_Attribute'
			'Date Of Birth'
			nil nil
			nil
		   )

		  ( item 'NINOStatus_Attribute'
			'NINO Status'
			nil nil
			nil
		   )

		  ( item 'IsDeceased_Attribute'
			'Is Deceased'
			nil nil
			nil
		   )

		  ( item 'DaytimeTelephoneNumber_Attribute'
			'Daytime Telephone Number'
			nil nil
			nil
		   )

		  ( item 'Second Name_Attribute'
			'Second Name'
			nil nil
			nil
		   )

		  ( item 'FirstName_Attribute'
			'First Name'
			nil nil
			nil
		   )

		  ( item 'sole Trader Agent_Relationship'
			'agent'
			nil nil
			nil
		   )

		  ( item 'sole Trader Agent_Relationship_NameOne'
			'sole Trader Agent'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Non Identifiable::Enumerations::TitleKind_Type'
		nil
		(items
		  ( item 'TitleKind_Type'
			'Title Kind'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Customers::CID Sole Trader Details_Type'
		nil
		(items
		  ( item 'CID Sole Trader Details_Type'
			'CID Sole Trader Details'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Customers::CIS Sole Trader Details_Type'
		nil
		(items
		  ( item 'Type Of Work_Attribute'
			'Type Of Work'
			nil nil
			nil
		   )

		  ( item 'Send Contractor Output To Agent_Attribute'
			'Send Contractor Output To Agent'
			nil nil
			nil
		   )

		  ( item 'Use Existing Agent_Attribute'
			'Use Existing Agent'
			nil nil
			nil
		   )

		  ( item 'VAT Registration Number_Attribute'
			'VAT Registration Number'
			nil nil
			nil
		   )

		  ( item 'Date Started Or Plan To Start Construction_Attribute'
			'Date Started Or Plan To Start Construction'
			nil nil
			nil
		   )

		  ( item 'Has Been Paid By Contractor_Attribute'
			'Has Been Paid By Contractor'
			nil nil
			nil
		   )

		  ( item 'Email Address_Attribute'
			'Email Address'
			nil nil
			nil
		   )

		  ( item 'FaxNumber_Attribute'
			'FaxNumber'
			nil nil
			nil
		   )

		  ( item 'MobilePhoneNumber_Attribute'
			'Mobile Phone Number'
			nil nil
			nil
		   )

		  ( item 'Evening Telephone Number_Attribute'
			'Evening Telephone Number'
			nil nil
			nil
		   )

		  ( item 'CISTradingName_Attribute'
			'CIS Trading Name'
			nil nil
			nil
		   )

		  ( item 'ID Documents Proving ID_Attribute'
			'ID Documents Proving ID'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Agents::Sole Trader Agent_Type'
		nil
		(items
		  ( item 'soleTraders_Relationship'
			'sole Traders'
			nil nil
			nil
		   )

		  ( item 'soleTraders_Relationship_NameOne'
			'soleTraders'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Customers::Partnereable_Type'
		nil
		(items
		  ( item 'partnerInPartnerships_Relationship'
			'partner In Partnerships'
			nil nil
			nil
		   )

		  ( item 'partnerInPartnerships_Relationship_NameOne'
			'partnerInPartnerships'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Customers::Partner_Type'
		nil
		(items
		  ( item 'UTRsPair_Attribute'
			'UTRs Pair'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Registrations::Partnership Partner Subcontractor Registration_Type'
		nil
		(items
		  ( item 'UTRs Pair_Attribute'
			'UTRs Pair'
			nil nil
			nil
		   )

		  ( item 'UTRsPair_Attribute'
			'UTRs Pair'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Customers::Address Sole Trader Details_Type'
		nil
		(items
		  ( item 'CIS Correspondence Address_Attribute'
			'CIS Corr. Address'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Registrations::Verification_Type'
		nil
		(items
		  ( item 'Verification Date_Attribute'
			'Verification Date'
			nil nil
			nil
		   )

		  ( item 'Verification Number_Attribute'
			'Verification Number'
			nil nil
			nil
		   )

		  ( item 'VerificationNumber_Attribute'
			'Verification Number'
			nil nil
			nil
		   )

		  ( item 'VerificationDate_Attribute'
			'Verification Date'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Registrations::Individual Verification_Type'
		nil
		(items
		  ( item 'VerificationNumberAndSuffix_Attribute'
			'Verification Number And Suffix'
			nil nil
			nil
		   )

		  ( item 'VerificationSuffix_Attribute'
			'Verification Suffix'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Returns::Multiple Return_Type'
		nil
		(items
		  ( item 'isResolved_Attribute'
			'IS Resolved'
			nil nil
			nil
		   )

		  ( item 'DisplayName_Attribute'
			'Display Name'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Returns::Processed Return_Type'
		nil
		(items
		  ( item 'IsEffective_Attribute'
			'Is Effective'
			nil nil
			nil
		   )

		  ( item 'Display Name_Attribute'
			'Display Name'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Returns::Abstract Return_Type'
		nil
		(items
		  ( item 'TraceToAction_Attribute'
			'Trace To Action'
			nil nil
			nil
		   )

		  ( item 'TraceFromAction_Attribute'
			'Trace From Action'
			nil nil
			nil
		   )

		  ( item 'AmendedDate_Attribute'
			'Amended Date'
			nil nil
			nil
		   )

		  ( item 'LoggedDate_Attribute'
			'Logged Date'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Returns::Unmatched Return_Type'
		nil
		(items
		  ( item 'IS Resolved_Attribute'
			'IS Resolved'
			nil nil
			nil
		   )

		  ( item 'AORef_Attribute'
			'AO Ref'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Non Identifiable::Address_Type'
		nil
		(items
		  ( item 'PostCode_Attribute'
			'Post Code'
			nil nil
			nil
		   )

		  ( item 'DisplayName_Attribute'
			'Display Name'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Contacts::Contact_Type'
		nil
		(items
		  ( item 'Contact_Type'
			'Contact'
			nil nil
			(usedItems
			   ( refToItemTranslation 'CMGO_Type' 'PrimitiveTypes::CMGO_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Returns::Liability Period_Type'
		nil
		(items
		  ( item 'allReturns_Relationship'
			'All Returns'
			nil nil
			nil
		   )

		  ( item 'allReturns_Relationship_NameOne'
			'allReturns'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Returns::Subcontractor Payment_Type'
		nil
		(items
		  ( item 'DisplayUTRs_Attribute'
			'Display UTR (s)'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'gvMetrica_Module'
		nil
		(items
		  ( item 'gvMetrica_Module'
			'gvMetrica'
			nil nil
			nil
		   )

		 )
	   )

	 )
   )!

kronoSimpleTranslationStoreSpanish

	"(TranslationModelBase newFromPersistenceAsCode: KRSimpleTranslationHolder kronoSimpleTranslationStore) browsePath"

	self ojoTranslation.

	^   #( application 'KronoSimple_ApplicationTranslation'
	KRSimpleTranslationHolder kronoSimpleTranslationStore
	nil
	(groups
	  ( group 'Domain_Module'
		nil
		(items
		  ( item 'Domain_Module'
			'Dominio'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'DomainRootElements_Module'
		nil
		(items
		  ( item 'DomainRootElements_Module'
			'Elementos Raiz del Dominio'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'domainRoots_Module'
		nil
		(items
		  ( item 'domainRoots_Module'
			'Modulo de Dominio (Modelo)'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'domainRoots_Model'
		nil
		(items
		  ( item 'domainRoots_Model'
			'Modelo de Dominio'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'DomainRootElements::Domain_Type'
		nil
		(items
		  ( item 'Domain_Type'
			'Dominio'
			nil nil
			nil
		   )

		  ( item 'homes_Relationship'
			'Origenes'
			nil nil
			nil
		   )

		  ( item 'homes_Relationship_NameOne'
			'Origen'
			nil nil
			nil
		   )

		  ( item 'domainModelCMGO_Attribute'
			'ModeloDominio'
			nil nil
			nil
		   )

		  ( item 'domainModelCMGO_Attribute_NameOne'
			'domainModelCMGO'
			nil nil
			nil
		   )

		  ( item 'homesCMGO_Relationship'
			'Origenes'
			nil nil
			nil
		   )

		  ( item 'homesCMGO_Relationship_NameOne'
			'Origen'
			nil nil
			nil
		   )

		  ( item 'domainNameCMGO_Attribute'
			'NombreDominio'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'DomainRootElements::Home_Type'
		nil
		(items
		  ( item 'Home_Type'
			'Origen'
			nil nil
			nil
		   )

		  ( item 'domain_Relationship'
			'Dominio'
			nil nil
			nil
		   )

		  ( item 'homeIDCounterCMGO_Attribute'
			'ContadorIdentificadores'
			nil nil
			nil
		   )

		  ( item 'homeIDCounterCMGO_Attribute_NameOne'
			'homeIDCounterCMGO'
			nil nil
			nil
		   )

		  ( item 'homeRootsCMGO_Attribute'
			'Raices'
			nil nil
			nil
		   )

		  ( item 'homeRootsCMGO_Attribute_NameOne'
			'Raiz'
			nil nil
			nil
		   )

		  ( item 'metaInfoCMGO_Attribute'
			'MetaInfo'
			nil nil
			nil
		   )

		  ( item 'metaInfoCMGO_Attribute_NameOne'
			'metaInfoCMGO'
			nil nil
			nil
		   )

		  ( item 'homedElementsTypeMetaInfoCMGO_Attribute'
			'TipoRaizMetaInfo'
			nil nil
			nil
		   )

		  ( item 'homedElementsTypeMetaInfoCMGO_Attribute_NameOne'
			'homedElementsTypeMetaInfoCMGO'
			nil nil
			nil
		   )

		  ( item 'domainCMGO_Relationship'
			'Dominio'
			nil nil
			nil
		   )

		  ( item 'homeNameCMGO_Attribute'
			'NombreOrigen'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::Integer_Type'
		nil
		(items
		  ( item 'Integer_Type'
			'Entero'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::CODEElement_Type'
		nil
		(items
		  ( item 'CODEElement_Type'
			'MetaInfo'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::CODEType_Type'
		nil
		(items
		  ( item 'CODEType_Type'
			'Tipo%1'
			nil nil
			(usedItems
			   ( refToItemTranslation 'CODEElement_Type' 'PrimitiveTypes::CODEElement_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::CMGO_Type'
		nil
		(items
		  ( item 'CMGO_Type'
			'ObjetoGenerico'
			nil nil
			nil
		   )

		  ( item 'objectDomainCMGO_Attribute'
			'Dominio'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::CODEModel_Type'
		nil
		(items
		  ( item 'CODEModel_Type'
			'Modelo%1'
			nil nil
			(usedItems
			   ( refToItemTranslation 'CODEElement_Type' 'PrimitiveTypes::CODEElement_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'PrimitiveTypes_Module'
		nil
		(items
		  ( item 'PrimitiveTypes_Module'
			'Tipos Primitivos'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::String_Type'
		nil
		(items
		  ( item 'String_Type'
			'Letras'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'Aux_Module'
		nil
		(items
		  ( item 'Aux_Module'
			'AuxiliarUsoInterno'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'Nucleo_Module'
		nil
		(items
		  ( item 'Nucleo_Module'
			'Nucleo'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'Nucleo::Archivo_Type'
		nil
		(items
		  ( item 'Archivo_Type'
			'Archivo'
			nil nil
			nil
		   )

		  ( item 'historias_Relationship'
			'historias'
			nil nil
			nil
		   )

		  ( item 'historias_Relationship_NameOne'
			'Historia'
			nil nil
			nil
		   )

		  ( item 'personaClinicas_Relationship'
			'personasClinica'
			nil nil
			nil
		   )

		  ( item 'personaClinicas_Relationship_NameOne'
			'personaClinica'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'Nucleo::Historia_Type'
		nil
		(items
		  ( item 'Historia_Type'
			'Historia'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'Nucleo::Paciente_Type'
		nil
		(items
		  ( item 'Paciente_Type'
			'Paciente'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Persona_Type' 'Nucleo::Persona_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		  ( item 'direccions_Relationship'
			'direcciones'
			nil nil
			nil
		   )

		  ( item 'direccions_Relationship_NameOne'
			'direccion'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'TiposBasicos::DNINIF_Type'
		nil
		(items
		  ( item 'DNINIF_Type'
			'DNINIF'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::Date_Type'
		nil
		(items
		  ( item 'Date_Type'
			'Fecha'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::Time_Type'
		nil
		(items
		  ( item 'Time_Type'
			'Hora'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::Number_Type'
		nil
		(items
		  ( item 'Number_Type'
			'Numero'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'TiposBasicos::CodigoHistoria_Type'
		nil
		(items
		  ( item 'CodigoHistoria_Type'
			'CodigoHistoria (%1)'
			nil nil
			(usedItems
			   ( refToItemTranslation 'DNINIF_Type' 'TiposBasicos::DNINIF_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Nucleo::Persona_Type'
		nil
		(items
		  ( item 'Persona_Type'
			'Persona'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'TiposBasicos::NumeroTelefonico_Type'
		nil
		(items
		  ( item 'NumeroTelefonico_Type'
			'NumeroTelefonico'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'TiposBasicos::Telefono_Type'
		nil
		(items
		  ( item 'Telefono_Type'
			'TEL (%1)'
			nil nil
			(usedItems
			   ( refToItemTranslation 'NumeroTelefonico_Type' 'TiposBasicos::NumeroTelefonico_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::Fax_Type'
		nil
		(items
		  ( item 'Fax_Type'
			'FAX (%1)'
			nil nil
			(usedItems
			   ( refToItemTranslation 'NumeroTelefonico_Type' 'TiposBasicos::NumeroTelefonico_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::URL_Type'
		nil
		(items
		  ( item 'URL_Type'
			'URL'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'TiposBasicos::PaginaWeb_Type'
		nil
		(items
		  ( item 'PaginaWeb_Type'
			'PaginaWeb (%1)'
			nil nil
			(usedItems
			   ( refToItemTranslation 'URL_Type' 'TiposBasicos::URL_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::FechaCreacion_Type'
		nil
		(items
		  ( item 'FechaCreacion_Type'
			'%1 Creacion'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Date_Type' 'PrimitiveTypes::Date_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Nucleo::PersonaClinica_Type'
		nil
		(items
		  ( item 'PersonaClinica_Type'
			'PersonaClinica'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Persona_Type' 'Nucleo::Persona_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Conceptos::Exploracion_Type'
		nil
		(items
		  ( item 'Exploracion_Type'
			'Exploracion'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Concepto_Type' 'Nucleo::Concepto_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Nucleo::Concepto_Type'
		nil
		(items
		  ( item 'Concepto_Type'
			'Concepto'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'Conceptos::Diagnostico_Type'
		nil
		(items
		  ( item 'Diagnostico_Type'
			'Diagnostico'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Concepto_Type' 'Nucleo::Concepto_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Conceptos::Tratamiento_Type'
		nil
		(items
		  ( item 'Tratamiento_Type'
			'Tratamiento'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Concepto_Type' 'Nucleo::Concepto_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Conceptos::ResultadosAnalisis_Type'
		nil
		(items
		  ( item 'ResultadosAnalisis_Type'
			'ResultadosAnalisis'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Concepto_Type' 'Nucleo::Concepto_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Conceptos::SolicitudAnalisis_Type'
		nil
		(items
		  ( item 'SolicitudAnalisis_Type'
			'SolicitudAnalisis'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Concepto_Type' 'Nucleo::Concepto_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Nucleo::ActoClinico_Type'
		nil
		(items
		  ( item 'ActoClinico_Type'
			'ActoClinico'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'ActosClinicos::Diagnostico_Type'
		nil
		(items
		  ( item 'Diagnostico_Type'
			'Diagnostico'
			nil nil
			(usedItems
			   ( refToItemTranslation 'ActoClinico_Type' 'Nucleo::ActoClinico_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'ActosClinicos::Exploracion_Type'
		nil
		(items
		  ( item 'Exploracion_Type'
			'Exploracion'
			nil nil
			(usedItems
			   ( refToItemTranslation 'ActoClinico_Type' 'Nucleo::ActoClinico_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'ActosClinicos::ResultadosAnalisis_Type'
		nil
		(items
		  ( item 'ResultadosAnalisis_Type'
			'ResultadosAnalisis'
			nil nil
			(usedItems
			   ( refToItemTranslation 'ActoClinico_Type' 'Nucleo::ActoClinico_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'ActosClinicos::SolicitudAnalisis_Type'
		nil
		(items
		  ( item 'SolicitudAnalisis_Type'
			'SolicitudAnalisis'
			nil nil
			(usedItems
			   ( refToItemTranslation 'ActoClinico_Type' 'Nucleo::ActoClinico_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'ActosClinicos::Tratamiento_Type'
		nil
		(items
		  ( item 'Tratamiento_Type'
			'Tratamiento'
			nil nil
			(usedItems
			   ( refToItemTranslation 'ActoClinico_Type' 'Nucleo::ActoClinico_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::FechaFactura_Type'
		nil
		(items
		  ( item 'FechaFactura_Type'
			'%1 Factura'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Date_Type' 'PrimitiveTypes::Date_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::FechaInicioValidez_Type'
		nil
		(items
		  ( item 'FechaInicioValidez_Type'
			'FechaInicioValidez'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Date_Type' 'PrimitiveTypes::Date_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::FechaFinValidez_Type'
		nil
		(items
		  ( item 'FechaFinValidez_Type'
			'FechaFinValidez'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Date_Type' 'PrimitiveTypes::Date_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::FechaNacimiento_Type'
		nil
		(items
		  ( item 'FechaNacimiento_Type'
			'FechaNacimiento'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Date_Type' 'PrimitiveTypes::Date_Type' 'KronoSimple_ApplicationTranslation' ) 
			   ( refToItemTranslation 'SpanishDate_Type' 'PrimitiveTypes::SpanishDate_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::FechaCambioEstado_Type'
		nil
		(items
		  ( item 'FechaCambioEstado_Type'
			'FechaCambioEstado'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Date_Type' 'PrimitiveTypes::Date_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::MotivoRecomendacionCambioEstadoAdministrativo_Type'
		nil
		(items
		  ( item 'MotivoRecomendacionCambioEstadoAdministrativo_Type'
			'MotivoRecomendacionCambioEstadoAdministrativo'
			nil nil
			(usedItems
			   ( refToItemTranslation 'String_Type' 'PrimitiveTypes::String_Type' 'KronoSimple_ApplicationTranslation' ) 
			   ( refToItemTranslation 'Text_Type' 'PrimitiveTypes::Text_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::RespuestaAutorizacionCambioEstadoAdministrativo_Type'
		nil
		(items
		  ( item 'RespuestaAutorizacionCambioEstadoAdministrativo_Type'
			'RespuestaAutorizacionCambioEstadoAdministrativo'
			nil nil
			(usedItems
			   ( refToItemTranslation 'String_Type' 'PrimitiveTypes::String_Type' 'KronoSimple_ApplicationTranslation' ) 
			   ( refToItemTranslation 'Text_Type' 'PrimitiveTypes::Text_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::Boolean_Type'
		nil
		(items
		  ( item 'Boolean_Type'
			'Logico'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'TiposBasicos::FechaAtestado_Type'
		nil
		(items
		  ( item 'FechaAtestado_Type'
			'FechaAtestado'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Date_Type' 'PrimitiveTypes::Date_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::HoraAtestado_Type'
		nil
		(items
		  ( item 'HoraAtestado_Type'
			'HoraAtestado'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Time_Type' 'PrimitiveTypes::Time_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::FirmaDigital_Type'
		nil
		(items
		  ( item 'FirmaDigital_Type'
			'FirmaDigital'
			nil nil
			(usedItems
			   ( refToItemTranslation 'String_Type' 'PrimitiveTypes::String_Type' 'KronoSimple_ApplicationTranslation' ) 
			   ( refToItemTranslation 'Text_Type' 'PrimitiveTypes::Text_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::ComentarioHistoria_Type'
		nil
		(items
		  ( item 'ComentarioHistoria_Type'
			'ComentarioHistoria'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Text_Type' 'PrimitiveTypes::Text_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::Text_Type'
		nil
		(items
		  ( item 'Text_Type'
			'Texto'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'TiposBasicos::ComentarioNotificacionCambioEstadoAdministrativo_Type'
		nil
		(items
		  ( item 'ComentarioNotificacionCambioEstadoAdministrativo_Type'
			'ComentarioNotificacionCambioEstadoAdministrativo'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Text_Type' 'PrimitiveTypes::Text_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::FechaCreacionInteraccionCambioEstadoAdministrativo_Type'
		nil
		(items
		  ( item 'FechaCreacionInteraccionCambioEstadoAdministrativo_Type'
			'FechaCreacionInteraccionCambioEstadoAdministrativo'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Date_Type' 'PrimitiveTypes::Date_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Nucleo::Doctor_Type'
		nil
		(items
		  ( item 'Doctor_Type'
			'Doctor'
			nil nil
			(usedItems
			   ( refToItemTranslation 'PersonaClinica_Type' 'Nucleo::PersonaClinica_Type' 'KronoSimple_ApplicationTranslation' ) 
			   ( refToItemTranslation 'ParticipanteActoClinico_Type' 'Nucleo::ParticipanteActoClinico_Type' 'KronoSimple_ApplicationTranslation' ) 
			   ( refToItemTranslation 'Diplomado_Type' 'Nucleo::Diplomado_Type' 'KronoSimple_ApplicationTranslation' ) 
			   ( refToItemTranslation 'Recepcionista_Type' 'Nucleo::Recepcionista_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Nucleo::Tecnico_Type'
		nil
		(items
		  ( item 'Tecnico_Type'
			'Tecnico'
			nil nil
			(usedItems
			   ( refToItemTranslation 'PersonaClinica_Type' 'Nucleo::PersonaClinica_Type' 'KronoSimple_ApplicationTranslation' ) 
			   ( refToItemTranslation 'ParticipanteActoClinico_Type' 'Nucleo::ParticipanteActoClinico_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Nucleo::Diplomado_Type'
		nil
		(items
		  ( item 'Diplomado_Type'
			'Diplomado'
			nil nil
			(usedItems
			   ( refToItemTranslation 'PersonaClinica_Type' 'Nucleo::PersonaClinica_Type' 'KronoSimple_ApplicationTranslation' ) 
			   ( refToItemTranslation 'ParticipanteActoClinico_Type' 'Nucleo::ParticipanteActoClinico_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Nucleo::PersonaAuxiliar_Type'
		nil
		(items
		  ( item 'PersonaAuxiliar_Type'
			'PersonaAuxiliar'
			nil nil
			(usedItems
			   ( refToItemTranslation 'PersonaClinica_Type' 'Nucleo::PersonaClinica_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Nucleo::Administrativo_Type'
		nil
		(items
		  ( item 'Administrativo_Type'
			'Administrativo'
			nil nil
			(usedItems
			   ( refToItemTranslation 'PersonaClinica_Type' 'Nucleo::PersonaClinica_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Facturacion::Facturacion_Type'
		nil
		(items
		  ( item 'Facturacion_Type'
			'Facturacion'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'Facturacion::FacturacionAlquiler_Type'
		nil
		(items
		  ( item 'FacturacionAlquiler_Type'
			'FacturacionAlquiler'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Facturacion_Type' 'Facturacion::Facturacion_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Facturacion::FacturacionClinica_Type'
		nil
		(items
		  ( item 'FacturacionClinica_Type'
			'FacturacionClinica'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Facturacion_Type' 'Facturacion::Facturacion_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Nucleo::Recepcionista_Type'
		nil
		(items
		  ( item 'Recepcionista_Type'
			'Recepcionista'
			nil nil
			(usedItems
			   ( refToItemTranslation 'PersonaClinica_Type' 'Nucleo::PersonaClinica_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'ActosClinicos::OtrosActosClinicos_Type'
		nil
		(items
		  ( item 'OtrosActosClinicos_Type'
			'OtrosActosClinicos'
			nil nil
			(usedItems
			   ( refToItemTranslation 'ActoClinico_Type' 'Nucleo::ActoClinico_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::ComentarioActoClinico_Type'
		nil
		(items
		  ( item 'ComentarioActoClinico_Type'
			'ComentarioActoClinico'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Text_Type' 'PrimitiveTypes::Text_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Nucleo::ParticipanteActoClinico_Type'
		nil
		(items
		  ( item 'ParticipanteActoClinico_Type'
			'ParticipanteActoClinico'
			nil nil
			(usedItems
			   ( refToItemTranslation 'PersonaClinica_Type' 'Nucleo::PersonaClinica_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::TiempoParticipacionActoClinico_Type'
		nil
		(items
		  ( item 'TiempoParticipacionActoClinico_Type'
			'TiempoParticipacionActoClinico'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Number_Type' 'PrimitiveTypes::Number_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'EstadosAdministrativos::InteraccionCambioEstadoAdministrativo_Type'
		nil
		(items
		  ( item 'InteraccionCambioEstadoAdministrativo_Type'
			'InteraccionCambioEstadoAdministrativo'
			nil nil
			(usedItems
			   ( refToItemTranslation 'CMGO_Type' 'PrimitiveTypes::CMGO_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Organizaciones::OrigenPaciente_Type'
		nil
		(items
		  ( item 'OrigenPaciente_Type'
			'OrigenPaciente'
			nil nil
			(usedItems
			   ( refToItemTranslation 'CMGO_Type' 'PrimitiveTypes::CMGO_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Organizaciones::CoverturaPagoPrestaciones_Type'
		nil
		(items
		  ( item 'CoverturaPagoPrestaciones_Type'
			'CoverturaPagoPrestaciones'
			nil nil
			(usedItems
			   ( refToItemTranslation 'CMGO_Type' 'PrimitiveTypes::CMGO_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Organizaciones::IntermediacionPrestaciones_Type'
		nil
		(items
		  ( item 'IntermediacionPrestaciones_Type'
			'IntermediacionPrestaciones'
			nil nil
			(usedItems
			   ( refToItemTranslation 'CMGO_Type' 'PrimitiveTypes::CMGO_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::RazonSocial_Type'
		nil
		(items
		  ( item 'RazonSocial_Type'
			'RazonSocial'
			nil nil
			(usedItems
			   ( refToItemTranslation 'String_Type' 'PrimitiveTypes::String_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::NombreComercial_Type'
		nil
		(items
		  ( item 'NombreComercial_Type'
			'NombreComercial'
			nil nil
			(usedItems
			   ( refToItemTranslation 'String_Type' 'PrimitiveTypes::String_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::OtrosTiposContacto_Type'
		nil
		(items
		  ( item 'OtrosTiposContacto_Type'
			'OtrosTiposContacto'
			nil nil
			(usedItems
			   ( refToItemTranslation 'String_Type' 'PrimitiveTypes::String_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Tarifas::PrecioTarifaAlquiler_Type'
		nil
		(items
		  ( item 'PrecioTarifaAlquiler_Type'
			'PrecioTarifaAlquiler'
			nil nil
			(usedItems
			   ( refToItemTranslation 'CMGO_Type' 'PrimitiveTypes::CMGO_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Tarifas::TarifaAlquiler_Type'
		nil
		(items
		  ( item 'TarifaAlquiler_Type'
			'TarifaAlquiler'
			nil nil
			(usedItems
			   ( refToItemTranslation 'PrecioTarifaAlquiler_Type' 'Tarifas::PrecioTarifaAlquiler_Type' 'KronoSimple_ApplicationTranslation' ) 
			   ( refToItemTranslation 'CMGO_Type' 'PrimitiveTypes::CMGO_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::PorcentajeIVA_Type'
		nil
		(items
		  ( item 'PorcentajeIVA_Type'
			'PorcentajeIVA'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Number_Type' 'PrimitiveTypes::Number_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::NumeroFactura_Type'
		nil
		(items
		  ( item 'NumeroFactura_Type'
			'NumeroFactura'
			nil nil
			(usedItems
			   ( refToItemTranslation 'String_Type' 'PrimitiveTypes::String_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::SpanishDate_Type'
		nil
		(items
		  ( item 'SpanishDate_Type'
			'FechaEuropea'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'TiposBasicos::FechaCierreHistoria_Type'
		nil
		(items
		  ( item 'FechaCierreHistoria_Type'
			'FechaCierreHistoria'
			nil nil
			(usedItems
			   ( refToItemTranslation 'SpanishDate_Type' 'PrimitiveTypes::SpanishDate_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::ValorPrecioClinica_Type'
		nil
		(items
		  ( item 'ValorPrecioClinica_Type'
			'ValorPrecioClinica'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Number_Type' 'PrimitiveTypes::Number_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::DescripcionConceptoAlquiler_Type'
		nil
		(items
		  ( item 'DescripcionConceptoAlquiler_Type'
			'DescripcionConceptoAlquiler'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Text_Type' 'PrimitiveTypes::Text_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::DescripcionConceptoClinica_Type'
		nil
		(items
		  ( item 'DescripcionConceptoClinica_Type'
			'DescripcionConceptoClinica'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Text_Type' 'PrimitiveTypes::Text_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Organizaciones::OrigenPacienteReferido_Type'
		nil
		(items
		  ( item 'OrigenPacienteReferido_Type'
			'OrigenPacienteReferido'
			nil nil
			(usedItems
			   ( refToItemTranslation 'OrigenPaciente_Type' 'Organizaciones::OrigenPaciente_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Organizaciones::OrigenPacienteParticular_Type'
		nil
		(items
		  ( item 'OrigenPacienteParticular_Type'
			'OrigenPacienteParticular'
			nil nil
			(usedItems
			   ( refToItemTranslation 'OrigenPaciente_Type' 'Organizaciones::OrigenPaciente_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Facturacion::FacturaClinicaPorPaciente_Type'
		nil
		(items
		  ( item 'FacturaClinicaPorPaciente_Type'
			'FacturaClinicaPorPaciente'
			nil nil
			(usedItems
			   ( refToItemTranslation 'FacturacionClinica_Type' 'Facturacion::FacturacionClinica_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::NumeroHistoria_Type'
		nil
		(items
		  ( item 'NumeroHistoria_Type'
			'NumeroHistoria'
			nil nil
			(usedItems
			   ( refToItemTranslation 'String_Type' 'PrimitiveTypes::String_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::NombreEntradaCalendario_Type'
		nil
		(items
		  ( item 'NombreEntradaCalendario_Type'
			'NombreEntradaCalendario'
			nil nil
			(usedItems
			   ( refToItemTranslation 'String_Type' 'PrimitiveTypes::String_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::HoraComienzo_Type'
		nil
		(items
		  ( item 'HoraComienzo_Type'
			'HoraComienzo'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Time_Type' 'PrimitiveTypes::Time_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::HoraFin_Type'
		nil
		(items
		  ( item 'HoraFin_Type'
			'HoraFin'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Time_Type' 'PrimitiveTypes::Time_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'ActosClinicos::TotalizadoParaFactura_Type'
		nil
		(items
		  ( item 'TotalizadoParaFactura_Type'
			'TotalizadoParaFactura'
			nil nil
			(usedItems
			   ( refToItemTranslation 'ActoClinico_Type' 'Nucleo::ActoClinico_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Facturacion::FacturaClinicaAseguradoDetallada_Type'
		nil
		(items
		  ( item 'FacturaClinicaAseguradoDetallada_Type'
			'FacturaClinicaAseguradoDetallada'
			nil nil
			(usedItems
			   ( refToItemTranslation 'FacturacionClinica_Type' 'Facturacion::FacturacionClinica_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Facturacion::FacturaClinicaParticular_Type'
		nil
		(items
		  ( item 'FacturaClinicaParticular_Type'
			'FacturaClinicaParticular'
			nil nil
			(usedItems
			   ( refToItemTranslation 'FacturacionClinica_Type' 'Facturacion::FacturacionClinica_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Calendario::Ejercicio_Type'
		nil
		(items
		  ( item 'periodos_Relationship'
			'PPPerrrriodddoosss'
			nil nil
			nil
		   )

		  ( item 'periodos_Relationship_NameOne'
			'PPeeRRiiOOddOOss'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Customers::Sole Trader_Type'
		nil
		(items
		  ( item 'Date Started Self Employment_Attribute'
			'Date Started Self Employment'
			nil nil
			nil
		   )

		  ( item 'DistrictNumber_Attribute'
			'District Number'
			nil nil
			nil
		   )

		  ( item 'ProcessingOffice_Attribute'
			'Processing Office'
			nil nil
			nil
		   )

		  ( item 'DateOfBirth_Attribute'
			'Date Of Birth'
			nil nil
			nil
		   )

		  ( item 'NINOStatus_Attribute'
			'NINO Status'
			nil nil
			nil
		   )

		  ( item 'IsDeceased_Attribute'
			'Is Deceased'
			nil nil
			nil
		   )

		  ( item 'DaytimeTelephoneNumber_Attribute'
			'Daytime Telephone Number'
			nil nil
			nil
		   )

		  ( item 'Second Name_Attribute'
			'Second Name'
			nil nil
			nil
		   )

		  ( item 'FirstName_Attribute'
			'First Name'
			nil nil
			nil
		   )

		  ( item 'sole Trader Agent_Relationship'
			'agent'
			nil nil
			nil
		   )

		  ( item 'sole Trader Agent_Relationship_NameOne'
			'sole Trader Agent'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Non Identifiable::Enumerations::TitleKind_Type'
		nil
		(items
		  ( item 'TitleKind_Type'
			'Title Kind'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Customers::CID Sole Trader Details_Type'
		nil
		(items
		  ( item 'CID Sole Trader Details_Type'
			'CID Sole Trader Details'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Customers::CIS Sole Trader Details_Type'
		nil
		(items
		  ( item 'Type Of Work_Attribute'
			'Type Of Work'
			nil nil
			nil
		   )

		  ( item 'Send Contractor Output To Agent_Attribute'
			'Send Contractor Output To Agent'
			nil nil
			nil
		   )

		  ( item 'Use Existing Agent_Attribute'
			'Use Existing Agent'
			nil nil
			nil
		   )

		  ( item 'VAT Registration Number_Attribute'
			'VAT Registration Number'
			nil nil
			nil
		   )

		  ( item 'Date Started Or Plan To Start Construction_Attribute'
			'Date Started Or Plan To Start Construction'
			nil nil
			nil
		   )

		  ( item 'Has Been Paid By Contractor_Attribute'
			'Has Been Paid By Contractor'
			nil nil
			nil
		   )

		  ( item 'Email Address_Attribute'
			'Email Address'
			nil nil
			nil
		   )

		  ( item 'FaxNumber_Attribute'
			'FaxNumber'
			nil nil
			nil
		   )

		  ( item 'MobilePhoneNumber_Attribute'
			'Mobile Phone Number'
			nil nil
			nil
		   )

		  ( item 'Evening Telephone Number_Attribute'
			'Evening Telephone Number'
			nil nil
			nil
		   )

		  ( item 'CISTradingName_Attribute'
			'CIS Trading Name'
			nil nil
			nil
		   )

		  ( item 'ID Documents Proving ID_Attribute'
			'ID Documents Proving ID'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Agents::Sole Trader Agent_Type'
		nil
		(items
		  ( item 'soleTraders_Relationship'
			'sole Traders'
			nil nil
			nil
		   )

		  ( item 'soleTraders_Relationship_NameOne'
			'soleTraders'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Customers::Partnereable_Type'
		nil
		(items
		  ( item 'partnerInPartnerships_Relationship'
			'partner In Partnerships'
			nil nil
			nil
		   )

		  ( item 'partnerInPartnerships_Relationship_NameOne'
			'partnerInPartnerships'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Customers::Partner_Type'
		nil
		(items
		  ( item 'UTRsPair_Attribute'
			'UTRs Pair'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Registrations::Partnership Partner Subcontractor Registration_Type'
		nil
		(items
		  ( item 'UTRs Pair_Attribute'
			'UTRs Pair'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Customers::Address Sole Trader Details_Type'
		nil
		(items
		  ( item 'CIS Correspondence Address_Attribute'
			'CIS Corr. Address'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Registrations::Verification_Type'
		nil
		(items
		  ( item 'Verification Date_Attribute'
			'Verification Date'
			nil nil
			nil
		   )

		  ( item 'Verification Number_Attribute'
			'Verification Number'
			nil nil
			nil
		   )

		  ( item 'VerificationNumber_Attribute'
			'Verification Number'
			nil nil
			nil
		   )

		  ( item 'VerificationDate_Attribute'
			'Verification Date'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Registrations::Individual Verification_Type'
		nil
		(items
		  ( item 'VerificationNumberAndSuffix_Attribute'
			'Verification Number And Suffix'
			nil nil
			nil
		   )

		  ( item 'VerificationSuffix_Attribute'
			'Verification Suffix'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Returns::Multiple Return_Type'
		nil
		(items
		  ( item 'isResolved_Attribute'
			'IS Resolved'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Returns::Processed Return_Type'
		nil
		(items
		  ( item 'IsEffective_Attribute'
			'Is Effective'
			nil nil
			nil
		   )

		  ( item 'Display Name_Attribute'
			'Display Name'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Returns::Abstract Return_Type'
		nil
		(items
		  ( item 'TraceToAction_Attribute'
			'Trace To Action'
			nil nil
			nil
		   )

		  ( item 'TraceFromAction_Attribute'
			'Trace From Action'
			nil nil
			nil
		   )

		  ( item 'AmendedDate_Attribute'
			'Amended Date'
			nil nil
			nil
		   )

		  ( item 'LoggedDate_Attribute'
			'Logged Date'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Returns::Unmatched Return_Type'
		nil
		(items
		  ( item 'IS Resolved_Attribute'
			'IS Resolved'
			nil nil
			nil
		   )

		  ( item 'AORef_Attribute'
			'AO Ref'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Non Identifiable::Address_Type'
		nil
		(items
		  ( item 'PostCode_Attribute'
			'Post Code'
			nil nil
			nil
		   )

		  ( item 'DisplayName_Attribute'
			'Display Name'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'CISR::CISR CORE::Contacts::Contact_Type'
		nil
		(items
		  ( item 'Contact_Type'
			'Contact'
			nil nil
			(usedItems
			   ( refToItemTranslation 'CMGO_Type' 'PrimitiveTypes::CMGO_Type' 'KronoSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	 )
   )!

kronoSimpleUITranslationStore

	"(TranslationModelBase newFromPersistenceAsCode: KRSimpleTranslationHolder kronoSimpleUITranslationStore) browsePath"

	self ojoTranslation.

	^   #( application 'KronoSimpleUI_ApplicationTranslation'
	KRSimpleTranslationHolder kronoSimpleUITranslationStore
	(superApplicationTranslation
	  ( refToApplicationTranslationStore 'KronoSimple_ApplicationTranslation' kronoSimpleTranslationStore KRSimpleTranslationHolder )

	 )
	nil
   )! !

KRONO_Simple_Translation initializeAfterLoad!
KRSimpleTranslationHolder initializeAfterLoad!

KRONO_Simple_Translation loaded!
