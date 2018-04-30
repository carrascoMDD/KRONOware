'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:38:03 pm'!



((KRONO_Reports createSubApplication: #KRONO_Reports_Custom in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

KRONO_Reports_Custom becomeDefault!

KRReportWriter subclass: #KRFacturaAlquilerReportWriter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reports_Custom becomeDefault!

KRReportSection subclass: #KRFacturaAlquilerClienteFooter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reports_Custom becomeDefault!

KRReportSection subclass: #KRFacturaAlquilerClienteHeader
	instanceVariableNames: 'nombreCliente nifCifClienteLabel nifCifCliente direccionCliente cifCliente cifClienteLabel codigoPostalPoblacionProvinciaCliente '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reports_Custom becomeDefault!

KRReportSection subclass: #KRFacturaAlquilerClinicaFooter
	instanceVariableNames: 'nombreClinica inscripcionClinica '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reports_Custom becomeDefault!

KRReportSection subclass: #KRFacturaAlquilerClinicaHeader
	instanceVariableNames: 'nombreClinica cifClinicaLabel direccionClinica cifClinica telefonoDosClinica telefonoUnoClinica codigoPostalPoblacionProvinciaClinica faxClinica '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reports_Custom becomeDefault!

KRReportSection subclass: #KRFacturaAlquilerFacturaFooter
	instanceVariableNames: 'totalAlquiler totalIVA totalIRPF percentIVA percentIRPF totalFactura totalIVALabel totalIRPFLabel totalFacturaLabel totalAlquilerLabel '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reports_Custom becomeDefault!

KRReportSection subclass: #KRFacturaAlquilerFacturaHeader
	instanceVariableNames: 'numeroFactura fechaFactura fechaFacturaLabel numeroFacturaLabel '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reports_Custom becomeDefault!

KRReportSection subclass: #KRFacturaAlquilerFooter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reports_Custom becomeDefault!

KRReportSection subclass: #KRFacturaAlquilerHeader
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reports_Custom becomeDefault!

KRReportSection subclass: #KRFacturaAlquilerLineaFactura
	instanceVariableNames: 'numeroLineaFactura precioUnitarioLineaFactura numeroUnidadesLineaFactura conceptoLineaFactura nombreUnidadLineaFactura importeLineaFactura '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reports_Custom becomeDefault!

KRReportSection subclass: #KRFacturaAlquilerTitle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reports_Custom becomeDefault!

SubApplication subclass: #KRONO_Reports_Custom
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reports_Custom becomeDefault!

!KRFacturaAlquilerClienteFooter class publicMethodsFor: 'interface specs'!

facturaAlquilerClienteFooterReportComponentSpec
	"UIPainter new openOnClass: self andSelector: #facturaAlquilerClienteFooterReportComponentSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'facturalAlquilerClienteFooter' 
			#min: #(#Point 1  2 ) 
			#bounds: #(#Rectangle 0 0 1 2 ) 
			#isEventDriven: true ) 
		#component: 
		#(#SpecCollection 
			#collection: #() ) )! !

!KRFacturaAlquilerClienteHeader class publicMethodsFor: 'interface specs'!

facturaAlquilerClienteHeaderReportComponentSpec
	"UIPainter new openOnClass: self andSelector: #facturaAlquilerClienteHeaderReportComponentSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'facturaAlquilerClienteHeader' 
			#min: #(#Point 538 153 ) 
			#bounds: #(#Rectangle 380 574 918 727 ) 
			#isEventDriven: true ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 20 0 20 0 -20 1 40 0 ) 
					#name: #nombreClienteID 
					#flags: 0 
					#isOpaque: true 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #nombreCliente 
					#tabable: false 
					#alignment: #center 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 0 0.416666 110 0 -20 1 130 0 ) 
					#name: #cifClienteID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #cifCliente 
					#tabable: false 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame -60 0.416666 110 0 -10 0.416666 130 0 ) 
					#name: #cifClienteLabelID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #cifClienteLabel 
					#tabable: false 
					#alignment: #right 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 20 0 50 0 -20 1 70 0 ) 
					#name: #direccionClienteID 
					#flags: 0 
					#isOpaque: true 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #direccionCliente 
					#tabable: false 
					#alignment: #center 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 20 0 80 0 -20 1 100 0 ) 
					#name: #codigoPostalPoblacionProvinciaClienteID 
					#flags: 0 
					#isOpaque: true 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #codigoPostalPoblacionProvinciaCliente 
					#tabable: false 
					#alignment: #center 
					#style: #pixelDefault 
					#isReadOnly: true ) ) ) )! !

!KRFacturaAlquilerClienteHeader publicMethodsFor: 'aspects'!

cifCliente
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^cifCliente isNil
		ifTrue:
			[cifCliente := String new asValue]
		ifFalse:
			[cifCliente]!

cifClienteLabel
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^cifClienteLabel isNil
		ifTrue:
			[cifClienteLabel := String new asValue]
		ifFalse:
			[cifClienteLabel]!

codigoPostalPoblacionProvinciaCliente
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^codigoPostalPoblacionProvinciaCliente isNil
		ifTrue:
			[codigoPostalPoblacionProvinciaCliente := String new asValue]
		ifFalse:
			[codigoPostalPoblacionProvinciaCliente]!

direccionCliente
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^direccionCliente isNil
		ifTrue:
			[direccionCliente := String new asValue]
		ifFalse:
			[direccionCliente]!

nombreCliente
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^nombreCliente isNil
		ifTrue:
			[nombreCliente := String new asValue]
		ifFalse:
			[nombreCliente]! !

!KRFacturaAlquilerClienteHeader publicMethodsFor: 'info creation'!

addDataPacket: theDataPacket 

	| aEsPersonaFisica aFactura |
	super addDataPacket: theDataPacket.

	aFactura := theDataPacket metaInfo getObject: theDataPacket featureNamedValue: 'factura'.

	aEsPersonaFisica := ((aFactura metaInfo getObject: aFactura expression: 'noclone clienteFactura') first metaInfo == 
		(aFactura metaInfo model typeNamed:  'Facturacion::ClienteFacturaAlquilerPersonaFisica')).

	self nombreCliente  value: (
		aEsPersonaFisica
			ifTrue: [ 
				(aFactura metaInfo getObject: aFactura expression: 'noclone clienteFactura nombre') first,
				' ',
				(aFactura metaInfo getObject: aFactura expression: 'noclone clienteFactura apellidos') first
			]
			ifFalse: [  (aFactura metaInfo getObject: aFactura expression: 'noclone clienteFactura razonSocial') first]
	).

	self cifCliente  value: (
		aEsPersonaFisica
			ifTrue: [ (aFactura metaInfo getObject: aFactura expression: 'noclone clienteFactura dninif') first]
			ifFalse: [ 
				(aFactura metaInfo getObject: aFactura expression: 'noclone clienteFactura cif') first
			]
	).

	self direccionCliente  value: (
		(aFactura metaInfo getObject: aFactura expression: 
			'noclone clienteFactura direccionFacturaAlquiler calle') first , 
		' ',
		(aFactura metaInfo getObject: aFactura expression: 
			'noclone clienteFactura direccionFacturaAlquiler numeroEnCalle') first ,
		'-',
		(aFactura metaInfo getObject: aFactura expression: 
			'noclone clienteFactura direccionFacturaAlquiler apartamento') first).

	self cifClienteLabel value: (
		aEsPersonaFisica
			ifTrue: [ 'N.I.F.' copy]
			ifFalse: [ 'C.I.F.' copy]
	).
	self codigoPostalPoblacionProvinciaCliente value: 
		(aFactura metaInfo getObject: aFactura expression: 
			'noclone clienteFactura direccionFacturaAlquiler distritoPostal') first,
		' ',
		(aFactura metaInfo getObject: aFactura expression: 
			'noclone clienteFactura direccionFacturaAlquiler poblacion') first,
		' ',
		(aFactura metaInfo getObject: aFactura expression: 
			'noclone clienteFactura direccionFacturaAlquiler provincia') first.! !

!KRFacturaAlquilerClinicaFooter class publicMethodsFor: 'interface specs'!

facturaAlquilerClinicaFooterReportComponentSpec
	"UIPainter new openOnClass: self andSelector: #facturaAlquilerClinicaFooterReportComponentSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'facturaAlquilerClinicaFooter' 
			#min: #(#Point 140 60 ) 
			#bounds: #(#Rectangle 0 0 500 60 ) 
			#isEventDriven: true ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 20 0 20 0 -20 1 50 0 ) 
					#name: #inscripcionClinicaID 
					#flags: 0 
					#isOpaque: true 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #inscripcionClinica 
					#tabable: false 
					#style: #pixelDefault 
					#isReadOnly: true ) ) ) )! !

!KRFacturaAlquilerClinicaFooter publicMethodsFor: 'aspects'!

inscripcionClinica
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^inscripcionClinica isNil
		ifTrue:
			[inscripcionClinica := String new asValue]
		ifFalse:
			[inscripcionClinica]! !

!KRFacturaAlquilerClinicaFooter publicMethodsFor: 'info creation'!

addDataPacket: theDataPacket 

	| aFactura |
	super addDataPacket: theDataPacket.

	aFactura := theDataPacket metaInfo getObject: theDataPacket featureNamedValue: 'factura'.

	self inscripcionClinica value: 
		( 'Clinica ', 
		(aFactura metaInfo getObject: aFactura expression:  
			'noclone periodoFacturacion ejercicioFacturacion facturacion archivo clinica razonSocial') first, 
		' inscrita en Valencia con C.I.F. ', 
		(aFactura metaInfo getObject: aFactura expression:  
			'noclone periodoFacturacion ejercicioFacturacion facturacion archivo clinica cif') first).
	^self! !

!KRFacturaAlquilerClinicaHeader class publicMethodsFor: 'interface specs'!

facturaAlquilerClinicaHeaderReportComponentSpec
	"UIPainter new openOnClass: self andSelector: #facturaAlquilerClinicaHeaderReportComponentSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'facturaAlquilerClinicaHeader' 
			#min: #(#Point 580 180 ) 
			#bounds: #(#Rectangle 378 340 958 520 ) 
			#isEventDriven: true ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#ArbitraryComponentSpec 
					#layout: #(#Rectangle 20 20 265 128 ) 
					#name: #imageViewID 
					#flags: 0 
					#component: #imageView ) 
				#(#InputFieldSpec 
					#layout: #(#Rectangle 20 140 145 160 ) 
					#name: #nombreClinicaID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #nombreClinica 
					#tabable: false 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#Rectangle 185 140 275 160 ) 
					#name: #cifClinicaID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #cifClinica 
					#tabable: false 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#Rectangle 150 140 180 160 ) 
					#name: #cifClinicaLabelID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #cifClinicaLabel 
					#tabable: false 
					#alignment: #right 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 286 0 20 0 -19 1 40 0 ) 
					#name: #direccionClinicaID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #direccionClinica 
					#tabable: false 
					#alignment: #right 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 285 0 140 0 -20 1 160 0 ) 
					#name: #codigoPostalPoblacionProvinciaClinicaID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #codigoPostalPoblacionProvinciaClinica 
					#tabable: false 
					#alignment: #right 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 285 0 50 0 -20 1 70 0 ) 
					#name: #telefonoUnoClinicaID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #telefonoUnoClinica 
					#tabable: false 
					#alignment: #right 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 285 0 80 0 -20 1 100 0 ) 
					#name: #telefonoDosClinicaID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #telefonoDosClinica 
					#tabable: false 
					#alignment: #right 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 285 0 110 0 -20 1 130 0 ) 
					#name: #faxClinicaID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #faxClinica 
					#tabable: false 
					#alignment: #right 
					#style: #pixelDefault 
					#isReadOnly: true ) ) ) )! !

!KRFacturaAlquilerClinicaHeader publicMethodsFor: 'adding'!

addDataPacket: theDataPacket 
| aFactura |

	super addDataPacket: theDataPacket.

	aFactura := theDataPacket metaInfo getObject: theDataPacket featureNamedValue: 'factura'.

	self nombreClinica value: (aFactura metaInfo getObject: aFactura expression: 
		'noclone periodoFacturacion ejercicioFacturacion facturacion archivo clinica razonSocial') first.
	self direccionClinica value: 
		(aFactura metaInfo getObject: aFactura expression: 
			'noclone periodoFacturacion ejercicioFacturacion facturacion archivo clinica direccionFacturaAlquiler calle') first , 
		' ',
		(aFactura metaInfo getObject: aFactura expression: 
			'noclone periodoFacturacion ejercicioFacturacion facturacion archivo clinica direccionFacturaAlquiler numeroEnCalle') first ,
		'-',
		(aFactura metaInfo getObject: aFactura expression: 
			'noclone periodoFacturacion ejercicioFacturacion facturacion archivo clinica direccionFacturaAlquiler apartamento') first.
	self cifClinica value: (aFactura metaInfo getObject: aFactura expression: 
		'noclone periodoFacturacion ejercicioFacturacion facturacion archivo clinica cif') first.
	self cifClinicaLabel value: 'C.I.F.'.
	self codigoPostalPoblacionProvinciaClinica value: 
		(aFactura metaInfo getObject: aFactura expression: 
			'noclone periodoFacturacion ejercicioFacturacion facturacion archivo clinica direccionFacturaAlquiler distritoPostal') first,
		' ',
		(aFactura metaInfo getObject: aFactura expression: 
			'noclone periodoFacturacion ejercicioFacturacion facturacion archivo clinica direccionFacturaAlquiler poblacion') first asUppercase,
		' (',
		(aFactura metaInfo getObject: aFactura expression: 
			'noclone periodoFacturacion ejercicioFacturacion facturacion archivo clinica direccionFacturaAlquiler provincia') first,
		')'.

	self telefonoUnoClinica value: 'Tel. ', (aFactura metaInfo getObject: aFactura expression: 
		'noclone periodoFacturacion ejercicioFacturacion facturacion archivo clinica numeroTelecomunicacionTelefonoOficialUno numero') first.
	self telefonoDosClinica value: (aFactura metaInfo getObject: aFactura expression: 
		'noclone periodoFacturacion ejercicioFacturacion facturacion archivo clinica numeroTelecomunicacionTelefonoOficialDos numero') first.
	self faxClinica value: 'Fax. ', (aFactura metaInfo getObject: aFactura expression: 
		'noclone periodoFacturacion ejercicioFacturacion facturacion archivo clinica numeroTelecomunicacionFaxOficial numero') first.! !

!KRFacturaAlquilerClinicaHeader publicMethodsFor: 'aspects'!

cifClinica
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^cifClinica isNil
		ifTrue:
			[cifClinica := 'aaa' asValue]
		ifFalse:
			[cifClinica]!

cifClinicaLabel
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^cifClinicaLabel isNil
		ifTrue:
			[cifClinicaLabel := String new asValue]
		ifFalse:
			[cifClinicaLabel]!

codigoPostalPoblacionProvinciaClinica
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^codigoPostalPoblacionProvinciaClinica isNil
		ifTrue:
			[codigoPostalPoblacionProvinciaClinica := String new asValue]
		ifFalse:
			[codigoPostalPoblacionProvinciaClinica]!

direccionClinica
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^direccionClinica isNil
		ifTrue:
			[direccionClinica := 'aaaa' asValue]
		ifFalse:
			[direccionClinica]!

faxClinica
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^faxClinica isNil
		ifTrue:
			[faxClinica := String new asValue]
		ifFalse:
			[faxClinica]!

imageView
	^self logoFacturaAlquilerClinicaReportTitle!

logoFacturaAlquilerClinicaReportTitle
	| anImage |
	anImage := GIFReadWriter imageFromFileNamed: self defaultFacturaAlquilerReportClinicaTitleImageFileName.
	^anImage!

nombreClinica
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^nombreClinica isNil
		ifTrue:
			[nombreClinica := String new asValue]
		ifFalse:
			[nombreClinica]!

telefonoDosClinica
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^telefonoDosClinica isNil
		ifTrue:
			[telefonoDosClinica := String new asValue]
		ifFalse:
			[telefonoDosClinica]!

telefonoUnoClinica
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^telefonoUnoClinica isNil
		ifTrue:
			[telefonoUnoClinica := String new asValue]
		ifFalse:
			[telefonoUnoClinica]! !

!KRFacturaAlquilerClinicaHeader publicMethodsFor: 'image access'!

defaultFacturaAlquilerReportClinicaTitleImageFileName
	^'C:\Works\VW\KronoWare\FerSalud\imagenes\FerSaludLogoFactura245_108.gif' copy! !

!KRFacturaAlquilerFacturaFooter class publicMethodsFor: 'interface specs'!

facturaAlquilerFacturaFooterReportComponentSpec
	"UIPainter new openOnClass: self andSelector: #facturaAlquilerFacturaFooterReportComponentSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'facturaAlquilerFacturaFooter' 
			#min: #(#Point 376 189 ) 
			#bounds: #(#Rectangle 447 380 1018 569 ) 
			#isEventDriven: true ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 10 0.5 20 0 -20 0.85 40 0 ) 
					#name: #totalAlquilerID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #totalAlquiler 
					#tabable: false 
					#alignment: #right 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 20 0.15 20 0 -10 0.5 40 0 ) 
					#name: #totalAlquilerLabelID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #totalAlquilerLabel 
					#tabable: false 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 10 0.35 80 0 -10 0.5 100 0 ) 
					#name: #percentIRPFID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #percentIRPF 
					#tabable: false 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 10 0.5 80 0 -20 0.85 100 0 ) 
					#name: #totalIRPFID 
					#flags: 0 
					#isOpaque: true 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #totalIRPF 
					#tabable: false 
					#alignment: #right 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 10 0.35 50 0 -10 0.5 70 0 ) 
					#name: #percentIVAID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #percentIVA 
					#tabable: false 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 10 0.5 50 0 -20 0.85 70 0 ) 
					#name: #totalIVAID 
					#flags: 0 
					#isOpaque: true 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #totalIVA 
					#tabable: false 
					#alignment: #right 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 20 0.1 140 0 -10 0.5 170 0 ) 
					#name: #totalFacturaLabelID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #totalFacturaLabel 
					#tabable: false 
					#style: #pixelLarge 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 10 0.5 140 0 -20 0.9 170 0 ) 
					#name: #totalFacturaID 
					#flags: 0 
					#isOpaque: true 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #totalFactura 
					#tabable: false 
					#alignment: #right 
					#style: #pixelLarge 
					#isReadOnly: true ) 
				#(#DividerSpec 
					#layout: #(#LayoutFrame 20 0.1 172 0 -20 0.9 174 0 ) ) 
				#(#LabelSpec 
					#layout: #(#LayoutFrame 20 0.15 30 0 -20 0.85 42 0 ) 
					#name: #totalAlquilerDotsLabelID 
					#label: '..................................................................................................................................................................................................................................................................................................................................................................................................................' 
					#style: #pixelSmall ) 
				#(#LabelSpec 
					#layout: #(#LayoutFrame 20 0.15 90 0 -20 0.85 102 0 ) 
					#name: #totalAlquilerDotsLabelID 
					#label: '..................................................................................................................................................................................................................................................................................................................................................................................................................' 
					#style: #pixelSmall ) 
				#(#LabelSpec 
					#layout: #(#LayoutFrame 20 0.15 60 0 -20 0.85 72 0 ) 
					#name: #totalAlquilerDotsLabelID 
					#label: '..................................................................................................................................................................................................................................................................................................................................................................................................................' 
					#style: #pixelSmall ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 20 0.15 80 0 0 0.35 100 0 ) 
					#name: #totalIRPFLabelID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #totalIRPFLabel 
					#tabable: false 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 20 0.15 50 0 0 0.35 70 0 ) 
					#name: #totalIVALabelID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #totalIVALabel 
					#tabable: false 
					#style: #pixelDefault 
					#isReadOnly: true ) ) ) )!

facturaAlquilerWideFacturaFooterReportComponentSpec
	"UIPainter new openOnClass: self andSelector: #facturaAlquilerWideFacturaFooterReportComponentSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'facturaAlquilerFacturaFooter' 
			#min: #(#Point 376 189 ) 
			#bounds: #(#Rectangle 308 455 879 644 ) 
			#isEventDriven: true ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 10 0.5 20 0 -20 1 40 0 ) 
					#name: #totalAlquilerID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #totalAlquiler 
					#tabable: false 
					#alignment: #right 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 20 0 20 0 -10 0.5 40 0 ) 
					#name: #totalAlquilerLabelID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #totalAlquilerLabel 
					#tabable: false 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 10 0.25 80 0 -10 0.5 100 0 ) 
					#name: #percentIRPFID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #percentIRPF 
					#tabable: false 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 10 0.5 80 0 -20 1 100 0 ) 
					#name: #totalIRPFID 
					#flags: 0 
					#isOpaque: true 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #totalIRPF 
					#tabable: false 
					#alignment: #right 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 10 0.25 50 0 -10 0.5 70 0 ) 
					#name: #percentIVAID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #percentIVA 
					#tabable: false 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 10 0.5 50 0 -20 1 70 0 ) 
					#name: #totalIVAID 
					#flags: 0 
					#isOpaque: true 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #totalIVA 
					#tabable: false 
					#alignment: #right 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 20 0 140 0 -10 0.5 170 0 ) 
					#name: #totalFacturaLabelID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #totalFacturaLabel 
					#tabable: false 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 10 0.5 140 0 -20 1 170 0 ) 
					#name: #totalFacturaID 
					#flags: 0 
					#isOpaque: true 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #totalFactura 
					#tabable: false 
					#alignment: #right 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#DividerSpec 
					#layout: #(#LayoutFrame 20 0 172 0 -20 1 174 0 ) ) 
				#(#LabelSpec 
					#layout: #(#LayoutFrame 20 0 30 0 -20 1 42 0 ) 
					#name: #totalAlquilerDotsLabelID 
					#label: '..................................................................................................................................................................................................................................................................................................................................................................................................................' 
					#style: #pixelSmall ) 
				#(#LabelSpec 
					#layout: #(#LayoutFrame 20 0 90 0 -20 1 102 0 ) 
					#name: #totalAlquilerDotsLabelID 
					#label: '..................................................................................................................................................................................................................................................................................................................................................................................................................' 
					#style: #pixelSmall ) 
				#(#LabelSpec 
					#layout: #(#LayoutFrame 20 0 60 0 -20 1 72 0 ) 
					#name: #totalAlquilerDotsLabelID 
					#label: '..................................................................................................................................................................................................................................................................................................................................................................................................................' 
					#style: #pixelSmall ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 20 0 80 0 0 0.25 100 0 ) 
					#name: #totalIRPFLabelID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #totalIRPFLabel 
					#tabable: false 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 20 0 50 0 0 0.25 70 0 ) 
					#name: #totalIVALabelID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #totalIVALabel 
					#tabable: false 
					#style: #pixelDefault 
					#isReadOnly: true ) ) ) )! !

!KRFacturaAlquilerFacturaFooter publicMethodsFor: 'adding'!

addDataPacket: theDataPacket 

	| aFactura |
	super addDataPacket: theDataPacket.

	aFactura := theDataPacket metaInfo getObject: theDataPacket featureNamedValue: 'factura'.

	self totalAlquilerLabel value: 'Alquiler' copy.
	self totalAlquiler value: (aFactura metaInfo getObject: aFactura expression:  'noclone totalFactura') first.
	self totalIVALabel value: 'I.V.A.'.
	self percentIVA value: (aFactura metaInfo getObject: aFactura expression:  'noclone porcentajeIVA') first printString , ' %'.
	self totalIVA value: (aFactura metaInfo getObject: aFactura expression:  'noclone iva') first.
	self totalIRPFLabel value: 'Retencion I.R.P.F.'.
	self percentIRPF value: (aFactura metaInfo getObject: aFactura expression:  'noclone porcentajeIRPF') first printString, ' %'.
	self totalIRPF value: (aFactura metaInfo getObject: aFactura expression: 'noclone irpf') first.
	self totalFacturaLabel value: 'Total a Pagar'.
	self totalFactura value: (aFactura metaInfo getObject: aFactura expression:  'noclone totalFacturaConIVAeIRPF') first.! !

!KRFacturaAlquilerFacturaFooter publicMethodsFor: 'aspects'!

percentIRPF
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^percentIRPF isNil
		ifTrue:
			[percentIRPF := String new asValue]
		ifFalse:
			[percentIRPF]!

percentIVA
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^percentIVA isNil
		ifTrue:
			[percentIVA := String new asValue]
		ifFalse:
			[percentIVA]!

totalAlquiler
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^totalAlquiler isNil
		ifTrue:
			[totalAlquiler := String new asValue]
		ifFalse:
			[totalAlquiler]!

totalAlquilerLabel
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^totalAlquilerLabel isNil
		ifTrue:
			[totalAlquilerLabel := String new asValue]
		ifFalse:
			[totalAlquilerLabel]!

totalFactura
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^totalFactura isNil
		ifTrue:
			[totalFactura := String new asValue]
		ifFalse:
			[totalFactura]!

totalFacturaLabel
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^totalFacturaLabel isNil
		ifTrue:
			[totalFacturaLabel := String new asValue]
		ifFalse:
			[totalFacturaLabel]!

totalIRPF
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^totalIRPF isNil
		ifTrue:
			[totalIRPF := String new asValue]
		ifFalse:
			[totalIRPF]!

totalIRPFLabel
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^totalIRPFLabel isNil
		ifTrue:
			[totalIRPFLabel := String new asValue]
		ifFalse:
			[totalIRPFLabel]!

totalIVA
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^totalIVA isNil
		ifTrue:
			[totalIVA := String new asValue]
		ifFalse:
			[totalIVA]!

totalIVALabel
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^totalIVALabel isNil
		ifTrue:
			[totalIVALabel := String new asValue]
		ifFalse:
			[totalIVALabel]! !

!KRFacturaAlquilerFacturaHeader class publicMethodsFor: 'interface specs'!

facturaAlquilerFacturaHeaderReportComponentSpec
	"UIPainter new openOnClass: self andSelector: #facturaAlquilerFacturaHeaderReportComponentSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'facturaAlquilerFacturaHeader' 
			#min: #(#Point 346 90 ) 
			#bounds: #(#Rectangle 532 493 1047 583 ) 
			#isEventDriven: true ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 10 0.2 20 0 -20 1 40 0 ) 
					#name: #numeroFacturaID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #numeroFactura 
					#tabable: false 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 20 0 20 0 0 0.2 40 0 ) 
					#name: #numeroFacturaLabelID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #numeroFacturaLabel 
					#tabable: false 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 10 0.2 50 0 -20 1 70 0 ) 
					#name: #fechaFacturaID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #fechaFactura 
					#tabable: false 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 20 0 50 0 0 0.2 70 0 ) 
					#name: #fechaFacturaLabelID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #fechaFacturaLabel 
					#tabable: false 
					#style: #pixelDefault 
					#isReadOnly: true ) ) ) )! !

!KRFacturaAlquilerFacturaHeader publicMethodsFor: 'adding'!

addDataPacket: theDataPacket 

	| aFactura |
	super addDataPacket: theDataPacket.

	aFactura := theDataPacket metaInfo getObject: theDataPacket featureNamedValue: 'factura'.

	self fechaFacturaLabel value: 'Fecha'.
	self fechaFactura value: (aFactura metaInfo getObject: aFactura expression: 
		'noclone fechaFactura') first.
	self numeroFacturaLabel value: 'Numero.'.
	self numeroFactura value: (aFactura metaInfo getObject: aFactura expression: 
		'noclone numeroFactura') first.! !

!KRFacturaAlquilerFacturaHeader publicMethodsFor: 'aspects'!

fechaFactura
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^fechaFactura isNil
		ifTrue:
			[fechaFactura := String new asValue]
		ifFalse:
			[fechaFactura]!

fechaFacturaLabel
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^fechaFacturaLabel isNil
		ifTrue:
			[fechaFacturaLabel := String new asValue]
		ifFalse:
			[fechaFacturaLabel]!

numeroFactura
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^numeroFactura isNil
		ifTrue:
			[numeroFactura := String new asValue]
		ifFalse:
			[numeroFactura]!

numeroFacturaLabel
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^numeroFacturaLabel isNil
		ifTrue:
			[numeroFacturaLabel := String new asValue]
		ifFalse:
			[numeroFacturaLabel]! !

!KRFacturaAlquilerFooter class publicMethodsFor: 'interface specs'!

facturaAlquilerFooterReportComponentSpec
	"UIPainter new openOnClass: self andSelector: #facturaAlquilerFooterReportComponentSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'facturaAlquilerFooter' 
			#min: #(#Point 1  2 ) 
			#bounds: #(#Rectangle 0 0 1 2 ) 
			#isEventDriven: true ) 
		#component: 
		#(#SpecCollection 
			#collection: #() ) )! !

!KRFacturaAlquilerHeader class publicMethodsFor: 'interface specs'!

facturaAlquilerHeaderReportComponentSpec
	"UIPainter new openOnClass: self andSelector: #facturaAlquilerHeaderReportComponentSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'facturaAlquilerHeader' 
			#min: #(#Point 1 2 ) 
			#bounds: #(#Rectangle 0 0 1 2 ) 
			#isEventDriven: true ) 
		#component: 
		#(#SpecCollection 
			#collection: #() ) )!

KRONOfacturaAlquilerHeaderReportComponentSpec
	"UIPainter new openOnClass: self andSelector: #facturaAlquilerHeaderReportComponentSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'facturaAlquilerHeader' 
			#min: #(#Point 1 2 ) 
			#bounds: #(#Rectangle 0 0 1 2 ) 
			#isEventDriven: true ) 
		#component: 
		#(#SpecCollection 
			#collection: #() ) )!

xKRONOfacturaAlquilerHeaderReportComponentSpec
	"UIPainter new openOnClass: self andSelector: #xKRONOfacturaAlquilerHeaderReportComponentSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'facturaAlquilerHeader' 
			#min: #(#Point 500 50 ) 
			#bounds: #(#Rectangle 0 0 500 50 ) 
			#isEventDriven: true ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#ArbitraryComponentSpec 
					#layout: #(#LayoutFrame 0 0 0 0 0 1 0 1 ) 
					#name: #imageViewID 
					#flags: 0 
					#component: #imageView ) ) ) )! !

!KRFacturaAlquilerHeader publicMethodsFor: 'aspects'!

imageView
	^self logoKRONOwareReportTitle!

logoKRONOwareReportTitle
	| anImage |
	anImage := GIFReadWriter imageFromFileNamed: self defaulKRONOwareReportTitleImageFileName.
	^anImage! !

!KRFacturaAlquilerHeader publicMethodsFor: 'image access'!

defaulKRONOwareReportTitleImageFileName
	^'C:\Works\VW\KronoWare\FerSalud\imagenes\KRONOwareACV.gif'! !

!KRFacturaAlquilerLineaFactura class publicMethodsFor: 'interface specs'!

facturaAlquilerDetailedLineaFacturaReportComponentSpec
	"UIPainter new openOnClass: self andSelector: #facturaAlquilerDetailedLineaFacturaReportComponentSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'facturaAlquilerLineaFactura' 
			#min: #(#Point 500 31 ) 
			#bounds: #(#Rectangle 245 385 745 416 ) 
			#isEventDriven: true ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 20 0 5 0 0 0.5 25 0 ) 
					#name: #conceptoLineaFacturaID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #conceptoLineaFactura 
					#tabable: false 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 10 0.5 5 0 0 0.583333 25 0 ) 
					#name: #numeroUnidadesLineaFacturaID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #numeroUnidadesLineaFactura 
					#tabable: false 
					#alignment: #right 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 5 0.583333 5 0 0 0.7 25 0 ) 
					#name: #nombreUnidadLineaFacturaID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #nombreUnidadLineaFactura 
					#tabable: false 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 5 0.7 5 0 0 0.82 25 0 ) 
					#name: #precioUnitarioLineaFacturaID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #precioUnitarioLineaFactura 
					#tabable: false 
					#alignment: #right 
					#style: #pixelDefault 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 5 0.82 5 0 -20 1 25 0 ) 
					#name: #importeLineaFacturaID 
					#flags: 0 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #importeLineaFactura 
					#tabable: false 
					#alignment: #right 
					#style: #pixelDefault 
					#isReadOnly: true ) ) ) )!

facturaAlquilerLineaFacturaReportComponentSpec
	"UIPainter new openOnClass: self andSelector: #facturaAlquilerLineaFacturaReportComponentSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'facturaAlquilerLineaFactura' 
			#min: #(#Point 500 65 ) 
			#bounds: #(#Rectangle 442 576 940 641 ) 
			#isEventDriven: true ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#TextEditorSpec 
					#layout: #(#LayoutFrame 20 0.15 5 0 -20 0.85 60 0 ) 
					#name: #conceptoLineaFacturaID 
					#flags: 4 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 8191 8191 7167 ) ) 
					#model: #conceptoLineaFactura 
					#style: #pixelDefault 
					#isReadOnly: true ) ) ) )! !

!KRFacturaAlquilerLineaFactura publicMethodsFor: 'adding'!

addDataPacket: theDataPacket 

	super addDataPacket: theDataPacket.

	self conceptoLineaFactura value: 
		(theDataPacket metaInfo getObject: theDataPacket expression: 'noclone concepto') first.
	self numeroUnidadesLineaFactura value: 
		(theDataPacket metaInfo getObject: theDataPacket expression: 'noclone numeroUnidades') first.
	self nombreUnidadLineaFactura value:
		(theDataPacket metaInfo getObject: theDataPacket expression: 
			'noclone precioTarifaAlquiler conceptoAlquiler nombreUnidadFacturable') first.
	self precioUnitarioLineaFactura value: 
		(theDataPacket metaInfo getObject: theDataPacket expression: 'noclone precioUnitario') first.
	self importeLineaFactura value:
		(theDataPacket metaInfo getObject: theDataPacket expression: 'noclone precio') first.! !

!KRFacturaAlquilerLineaFactura publicMethodsFor: 'aspects'!

conceptoLineaFactura
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^conceptoLineaFactura isNil
		ifTrue:
			[conceptoLineaFactura := String new asValue]
		ifFalse:
			[conceptoLineaFactura]!

importeLineaFactura
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^importeLineaFactura isNil
		ifTrue:
			[importeLineaFactura := String new asValue]
		ifFalse:
			[importeLineaFactura]!

nombreUnidadLineaFactura
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^nombreUnidadLineaFactura isNil
		ifTrue:
			[nombreUnidadLineaFactura := String new asValue]
		ifFalse:
			[nombreUnidadLineaFactura]!

numeroUnidadesLineaFactura
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^numeroUnidadesLineaFactura isNil
		ifTrue:
			[numeroUnidadesLineaFactura := String new asValue]
		ifFalse:
			[numeroUnidadesLineaFactura]!

precioUnitarioLineaFactura
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^precioUnitarioLineaFactura isNil
		ifTrue:
			[precioUnitarioLineaFactura := String new asValue]
		ifFalse:
			[precioUnitarioLineaFactura]! !

!KRFacturaAlquilerReportWriter class publicMethodsFor: 'example'!

facturaAlquilerReportExample
	"KRFacturaAlquilerReportWriter facturaAlquilerReportExample"

	(KRFacturaAlquilerReportWriter reportName: #facturaAlquilerReportDefinition 
		executeOnStream: (Array with: Object new) readStream reportObject: Collection) displayReport.!

xfacturaAlquilerReport: laFactura
	"KRFacturaAlquilerReportWriter facturaAlquilerReport: nil"

	(KRFacturaAlquilerReportWriter reportName: #facturaAlquilerReportDefinition 
		executeOnStream: (Array with: laFactura) readStream reportObject: Collection) displayReport.!

xxfacturaAlquilerReport: laFactura
	"KRFacturaAlquilerReportWriter facturaAlquilerReport: nil"

	self
		reportName: #facturaAlquilerReportDefinition
		executeOnStream: (Array with: laFactura) readStream
		reportObject: nil! !

!KRFacturaAlquilerReportWriter class publicMethodsFor: 'report execution'!

facturaAlquilerReport: laFactura
	"KRFacturaAlquilerReportWriter facturaAlquilerReport: nil"
	
	| someLineasFactura |
	someLineasFactura := laFactura metaInfo getObject: laFactura featureNamedValue: 'lineasFactura'.
	(someLineasFactura isNil or: [ someLineasFactura isEmpty]) ifTrue: [ 
		Dialog warn: ('La Factua de Alquiler no tiene lineas de Factura\',
			'Por favor cree alguna linea de Factura') withCRs.
			^self
	].

	(self
		reportName: #facturaAlquilerReportDefinition
		executeOnStream: someLineasFactura readStream
		reportObject: nil) open! !

!KRFacturaAlquilerReportWriter class publicMethodsFor: 'reports'!

facturaAlquilerReportDefinition

	<resource: #krReport>
	^#(KRReportDefinition 
		bodyClass: KRFacturaAlquilerLineaFactura bodySelector: facturaAlquilerLineaFacturaReportComponentSpec 
		headerClass: KRFacturaAlquilerHeader headerSelector: facturaAlquilerHeaderReportComponentSpec 
		footerClass: KRFacturaAlquilerFooter footerSelector: facturaAlquilerFooterReportComponentSpec 
		titleClass: KRFacturaAlquilerTitle titleSelector: facturaAlquilerTitleReportComponentSpec 
		summaryClass: nil summarySelector: nil 
		height: 10.9 width: 8.48 leftMargin: 0.0 topMargin: 0.0 rightMargin: 0.0 bottomMargin: 0.0 
		separateSummaryPage: false separateTitlePage: false 
		showSummaryHeader: false showSummaryFooter: false showTitleHeader: true showTitleFooter: true 
		breakDefinitions: #(
	(KRMuThBreakDefinition 
		headerClass: KRFacturaAlquilerClinicaHeader headerSelector: facturaAlquilerClinicaHeaderReportComponentSpec 
		footerClass: KRFacturaAlquilerClinicaFooter footerSelector: facturaAlquilerClinicaFooterReportComponentSpec 
		level: 1 newPage: false selectorCollection: (class) hasSummaryInfo: true) 
	(KRMuThBreakDefinition 
		headerClass: KRFacturaAlquilerClienteHeader headerSelector: facturaAlquilerClienteHeaderReportComponentSpec 
		footerClass: KRFacturaAlquilerClienteFooter footerSelector: facturaAlquilerClienteFooterReportComponentSpec 
		level: 2 newPage: false selectorCollection: (class) hasSummaryInfo: true)
	(KRMuThBreakDefinition 
		headerClass: KRFacturaAlquilerFacturaHeader headerSelector: facturaAlquilerFacturaHeaderReportComponentSpec 
		footerClass: KRFacturaAlquilerFacturaFooter footerSelector: facturaAlquilerFacturaFooterReportComponentSpec 
		level: 3 newPage: false selectorCollection: (class) hasSummaryInfo: true)))! !

!KRFacturaAlquilerTitle class publicMethodsFor: 'interface specs'!

facturaAlquilerTitleReportComponentSpec
	"UIPainter new openOnClass: self andSelector: #facturaAlquilerTitleReportComponentSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'facturaAlquilerTitle' 
			#min: #(#Point 1 2 ) 
			#bounds: #(#Rectangle 0 0 1 2 ) 
			#isEventDriven: true ) 
		#component: 
		#(#SpecCollection 
			#collection: #() ) )! !

KRFacturaAlquilerReportWriter initializeAfterLoad!
KRFacturaAlquilerClienteFooter initializeAfterLoad!
KRFacturaAlquilerClienteHeader initializeAfterLoad!
KRFacturaAlquilerClinicaFooter initializeAfterLoad!
KRFacturaAlquilerClinicaHeader initializeAfterLoad!
KRFacturaAlquilerFacturaFooter initializeAfterLoad!
KRFacturaAlquilerFacturaHeader initializeAfterLoad!
KRFacturaAlquilerFooter initializeAfterLoad!
KRFacturaAlquilerHeader initializeAfterLoad!
KRFacturaAlquilerLineaFactura initializeAfterLoad!
KRFacturaAlquilerTitle initializeAfterLoad!
KRONO_Reports_Custom initializeAfterLoad!

KRONO_Reports_Custom loaded!
