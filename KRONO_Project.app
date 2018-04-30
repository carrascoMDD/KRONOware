'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:38:03 pm'!



Application create: #KRONO_Project with: 
    (#( KRONO_Simple)
        collect: [:each | Smalltalk at: each ifAbsent: [
        self error: 'Not all of the prerequisites are loaded']])!

KRONO_Project becomeDefault!

Object subclass: #KRONOProject
	instanceVariableNames: 'name folder domainCMGO '
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Project becomeDefault!

Application subclass: #KRONO_Project
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Project becomeDefault!

!KRONOProject class publicMethodsFor: 'defaults'!

backupPostfix
	^'_Copia_'!

defaultFolderName
	^Filename findDefaultDirectory asString!

defaultProjectName
	^'FerSalud' copy!

savePostfix
	^'.krono'! !

!KRONOProject class publicMethodsFor: 'info store ref'!

codesStoreClass
	^KRIcd9CMCodesInfoHolder!

infoStoreClass
	^KRSimpleInfoHolder! !

!KRONOProject class publicMethodsFor: 'instance creation'!

createNewProject
	| aProject aDomainCMGO aInfoStoreClass aProjectName aCodesStoreClass |

	aInfoStoreClass := self infoStoreClass.
	aInfoStoreClass isNil ifTrue: [ ^nil].
	aInfoStoreClass resetCurrentInfos.

	aCodesStoreClass := self codesStoreClass.
	aCodesStoreClass isNil ifTrue: [ ^nil].
	aCodesStoreClass resetCurrentInfos.
	
	aDomainCMGO := aInfoStoreClass currentInfo.
	aDomainCMGO isNil ifTrue: [ ^nil].

	aProject := self new.
	aProject domainCMGO: aDomainCMGO.

	aProjectName := aDomainCMGO metaInfo getObject: aDomainCMGO 
		featureNamedValue: aDomainCMGO metaInfo class domainNameCMGODomainAttributeName.
	(aProjectName isNil or: [ aProjectName isEmpty]) ifTrue: [
		 aProjectName := self defaultProjectName
	].
	aProject name: aProjectName.

	^aProject!

openProjectMono
	^self openProjectMonoInFolder: nil!

openProjectMonoInFolder: theFolder
	| aFolder	aFileDialogInterface aFilename aFilePath aFilePathFilename aSavePostfix aInfoStoreClass aCodesStoreClass aDomainCMGO aProject aFolderName aProjectName aDotIndex |

	aSavePostfix := KRONOProject savePostfix.

	aFileDialogInterface := FileDialogInterface new.
	aFileDialogInterface isNil ifTrue: [ ^nil].
	aFileDialogInterface titleBarText: 'Abrir Fichero Clinica' , (self name isNil ifTrue: [ ' - sin nombre - '] ifFalse: [ self name]).
	aFileDialogInterface addOption: #fileMustExist.
	aFileDialogInterface fileFilter: ((OrderedCollection new: 1) 
		add:  (Association key:  'Ficheros de Clinicas KRONOware  (*', aSavePostfix, ')' value: '*' ,aSavePostfix); 
		add:  (Association key: 'Todos los ficheros (*.*)' value: '*.*'); 
		yourself).

	aFolder := theFolder.
	(aFolder isNil not and: [ aFolder isEmpty not]) ifTrue: [
		aFilename := aFolder asFilename.
		(aFilename exists and: [aFilename isDirectory]) ifTrue: [
			aFileDialogInterface initialDirectory: aFolder]].

	aFileDialogInterface openFileDialog == true ifFalse: [ ^false].

	aFilePath := aFileDialogInterface returnedFilePath.
	aFilePath isNil ifTrue: [ ^nil].
	
	aFilePathFilename := aFilePath asFilename.

	aFilePathFilename asFilename exists ifFalse: [
		Dialog warn: 'Fichero ', aFilePathFilename, ' no existe'.
		^nil].

	Transcript show: 'Cargando Clinica desde el fichero', aFilePath , '...'; cr.


	aInfoStoreClass := self infoStoreClass.
	aInfoStoreClass isNil ifTrue: [ ^nil].
	aInfoStoreClass resetCurrentInfos.

	aCodesStoreClass := self codesStoreClass.
	aCodesStoreClass isNil ifTrue: [ ^nil].
	aCodesStoreClass resetCurrentInfos.
self halt.
	aDomainCMGO := aInfoStoreClass loadCurrentInfoClinicaFrom: aFilePath.

	(aDomainCMGO isKindOf: CMGenericObject) ifFalse: [
		Transcript show: 'No ha sido posible cargar una Clinica KRONOware desde el fichero ', aFilePath ; cr.
		^nil].
		
	aProject := self new.
	aProject domainCMGO: aDomainCMGO.

	aFolderName := aFilePathFilename head.
	aProjectName := aFilePathFilename tail.
	aDotIndex := aProjectName indexOf: $..
	aDotIndex  > 0 ifTrue: [ 
		aProjectName := (aDotIndex = 1 or: [aProjectName size < 2])
			ifTrue: [ self class defaultProjectName ]
			ifFalse: [ aProjectName copyFrom: 1 to: aDotIndex - 1]
	].

	aProject folder: aFolderName.
	aProject name: aProjectName.

	^aProject! !

!KRONOProject publicMethodsFor: 'accessing'!

domainCMGO

	^domainCMGO!

domainCMGO: theDomainCMGO

	domainCMGO := theDomainCMGO.
	self changed: #domainCMGO!

folder

	folder isNil ifTrue: [ folder := self class defaultFolderName].
	^folder!

folder: theFolder

	folder := theFolder.
	self changed: #folder!

name
	name isNil ifTrue: [ name := self class defaultProjectName].
	^name!

name: theName
	name := theName.
	self changed: #name!

saveFileName
	^self name , self class savePostfix!

saveFilePath
	^(self folder asFilename construct: self saveFileName) asString! !

!KRONOProject publicMethodsFor: 'actions'!

close

	| aDomainCMGO aInfoStoreClass |
	aDomainCMGO := self domainCMGO.
	aDomainCMGO isNil ifFalse: [ 
		aInfoStoreClass := self class infoStoreClass.
		aInfoStoreClass isNil ifFalse: [ 
			(aInfoStoreClass hasCurrentInfo: aDomainCMGO) ifTrue: [ 
				aInfoStoreClass resetCurrentInfo
			]
		]
	].

	self release!

save

	| aDomainCMGO aInfoStoreClass aFilePath |

	aDomainCMGO := self domainCMGO.
	aDomainCMGO isNil ifTrue: [ ^nil].

	aInfoStoreClass := self class infoStoreClass.
	aInfoStoreClass isNil ifTrue: [ ^nil].

	(aInfoStoreClass hasCurrentInfo: aDomainCMGO) ifFalse: [ ^nil].

	aFilePath := self saveFilePath.
	(aFilePath isNil or: [ aFilePath isEmpty]) ifTrue: [ ^nil].

	^aInfoStoreClass saveCurrentInfoClinicaAs: aFilePath!

saveAs: theFilePath

	| aDomainCMGO aInfoStoreClass |

	(theFilePath isNil or: [ theFilePath isEmpty]) ifTrue: [ ^nil].

	aDomainCMGO := self domainCMGO.
	aDomainCMGO isNil ifTrue: [ ^nil].

	aInfoStoreClass := self class infoStoreClass.
	aInfoStoreClass isNil ifTrue: [ ^nil].

	(aInfoStoreClass hasCurrentInfo: aDomainCMGO) ifFalse: [ ^nil].

	^aInfoStoreClass saveCurrentInfoClinicaAs: theFilePath! !

!KRONOProject publicMethodsFor: 'initialize-release'!

release

	name := nil.
	folder := nil.
	domainCMGO := nil.! !

KRONOProject initializeAfterLoad!
KRONO_Project initializeAfterLoad!

KRONO_Project loaded!
