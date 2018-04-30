'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:38:03 pm'!



Application create: #KRONO_Simple with: 
    (#( CODE_META)
        collect: [:each | Smalltalk at: each ifAbsent: [
        self error: 'Not all of the prerequisites are loaded']])!

KRONO_Simple becomeDefault!

Application subclass: #KRONO_Simple
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Simple becomeDefault!

KRONO_Simple initializeAfterLoad!

KRONO_Simple loaded!
