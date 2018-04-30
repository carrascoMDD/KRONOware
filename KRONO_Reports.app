'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:38:03 pm'!



Application create: #KRONO_Reports with: 
    (#( KRONO_Reporter KRONO_Simple)
        collect: [:each | Smalltalk at: each ifAbsent: [
        self error: 'Not all of the prerequisites are loaded']])!

KRONO_Reports becomeDefault!

Application subclass: #KRONO_Reports
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reports becomeDefault!

KRONO_Reports initializeAfterLoad!

KRONO_Reports loaded!
