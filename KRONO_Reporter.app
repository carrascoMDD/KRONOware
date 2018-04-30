'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:38:03 pm'!



Application create: #KRONO_Reporter with: 
    (#( META_ApplicationOrDialog Printing)
        collect: [:each | Smalltalk at: each ifAbsent: [
        self error: 'Not all of the prerequisites are loaded']])!

KRONO_Reporter becomeDefault!

Application subclass: #KRONO_Reporter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

KRONO_Reporter becomeDefault!

KRONO_Reporter initializeAfterLoad!

KRONO_Reporter loaded!
