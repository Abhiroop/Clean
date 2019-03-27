definition module trans

import StdEnv
import syntax,classify,predef

:: FusionOptions = { compile_with_fusion :: !Bool, generic_fusion :: !Bool, strip_unused :: !Bool }

transformGroups :: !CleanupInfo !Int !Int !Int !Int !*{!Component} !*{#FunDef} !*{!.ConsClasses} !{# CommonDefs}  !{# {# FunType} }
		!*ImportedTypes !*TypeDefInfos !*VarHeap !*TypeHeaps !*ExpressionHeap !FusionOptions !*File !*PredefinedSymbols
			-> (!*{!Component}, !*{#FunDef}, !*ImportedTypes, !ImportedConstructors, !*VarHeap, !*TypeHeaps, !*ExpressionHeap, !*File, !*PredefinedSymbols)
