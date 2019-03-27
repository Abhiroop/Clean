definition module AbsSyn

:: FilePath:==Pathname
from PmTypes import ::Pathname,::Output

/**
 * Datatypes
 */
:: CpmAction
  =  Project FilePath ProjectAction
  |  Module String ModuleAction
  |  Environment EnvironmentAction
  |  CpmMake
  |  CpmHelp

:: ProjectAction
  =  CreateProject
  |  ShowProject
  |  BuildProject Bool FilePath
  |  ProjectPath PathAction
  |  SetRelativeRoot String
  |  SetTarget String
  |  SetExec String
  |  SetProjectOptions [ProjectOption]
  |  ProjectHelp

:: PathAction
  =  AddPathAction String
  |  RemovePathAction Int
  |  ListPathsAction
  |  MovePathAction Int PathDirection
  |  PathHelp

:: PathDirection
  =  MovePathUp
  |  MovePathDown
  |  MovePathTop
  |  MovePathBottom

:: ProjectOption
	= DynamicsOn
	| DynamicsOff
	| GenericFusionOn
	| GenericFusionOff
	| DescExLOn
	| DescExLOff
	| HeapSize !Int
	| StackSize !Int
	| Output !Output
	| LinkerGenerateSymbolsOn
	| LinkerGenerateSymbolsOff

:: ModuleAction
  =  CreateModule ModuleType
  |  ModuleHelp

:: ModuleType
  =  ApplicationModule
  |  LibraryModule

:: EnvironmentAction
  =  ListEnvironments
  |  ImportEnvironment FilePath
  |  RemoveEnvironment String
  |  ShowEnvironment String
  |  ExportEnvironment String
  |  CreateEnvironment String
  |  RenameEnvironment String String
  |  SetEnvironmentCompiler String String
  |  SetEnvironmentCodeGen String String
  |  EnvironmentHelp
  // TODO: EnvironmentPaths, EnvironmentVersion, EnvironmentProcessor, Environment64BitProcessor
