implementation module CpmLogic

/**
 * Clean libraries imports
 */
import StdEnv,StdStrictLists
from StdOverloadedList import ++|,Last,Init,RemoveAt,SplitAt,instance length [!!]
import set_return_code,Directory

/**
 * CPM imports
 */
import AbsSyn,CpmPaths

/**
 * CleanIDE imports
 */
import UtilIO,IdeState,Platform,PmPath,PmEnvironment,PmProject,PmDriver

/**
 * Execute a general CPM action
 */
doCpmAction :: String String !CpmAction !*World -> *World
doCpmAction cleanhome  pwd  CpmMake           world = doMake cleanhome pwd world
doCpmAction cleanhome  pwd  (Project pn pa)   world = doProjectAction cleanhome pwd pn pa world
doCpmAction cleanhome  pwd  (Module mn ma)    world = doModuleAction cleanhome mn ma world
doCpmAction cleanhome  pwd  (Environment ea)  world = doEnvironmentAction cleanhome pwd ea world
doCpmAction _          _    _                 world =
  help  "cpm <target>"
    [  "Where <target> is one of the following:"
    ,  "  <projectname> [--force] [--envs=filename] : build project <projectname>."
    ,  "                                              Optionally force build (default: 'false')"
    ,  "                                              Optionally specify the environments file (default: 'IDEEnvs')"
    ,  "  project <projectfile>                     : project actions"
    ,  "  module <modulename>                       : module actions"
    //,  "  environment                               : environment actions"
    ,  "  make                                      : build all projects in the current directory"
    ,  ""
    ,  "Execute `cpm <target> help` to get help for specific actions."] world

/**
 * Find all project files in the current working directory and build them
 */
doMake :: String !String !*World -> *World
doMake cleanhome pwd world
  # ((ok,pwd_path),world) = pd_StringToPath pwd world
  | not ok
 	= error ("Failed to read current directory ("+++pwd+++")") world
  # ((err,entries), world) = getDirectoryContents pwd_path world
  | err<>NoDirError
 	= error ("Failed to read current directory ("+++pwd+++")") world
 	# xs = [e \\ {fileName=e}<-entries
 			| size e>=4 && e.[size e-4]=='.' && e.[size e-3]=='p' && e.[size e-2]=='r' && e.[size e-1]=='j']
 	| isEmpty xs
		= error ("No project file found in " +++ pwd) world
		= foldr (\pn -> doProjectAction cleanhome pwd pn (BuildProject False EnvsFileName)) world xs

/**
 * Default compiler options. Currently it is a simple alias for
 * forwards-compatibility.
 */
compilerOptions :: CompilerOptions
compilerOptions = DefaultCompilerOptions

getLine :: *World -> *(String, *World)
getLine world
  # (console, world)  = stdio world
  # (line, console)   = freadline console
  # (_, world)        = fclose console world
  = (line, world)

/**
 * Execute project-specific actions
 */
doProjectAction :: String String String ProjectAction *World -> *World
doProjectAction cleanhome pwd  pn  CreateProject world
  //Check if main module exists
  # (exists,world)  = accFiles (FExists mainmodule) world
  | not exists //       = error ("Main module " +++ mainmodule +++ " does not exist.") world
    # world         = showLines ["Main module " +++ mainmodule +++ " does not exist. Create it? [y/n]"] world
    # (line, world) = getLine world
    | line.[0] == 'y' = mkMainAndProject world
    | otherwise       = error ("Failed to create project. Need " +++ mainmodule) world
  | otherwise       = mkProject world
  where
  mainmodule   = MakeImpPathname pn

  mkMainAndProject world
    # world = doModuleAction "" mainmodule (CreateModule ApplicationModule) world
    = mkProject world
  mkProject world
    # edit_options   = {eo={newlines=NewlineConventionUnix},pos_size=NoWindowPosAndSize}
    //Create project file using the Clean IDE libraries
    # prj            = PR_NewProject mainmodule edit_options compilerOptions DefCodeGenOptions
                         DefApplicationOptions [!!] DefaultLinkOptions
    # project        = PR_SetRoot mainmodule edit_options compilerOptions prj
    # projectfile    = MakeProjectPathname pn
    = saveProject cleanhome pwd project projectfile world

doProjectAction cleanhome pwd  pn  ShowProject world
  # (proj_path, project, ok, world) = openProject pwd pn cleanhome world
  | not ok
	= world
  = showLines  [  "Content of " +++ proj_path +++ ":"
               ,  "ProjectRoot..: " +++ PR_GetRelativeRootDir project
               ,  "Target.......: " +++ PR_GetTarget project
               ,  "Executable...: " +++ PR_GetExecPath project
               ,  "Paths........:"
               :  showPaths project
               ] world

doProjectAction cleanhome pwd  pn  (BuildProject force ideenvs) world
  # (envs, world)                = readIDEEnvs cleanhome ideenvs world
  # (proj_path, proj, ok, world) = openProject pwd pn cleanhome world
  | not ok
	= world
  # (console, world)             = stdio world
  # iniGeneral                   = initGeneral True compilerOptions cleanhome proj_path proj envs console
  # {ls, gst_world}              = pinit force {ls=iniGeneral,gst_world=world,gst_continue_or_stop=False}
  = gst_world
  where
  pinit force_rebuild gst = BringProjectUptoDate force_rebuild cleanup gst
  cleanup exepath bool1 bool2 ps = abortLog (not bool2) "" ps

doProjectAction cleanhome pwd pn  (ProjectPath pa) world
  # (proj_path, project, ok, world) = openProject pwd pn cleanhome world
  | not ok
	= world
  = doProjectPathAction cleanhome pwd pn project pa world

doProjectAction cleanhome pwd pn (SetRelativeRoot relroot) world
  = withProject pwd pn cleanhome (uncurry (change_root_directory_of_project relroot) o PR_GetRootPathName) world

doProjectAction cleanhome pwd pn (SetTarget target) world
  = withProject pwd pn cleanhome (PR_SetTarget target) world

doProjectAction cleanhome pwd pn (SetExec exec) world
  = withProject pwd pn cleanhome (PR_SetExecPath exec) world

doProjectAction cleanhome pwd pn (SetProjectOptions project_options) world
	= withProject pwd pn cleanhome (set_project_options project_options) world
where
	set_project_options [project_option:project_options] project
		# project = set_project_option project_option project
		= set_project_options project_options project
	set_project_options [] project
		= project

	set_project_option DynamicsOn project
		= PR_SetApplicationOptions {PR_GetApplicationOptions project & dynamics = True} project
	set_project_option DynamicsOff project
		= PR_SetApplicationOptions {PR_GetApplicationOptions project & dynamics = False} project
	set_project_option GenericFusionOn project
		= PR_SetApplicationOptions {PR_GetApplicationOptions project & generic_fusion = True} project
	set_project_option GenericFusionOff project
		= PR_SetApplicationOptions {PR_GetApplicationOptions project & generic_fusion = False} project
	set_project_option DescExLOn project
		= PR_SetApplicationOptions {PR_GetApplicationOptions project & desc_exl = True} project
	set_project_option DescExLOff project
		= PR_SetApplicationOptions {PR_GetApplicationOptions project & desc_exl = False} project
	set_project_option (HeapSize hs) project
		= PR_SetApplicationOptions {PR_GetApplicationOptions project & hs = hs} project
	set_project_option (StackSize ss) project
		= PR_SetApplicationOptions {PR_GetApplicationOptions project & ss = ss} project
	set_project_option (Output output) project
		= PR_SetApplicationOptions {PR_GetApplicationOptions project & o = output} project
	set_project_option LinkerGenerateSymbolsOn project
		= PR_SetLinkOptions project {PR_GetLinkOptions project & generate_symbol_table=True}
	set_project_option LinkerGenerateSymbolsOff project
		= PR_SetLinkOptions project {PR_GetLinkOptions project & generate_symbol_table=False}

doProjectAction _          _  _   _    world             =
  help "cpm project <projectfile> <action>"
    [  "Where <action> is one of the following"
    ,  "  create                            : create a new project"
    ,  "  show                              : show project information"
    ,  "  build [--force] [--envs=filename] : build the project. Optionally force build (default: 'false')"
    ,  "                                      Optionally specify the environments file (default: 'IDEEnvs')"
    ,  "  path                              : manage project paths"
    ,  "  root .[.]                         : set the project root relative to the project file."
    ,  "                                    :  . is the same dir, .. the parent, ... the grandparent, etc."
    ,  "  target <env>                      : set target environment to <env>"
    ,  "  exec <execname>                   : set executable name to <execname>"
    ,  "  set option [option]               : Set one or more of the following options:"
    ,  "                                    : -generic_fusion,-ngeneric_fusion"
    ,  "                                    :     Enable or disable generic fusion"
    ,  "                                    : -dynamics,-ndynamics"
    ,  "                                    :     Enable or disable dynamics"
    ,  "                                    : -descexl,-descexl"
    ,  "                                    :     Enable or disable descriptor generation and label exporting"
    ,  "                                    :     This translates to passing -desc and -exl to cocl"
    ,  "                                    : -b,-nr,-nc,-sc"
    ,  "                                    :     Set the output option to BasicValuesOnly, NoReturnType,"
    ,  "                                    :     NoConsole or ShowConstructors respectively"
    ] world

/**
 * Execute environment-specific actions
 */
doEnvironmentAction :: String String EnvironmentAction *World -> *World
doEnvironmentAction cleanhome  pwd  ListEnvironments                world = error ("Not implemented") world
doEnvironmentAction cleanhome  pwd  (ImportEnvironment ef)          world = error ("Not implemented") world
doEnvironmentAction cleanhome  pwd  (RemoveEnvironment en)          world = error ("Not implemented") world
doEnvironmentAction cleanhome  pwd  (ShowEnvironment en)            world = error ("Not implemented") world
doEnvironmentAction cleanhome  pwd  (ExportEnvironment en)          world = error ("Not implemented") world
doEnvironmentAction cleanhome  pwd  (CreateEnvironment en)          world = error ("Not implemented") world
doEnvironmentAction cleanhome  pwd  (RenameEnvironment en en`)      world = error ("Not implemented") world
doEnvironmentAction cleanhome  pwd  (SetEnvironmentCompiler en cp)  world = error ("Not implemented") world
doEnvironmentAction cleanhome  pwd  (SetEnvironmentCodeGen en cp)   world = error ("Not implemented") world
doEnvironmentAction _          _    _                               world =
  help "cpm environment <action>"
    [  "Where <action> is one of the following"
    ,  "  list                                  : list all available environments"
    ,  "  import <filepath>                     : import an environement from file <filepath>"
    ,  "  create <envname>                      : create a new environment with name <envname>"
    ,  "  remove <envname>                      : remove evironment <envname>"
    ,  "  show <envname>                        : show environment <envname>"
    ,  "  export <envname>                      : export environment <envname>"
    ,  "  rename <envname> <envname`>           : rename environment <envname> to <envname`>"
    ,  "  setcompiler <envname> <compilername>  : set compiler for <envname> to <compilername>"
    ,  "  setcodegen <envname> <codegenname>    : set codegen for <envname> to <codegenname>"
    ] world

/**
 * Modify a project
 */
withProject :: !String !String !String (Project -> Project) *World -> *World
withProject pwd pn cleanhome f world
  # (project_path, project, ok, world) = openProject pwd pn cleanhome world
  | not ok
	= world
  = saveProject cleanhome pwd (f project) project_path world

/**
 * Execute path-related project actions
 */
doProjectPathAction :: String String String Project PathAction *World -> *World
doProjectPathAction cleanhome pwd pn project (AddPathAction path) world
  = doModPaths cleanhome pwd pn project (\t -> [!GetLongPathName path:t!]) world

doProjectPathAction cleanhome pwd pn project (RemovePathAction i) world
  = doModPaths cleanhome pwd pn project (RemoveAt i) world

doProjectPathAction _ _ _ project ListPathsAction world
  = showLines ["Paths for project:" : showPaths project] world

doProjectPathAction cleanhome pwd pn project (MovePathAction i pdir) world
  = doModPaths cleanhome pwd pn project (moveStrictListIdx i pdir) world

doProjectPathAction _          _ _   _     _  world
  = help "cpm project <projectname.prj> path <action>"
    [  "Where <action> is one of the following"
    ,  "  add <path>          : add a path to the project"
    ,  "  list                : list all project paths and their index"
    ,  "  remove <i>          : remove path <i> from the list of projects"
    ,  "  move <i> <up|down>  : move path <i> up or down one position" ] world

/**
 * Collect all project paths in a list with an index prefixed
 */
showPaths :: !Project -> [String]
showPaths project = ["  [" +++ toString n +++ "]  " +++ p \\ p<|-PR_GetPaths project & n<-[0..]]

/**
 * Modify the list of paths in a project given a modification function which
 * takes a strict list of project paths and returns a strict list of project
 * paths.
 */
doModPaths :: !String !String !String !Project ([!String!] -> [!String!]) *World -> *World
doModPaths cleanhome pwd pn project f world
  # paths = PR_GetPaths project
  # prj   = PR_SetPaths False paths (f paths) project
  # world = saveProject cleanhome pwd prj pn world
  = showLines ["Successfully modified project paths"] world

append_dir_separator :: !{#Char} -> {#Char}
append_dir_separator s
	| size s>0 && s.[size s-1]==DirSeparator
		= s
		= s+++DirSeparatorString

/**
 * Open a project file
 */
openProject :: !FilePath !FilePath !FilePath !*World -> (!FilePath, !Project, Bool, !*World)
openProject pwd pn cleanhome world
  # proj_path                = GetLongPathName (append_dir_separator pwd +++ pn)
  # ((prj, ok, err), world)  = accFiles (ReadProjectFile proj_path cleanhome) world
  | not ok || err <> ""
	= (proj_path, prj, ok, error err world)
  = (proj_path, prj, ok, world)

/**
 * Save a project back to its project file
 */
saveProject :: !FilePath !FilePath !Project !FilePath !*World -> *World
saveProject cleanhome pwd prj projectfile world
  # proj_path = GetLongPathName projectfile
  # (ok, world) = accFiles (SaveProjectFile proj_path prj cleanhome) world
  | not ok
	= error "Error saving project" world
  = world

/**
 * Move a path at a given index up or down the list of paths. Abort execution
 * if the index is out of bounds.
 */
moveStrictListIdx :: !Int PathDirection [!a!] -> [!a!]
moveStrictListIdx i dir xs
  | i < 0 || i > length xs - 1 = abort ("Index " +++ toString i +++ " out of bounds")
  | otherwise                  = msl dir (SplitAt i xs)
  where  msl MovePathUp      ([!!], xs)        = xs
         msl MovePathUp      (xs, [!x:ys!])    = Init xs ++| [!x : Last xs : ys!]
         msl MovePathDown    ([!!], [!x:y:ys!])= [!y:x:ys!]
         msl MovePathDown    (xs, [!!])        = xs
         msl MovePathDown    (xs, [!y!])       = xs ++| [!y!]
         msl MovePathDown    (xs, [!x:y:ys!])  = xs ++| [!y:x:ys!]
         msl MovePathTop     (xs, [!!])        = xs
         msl MovePathTop     (xs, [!y:ys!])    = [!y:xs++|ys!]
         msl MovePathBottom  (xs, [!!])        = xs
         msl MovePathBottom  (xs, [!y:ys!])    = xs ++| ys ++| [!y!]

/**
 * Execute module-related actions
 */
doModuleAction :: String !String !ModuleAction !*World -> *World
doModuleAction _ mn  (CreateModule mt) world
  # (dclexists, world)  = accFiles (FExists dclnm) world
  | dclexists           = error ("Definition module '" +++ dclnm +++ "' already exists.") world
  # (iclexists, world)  = accFiles (FExists iclnm) world
  | iclexists           = error ("Implementation module '" +++ iclnm +++ "' already exists.") world
  = writeMods mt world
  where
  dclnm      = MakeDefPathname mn
  iclnm      = MakeImpPathname mn
  basenm     = iclnm % (0,size iclnm-5)

  mkmod mty  = mty +++ "module " +++ basenm

  writeMods ApplicationModule world = writeicl ApplicationModule world
  writeMods LibraryModule world
    # world = writeicl ApplicationModule world
    = writedcl world

  writeicl ApplicationModule  world = writeicl` "" world
  writeicl LibraryModule      world = writeicl` "implementation " world

  writeicl` pref world = writemod iclnm pref ("Failed to write implementation module '" +++ basenm +++ "'") world

  writedcl world = writemod dclnm "definition " ("Failed to write definition module '" +++ basenm +++ "'") world

  writemod nm pref errmsg world
  	# (ok,file,world) = fopen nm FWriteText world
  	| not ok
  		= error errmsg world
  	# file = fwrites (mkmod pref) file
  	  (ok,world) = fclose file world
  	| not ok
  		= error errmsg world
	    = world

doModuleAction _ _   _  world                =
  help "cpm module <modulename> <action>"
    [  "Where <action> is one of the following"
    ,  "  create [application|library]  : create a new module. Optionally specify module type (default: 'library')"
    //,  "  check <projectname.prj>       : type-check module in the context of project <projectname.prj>"
    //,  "  compile <projectname.prj>     : compile module in the context of project <projectname.prj>"
    ] world

/**
 * Show an error message
 */
error :: !String !*World -> *World
error message world
  # stderr     = fwrites message stderr
  # (ok,world) = fclose stderr world
  = set_return_code_world (-1) world

/**
 * Show a help message
 */
help :: !String ![String] !*World -> *World
help cmd lines world
  # lines` = [ "CPM: Clean Project Manager"
             : ""
             : "Usage: " +++ cmd
             : lines]
  = showLines lines` world

/**
 * Given a list of strings, concatenate them to a single string with newlines
 * in between, then print that new string to console.
 */
showLines :: ![String] !*World -> *World
showLines lines world
  # (console, world) = stdio world
  # console = foldl (\file s -> fwritec '\n' (fwrites s file)) console lines
  = snd (fclose console world)
