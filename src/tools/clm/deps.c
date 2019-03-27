# include <stdio.h>
# include <stdlib.h>

typedef	struct	Deps	*Deps;
typedef	struct	DepList *DepList;

struct Deps
{
	char	*name;
	DepList	depList;
	int		mark;
	int		top;
	Deps	next;
};

struct	DepList
{
	Deps	mod;
	int		removed;
	DepList	next;
};

static	Deps	gDeps = NULL;

static	void
fatal (char *message)
{
	fprintf (stderr, "fatal error : %s\n", message);
} /* fatal */

static	Deps
NewDep (char *name)
{
	Deps	dep;

	for (dep = gDeps; dep != NULL; dep = dep->next)
		if (strcmp (dep->name, name) == 0)
			return (dep);

	dep	= malloc (sizeof (struct Deps));
	dep->name	= malloc (strlen (name) + 1);
	strcpy (dep->name, name);
	dep->depList	= NULL;
	dep->top		= 0;
	dep->mark		= 0;
	dep->next		= gDeps;

	gDeps	 		= dep;

	return (dep);
}

static void
AddDep (Deps mod, Deps dep)
{
	DepList	depList;

	depList	= malloc (sizeof (struct DepList));
	depList->mod		= dep;
	depList->removed	= 0;
	depList->next		= mod->depList;

	mod->depList		= depList;
}

static void
ReadDeps (void)
{
	char	line [80];
	Deps	mod = NULL;

	while (fgets (line,80,stdin) != NULL)
	{
		int length;

		length=strlen (line);

		if (length>0 && line[length-1]=='\n'){
			--length;
			line[length]='\0';
		}

		if (line [0] == '\t')
		{
			Deps	dep;

			if (mod == NULL)
				fatal ("ReadDeps");
		
			if (strcmp (line+1,mod->name)!=0){
				dep	= NewDep (line+1);
				AddDep (mod, dep);
			}
		}
		else
		{
			mod	= NewDep (line);
			mod->top	= 1;
		}
	}
} /* ReadDeps */

static	void
WriteDeps (void)
{
	Deps	dep;

	for (dep = gDeps; dep != NULL; dep = dep->next)
		if (dep->top)
		{
			DepList 	depList;
			int has_dep;

			fprintf (stdout, "%s\n", dep->name);
		
			has_dep=0;	
			for (depList = dep->depList; depList != NULL; depList = depList->next)
				if (!depList->removed){
					if (!has_dep){
						fprintf (stdout,"\t(");
						has_dep=1;
					} else
						fprintf (stdout," ");
					fprintf (stdout,"%s",depList->mod->name);
				}
			if (has_dep)
				fprintf (stdout,")\n");
		}
} /* WriteDeps */

static	char *gCurrent;

static	DepList
RemoveOneDep (char *name, DepList depList)
{
	for ( ; depList != NULL; depList = depList->next)
		if (strcmp (depList->mod->name, name) == 0)
		{
			if (!depList->removed)
				fprintf (stderr, "removing %s from %s\n", name, gCurrent);
			depList->removed	= 1;
		}
}

static	void
RemoveDep (Deps dep, DepList change)
{
	DepList	depList;
	
	if (dep->mark)
		return;

	dep->mark	= 1;

	for (depList = dep->depList; depList != NULL; depList = depList->next)
	{
		if (!depList->removed)
		{
			if (!depList->mod->mark)
				RemoveOneDep (depList->mod->name, change);
			RemoveDep (depList->mod, change);
		}
	}

	dep->mark	= 0;
}

static void
MinDep (Deps dep)
{
	DepList	depList;


	gCurrent	= dep->name;
	for (depList = dep->depList; depList != NULL; depList = depList->next)
		if (!depList->removed)
			RemoveDep (depList->mod, dep->depList);
}

static void
MinDeps (void)
{
	Deps	dep;

	for (dep = gDeps; dep != NULL; dep = dep->next)
		MinDep (dep);
}

int
main (void)
{
	ReadDeps ();
	MinDeps ();
	WriteDeps ();

	return 0;
}
