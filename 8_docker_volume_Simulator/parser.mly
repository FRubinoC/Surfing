%{
	open Ast
%}

%token OPENNODE ENDNODE							/* ":[", "]" */
%token LROUND RROUND
%token DOT SEMICOLON COMMA QUESTION COLON		/* ".", ";", ",", "?", ":" */
%token OPENITER CLOSEITER						/* "µh", "h" */
%token OPENCONDBODY CLOSECONDBODY
%token LPAREN RPAREN
%token ADD SUB MUL DIV
%token <string> SENSORLOC ACTUATORLOC PROCESSNAME
%token <int> INT
%token <bool> BOOL
%token <char> CHAR
%token <string> STRING
%token <string> IDENT
%token PARALNODE PCOMPONENT						/* "|", "||" */ 
%token OPENOUTPUT CLOSEOUTPUT					/* "«", "»→" */
%token ASSIGNMENT EQUAL								/* ":=" */
%token OPENACTOUTPUT CLOSEACTOUTPUT				/* "<", ">" */
%token OPENACTINPUT CLOSEACTINPUT				/* "(|", "|)" */
%token INTERNALACT								/* "ŧ" */
%token EQUALORGREATER EQUALORLOWER
%token DOUBLEEQUAL
%token INACTTOKEN
%token NODESDECLARATION NODESDEFINITION FUNCTDEFINITION
%token WAIT RELEASE USED PREPARE
%token ENCRYPT DECRYPT
%token EOF

%left MUL DIV ADD SUB COMMA PARALNODE PCOMPONENT

%start main
%type <iot_structure> main


%%


main:
	iot_structure EOF {$1}
;

/* Nodes definition */
term:
	  value								/* value */
		{ Value($1) }
	| IDENT LPAREN term RPAREN			/* functions */
		{ Funct($1, $3) }
	| SENSORLOC
		{ SensorLoc($1) }
	| IDENT								/* variable */
		{ Variable($1) }
	| term COMMA term
		{ ParallelTerms($1, $3) }
	| term ADD term
		{ Add($1, $3) }
	| term SUB term
		{ Sub($1, $3) }
	| term MUL term
		{ Mul($1, $3) }
	| term DIV term
		{ Div($1, $3) } 
	| LPAREN term RPAREN
		{ $2 }
;
value:
	  INT
	  	{ Eint($1) }
	| BOOL
		{ Ebool($1) }
	| CHAR
		{ Echar($1) }
	| STRING
		{ Estring($1) }
;
node_number:
	  node_number COMMA node_number			/* node id listing */
		{ ParallelNodeNumber($1, $3) }
	| INACTTOKEN 					/* inactive node */
		{ NoNode }
	| INT							/* node id */
		{ NodeNumber($1) }
	
;
component:
	  component PCOMPONENT component
		{ ParallelComponent($1, $3) }	  
	| PROCESSNAME EQUAL LROUND process RROUND
		{ Process($1, $4) }
	| SENSORLOC EQUAL LROUND sensor RROUND
		{ Sensor($1, $4) }
	| ACTUATORLOC EQUAL LROUND actuator RROUND
		{ Actuator($1, $4) }
	| INACTTOKEN
	  	{ InactiveComponent }
;
condition:
	  BOOL
	  	{ Bool($1) }
	| term CLOSEACTOUTPUT term
		{ Greater($1, $3)}
	| term OPENACTOUTPUT term
		{ Lower($1, $3) }
	| term DOUBLEEQUAL term
		{ Equal($1, $3) }
	| term EQUALORGREATER term
		{ EqGr($1, $3) }
	| term EQUALORLOWER term
		{ EqLw($1, $3) }
;
process:
	  INACTTOKEN								/* inactive process */
		{ InactProcess }
	| OPENOUTPUT IDENT SEMICOLON term CLOSEOUTPUT node_number DOT process  			/* output statement */
		{ MultiOutput($2, $4, $6, $8) }
	| LPAREN INT SEMICOLON IDENT SEMICOLON term RPAREN DOT process  		/*  input statement */
		{ InputProc($2, $4, $6, $9) }	
	| LPAREN condition RPAREN QUESTION OPENCONDBODY process COLON process CLOSECONDBODY DOT	process		/* conditional statement */
		{ ConditionProc($2, $6, $8, $11) }
	| CLOSEITER 								/* closing the iteration */
		{ PCloseIter }
	| OPENITER DOT process 							/* opening the iteration */
		{ POpenIter($3) }
	| IDENT ASSIGNMENT term DOT process 						/* assignment statement */
		{ Assignment($1, $3, $5) }	
	| OPENACTOUTPUT ACTUATORLOC COMMA IDENT CLOSEACTOUTPUT DOT process 	 	/* output actutator action */
		{ ActivateActuator($2, $4, $7) }
	| WAIT IDENT DOT process
		{ Wait($2, $4) }
	| RELEASE IDENT DOT process
		{ Release($2, $4) }
	| USED IDENT DOT process
		{ Used($2, $4) }
	| PREPARE IDENT DOT process
		{ Prepare($2, $4) }
	| ENCRYPT IDENT DOT process
		{ Encrypt($2, $4) }
	| DECRYPT IDENT DOT process
		{ Decrypt($2, $4) }
;
sensor:
	  INACTTOKEN					/* inactive sensor */
		{ InactiveSensor }
	| INTERNALACT DOT sensor  				/* internal action of the sensor */
		{ SensorIntAction($3) }
	| SENSORLOC ASSIGNMENT LPAREN INT COMMA INT RPAREN DOT sensor 		/* assignment of the sensor location */
		{ SensorStore($1, $4, $6, $9) }
	| CLOSEITER 						/* closing iteration */
		{  SCloseIter }
	| OPENITER DOT sensor 					/* opening iteration */
		{ SOpenIter($3) }
;
actuator:
	  INACTTOKEN								/* inactive actuator */
		{ InactiveActuator }
	| INTERNALACT DOT actuator 						/* internal action */
		{ ActuatorIntAction($3) }
	| OPENACTINPUT term CLOSEACTINPUT DOT actuator  	/* actuator action */
		{ ActuatorCommand($2, $5) }
	| CLOSEITER 								/* closing iteration */
		{ACloseIter}
	| OPENITER DOT actuator							/* opening iteration */
		{ AOpenIter($3) }
;
node:
	  node PARALNODE node 						/* parallel composition of nodes */
		{ ParallelNodes($1, $3) }
	| INACTTOKEN  
	  	{ InactNode }
	| INT OPENNODE component ENDNODE	 	/* single node */
		{ Node($1, $3) }
;
funct_definition:
	  funct_definition funct_definition
		{ ParallelFunctions($1, $2) }
	| INACTTOKEN
		{ InactFunction }
	| IDENT LPAREN term RPAREN LROUND term RROUND
		{ FunctionDefinition($1, $3, $6) }
;

/* Nodes declaration */
node_declaration:
	  node_declaration PARALNODE node_declaration
		{ ParallelNodesDeclaration($1, $3)}
	| INT COLON LPAREN declaration RPAREN
		{ Store($1, $4) }

;
declaration:
 	  declaration COMMA declaration
		{ ParallelDeclarations($1, $3) }
	| SENSORLOC
	  	{ SensorDeclaration($1) }
	| ACTUATORLOC
		{ ActuatorDeclaration($1) }
	| IDENT
		{ VariableDeclaration($1) }
;

/* Top of the AST */
iot_structure:
	NODESDECLARATION LROUND node_declaration RROUND FUNCTDEFINITION LROUND funct_definition RROUND NODESDEFINITION LROUND node RROUND
		{ IoTStructure($3, $7, $11) }
;