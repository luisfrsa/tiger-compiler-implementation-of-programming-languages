/* Tiger compiler
   Copyright (C) 2016 Free Software Foundation, Inc.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


#include "config.h"       
#include "system.h"       
#include "coretypes.h"       
#include "target.h"       
#include "tree.h"       
#include "gimple-expr.h"       
#include "diagnostic.h"       
#include "opts.h"       
#include "fold-const.h"       
#include "gimplify.h"       
#include "stor-layout.h"       
#include "debug.h"       
#include "convert.h"       
#include "langhooks.h"       
#include "langhooks-def.h"       
#include "common/common-target.h"       
#include "tiger/tiger-parser.h"       
#include "tiger/tiger-lexer.h"   
/* Language-dependent contents of a type.  */

struct GTY (()) lang_type
{
  char dummy;
};

/* Language-dependent contents of a decl.  */

struct GTY (()) lang_decl
{
  char dummy;
};

/* Language-dependent contents of an identifier.  This must include a
   tree_identifier.  */

struct GTY (()) lang_identifier
{
  struct tree_identifier common;
};

/* The resulting tree type.  */

union GTY ((desc ("TREE_CODE (&%h.generic) == IDENTIFIER_NODE"),
	    chain_next ("CODE_CONTAINS_STRUCT (TREE_CODE (&%h.generic), "
			"TS_COMMON) ? ((union lang_tree_node *) TREE_CHAIN "
			"(&%h.generic)) : NULL"))) lang_tree_node
{
  union tree_node GTY ((tag ("0"), desc ("tree_node_structure (&%h)"))) generic;
  struct lang_identifier GTY ((tag ("1"))) identifier;
};

/* We don't use language_function.  */

struct GTY (()) language_function
{
  int dummy;
};

/* Language hooks.  */

static bool
tiger_langhook_init (void)
{
  build_common_tree_nodes (false);

  /* I don't know why this has to be done explicitly.  */
  void_list_node = build_tree_list (NULL_TREE, void_type_node);

  build_common_builtin_nodes ();

  return true;
}

static void
tiger_langhook_parse_file (void)
{
  tiger_parse_files (num_in_fnames, in_fnames);
}

/*comentado no tiger-parser.cc e adicionado aqui */
static void tiger_parse_file (const char *filename);

void
tiger_parse_files (int num_files, const char **files)
{
  for (int i = 0; i < num_files; i++)
    {
      tiger_parse_file (files[i]);
    }
}

static void
tiger_parse_file (const char *filename)
{
  FILE *file = fopen (filename, "r");
  if (file == NULL)
    {
      fatal_error (UNKNOWN_LOCATION, "cannot open filename %s: %m", filename);
    }
 
  // Here we would parse our file
  Tiger::Lexer lex (filename, file);
 
  Tiger::const_TokenPtr tok = lex.peek_token ();
  for (;;)
    {
      bool has_text = tok->get_id () == Tiger::IDENTIFIER
          || tok->get_id () == Tiger::INTEGER_LITERAL
          || tok->get_id () == Tiger::REAL_LITERAL
          || tok->get_id () == Tiger::STRING_LITERAL;
 
      location_t loc = tok->get_locus ();
 
      fprintf (stderr, "<id=%s%s, %s, line=%d, col=%d>\n", tok->token_id_to_str (),
         has_text ? (std::string(", text=") + tok->get_str ()).c_str () : "",
         LOCATION_FILE (loc), LOCATION_LINE (loc), LOCATION_COLUMN (loc));
 
      if (tok->get_id() == Tiger::END_OF_FILE)
          break;
 
      lex.skip_token ();
      tok = lex.peek_token ();
    }
 
  fclose (file);
}
/*comentado no tiger-parser.cc e adicionado aqui */



static tree
tiger_langhook_type_for_mode (enum machine_mode mode, int unsignedp)
{
  if (mode == TYPE_MODE (float_type_node))
    return float_type_node;

  if (mode == TYPE_MODE (double_type_node))
    return double_type_node;

  if (mode == TYPE_MODE (intQI_type_node))
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;
  if (mode == TYPE_MODE (intHI_type_node))
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;
  if (mode == TYPE_MODE (intSI_type_node))
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;
  if (mode == TYPE_MODE (intDI_type_node))
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;
  if (mode == TYPE_MODE (intTI_type_node))
    return unsignedp ? unsigned_intTI_type_node : intTI_type_node;

  if (mode == TYPE_MODE (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;

  if (mode == TYPE_MODE (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;

  if (mode == TYPE_MODE (long_long_integer_type_node))
    return unsignedp ? long_long_unsigned_type_node
		     : long_long_integer_type_node;

  if (COMPLEX_MODE_P (mode))
    {
      if (mode == TYPE_MODE (complex_float_type_node))
	return complex_float_type_node;
      if (mode == TYPE_MODE (complex_double_type_node))
	return complex_double_type_node;
      if (mode == TYPE_MODE (complex_long_double_type_node))
	return complex_long_double_type_node;
      if (mode == TYPE_MODE (complex_integer_type_node) && !unsignedp)
	return complex_integer_type_node;
    }

  /* gcc_unreachable */
  return NULL;
}

static tree
tiger_langhook_type_for_size (unsigned int bits ATTRIBUTE_UNUSED,
			     int unsignedp ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
  return NULL;
}

/* Record a builtin function.  We just ignore builtin functions.  */

static tree
tiger_langhook_builtin_function (tree decl)
{
  return decl;
}

static bool
tiger_langhook_global_bindings_p (void)
{
  return false;
}

static tree
tiger_langhook_pushdecl (tree decl ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

static tree
tiger_langhook_getdecls (void)
{
  return NULL;
}

#undef LANG_HOOKS_NAME
#define LANG_HOOKS_NAME "Tiger"

#undef LANG_HOOKS_INIT
#define LANG_HOOKS_INIT tiger_langhook_init

#undef LANG_HOOKS_PARSE_FILE
#define LANG_HOOKS_PARSE_FILE tiger_langhook_parse_file

#undef LANG_HOOKS_TYPE_FOR_MODE
#define LANG_HOOKS_TYPE_FOR_MODE tiger_langhook_type_for_mode

#undef LANG_HOOKS_TYPE_FOR_SIZE
#define LANG_HOOKS_TYPE_FOR_SIZE tiger_langhook_type_for_size

#undef LANG_HOOKS_BUILTIN_FUNCTION
#define LANG_HOOKS_BUILTIN_FUNCTION tiger_langhook_builtin_function

#undef LANG_HOOKS_GLOBAL_BINDINGS_P
#define LANG_HOOKS_GLOBAL_BINDINGS_P tiger_langhook_global_bindings_p

#undef LANG_HOOKS_PUSHDECL
#define LANG_HOOKS_PUSHDECL tiger_langhook_pushdecl

#undef LANG_HOOKS_GETDECLS
#define LANG_HOOKS_GETDECLS tiger_langhook_getdecls

struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

#include "gt-tiger-tiger1.h"
#include "gtype-tiger.h"
