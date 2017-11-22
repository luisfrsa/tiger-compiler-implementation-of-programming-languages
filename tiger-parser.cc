/* Tiger parser
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

#include <iostream>
#include <memory>

#include <fstream> //alteracao_luis
#include <algorithm>//alteracao_luis
#include <sstream>
#include <string>
using namespace std; //alteracao_luis

#include "tiger/tiger-parser.h"
#include "tiger/tiger-lexer.h"
#include "tiger/tiger-tree.h"
#include "tiger/tiger-symbol.h"
#include "tiger/tiger-symbol-mapping.h"
#include "tiger/tiger-scope.h"

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "tree.h"
#include "tree-iterator.h"
#include "input.h"
#include "diagnostic.h"
#include "stringpool.h"
#include "cgraph.h"
#include "gimplify.h"
#include "gimple-expr.h"
#include "convert.h"
#include "print-tree.h"
#include "stor-layout.h"
#include "fold-const.h"


namespace
{
enum binding_powers
{
  // Highest priority
  LBP_HIGHEST = 100,

  LBP_DOT = 90,

  LBP_ARRAY_REF = 80,

  LBP_UNARY_PLUS = 50,  // Used only when the null denotation is +
  LBP_UNARY_MINUS = LBP_UNARY_PLUS, // Used only when the null denotation is -

  LBP_MUL = 40,
  LBP_DIV = LBP_MUL,
  LBP_MOD = LBP_MUL,

  LBP_PLUS = 30,
  LBP_MINUS = LBP_PLUS,

  LBP_EQUAL = 20,
  LBP_DIFFERENT = LBP_EQUAL,
  LBP_LOWER_THAN = LBP_EQUAL,
  LBP_LOWER_EQUAL = LBP_EQUAL,
  LBP_GREATER_THAN = LBP_EQUAL,
  LBP_GREATER_EQUAL = LBP_EQUAL,

  LBP_LOGICAL_AND = 10,
  LBP_LOGICAL_OR = LBP_LOGICAL_AND,
  LBP_LOGICAL_NOT = LBP_LOGICAL_AND,

  // Lowest priority
  LBP_LOWEST = 0,
};
}

namespace Tiger
{
struct Parser
{
private:
  void skip_after_semicolon ();
  void skip_after_end ();

  bool skip_token (TokenId);
  const_TokenPtr expect_token (TokenId);
  void unexpected_token (const_TokenPtr);

  // Expression parsing
  int left_binding_power (const_TokenPtr tok);
  Tree null_denotation (const_TokenPtr tok);
  Tree left_denotation (const_TokenPtr tok, Tree left);

  Tree parse_expression (int right_binding_power);

  Tree coerce_binary_arithmetic (const_TokenPtr tok, Tree *left, Tree *right);
  bool check_logical_operands (const_TokenPtr tok, Tree left, Tree right);

  Tree get_printf_addr ();
  Tree get_puts_addr ();

  Tree get_scanf_addr ();

  Tree build_label_decl (const char *name, location_t loc);
  Tree build_if_statement (Tree bool_expr, Tree then_part, Tree else_part);
  Tree build_while_statement (Tree bool_expr, Tree while_body);
  Tree build_for_statement (SymbolPtr ind_var, Tree lower_bound, Tree upper_bound,Tree for_body_stmt_list);

  const char *print_type (Tree type);

  TreeStmtList &get_current_stmt_list ();

  void enter_scope ();

  struct TreeSymbolMapping
  {
    Tree bind_expr;
    Tree block;
    /*Tiger*/
    Tree tipo_retorno;
    Tree retorno;
    /*Tiger*/
  };

  TreeSymbolMapping leave_scope ();

  SymbolPtr query_type (const std::string &name, location_t loc);
  SymbolPtr query_variable (const std::string &name, location_t loc);
  SymbolPtr query_integer_variable (const std::string &name, location_t loc);

  void parse_statement_seq (bool (Parser::*done) ());
  //Tree parse_statement_seq (bool (Parser::*done) ());

  bool done_end ();
  bool done_end_or_else ();
  bool done_end_of_file ();

  typedef Tree (Parser::*BinaryHandler) (const_TokenPtr, Tree);
  BinaryHandler get_binary_handler (TokenId id);

   /*Tiger*/
  void parse_let_declaration_seq (bool (Parser::*done) ());
  bool done_in ();
  Tree parse_let_statement();
  Tree parse_literal_statement();
  Tree parse_declaration_let();
  void skip_before_new_declaration();
  void skip_after_semicolon_or_other();


  /*Tiger*/

#define BINARY_HANDLER_LIST                                                    \
  BINARY_HANDLER (plus, PLUS)                                                  \
  BINARY_HANDLER (minus, MINUS)                                                \
  BINARY_HANDLER (mult, ASTERISK)                                              \
  BINARY_HANDLER (div, SLASH)                                                  \
  BINARY_HANDLER (mod, PERCENT)                                                \
                                                                               \
  BINARY_HANDLER (equal, EQUAL)                                                \
  BINARY_HANDLER (different, DIFFERENT)                                        \
  BINARY_HANDLER (lower_than, LOWER)                                           \
  BINARY_HANDLER (lower_equal, LOWER_OR_EQUAL)                                 \
  BINARY_HANDLER (greater_than, GREATER)                                       \
  BINARY_HANDLER (greater_equal, GREATER_OR_EQUAL)                             \
                                                                               \
  BINARY_HANDLER (logical_and, AND)                                            \
  BINARY_HANDLER (logical_or, OR)                                              \
                                                                               \
  BINARY_HANDLER (array_ref, LEFT_SQUARE)                                      \
                                                                               \
  BINARY_HANDLER (field_ref, DOT)

#define BINARY_HANDLER(name, _)                                                \
  Tree binary_##name (const_TokenPtr tok, Tree left);
  BINARY_HANDLER_LIST
#undef BINARY_HANDLER

public:
  Parser (Lexer &lexer_) : lexer (lexer_), puts_fn (), printf_fn (), scanf_fn ()
  {
  }

  void parse_program ();

  Tree parse_statement ();

  Tree parse_variable_declaration ();
  Tree parse_type_declaration ();

  Tree parse_type ();
  Tree parse_record ();
  Tree parse_field_declaration (std::vector<std::string> &field_names);

  Tree parse_assignment_statement (Tree variable);
  Tree parse_if_statement ();
  Tree parse_if_statement (int i);
  Tree parse_while_statement ();
  Tree parse_for_statement ();
  Tree parse_read_statement ();
  Tree parse_write_statement ();

  Tree parse_expression ();
  Tree parse_expression_naming_variable();
  Tree parse_lhs_assignment_expression();
  Tree parse_boolean_expression ();
  Tree parse_integer_expression ();

  /*Tiger*/
  Tree parse_statement (const_TokenPtr t);
  /*Tiger*/
private:
  Lexer &lexer;
  Scope scope;

  tree main_fndecl;

  Tree puts_fn;
  Tree printf_fn;
  Tree scanf_fn;

  std::vector<TreeStmtList> stack_stmt_list;
  std::vector<TreeChain> stack_var_decl_chain;

  std::vector<BlockChain> stack_block_chain;
};

void
Parser::skip_after_semicolon ()
{
  const_TokenPtr t = lexer.peek_token ();
  while (t->get_id () != Tiger::END_OF_FILE &&
         t->get_id () != Tiger::SEMICOLON){
      lexer.skip_token ();
      t = lexer.peek_token ();
  }
  if (t->get_id () == Tiger::SEMICOLON)
    lexer.skip_token ();
}
void
Parser::skip_after_semicolon_or_other ()
{
  const_TokenPtr t = lexer.peek_token ();
  //printf("skip_after_semicolon_or_other \n");

  while (t->get_id () != Tiger::END_OF_FILE &&
         t->get_id () != Tiger::SEMICOLON &&
         t->get_id () != Tiger::IN &&
         t->get_id () != Tiger::TYPE &&
         t->get_id () != Tiger::VAR &&
         t->get_id () != Tiger::FUNCTION){
      lexer.skip_token ();
      t = lexer.peek_token ();
    //  printf("tkn _or_other %s \n", t->get_token_description ());

  }
  if (t->get_id () == Tiger::SEMICOLON)
    lexer.skip_token ();

}
void
Parser::skip_before_new_declaration ()
{
  const_TokenPtr t = lexer.peek_token ();
  //printf("skip_before_new_declaration \n");
  while (
    t->get_id () != Tiger::END_OF_FILE &&
    t->get_id () != Tiger::IN &&
    t->get_id () != Tiger::TYPE &&
    t->get_id () != Tiger::FUNCTION && 
    t->get_id () != Tiger::VAR)
    {
     // printf("tkn new decl %s \n", t->get_token_description ());

      lexer.skip_token ();
      t = lexer.peek_token ();
  }
  //printf("tkn new decl %s \n", t->get_token_description ());


  /*if (t->get_id () != Tiger::END_OF_FILE)
    lexer.skip_token ();*/
}

void
Parser::skip_after_end ()
{
  const_TokenPtr t = lexer.peek_token ();

  while (t->get_id () != Tiger::END_OF_FILE && t->get_id () != Tiger::END)
    {
      lexer.skip_token ();
      t = lexer.peek_token ();
    }

  if (t->get_id () == Tiger::END)
    lexer.skip_token ();
}
/*
===(5)===
Function expect_token checks the current token.
If its id is the same as the one we expect, 
it skips and returns it, otherwise it diagnoses 
an error and returns an empty pointer (i.e. a null pointer).
*/
const_TokenPtr
Parser::expect_token (Tiger::TokenId token_id)
{
  const_TokenPtr t = lexer.peek_token ();
  if (t->get_id () == token_id){
      lexer.skip_token ();
      return t;
  }else{
      error_at (t->get_locus (), "expecting %s but %s found\n",get_token_description (token_id), t->get_token_description ());
      return const_TokenPtr ();
  }
}
/*
===(4)===
Here we use a function skip_token that given a token id, 
checks if the current token has that same id. If it has, 
it just skips it and returns true. Otherwise diagnoses an 
error and returns false. When skip_token fails (i.e. returns 
false) we immediately go to panic mode and give up parsing 
the current statement.  No wonder there exist tools, 
like ANTLR by Terence Parr, that automate the code generation 
of recursive descent recognizers.
Function skip_token simply forwards to expect_token.
*/
bool
Parser::skip_token (Tiger::TokenId token_id)
{
  return expect_token (token_id) != const_TokenPtr();
}

void
Parser::unexpected_token (const_TokenPtr t)
{
  ::error_at (t->get_locus (), "unexpected %s\n", t->get_token_description ());
}

void
Parser::parse_program ()
{
  printf("parse_program\n");

  // Built type of main "int (int, char**)"
  tree main_fndecl_type_param[] = {integer_type_node,/* int */
                                   build_pointer_type (build_pointer_type (char_type_node)) /* char** */  
                                  };
  tree main_fndecl_type = build_function_type_array (integer_type_node, 2, main_fndecl_type_param);
  // Create function declaration "int main(int, char**)"
  main_fndecl = build_fn_decl ("main", main_fndecl_type);

  // Enter top level scope
  enter_scope ();
  // program -> statement*
  parse_statement_seq (&Parser::done_end_of_file);
  // Append "return 0;"
  tree resdecl = build_decl (UNKNOWN_LOCATION, RESULT_DECL, NULL_TREE, integer_type_node);
  DECL_CONTEXT (resdecl) = main_fndecl;
  DECL_RESULT (main_fndecl) = resdecl;
  tree set_result = build2 (INIT_EXPR, void_type_node, DECL_RESULT (main_fndecl),build_int_cst_type (integer_type_node, 0));
  tree return_stmt = build1 (RETURN_EXPR, void_type_node, set_result);

  get_current_stmt_list ().append (return_stmt);

  // Leave top level scope, get its binding expression and its main block
  TreeSymbolMapping main_tree_scope = leave_scope ();
  Tree main_block = main_tree_scope.block;

  // Finish main function
  BLOCK_SUPERCONTEXT (main_block.get_tree ()) = main_fndecl;
  DECL_INITIAL (main_fndecl) = main_block.get_tree ();
  DECL_SAVED_TREE (main_fndecl) = main_tree_scope.bind_expr.get_tree ();

  DECL_EXTERNAL (main_fndecl) = 0;
  DECL_PRESERVE_P (main_fndecl) = 1;

  // Convert from GENERIC to GIMPLE
  gimplify_function_tree (main_fndecl);

  // Insert it into the graph
  cgraph_node::finalize_function (main_fndecl, true);

  main_fndecl = NULL_TREE;
}

bool
Parser::done_end_of_file ()
{
  const_TokenPtr t = lexer.peek_token ();
  return (t->get_id () == Tiger::END_OF_FILE);
}

bool
Parser::done_end ()
{
  const_TokenPtr t = lexer.peek_token ();
  return (t->get_id () == Tiger::END || t->get_id () == Tiger::END_OF_FILE);
}

bool
Parser::done_end_or_else ()
{
  const_TokenPtr t = lexer.peek_token ();
  return (t->get_id () == Tiger::END || t->get_id () == Tiger::ELSE || t->get_id () == Tiger::END_OF_FILE);
}
/*Tiger*/
bool
Parser::done_in ()
{
  const_TokenPtr t = lexer.peek_token ();
  return (t->get_id () == Tiger::IN || t->get_id () == Tiger::END_OF_FILE);
}
/*Tiger*/

/*
===(1)===
This is fine but if you check the syntax of tiny, you will see that the condition 
of finalization of a 〈statement-seq〉 is not always the end of file. Sometimes
can be end (in the then or else part of an if statement, int the body for statement
and in the body of a while statement) and sometimes is else (in the then part 
of an if statement). So this means that parse_statement_seq can be reused if
we parameterize the finalization condition. Something like this.
*done pode ser done_end_of_file, etc
*/
void
Parser::parse_statement_seq (bool (Parser::*done) ())
{
  printf("parse_statement_seq\n");

  // Parse statements until done and append to the current stmt list;
  while (!(this->*done) ())
    {
      //Tree stmt = parse_statement ();
      Tree stmt = parse_expression ();
      //printf("STMT de parse_statement do tipo %s stmt \n",print_type (stmt.get_type ()));
      get_current_stmt_list ().append (stmt);
      get_current_stmt_list ().set_tipo(stmt.get_type ());
    }
    printf("Ultimo current_stmt_list do tipo %s \n ",print_type (get_current_stmt_list ().get_tipo ()));
}
void
Parser::parse_let_declaration_seq (bool (Parser::*done) ())
{
  while (!(this->*done) ())
    {
      Tree stmt = parse_declaration_let ();
      get_current_stmt_list ().append (stmt);
    }
}

void
Parser::enter_scope ()
{
  scope.push_scope ();

  TreeStmtList stmt_list;
  stack_stmt_list.push_back (stmt_list);

  stack_var_decl_chain.push_back (TreeChain ());
  stack_block_chain.push_back (BlockChain ());
}

Parser::TreeSymbolMapping
Parser::leave_scope ()
{
  TreeStmtList current_stmt_list = get_current_stmt_list ();
  stack_stmt_list.pop_back ();

  TreeChain var_decl_chain = stack_var_decl_chain.back ();
  stack_var_decl_chain.pop_back ();

  BlockChain subblocks = stack_block_chain.back ();
  stack_block_chain.pop_back ();

  tree new_block = build_block (var_decl_chain.first.get_tree (), subblocks.first.get_tree (), /* supercontext */ NULL_TREE, /* chain */ NULL_TREE);

  // Add the new block to the current chain of blocks (if any)
  if (!stack_block_chain.empty ())
    {
      stack_block_chain.back ().append (new_block);
    }

  // Set the subblocks to have the new block as their parent
  for (tree it = subblocks.first.get_tree (); it != NULL_TREE;
       it = BLOCK_CHAIN (it))
    BLOCK_SUPERCONTEXT (it) = new_block;

  tree bind_expr = build3 (BIND_EXPR, void_type_node, var_decl_chain.first.get_tree (), current_stmt_list.get_tree (), new_block);

  TreeSymbolMapping tree_scope;
  tree_scope.bind_expr = bind_expr;
  tree_scope.block = new_block;

  scope.pop_scope();

  return tree_scope;
}

TreeStmtList &
Parser::get_current_stmt_list ()
{
  return stack_stmt_list.back ();
}


/*
===(2)===
We peek the current token and we check which 
statement it can initiate. If no statement can 
be initiated given the current token, the we 
call a diagnostic function with the unexpected 
token. We do some minimal error recovery by skiping 
all tokens until a semicolon is found.
*/
Tree
Parser::parse_statement ()
{
  const_TokenPtr t = lexer.peek_token ();

  return parse_statement (t);
}
  Tree
Parser::parse_statement (const_TokenPtr t)
{

  /*
    statement ->  variable_declaration
	   |  assignment_statement
	   |  if_statement
	   |  while_statement
	   |  for_statement
	   |  read_statement
	   |  write_statement
	   */
  printf("parse_statement\n");
  printf("---tkn  %s\n", t->get_token_description ());
  switch (t->get_id ())
    {
   /* case Tiger::VAR:
      return parse_variable_declaration ();
      break;
    case Tiger::TYPE:
      return parse_type_declaration ();
      break;
      */
    case Tiger::IF:
      return parse_if_statement ();
      break;
    case Tiger::WHILE:
      return parse_while_statement ();
      break;
    case Tiger::FOR:
      return parse_for_statement ();
      break;
    case Tiger::READ:
      return parse_read_statement ();
      break;
    case Tiger::WRITE:
      return parse_write_statement ();
      break;
      /*
    case Tiger::IDENTIFIER:
      return parse_assignment_statement ();
      break;
      */
    /*Tiger*/
    case Tiger::LET:
      return parse_let_statement ();
      break;
      /*
  assignment e literal devem retornar valores
        */
    case Tiger::INTEGER_LITERAL:
    case Tiger::STRING_LITERAL:
    case Tiger::FLOAT_LITERAL:
    case Tiger::LEFT_PAREN:
      return parse_literal_statement ();
      break;
  
    /*Tiger*/
    default:
      unexpected_token (t);
      skip_after_semicolon_or_other ();
      return Tree::error ();
      break;
      /*      
      //return null_denotation(t);
      return parse_expression();
      break;
      */
    }

  gcc_unreachable ();
}
Tree
Parser::parse_declaration_let ()
{
 printf("parse_declaration_let\n");
 const_TokenPtr t = lexer.peek_token ();
 switch (t->get_id ())
    {
    case Tiger::VAR:
      return parse_variable_declaration ();
      break;
    case Tiger::TYPE:
      return parse_type_declaration ();
      break;
    case Tiger::FUNCTION:
      /*return parse_type_declaration ();*/
      break;
    default:
      unexpected_token (t);
      skip_before_new_declaration ();
      return Tree::error ();
      break;
    }
  gcc_unreachable ();

}
/*
===(6)===
A Pratt parser defines the concept of binding power as some sort of priority number: the 
higher the binding power the more priority the operand has. This parser associates three 
extra values to the tokens of expressions: a left binding power, a null denotation 
function and a left denotation function.

Parsing an expression requires a right binding power. A top level expression 
will use the lowest priority possible. Then the parser starts by peeking the 
current token t1 and skipping it. Then it invokes the null denotation function 
of t1. If this token cannot appear at this point then its null denotation 
function will diagnose an error and the parsing will end at this point. 
Otherwise the null denotation function will do something (that may include 
advancing the token stream, more on this later). Once we are back from 
the null denotation, the parser checks if the current right binding power 
is lower or than that of the current token (call it t2, but note that 
it may not be the next one after t1). If it is not, parsing ends here. 
Otherwise the parser skips the token and the left denotation function is 
invoked on t2. The left denotation function (will do something, including 
advancing the current token, more on this later). Once we are back from the 
left denotation we will check again if the current token has a higher left 
binding power than the current right binding power and proceed likewise.

Ok, I tried, but the explanation above is rather dense. Behold the stunning 
simplicity of this parser at its core.
*/
// This is a Pratt parser
Tree
Parser::parse_expression (int right_binding_power)
{
  printf("parse_expression\n");

  const_TokenPtr current_token = lexer.peek_token ();
  lexer.skip_token ();

  Tree expr = null_denotation (current_token);

  if (expr.is_error ())
    return Tree::error ();

  while (right_binding_power < left_binding_power (lexer.peek_token ())){
      current_token = lexer.peek_token();
      lexer.skip_token ();

      expr = left_denotation (current_token, expr);
      if (expr.is_error ())
	     return Tree::error ();
  }
  printf("FIM EXP, TIPO   %s \n ",print_type (expr.get_type ()));

  return expr;
}

/*
===(7)===
There is little to do now for identifiers, real, integer and string literals. 
So they trivially return true (lines 6 to 10).

If the current token is ( (line 11) it means that we have to parse a whole 
expression. So we do by recursively invoking parse_expression (with the lowest 
priority possible, as if it were a top-level expression). When we return from 
parse_expression we have to make sure that the current token is ) (line 16).

If the current token is +, – or not (lines 18, 24, 30) it means that this is 
a unary operator. We will invoke parse_expression recursively with the 
appropiate priority for each operand (LBP_UNARY_PLUS, LBP_UNARY_NEG, LBP_LOGICAL_NOT, more on this later).

It may not be obvious now, but tok, is not the current token in the input 
stream but the previous one since parse_expression already skipped tok before calling null_denotation.
*/
// This is invoked when a token (including prefix operands) is found at a
// "prefix" position
Tree
Parser::null_denotation(const_TokenPtr tok) {
  /*
  Note that using Tree rather than the GENERIC tree is essential for primaries. 
  In the code above s->get_tree_decl() returns a tree with the location of the 
  variable declaration. We could use this tree but for diagnostics purposes we 
  want the location where the variable is being referenced.

  For literals, the literal itself encodes the value. So the text of the token 
  will have to be interpreted as the appropiate value. For integers we can just use atoi.
  */
  printf("null_denotation\n");

  switch (tok->get_id()) {
  case Tiger::IDENTIFIER:{
      SymbolPtr s = query_variable(tok->get_str(), tok->get_locus());
      if(lexer.peek_token()->get_id () == Tiger::ASSIG){
        return parse_assignment_statement (Tree(s->get_tree_decl(), tok->get_locus()));
      }else if (s == NULL){
        return Tree::error();
      }else{
        return Tree(s->get_tree_decl(), tok->get_locus());
      }
  }
  case Tiger::INTEGER_LITERAL:
    // FIXME : check ranges
    return Tree(build_int_cst_type(integer_type_node, atoi(tok->get_str().c_str())),
      tok->get_locus());
    break;
    /*
  For a real literal we have to invoke the (GCC-provided) function real_from_string3 (line 27) 
  to get a real value representation from a string. This function expects the machine (i.e. architecture dependent)
  mode of the type, that we can obtain using TYPE_MODE. It returns its value in a REAL_VALUE_TYPE that then can 
  be used to build a real constant tree using the (GCC-provided) function build_real.
      */
  case Tiger::REAL_LITERAL:
    {
      REAL_VALUE_TYPE real_value;
      real_from_string3( & real_value,tok->get_str().c_str(),TYPE_MODE(float_type_node));
      return Tree(build_real(float_type_node, real_value),tok->get_locus());
    }
    break;
  /*
  To create a string literal we use the (GCC-provided) function build_string_literal. 
  For practical reasons our string literal will contain the NULL terminator, otherwise 
  the string literal itself will not be useable in C functions (more on this later).

  While the type GENERIC trees created for integer and real literals was obviously 
  integer_type_node and float_type_node, it is not so clear for string literals. 
  The tree created by build_string_literal has type pointer to a character type. 
  Pointer types have a tree code of POINTER_TYPE and the pointee type is found in TREE_TYPE.
  Sometimes we will need to check if an expression has the type of a string literal, 
  so we will use the following auxiliar function.
      */
  case Tiger::STRING_LITERAL:
    {
      std::string str = tok->get_str();
      const char * c_str = str.c_str();
      return Tree(build_string_literal(::strlen(c_str) + 1, c_str),tok->get_locus());
    }
    break;
  case Tiger::TRUE_LITERAL:
    {
      return Tree(build_int_cst_type(boolean_type_node, 1),tok->get_locus());
    }
    break;
  case Tiger::FALSE_LITERAL:
    {
      return Tree(build_int_cst_type(boolean_type_node, 0),tok->get_locus());
    }
    break;
  case Tiger::LEFT_PAREN:
    {
      Tree expr = parse_expression();
      tok = lexer.peek_token();
      if (tok->get_id() != Tiger::RIGHT_PAREN)
        error_at(tok->get_locus(), "expecting ) but %s found\n",tok->get_token_description());
      else
        lexer.skip_token();
      return Tree(expr, tok->get_locus());
    }
  case Tiger::PLUS:
    {
      Tree expr = parse_expression(LBP_UNARY_PLUS);
      if (expr.is_error())
        return Tree::error();
      if (expr.get_type() != integer_type_node || expr.get_type() != float_type_node) {
        error_at(tok->get_locus(),"operand of unary plus must be int or float but it is %s",print_type(expr.get_type()));
        return Tree::error();
      }
      return Tree(expr, tok->get_locus());
    }
  case Tiger::MINUS:
    {
      Tree expr = parse_expression(LBP_UNARY_MINUS);
      if (expr.is_error())
        return Tree::error();

      if (expr.get_type() != integer_type_node || expr.get_type() != float_type_node) {
        error_at(tok->get_locus(),"operand of unary minus must be int or float but it is %s",print_type(expr.get_type()));
        return Tree::error();
      }

      expr = build_tree(NEGATE_EXPR, tok->get_locus(), expr.get_type(), expr);
      return expr;
    }
  case Tiger::NOT:
    {
      Tree expr = parse_expression(LBP_LOGICAL_NOT);
      if (expr.is_error())
        return Tree::error();

      if (expr.get_type() != boolean_type_node) {
        error_at(tok->get_locus(),"operand of logical not must be a boolean but it is %s",print_type(expr.get_type()));
        return Tree::error();
      }

      expr = build_tree(TRUTH_NOT_EXPR, tok->get_locus(), boolean_type_node, expr);
      return expr;
    }
    //Tiger
    /*
  case Tiger::IF:
    {
      //Tree exp_if = Tree(parse_if_statement(1), tok->get_locus());
      Tree exp_if = parse_if_statement(1);
      printf("EXP IF TIPO   %s \n ", print_type(exp_if.get_type()));
      return exp_if;
      //return Tree (parse_if_statement (1),tok->get_locus ());
    }
    */
    //Tiger
  default:
    return parse_statement(tok);
  /*
    unexpected_token(tok);
    skip_after_semicolon_or_other();
    return Tree::error();
    */
  }
}
/*
We first parse the syntactic elements of a variable declaration. We skip the initial var in lines 4 to 8. 
In line 10 we keep the identifier token because it will be used later. We skip the colon in lines 17 to 21. 
In line 23 we parse the type (by calling parse_type, more on this later) and finally in line 31 we skip the semicolon.

Now the semantic checks of a variable declaration can start. In line 33, we check if the current mapping 
of the scope already contains a mapping for the identifier. If there is such a mapping, this is an error 
and we give up, otherwise we create a new symbol (line 39) using the given identifier and we insert it 
into the current mapping (line 40).

Now we need to create some GENERIC for this new variable declaration (line 43). It will have a tree 
code of VAR_DECL. The first operand of that tree is an IDENTIFIER_NODE for the identifier itself. 
These trees are shared in GENERIC: two identical identifiers will use the same tree. For this reason 
we need to request an IDENTIFIER_NODE rather than creating it manually. We do that calling the 
(GCC-provided) function get_identifier (line 44). The second operand that we will need is the type 
of the declaration. This was obtained in an earlier call to parse_type. Note that we are calling 
the (GCC-provided) function build_decl. This is so because there is an extra step (setting some 
internal type and operation mode of the declaration) that has to be performed for a VAR_DECL. 
Function build_decl takes care of that for us and it is in practice like calling build2_loc.

In line 50 we associate the new Symbol with the VAR_DECL we have created. We do this because every 
time we need to refer to an existing variable in GENERIC we will need to use a VAR_DECL. But it cannot 
be a new VAR_DECL every time since this would mean a new variable with the same name. So we just 
keep a single VAR_DECL in a Symbol so we can reuse it as many times as needed.

The VAR_DECL is also kept in the top list of the stack stack_var_decl_chain. We will need this 
later when we talk about blocks.
  TIGER
  var a := 1;
  var a :int :=nil;
  var a :int :=1;
  TINY
  var n : int;
*/
Tree
Parser::parse_variable_declaration (){
 printf("parse_variable_declaration\n");

  Tree type_tree;
  Tree expr;
  const_TokenPtr first_of_expr;
  const_TokenPtr assig_tok;
  // variable_declaration -> "var" identifier ":" type ";"
  if (!skip_token (Tiger::VAR))
    {
      skip_before_new_declaration ();
      return Tree::error ();
    }
  const_TokenPtr identifier = expect_token (Tiger::IDENTIFIER);
  if (identifier == NULL){
      skip_before_new_declaration ();
      return Tree::error ();
  }

  const_TokenPtr tok = lexer.peek_token ();
  if (tok->get_id () == Tiger::COLON){
    skip_token (Tiger::COLON);
    type_tree = parse_type (); 
    if (type_tree.is_error ()){
      skip_before_new_declaration();
      return Tree::error ();
    }    
    assig_tok = expect_token (Tiger::ASSIG);
    if (assig_tok == NULL){//var a :int:=
        skip_before_new_declaration ();
        return Tree::error ();
    }
    first_of_expr = lexer.peek_token ();
    expr = parse_expression ();
    if (expr.is_error ())
      return Tree::error ();

  }else{
    assig_tok = expect_token (Tiger::ASSIG);
    if (assig_tok == NULL){
        skip_before_new_declaration ();
        return Tree::error ();
    }   
    if (type_tree.is_error ()){
      skip_before_new_declaration();
      return Tree::error ();
    }    
    first_of_expr = lexer.peek_token ();
    expr = parse_expression ();
    if (expr.is_error ())
      return Tree::error ();
  //fiz aqui, gettype para var a :=1;
  	type_tree = expr.get_type(); 
    
  }

  //skip_token (Tiger::SEMICOLON);

  if (scope.get_current_mapping ().get (identifier->get_str ())){
      error_at (identifier->get_locus (),"name '%s' already declared in this scope",identifier->get_str ().c_str ());
  }
  SymbolPtr sym (new Symbol (Tiger::VARIABLE, identifier->get_str ()));
  scope.get_current_mapping ().insert (sym);

  Tree decl = build_decl (identifier->get_locus (),
                          VAR_DECL,
			                    get_identifier (sym->get_name ().c_str ()),
			                    type_tree.get_tree ());

  DECL_CONTEXT (decl.get_tree()) = main_fndecl;

  gcc_assert (!stack_var_decl_chain.empty ());
  stack_var_decl_chain.back ().append (decl);

  sym->set_tree_decl (decl);
  
  if (decl.get_type () != expr.get_type ()){
      error_at (first_of_expr->get_locus (),"cannot assign value of type %s to variable '%s' of type %s",                print_type (expr.get_type ()),                sym->get_name ().c_str (),                print_type (decl.get_type ()));
      return Tree::error ();
 }

  //Tree stmt = build_tree (DECL_EXPR, identifier->get_locus (), void_type_node, decl);

  /*nao estava dando, sive que recuperar o simbolo novamente*/
  sym = query_variable (identifier->get_str (), identifier->get_locus ());

  gcc_assert (!sym->get_tree_decl ().is_null ());

  Tree var_decl = sym->get_tree_decl ();

  Tree assig_expr = build_tree (MODIFY_EXPR, assig_tok->get_locus (), void_type_node, var_decl, expr);

  return assig_expr;
}



Tree
Parser::parse_type_declaration ()
{
  // type_declaration -> "type" identifier ":" type ";"
  if (!skip_token (Tiger::TYPE))
    {
      skip_after_semicolon_or_other ();
      return Tree::error ();
    }

  const_TokenPtr identifier = expect_token (Tiger::IDENTIFIER);
  if (identifier == NULL)
    {
      skip_after_semicolon_or_other ();
      return Tree::error ();
    }

  if (!skip_token (Tiger::COLON))
    {
      skip_after_semicolon_or_other ();
      return Tree::error ();
    }

  Tree type_tree = parse_type ();

  if (type_tree.is_error ())
    {
      skip_after_semicolon_or_other();
      return Tree::error ();
    }

  skip_token (Tiger::SEMICOLON);

  if (scope.get_current_mapping ().get (identifier->get_str ()))
  {
    error_at (identifier->get_locus (),
	            "name '%s' already declared in this scope",
	            identifier->get_str ().c_str ());
  }
  SymbolPtr sym (new Symbol (Tiger::TYPENAME, identifier->get_str ()));
  scope.get_current_mapping ().insert (sym);

  Tree decl = build_decl (identifier->get_locus (), TYPE_DECL,
                  			  get_identifier (sym->get_name ().c_str ()),
                  			  type_tree.get_tree ());
  DECL_CONTEXT (decl.get_tree()) = main_fndecl;

  gcc_assert (!stack_var_decl_chain.empty ());
  stack_var_decl_chain.back ().append (decl);

  sym->set_tree_decl (decl);

  Tree stmt= build_tree (DECL_EXPR, identifier->get_locus (), void_type_node, decl);

  return stmt;
}

namespace
{

bool
is_string_type (Tree type)
{
  gcc_assert (TYPE_P (type.get_tree ()));
  return type.get_tree_code () == POINTER_TYPE && TYPE_MAIN_VARIANT (TREE_TYPE (type.get_tree ())) == char_type_node;
}

bool
is_array_type (Tree type)
{
  gcc_assert (TYPE_P (type.get_tree ()));
  return type.get_tree_code () == ARRAY_TYPE;
}

bool
is_record_type (Tree type)
{
  gcc_assert (TYPE_P (type.get_tree ()));
  return type.get_tree_code () == RECORD_TYPE;
}

}

const char *
Parser::print_type (Tree type)
{
  gcc_assert (TYPE_P (type.get_tree ()));

  if (type == void_type_node)
    {
      return "void";
    }
  else if (type == integer_type_node)
    {
      return "int";
    }
  else if (type == float_type_node)
    {
      return "float";
    }
  else if (is_string_type (type))
    {
      return "string";
    }
  else if (is_array_type(type))
    {
      return "array";
    }
  else if (type == boolean_type_node)
    {
      return "boolean";
    }
  else
    {
      return "<<unknown-type>>";
    }
}

Tree
Parser::parse_field_declaration (std::vector<std::string> &field_names)
{
  // identifier ':' type ';'
  const_TokenPtr identifier = expect_token (Tiger::IDENTIFIER);
  if (identifier == NULL)
    {
      skip_after_semicolon_or_other ();
      return Tree::error ();
    }

  skip_token (Tiger::COLON);

  Tree type = parse_type();

  skip_token (Tiger::SEMICOLON);

  if (type.is_error ())
    return Tree::error ();

  if (std::find (field_names.begin (), field_names.end (),identifier->get_str ()) != field_names.end ())
    {
      error_at (identifier->get_locus (), "repeated field name");
      return Tree::error ();
    }
  field_names.push_back (identifier->get_str ());

  Tree field_decl = build_decl (identifier->get_locus (), FIELD_DECL,
                          		  get_identifier (identifier->get_str ().c_str ()),
                          		  type.get_tree());
  TREE_ADDRESSABLE (field_decl.get_tree ()) = 1;

  return field_decl;
}

Tree
Parser::parse_record ()
{
  // "record" field-decl* "end"
  const_TokenPtr record_tok = expect_token (Tiger::RECORD);
  if (record_tok == NULL)
    {
      skip_after_semicolon_or_other ();
      return Tree::error ();
    }

  Tree record_type = make_node(RECORD_TYPE);
  Tree field_list, field_last;
  std::vector<std::string> field_names;

  const_TokenPtr next = lexer.peek_token ();
  while (next->get_id () != Tiger::END) {
      Tree field_decl = parse_field_declaration (field_names);
      if (!field_decl.is_error ()){
    	  DECL_CONTEXT (field_decl.get_tree ()) = record_type.get_tree();
    	  if (field_list.is_null ())
    	    field_list = field_decl;
    	  if (!field_last.is_null ())
    	    TREE_CHAIN (field_last.get_tree ()) = field_decl.get_tree ();
    	  field_last = field_decl;
    	}
      next = lexer.peek_token ();
  }

  skip_token (Tiger::END);

  TYPE_FIELDS (record_type.get_tree ()) = field_list.get_tree();
  layout_type (record_type.get_tree ());

  return record_type;
}

Tree
Parser::parse_type ()
{
  // type -> "int"
  //      | "float"
  //      | "bool"
  //      | IDENTIFIER
  //      | type '[' expr ']'
  //      | type '(' expr : expr ')'
  //      | "record" field-decl* "end"

  const_TokenPtr t = lexer.peek_token ();

  Tree type;
  switch (t->get_id ())
    {
    case Tiger::INT:
      lexer.skip_token ();
      type = integer_type_node;
      break;
    case Tiger::FLOAT:
      lexer.skip_token ();
      type = float_type_node;
      break;
    case Tiger::BOOL:
      lexer.skip_token ();
      type = boolean_type_node;
      break;
    case Tiger::IDENTIFIER:
      {
  	    SymbolPtr s = query_type (t->get_str (), t->get_locus ());
        lexer.skip_token ();
	      if (s == NULL)
	         type = Tree::error ();
        else
           type = TREE_TYPE (s->get_tree_decl ().get_tree ());
      }
      break;
    case Tiger::RECORD:
      type = parse_record ();
      break;
    default:
      unexpected_token (t);
      return Tree::error ();
      break;
    }

  typedef std::vector<std::pair<Tree, Tree> > Dimensions;
  Dimensions dimensions;

  t = lexer.peek_token ();
  while (t->get_id () == Tiger::LEFT_PAREN || t->get_id () == Tiger::LEFT_SQUARE){
      lexer.skip_token ();

      Tree lower_bound, upper_bound;
      if (t->get_id () == Tiger::LEFT_SQUARE){
    	  Tree size = parse_integer_expression ();
    	  skip_token (Tiger::RIGHT_SQUARE);

    	  lower_bound = Tree (build_int_cst_type (integer_type_node, 0),
    			                  size.get_locus ());
    	  upper_bound = build_tree (MINUS_EXPR, size.get_locus (), 
                                  integer_type_node,
    			                        size, 
                                  build_int_cst (integer_type_node, 1));

    	}else if (t->get_id () == Tiger::LEFT_PAREN){
    	  lower_bound = parse_integer_expression ();
    	  skip_token (Tiger::COLON);

    	  upper_bound = parse_integer_expression ();
    	  skip_token (Tiger::RIGHT_PAREN);
    	}else{
    	  gcc_unreachable ();
    	}

      dimensions.push_back (std::make_pair (lower_bound, upper_bound));
      t = lexer.peek_token ();
  }

  for (Dimensions::reverse_iterator it = dimensions.rbegin ();it != dimensions.rend (); it++) {
      it->first = Tree (fold (it->first.get_tree ()), it->first.get_locus ());
      it->second = Tree (fold (it->second.get_tree ()), it->second.get_locus ());

    if (!type.is_error ()){
  	  Tree range_type = build_range_type (integer_type_node, 
                                          it->first.get_tree (),
  				                                it->second.get_tree ());
  	  type = build_array_type (type.get_tree (), range_type.get_tree ());
	   }
  }

  return type;
}

SymbolPtr
Parser::query_type (const std::string &name, location_t loc)
{
  SymbolPtr sym = scope.lookup (name);
  if (sym == NULL)
    {
      error_at (loc, "type '%s' not declared in the current scope",name.c_str ());
    }
  else if (sym->get_kind () != Tiger::TYPENAME)
    {
      error_at (loc, "name '%s' is not a type", name.c_str ());
      sym = SymbolPtr();
    }
  return sym;
}

SymbolPtr
Parser::query_variable (const std::string &name, location_t loc)
{
  SymbolPtr sym = scope.lookup (name);
  if (sym == NULL)
    {
      error_at (loc, "variable '%s' not declared in the current scope",name.c_str ());
    }
  else if (sym->get_kind () != Tiger::VARIABLE)
    {
      error_at (loc, "name '%s' is not a variable", name.c_str ());
      sym = SymbolPtr();
    }
  return sym;
}

SymbolPtr
Parser::query_integer_variable (const std::string &name, location_t loc)
{
  SymbolPtr sym = query_variable (name, loc);
  if (sym != NULL)
    {
      Tree var_decl = sym->get_tree_decl ();
      gcc_assert (!var_decl.is_null ());

      if (var_decl.get_type () != integer_type_node)
	{
	  error_at (loc, "variable '%s' does not have integer type",name.c_str ());
	  sym = SymbolPtr();
	}
    }

  return sym;
}

Tree
Parser::parse_assignment_statement (Tree variable){
  // assignment_statement -> expression ":=" expression ";"
  //Tree variable = parse_lhs_assignment_expression ();
  if (variable.is_error ())
    return Tree::error ();
  const_TokenPtr assig_tok = expect_token (Tiger::ASSIG);
  if (assig_tok == NULL){
      skip_after_semicolon_or_other ();
      return Tree::error ();
  }

  const_TokenPtr first_of_expr = lexer.peek_token ();

  Tree expr = parse_expression ();

  printf("parse_assignment_statement IF?   %s \n ",print_type (expr.get_type ()));

  if (expr.is_error ())
    return Tree::error ();

  skip_token (Tiger::SEMICOLON);

  if (variable.get_type () != expr.get_type ()){error_at (first_of_expr->get_locus (),		"cannot assign value of type %s to a variable of type %s",		print_type (expr.get_type ()),		print_type (variable.get_type ()));
    return Tree::error ();
  }
    /*
    Tree assig_expr = build_tree (MODIFY_EXPR, assig_tok->get_locus (),void_type_node, variable, expr);
    retornando o tipo da atribuição, antes era void_type_node
    */
  Tree assig_expr = build_tree (MODIFY_EXPR, assig_tok->get_locus (),expr.get_type (), variable, expr);

  return assig_expr;
}

Tree
Parser::build_label_decl (const char *name, location_t loc)
{
  tree t = build_decl (loc, LABEL_DECL, get_identifier (name), void_type_node);

  gcc_assert (main_fndecl != NULL_TREE);
  DECL_CONTEXT (t) = main_fndecl;

  return t;
}

Tree
Parser::build_if_statement (Tree bool_expr, Tree then_part, Tree else_part)
{
  if (bool_expr.is_error ())
    return Tree::error ();

  Tree then_label_decl = build_label_decl ("then", then_part.get_locus ());

  Tree else_label_decl;
  if (!else_part.is_null ())
    else_label_decl = build_label_decl ("else", else_part.get_locus ());

  Tree endif_label_decl = build_label_decl ("end_if", then_part.get_locus ());

  Tree goto_then = build_tree (GOTO_EXPR, bool_expr.get_locus (), void_type_node, then_label_decl);
  Tree goto_endif = build_tree (GOTO_EXPR, bool_expr.get_locus (),void_type_node, endif_label_decl);

  Tree goto_else_or_endif;
  if (!else_part.is_null ())
    goto_else_or_endif = build_tree (GOTO_EXPR, bool_expr.get_locus (),void_type_node, else_label_decl);
  else
    goto_else_or_endif = goto_endif;

  TreeStmtList stmt_list;

  Tree cond_expr = build_tree (COND_EXPR, bool_expr.get_locus (), void_type_node, bool_expr, goto_then, goto_else_or_endif);
  stmt_list.append (cond_expr);

  Tree then_label_expr = build_tree (LABEL_EXPR, then_part.get_locus (), void_type_node, then_label_decl);
  stmt_list.append (then_label_expr);

  stmt_list.append (then_part);

  if (!else_part.is_null ())
    {
      // Make sure after then part has been executed we go to the end if
      stmt_list.append (goto_endif);

      Tree else_label_expr = build_tree (LABEL_EXPR, else_part.get_locus (),void_type_node, else_label_decl);
      stmt_list.append (else_label_expr);

      stmt_list.append (else_part);
    }

  // FIXME - location
  // FIXED :D
  Tree endif_label_expr = build_tree (LABEL_EXPR, UNKNOWN_LOCATION, void_type_node, endif_label_decl);
  stmt_list.append (endif_label_expr);

  return stmt_list.get_tree ();
}
/*
===(5)===
Another interesting statement is the if-statement. Let’s recall its syntax definition.
〈if〉 → if 〈expression〉 then 〈statement〉* end |
        if 〈expression〉 then 〈statement〉* else 〈statement〉* end
 As shown, deriving a parse function for the rule 〈if〉 is not obvious because the 
 two forms share a lot of elements. It may help to split the rule 〈if〉 in two rules follows.
〈if〉 → 〈if-then〉 end |
        〈if-then〉 else 〈statement〉* end |
        〈if-then〉 → if 〈expression〉 then 〈statement〉*

From this definition it is clear that we have to parse first an if, followed by an 
expression, followed by a then and followed by a statement sequence. In this case
the statement sequence will finish when we encounter an end or an else token. If
we find an end we are done parsing the if statement. If we find an else, it 
means that we still have to parse a statement sequence (this time the sequence 
finishes only if we encounter an end) and then an end token.        
*/
Tree
Parser::parse_if_statement ()
{
  return parse_if_statement(0);
}
Tree
Parser::parse_if_statement (int tkn_if)
{
  /*
  if (tkn_if==0 && !skip_token (Tiger::IF))
    {
      skip_after_end ();
      return Tree::error ();
    }
*/
  Tree expr = parse_boolean_expression ();

  skip_token (Tiger::THEN);

  enter_scope ();
  parse_statement_seq (&Parser::done_end_or_else);

  printf("Ultimo current_stmt_list DENTRO DO THEN %s \n ",print_type (get_current_stmt_list ().get_tipo ()));


  TreeSymbolMapping then_tree_scope = leave_scope ();
  Tree then_stmt = then_tree_scope.bind_expr;  

  Tree else_stmt;
  const_TokenPtr tok = lexer.peek_token ();
  if (tok->get_id () == Tiger::ELSE)
    {
      // Consume 'else'
      skip_token (Tiger::ELSE);

      enter_scope ();
      parse_statement_seq (&Parser::done_end);
      printf("Ultimo current_stmt_list DENTRO DO ELSE %s \n ",print_type (get_current_stmt_list ().get_tipo ()));
      TreeSymbolMapping else_tree_scope = leave_scope ();
      else_stmt = else_tree_scope.bind_expr;

      // Consume 'end'
      skip_token (Tiger::END);
    }
  else if (tok->get_id () == Tiger::END)
    {
      // Consume 'end'
      skip_token (Tiger::END);
    }
  else
    {
      unexpected_token (tok);
      return Tree::error ();
    }
  


  Tree stmt = build_if_statement (expr, then_stmt, else_stmt);
  return stmt;
}

Tree
Parser::build_while_statement (Tree bool_expr, Tree while_body)
{
  if (bool_expr.is_error ())
    return Tree::error ();

  TreeStmtList stmt_list;

  Tree while_check_label_decl = build_label_decl ("while_check", bool_expr.get_locus ());

  Tree while_check_label_expr = build_tree (LABEL_EXPR, bool_expr.get_locus (), void_type_node, while_check_label_decl);
  stmt_list.append (while_check_label_expr);

  Tree while_body_label_decl = build_label_decl ("while_body", while_body.get_locus ());

  Tree end_of_while_label_decl = build_label_decl ("end_of_while", UNKNOWN_LOCATION);

  Tree cond_expr = build_tree (COND_EXPR, bool_expr.get_locus (), void_type_node, bool_expr,
		                           build_tree (GOTO_EXPR, bool_expr.get_locus (), void_type_node, while_body_label_decl),
		                           build_tree (GOTO_EXPR, bool_expr.get_locus (), void_type_node, end_of_while_label_decl));
  stmt_list.append (cond_expr);

  Tree while_body_label_expr = build_tree (LABEL_EXPR, while_body.get_locus (), void_type_node, while_body_label_decl);
  stmt_list.append (while_body_label_expr);

  stmt_list.append (while_body);

  // FIXME - location
  Tree goto_check = build_tree (GOTO_EXPR, UNKNOWN_LOCATION, void_type_node, while_check_label_decl);
  stmt_list.append (goto_check);

  // FIXME - location
  Tree end_of_while_label_expr = build_tree (LABEL_EXPR, UNKNOWN_LOCATION, void_type_node, end_of_while_label_decl);
  stmt_list.append (end_of_while_label_expr);

  return stmt_list.get_tree ();
}

Tree
Parser::parse_literal_statement(){
  Tree literal = parse_expression ();
  
    printf("parse_literal_statement %s \n ",print_type (literal.get_type ()));

  TreeStmtList stmt_list;
  stmt_list.append (literal);
  return stmt_list.get_tree();

}

Tree
Parser::parse_let_statement(){
  printf("parse_let_statement\n");
  /*if (!skip_token (Tiger::LET))
  { 
    skip_after_end ();
    return Tree::error ();
  }
  */
  //enter_scope ();
  //parse_let_declaration_seq (&Parser::done_in);
  //Tree decl = parse_let_declaration_seq ();

  enter_scope ();
  parse_let_declaration_seq (&Parser::done_in);

  //Tree in_stmt;
  const_TokenPtr tok = lexer.peek_token ();
  if (tok->get_id () == Tiger::IN){
      skip_token (Tiger::IN);
      enter_scope ();
      parse_statement_seq (&Parser::done_end);
      /*TreeSymbolMapping in_tree_scope = leave_scope ();
      in_stmt = in_tree_scope.bind_expr;
      */
  }

  skip_token (Tiger::END);

  TreeSymbolMapping let_tree_scope = leave_scope ();
  Tree let_stmt = let_tree_scope.bind_expr;

  TreeStmtList stmt_list;

/*  Tree cond_expr = build_tree (COND_EXPR, bool_expr.get_locus (), void_type_node, bool_expr,
      goto_then, goto_else_or_endif);
      */
  stmt_list.append (let_stmt);
  //stmt_list.append (in_stmt); 

  return stmt_list.get_tree();

  //return build_if_statement (expr, let_stmt, int_stmt);
  //return build_while_statement (expr, let_body_stmt);
}

Tree
Parser::parse_while_statement ()
{
  /*
  if (!skip_token (Tiger::WHILE))
    {
      skip_after_end ();
      return Tree::error ();
    }
*/
  Tree expr = parse_boolean_expression ();
  if (!skip_token (Tiger::DO))
    {
      skip_after_end ();
      return Tree::error ();
    }

  enter_scope ();
  parse_statement_seq (&Parser::done_end);
  TreeSymbolMapping while_body_tree_scope = leave_scope ();

  Tree while_body_stmt = while_body_tree_scope.bind_expr;

  skip_token (Tiger::END);

  return build_while_statement (expr, while_body_stmt);
}

Tree
Parser::build_for_statement (SymbolPtr ind_var, Tree lower_bound, Tree upper_bound, Tree for_body_stmt_list)
{
  if (ind_var == NULL)
    return Tree::error ();
  Tree ind_var_decl = ind_var->get_tree_decl ();

  // Lower
  if (lower_bound.is_error ())
    return Tree::error ();

  // Upper
  if (upper_bound.is_error ())
    return Tree::error ();

  // ind_var := lower;
  TreeStmtList stmt_list;

  Tree init_ind_var = build_tree (MODIFY_EXPR, /* FIXME */ UNKNOWN_LOCATION, void_type_node, ind_var_decl, lower_bound);
  stmt_list.append (init_ind_var);

  // ind_var <= upper
  Tree while_condition = build_tree (LE_EXPR, upper_bound.get_locus (), boolean_type_node, ind_var_decl, upper_bound);

  // for-body
  // ind_var := ind_var + 1
  Tree incr_ind_var = build_tree (MODIFY_EXPR, /* FIXME */ UNKNOWN_LOCATION, void_type_node,ind_var_decl,
		                              build_tree (PLUS_EXPR, UNKNOWN_LOCATION, integer_type_node,ind_var_decl,
			                                        build_int_cst_type (::integer_type_node, 1)));

  // Wrap as a stmt list
  TreeStmtList for_stmt_list = for_body_stmt_list;
  for_stmt_list.append (incr_ind_var);

  // construct the associated while statement
  Tree while_stmt = build_while_statement (while_condition, for_stmt_list.get_tree ());
  stmt_list.append (while_stmt);

  return stmt_list.get_tree ();
}

Tree
Parser::parse_for_statement ()
{
  /*
  if (!skip_token (Tiger::FOR))
    {
      skip_after_end ();
      return Tree::error ();
    }
*/
  const_TokenPtr identifier = expect_token (Tiger::IDENTIFIER);
  if (identifier == NULL)
    {
      skip_after_end ();
      return Tree::error ();
    }

  if (!skip_token (Tiger::ASSIG))
    {
      skip_after_end ();
      return Tree::error ();
    }

  Tree lower_bound = parse_integer_expression ();

  if (!skip_token (Tiger::TO))
    {
      skip_after_end ();
      return Tree::error ();
    }

  Tree upper_bound = parse_integer_expression ();

  if (!skip_token (Tiger::DO))
    {
      skip_after_end ();
      return Tree::error ();
    }

  enter_scope ();
  parse_statement_seq (&Parser::done_end);

  TreeSymbolMapping for_body_tree_scope = leave_scope ();
  Tree for_body_stmt = for_body_tree_scope.bind_expr;

  skip_token (Tiger::END);

  // Induction var
  SymbolPtr ind_var = query_integer_variable (identifier->get_str (), identifier->get_locus ());

  return build_for_statement (ind_var, lower_bound, upper_bound, for_body_stmt);
}

Tree
Parser::get_scanf_addr ()
{
  if (scanf_fn.is_null ()){
    tree fndecl_type_param[] = { build_pointer_type (build_qualified_type (char_type_node, TYPE_QUAL_CONST)) /* const char* */  };
    tree fndecl_type = build_varargs_function_type_array (integer_type_node, 1, fndecl_type_param);

    tree scanf_fn_decl = build_fn_decl ("scanf", fndecl_type);
    DECL_EXTERNAL (scanf_fn_decl) = 1;

    scanf_fn = build1 (ADDR_EXPR, build_pointer_type (fndecl_type), scanf_fn_decl);
  }
  return scanf_fn;
}

Tree
Parser::parse_read_statement ()
{
  /*
  if (!skip_token (Tiger::READ)) {
    skip_after_semicolon_or_other ();
    return Tree::error ();
  }*/

  const_TokenPtr first_of_expr = lexer.peek_token ();
  Tree expr = parse_expression_naming_variable ();

  skip_token (Tiger::SEMICOLON);

  if (expr.is_error ())
    return Tree::error ();

  // Now this variable must be addressable
  TREE_ADDRESSABLE (expr.get_tree ()) = 1;

  const char *format = NULL;
  if (expr.get_type () == integer_type_node){
    format = "%d";
  } else if (expr.get_type () == float_type_node){
    format = "%f";
  }else{
    error_at (first_of_expr->get_locus (), "variable of type %s is not a valid read operand",print_type (expr.get_type ()));
    return Tree::error ();
  }

  tree args[] = {build_string_literal (strlen (format) + 1, format), // FIXME
                 build_tree (ADDR_EXPR, first_of_expr->get_locus (),
          		   build_pointer_type (expr.get_type ().get_tree ()), expr).get_tree ()
                };

  Tree scanf_fn = get_scanf_addr ();

  tree stmt = build_call_array_loc (first_of_expr->get_locus (), integer_type_node, scanf_fn.get_tree (), 2, args);

  return stmt;
}

Tree
Parser::get_puts_addr ()
{
  if (puts_fn.is_null ()){
    tree fndecl_type_param[] = {build_pointer_type (build_qualified_type (char_type_node,TYPE_QUAL_CONST)) /* const char* */};
    tree fndecl_type = build_function_type_array (integer_type_node, 1, fndecl_type_param);

    tree puts_fn_decl = build_fn_decl ("puts", fndecl_type);
    DECL_EXTERNAL (puts_fn_decl) = 1;

    puts_fn = build1 (ADDR_EXPR, build_pointer_type (fndecl_type), puts_fn_decl);
  }

  return puts_fn;
}

Tree
Parser::get_printf_addr ()
{
  if (printf_fn.is_null ()){
    tree fndecl_type_param[] = {build_pointer_type (build_qualified_type (char_type_node,TYPE_QUAL_CONST)) /* const char* */};
    tree fndecl_type = build_varargs_function_type_array (integer_type_node, 1, fndecl_type_param);

    tree printf_fn_decl = build_fn_decl ("printf", fndecl_type);
    DECL_EXTERNAL (printf_fn_decl) = 1;

    printf_fn = build1 (ADDR_EXPR, build_pointer_type (fndecl_type), printf_fn_decl);
  }

  return printf_fn;
}

Tree
Parser::parse_write_statement ()
{
  // write_statement -> "write" expression ";"
/*
  if (!skip_token (Tiger::WRITE)){
    skip_after_semicolon_or_other ();
    return Tree::error ();
  }
*/
  const_TokenPtr first_of_expr = lexer.peek_token ();
  Tree expr = parse_expression ();

  skip_token (Tiger::SEMICOLON);

  if (expr.is_error ())
    return Tree::error ();

  if (expr.get_type () == integer_type_node){
      // printf("%d\n", expr)
      const char *format_integer = "%d\n";
      tree args[]= {build_string_literal (strlen (format_integer) + 1, format_integer), expr.get_tree ()};

      Tree printf_fn = get_printf_addr ();

      tree stmt = build_call_array_loc (first_of_expr->get_locus (), integer_type_node, printf_fn.get_tree (), 2, args);

      return stmt;
  }else if (expr.get_type () == float_type_node){
      // printf("%f\n", (double)expr)
      const char *format_float = "%f\n";
      tree args[] = {build_string_literal (strlen (format_float) + 1, format_float), convert (double_type_node, expr.get_tree ())};

      Tree printf_fn = get_printf_addr ();

      tree stmt = build_call_array_loc (first_of_expr->get_locus (), integer_type_node, printf_fn.get_tree (), 2, args);

      return stmt;
    }
  else if (is_string_type (expr.get_type ())){
      // Alternatively we could use printf('%s\n', expr) instead of puts(expr)
      tree args[] = {expr.get_tree ()};
      Tree puts_fn = get_puts_addr ();

      tree stmt = build_call_array_loc (first_of_expr->get_locus (), integer_type_node, puts_fn.get_tree (), 1, args);
      return stmt;
    }
  else
    {
      error_at (first_of_expr->get_locus (), "value of type %s is not a valid write operand", print_type (expr.get_type ()));
      return Tree::error ();
    }

  gcc_unreachable ();
}
//antiga binding_powers aqui
//ANTIGA FUNCAO NULL E PARSE EXP ESTAVAM AQUI


// This implements priorities
int
Parser::left_binding_power (const_TokenPtr token)
{
  switch (token->get_id ())
    {
    case Tiger::DOT:
      return LBP_DOT;
    //
    case Tiger::LEFT_SQUARE:
      return LBP_ARRAY_REF;
    //
    case Tiger::ASTERISK:
      return LBP_MUL;
    case Tiger::SLASH:
      return LBP_DIV;
    case Tiger::PERCENT:
      return LBP_MOD;
    //
    case Tiger::PLUS:
      return LBP_PLUS;
    case Tiger::MINUS:
      return LBP_MINUS;
    //
    case Tiger::EQUAL:
      return LBP_EQUAL;
    case Tiger::DIFFERENT:
      return LBP_DIFFERENT;
    case Tiger::GREATER:
      return LBP_GREATER_THAN;
    case Tiger::GREATER_OR_EQUAL:
      return LBP_GREATER_EQUAL;
    case Tiger::LOWER:
      return LBP_LOWER_THAN;
    case Tiger::LOWER_OR_EQUAL:
      return LBP_LOWER_EQUAL;
    //
    case Tiger::OR:
      return LBP_LOGICAL_OR;
    case Tiger::AND:
      return LBP_LOGICAL_AND;
    case Tiger::NOT:
      return LBP_LOGICAL_NOT;
    // Anything that cannot appear after a left operand
    // is considered a terminator
    default:
      return LBP_LOWEST;
    }
}
//null estava aqui

Tree
Parser::coerce_binary_arithmetic (const_TokenPtr tok, Tree *left, Tree *right)
{
  Tree left_type = left->get_type ();
  Tree right_type = right->get_type ();

  if (left_type.is_error () || right_type.is_error ())
    return Tree::error ();

  if (left_type == right_type){
      if (left_type == integer_type_node || left_type == float_type_node){
	       return left_type;
	    }
  }else if ((left_type == integer_type_node && right_type == float_type_node) || (left_type == float_type_node && right_type == integer_type_node)) {
      // We will coerce the integer into a float
      if (left_type == integer_type_node) {
	       *left = build_tree (FLOAT_EXPR, left->get_locus (), float_type_node, left->get_tree ());
	    } else {
	       *right = build_tree (FLOAT_EXPR, right->get_locus (), float_type_node, right->get_tree ());
	    }
      return float_type_node;
  }

  // i.e. int + boolean
  error_at (tok->get_locus (), "invalid operands of type %s and %s for operator %s", print_type (left_type), print_type (right_type), tok->get_token_description ());
  return Tree::error ();
}

Parser::BinaryHandler
Parser::get_binary_handler (TokenId id)
{
  switch (id)
    {
    #define BINARY_HANDLER(name, token_id)                                         \
      case Tiger::token_id:                                                        \
        return &Parser::binary_##name;
          BINARY_HANDLER_LIST
    #undef BINARY_HANDLER
        default:
          return NULL;
    }
}

Tree
Parser::binary_plus (const_TokenPtr tok, Tree left)
{
  Tree right = parse_expression (LBP_PLUS);
  if (right.is_error ())
    return Tree::error ();

  Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (tree_type.is_error ())
    return Tree::error ();

  return build_tree (PLUS_EXPR, tok->get_locus (), tree_type, left, right);
}

Tree
Parser::binary_minus (const_TokenPtr tok, Tree left)
{
  Tree right = parse_expression (LBP_MINUS);
  if (right.is_error ())
    return Tree::error ();

  Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (tree_type.is_error ())
    return Tree::error ();

  return build_tree (MINUS_EXPR, tok->get_locus (), tree_type, left, right);
}

Tree
Parser::binary_mult (const_TokenPtr tok, Tree left)
{
  Tree right = parse_expression (LBP_MUL);
  if (right.is_error ())
    return Tree::error ();

  Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (tree_type.is_error ())
    return Tree::error ();

  return build_tree (MULT_EXPR, tok->get_locus (), tree_type, left, right);
}

Tree
Parser::binary_div (const_TokenPtr tok, Tree left)
{
  Tree right = parse_expression (LBP_DIV);
  if (right.is_error ())
    return Tree::error ();

  if (left.get_type () == integer_type_node && right.get_type () == integer_type_node)  {
      // Integer division (truncating, like in C)
      return build_tree (TRUNC_DIV_EXPR, tok->get_locus (), integer_type_node, left, right);
    } else {
      // Real division
      Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
      if (tree_type.is_error ())
	       return Tree::error ();

      gcc_assert (tree_type == float_type_node);

      return build_tree (RDIV_EXPR, tok->get_locus (), tree_type, left, right);
    }
}

Tree
Parser::binary_mod (const_TokenPtr tok, Tree left)
{
  Tree right = parse_expression (LBP_MOD);
  if (right.is_error ())
    return Tree::error ();

  if (left.get_type () == integer_type_node && right.get_type () == integer_type_node) {
      // Integer division (truncating, like in C)
      return build_tree (TRUNC_MOD_EXPR, tok->get_locus (), integer_type_node, left, right);
    } else {
      error_at (tok->get_locus (), "operands of modulus must be of integer type");
      return Tree::error ();
    }
}

Tree
Parser::binary_equal (const_TokenPtr tok, Tree left)
{
  Tree right = parse_expression (LBP_EQUAL);
  if (right.is_error ())
    return Tree::error ();

  Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (tree_type.is_error ())
    return Tree::error ();

  return build_tree (EQ_EXPR, tok->get_locus (), boolean_type_node, left, right);
}

Tree
Parser::binary_different (const_TokenPtr tok, Tree left)
{
  Tree right = parse_expression (LBP_DIFFERENT);
  if (right.is_error ())
    return Tree::error ();

  Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (tree_type.is_error ())
    return Tree::error ();

  return build_tree (NE_EXPR, tok->get_locus (), boolean_type_node, left, right);
}

Tree
Parser::binary_lower_than (const_TokenPtr tok, Tree left)
{
  Tree right = parse_expression (LBP_LOWER_THAN);
  if (right.is_error ())
    return Tree::error ();

  Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (tree_type.is_error ())
    return Tree::error ();

  return build_tree (LT_EXPR, tok->get_locus (), boolean_type_node, left, right);
}

Tree
Parser::binary_lower_equal (const_TokenPtr tok, Tree left) {
  Tree right = parse_expression (LBP_LOWER_EQUAL);
  if (right.is_error ())
    return Tree::error ();

  Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (tree_type.is_error ())
    return Tree::error ();

  return build_tree (LE_EXPR, tok->get_locus (), boolean_type_node, left, right);
}

Tree
Parser::binary_greater_than (const_TokenPtr tok, Tree left)
{
  Tree right = parse_expression (LBP_GREATER_THAN);
  if (right.is_error ())
    return Tree::error ();

  Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (tree_type.is_error ())
    return Tree::error ();

  return build_tree (GT_EXPR, tok->get_locus (), boolean_type_node, left, right);
}

Tree
Parser::binary_greater_equal (const_TokenPtr tok, Tree left) {
  Tree right = parse_expression (LBP_GREATER_EQUAL);
  if (right.is_error ())
    return Tree::error ();

  Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (tree_type.is_error ())
    return Tree::error ();

  return build_tree (GE_EXPR, tok->get_locus (), boolean_type_node, left, right);
}

bool
Parser::check_logical_operands (const_TokenPtr tok, Tree left, Tree right) {
  if (left.get_type () != boolean_type_node || right.get_type () != boolean_type_node) {
      error_at ( tok->get_locus (), "operands of operator %s must be boolean but they are %s and %s\n", tok->get_token_description (), print_type (left.get_type ()), print_type (right.get_type ()));
      return false;
  }
  return true;
}

Tree
Parser::binary_logical_and (const_TokenPtr tok, Tree left)
{
  Tree right = parse_expression (LBP_LOGICAL_AND);
  if (right.is_error ())
    return Tree::error ();

  if (!check_logical_operands (tok, left, right))
    return Tree::error ();

  return build_tree (TRUTH_ANDIF_EXPR, tok->get_locus (), boolean_type_node, left, right);
}

Tree
Parser::binary_logical_or (const_TokenPtr tok, Tree left)
{
  Tree right = parse_expression (LBP_LOGICAL_OR);
  if (right.is_error ())
    return Tree::error ();

  if (!check_logical_operands (tok, left, right))
    return Tree::error ();

  return build_tree (TRUTH_ORIF_EXPR, tok->get_locus (), boolean_type_node, left, right);
}

Tree
Parser::binary_array_ref (const const_TokenPtr tok, Tree left)
{
  Tree right = parse_integer_expression ();
  if (right.is_error ())
    return Tree::error ();

  if (!skip_token (Tiger::RIGHT_SQUARE))
    return Tree::error ();

  if (!is_array_type (left.get_type ()))
    {
      error_at (left.get_locus(), "does not have array type");
      return Tree::error ();
    }

  Tree element_type = TREE_TYPE(left.get_type().get_tree());

  return build_tree (ARRAY_REF, tok->get_locus (), element_type, left, right, Tree(), Tree());
}

Tree
Parser::binary_field_ref (const const_TokenPtr tok, Tree left)
{
  const_TokenPtr identifier = expect_token (Tiger::IDENTIFIER);
  if (identifier == NULL){
      return Tree::error ();
  }

  if (!is_record_type (left.get_type ())){
      error_at (left.get_locus (), "does not have record type");
      return Tree::error ();
   }

  Tree field_decl = TYPE_FIELDS (left.get_type ().get_tree ());
  while (!field_decl.is_null ()){
      Tree decl_name = DECL_NAME (field_decl.get_tree ());
      const char *field_name = IDENTIFIER_POINTER (decl_name.get_tree ());
      if (field_name == identifier->get_str ())
	       break;

      field_decl = TREE_CHAIN (field_decl.get_tree ());
    }

  if (field_decl.is_null ()){
      error_at (left.get_locus (), "record type does not have a field named '%s'", identifier->get_str ().c_str ());
      return Tree::error ();
    }

  return build_tree (COMPONENT_REF, tok->get_locus (), TREE_TYPE (field_decl.get_tree ()), left, field_decl,Tree ());
}

/*
===(8)===
Rather than making a relatively large switch 
(like we did in null_denotation), here we call 
a function that given a token will return us a 
pointer to the member function that implements 
the left denotation for token tok. We could have 
taken the same approach in the null_denotation 
function but given that there are much less unary 
operators it looked like unnecesary.
*/
// This is invoked when a token (likely an operand) is found at a (likely
// infix) non-prefix position
Tree
Parser::left_denotation (const_TokenPtr tok, Tree left)
{
  BinaryHandler binary_handler = get_binary_handler (tok->get_id ());
  if (binary_handler == NULL)
    {
      unexpected_token (tok);
      return Tree::error ();
    }

  return (this->*binary_handler) (tok, left);
}

Tree
Parser::parse_expression ()
{
  return parse_expression (/* right_binding_power */ 0);
}

Tree
Parser::parse_boolean_expression ()
{
  Tree expr = parse_expression ();
  if (expr.is_error ())
    return expr;

  if (expr.get_type () != boolean_type_node)
    {
      error_at (expr.get_locus (), "expected expression of boolean type but its type is %s", print_type (expr.get_type ()));
      return Tree::error ();
    }
  return expr;
}

Tree
Parser::parse_integer_expression ()
{
  Tree expr = parse_expression ();
  if (expr.is_error ())
    return expr;

  if (expr.get_type () != integer_type_node)
    {
      error_at (expr.get_locus (), "expected expression of integer type but its type is %s", print_type (expr.get_type ()));
      return Tree::error ();
    }
  return expr;
}

Tree
Parser::parse_expression_naming_variable ()
{
  Tree expr = parse_expression ();
  if (expr.is_error ())
    return expr;

  if (expr.get_tree_code () != VAR_DECL && expr.get_tree_code () != ARRAY_REF && expr.get_tree_code () != COMPONENT_REF)
    {
      error_at (expr.get_locus (), "does not designate a variable, array element or field");
      return Tree::error ();
    }
  return expr;
}

Tree
Parser::parse_lhs_assignment_expression ()
{
  return parse_expression_naming_variable();
}
}

// ------------------------------------------------------
// ------------------------------------------------------
// ------------------------------------------------------

static void tiger_parse_file (const char *filename);
static void tiger_parse_file_test_lex (const char *filename);

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
  tiger_parse_file_test_lex(filename);
  // FIXME: handle stdin "-"
  FILE *file = fopen (filename, "r");
  if (file == NULL)
    {
      fatal_error (UNKNOWN_LOCATION, "cannot open filename %s: %m", filename);
    }

  Tiger::Lexer lexer (filename, file);
  Tiger::Parser parser (lexer);

  parser.parse_program ();

  fclose (file);
}
static void 
tiger_parse_file_test_lex (const char *filename)
{
 // FIXME: handle stdin "-"
  FILE *file = fopen (filename, "r");
  if (file == NULL)
    {
      fatal_error (UNKNOWN_LOCATION, "cannot open filename %s: %m", filename);
    }

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