  
#include <iostream>
#include <memory>

#include "tiger/tiger-parser.h"
#include "tiger/tiger-lexer.h"
/*
#include "tiger/tiger-tree.h"
#include "tiger/tiger-symbol.h"
#include "tiger/tiger-symbol-mapping.h"
#include "tiger/tiger-scope.h"
*/

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

namespace Tiger{

  struct Parser{
    // ...
    public:
    Parser (Lexer & lexer_): lexer (lexer_) { }
    void parse_program();
    void parse_statement_seq(bool(Parser:: * done)());
    bool done_end_or_else ();
    bool done_end_of_file ();
    void parse_statement ();
    void parse_variable_declaration ();
    void parse_while_statement();
    void parse_for_statement();
    void parse_read_statement();
    void parse_write_statement();
    void parse_assignment_statement();
    void unexpected_token(const_TokenPtr t);
    void skip_after_semicolon();
    bool done_end();
    void skip_after_end();
    bool skip_token(Tiger::TokenId token_id);
    void parse_if_statement();
    const_TokenPtr expect_token(Tiger::TokenId token_id);
    bool parse_type();
    bool parse_expression(int right_binding_power);
    bool parse_expression();
    bool null_denotation(const_TokenPtr tok);
    int left_binding_power (const_TokenPtr token);
    bool left_denotation (const_TokenPtr tok);

    typedef bool (Parser::*BinaryHandler) (const_TokenPtr);
    BinaryHandler get_binary_handler (TokenId id);

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
      bool binary_##name (const_TokenPtr tok);
      BINARY_HANDLER_LIST
    #undef BINARY_HANDLER
    private:
      Lexer &lexer;

   };
  void
  Parser::parse_program ()
  {
     parse_statement_seq(&Parser::done_end_of_file);
  }
  void
  Parser::parse_statement_seq (bool (Parser::*done) ())
  {
    // Parse statements until done and append to the current stmt list;
    while (!(this->*done) ())
      {
        parse_statement ();
      }
  }
  bool
  Parser::done_end_of_file ()
  {
    const_TokenPtr t = lexer.peek_token ();
    return (t->get_id () == Tiger::END_OF_FILE);
  }
   bool
  Parser::done_end_or_else ()
  {
    const_TokenPtr t = lexer.peek_token ();
    return (t->get_id () == Tiger::END || t->get_id () == Tiger::ELSE
      || t->get_id () == Tiger::END_OF_FILE);
  }
  bool
  Parser::done_end ()
  {
    const_TokenPtr t = lexer.peek_token ();
    return (t->get_id () == Tiger::END || t->get_id () == Tiger::END_OF_FILE);
  }
  void
  Parser::parse_statement ()
  {
    const_TokenPtr t = lexer.peek_token ();
    switch (t->get_id ())
      {
      case Tiger::VAR:
        parse_variable_declaration ();
        break;
      case Tiger::IF:
        parse_if_statement ();
        break;
      /*case Tiger::WHILE:
        parse_while_statement ();
        break;
       
      case Tiger::FOR:
        parse_for_statement ();
        break;
      case Tiger::READ:
        parse_read_statement ();
        break;
      case Tiger::WRITE:
        parse_write_statement ();
        break;
      case Tiger::IDENTIFIER:
        parse_assignment_statement ();
        break;
        */
      default:
        unexpected_token (t);
        skip_after_semicolon ();
        break;
      }
  }
  void
  Parser::unexpected_token (const_TokenPtr t)
  {
    error_at (t->get_locus (), "unexpected %s\n", t->get_token_description ());
  }
   
  void
  Parser::skip_after_semicolon ()
  {
    const_TokenPtr t = lexer.peek_token ();
   
    while (t->get_id () != Tiger::END_OF_FILE && t->get_id () != Tiger::SEMICOLON)
      {
        lexer.skip_token ();
        t = lexer.peek_token ();
      }
   
    if (t->get_id () == Tiger::SEMICOLON)
      lexer.skip_token ();
  }
  void
  Parser::parse_variable_declaration ()
  /*
  Tiny:
  var a:int;
  Tiger:
  var a := 1;
  var a :int;
  var a :int :=1;
  var a :int :=nil;
  */
  {
    if (!skip_token (Tiger::VAR))
      {
        skip_after_semicolon ();
        return;
      }
   
    const_TokenPtr identifier = expect_token (Tiger::IDENTIFIER);
    if (identifier == NULL)
      {
        skip_after_semicolon ();
        return;
      }
   
    if (!skip_token (Tiger::COLON))
      {
        skip_after_semicolon ();
        return;
      }
   
    if (!parse_type ())
       return;
   
    skip_token (Tiger::SEMICOLON);
  }
  bool
  Parser::parse_type ()
  {
    const_TokenPtr t = lexer.peek_token ();
   
    switch (t->get_id ())
      {
      case Tiger::INT:
        lexer.skip_token ();
        return true;
      case Tiger::REAL:
        lexer.skip_token ();
        return true;
      default:
        unexpected_token (t);
        return false;
      }
  }

  /*
  Here we use a function skip_token that given a token id, 
  checks if the current token has that same id. If it has, 
  it just skips it and returns true. Otherwise diagnoses an 
  error and returns false. When skip_token fails (i.e. returns false)
  we immediately go to panic mode and give up parsing the current statement.
  As you can see this code quickly becomes tedious and repetitive.
  No wonder there exist tools, like ANTLR by Terence Parr, 
  that automate the code generation of recursive descent recognizers.
  */
  bool
  Parser::skip_token (Tiger::TokenId token_id)
  {
    return expect_token (token_id) != const_TokenPtr ();
  }
  const_TokenPtr
  Parser::expect_token (Tiger::TokenId token_id)
  {
    const_TokenPtr t = lexer.peek_token ();
    if (t->get_id () == token_id)
      {
        lexer.skip_token ();
        return t;
      }
    else
      {
        error_at (t->get_locus (), "expecting %s but %s found\n",
      get_token_description (token_id), t->get_token_description ());
        return const_TokenPtr ();
      }
  }
  
  void
  Parser::parse_if_statement ()
  {
    if (!skip_token (Tiger::IF))
      {
        skip_after_end ();
        return;
      }
   
    parse_expression ();
   
    skip_token (Tiger::THEN);
   
    parse_statement_seq (&Parser::done_end_or_else);
   
    const_TokenPtr tok = lexer.peek_token ();
    if (tok->get_id () == Tiger::ELSE)
      {
        // Consume 'else'
        skip_token (Tiger::ELSE);
   
        parse_statement_seq (&Parser::done_end);
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
        skip_after_end ();
      }
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
  // This is a Pratt parser
  bool
  Parser::parse_expression (int right_binding_power)
  {
    const_TokenPtr current_token = lexer.peek_token ();
    lexer.skip_token ();
   
    if (!null_denotation (current_token))
       return false;
   
    while (right_binding_power < left_binding_power (lexer.peek_token ()))
      {
        current_token = lexer.peek_token();
        lexer.skip_token ();
   
        if (!left_denotation (current_token))
           return false;
      }
   
    return true;
  }
   
  bool
  Parser::parse_expression ()
  {
    return parse_expression(/*LBP_LOWEST*/0);
  }
  bool
  Parser::null_denotation (const_TokenPtr tok)
  {
    switch (tok->get_id ())
      {
      case Tiger::IDENTIFIER:
      case Tiger::INTEGER_LITERAL:
      case Tiger::REAL_LITERAL:
      case Tiger::STRING_LITERAL:
        return true;
      case Tiger::LEFT_PAREN:
        {
    if (!parse_expression ())
             return false;
    tok = lexer.peek_token ();
    return skip_token(Tiger::RIGHT_PAREN);
        }
      case Tiger::PLUS:
        {
    if (!parse_expression (LBP_UNARY_PLUS))
             return false;
          return true;
        }
      case Tiger::MINUS:
        {
    if (!parse_expression (LBP_UNARY_MINUS))
             return false;
          return true;
        }
      case Tiger::NOT:
        {
          if (!parse_expression (LBP_LOGICAL_NOT))
             return false;
    return true;
        }
      default:
        unexpected_token (tok);
        return false;
      }
  }
  bool
  Parser::left_denotation (const_TokenPtr tok)
  {
    BinaryHandler binary_handler = get_binary_handler (tok->get_id ());
    if (binary_handler == NULL)
      {
        unexpected_token (tok);
        return false;
      }
   
    return (this->*binary_handler) (tok);
  }
  Parser::BinaryHandler
  Parser::get_binary_handler (TokenId id)
  {
    switch (id)
      {
        /*
  #define BINARY_HANDLER(name, token_id)                                         \
    case Tiger::token_id:                                                         \
      return &Parser::binary_##name;
        BINARY_HANDLER_LIST
  #undef BINARY_HANDLER
      default:
        return NULL;
      */
      }
  }
  bool
  Parser::binary_plus (const_TokenPtr tok)
  {
    if (!parse_expression (LBP_PLUS))
       return false;
    return true;
  }
  
   
  int
  Parser::left_binding_power (const_TokenPtr token)
  {
    switch (token->get_id ())
      {
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
      // Anything that cannot appear in an infix position
      // is given the lowest priority
      default:
        return LBP_LOWEST;
      }
  }
  
}
// ------------------------------------------------------
// ------------------------------------------------------
// ------------------------------------------------------

/*---------------------------------------------*/
/*---------------------------------------------*/
/*---------------------------------------------*/
/*---------------------------------------------*/

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
      break;
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
  Tiger::Parser parser (lex);

  parser.parse_program ();

  fclose (file);
}
/*comentado no tiger-parser.cc e adicionado aqui */