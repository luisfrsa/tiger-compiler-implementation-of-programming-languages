#ifndef TIGER_SYMBOL_H
#define TIGER_SYMBOL_H

#include "tiger/tiger-tree.h"

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"

#include <tr1/memory>

#include <fstream> //alteracao_luis
//using namespace std; //alteracao_luis

/*
tiny2tiger
http://thinkingeek.com/2016/01/16/tiny-gcc-front-part-5/
We will use the class Symbol to represent a named entity of a tiny program. 
So far the only named entities we have in tiny are variables. Other languages 
may have types, constants and functions in their set of entities with names. Symbol class would be used as well for such entities.
*/
namespace Tiger
{

enum /* class */ SymbolKind
{
  INVALID,
  VARIABLE,
  TYPENAME
};

struct Symbol
{
public:
  Symbol (SymbolKind kind, const std::string &name_) : kind(kind), name (name_), decl (error_mark_node)
  {
    gcc_assert (name.size () > 0);
  }

  SymbolKind
  get_kind () const
  {
    return kind;
  }

  std::string
  get_name () const
  {
    return name;
  }

  void
  set_tree_decl (Tree decl_)
  {
    gcc_assert ((kind == VARIABLE && decl_.get_tree_code() == VAR_DECL)
                    || (kind == TYPENAME && decl_.get_tree_code() == TYPE_DECL));
    decl = decl_;
  }

  Tree
  get_tree_decl () const
  {
    return decl;
  }

private:
  SymbolKind kind;
  std::string name;
  Tree decl;
};

typedef std::tr1::shared_ptr<Symbol> SymbolPtr;
typedef std::tr1::shared_ptr<const Symbol> const_SymbolPtr;

}

#endif // TIGER_SYMBOL_H
