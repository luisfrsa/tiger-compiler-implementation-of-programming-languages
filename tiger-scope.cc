#include "tiger-scope.h"

namespace Tiger
{

Scope::Scope ()
{
}

void
Scope::push_scope ()
{
  map_stack.push_back (SymbolMapping());
}

void
Scope::pop_scope ()
{
  gcc_assert (!map_stack.empty());
  map_stack.pop_back ();
}

/*
We can manage the current symbol mapping using Scope::push_scope() 
and Scope::pop_scope(). The former will be used when we need a fresh
mapping (as it will happen when handling if, while and for statements). 
Scope::get_current_mapping returns the current mapping (i.e. the one that 
was created in the last push_scope that has not been popped yet).

Function Scope::lookup is used to get the last mapping for a given string 
(or null if there is no such mapping).
*/
SymbolPtr
Scope::lookup (const std::string &str)
{
  for (MapStack::reverse_iterator map = map_stack.rbegin ();
       map != map_stack.rend (); map++)
    {
      if (SymbolPtr sym = map->get (str))
	{
	  return sym;
	}
    }
  return SymbolPtr();
}
}
