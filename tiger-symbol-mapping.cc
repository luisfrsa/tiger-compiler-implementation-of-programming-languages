#include <utility>
#include <sstream>

#include "tiger-symbol-mapping.h"

#include "config.h"
#include "system.h"

namespace Tiger
{

/*
As you can see it is a very thin wrapper to a map of strings 
to Symbol (for this reason sometimes a structure like this is called a symbol table).

SymbolMapping::insert adds a new Symbol into the map using its 
name as the key. It also checks that the name is not being added 
twice: this is not possible in tiny.
*/
void
SymbolMapping::insert (SymbolPtr s)
{
  gcc_assert (s != NULL);
  std::pair<Map::iterator, bool> p
    = map.insert (std::make_pair (s->get_name (), s));

  gcc_assert (p.second);
}
/*
SymbolMapping::get returns the mapped Symbol for the given string. 
Since it may happen that there is no such mapping this function may return a nul Symbol.
*/
SymbolPtr
SymbolMapping::get (const std::string &str) const
{
  Map::const_iterator it = map.find (str);
  if (it != map.end ())
    {
      return it->second;
    }
  return SymbolPtr();
}

}
