#include "tiger-token.h"

namespace Tiger
{

const char *
get_token_description (TokenId tid)
{
  switch (tid)
    {
#define TIGER_TOKEN(name, descr)                                                \
  case name:                                                                   \
    return descr;
#define TIGER_TOKEN_KEYWORD(x, y) TIGER_TOKEN (x, y)
      TIGER_TOKEN_LIST
#undef TIGER_TOKEN_KEYWORD
#undef TIGER_TOKEN
    default:
      gcc_unreachable ();
    }
}

const char *
token_id_to_str (TokenId tid)
{
  switch (tid)
    {
#define TIGER_TOKEN(name, _)                                                    \
  case name:                                                                   \
    return #name;
#define TIGER_TOKEN_KEYWORD(x, y) TIGER_TOKEN (x, y)
      TIGER_TOKEN_LIST
#undef TIGER_TOKEN_KEYWORD
#undef TIGER_TOKEN
    default:
      gcc_unreachable ();
    }
}

}
