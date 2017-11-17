#ifndef TIGER_TREE_H
#define TIGER_TREE_H

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "tree-iterator.h"
#include "input.h"

/*
If you recall part 2, the final goal of our front end is creating a GENERIC tree and handing it to the rest of the compiler. 
Let’s talk about bit more about GENERIC trees.

GENERIC trees are represented using the type tree. A tree can be a NULL_TREE or point to an actual tree. Each tree has a tree 
code that is specified at the moment of creation. Given a tree we can use the macro TREE_CODE to get the tree code. Most trees, 
but not all, have a location that we can obtain using the macro EXPR_LOC, if it does not have location it will return 
UNKNOWN_LOCATION.

Trees are created using macros build0, build1, build2, …, build5. The first parameter of each buildN macro is the tree 
code and the remaining N arguments are trees, called the operands. As an alternative build0_loc, build1_loc, 
build2_loc, …, build5_loc can be used instead to create a tree along with a location. The location goes in the first 
argument and the remaining arguments are the same as in buildN.

Despite their name, GENERIC trees do not collectively form a tree but a graph. This happens because it is not an error 
that a tree appears as the operand of two or more trees.

Each tree of a specific tree code may have associated several attributes. These attributes are accessed using macros. 
Most of these macros expand in a way that can be used to set the attribute to the tree. So given a tree t, an attribute 
can be queried doing SOME_TREE_PROPERTY(t) and can be set doing SOME_TREE_PROPERTY(t) = property. These attributes are of 
different nature, sometimes are other trees, sometimes are boolean values (zero or nonzero), etc.

GENERIC trees are used to represent many aspects of a program but there are three important classes of trees: declarations, 
expressions and types.

Declarations are used to tell the compiler about the existence of something. Variables go into a tree with code VAR_DECL. 
Labels of the program (used for gotos) go into a LABEL_DECL. tiny does not have functions explicitly but if we declare 
a function, it goes into a FUNCTION_DECL and each of its parameters would be represented using PARM_DECL.

Expressions represent trees that can be evaluated. There are a lot of tree codes related to expressions that we will 
see later. One distinguished node, error_mark_node, will be used as a marker for erroneous trees that may appear 
during semantic analysis. Given a tree t, the macro error_operand_p(t) returns true if t is error_mark_node.

Finally, types represent data types. They are represented as trees because most type systems have a recursive structure 
that fits well in a graph-like structure like GENERIC. Type trees are heavily reused in GENERIC. In tiny we will 
need tree types for int, float, boolean and strings. Expressions and declarations have type and it can be accessed 
using TREE_TYPE.

GENERIC is an intermediate representation that is heavily biased towards a C model of execution (like a relatively 
high-level assembler). The reason is that GCC was originally a C compiler that later on was extended to support other 
programming languages. Imperative programming languages, like tiny, fit relatively well in GENERIC. Other programming 
languages, like functional ones, do not fit so well in GENERIC and a front end for such languages likely uses its own 
representation that ends being lowered to GENERIC.

Almost GENERIC

Tiny is so simple that we can use GENERIC trees almost directly. Almost, because not all GENERIC trees may have 
locations so we will pair a tree and a location, to make sure we have a location. Getting the GENERIC tree is, 
then, as simple as requesting the tree member of the pair. We want to have location in all trees for diagnostic purposes.

In order to ease using GENERIC trees, we will use a Tree class (mind the uppercase) that will be a very thin wrapper to tree.
*/
namespace Tiger
{

// This wrapper is similar to cp_tree used in C++ FE
// and is used to keep a location for those trees
// that do not have it
//
struct Tree
{
public:
  Tree () : t (NULL_TREE), loc (UNKNOWN_LOCATION) {}
  Tree (tree t_) : t (t_), loc (EXPR_LOCATION (t)) {}
  Tree (tree t_, location_t loc_) : t (t_), loc (loc_) {}
  Tree (Tree t_, location_t loc_) : t (t_.get_tree ()), loc (loc_) {}

  location_t
  get_locus () const
  {
    return loc;
  }

  void
  set_locus (location_t loc_)
  {
    loc = loc_;
  }

  tree
  get_tree () const
  {
    return t;
  }

  tree_code
  get_tree_code () const
  {
    return TREE_CODE (t);
  }

  void
  set_tree (tree t_)
  {
    t = t_;
  }

  bool
  is_error () const
  {
    return error_operand_p (t);
  }

  bool
  is_null ()
  {
    return t == NULL_TREE;
  }

  static Tree
  error ()
  {
    return Tree (error_mark_node);
  }
  
  Tree
  get_type () const
  {
    return TREE_TYPE (t);
  }

private:
  tree t;
  location_t loc;
};
/*
A GENERIC tree is actually a pointer, so comparison by identity is possible. 
For simplicity, let’s teach Tree to do identity comparisons as well.
*/
inline bool operator==(Tree t1, Tree t2) { return t1.get_tree () == t2.get_tree (); }
inline bool operator!=(Tree t1, Tree t2) { return !(t1 == t2); }

inline Tree
build_tree (tree_code tc, location_t loc, Tree type, Tree t1)
{
  return build1_loc (loc, tc, type.get_tree (), t1.get_tree ());
}

inline Tree
build_tree (tree_code tc, location_t loc, Tree type, Tree t1, Tree t2)
{
  return build2_loc (loc, tc, type.get_tree (), t1.get_tree (), t2.get_tree ());
}

inline Tree
build_tree (tree_code tc, location_t loc, Tree type, Tree t1, Tree t2, Tree t3)
{
  return build3_loc (loc, tc, type.get_tree (), t1.get_tree (), t2.get_tree (),
		     t3.get_tree ());
}

inline Tree
build_tree (tree_code tc, location_t loc, Tree type, Tree t1, Tree t2, Tree t3,
	    Tree t4)
{
  return build4_loc (loc, tc, type.get_tree (), t1.get_tree (), t2.get_tree (),
		     t3.get_tree (), t4.get_tree ());
}

inline Tree
build_tree (tree_code tc, location_t loc, Tree type, Tree t1, Tree t2, Tree t3,	    Tree t4, Tree t5)
{
  return build5_loc (loc, tc, type.get_tree (), t1.get_tree (), t2.get_tree (),
		     t3.get_tree (), t4.get_tree (), t5.get_tree ());
}

// Adapter for TREE_LIST
struct TreeStmtList
{
public:
  TreeStmtList () : list (alloc_stmt_list ()) {}
  TreeStmtList (Tree t) : list (t.get_tree ()) {}
  void
  append (Tree t)
  {
    append_to_statement_list (t.get_tree (), &list);
  }
 
  tree get_tree () const
  {
    return list;
  }
  void set_tipo(Tree t){
    tipo = t;
  }
  Tree get_tipo ()
  {
    return tipo;
  }

private:
  Tree tipo;

  tree list;
};

// FIXME - Check if this already exists in GCC
template <typename Append> struct TreeChainBase
{
  Tree first;
  Tree last;

  TreeChainBase () : first (), last () {}

  void
  append (Tree t){
    gcc_assert (!t.is_null());
    if (first.is_null()){
	     first = last = t;
    }else{
	     Append () (last, t);
	     last = t;
    }
  }
};

struct tree_chain_append
{
  void operator() (Tree t, Tree a) { TREE_CHAIN (t.get_tree()) = a.get_tree(); }
};

struct TreeChain : TreeChainBase<tree_chain_append>
{
};

struct block_chain_append
{
  void operator() (Tree t, Tree a) { BLOCK_CHAIN (t.get_tree()) = a.get_tree(); }
};

struct BlockChain : TreeChainBase<block_chain_append>
{
};

}

#endif // TIGER_TREE_H
