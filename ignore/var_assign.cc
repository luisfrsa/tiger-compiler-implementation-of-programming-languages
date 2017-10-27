Tree
Parser::parse_assignment_statement ()
{
  const_TokenPtr identifier = expect_token (Tiny::IDENTIFIER);
  if (identifier == NULL)
    {
      skip_after_semicolon ();
      return Tree::error ();
    }
 
  SymbolPtr sym
    = query_variable (identifier->get_str (), identifier->get_locus ());
  if (sym == NULL)
    {
      skip_after_semicolon ();
      return Tree::error ();
    }
 
  gcc_assert (!sym->get_tree_decl ().is_null ());
  Tree var_decl = sym->get_tree_decl ();
 
  const_TokenPtr assig_tok = expect_token (Tiny::ASSIG);
  if (assig_tok == NULL)
    {
      skip_after_semicolon ();
      return Tree::error ();
    }
 
  const_TokenPtr first_of_expr = lexer.peek_token ();
 
  Tree expr = parse_expression ();
  if (expr.is_error ())
    return Tree::error ();
 
  skip_token (Tiny::SEMICOLON);
 
  if (var_decl.get_type () != expr.get_type ())
    {
      error_at (first_of_expr->get_locus (),
    "cannot assign value of type %s to variable '%s' of type %s",
    print_type (expr.get_type ()), sym->get_name ().c_str (),
    print_type (var_decl.get_type ()));
      return Tree::error ();
    }
 
  Tree assig_expr = build_tree (MODIFY_EXPR, assig_tok->get_locus (),
        void_type_node, var_decl, expr);
 
  return assig_expr;
}















