
Tree
Parser::parse_variable_declaration ()
{
  // variable_declaration -> "var" identifier ":" type ";"
  if (!skip_token (Tiger::VAR))
    {
      skip_after_semicolon ();
      return Tree::error ();
    }
  /*verifica se é identificador*/
  const_TokenPtr identifier = expect_token (Tiger::IDENTIFIER);
  if (identifier == NULL)
    {
      skip_after_semicolon ();
      return Tree::error ();
    }

  /*verifica se é : ou :=*/
  const_TokenPtr tok = lexer.peek_token ();
  if (tok->get_id () == Tiger::COLON){ //var a :
    skip_token (Tiger::COLON);
    Tree type_tree = parse_type (); //var a :int

    if (type_tree.is_error ()){
      skip_after_semicolon();
      return Tree::error ();
    }    
    if (!skip_token (Tiger::ASSIG)){//var a :int:=
        skip_after_semicolon ();
        return Tree::error ();
    }

  }else if(tok->get_id () == Tiger::ASSIG){//var a :=
    skip_token (Tiger::ASSIG);
    /*fiz: parse_assignment_statement com sobreescrita caso seja passado uma
    tree como parametro, nao sei se vai dar certo pois essa tree é gerada a baixo
    se tiver muito hard, iniciar com um while, ou for
    */
    parse_assignment_statement ()
  }else{
      skip_after_semicolon ();
      return Tree::error ();
  }
  //espera um ;
  skip_token (Tiger::SEMICOLON);

  //verifica se já existe variavel declarada com este nome ;
  if (scope.get_current_mapping ().get (identifier->get_str ())){
      error_at (identifier->get_locus (),
                "name '%s' already declared in this scope",
                identifier->get_str ().c_str ());
  }
  //cria e insere simbolo
  SymbolPtr sym (new Symbol (Tiger::VARIABLE, identifier->get_str ()));
  scope.get_current_mapping ().insert (sym);

  //cria arvore de declaracao(localizacao, var_decl, nome ident, tipo)
  Tree decl = build_decl (identifier->get_locus (),
                          VAR_DECL,
                          get_identifier (sym->get_name ().c_str ()),
                          type_tree.get_tree ());
  DECL_CONTEXT (decl.get_tree()) = main_fndecl;

  gcc_assert (!stack_var_decl_chain.empty ());
  stack_var_decl_chain.back ().append (decl);

  sym->set_tree_decl (decl);

  Tree stmt = build_tree (DECL_EXPR, identifier->get_locus (), void_type_node, decl);

  return stmt;
}

