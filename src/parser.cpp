using AstScopeId = unsigned;

struct AstNode;

struct AstPredicate
{
  std::list<std::unique_ptr<AstNode>> args;
  std::list<AstNode> body;
  AstScopeId inner_scope;
};

struct AstSymbolPredicateList
{
  std::list<AstPredicate> predicates;
};

struct AstSymbolVariable
{
  unsigned depth = 0;
};

union AstSymbolData
{
  AstSymbolPredicateList predicate_list;
  AstSymbolVariable variable;

  AstSymbolData()
  {
    // Union is ill-formed without constructor.
  }

  ~AstSymbolData()
  {
    // Union is ill-formed without destructor.
  }
};

struct AstSymbolKey
{
  std::string text;
  AstScopeId scope;

  bool operator==(const AstSymbolKey &other) const
  {
    return this->scope == other.scope && this->text == other.text;
  }
};

struct AstSymbol
{
  enum Type
    {
      Predicate_List,
      Variable,
      Atom,
    };

  Type type;
  AstSymbolData as;
  LineInfo line_info;

  AstSymbol(AstSymbol &other) = delete;
  AstSymbol(AstSymbol &&other) = delete;
  AstSymbol &operator=(AstSymbol &other) = delete;

  AstSymbol(Type type)
    : type(type)
  {
    switch (type)
      {
      case AstSymbol::Predicate_List:
        new (&as.predicate_list) AstSymbolPredicateList{ };
        break;
      case AstSymbol::Variable:
        new (&as.variable) AstSymbolVariable{ };
        break;
      case AstSymbol::Atom:
        break;
      }
  }

  ~AstSymbol()
  {
    switch (type)
      {
      case AstSymbol::Predicate_List:
        as.predicate_list.~AstSymbolPredicateList();
        break;
      case AstSymbol::Variable:
        as.variable.~AstSymbolVariable();
        break;
      case AstSymbol::Atom:
        break;
      }
  }
};

struct AstNodeBinaryOp
{
  enum Type
    {
      Add,
      Mul,
    };

  Type type;
  std::unique_ptr<AstNode> lhs, rhs;
};

struct AstNodeUnify
{
  std::unique_ptr<AstNode> lhs, rhs;
};

struct AstNodeIs
{
  std::unique_ptr<AstNode> lhs, rhs;
};

struct AstNodeCall
{
  AstSymbol *symbol;
  std::list<std::unique_ptr<AstNode>> args;
};

union AstNodeData
{
  AstNodeBinaryOp binary_op;
  AstNodeUnify unify;
  AstNodeIs is;
  AstNodeCall call;
  AstSymbol *variable;
  AstSymbol *atom;
  uint64_t uint64;

  AstNodeData()
  {
    // Union is ill-formed without constructor.
  }

  ~AstNodeData()
  {
    // Union is ill-formed without destructor.
  }
};

struct AstNode
{
  enum Type
    {
      Binary_Op,
      Unify,
      Is,
      Call,
      Variable,
      Atom,
      Uint64,
    };

  Type type;
  AstNodeData as;
  LineInfo line_info;

  AstNode(const AstNode &other) = delete;
  AstNode &operator=(const AstNode &other) = delete;

  AstNode(Type type)
    : type(type)
  {
    switch (type)
      {
      case AstNode::Binary_Op:
        new (&as.binary_op) AstNodeBinaryOp{ };
        break;
      case AstNode::Unify:
        new (&as.unify) AstNodeUnify{ };
        break;
      case AstNode::Is:
        new (&as.is) AstNodeIs{ };
        break;
      case AstNode::Call:
        new (&as.call) AstNodeCall{ };
        break;
      case AstNode::Variable:
        as.variable = nullptr;
        break;
      case AstNode::Atom:
        as.atom = nullptr;
        break;
      case AstNode::Uint64:
        as.uint64 = 0;
        break;
      }
  }

  AstNode(AstNode &&other)
  {
    type = other.type;
    switch (type)
      {
      case AstNode::Binary_Op:
        new (&as.binary_op) AstNodeBinaryOp{ std::move(other.as.binary_op) };
        break;
      case AstNode::Unify:
        new (&as.unify) AstNodeUnify{ std::move(other.as.unify) };
        break;
      case AstNode::Is:
        new (&as.is) AstNodeIs{ std::move(other.as.is) };
        break;
      case AstNode::Call:
        new (&as.call) AstNodeCall{ std::move(other.as.call) };
        break;
      case AstNode::Variable:
        as.variable = other.as.variable;
        break;
      case AstNode::Atom:
        as.atom = other.as.atom;
        break;
      case AstNode::Uint64:
        as.uint64 = other.as.uint64;
        break;
      }
  }

  ~AstNode()
  {
    switch (type)
      {
      case AstNode::Binary_Op:
        as.binary_op.~AstNodeBinaryOp();
        break;
      case AstNode::Unify:
        as.unify.~AstNodeUnify();
        break;
      case AstNode::Is:
        as.is.~AstNodeIs();
        break;
      case AstNode::Call:
        as.call.~AstNodeCall();
        break;
      case AstNode::Variable:
      case AstNode::Atom:
      case AstNode::Uint64:
        break;
      }
  }
};

struct AstSymbolKeyHash
{
  size_t operator()(const AstSymbolKey &key) const
  {
    auto hash0 = std::hash<std::string>{}(key.text);
    auto hash1 = std::hash<AstScopeId>{}(key.scope);
    return hash0 + 33 * hash1;
  }
};

using AstSymbolTable = std::unordered_map<AstSymbolKey, std::unique_ptr<AstSymbol>, AstSymbolKeyHash>;

struct Ast
{
  constexpr static AstScopeId GLOBAL_SCOPE = 0;
  constexpr static int LOWEST_PREC = INT_MIN + 1;

  AstPredicate goal;
  AstSymbolTable symbols;
  AstScopeId next_scope_id = GLOBAL_SCOPE;
  bool has_goal = false;
};

const char *
ast_symbol_type_to_string(AstSymbol::Type type)
{
  switch (type)
    {
    case AstSymbol::Predicate_List: return "predicate";
    case AstSymbol::Variable:       return "variable";
    case AstSymbol::Atom:           return "atom";
    }

  UNREACHABLE();
}

AstSymbol *
insert_symbol(Ast &ast, Tokenizer &t, Token &id, AstSymbol::Type type)
{
  auto key = AstSymbolKey{ };
  key.text = std::string{ id.text };
  key.scope = Ast::GLOBAL_SCOPE;

  switch (type)
    {
    case AstSymbol::Predicate_List:
    case AstSymbol::Atom:           break;
    case AstSymbol::Variable:       key.scope = ast.next_scope_id; break;
    }

  AstSymbol *result = nullptr;

  {
    auto symbol = std::make_unique<AstSymbol>(type);
    auto [it, was_inserted] = ast.symbols.emplace(std::move(key), std::move(symbol));
    auto &[new_key, symbol_ptr] = *it;
    result = symbol_ptr.get();

    if (was_inserted)
      result->line_info = id.line_info;
  }

  if (result->type != type)
    {
      PRINT_ERROR(t.filepath, id.line_info, "expected %s, but got %s", ast_symbol_type_to_string(type), ast_symbol_type_to_string(result->type));
      exit(EXIT_FAILURE);
    }

  return result;
}

void parse_symbol(Ast &ast, Tokenizer &t);

Ast
parse_top_level(Tokenizer &t)
{
  auto ast = Ast{ };

  while (t.peek() != Token::End_Of_File)
    parse_symbol(ast, t);

  if (!ast.has_goal)
    {
      LineInfo line_info;
      PRINT_ERROR0(t.filepath, line_info, "goal is not defined");
      exit(EXIT_FAILURE);
    }

  return ast;
}

AstNodeBinaryOp::Type
token_type_to_binary_op_type(Token::Type type)
{
  switch (type)
    {
    case Token::Add:   return AstNodeBinaryOp::Add;
    case Token::Mul:   return AstNodeBinaryOp::Mul;
    default: UNREACHABLE();
    }
}

int
prec_of(Token::Type op)
{
  switch (op)
    {
    case Token::Add:   return 1;
    case Token::Mul:   return 2;
    default: return Ast::LOWEST_PREC - 1;
    }
}

std::unique_ptr<AstNode> parse_expr(Ast &ast, Tokenizer &t);

std::list<std::unique_ptr<AstNode>>
parse_arg_list(Ast &ast, Tokenizer &t)
{
  std::list<std::unique_ptr<AstNode>> args;

  t.expect(Token::Open_Paren);

  auto tt = t.peek();
  while (tt != Token::End_Of_File
         && tt != Token::Close_Paren)
    {
      auto node = parse_expr(ast, t);
      args.push_back(std::move(node));

      tt = t.peek();
      if (tt != Token::End_Of_File
          && tt != Token::Close_Paren)
        {
          t.expect(Token::Comma);
          tt = t.peek();
        }
    }

  t.expect(Token::Close_Paren);

  return args;
}

std::unique_ptr<AstNode>
parse_highest_prec(Ast &ast, Tokenizer &t)
{
  auto token = t.grab();
  t.advance();

  switch (token.type)
    {
    case Token::Open_Paren:
      {
        auto result = parse_expr(ast, t);
        t.expect(Token::Close_Paren);

        return result;
      }
    case Token::Variable:
      {
        auto symbol = insert_symbol(ast, t, token, AstSymbol::Variable);
        auto result = std::make_unique<AstNode>(AstNode::Variable);
        result->as.variable = symbol;
        result->line_info = token.line_info;

        return result;
      }
    case Token::Atom:
      {
        if (t.peek() == Token::Open_Paren)
          {
            auto args = parse_arg_list(ast, t);
            auto symbol = insert_symbol(ast, t, token, AstSymbol::Predicate_List);
            auto result = std::make_unique<AstNode>(AstNode::Call);
            result->as.call.symbol = symbol;
            result->as.call.args = std::move(args);
            result->line_info = token.line_info;

            return result;
          }
        else
          {
            auto symbol = insert_symbol(ast, t, token, AstSymbol::Atom);
            auto result = std::make_unique<AstNode>(AstNode::Atom);
            result->as.atom = symbol;
            result->line_info = token.line_info;

            return result;
          }
      }
    case Token::Integer:
      {
        uint64_t value = 0;

        for (auto ch: token.text)
          value = 10 * value + (ch - '0');

        auto result = std::make_unique<AstNode>(AstNode::Uint64);
        result->as.uint64 = value;
        result->line_info = token.line_info;

        return result;
      }
    default:
      {
        PRINT_ERROR(t.filepath, token.line_info, "'%.*s' doesn't start an expression.", (int)token.text.size(), token.text.data());
        exit(EXIT_FAILURE);
      }
    }
}

std::unique_ptr<AstNode>
parse_prec(Ast &ast, Tokenizer &t, int min_prec)
{
  auto lhs = parse_highest_prec(ast, t);
  auto op = t.peek();
  int prev_prec = INT_MAX, curr_prec = prec_of(op);

  while (curr_prec < prev_prec && curr_prec >= min_prec)
    {
      do
        {
          t.advance();

          auto rhs = parse_prec(ast, t, curr_prec + 1);
          auto new_lhs = std::make_unique<AstNode>(AstNode::Binary_Op);
          new_lhs->as.binary_op.type = token_type_to_binary_op_type(op);
          new_lhs->line_info = lhs->line_info;
          new_lhs->as.binary_op.lhs = std::move(lhs);
          new_lhs->as.binary_op.rhs = std::move(rhs);
          lhs = std::move(new_lhs);

          op = t.peek();
        }
      while (curr_prec == prec_of(op));

      prev_prec = curr_prec;
      curr_prec = prec_of(op);
    }

  return lhs;
}

std::unique_ptr<AstNode>
parse_expr(Ast &ast, Tokenizer &t)
{
  return parse_prec(ast, t, Ast::LOWEST_PREC);
}

AstNode
parse_stmt(Ast &ast, Tokenizer &t)
{
  auto lhs = parse_expr(ast, t);

  switch (t.peek())
    {
    case Token::Equal:
      {
        t.advance();
        auto rhs = parse_expr(ast, t);
        auto result = AstNode{ AstNode::Unify };
        result.line_info = lhs->line_info;
        result.as.unify.lhs = std::move(lhs);
        result.as.unify.rhs = std::move(rhs);

        return result;
      }
    case Token::Is:
      {
        t.advance();
        auto rhs = parse_expr(ast, t);
        auto result = AstNode{ AstNode::Is };
        result.line_info = lhs->line_info;
        result.as.is.lhs = std::move(lhs);
        result.as.is.rhs = std::move(rhs);

        return result;
      }
    default:
      return std::move(*lhs);
    }
}

void
parse_symbol(Ast &ast, Tokenizer &t)
{
  auto head = std::list<std::unique_ptr<AstNode>>{ };
  auto body = std::list<AstNode>{ };
  auto atom_id = t.grab();

  ++ast.next_scope_id;

  if (t.peek() == Token::Atom)
    {
      t.advance();
      head = parse_arg_list(ast, t);
    }

  if (t.peek() == Token::Left_Imply)
    {
      t.advance();

      auto tt = t.peek();
      while (tt != Token::End_Of_File
             && tt != Token::Dot)
        {
          auto node = parse_stmt(ast, t);
          body.push_back(std::move(node));

          tt = t.peek();
          if (tt != Token::End_Of_File
              && tt != Token::Dot)
            {
              t.expect(Token::Comma);
              tt = t.peek();
            }
        }
    }

  --ast.next_scope_id;

  t.expect(Token::Dot);

  if (!head.empty())
    {
      auto symbol = insert_symbol(ast, t, atom_id, AstSymbol::Predicate_List);
      // Override line info if the symbol was seen before it was defined.
      symbol->line_info = atom_id.line_info;

      auto predicate = AstPredicate{ };
      predicate.args = std::move(head);
      predicate.body = std::move(body);
      predicate.inner_scope = ast.next_scope_id + 1;

      symbol->as.predicate_list.predicates.push_back(std::move(predicate));
    }
  else if (!body.empty())
    {
      if (ast.has_goal)
        {
          PRINT_ERROR0(t.filepath, atom_id.line_info, "goal is already defined");
          exit(EXIT_FAILURE);
        }

      ast.has_goal = true;
      ast.goal.body = std::move(body);
      ast.goal.inner_scope = ast.next_scope_id + 1;
    }
  else
    {
      PRINT_ERROR(t.filepath, atom_id.line_info, "'%.*s' doesn't start predicate or goal definition", (int)atom_id.text.size(), atom_id.text.data());
      exit(EXIT_FAILURE);
    }
}

Ast
parse(const char *filepath)
{
  auto source = read_entire_file(filepath);
  auto t = Tokenizer{ };
  t.filepath = filepath;
  t.source = source;

  return parse_top_level(t);
}
