#define PRINT_ERROR0(filepath, line_info, message) fprintf(stderr, "%s:%zu:%zu: error: " message "\n", filepath, (line_info).line, (line_info).column)
#define PRINT_ERROR(filepath, line_info, message, ...) fprintf(stderr, "%s:%zu:%zu: error: " message "\n", filepath, (line_info).line, (line_info).column, __VA_ARGS__)

struct LineInfo
{
  size_t offset = 0, line = 1, column = 1;
};

struct Token
{
  enum Type
    {
      Add,
      Mul,

      Open_Paren,
      Close_Paren,
      Comma,
      Dot,
      Equal,
      Left_Imply,

      Is,

      Variable,
      Atom,
      Integer,

      End_Of_File,
    };

  Type type;
  std::string_view text;
  LineInfo line_info;
};

struct Tokenizer
{
  constexpr static uint8_t LOOKAHEAD = 2;

  Token tokens[LOOKAHEAD];
  uint8_t token_start = 0;
  uint8_t token_count = 0;
  LineInfo line_info;
  const char *filepath;
  std::string_view source;

  Token grab()
  {
    if (token_count == 0)
      buffer_token();

    return tokens[token_start];
  }

  Token::Type peek()
  {
    if (token_count == 0)
      buffer_token();

    return tokens[token_start].type;
  }

  void advance()
  {
    assert(token_count > 0);
    ++token_start;
    token_start %= LOOKAHEAD;
    --token_count;
  }

  void advance_line_info(char ch)
  {
    ++line_info.offset;
    ++line_info.column;
    if (ch == '\n')
      {
        ++line_info.line;
        line_info.column = 1;
      }
  }

  void buffer_token()
  {
    auto at = &source[line_info.offset];

    do
      {
        while (isspace(*at))
          advance_line_info(*at++);

        if (at[0] == '/' && at[1] == '/')
          while (*at != '\0' && *at != '\n')
            advance_line_info(*at++);
        else
          break;
      }
    while (*at != '\0');

    auto token = Token{ };
    token.type = Token::End_Of_File;
    token.text = { at, 0 };
    token.line_info = line_info;

    if (*at == '\0')
      ;
    else if (isdigit(*at))
      {
        do
          advance_line_info(*at++);
        while (isdigit(*at));

        token.type = Token::Integer;
        token.text = { token.text.data(), size_t(at - token.text.data()) };
      }
    else if (isupper(*at) || *at == '_')
      {
        do
          advance_line_info(*at++);
        while (isalnum(*at) || *at == '_');

        token.type = Token::Variable;
        token.text = { token.text.data(), size_t(at - token.text.data()) };
      }
    else if (isalpha(*at))
      {
        do
          advance_line_info(*at++);
        while (isalnum(*at) || *at == '_');

        token.type = Token::Atom;
        token.text = { token.text.data(), size_t(at - token.text.data()) };

        struct ReservedKeyword
        {
          std::string_view text;
          Token::Type type;
        };

        constexpr ReservedKeyword keywords[] = {
          { "is", Token::Is },
        };

        for (auto &keyword: keywords)
          {
            if (token.text == keyword.text)
              {
                token.type = keyword.type;
                break;
              }
          }
      }
    else
      {
        struct ReservedSymbol
        {
          std::string_view text;
          Token::Type type;
        };

        constexpr ReservedSymbol symbols[] = {
          { "+", Token::Add },
          { "*", Token::Mul },
          { "(", Token::Open_Paren },
          { ")", Token::Close_Paren },
          { ",", Token::Comma },
          { ".", Token::Dot },
          { "=", Token::Equal },
          { "<=", Token::Left_Imply },
        };

        auto text = std::string_view{ token.text.data(), size_t(source.end() - at) };

        for (auto &symbol: symbols)
          {
            if (is_prefix(symbol.text, text))
              {
                at += symbol.text.size();
                line_info.offset += symbol.text.size();
                line_info.column += symbol.text.size();

                token.type = symbol.type;
                token.text = { token.text.data(), symbol.text.size() };

                goto push_token;
              }
          }

        PRINT_ERROR(filepath, token.line_info, "unrecognized character '%c'", *at);
        exit(EXIT_FAILURE);
      }

  push_token:
    assert(token_count < LOOKAHEAD);
    uint8_t index = (token_start + token_count) % LOOKAHEAD;
    tokens[index] = token;
    ++token_count;
  }
};

std::string_view
token_type_to_string(Token::Type type)
{
  switch (type)
    {
    case Token::Add:         return "'+'";
    case Token::Mul:         return "'*'";
    case Token::Open_Paren:  return "'('";
    case Token::Close_Paren: return "')'";
    case Token::Comma:       return "','";
    case Token::Dot:         return "'.'";
    case Token::Equal:       return "'='";
    case Token::Left_Imply:  return "'<='";
    case Token::Is:          return "'is'";
    case Token::Variable:    return "variable";
    case Token::Atom:        return "atom";
    case Token::Integer:     return "integer";
    case Token::End_Of_File: return "EOF";
    }

  UNREACHABLE();
}
