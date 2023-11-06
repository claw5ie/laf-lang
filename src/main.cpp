#include <iostream>
#include <fstream>

#include <cstring>
#include <cassert>
#include <cstdint>

#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

#include "utils.cpp"
#include "tokenizer.cpp"

int
main(int argc, char **argv)
{
  auto filepath = argc <= 1 ? "examples/debug" : argv[1];
  auto source = read_entire_file(filepath);
  auto t = Tokenizer{ };
  t.filepath = filepath;
  t.source = source;

  while (true)
    {
      auto token = t.grab();
      t.advance();

      std::cout << token_type_to_string(token.type)
                << ":\n    text:   '" << token.text << "'"
                << "\n    line:   " << token.line_info.line
                << "\n    column: " << token.line_info.column
                << "\n    offset: " << token.line_info.offset << '\n';

      if (token.type == Token::End_Of_File)
        break;
    }
}
