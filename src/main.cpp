#include <iostream>
#include <fstream>
#include <list>
#include <unordered_map>
#include <memory>

#include <cstring>
#include <cassert>
#include <cstdint>
#include <climits>

#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

#include "utils.cpp"
#include "tokenizer.cpp"
#include "parser.cpp"

int
main(int argc, char **argv)
{
  auto ast = parse(argc <= 1 ? "examples/debug" : argv[1]);
}
