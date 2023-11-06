#define UNREACHABLE() assert(false && "unreachable")

std::string
read_entire_file(const char *filepath)
{
  auto file = std::ifstream{ filepath };
  auto result = std::string{ };

  if (!file.is_open())
    {
      fprintf(stderr, "error: couldn't open '%s'.", filepath);
      exit(EXIT_FAILURE);
    }

  {
    struct stat stats;
    if (stat(filepath, &stats) == -1)
      goto report_error;
    result.resize(stats.st_size + 1);
  }

  file.read(&result[0], result.size() - 1);
  result.back() = '\0';
  file.close();

  return result;

 report_error:
  std::cerr << strerror(errno);
  exit(EXIT_FAILURE);
}

bool
is_prefix(std::string_view prefix, std::string_view string)
{
  if (prefix.size() > string.size())
    return false;
  else
    return memcmp(prefix.data(), string.data(), prefix.size()) == 0;
}
