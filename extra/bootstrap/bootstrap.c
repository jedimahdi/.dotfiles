#include <sys/stat.h>
#include <sys/types.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

// ANSI color codes
#define COLOR_RED "\x1b[31m"
#define COLOR_YELLOW "\x1b[33m"
#define COLOR_GREEN "\x1b[32m"
#define COLOR_RESET "\x1b[0m"

#define LOG_FATAL(fmt, ...)                            \
  do {                                                 \
    const char *fstr = (fmt);                          \
    size_t len = strlen(fstr);                         \
    fprintf(stderr, COLOR_RED "[FATAL] " COLOR_RESET); \
    fprintf(stderr, fmt, ##__VA_ARGS__);               \
    if (len > 0 && fstr[len - 1] == ':') {             \
      fprintf(stderr, " %s", strerror(errno));         \
    }                                                  \
    fprintf(stderr, " (%s:%d)\n", __FILE__, __LINE__); \
    exit(EXIT_FAILURE);                                \
  } while (0)

#define LOG_INFO(msg, ...) printf("[INFO] " msg "\n", ##__VA_ARGS__)

#define HOME "/home/mahdi"

// void ensure_dir(const char *path) {
//   char *tmp = strdup(path);
//   char *p = tmp;
// }

int main(void) {
  mkdir(HOME "/tmp2", 0755);
  if (errno == EEXIST) {
    printf("tes\n");
  }
  LOG_FATAL("mkdir:");

  LOG_INFO("Hello World!");

  return 0;
}
