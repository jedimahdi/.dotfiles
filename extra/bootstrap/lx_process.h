#ifndef LX_PROCESS_H_
#define LX_PROCESS_H_

#include <stdbool.h>
#include <sys/types.h>

#ifndef LX_ASSERT
#include <assert.h>
#define LX_ASSERT assert
#endif

typedef enum {
  LX_FD_INHERIT,
  LX_FD_NULL,
  LX_FD_USE,
  LX_FD_PIPE,
  LX_FD_FILE,
} lx_fd_mode;

typedef struct {
  lx_fd_mode mode;
  int fd;
  const char *path;
  bool append;
} lx_fd_spec;

typedef struct {
  lx_fd_spec stdin_spec;
  lx_fd_spec stdout_spec;
  lx_fd_spec stderr_spec;
} lx_spawn_config;

typedef struct {
  pid_t pid;
  int stdin_fd;
  int stdout_fd;
  int stderr_fd;
  bool valid;
  int err;
} lx_child;

int lx_child_wait(lx_child *child);

lx_child lx_spawn_argv(const lx_spawn_config *cfg, const char **argv);
lx_child lx_spawn_shell(const lx_spawn_config *cfg, const char *shell);
int lx_spawn_argv_and_wait(const lx_spawn_config *cfg, const char **argv);
int lx_spawn_shell_and_wait(const lx_spawn_config *cfg, const char *shell);

#define lx_spawn(cfg, command, ...) lx_spawn_argv((cfg), (const char *[]){command, ##__VA_ARGS__, NULL})
#define lx_spawn_and_wait(cfg, command, ...) lx_spawn_argv_and_wait((cfg), (const char *[]){command, ##__VA_ARGS__, NULL})

#endif
