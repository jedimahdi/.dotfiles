#ifndef LX_PROCESS_H_
#define LX_PROCESS_H_

#include <stdbool.h>
#include <sys/types.h>

#ifndef LX_ASSERT
#include <assert.h>
#define LX_ASSERT assert
#endif

typedef enum {
  LX_FD_INHERIT = 0,
  LX_FD_NULL,
  LX_FD_USE,
  LX_FD_PIPE,
  LX_FD_FILE,
  LX_FD_FILE_APPEND,
} lx_fd_mode;

typedef union {
  int fd;
  const char *path;
} lx_fd_payload;

typedef struct {
  lx_fd_mode stdin_mode;
  lx_fd_payload stdin_payload;

  lx_fd_mode stdout_mode;
  lx_fd_payload stdout_payload;

  lx_fd_mode stderr_mode;
  lx_fd_payload stderr_payload;
} lx_run_opts;

typedef struct {
  pid_t pid;
  int stdin_fd;
  int stdout_fd;
  int stderr_fd;
  bool valid;
  int err;
} lx_child;

int lx_child_wait(lx_child *child);

bool lx_run_async_argv(const lx_run_opts *opts, lx_child *child, const char **argv);
bool lx_run_async_shell(const lx_run_opts *opts, lx_child *child, const char *shell);
int lx_run_sync_argv(const lx_run_opts *opts, const char **argv);
int lx_run_sync_shell(const lx_run_opts *opts, const char *shell);

#define lx_run_async(opts, child, program, ...) lx_run_async_argv((opts), (child), (const char *[]){program, ##__VA_ARGS__, NULL})
#define lx_run_sync(opts, program, ...) lx_run_sync_argv((opts), (const char *[]){program, ##__VA_ARGS__, NULL})

#endif
