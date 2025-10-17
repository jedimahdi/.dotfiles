#include "lx_process.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>

static void child_setup_spec(lx_fd_mode mode, lx_fd_payload payload, int fd, int pipes[2]) {
  switch (mode) {
  case LX_FD_INHERIT:
    break;
  case LX_FD_NULL: {
    int devnull = open("/dev/null", fd == STDIN_FILENO ? O_RDONLY : O_WRONLY);
    if (devnull < 0) _exit(127);
    dup2(devnull, fd);
    close(devnull);
    break;
  }
  case LX_FD_USE:
    dup2(payload.fd, fd);
    if (payload.fd != fd && payload.fd != STDIN_FILENO && payload.fd != STDOUT_FILENO && payload.fd != STDERR_FILENO) {
      close(payload.fd);
    }
    break;
  case LX_FD_PIPE:
    if (fd == STDIN_FILENO) {
      dup2(pipes[0], fd);
    } else {
      dup2(pipes[1], fd);
    }
    close(pipes[0]);
    close(pipes[1]);
    break;
  case LX_FD_FILE_APPEND:
  case LX_FD_FILE: {
    int file_fd;
    if (fd == STDIN_FILENO) {
      file_fd = open(payload.path, O_RDONLY);
    } else {
      int flags = O_WRONLY | O_CREAT;
      flags |= mode == LX_FD_FILE_APPEND ? O_APPEND : O_TRUNC;
      file_fd = open(payload.path, flags, 0666);
    }
    if (file_fd < 0) _exit(127);
    if (dup2(file_fd, fd) < 0) _exit(127);
    close(file_fd);
    break;
  }
  default:
    _exit(127);
  }
}

bool lx_run_async_argv(const lx_run_opts *opts, lx_child *child, const char **argv) {
  child->pid = -1;
  int pipe_in[2] = {-1, -1};
  int pipe_out[2] = {-1, -1};
  int pipe_err[2] = {-1, -1};

  if (opts->stdin_mode == LX_FD_PIPE && pipe(pipe_in) < 0) goto fail;
  if (opts->stdout_mode == LX_FD_PIPE && pipe(pipe_out) < 0) goto fail;
  if (opts->stderr_mode == LX_FD_PIPE && pipe(pipe_err) < 0) goto fail;

  pid_t pid = fork();
  if (pid < 0) goto fail;

  if (pid == 0) {
    child_setup_spec(opts->stdin_mode, opts->stdin_payload, STDIN_FILENO, pipe_in);
    child_setup_spec(opts->stdout_mode, opts->stdout_payload, STDOUT_FILENO, pipe_out);
    child_setup_spec(opts->stderr_mode, opts->stderr_payload, STDERR_FILENO, pipe_err);

    execvp(argv[0], (char *const *)argv);
    _exit(127);
  }

  child->pid = pid;
  child->stdin_fd = -1;
  child->stdout_fd = -1;
  child->stderr_fd = -1;

  if (opts->stdin_mode == LX_FD_PIPE) {
    close(pipe_in[0]);
    child->stdin_fd = pipe_in[1];
  }
  if (opts->stdout_mode == LX_FD_PIPE) {
    close(pipe_out[1]);
    child->stdout_fd = pipe_out[0];
  }
  if (opts->stderr_mode == LX_FD_PIPE) {
    close(pipe_err[1]);
    child->stderr_fd = pipe_err[0];
  }

  return true;

fail:
  if (pipe_in[0] != -1) close(pipe_in[0]);
  if (pipe_in[1] != -1) close(pipe_in[1]);
  if (pipe_out[0] != -1) close(pipe_out[0]);
  if (pipe_out[1] != -1) close(pipe_out[1]);
  if (pipe_err[0] != -1) close(pipe_err[0]);
  if (pipe_err[1] != -1) close(pipe_err[1]);
  return false;
}

int lx_run_sync_argv(const lx_run_opts *opts, const char **argv) {
  lx_child c;
  if (!lx_run_async_argv(opts, &c, argv)) return -1;
  return lx_child_wait(&c);
}

int lx_child_wait(lx_child *child) {
  if (!child || child->pid < 0) return -1;
  int status = 0;
  if (waitpid(child->pid, &status, 0) < 0) return -1;
  if (WIFEXITED(status)) return WEXITSTATUS(status);
  if (WIFSIGNALED(status)) return 128 + WTERMSIG(status);
  return -1;
}

bool lx_run_async_shell(const lx_run_opts *opts, lx_child *child, const char *shell) {
  return lx_run_async_argv(opts, child, (const char *[]){"/bin/sh", "-c", shell, NULL});
}

int lx_run_sync_shell(const lx_run_opts *opts, const char *shell) {
  lx_child c;
  if (!lx_run_async_shell(opts, &c, shell)) return -1;
  return lx_child_wait(&c);
}
