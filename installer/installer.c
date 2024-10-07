#include "installer.h"
#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/sysmacros.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

bool nob_proc_wait(Nob_Proc proc) {
  if (proc == NOB_INVALID_PROC)
    return false;

  for (;;) {
    int wstatus = 0;
    if (waitpid(proc, &wstatus, 0) < 0) {
      nob_log(NOB_ERROR, "could not wait on command (pid %d): %s", proc,
              strerror(errno));
      return false;
    }

    if (WIFEXITED(wstatus)) {
      int exit_status = WEXITSTATUS(wstatus);
      if (exit_status != 0) {
        nob_log(NOB_ERROR, "command exited with exit code %d", exit_status);
        return false;
      }

      break;
    }

    if (WIFSIGNALED(wstatus)) {
      nob_log(NOB_ERROR, "command process was terminated by %s",
              strsignal(WTERMSIG(wstatus)));
      return false;
    }
  }

  return true;
}

void nob_log(Nob_Log_Level level, const char *fmt, ...) {
  switch (level) {
  case NOB_INFO:
    fprintf(stderr, "[INFO] ");
    break;
  case NOB_WARNING:
    fprintf(stderr, "[WARNING] ");
    break;
  case NOB_ERROR:
    fprintf(stderr, "[ERROR] ");
    break;
  default:
    NOB_ASSERT(0 && "unreachable");
  }

  va_list args;
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
  fprintf(stderr, "\n");
}

void nob_cmd_render(Nob_Cmd cmd, Nob_String_Builder *render) {
  for (size_t i = 0; i < cmd.count; ++i) {
    const char *arg = cmd.items[i];
    if (arg == NULL)
      break;
    if (i > 0)
      nob_sb_append_cstr(render, " ");
    if (!strchr(arg, ' ')) {
      nob_sb_append_cstr(render, arg);
    } else {
      nob_sb_append_cstr(render, "\'");
      nob_sb_append_cstr(render, arg);
      nob_sb_append_cstr(render, "\'");
    }
  }
}

Nob_Proc nob_cmd_run_async(Nob_Cmd cmd) {
  if (cmd.count < 1) {
    nob_log(NOB_ERROR, "Could not run empty command");
    return NOB_INVALID_PROC;
  }
  Nob_String_Builder sb = {0};
  nob_cmd_render(cmd, &sb);
  nob_sb_append_null(&sb);
  nob_log(NOB_INFO, "CMD: %s", sb.items);
  nob_sb_free(sb);
  memset(&sb, 0, sizeof(sb));

  pid_t cpid = fork();
  if (cpid < 0) {
    nob_log(NOB_ERROR, "Could not fork child process: %s", strerror(errno));
    return NOB_INVALID_PROC;
  }

  if (cpid == 0) {
    Nob_Cmd cmd_null = {0};
    nob_da_append_many(&cmd_null, cmd.items, cmd.count);
    nob_cmd_append(&cmd_null, NULL);

    if (execvp(cmd.items[0], (char *const *)cmd_null.items) < 0) {
      nob_log(NOB_ERROR, "Could not exec child process: %s", strerror(errno));
      exit(1);
    }
    NOB_ASSERT(0 && "unreachable");
  }

  return cpid;
}

int main(int argc, char *argv[]) {
  // Nob_Cmd cmd = {0};
  // nob_cmd_append(&cmd, "ls");
  // nob_cmd_run_async(cmd);

  struct stat sb;

  if (argc != 2) {
    fprintf(stderr, "Usage: %s <pathname>\n", argv[0]);
    exit(EXIT_FAILURE);
  }

  if (lstat(argv[1], &sb) == -1) {
    perror("lstat");
    exit(EXIT_FAILURE);
  }

  printf("ID of containing device:  [%x,%x]\n", major(sb.st_dev),
         minor(sb.st_dev));

  printf("File type:                ");

  switch (sb.st_mode & S_IFMT) {
  case S_IFBLK:
    printf("block device\n");
    break;
  case S_IFCHR:
    printf("character device\n");
    break;
  case S_IFDIR:
    printf("directory\n");
    break;
  case S_IFIFO:
    printf("FIFO/pipe\n");
    break;
  case S_IFLNK:
    printf("symlink\n");
    break;
  case S_IFREG:
    printf("regular file\n");
    break;
  case S_IFSOCK:
    printf("socket\n");
    break;
  default:
    printf("unknown?\n");
    break;
  }

  printf("I-node number:            %ju\n", (uintmax_t)sb.st_ino);

  printf("Mode:                     %jo (octal)\n", (uintmax_t)sb.st_mode);

  printf("Link count:               %ju\n", (uintmax_t)sb.st_nlink);
  printf("Ownership:                UID=%ju   GID=%ju\n", (uintmax_t)sb.st_uid,
         (uintmax_t)sb.st_gid);

  printf("Preferred I/O block size: %jd bytes\n", (intmax_t)sb.st_blksize);
  printf("File size:                %jd bytes\n", (intmax_t)sb.st_size);
  printf("Blocks allocated:         %jd\n", (intmax_t)sb.st_blocks);

  printf("Last status change:       %s", ctime(&sb.st_ctime));
  printf("Last file access:         %s", ctime(&sb.st_atime));
  printf("Last file modification:   %s", ctime(&sb.st_mtime));
}
