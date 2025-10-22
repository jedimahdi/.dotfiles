#define _XOPEN_SOURCE 700
#include <time.h>
#include "utils.h"
#include "lx_process.h"
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <linux/limits.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>

void log_title(const char *title) {
  printf("\n=== %s ===\n", title);
}

void log_info(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  printf(ANSI_DIM "  • " ANSI_RESET);
  vprintf(fmt, ap);
  printf("\n");
  va_end(ap);
}

void log_warn(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  printf(ANSI_YELLOW "  ! " ANSI_RESET);
  vprintf(fmt, ap);
  printf("\n");
  va_end(ap);
}

void log_success(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  printf(ANSI_GREEN "  ✓ " ANSI_RESET);
  vprintf(fmt, ap);
  printf("\n");
  va_end(ap);
}

void log_fatal(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  printf(ANSI_RED "  ✗ " ANSI_RESET);
  vprintf(fmt, ap);
  size_t len = strlen(fmt);
  if (len > 0 && fmt[len - 1] == ':') {
    printf(" %s", strerror(errno));
  }
  printf("\n");
  va_end(ap);
  exit(1);
}

void *xmalloc(size_t size) {
  void *p = malloc(size);
  if (!p) log_fatal("malloc failed:");
  return p;
}

void xmkdir(const char *path, mode_t mode) {
  if (mkdir(path, mode) != 0 && errno != EEXIST) {
    log_fatal("mkdir failed on %s:", path);
  }
}

FILE *xfopen(const char *path, const char *mode) {
  FILE *f = fopen(path, mode);
  if (!f) log_fatal("fopen failed on %s:", path);
  return f;
}

bool xstat(const char *path, struct stat *st) {
  if (stat(path, st) == 0) {
    return true;
  }
  if (errno == ENOENT) {
    return false;
  }
  log_fatal("stat failed on %s:", path);
  return false; // unreachable
}

bool xlstat(const char *path, struct stat *st) {
  if (lstat(path, st) == 0) {
    return true;
  }
  if (errno == ENOENT) {
    return false;
  }
  log_fatal("lstat failed on %s:", path);
  return false; // unreachable
}

ssize_t xreadlink(const char *path, char *buf, size_t buflen) {
  ssize_t len = readlink(path, buf, buflen - 1);
  if (len < 0) {
    log_fatal("readlink failed on %s:", path);
  }
  buf[len] = '\0';
  return len;
}

char *expand_path(const char *path, char *buf, size_t buflen) {
  if (!path || !buf) {
    log_fatal("expand_path: invalid arguments");
  }

  const char *home = getenv("HOME");
  if (!home) {
    log_fatal("$HOME not set");
  }

  const char *prefix = NULL;
  const char *suffix = NULL;

  if (strncmp(path, "$HOME", 5) == 0) {
    prefix = home;
    suffix = path + 5;
  } else if (strncmp(path, "$XDG_CONFIG_HOME", 16) == 0) {
    prefix = getenv("XDG_CONFIG_HOME");
    if (!prefix) {
      snprintf(buf, buflen, "%s/.config%s", home, path + 16);
      return buf;
    }
    suffix = path + 16;
  } else if (strncmp(path, "$XDG_CACHE_HOME", 15) == 0) {
    prefix = getenv("XDG_CACHE_HOME");
    if (!prefix) {
      snprintf(buf, buflen, "%s/.cache%s", home, path + 15);
      return buf;
    }
    suffix = path + 15;
  } else if (strncmp(path, "$XDG_STATE_HOME", 15) == 0) {
    prefix = getenv("XDG_STATE_HOME");
    if (!prefix) {
      snprintf(buf, buflen, "%s/.local/state%s", home, path + 15);
      return buf;
    }
    suffix = path + 15;
  } else if (strncmp(path, "$DOTFILES", 9) == 0) {
    prefix = getenv("DOTFILES");
    if (!prefix) {
      snprintf(buf, buflen, "%s/.dotfiles%s", home, path + 9);
      return buf;
    }
    suffix = path + 9;
  } else {
    // No expansion → copy as is
    snprintf(buf, buflen, "%s", path);
    return buf;
  }

  if (!prefix) {
    log_fatal("expand_path: missing prefix for %s", path);
  }

  snprintf(buf, buflen, "%s%s", prefix, suffix);
  return buf;
}

int cmd_run(const char *cmd, cmd_flags flags) {
  lx_fd_mode stdout_mode = (flags & CMD_DISCARD_STDOUT) ? LX_FD_NULL : LX_FD_INHERIT;
  lx_fd_mode stderr_mode = (flags & CMD_DISCARD_STDERR) ? LX_FD_NULL : LX_FD_INHERIT;
  lx_run_opts opts = {.stdout_mode = stdout_mode, .stderr_mode = stderr_mode};
  int status = lx_run_sync_shell(&opts, cmd);
  if (status < 0) {
    log_fatal("Failed to run cmd '%s':", cmd);
  }
  return status;
}

int cmd_runf(const char *fmt, ...) {
  char buf[PATH_MAX];
  va_list ap;
  va_start(ap, fmt);
  int n = vsnprintf(buf, sizeof(buf), fmt, ap);
  va_end(ap);

  if (n < 0 || (size_t)n >= sizeof(buf)) {
    log_fatal("cmd_runf: command string truncated");
  }

  return cmd_run(buf, CMD_NO_DISCARD);
}

int cmd_runf_quiet(const char *fmt, ...) {
  char buf[PATH_MAX];
  va_list ap;
  va_start(ap, fmt);
  int n = vsnprintf(buf, sizeof(buf), fmt, ap);
  va_end(ap);

  if (n < 0 || (size_t)n >= sizeof(buf)) {
    log_fatal("cmd_runf: command string truncated");
  }

  return cmd_run(buf, CMD_DISCARD_STDERR | CMD_DISCARD_STDOUT);
}

int cmd_run_argv(const char *const argv[], cmd_flags flags) {
  if (!argv || !argv[0]) {
    log_fatal("cmd_run_argv called with empty argv");
  }

  lx_fd_mode stdout_mode = (flags & CMD_DISCARD_STDOUT) ? LX_FD_NULL : LX_FD_INHERIT;
  lx_fd_mode stderr_mode = (flags & CMD_DISCARD_STDERR) ? LX_FD_NULL : LX_FD_INHERIT;
  lx_run_opts opts = {.stdout_mode = stdout_mode, .stderr_mode = stderr_mode};

  int status = lx_run_sync_argv(&opts, (const char **)argv);
  if (status < 0) {
    log_fatal("Failed to run argv[0]='%s'", argv[0]);
  }
  return status;
}

void cmd_run_or_die_argv(const char *const argv[], cmd_flags flags) {
  int status = cmd_run_argv(argv, flags);
  if (status != 0) {
    log_fatal("Command '%s' failed with exit code %d", argv[0], status);
  }
}

static char *cmd_getline_flags(const char *cmd, char *buf, size_t buflen, cmd_flags flags) {
  lx_child child;
  lx_fd_mode stderr_mode = (flags & CMD_DISCARD_STDERR) ? LX_FD_NULL : LX_FD_INHERIT;
  lx_run_opts opts = {.stdout_mode = LX_FD_PIPE, .stderr_mode = stderr_mode};

  if (!lx_run_async_shell(&opts, &child, cmd)) {
    log_fatal("Failed to run cmd '%s':", cmd);
  }

  size_t len = 0;
  buf[0] = '\0';
  while (len < buflen - 1) {
    ssize_t nread = read(child.stdout_fd, buf + len, buflen - len - 1);
    if (nread < 0) {
      log_fatal("Failed to read from cmd '%s':", cmd);
    }
    if (nread == 0) break;

    char *p = memchr(buf + len, '\n', buflen - len);
    if (p) {
      *p = '\0';
      break;
    }
    len += nread;
  }

  buf[buflen - 1] = '\0';
  close(child.stdout_fd);
  lx_child_wait(&child);

  if (buf[0] == '\0') {
    log_fatal("Command '%s' produced no output", cmd);
  }

  return buf;
}

char *cmd_getline(const char *cmd, char *buf, size_t buflen) {
  return cmd_getline_flags(cmd, buf, buflen, 0);
}

char *cmd_getline_quiet(const char *cmd, char *buf, size_t buflen) {
  return cmd_getline_flags(cmd, buf, buflen, CMD_DISCARD_STDERR);
}

static char *cmd_capture_flags(const char *cmd, cmd_flags flags) {
  lx_child child;
  lx_fd_mode stderr_mode = (flags & CMD_DISCARD_STDERR) ? LX_FD_NULL : LX_FD_INHERIT;
  lx_run_opts opts = {.stdout_mode = LX_FD_PIPE, .stderr_mode = stderr_mode};

  if (!lx_run_async_shell(&opts, &child, cmd)) {
    log_fatal("Failed to run cmd '%s':", cmd);
  }

  size_t cap = 4096;
  size_t len = 0;
  char *buf = malloc(cap);
  if (!buf) {
    close(child.stdout_fd);
    log_fatal("Failed to malloc in running cmd %s", cmd);
  }

  for (;;) {
    if (len + 1 >= cap) { // leave space for '\0'
      cap *= 2;
      char *tmp = realloc(buf, cap);
      if (!tmp) {
        free(buf);
        close(child.stdout_fd);
        log_fatal("Failed to realloc in running cmd %s", cmd);
      }
      buf = tmp;
    }

    ssize_t nread = read(child.stdout_fd, buf + len, cap - len - 1);
    if (nread < 0) {
      free(buf);
      close(child.stdout_fd);
      log_fatal("Failed to read from cmd '%s'", cmd);
    }
    if (nread == 0) break; // EOF

    len += nread;
  }

  buf[len] = '\0';
  close(child.stdout_fd);
  lx_child_wait(&child);
  return buf;
}

char *cmd_capture(const char *cmd) {
  return cmd_capture_flags(cmd, 0);
}

char *cmd_capture_quiet(const char *cmd) {
  return cmd_capture_flags(cmd, CMD_DISCARD_STDERR);
}

void cmd_run_or_die(const char *cmd, cmd_flags flags) {
  int status = cmd_run(cmd, flags);
  if (status != 0) {
    log_fatal("Command '%s' failed with exit code %d", cmd, status);
  }
}

char *expand_path_buf(char *out, size_t outsz, const char *path) {
  if (!out || !path) return NULL;

  const char *prefix = NULL;
  const char *suffix = NULL;

  // $HOME
  if (strncmp(path, "$HOME", 5) == 0) {
    prefix = getenv("HOME");
    suffix = path + 5;
  }
  // $XDG_CONFIG_HOME
  else if (strncmp(path, "$XDG_CONFIG_HOME", 16) == 0) {
    prefix = getenv("XDG_CONFIG_HOME");
    if (!prefix) {
      const char *home = getenv("HOME");
      if (!home) return NULL;
      snprintf(out, outsz, "%s/.config%s", home, path + 16);
      return out;
    }
    suffix = path + 16;
  }
  // $XDG_CACHE_HOME
  else if (strncmp(path, "$XDG_CACHE_HOME", 15) == 0) {
    prefix = getenv("XDG_CACHE_HOME");
    if (!prefix) {
      const char *home = getenv("HOME");
      if (!home) return NULL;
      snprintf(out, outsz, "%s/.cache%s", home, path + 15);
      return out;
    }
    suffix = path + 15;
  }
  // $XDG_STATE_HOME
  else if (strncmp(path, "$XDG_STATE_HOME", 15) == 0) {
    prefix = getenv("XDG_STATE_HOME");
    if (!prefix) {
      const char *home = getenv("HOME");
      if (!home) return NULL;
      snprintf(out, outsz, "%s/.local/state%s", home, path + 15);
      return out;
    }
    suffix = path + 15;
  }
  // $DOTFILES
  else if (strncmp(path, "$DOTFILES", 9) == 0) {
    prefix = getenv("DOTFILES");
    if (!prefix) {
      const char *home = getenv("HOME");
      if (!home) return NULL;
      snprintf(out, outsz, "%s/.dotfiles%s", home, path + 9);
      return out;
    }
    suffix = path + 9;
  }
  // No expansion → copy as is
  else {
    snprintf(out, outsz, "%s", path);
    return out;
  }

  if (!prefix) return NULL;
  snprintf(out, outsz, "%s%s", prefix, suffix);
  return out;
}

char *sanitize_path_for_filename(const char *path, char *dst, size_t dstlen) {
  if (!dst || dstlen == 0 || !path) {
    log_fatal("sanitize_path_for_filename called with invalid args");
  }

  size_t len = strlen(path);
  if (len >= dstlen) {
    log_fatal("Path too long to sanitize: %s", path);
  }

  for (size_t i = 0; i < len; i++) {
    dst[i] = (path[i] == '/') ? '_' : path[i];
  }
  dst[len] = '\0';

  return dst;
}

void backup_system_path(const char *path) {
  char cache_buf[PATH_MAX];
  if (!expand_path_buf(cache_buf, sizeof(cache_buf), "$XDG_CACHE_HOME/bootstrap_backups")) {
    log_fatal("Failed to expand XDG_CACHE_HOME for backups");
  }

  char sanitized[PATH_MAX];
  sanitize_path_for_filename(path, sanitized, sizeof(sanitized));

  char backup[PATH_MAX * 2 + 128];
  snprintf(backup, sizeof(backup), "%s/%s.%ld", cache_buf, sanitized, time(NULL));

  const char *argv[] = {"sudo", "mv", path, backup, NULL};
  int status = cmd_run_argv(argv, 0);
  if (status != 0) {
    log_fatal("Failed to back up '%s' to '%s'", path, backup);
  }
  log_warn("Backed up existing '%s' -> '%s'", path, backup);
}

void backup_path(const char *path) {
  char cache_buf[PATH_MAX];
  if (!expand_path_buf(cache_buf, sizeof(cache_buf), "$XDG_CACHE_HOME/bootstrap_backups")) {
    log_fatal("Failed to expand XDG_CACHE_HOME for backups");
  }

  char sanitized[PATH_MAX];
  sanitize_path_for_filename(path, sanitized, sizeof(sanitized));

  char backup[PATH_MAX * 2 + 128];
  snprintf(backup, sizeof(backup), "%s/%s.%ld", cache_buf, sanitized, time(NULL));

  if (rename(path, backup) != 0) {
    log_fatal("Failed to back up '%s' to '%s'", path, backup);
  }
  log_warn("Backed up existing '%s' -> '%s'", path, backup);
}

static void ensure_system_service_enabled(const char *service) {
  // cmd_run_argv((const char *[]){"sudo", "systemctl", "--quiet", "is-enabled", service, NULL});
  lx_run_opts opts = {0};

  int status = lx_run_sync(&opts, "sudo", "systemctl", "--quiet", "is-enabled", service);
  if (status < 0) {
    log_fatal("Failed to check %s is enabled:", service);
  }
  if (status == 0) {
    log_info("System service '%s' already enabled", service);
    return;
  }

  status = lx_run_sync(&opts, "sudo", "systemctl", "--quiet", "enable", "--now", service);
  if (status < 0) {
    log_fatal("Failed to enable '%s':", service);
  } else if (status > 0) {
    log_fatal("Failed to enable '%s'", service);
  }
  log_success("Enabled systemd service '%s'", service);
}

static void system_service_restart(const char *service) {
  lx_run_opts opts = {0};
  int status = lx_run_sync(&opts, "sudo", "systemctl", "restart", service);
  if (status != 0) {
    log_fatal("Failed to restart system service '%s'", service);
  }

  log_success("Restarted system service '%s'", service);
}

static void user_service_restart(const char *service) {
  lx_run_opts opts = {0};
  int status = lx_run_sync(&opts, "systemctl", "--user", "restart", service);
  if (status != 0) {
    log_fatal("Failed to restart user service '%s'", service);
  }
  log_success("Restarted user service '%s'", service);
}

static void ensure_user_service_enabled(const char *service) {
  lx_run_opts opts = {0};
  int status = lx_run_sync(&opts, "systemctl", "--user", "--quiet", "is-enabled", service);
  if (status < 0) {
    log_fatal("Failed to check %s is enabled:", service);
  }
  if (status == 0) {
    log_info("User service '%s' already enabled", service);
    return;
  }

  status = lx_run_sync(&opts, "systemctl", "--user", "--quiet", "enable", "--now", service);
  if (status < 0) {
    log_fatal("Failed to enable '%s':", service);
  } else if (status > 0) {
    log_fatal("Failed to enable '%s'", service);
  }
  log_success("Enabled systemd user service '%s'", service);
}

static void ensure_directory_exists(const char *path) {
  char buf[PATH_MAX];
  if (!expand_path_buf(buf, sizeof(buf), path)) {
    log_fatal("Failed to expand path '%s'", path);
  }

  struct stat st;
  if (stat(buf, &st) == 0) {
    if (S_ISDIR(st.st_mode)) {
      log_info("Directory '%s' already exists", buf);
      return;
    } else {
      log_fatal("Path '%s' exists but is not a directory", buf);
    }
  } else if (errno != ENOENT) {
    log_fatal("Failed to stat '%s':", buf);
  }

  // Walk and create intermediate directories
  for (char *p = buf + 1; *p; p++) {
    if (*p == '/') {
      *p = '\0';
      if (mkdir(buf, 0777) != 0 && errno != EEXIST) {
        log_fatal("Failed to create directory '%s':", buf);
      }
      *p = '/';
    }
  }

  if (mkdir(buf, 0777) == 0) {
    log_success("Created directory '%s'", buf);
  } else if (errno == EEXIST) {
    log_info("Directory '%s' already exists", buf);
  } else {
    log_fatal("Failed to create directory '%s':", buf);
  }
}

void ensure_system_directory_exists(const char *path) {
  struct stat st;
  if (stat(path, &st) == 0) {
    if (S_ISDIR(st.st_mode)) {
      log_info("System directory '%s' already exists", path);
      return;
    } else {
      log_fatal("Path '%s' exists but is not a directory", path);
    }
  } else if (errno != ENOENT) {
    log_fatal("Failed to stat '%s':", path);
  }

  lx_run_opts opts = {0};
  int status = lx_run_sync(&opts, "sudo", "mkdir", "-p", path);
  if (status != 0) {
    log_fatal("Failed to create system directory '%s'", path);
  }

  log_success("Created system directory '%s'", path);
}

static bool ensure_symlink_exists(const char *target, const char *linkpath) {
  char target_buf[PATH_MAX];
  char link_buf[PATH_MAX];

  if (!expand_path_buf(target_buf, sizeof(target_buf), target) ||
      !expand_path_buf(link_buf, sizeof(link_buf), linkpath)) {
    log_fatal("Failed to expand symlink target='%s' link='%s'", target, linkpath);
  }

  struct stat st;
  if (lstat(link_buf, &st) == 0) {
    if (S_ISLNK(st.st_mode)) {
      char buf[PATH_MAX];
      ssize_t len = readlink(link_buf, buf, sizeof(buf) - 1);
      if (len < 0) {
        log_fatal("Failed to read symlink '%s':", link_buf);
      }
      buf[len] = '\0';

      if (strcmp(buf, target_buf) == 0) {
        log_info("Symlink '%s' already points to '%s'", link_buf, target_buf);
        return false;
      } else {
        // Wrong symlink → back it up
        backup_path(link_buf);
      }
    } else {
      // Not a symlink → back it up
      backup_path(link_buf);
    }
  } else if (errno != ENOENT) {
    log_fatal("Failed to lstat '%s':", link_buf);
  }

  // Create new symlink
  if (symlink(target_buf, link_buf) != 0) {
    log_fatal("Failed to create symlink '%s' -> '%s'", link_buf, target_buf);
  }
  log_success("Created symlink '%s' -> '%s'", link_buf, target_buf);
  return true;
}

bool ensure_system_symlink_exists(const char *target, const char *linkpath) {
  char target_buf[PATH_MAX];
  char link_buf[PATH_MAX];

  if (!expand_path_buf(target_buf, sizeof(target_buf), target) ||
      !expand_path_buf(link_buf, sizeof(link_buf), linkpath)) {
    log_fatal("Failed to expand symlink target='%s' link='%s'", target, linkpath);
  }

  struct stat st;
  if (lstat(link_buf, &st) == 0) {
    if (S_ISLNK(st.st_mode)) {
      char buf[PATH_MAX];
      ssize_t len = readlink(link_buf, buf, sizeof(buf) - 1);
      if (len < 0) {
        log_fatal("Failed to read existing symlink '%s':", link_buf);
      }
      buf[len] = '\0';

      if (strcmp(buf, target_buf) == 0) {
        log_info("System symlink '%s' already points to '%s'",
                 link_buf, target_buf);
        return false;
      } else {
        backup_system_path(link_buf);
      }
    } else {
      backup_system_path(link_buf);
    }
  } else if (errno != ENOENT) {
    log_fatal("Failed to lstat '%s':", link_buf);
  }

  // Create new symlink with sudo
  lx_run_opts opts = {0};
  int status = lx_run_sync(&opts, "sudo", "ln", "-s", target_buf, link_buf);
  if (status != 0) {
    log_fatal("Failed to create system symlink '%s' -> '%s'",
              link_buf, target_buf);
  }

  log_success("Created system symlink '%s' -> '%s'", link_buf, target_buf);
  return true;
}

bool is_package_installed(const char *pkg) {
  lx_run_opts opts = {
      .stdout_mode = LX_FD_NULL,
      .stderr_mode = LX_FD_NULL,
  };
  int status = lx_run_sync(&opts, "pacman", "-Q", "--quiet", pkg);
  if (status < 0) {
    log_fatal("Failed to check if package '%s' is installed:", pkg);
  }
  return status == 0;
}

static void ensure_package_installed(const char *pkg) {
  if (is_package_installed(pkg)) {
    log_info("Package '%s' already installed", pkg);
    return;
  }

  lx_run_opts opts = {0};
  int status = lx_run_sync(&opts, "sudo", "pacman", "-S", "--quiet", "--needed", "--noconfirm", pkg);
  if (status < 0) {
    log_fatal("Failed to install package '%s':", pkg);
  } else if (status > 0) {
    log_fatal("pacman failed to install package '%s'", pkg);
  }

  log_success("Installed package '%s'", pkg);
}

void ensure_package_removed(const char *pkg) {
  if (!is_package_installed(pkg)) {
    log_info("Package '%s' already removed", pkg);
    return;
  }

  lx_run_opts opts = {0};
  int status = lx_run_sync(&opts, "sudo", "pacman", "-Rns", "--noconfirm", pkg);
  if (status < 0) {
    log_fatal("Failed to remove package '%s':", pkg);
  } else if (status > 0) {
    log_fatal("pacman failed to remove package '%s'", pkg);
  }

  log_success("Removed package '%s'", pkg);
}

int files_are_identical(const char *a, const char *b) {
  struct stat sa, sb;
  if (stat(a, &sa) != 0 || stat(b, &sb) != 0) {
    return 0;
  }

  // Compare size and mode
  if (sa.st_size != sb.st_size) return 0;
  if ((sa.st_mode & 0777) != (sb.st_mode & 0777)) return 0;

  FILE *fa = fopen(a, "rb");
  FILE *fb = fopen(b, "rb");
  if (!fa || !fb) {
    if (fa) fclose(fa);
    if (fb) fclose(fb);
    return 0;
  }

  int result = 1;
  char buf1[4096], buf2[4096];
  size_t n1, n2;
  while ((n1 = fread(buf1, 1, sizeof(buf1), fa)) > 0 &&
         (n2 = fread(buf2, 1, sizeof(buf2), fb)) > 0) {
    if (n1 != n2 || memcmp(buf1, buf2, n1) != 0) {
      result = 0;
      break;
    }
  }

  fclose(fa);
  fclose(fb);
  return result;
}

static bool ensure_system_file_sync_to(const char *src, const char *dest) {
  char src_buf[PATH_MAX];
  char dest_buf[PATH_MAX];

  if (!expand_path_buf(src_buf, sizeof(src_buf), src) ||
      !expand_path_buf(dest_buf, sizeof(dest_buf), dest)) {
    log_fatal("Failed to expand src='%s' dest='%s'", src, dest);
  }

  struct stat st;
  if (lstat(dest_buf, &st) == 0) {
    if (!S_ISREG(st.st_mode)) {
      log_fatal("Destination '%s' exists but is not a regular file", dest_buf);
    }

    // Compare contents
    if (files_are_identical(src_buf, dest_buf)) {
      log_info("Destination '%s' already identical to '%s'", dest_buf, src_buf);
      return false; // ✅ nothing to do
    }

    // Backup existing file
    backup_system_path(dest_buf);
  } else if (errno != ENOENT) {
    log_fatal("Failed to lstat '%s':", dest_buf);
  }

  // Copy new file into place with sudo
  lx_run_opts opts = {0};
  int status = lx_run_sync(&opts, "sudo", "cp", src_buf, dest_buf);
  if (status != 0) {
    log_fatal("Failed to copy '%s' -> '%s'", src_buf, dest_buf);
  }

  log_success("Copied '%s' -> '%s'", src_buf, dest_buf);
  return true;
}

// Safe replace: replaces all {{KEY}} with VALUE in a line buffer
static void apply_substitutions(char *line, size_t buflen,
                                const struct kv_pair *vars, size_t var_count) {
  char tmp[1024];
  for (size_t i = 0; i < var_count; i++) {
    const char *pattern_fmt = "{{%s}}";
    char pattern[64];
    snprintf(pattern, sizeof(pattern), pattern_fmt, vars[i].key);

    // naive but safe: repeatedly replace until no more matches
    while (strstr(line, pattern)) {
      char *pos = strstr(line, pattern);
      size_t prefix_len = pos - line;
      snprintf(tmp, sizeof(tmp), "%.*s%s%s",
               (int)prefix_len, line,
               vars[i].value,
               pos + strlen(pattern));
      strncpy(line, tmp, buflen - 1);
      line[buflen - 1] = '\0';
    }
  }
}

bool ensure_system_template_sync_to(const char *template_path,
                                    const char *dest_path,
                                    const struct kv_pair *vars,
                                    size_t var_count) {
  char src_buf[PATH_MAX], dest_buf[PATH_MAX];
  if (!expand_path_buf(src_buf, sizeof(src_buf), template_path) ||
      !expand_path_buf(dest_buf, sizeof(dest_buf), dest_path)) {
    log_fatal("Failed to expand paths: src='%s' dest='%s'", template_path, dest_path);
  }

  // Render into a temporary file
  char tmpfile[] = "/tmp/templateXXXXXX";
  int fd = mkstemp(tmpfile);
  if (fd < 0) log_fatal("mkstemp failed");
  fchmod(fd, 0644);
  FILE *out = fdopen(fd, "w");
  FILE *in = fopen(src_buf, "r");
  if (!in || !out) log_fatal("Failed to open template or tmpfile");

  char line[1024];
  while (fgets(line, sizeof(line), in)) {
    apply_substitutions(line, sizeof(line), vars, var_count);
    fputs(line, out);
  }
  fclose(in);
  fclose(out);

  if (ensure_system_file_sync_to(tmpfile, dest_buf)) {
    unlink(tmpfile);
    log_success("Rendered template '%s' -> '%s'", src_buf, dest_buf);
    return true;
  } else {
    unlink(tmpfile);
    return false;
  }
}

// Return first wireless interface (wl*) seen by ip link
int get_first_wireless_ifname(char *buf, size_t buflen) {
  // lx_child child;
  // lx_run_opts opts = {.stdout_mode = LX_FD_PIPE};
  // if (!lx_run_async_shell(&opts, &child, "ip -o link show | awk -F': ' '/wl/ {print $2; exit}'")) return -1;

  FILE *fp = popen("ip -o link show | awk -F': ' '/wl/ {print $2; exit}'", "r");
  if (!fp) return -1;
  if (!fgets(buf, buflen, fp)) {
    pclose(fp);
    return -1;
  }
  buf[strcspn(buf, "\n")] = 0;
  pclose(fp);
  return 0;
}

// Return predictable name (ID_NET_NAME_PATH) for a given ifname
int get_predictable_ifname(const char *ifname, char *buf, size_t buflen) {
  char cmd[128];
  snprintf(cmd, sizeof(cmd),
           "udevadm info /sys/class/net/%s | grep ID_NET_NAME_PATH= | cut -d= -f2",
           ifname);
  FILE *fp = popen(cmd, "r");
  if (!fp) return -1;
  if (!fgets(buf, buflen, fp)) {
    pclose(fp);
    return -1;
  }
  buf[strcspn(buf, "\n")] = 0;
  pclose(fp);
  return 0;
}

void get_mac_address(char *buf, size_t buflen) {
  char ifname[IFNAME_MAX];
  get_first_wireless_ifname(ifname, sizeof(ifname));

  char path[128];
  snprintf(path, sizeof(path), "/sys/class/net/%s/address", ifname);
  FILE *fp = fopen(path, "r");
  if (!fp) log_fatal("Failed to open path '%s' to get mac address", path);
  if (!fgets(buf, buflen, fp)) {
    fclose(fp);
    log_fatal("Cannot read '%s' to get mac address", path);
  }
  buf[strcspn(buf, "\n")] = 0;
  fclose(fp);
}

bool directory_exists(const char *path) {
  char expanded_path[PATH_MAX];
  expand_path(path, expanded_path, sizeof(expanded_path));

  struct stat st;
  xstat(path, &st);
  return S_ISDIR(st.st_mode);
}

bool symlink_points_to(const char *link_path, const char *target_path) {
  char link_path_expanded[PATH_MAX];
  char target_path_expanded[PATH_MAX];
  expand_path(link_path, link_path_expanded, sizeof(link_path_expanded));
  expand_path(target_path, target_path_expanded, sizeof(target_path_expanded));

  struct stat st;
  if (!xlstat(link_path_expanded, &st)) {
    return false; // dst does not exist
  }

  if (!S_ISLNK(st.st_mode)) {
    return false; // exists but not a symlink
  }

  char target[PATH_MAX];
  xreadlink(link_path_expanded, target, sizeof(target));

  return strcmp(target, target_path_expanded) == 0;
}
