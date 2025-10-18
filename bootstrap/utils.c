#define _XOPEN_SOURCE 700
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

char *expand_path_buf(char *out, size_t outsz, const char *path) {
  if (!out || !path) return NULL;

  const char *prefix = NULL;
  const char *suffix = NULL;

  // $HOME
  if (strncmp(path, "$HOME/", 6) == 0) {
    prefix = getenv("HOME");
    suffix = path + 5;
  }
  // $XDG_CONFIG_HOME
  else if (strncmp(path, "$XDG_CONFIG_HOME/", 17) == 0) {
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
  else if (strncmp(path, "$XDG_CACHE_HOME/", 16) == 0) {
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
  else if (strncmp(path, "$XDG_STATE_HOME/", 16) == 0) {
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

static char g_pathbuf[PATH_MAX];
const char *expand_path(const char *path) {
  if (!path) return NULL;
  expand_path_buf(g_pathbuf, sizeof(g_pathbuf), path);
  return g_pathbuf;
}

void ensure_system_service_enabled(const char *service) {
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

void ensure_user_service_enabled(const char *service) {
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

void ensure_directory_exists(const char *path) {
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

  mode_t old_umask = umask(0);
  umask(old_umask);
  mode_t mode = 0777 & ~old_umask;

  char *p = buf;
  if (p[0] == '/')
    p++;

  for (; *p; p++) {
    if (*p == '/') {
      *p = '\0';
      if (mkdir(buf, mode) != 0 && errno != EEXIST) {
        log_fatal("Failed to create directory '%s':", buf);
      }
      *p = '/';
    }
  }

  if (mkdir(buf, mode) == 0) {
    log_success("Created directory '%s'", buf);
  } else if (errno == EEXIST) {
    log_info("Directory '%s' already exists", buf);
  } else {
    log_fatal("Failed to create directory '%s':", buf);
  }
}

void ensure_symlink_exists(const char *target, const char *linkpath) {
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
        return;
      } else {
        if (unlink(link_buf) != 0) {
          log_fatal("Failed to remove wrong symlink '%s':", link_buf);
        }
        if (symlink(target_buf, link_buf) != 0) {
          log_fatal("Failed to create symlink '%s' -> '%s':", link_buf, target_buf);
        }
        log_success("Replaced symlink '%s' -> '%s'", link_buf, target_buf);
        return;
      }
    } else {
      log_fatal("Path '%s' exists but is not a symlink", link_buf);
    }
  } else if (errno != ENOENT) {
    log_fatal("Failed to lstat '%s':", link_buf);
  }

  // Doesn’t exist → create
  if (symlink(target_buf, link_buf) != 0) {
    log_fatal("Failed to create symlink '%s' -> '%s':", link_buf, target_buf);
  }
  log_success("Created symlink '%s' -> '%s'", link_buf, target_buf);
}

static bool is_package_installed(const char *pkg) {
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

void ensure_package_installed(const char *pkg) {
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
