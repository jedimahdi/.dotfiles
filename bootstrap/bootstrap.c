#include <linux/limits.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "lx_process.h"

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

#define LOG_ITEM(msg, ...) printf(msg "\n", ##__VA_ARGS__)

void ensure_dir(const char *path) {
  struct stat st;
  if (stat(path, &st) == 0) {
    if (S_ISDIR(st.st_mode)) {
      LOG_ITEM("Directory '%s' already exists", path);
      return;
    } else {
      LOG_FATAL("Path '%s' exists but is not a directory", path);
    }
  } else if (errno != ENOENT) {
    LOG_FATAL("Failed to stat '%s':", path);
  }

  mode_t old_umask = umask(0);
  umask(old_umask);

  char *tmp = strdup(path);
  if (!tmp)
    LOG_FATAL("Out of memory while duplicating path");

  char *p = tmp;
  if (p[0] == '/')
    p++;

  for (; *p; p++) {
    if (*p == '/') {
      *p = '\0';
      if (mkdir(tmp, 0777 & ~old_umask) == 0) {
        LOG_ITEM("Created directory '%s'", tmp);
      } else if (errno != EEXIST) {
        LOG_FATAL("Failed to create directory '%s':", tmp);
      }
      *p = '/';
    }
  }

  if (mkdir(tmp, 0777 & ~old_umask) == 0) {
    LOG_ITEM("Created directory '%s'", tmp);
  } else if (errno != EEXIST) {
    LOG_FATAL("Failed to create directory '%s':", tmp);
  }

  free(tmp);
}

void ensure_system_service_enabled(const char *service) {
  lx_run_opts opts = {0};
  int status = lx_run_sync(&opts, "sudo", "systemctl", "--quiet", "is-enabled", service);
  if (status < 0) {
    LOG_FATAL("Failed to check %s is enabled:", service);
  }
  if (status == 0) {
    LOG_ITEM("System service '%s' already enabled", service);
    return;
  }

  status = lx_run_sync(&opts, "sudo", "systemctl", "--quiet", "enable", "--now", service);
  if (status < 0) {
    LOG_FATAL("Failed to enable '%s':", service);
  } else if (status > 0) {
    LOG_FATAL("Failed to enable '%s'", service);
  }
  LOG_ITEM("Enabled systemd service '%s'", service);
}

void ensure_user_service_enabled(const char *service) {
  lx_run_opts opts = {0};
  int status = lx_run_sync(&opts, "systemctl", "--user", "--quiet", "is-enabled", service);
  if (status < 0) {
    LOG_FATAL("Failed to check %s is enabled:", service);
  }
  if (status == 0) {
    LOG_ITEM("User service '%s' already enabled", service);
    return;
  }

  status = lx_run_sync(&opts, "systemctl", "--user", "--quiet", "enable", "--now", service);
  if (status < 0) {
    LOG_FATAL("Failed to enable '%s':", service);
  } else if (status > 0) {
    LOG_FATAL("Failed to enable '%s'", service);
  }
  LOG_ITEM("Enabled systemd user service '%s'", service);
}

static char g_pathbuf[PATH_MAX];
char *expand_path(const char *path) {
  if (!path) return NULL;

  const char *prefix = NULL;
  const char *suffix = NULL;

  if (strncmp(path, "$HOME/", 6) == 0) {
    prefix = getenv("HOME");
    suffix = path + 5;
  } else if (strncmp(path, "$XDG_CONFIG_HOME/", 17) == 0) {
    prefix = getenv("XDG_CONFIG_HOME");
    if (!prefix) {
      const char *home = getenv("HOME");
      if (!home) return NULL;
      snprintf(g_pathbuf, sizeof(g_pathbuf), "%s/.config%s", home, path + 16);
      return g_pathbuf;
    }
    suffix = path + 16;
  } else if (strncmp(path, "$XDG_CACHE_HOME/", 16) == 0) {
    prefix = getenv("XDG_CACHE_HOME");
    if (!prefix) {
      const char *home = getenv("HOME");
      if (!home) return NULL;
      snprintf(g_pathbuf, sizeof(g_pathbuf), "%s/.cache%s", home, path + 15);
      return g_pathbuf;
    }
    suffix = path + 15;
  } else if (strncmp(path, "$XDG_STATE_HOME/", 16) == 0) {
    prefix = getenv("XDG_STATE_HOME");
    if (!prefix) {
      const char *home = getenv("HOME");
      if (!home) return NULL;
      snprintf(g_pathbuf, sizeof(g_pathbuf), "%s/.local/state%s", home, path + 15);
      return g_pathbuf;
    }
    suffix = path + 15;
  } else if (strncmp(path, "$DOTFILES", 9) == 0) {
    prefix = getenv("DOTFILES");
    if (!prefix) {
      const char *home = getenv("HOME");
      if (!home) return NULL;
      snprintf(g_pathbuf, sizeof(g_pathbuf), "%s/.dotfiles%s", home, path + 9);
      return g_pathbuf;
    }
    suffix = path + 9;
  } else {
    snprintf(g_pathbuf, sizeof(g_pathbuf), "%s", path);
    return g_pathbuf;
  }

  if (!prefix) return NULL;
  snprintf(g_pathbuf, sizeof(g_pathbuf), "%s%s", prefix, suffix);
  return g_pathbuf;
}

int main(void) {
  ensure_dir(expand_path("$HOME/tmp"));
  return 0;
}
