#define _XOPEN_SOURCE 700
#include "action.h"
#include "utils.h"
#include <stdio.h>
#include <string.h>
#include <libgen.h>
#include <stdarg.h>
#include <unistd.h>

static ActionGroup groups[ACTION_GROUP_COUNT];
static ActionGroupKind curr_group = ACTION_GROUP_DEFAULT;

void init_action_groups(void) {
  for (int i = 0; i < ACTION_GROUP_COUNT; i++) {
    groups[i].kind = (ActionGroupKind)i;
    groups[i].count = 0;
  }
  curr_group = ACTION_GROUP_DEFAULT;
}

void set_current_action_group(ActionGroupKind kind) {
  curr_group = kind;
}

const char *group_name(ActionGroupKind kind) {
  switch (kind) {
  case ACTION_GROUP_DEFAULT:
    return "Default";
  case ACTION_GROUP_NOTIFICATION:
    return "Notifications";
  case ACTION_GROUP_AUDIO:
    return "Audio";
  case ACTION_GROUP_NETWORK:
    return "Network";
  case ACTION_GROUP_TIME:
    return "Time";
  case ACTION_GROUP_ZSH:
    return "ZSH";
  case ACTION_GROUP_TMUX:
    return "Tmux";
  case ACTION_GROUP_NEOVIM:
    return "Neovim";
  default:
    return "Unknown";
  }
}

const char *action_name(ActionType type) {
  switch (type) {
  case ACTION_INSTALL_PACKAGE:
    return "Install package";
  case ACTION_REMOVE_PACKAGE:
    return "Remove package";
  case ACTION_ENABLE_SERVICE:
    return "Enable service";
  case ACTION_RESTART_SERVICE:
    return "Restart service";
  case ACTION_CREATE_DIR:
    return "Create directory";
  case ACTION_CREATE_SYMLINK:
    return "Create symlink";
  case ACTION_CREATE_SYNC:
    return "Create sync";
  case ACTION_BACKUP:
    return "Create Backup";
  case ACTION_RUN_CMD:
    return "Run command";
  default:
    return "Unknown";
  }
}

const char *scope_name(ActionScope s) {
  return s == ACTION_SCOPE_USER ? "User" : "System";
}

void print_action_groups(void) {
  for (int gi = 0; gi < ACTION_GROUP_COUNT; gi++) {
    ActionGroup *g = &groups[gi];
    if (g->count == 0) continue;

    printf("=== %s (%zu actions) ===\n", group_name(g->kind), g->count);
    for (size_t i = 0; i < g->count; i++) {
      Action *act = &g->actions[i];
      const char *mark = (act->status == ACT_DONE) ? "[✓]" : "[ ]";
      const char *scope = (act->scope == ACTION_SCOPE_SYSTEM) ? "[SYSTEM]" : "[USER]";
      switch (act->type) {
      case ACTION_CREATE_SYNC:
      case ACTION_CREATE_SYMLINK:
        printf("  %s %s %s: %s -> %s\n", mark, scope, action_name(act->type), act->arg1, act->arg2);
        break;
      default:
        printf("  %s %s %s: %s\n", mark, scope, action_name(act->type), act->arg1);
        break;
      }
    }
    printf("\n");
  }
}

void run_sync_action(Action *a) {
  if (a->scope == ACTION_SCOPE_USER) {
    log_fatal("Not implemented yet!");
  }
  char src_expanded[PATH_MAX];
  char dest_expanded[PATH_MAX];
  expand_path(a->arg1, src_expanded, sizeof(src_expanded));
  expand_path(a->arg2, dest_expanded, sizeof(dest_expanded));
  printf("sudo cp %s %s\n", src_expanded, dest_expanded);
}

void run_enable_service(Action *a) {
  char *service = a->arg1;
  if (a->scope == ACTION_SCOPE_USER) {
    printf("systemctl --user enable --now %s\n", service);
  } else {
    printf("sudo systemctl enable --now %s\n", service);
  }
}

void run_install_package_action(Action *a) {
  char *pkg = a->arg1;
  printf("sudo pacman -S %s\n", pkg);
}

void run_remove_package_action(Action *a) {
  char *pkg = a->arg1;
  printf("sudo pacman -S %s\n", pkg);
}

void run_symlink_action(Action *a) {
  char *target_path = a->arg1;
  char *link_path = a->arg2;
  char target_path_expanded[PATH_MAX];
  char link_path_expanded[PATH_MAX];
  expand_path(target_path, target_path_expanded, sizeof(target_path_expanded));
  expand_path(link_path, link_path_expanded, sizeof(link_path_expanded));

  if (a->scope == ACTION_SCOPE_USER) {
    printf("ln -s %s %s\n", target_path, link_path);
    symlink(target_path_expanded, link_path_expanded);

  } else {
    printf("sudo ln -s %s %s\n", target_path, link_path);
  }
}

void run_backup_action(Action *a) {
  char *path = a->arg1;
  char path_expanded[PATH_MAX];
  expand_path(path, path_expanded, sizeof(path_expanded));

  if (a->scope == ACTION_SCOPE_USER) {
    backup_path(path_expanded);
  } else {
    printf("sudo backup %s\n", path);
  }
}

void run_cmd_action(Action *a) {
}

void run_create_dir_action(Action *a) {
  char *path = a->arg1;
  if (a->scope == ACTION_SCOPE_USER) {
    cmd_runf("mkdir -p %s", path);
  } else {
    printf("sudo mkdir -p %s\n", path);
  }
}

void run_restart_service(Action *a) {
}

void run_action_groups(void) {
  for (int gi = 0; gi < ACTION_GROUP_COUNT; gi++) {
    ActionGroup *g = &groups[gi];
    if (g->count == 0) continue;

    for (size_t i = 0; i < g->count; i++) {
      Action *act = &g->actions[i];
      if (act->status == ACT_DONE) continue;
      printf("  %s: %s\n", action_name(act->type), act->arg1);

      switch (act->type) {

      case ACTION_INSTALL_PACKAGE:
        run_install_package_action(act);
        break;
      case ACTION_REMOVE_PACKAGE:
        run_remove_package_action(act);
        break;

      case ACTION_ENABLE_SERVICE:
        run_enable_service(act);
        break;
      case ACTION_RESTART_SERVICE:
        run_restart_service(act);
        break;

      case ACTION_BACKUP:
        run_backup_action(act);
        break;
      case ACTION_CREATE_DIR:
        run_create_dir_action(act);
        break;
      case ACTION_CREATE_SYMLINK:
        run_symlink_action(act);
        break;
      case ACTION_CREATE_SYNC:
        run_sync_action(act);
        break;

      case ACTION_RUN_CMD:
        run_cmd_action(act);
        break;

      default:
        printf("sheeesh!\n");
        break;
      }
    }
  }
}

void add_action(ActionType type, ActionScope scope, const char *a1, const char *a2, ActionStatus status) {
  ActionGroup *g = &groups[curr_group];
  if (g->count >= MAX_ACTIONS_PER_GROUP) {
    log_fatal("too many actions in group %d", curr_group);
  }
  Action *act = &g->actions[g->count++];
  act->type = type;
  act->status = status;
  act->scope = scope;
  snprintf(act->arg1, sizeof(act->arg1), "%s", a1 ? a1 : "");
  snprintf(act->arg2, sizeof(act->arg2), "%s", a2 ? a2 : "");
}

void add_actionf(ActionType type, ActionScope scope, ActionStatus status, const char *fmt, ...) {
  ActionGroup *g = &groups[curr_group];
  if (g->count >= MAX_ACTIONS_PER_GROUP) {
    log_fatal("too many actions in group %d", curr_group);
  }
  Action *act = &g->actions[g->count++];
  act->type = type;
  act->status = status;
  act->scope = scope;
  act->arg2[0] = '\0';

  va_list ap;
  va_start(ap, fmt);
  int n = vsnprintf(act->arg1, sizeof(act->arg1), fmt, ap);
  va_end(ap);

  if (n < 0 || (size_t)n >= sizeof(act->arg1)) {
    log_fatal("add_actionf: arg1 string truncated");
  }
}

void ensure_package_installed(const char *pkg) {
  ActionStatus action_status = ACT_PENDING;
  int status = cmd_runf_quiet("pacman -Q --quiet %s", pkg);
  if (status == 0) {
    action_status = ACT_DONE;
  }
  add_action(ACTION_INSTALL_PACKAGE, ACTION_SCOPE_SYSTEM, pkg, NULL, action_status);
}

void ensure_package_removed(const char *pkg) {
  ActionStatus action_status = ACT_PENDING;
  int status = cmd_runf_quiet("pacman -Q --quiet %s", pkg);
  if (status != 0) {
    action_status = ACT_DONE;
  }
  add_action(ACTION_REMOVE_PACKAGE, ACTION_SCOPE_SYSTEM, pkg, NULL, action_status);
}

static void ensure_service_enabled_scoped(const char *service, ActionScope scope) {
  ActionStatus action_status = ACT_PENDING;
  int status;
  if (scope == ACTION_SCOPE_SYSTEM) {
    status = cmd_runf_quiet("systemctl --quiet is-enabled %s", service);
  } else {
    status = cmd_runf_quiet("systemctl --user --quiet is-enabled %s", service);
  }
  if (status == 0) {
    action_status = ACT_DONE;
  }
  add_action(ACTION_ENABLE_SERVICE, scope, service, NULL, action_status);
}
void ensure_user_service_enabled(const char *service) {
  ensure_service_enabled_scoped(service, ACTION_SCOPE_USER);
}
void ensure_system_service_enabled(const char *service) {
  ensure_service_enabled_scoped(service, ACTION_SCOPE_SYSTEM);
}

void user_service_restart(const char *service) {
  add_action(ACTION_RESTART_SERVICE, ACTION_SCOPE_USER, service, NULL, ACT_PENDING);
}
void system_service_restart(const char *service) {
  add_action(ACTION_RESTART_SERVICE, ACTION_SCOPE_SYSTEM, service, NULL, ACT_PENDING);
}

static void ensure_directory_exists_scoped(const char *path, ActionScope scope) {
  ActionStatus action_status = ACT_PENDING;
  char path_expanded[PATH_MAX];
  expand_path(path, path_expanded, sizeof(path_expanded));
  struct stat st;
  if (xlstat(path_expanded, &st)) {
    if (S_ISDIR(st.st_mode)) {
      action_status = ACT_DONE;
    } else {
      add_action(ACTION_BACKUP, scope, path, NULL, action_status);
    }
  }
  add_action(ACTION_CREATE_DIR, scope, path, NULL, action_status);
}
void ensure_directory_exists(const char *path) {
  ensure_directory_exists_scoped(path, ACTION_SCOPE_USER);
}
void ensure_system_directory_exists(const char *path) {
  ensure_directory_exists_scoped(path, ACTION_SCOPE_SYSTEM);
}

static bool ensure_symlink_exists_scoped(const char *target_path, const char *link_path, ActionScope scope) {
  ActionStatus action_status = ACT_PENDING;
  bool changed = true;
  char target_path_expanded[PATH_MAX];
  char link_path_expanded[PATH_MAX];
  expand_path(target_path, target_path_expanded, sizeof(target_path_expanded));
  expand_path(link_path, link_path_expanded, sizeof(link_path_expanded));

  struct stat st;
  if (xlstat(link_path_expanded, &st)) {
    if (S_ISLNK(st.st_mode)) {
      char buf[PATH_MAX];
      xreadlink(link_path_expanded, buf, sizeof(buf));
      if (strcmp(buf, target_path_expanded) == 0) {
        changed = false;
        action_status = ACT_DONE;
      } else {
        add_action(ACTION_BACKUP, scope, link_path, NULL, action_status);
      }
    } else {
      add_action(ACTION_BACKUP, scope, link_path, NULL, action_status);
    }
  }
  add_action(ACTION_CREATE_SYMLINK, scope, target_path, link_path, action_status);
  return changed;
}
bool ensure_symlink_exists(const char *target_path, const char *link_path) {
  return ensure_symlink_exists_scoped(target_path, link_path, ACTION_SCOPE_USER);
}
bool ensure_system_symlink_exists(const char *target_path, const char *link_path) {
  return ensure_symlink_exists_scoped(target_path, link_path, ACTION_SCOPE_SYSTEM);
}

void ensure_ntp_enabled(void) {
  ActionStatus action_status = ACT_PENDING;
  char ntp[128];
  cmd_getline("timedatectl show -p NTP", ntp, sizeof(ntp));
  if (strcmp("NTP=yes", ntp) == 0) {
    action_status = ACT_DONE;
  }
  add_action(ACTION_RUN_CMD, ACTION_SCOPE_SYSTEM, "sudo timedatectl set-ntp true", NULL, action_status);
}

void ensure_timezone_tehran(void) {
  ActionStatus action_status = ACT_PENDING;
  char timezone[128];
  cmd_getline("timedatectl show -p Timezone", timezone, sizeof(timezone));
  if (strcmp("Timezone=Asia/Tehran", timezone) == 0) {
    action_status = ACT_DONE;
  }
  add_action(ACTION_RUN_CMD, ACTION_SCOPE_SYSTEM, "sudo timedatectl set-timezone Asia/Tehran", NULL, action_status);
}

bool ensure_system_file_sync_to(const char *target_path, const char *link_path) {
  ActionStatus action_status = ACT_PENDING;
  bool changed = true;
  char target_path_expanded[PATH_MAX];
  char link_path_expanded[PATH_MAX];
  expand_path(target_path, target_path_expanded, sizeof(target_path_expanded));
  expand_path(link_path, link_path_expanded, sizeof(link_path_expanded));

  struct stat st;
  if (xstat(link_path_expanded, &st)) {
    if (S_ISREG(st.st_mode)) {
      if (files_are_identical(link_path_expanded, target_path_expanded)) {
        changed = false;
        action_status = ACT_DONE;
      } else {
        add_action(ACTION_BACKUP, ACTION_SCOPE_SYSTEM, link_path, NULL, action_status);
      }
    } else {
      add_action(ACTION_BACKUP, ACTION_SCOPE_SYSTEM, link_path, NULL, action_status);
    }
  }

  add_action(ACTION_CREATE_SYNC, ACTION_SCOPE_SYSTEM, target_path, link_path, action_status);
  return changed;
}

bool ensure_system_template_sync_to(const char *template_path, const char *dest_path, const kv_pair *pairs, size_t pairs_count) {
  ActionStatus action_status = ACT_PENDING;
  bool changed = true;

  char template_expanded[PATH_MAX];
  expand_path(template_path, template_expanded, sizeof(template_expanded));

  char dest_expanded[PATH_MAX];
  expand_path(dest_path, dest_expanded, sizeof(dest_expanded));

  char build_dir[PATH_MAX];
  expand_path("$DOTFILES/build", build_dir, sizeof(build_dir));

  char rendered_path[PATH_MAX + 1];
  snprintf(rendered_path, sizeof(rendered_path), "%s/%s", build_dir, basename(dest_expanded));

  template_render_to_file(template_expanded, rendered_path, pairs, pairs_count);

  struct stat st;
  if (xstat(dest_expanded, &st)) {
    if (S_ISREG(st.st_mode)) {
      if (files_are_identical(dest_expanded, rendered_path)) {
        changed = false;
        action_status = ACT_DONE;
      } else {
        add_action(ACTION_BACKUP, ACTION_SCOPE_SYSTEM, dest_path, NULL, action_status);
      }
    } else {
      add_action(ACTION_BACKUP, ACTION_SCOPE_SYSTEM, dest_path, NULL, action_status);
    }
  }

  add_action(ACTION_CREATE_SYNC, ACTION_SCOPE_SYSTEM, rendered_path, dest_path, action_status);
  return changed;
}

void ensure_git_repo_cloned(const char *url, const char *dest_path) {
  ActionStatus action_status = ACT_PENDING;

  char dest_expanded[PATH_MAX];
  expand_path(dest_path, dest_expanded, sizeof(dest_expanded));

  struct stat st;
  if (xstat(dest_expanded, &st) && S_ISDIR(st.st_mode)) {
    int status = cmd_runf_quiet("git -C %s rev-parse --is-inside-work-tree", dest_expanded);
    if (status == 0) {
      char remote[1024];
      cmd_getlinef(remote, sizeof(remote), "git -C %s config --get remote.origin.url", dest_expanded);
      if (strcmp(remote, url) == 0) {
        action_status = ACT_DONE;
      } else {
        add_action(ACTION_BACKUP, ACTION_SCOPE_USER, dest_path, NULL, ACT_PENDING);
      }
    }
  }

  char cmd[PATH_MAX * 2];
  snprintf(cmd, sizeof(cmd), "git clone %s %s", url, dest_expanded);
  add_action(ACTION_RUN_CMD, ACTION_SCOPE_USER, cmd, NULL, action_status);
}

void ensure_git_remote(const char *repo_path, const char *remote_name, const char *desired_url) {
  ActionStatus action_status = ACT_PENDING;

  if (cmd_runf_quiet("git -C %s rev-parse --is-inside-work-tree", repo_path) != 0) {
    log_fatal("ensure_git_remote: not a git repo");
  }

  char current[1024];
  if (cmd_getlinef(current, sizeof(current), "git -C %s config --get remote.%s.url", repo_path, remote_name) == 0) {
    if (strcmp(current, desired_url) == 0) {
      action_status = ACT_DONE;
    } else {
      char cmd[PATH_MAX * 2];
      snprintf(cmd, sizeof(cmd), "git -C %s remote set-url %s %s", repo_path, remote_name, desired_url);
      add_action(ACTION_RUN_CMD, ACTION_SCOPE_USER, cmd, NULL, action_status);
    }
  } else {
    char cmd[PATH_MAX * 2];
    snprintf(cmd, sizeof(cmd), "git -C %s remote add %s %s", repo_path, remote_name, desired_url);
    add_action(ACTION_RUN_CMD, ACTION_SCOPE_USER, cmd, NULL, action_status);
  }
}

void ensure_git_repo_with_ssh_remote(const char *https_url, const char *ssh_url, const char *dest_path) {
  char dest_expanded[PATH_MAX];
  expand_path(dest_path, dest_expanded, sizeof(dest_expanded));

  struct stat st;
  bool exists = xstat(dest_expanded, &st);

  if (!exists) {
    add_actionf(ACTION_RUN_CMD, ACTION_SCOPE_USER, ACT_PENDING, "git clone %s %s", https_url, dest_expanded);
    add_actionf(ACTION_RUN_CMD, ACTION_SCOPE_USER, ACT_PENDING, "git -C %s remote set-url origin %s", dest_expanded, ssh_url);
    return;
  }

  if (!S_ISDIR(st.st_mode) || cmd_runf_quiet("git -C %s rev-parse --is-inside-work-tree", dest_expanded) != 0) {
    add_action(ACTION_BACKUP, ACTION_SCOPE_USER, dest_path, NULL, ACT_PENDING);
    add_actionf(ACTION_RUN_CMD, ACTION_SCOPE_USER, ACT_PENDING, "git clone %s %s", https_url, dest_expanded);
    add_actionf(ACTION_RUN_CMD, ACTION_SCOPE_USER, ACT_PENDING, "git -C %s remote set-url origin %s", dest_expanded, ssh_url);
    return;
  }

  char remote[1024];
  cmd_getlinef(remote, sizeof(remote), "git -C %s config --get remote.origin.url", dest_expanded);

  if (strcmp(remote, ssh_url) == 0) {
    add_actionf(ACTION_RUN_CMD, ACTION_SCOPE_USER, ACT_DONE, "git -C %s remote set-url origin %s", dest_expanded, ssh_url);
  } else if (strcmp(remote, https_url) == 0) {
    add_actionf(ACTION_RUN_CMD, ACTION_SCOPE_USER, ACT_PENDING, "git -C %s remote set-url origin %s", dest_expanded, ssh_url);
  } else {
    add_action(ACTION_BACKUP, ACTION_SCOPE_USER, dest_path, NULL, ACT_PENDING);
    add_actionf(ACTION_RUN_CMD, ACTION_SCOPE_USER, ACT_PENDING, "git clone %s %s", https_url, dest_expanded);
    add_actionf(ACTION_RUN_CMD, ACTION_SCOPE_USER, ACT_PENDING, "git -C %s remote set-url origin %s", dest_expanded, ssh_url);
  }
}
