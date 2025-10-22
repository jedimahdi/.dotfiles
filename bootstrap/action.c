#include "action.h"
#include "utils.h"
#include <stdio.h>
#include <string.h>

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
  default:
    return "Unknown";
  }
}

const char *action_name(ActionType type) {
  switch (type) {
  case ACTION_INSTALL_PACKAGE:
    return "Install package";
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

void ensure_package_installed(const char *pkg) {
  ActionStatus action_status = ACT_PENDING;
  int status = cmd_runf_quiet("pacman -Q --quiet %s", pkg);
  if (status == 0) {
    action_status = ACT_DONE;
  }
  add_action(ACTION_INSTALL_PACKAGE, ACTION_SCOPE_SYSTEM, pkg, NULL, action_status);
}

static void ensure_service_enabled(const char *service, ActionScope scope) {
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
  ensure_service_enabled(service, ACTION_SCOPE_USER);
}
void ensure_system_service_enabled(const char *service) {
  ensure_service_enabled(service, ACTION_SCOPE_SYSTEM);
}

void user_service_restart(const char *service) {
  add_action(ACTION_RESTART_SERVICE, ACTION_SCOPE_USER, service, NULL, ACT_PENDING);
}
void system_service_restart(const char *service) {
  add_action(ACTION_RESTART_SERVICE, ACTION_SCOPE_SYSTEM, service, NULL, ACT_PENDING);
}

void ensure_directory_exists(const char *path) {
  ActionStatus action_status = ACT_PENDING;
  char path_expanded[PATH_MAX];
  expand_path(path, path_expanded, sizeof(path_expanded));
  struct stat st;
  if (xstat(path_expanded, &st)) {
    if (S_ISDIR(st.st_mode)) {
      action_status = ACT_DONE;
    } else {
      add_action(ACTION_BACKUP, ACTION_SCOPE_USER, path, NULL, action_status);
    }
  }
  add_action(ACTION_CREATE_DIR, ACTION_SCOPE_USER, path, NULL, action_status);
}

bool ensure_symlink_exists(const char *target_path, const char *link_path) {
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
        add_action(ACTION_BACKUP, ACTION_SCOPE_USER, link_path, NULL, action_status);
      }
    } else {
      add_action(ACTION_BACKUP, ACTION_SCOPE_USER, link_path, NULL, action_status);
    }
  }
  add_action(ACTION_CREATE_SYMLINK, ACTION_SCOPE_USER, target_path, link_path, action_status);
  return changed;
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
