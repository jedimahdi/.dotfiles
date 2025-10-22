#ifndef ACTION_H_
#define ACTION_H_

#include <linux/limits.h>
#include <stdbool.h>
#include <stddef.h>

#define MAX_ACTIONS_PER_GROUP 32

typedef enum {
  ACTION_INSTALL_PACKAGE,
  ACTION_ENABLE_SERVICE,
  ACTION_RESTART_SERVICE,
  ACTION_BACKUP,
  ACTION_CREATE_DIR,
  ACTION_CREATE_SYMLINK,
  ACTION_CREATE_SYNC,
  ACTION_RUN_CMD,
} ActionType;

typedef enum {
  ACTION_SCOPE_USER,
  ACTION_SCOPE_SYSTEM,
} ActionScope;

typedef enum {
  ACT_PENDING,
  ACT_DONE
} ActionStatus;

typedef struct {
  ActionType type;
  ActionScope scope;
  ActionStatus status;
  char arg1[PATH_MAX];
  char arg2[PATH_MAX];
} Action;

typedef enum {
  ACTION_GROUP_DEFAULT = 0,
  ACTION_GROUP_NOTIFICATION,
  ACTION_GROUP_TIME,
  ACTION_GROUP_AUDIO,
  ACTION_GROUP_NETWORK,
  ACTION_GROUP_COUNT,
} ActionGroupKind;

typedef struct {
  ActionGroupKind kind;
  Action actions[MAX_ACTIONS_PER_GROUP];
  size_t count;
} ActionGroup;

void init_action_groups(void);
void set_current_action_group(ActionGroupKind kind);
void print_action_groups(void);

void add_action(ActionType type, ActionScope scope, const char *a1, const char *a2, ActionStatus status);

void ensure_symlink_action(ActionGroup *g, const char *src, const char *dst);
void ensure_package_action(ActionGroup *g, const char *pkg);

void ensure_package_installed(const char *pkg);
void ensure_user_service_enabled(const char *service);
void ensure_system_service_enabled(const char *service);
void ensure_directory_exists(const char *path);
bool ensure_symlink_exists(const char *target_path, const char *link_path);
void user_service_restart(const char *service);
void system_service_restart(const char *service);
void ensure_ntp_enabled(void);
void ensure_timezone_tehran(void);
bool ensure_system_file_sync_to(const char *target_path, const char *link_path);

#endif
