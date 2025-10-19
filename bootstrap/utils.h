#ifndef UTILS_H_
#define UTILS_H_

#include <stddef.h>
#include <stdbool.h>

#define ANSI_RESET "\033[0m"
#define ANSI_BOLD "\033[1m"
#define ANSI_GREEN "\033[32m"
#define ANSI_RED "\033[31m"
#define ANSI_YELLOW "\033[33m"
#define ANSI_BLUE "\033[34m"
#define ANSI_DIM "\033[2m"

#define IFNAME_MAX 64
#define MAC_MAX 32

typedef struct kv_pair {
  const char *key;
  const char *value;
} kv_pair;

void log_title(const char *title);
void log_info(const char *fmt, ...);
void log_success(const char *fmt, ...);
void log_warn(const char *fmt, ...);
void log_fatal(const char *fmt, ...);

char *expand_path_buf(char *out, size_t outsz, const char *path);
const char *expand_path(const char *path);
void backup_path(const char *path);
void backup_system_path(const char *path);

void ensure_system_service_enabled(const char *service);
void system_service_restart(const char *service);
void ensure_user_service_enabled(const char *service);
void user_service_restart(const char *service);

void ensure_directory_exists(const char *path);
void ensure_system_directory_exists(const char *path);

bool ensure_symlink_exists(const char *target, const char *linkpath);
bool ensure_system_symlink_exists(const char *target, const char *linkpath);
bool ensure_system_file_sync_to(const char *src, const char *dest);
bool ensure_system_template_sync_to(const char *template_path, const char *dest_path, const struct kv_pair *vars, size_t var_count);

void ensure_package_installed(const char *pkg);
void ensure_package_removed(const char *pkg);

int get_first_wireless_ifname(char *buf, size_t buflen);
int get_predictable_ifname(const char *ifname, char *buf, size_t buflen);
void get_mac_address(char *buf, size_t buflen);

#endif
