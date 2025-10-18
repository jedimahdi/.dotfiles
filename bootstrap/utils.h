#ifndef UTILS_H_
#define UTILS_H_

#include <stddef.h>

#define ANSI_RESET "\033[0m"
#define ANSI_BOLD "\033[1m"
#define ANSI_GREEN "\033[32m"
#define ANSI_RED "\033[31m"
#define ANSI_YELLOW "\033[33m"
#define ANSI_BLUE "\033[34m"
#define ANSI_DIM "\033[2m"

void log_title(const char *title);
void log_info(const char *fmt, ...);
void log_success(const char *fmt, ...);
void log_fatal(const char *fmt, ...);

char *expand_path_buf(char *out, size_t outsz, const char *path);
const char *expand_path(const char *path);

void ensure_system_service_enabled(const char *service);
void ensure_user_service_enabled(const char *service);
void ensure_directory_exists(const char *path);
void ensure_package_installed(const char *pkg);
void ensure_package_removed(const char *pkg);
void ensure_symlink_exists(const char *target, const char *linkpath);

#endif
