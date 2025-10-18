#include <stdio.h>
#include <stdlib.h>
#include "lx_process.h"
#include "utils.h"

void configure_notification(void) {
  log_title("Configuring Notification");

  ensure_package_installed("mako");
  ensure_package_installed("libnotify");

  ensure_user_service_enabled("mako.service");

  ensure_directory_exists("$XDG_CONFIG_HOME/mako");
  ensure_symlink_exists("$DOTFILES/configs/mako/config", "$XDG_CONFIG_HOME/mako/config");
}

int main(void) {
  configure_notification();
  return 0;
}
