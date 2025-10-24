#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lx_process.h"
#include "utils.h"
#include "action.h"

void prepare_bootstrap(void) {
  set_current_action_group(ACTION_GROUP_DEFAULT);

  ensure_directory_exists("$XDG_CACHE_HOME");
  ensure_directory_exists("$XDG_CONFIG_HOME");
  ensure_directory_exists("$XDG_CACHE_HOME/bootstrap_backups");

  ensure_package_installed("systemd");
  ensure_system_directory_exists("/etc/systemd");

  ensure_symlink_exists("$DOTFILES/configs/autostart", "$XDG_CONFIG_HOME/autostart");

  bool changed = false;
  changed |= ensure_symlink_exists("$DOTFILES/configs/systemd/user/wl-paste.service", "$XDG_CONFIG_HOME/systemd/user/wl-paste.service");
  if (changed) {
    add_actionf(ACTION_RUN_CMD, ACTION_SCOPE_USER, ACT_PENDING, "systemctl --user daemon-reload");
  }

  ensure_user_service_enabled("wl-paste2.service");
}

void configure_journal(void) {
  log_title("Configuring systemd-journald (persistent logging + limits)");

  ensure_package_removed("rsyslog");
  ensure_package_removed("syslog-ng");
  ensure_package_removed("metalog");
  ensure_package_removed("sysklogd");

  ensure_system_service_enabled("systemd-journald.service");

  ensure_system_directory_exists("/var/log/journal");

  bool changed = false;

  changed |= ensure_system_file_sync_to("$DOTFILES/configs/systemd/journald.conf", "/etc/systemd/journald.conf");

  if (changed) {
    system_service_restart("systemd-journald.service");
  }
}

void configure_pacman(void) {
  log_title("Configuring pacman");
  bool changed = false;

  changed |= ensure_system_file_sync_to("$DOTFILES/configs/pacman/pacman.conf", "/etc/pacman.conf");

  if (changed) {
    cmd_run_or_die("sudo pacman -Sy --noconfirm", 0); // refresh DB
  }
}

void configure_console(void) {
  log_title("Configuring Linux console (vconsole)");

  ensure_package_installed("terminus-font");

  bool changed = false;
  changed |= ensure_system_file_sync_to("$DOTFILES/configs/console/vconsole.conf", "/etc/vconsole.conf");

  if (changed) {
    system_service_restart("systemd-vconsole-setup.service");
  }
}

void configure_hostname(void) {
  log_title("Configuring hostname");

  bool changed = false;

  changed |= ensure_system_file_sync_to("$DOTFILES/configs/hostname/hostname", "/etc/hostname");
  changed |= ensure_system_file_sync_to("$DOTFILES/configs/hostname/hosts", "/etc/hosts");

  if (changed) {
    cmd_run("hostnamectl set-hostname $(cat /etc/hostname)", 0);
  }
}

void configure_environment_defaults(void) {
  log_title("Configuring system-wide environment defaults");

  ensure_system_file_sync_to("$DOTFILES/configs/environment/environment", "/etc/environment");
  // ensure_symlink_exists("$DOTFILES/configs/environment.d", "$XDG_CONFIG_HOME/environment.d");
}

void configure_notification(void) {
  set_current_action_group(ACTION_GROUP_NOTIFICATION);

  ensure_package_installed("mako");
  ensure_package_installed("libnotify");

  ensure_user_service_enabled("mako.service");

  ensure_directory_exists("$XDG_CONFIG_HOME/mako");

  bool changed = ensure_symlink_exists("$DOTFILES/configs/mako/config", "$XDG_CONFIG_HOME/mako/config");

  if (changed) {
    user_service_restart("mako.service");
  }
}

void configure_time(void) {
  set_current_action_group(ACTION_GROUP_TIME);

  ensure_system_service_enabled("systemd-timesyncd.service");

  bool changed = false;
  changed |= ensure_system_file_sync_to("$DOTFILES/configs/systemd/timesyncd.conf", "/etc/systemd/timesyncd.conf");
  if (changed) {
    system_service_restart("systemd-timesyncd.service");
  }

  ensure_ntp_enabled();
  ensure_timezone_tehran();
}

void configure_audio(void) {
  set_current_action_group(ACTION_GROUP_AUDIO);

  ensure_package_removed("pulseaudio");
  ensure_package_removed("pulseaudio-alsa");
  ensure_package_removed("pulseaudio-bluetooth");
  ensure_package_removed("pulseaudio-jack");
  ensure_package_removed("jack2");
  ensure_package_removed("pipewire-media-session");

  ensure_package_installed("pipewire");
  ensure_package_installed("pipewire-alsa");
  ensure_package_installed("pipewire-jack");
  ensure_package_installed("pipewire-pulse");
  ensure_package_installed("wireplumber");
  ensure_package_installed("alsa-utils");

  ensure_user_service_enabled("pipewire.service");
  ensure_user_service_enabled("pipewire-pulse.service");
  ensure_user_service_enabled("wireplumber.service");
}

void configure_network(void) {
  set_current_action_group(ACTION_GROUP_NETWORK);

  ensure_package_removed("dhcpcd");
  ensure_package_removed("netctl");
  ensure_package_removed("network-manager-applet");
  ensure_package_removed("networkmanager");
  ensure_package_removed("connman");
  ensure_package_removed("wpa_supplicant");

  ensure_package_installed("iwd");

  ensure_system_service_enabled("iwd.service");
  ensure_system_service_enabled("systemd-networkd.service");
  ensure_system_service_enabled("systemd-resolved.service");

  ensure_system_directory_exists("/etc/systemd/network");
  ensure_system_directory_exists("/etc/iwd");

  bool changed = false;
  char mac[MAC_MAX];
  get_mac_address(mac, sizeof(mac));

  changed |= ensure_system_template_sync_to("$DOTFILES/configs/systemd/25-wireless.network", "/etc/systemd/network/25-wireless.network", (kv_pair[]){{"MAC", mac}}, 1);
  changed |= ensure_system_file_sync_to("$DOTFILES/configs/systemd/resolved.conf", "/etc/systemd/resolved.conf");
  changed |= ensure_system_file_sync_to("$DOTFILES/configs/systemd/networkd.conf", "/etc/systemd/networkd.conf");
  changed |= ensure_system_file_sync_to("$DOTFILES/configs/systemd/20-wired.network", "/etc/systemd/network/20-wired.network");
  changed |= ensure_system_symlink_exists("/run/systemd/resolve/stub-resolv.conf", "/etc/resolv.conf");
  changed |= ensure_system_file_sync_to("$DOTFILES/configs/iwd/main.conf", "/etc/iwd/main.conf");

  if (changed) {
    system_service_restart("systemd-resolved.service");
    system_service_restart("systemd-networkd.service");
    system_service_restart("iwd.service");
  }
}

void configure_zsh(void) {
  set_current_action_group(ACTION_GROUP_ZSH);

  ensure_package_installed("zsh");
  ensure_package_installed("fzf");
  ensure_package_installed("zoxide");
  ensure_package_installed("eza");
  ensure_package_installed("less");
  ensure_package_installed("lazygit");
  ensure_package_installed("man-db");
  ensure_package_installed("man-pages");

  ensure_symlink_exists("$DOTFILES/configs/zsh/.zshrc", "$HOME/.zshrc");
  ensure_symlink_exists("$DOTFILES/configs/zsh/.zprofile", "$HOME/.zprofile");

  ensure_directory_exists("$XDG_DATA_HOME/zsh");
  ensure_directory_exists("$XDG_DATA_HOME/zsh/plugins");

  ensure_git_repo_cloned("https://github.com/zsh-users/zsh-autosuggestions.git", "$XDG_DATA_HOME/zsh/plugins/zsh-autosuggestions");
  ensure_git_repo_cloned("https://github.com/zsh-users/zsh-syntax-highlighting.git", "$XDG_DATA_HOME/zsh/plugins/zsh-syntax-highlighting");
}

void configure_neovim(void) {
  set_current_action_group(ACTION_GROUP_NEOVIM);

  ensure_package_installed("neovim");
  ensure_package_installed("lua-language-server");
  ensure_package_installed("stylua");
  ensure_package_installed("shfmt");
  ensure_package_installed("clang");
  ensure_package_installed("jq");

  ensure_git_repo_with_ssh_remote("https://github.com/jedimahdi/nvim.git", "git@github.com:jedimahdi/nvim.git", "$XDG_CONFIG_HOME/nvim");
}

void configure_tmux(void) {
  set_current_action_group(ACTION_GROUP_TMUX);

  ensure_package_installed("tmux");

  ensure_directory_exists("$XDG_CONFIG_HOME/tmux");

  ensure_symlink_exists("$DOTFILES/configs/tmux/tmux.conf", "$XDG_CONFIG_HOME/tmux/tmux.conf");
}

int main(void) {
  init_action_groups();

  // configure_notification();
  // configure_network();
  // configure_time();
  // configure_audio();
  prepare_bootstrap();
  // configure_zsh();
  // configure_neovim();
  // configure_tmux();

  print_action_groups();

  run_action_groups();

  // configure_hostname();
  // configure_environment_defaults();
  // configure_console();
  // configure_pacman();
  // configure_journal();
  return 0;
}
