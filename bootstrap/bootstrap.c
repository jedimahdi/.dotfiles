#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lx_process.h"
#include "utils.h"

void prepare_bootstrap(void) {
  log_title("Prepare Bootstrap");
  ensure_directory_exists("$XDG_CACHE_HOME");
  ensure_directory_exists("$XDG_CONFIG_HOME");
  ensure_directory_exists("$XDG_CACHE_HOME/bootstrap_backups");

  ensure_package_installed("systemd");
  ensure_system_directory_exists("/etc/systemd");
}

void configure_notification(void) {
  log_title("Configuring Notification");

  ensure_package_installed("mako");
  ensure_package_installed("libnotify");

  ensure_user_service_enabled("mako.service");

  ensure_directory_exists("$XDG_CONFIG_HOME/mako");
  bool changed = ensure_symlink_exists("$DOTFILES/configs/mako/config", "$XDG_CONFIG_HOME/mako/config");

  if (changed) {
    user_service_restart("mako.service");
  }
}

void configure_audio(void) {
  log_title("Configuring Audio (PipeWire + WirePlumber)");

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
  log_title("Configuring Network (iwd + systemd-networkd + systemd-resolved)");

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
  changed |= ensure_system_file_sync_to("$DOTFILES/configs/systemd/20-wired.network", "/etc/systemd/network/20-wired.network");
  changed |= ensure_system_symlink_exists("/run/systemd/resolve/stub-resolv.conf", "/etc/resolv.conf");
  changed |= ensure_system_file_sync_to("$DOTFILES/configs/iwd/main.conf", "/etc/iwd/main.conf");

  if (changed) {
    system_service_restart("systemd-resolved.service");
    system_service_restart("systemd-networkd.service");
    system_service_restart("iwd.service");
  }
}

void configure_time(void) {
  log_title("Configuring Time Sync (systemd-timesyncd)");

  ensure_system_service_enabled("systemd-timesyncd.service");

  bool changed = false;
  changed |= ensure_system_file_sync_to(
      "$DOTFILES/configs/systemd/timesyncd.conf",
      "/etc/systemd/timesyncd.conf");

  if (changed) {
    system_service_restart("systemd-timesyncd.service");
  }

  if (lx_run_sync(&(lx_run_opts){0}, "sudo", "timedatectl", "set-ntp", "true") != 0) {
    log_fatal("Failed to run: sudo timedatectl set-ntp true");
  }
  if (lx_run_sync(&(lx_run_opts){0}, "sudo", "timedatectl", "set-timezone", "Asia/Tehran") != 0) {
    log_fatal("Failed to run: sudo timedatectl set-ntp true");
  }
}

int main(void) {
  // char buf[128];
  // get_line_from_shell_command(buf, sizeof(buf), "timedatectl show -p NTP");
  // printf("'%s'\n", buf);

  char buf[] = "test\nskdljas";
  char *p = memchr(buf, '\n', 5);
  if (!p) {
    printf("p is null\n");
  } else {
    printf("p = '%s'\n", p);
  }

  // prepare_bootstrap();
  // configure_time();
  // configure_network();
  // configure_notification();
  // configure_audio();
  return 0;
}
