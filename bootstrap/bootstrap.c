#include <stdio.h>
#include <stdlib.h>
#include "lx_process.h"
#include "utils.h"

void prepare_bootstrap(void) {
  log_title("Prepare Bootstrap");
  ensure_directory_exists("$XDG_CACHE_HOME");
  ensure_directory_exists("$XDG_CONFIG_HOME");
  ensure_directory_exists("$XDG_CACHE_HOME/bootstrap_backups");

  ensure_package_installed("systemd");
}

void configure_notification(void) {
  log_title("Configuring Notification");

  ensure_package_installed("mako");
  ensure_package_installed("libnotify");

  ensure_user_service_enabled("mako.service");

  ensure_directory_exists("$XDG_CONFIG_HOME/mako");
  ensure_symlink_exists("$DOTFILES/configs/mako/config", "$XDG_CONFIG_HOME/mako/config");
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

  system_copy_file_to("$DOTFILES/configs/systemd/resolved.conf", "/etc/systemd/resolved.conf");
  system_copy_file_to("$DOTFILES/configs/systemd/20-wired.network", "/etc/systemd/network/20-wired.network");

  char ifname[IFNAME_MAX];
  char pred[IFNAME_MAX];
  char mac[MAC_MAX];
  if (get_first_wireless_ifname(ifname, sizeof(ifname)) == 0) {
    printf("First wireless: %s\n", ifname);

    if (get_predictable_ifname(ifname, pred, sizeof(pred)) == 0) {
      printf("Predictable name: %s\n", pred);
    }

    if (get_mac_address(ifname, mac, sizeof(mac)) == 0) {
      printf("MAC: %s\n", mac);
    }
  }

  struct kv_pair vars[] = {
      {"MAC", mac},
  };
  system_template_to("$DOTFILES/configs/systemd/20-wlan.network", "/etc/systemd/network/20-wlan.network", vars, 1);

  ensure_system_symlink_exists("/run/systemd/resolve/stub-resolv.conf", "/etc/resolv.conf");

  system_service_restart("systemd-resolved.service");
  system_service_restart("systemd-networkd.service");
}

int main(void) {
  prepare_bootstrap();
  configure_network();

  // configure_notification();
  // configure_audio();
  return 0;
}
