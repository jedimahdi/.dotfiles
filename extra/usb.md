## Format a Bootable USB to Normal

```bash
sudo umount /dev/sdb1
sudo fdisk -l
lsblk
sudo wipefs --all /dev/sdb
sudo mkfs.vfat /dev/sdb

sudo dd if=~/Downloads/name.iso of=/dev/sdb bs=4M status=progress
sync
```

```bash
sha512sum -c Endeavouros-Galileo-11-2023.iso.sha512sum
gpg --recv CDF595A1
gpg --verify Endeavouros-Galileo-11-2023.iso.sig


# or make a partition
sudo fdisk /dev/sdb # n to create partition
sudo mkfs.ext4 /dev/sdb1

```

## Another tool

https://github.com/ifd3f/caligula

sudo pacman -S caligula
caligula burn some-image-file.iso.gz
