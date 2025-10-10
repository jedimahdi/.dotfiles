## Format a Bootable USB to Normal

```bash
sudo umount /dev/sdb1
sudo fdisk -l
lsblk
sudo wipefs --all /dev/sdb
sudo mkfs.vfat /dev/sdb
```




```bash

# or make a partition
sudo fdisk /dev/sdb # n to create partition
sudo mkfs.ext4 /dev/sdb1

```


## Make Bootable usb

```bash
sudo wipefs --all /dev/sdb
sudo dd if=archlinux.iso of=/dev/sdX bs=4M status=progress oflag=sync
```

## Another tool

https://github.com/ifd3f/caligula

sudo pacman -S caligula
caligula burn some-image-file.iso.gz
