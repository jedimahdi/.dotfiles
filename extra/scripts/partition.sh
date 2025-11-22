#!/bin/bash

DISK=${1:-/dev/sda}

echo "Partitioning $DISK"

sfdisk "$DISK" <<EOF
label: gpt
device: $DISK

${DISK}1: size=500MiB, type=uefi
${DISK}2: size=8GiB, type=swap
${DISK}3: type=linux
EOF

mkfs.fat -F32 -n sys-efi ${DISK}1
mkswap -L sys-swap ${DISK}2
mkfs.ext4 -L sys-root ${DISK}3

swapon -L sys-swap
mount -L sys-root /mnt
mkdir -p /mnt/boot
mount -L sys-efi /mnt/boot

# genfstab -L /mnt >> /mnt/etc/fstab
