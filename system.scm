(use-modules
  (gnu)
  (gnu packages certs)
  (gnu packages shells)
  (nongnu packages linux)
  (nonnongnu packages linux))

(use-service-modules
  desktop
  networking
  ssh
  xorg)

(operating-system
  (kernel more-linux)
  (firmware (list linux-firmware))
  (locale "en_US.utf8")
  (timezone "Europe/Stockholm")
  (keyboard-layout (keyboard-layout "us" "dvorak"))
  (host-name "now-work")
  (users (cons* (user-account
                  (name "root")
                  (comment "")
                  (group "users")
                  (home-directory "/root")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video")))
                (user-account
                  (name "now")
                  (comment "Nikolai Weibull")
                  (group "users")
                  (home-directory "/home/now")
                  (shell (file-append zsh "/bin/zsh"))
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))
  (packages
    (append
      (list nss-certs
            zsh)
      %base-packages))
  (services
    (append
      (list (service gnome-desktop-service-type)
            (service openssh-service-type)
            (set-xorg-configuration
              (xorg-configuration
                (keyboard-layout keyboard-layout))))
      %desktop-services))
  (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets (list "/boot/efi"))
      (keyboard-layout keyboard-layout)))
  (swap-devices
    (list (swap-space
            (target
              (uuid "5d6409ab-386d-4b09-ad97-d0fe455c71f2")))))
  (file-systems
    (cons* (file-system
             (mount-point "/boot/efi")
             (device (uuid "F556-A8DA" 'fat32))
             (type "vfat"))
           (file-system
             (mount-point "/")
             (device
               (uuid "d4fc925e-d09b-4c48-9850-a2b089f9313f"
                     'ext4))
             (type "ext4"))
           %base-file-systems)))
