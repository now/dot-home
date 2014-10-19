sysconf_DATA = \
	host/portair.local/etc/apache2/extra/httpd-vhosts.conf \
	host/portair.local/etc/apache2/users/now.conf

$(call GROUP_template,$(DOTFILES),/private,,host/portair.local/)

sysconf_PATCHES = \
	host/portair.local/etc/apache2/httpd.conf.patch \
	host/portair.local/etc/hosts.patch

$(call PATCH_template,$(sysconf_PATCHES),/private,,host/portair.local/)
