sysconfapache2dir = $(sysconfdir)/apache2
sysconfapache2usersdir = $(sysconfapache2dir)/users

sysconfapache2users_DATA = \
	host/portair.local/etc/apache2/users/now.conf

sysconf_PATCHES = \
	host/portair.local/etc/apache2/extra/httpd-userdir.conf.patch \
	host/portair.local/etc/apache2/httpd.conf.patch \
	host/portair.local/etc/hosts.patch

$(call PATCH_template,$(sysconf_PATCHES),/private,,host/portair.local/)
