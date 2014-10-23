sysconfapache2dir = $(sysconfdir)/apache2
sysconfapache2extradir = $(sysconfapache2dir)/extra
sysconfapache2usersdir = $(sysconfapache2dir)/users

sysconfapache2extra_DATA = \
	host/portair.local/etc/apache2/extra/httpd-vhosts.conf

sysconfapache2users_DATA = \
	host/portair.local/etc/apache2/users/now.conf

sysconf_PATCHES = \
	host/portair.local/etc/apache2/httpd.conf.patch \
	host/portair.local/etc/hosts.patch

$(call PATCH_template,$(sysconf_PATCHES),/private,,host/portair.local/)
