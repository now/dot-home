userconfemacsdsitelisp_DATA += \
	emacs/site-lisp/windows-path.elc

userconf_DATA += \
	os/Cygwin/hotkeys.ahk \
	os/Cygwin/minttyrc

appdataghislerdir = $(appdata)/GHISLER
appdataghisler_DATA = \
	os/Cygwin/GHISLER/lsplugin.ini \
	os/Cygwin/GHISLER/no.bar \
	os/Cygwin/GHISLER/tcignore.txt \
	os/Cygwin/GHISLER/totalcmd.ini \
	os/Cygwin/GHISLER/usercmd.ini \
	os/Cygwin/GHISLER/work.ini

appdataghislerlanguagesdir = $(appdataghislerdir)/languages
appdataghislerlanguages_DATA = \
	os/Cygwin/GHISLER/languages/wcmd_now.lng \
	os/Cygwin/GHISLER/languages/wcmd_now.mnu

appdataghislerpackersrardir = $(appdataghislerdir)/packers/rar
appdataghislerpackersrar_DATA = \
	os/Cygwin/GHISLER/packers/rar/default.sfx \
	os/Cygwin/GHISLER/packers/rar/wincon.sfx \
	os/Cygwin/GHISLER/packers/rar/zip.sfx
appdataghislerpackersrar_SCRIPTS = \
	os/Cygwin/GHISLER/packers/rar/rar.exe \
	os/Cygwin/GHISLER/packers/rar/unrar.exe

appdataghislerpluginsdir = $(appdataghislerdir)/plugins
appdataghislerpluginswcxdir = $(appdataghislerpluginsdir)/wcx
appdataghislerpluginswdxdir = $(appdataghislerpluginsdir)/wdx
appdataghislerpluginswfxdir = $(appdataghislerpluginsdir)/wfx
appdataghislerpluginswlxdir = $(appdataghislerpluginsdir)/wlx

appdataghislerpluginswcx7zipdir = $(appdataghislerpluginswcxdir)/7zip
appdataghislerpluginswcx7zip_DATA = \
	os/Cygwin/GHISLER/plugins/wcx/7zip/7zip.ini
appdataghislerpluginswcx7zip_SCRIPTS = \
	os/Cygwin/GHISLER/plugins/wcx/7zip/7zip.wcx

appdataghislerpluginswcxisodir = $(appdataghislerpluginswcxdir)/iso
appdataghislerpluginswcxiso_SCRIPTS = \
	os/Cygwin/GHISLER/plugins/wcx/iso/iso.wcx

appdataghislerpluginswcxtargzbz2dir = $(appdataghislerpluginswcxdir)/targzbz2
appdataghislerpluginswcxtargzbz2_SCRIPTS = \
	os/Cygwin/GHISLER/plugins/wcx/targzbz2/targzbz2.wcx

appdataghislerpluginswdxencodingdir = $(appdataghislerpluginswdxdir)/encoding
appdataghislerpluginswdxencoding_SCRIPTS = \
	os/Cygwin/GHISLER/plugins/wdx/encoding/encoding.wdx

appdataghislerpluginswdxunicodetestdir = $(appdataghislerpluginswdxdir)/unicodetest
appdataghislerpluginswdxunicodetest_SCRIPTS = \
	os/Cygwin/GHISLER/plugins/wdx/unicodetest/unicodetest.wdx

appdataghislerpluginswfxenvironmentdir = $(appdataghislerpluginswfxdir)/environment
appdataghislerpluginswfxenvironment_SCRIPTS = \
	os/Cygwin/GHISLER/plugins/wfx/environment/environment.wfx

appdataghislerpluginswfxregistrydir = $(appdataghislerpluginswfxdir)/registry
appdataghislerpluginswfxregistry_DATA = \
	os/Cygwin/GHISLER/plugins/wfx/registry/English.lng
appdataghislerpluginswfxregistry_SCRIPTS = \
	os/Cygwin/GHISLER/plugins/wfx/registry/registry.wfx

appdataghislerpluginswlxgswlxdir = $(appdataghislerpluginswlxdir)/gswlx
appdataghislerpluginswlxgswlx_SCRIPTS = \
	os/Cygwin/GHISLER/plugins/wlx/gswlx/gswlx.wlx

appdataghislerpluginswlximaginedir = $(appdataghislerpluginswlxdir)/imagine
appdataghislerpluginswlximagine_DATA = \
	os/Cygwin/GHISLER/plugins/wlx/imagine/imagine.ini
appdataghislerpluginswlximagine_SCRIPTS = \
	os/Cygwin/GHISLER/plugins/wlx/imagine/imagine.dll \
	os/Cygwin/GHISLER/plugins/wlx/imagine/imagine.wcx \
	os/Cygwin/GHISLER/plugins/wlx/imagine/imagine.wlx \
	os/Cygwin/GHISLER/plugins/wlx/imagine/plugin/j2k.dll \
	os/Cygwin/GHISLER/plugins/wlx/imagine/plugin/jbig.dll

appdataghislertoolsworkdir = $(appdataghislerdir)/tools/work
appdataghislertoolswork_SCRIPTS = \
	os/Cygwin/GHISLER/tools/work/open-in-tageditor-non-retardedly.vbs

$(call DIR,appdataghisler)
$(call DIR,appdataghislerlanguages)
$(call DIR,appdataghislerpackersrar)
$(call DIR,appdataghislerpluginswcx7zip)
$(call DIR,appdataghislerpluginswcxiso)
$(call DIR,appdataghislerpluginswcxtargzbz2)
$(call DIR,appdataghislerpluginswdxencoding)
$(call DIR,appdataghislerpluginswdxunicodetest)
$(call DIR,appdataghislerpluginswfxenvironment)
$(call DIR,appdataghislerpluginswfxregistry)
$(call DIR,appdataghislerpluginswlxgswlx)
$(call DIR,appdataghislerpluginswlximagine)
$(call DIR,appdataghislertoolswork)

appdatadialogdeathdir = $(appdatadir)/Dialog\ Death
appdatadialogdeath_DATA = \
	os/Cygwin/dialog-death.ini

$(call DIR,appdatadialogdeath)

startupdir = $(call shell_quote,$(shell cygpath -P))/Startup
startup_SCRIPTS = \
	os/Cygwin/start-up/clipx.lnk \
	os/Cygwin/start-up/cygwin.lnk \
	os/Cygwin/start-up/hotkeys.lnk \
	os/Cygwin/start-up/totalcmd.lnk

$(call DIR,startup)

share_DATA = \
	os/Cygwin/share/icons/bak.ico \
	os/Cygwin/share/icons/cmp.ico \
	os/Cygwin/share/icons/tageditor-generic.ico \
	os/Cygwin/share/icons/text-html.ico \
	os/Cygwin/share/icons/text-x-generic.ico

$(eval $(call GROUP_template,$(share_DATA),$(sharedir),,os/Cygwin/share/))

xdgconfighomezshos_DATA = \
	os/Cygwin/zsh/env \
	os/Cygwin/zsh/login \
	os/Cygwin/zsh/rc

bin_SCRIPTS += \
	os/Cygwin/bin/im
