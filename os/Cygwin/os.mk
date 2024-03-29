appdatadir := $(call shell_quote,$(shell cygpath -u "$(APPDATA)"))
fontsdir := $(call shell_quote,$(shell cygpath --folder 20))
userconfmozillafirefoxdir = $(appdatadir)/Mozilla/Firefox
xdgconfighomevlcdir = $(appdatadir)/vlc

userconf_DATA += \
	os/Cygwin/hotkeys.ahk \
	os/Cygwin/minttyrc

appdataghislerdir = $(appdatadir)/GHISLER
appdataghislerlanguagesdir = $(appdataghislerdir)/languages
appdataghislerpackersrardir = $(appdataghislerdir)/packers/rar
appdataghislerpluginsdir = $(appdataghislerdir)/plugins
appdataghislerpluginswcxdir = $(appdataghislerpluginsdir)/wcx
appdataghislerpluginswcx7zipdir = $(appdataghislerpluginswcxdir)/7zip
appdataghislerpluginswcxisodir = $(appdataghislerpluginswcxdir)/iso
appdataghislerpluginswcxtargzbz2dir = $(appdataghislerpluginswcxdir)/targzbz2
appdataghislerpluginswdxdir = $(appdataghislerpluginsdir)/wdx
appdataghislerpluginswdxencodingdir = $(appdataghislerpluginswdxdir)/encoding
appdataghislerpluginswdxunicodetestdir = $(appdataghislerpluginswdxdir)/unicodetest
appdataghislerpluginswfxdir = $(appdataghislerpluginsdir)/wfx
appdataghislerpluginswfxenvironmentdir = $(appdataghislerpluginswfxdir)/environment
appdataghislerpluginswfxregistrydir = $(appdataghislerpluginswfxdir)/registry
appdataghislerpluginswlxdir = $(appdataghislerpluginsdir)/wlx
appdataghislerpluginswlxgswlxdir = $(appdataghislerpluginswlxdir)/gswlx
appdataghislerpluginswlximaginedir = $(appdataghislerpluginswlxdir)/imagine
appdataghislerpluginswlximagineplugindir = $(appdataghislerpluginswlximaginedir)/plugin
appdataghislertoolsworkdir = $(appdataghislerdir)/tools/work
appdatadialogdeathdir = $(appdatadir)/Dialog\ Death
startupdir := $(call shell_quote,$(shell cygpath -P))/Startup

appdataghisler_DATA = \
	os/Cygwin/GHISLER/lsplugin.ini \
	os/Cygwin/GHISLER/no.bar \
	os/Cygwin/GHISLER/tcignore.txt \
	os/Cygwin/GHISLER/usercmd.ini \
	os/Cygwin/GHISLER/wincmd.ini \
	os/Cygwin/GHISLER/work.ini

appdataghislerlanguages_DATA = \
	os/Cygwin/GHISLER/languages/wcmd_now.lng \
	os/Cygwin/GHISLER/languages/wcmd_now.mnu

appdataghislerpackersrar_DATA = \
	os/Cygwin/GHISLER/packers/rar/default.sfx \
	os/Cygwin/GHISLER/packers/rar/wincon.sfx \
	os/Cygwin/GHISLER/packers/rar/zip.sfx

appdataghislerpackersrar_SCRIPTS = \
	os/Cygwin/GHISLER/packers/rar/rar.exe \
	os/Cygwin/GHISLER/packers/rar/unrar.exe

appdataghislerpluginswcx7zip_DATA = \
	os/Cygwin/GHISLER/plugins/wcx/7zip/7zip.ini

appdataghislerpluginswcx7zip_SCRIPTS = \
	os/Cygwin/GHISLER/plugins/wcx/7zip/7zip.wcx

appdataghislerpluginswcxiso_SCRIPTS = \
	os/Cygwin/GHISLER/plugins/wcx/iso/iso.wcx

appdataghislerpluginswcxtargzbz2_SCRIPTS = \
	os/Cygwin/GHISLER/plugins/wcx/targzbz2/targzbz2.wcx

appdataghislerpluginswdxencoding_SCRIPTS = \
	os/Cygwin/GHISLER/plugins/wdx/encoding/encoding.wdx

appdataghislerpluginswdxunicodetest_SCRIPTS = \
	os/Cygwin/GHISLER/plugins/wdx/unicodetest/unicodetest.wdx

appdataghislerpluginswfxenvironment_SCRIPTS = \
	os/Cygwin/GHISLER/plugins/wfx/environment/environment.wfx

appdataghislerpluginswfxregistry_DATA = \
	os/Cygwin/GHISLER/plugins/wfx/registry/English.lng

appdataghislerpluginswfxregistry_SCRIPTS = \
	os/Cygwin/GHISLER/plugins/wfx/registry/registry.wfx

appdataghislerpluginswlxgswlx_SCRIPTS = \
	os/Cygwin/GHISLER/plugins/wlx/gswlx/gswlx.wlx

appdataghislerpluginswlximagine_DATA = \
	os/Cygwin/GHISLER/plugins/wlx/imagine/imagine.ini

appdataghislerpluginswlximagine_SCRIPTS = \
	os/Cygwin/GHISLER/plugins/wlx/imagine/imagine.dll \
	os/Cygwin/GHISLER/plugins/wlx/imagine/imagine.wcx \
	os/Cygwin/GHISLER/plugins/wlx/imagine/imagine.wlx \

appdataghislerpluginswlximagineplugin_SCRIPTS = \
	os/Cygwin/GHISLER/plugins/wlx/imagine/plugin/j2k.dll \
	os/Cygwin/GHISLER/plugins/wlx/imagine/plugin/jbig.dll

ifeq ($(enable_fonts), yes)
fonts_DATA = \
	$(fontsdejavu_DATA) \
	$(fontsfiracode_DATA)

$(addprefix $(fontsdir)/,$(notdir $(fonts_DATA))): \
	INSTALL_DATA = $(srcdir)/build/Cygwin/install-font "$(abs_builddir)"
endif

startup_SCRIPTS = \
	os/Cygwin/start-up/clipx.lnk \
	os/Cygwin/start-up/cygwin.lnk \
	os/Cygwin/start-up/hotkeys.lnk \
	os/Cygwin/start-up/totalcmd.lnk

# share_DATA = \
# 	os/Cygwin/share/icons/bak.ico \
# 	os/Cygwin/share/icons/cmp.ico \
# 	os/Cygwin/share/icons/text-html.ico \
# 	os/Cygwin/share/icons/text-x-generic.ico

# $(eval $(call GROUP_template,$(share_DATA),$(datarootdir),,os/Cygwin/share/))

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
$(call DIR,appdataghislerpluginswlximagineplugin)
$(call DIR,appdataghislertoolswork)
$(call DIR,fonts)
$(call DIR,startup)
