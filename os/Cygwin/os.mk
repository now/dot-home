firefoxprofilesdir=$(appdatadir)/Mozilla/Firefox

DOTFILES = \
	   os/Cygwin/firefox/profiles.ini

$(call GROUP_template,$(DOTFILES),$(firefoxprofilesdir),,os/Cygwin/firefox/)

DOTFILES = \
	   share/emacs/site-lisp/windows-path.el

$(emacsuserloaddefs): $(DOTFILES)

$(call EMACS_template,$(DOTFILES),$(userconfdir))

DOTFILES = \
	   os/Cygwin/autohotkey/digraphs.ahk \
	   os/Cygwin/hotkeys.ahk \
	   os/Cygwin/minttyrc

$(call GROUP_template,$(DOTFILES),$(userconfdir),.,os/Cygwin/)

APPDATAFILES = \
	       os/Cygwin/GHISLER/lsplugin.ini \
	       os/Cygwin/GHISLER/no.bar \
	       os/Cygwin/GHISLER/packers/rar/default.sfx \
	       os/Cygwin/GHISLER/packers/rar/wincon.sfx \
	       os/Cygwin/GHISLER/packers/rar/zip.sfx \
	       os/Cygwin/GHISLER/plugins/wcx/7zip/7zip.ini \
	       os/Cygwin/GHISLER/plugins/wfx/registry/English.lng \
	       os/Cygwin/GHISLER/plugins/wfx/sftp/rc/wfx_sftp_cfg.xrc \
	       os/Cygwin/GHISLER/plugins/wlx/imagine/imagine.ini \
	       os/Cygwin/GHISLER/plugins/wlx/ttfview/text.pat \
	       os/Cygwin/GHISLER/tango.icl \
	       os/Cygwin/GHISLER/tcignore.txt \
	       os/Cygwin/GHISLER/totalcmd.ini \
	       os/Cygwin/GHISLER/usercmd.ini \
	       os/Cygwin/GHISLER/work.ini

APPDATABINFILES = \
		  os/Cygwin/GHISLER/languages/wcmd_now.lng \
		  os/Cygwin/GHISLER/languages/wcmd_now.mnu \
		  os/Cygwin/GHISLER/packers/rar/rar.exe \
		  os/Cygwin/GHISLER/packers/rar/unrar.exe \
		  os/Cygwin/GHISLER/plugins/wcx/7zip/7zip.wcx \
		  os/Cygwin/GHISLER/plugins/wcx/iso/iso.wcx \
		  os/Cygwin/GHISLER/plugins/wcx/targzbz2/targzbz2.wcx \
		  os/Cygwin/GHISLER/plugins/wdx/encoding/encoding.wdx \
		  os/Cygwin/GHISLER/plugins/wdx/unicodetest/unicodetest.wdx \
		  os/Cygwin/GHISLER/plugins/wfx/environment/environment.wfx \
		  os/Cygwin/GHISLER/plugins/wfx/registry/registry.wfx \
		  os/Cygwin/GHISLER/plugins/wfx/sftp/password_crypter.dll \
		  os/Cygwin/GHISLER/plugins/wfx/sftp/psftp.dll \
		  os/Cygwin/GHISLER/plugins/wfx/sftp/sftp.wfx \
		  os/Cygwin/GHISLER/plugins/wfx/sftp/wfx_sftp_cfg.dll \
		  os/Cygwin/GHISLER/plugins/wlx/gswlx/gswlx.wlx \
		  os/Cygwin/GHISLER/plugins/wlx/imagine/imagine.dll \
		  os/Cygwin/GHISLER/plugins/wlx/imagine/imagine.wcx \
		  os/Cygwin/GHISLER/plugins/wlx/imagine/imagine.wlx \
		  os/Cygwin/GHISLER/plugins/wlx/imagine/plugin/j2k.dll \
		  os/Cygwin/GHISLER/plugins/wlx/imagine/plugin/jbig.dll \
		  os/Cygwin/GHISLER/plugins/wlx/ttfview/ttfview.wlx \
		  os/Cygwin/GHISLER/tango_shell32.dll \
		  os/Cygwin/GHISLER/tools/copy-to-multiple-folders.vbs \
		  os/Cygwin/GHISLER/tools/execute-many.vbs \
		  os/Cygwin/GHISLER/tools/work/add-files-to-trados-workbench-dialog.vbs \
		  os/Cygwin/GHISLER/tools/work/isodraw-run-macro.vbs \
		  os/Cygwin/GHISLER/tools/work/open-in-tradobeindecs.vbs \
		  os/Cygwin/GHISLER/tools/work/open-in-tageditor-non-retardedly.vbs \
		  os/Cygwin/GHISLER/tools/work/open-in-workbench-non-retardedly.vbs \
		  os/Cygwin/GHISLER/tools/work/ttx2x.wsf

$(eval $(call GROUP_template,$(APPDATAFILES),$(appdatadir),,os/Cygwin/))
$(eval $(call GROUP_template,$(APPDATABINFILES),$(appdatadir),,os/Cygwin/,755))

APPDATAFILES = \
	       os/Cygwin/dialog-death.ini

$(eval $(call GROUP_template,$(APPDATAFILES),$(appdatadir)/Dialog\ Death,,os/Cygwin/))

BINFILES = \
	   os/Cygwin/bin/im

$(call GROUP_template,$(BINFILES),~,,os/Cygwin/,755)

STARTUPFILES = \
	       os/Cygwin/start-up/clipx.lnk \
	       os/Cygwin/start-up/cygwin.lnk \
	       os/Cygwin/start-up/firefox.lnk \
	       os/Cygwin/start-up/hotkeys.lnk \
	       os/Cygwin/start-up/outlook.lnk \
	       os/Cygwin/start-up/totalcmd.lnk

startupdir = $(call shell_quote,$(shell cygpath -P))/Startup

$(eval $(call GROUP_template,$(STARTUPFILES),$(startupdir),,os/Cygwin/start-up/,755))

SHAREFILES = \
	     os/Cygwin/share/icons/bak.ico \
	     os/Cygwin/share/icons/cmp.ico \
	     os/Cygwin/share/icons/joy.ico \
	     os/Cygwin/share/icons/ord.ico \
	     os/Cygwin/share/icons/tageditor-generic.ico \
	     os/Cygwin/share/icons/text-html.ico \
	     os/Cygwin/share/icons/text-x-generic.ico

$(eval $(call GROUP_template,$(SHAREFILES),$(userconfdir),,os/Cygwin/))
