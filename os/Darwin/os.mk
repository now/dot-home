INSTALL = ginstall
ZSHELL = /opt/local/bin/zsh

appsupportdir = $(prefix)/Library/Application\ Support
userconfaudacitydir = $(appsupportdir)/audacity
userconfmozillafirefoxdir = $(appsupportdir)/Firefox
xdgconfighomevlcdir = $(prefix)/Library/Preferences/org.videolan.vlc

# {
#     "com.apple.AppleMultitouchTrackpad" =     {
#         Clicking = 1;
#         DragLock = 0;
#         Dragging = 0;
#         TrackpadCornerSecondaryClick = 0;
#         TrackpadFiveFingerPinchGesture = 0;
#         TrackpadFourFingerHorizSwipeGesture = 2;
#         TrackpadFourFingerPinchGesture = 0;
#         TrackpadFourFingerVertSwipeGesture = 2;
#         TrackpadHandResting = 1;
#         TrackpadHorizScroll = 1;
#         TrackpadMomentumScroll = 1;
#         TrackpadPinch = 1;
#         TrackpadRightClick = 1;
#         TrackpadRotate = 1;
#         TrackpadScroll = 1;
#         TrackpadThreeFingerDrag = 1;
#         TrackpadThreeFingerHorizSwipeGesture = 0;
#         TrackpadThreeFingerTapGesture = 2;
#         TrackpadThreeFingerVertSwipeGesture = 0;
#         TrackpadTwoFingerDoubleTapGesture = 1;
#         TrackpadTwoFingerFromRightEdgeSwipeGesture = 0;
#         UserPreferences = 1;
#     };
#     "com.apple.dock" =     {
#         showDesktopGestureEnabled = 0;
#         showLaunchpadGestureEnabled = 0;
#         tilesize = 52;
#     };
os/Darwin/.defaults.stamp: os/Darwin/os.mk
	defaults write com.apple.Terminal ShowTabBarInFullScreen -bool no
	defaults write com.apple.dock autohide -bool yes
	defaults write com.apple.dock dashboard-in-overlay -bool yes
	defaults write com.apple.dock expose-animation-duration -float 0
	defaults write com.apple.dock launchanim -bool no
	defaults write com.apple.dock mru-spaces -bool no
	defaults write com.apple.dock orientation left
	defaults write com.apple.dock show-process-indicators -bool no
	defaults write com.apple.menuextra.battery ShowPercent -bool no
	defaults write com.apple.screensaver askForPassword -bool yes
	defaults write com.apple.screensaver askForPasswordDelay -int 900
	defaults write com.blacktree.Quicksilver 'Check for Updates' -bool yes
	defaults write com.blacktree.Quicksilver 'Hide Dock Icon' -bool yes
	defaults write com.blacktree.Quicksilver QSActivationHotKey -dict keyCode 49 modifiers 1310985
	defaults write com.blacktree.Quicksilver QSAgreementAccepted -bool yes
	defaults write com.blacktree.Quicksilver QSCatalogRescanFrequency -int 1440
	defaults write com.blacktree.Quicksilver QSShowMenuIcon -bool no
	defaults write com.blacktree.Quicksilver 'Setup Assistant Completed' -bool yes
	defaults write -g AppleEnableMenuBarTransparency -bool no
	defaults write -g AppleICUDateFormatStrings -dict 1 yy-MM-dd 2 y-MM-dd 3 y-MM-dd
	defaults write -g AppleICUNumberSymbols -dict 0 . 10 .
	defaults write -g AppleLocale en_SE
	defaults write -g AppleMeasurementUnits Centimeters
	defaults write -g AppleMetricUnits -bool true
	defaults write -g AppleScrollerPagingBehavior -int 1
	defaults write -g AppleUserLanguages -int 1
	defaults write -g InitialKeyRepeat -int 15
	defaults write -g KeyRepeat -int 2
	defaults write -g NSAutomaticWindowAnimationsEnabled -bool no
	defaults write -g NSToolbarFullScreenAnimationDuration -float 0
	defaults write -g NSWindowResizeTime -float 0.001
	defaults write -g QLPanelAnimationDuration -float 0
	defaults write -g com.apple.keyboard.fnState -int 1
	defaults write -g com.apple.springing.delay -float 0.5
	defaults write -g com.apple.springing.enabled -bool yes
	defaults write -g userMenuExtraStyle -int 2
	touch $@

bin/a bin/im: %: os/Darwin/%.in Makefile
	$(R_bin_GEN)
