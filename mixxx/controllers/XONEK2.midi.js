var XoneK2 = {
  init: function(id, debugging) {
    // TODO Remove.
    engine.connectControl('[Channel1]', 'pitch_adjust', function(value) { print(value); });
  },
  shutdown: function(id) {
    // TODO Clear buttons
  },
  clickButton: function(group, control) {
    engine.setParameter(group, control, 1);
    engine.setParameter(group, control, 0);
  },
  turnKnob: function(group, control, value) {
    engine.setParameter(group, control, value > 63 ? value - 128 : value);
  },
  unshiftedLayer: {
    hotcueActivateOrClear: function(hotcue, channel, control, value, status, group) {
      XoneK2.clickButton(group, 'hotcue_' + hotcue + '_activate');
    }
  },
  shiftedLayer: {
    hotcueActivateOrClear: function(hotcue, channel, control, value, status, group) {
      XoneK2.clickButton(group, 'hotcue_' + hotcue + '_clear');
    }
  },
  shiftOn: function(channel, control, value, status, group) {
    XoneK2.layer = XoneK2.shiftedLayer;
    midi.sendShortMsg(status, control, value);
  },
  shiftOff: function(channel, control, value, status, group) {
    XoneK2.layer = XoneK2.unshiftedLayer;
    midi.sendShortMsg(status, control, value);
  },
  maximizeLibrary: function() {
    if (!engine.getParameter('[Master]', 'maximize_library'))
      engine.setParameter('[Master]', 'maximize_library', 1);
  },
  hideLibrary: function() {
    if (!engine.getParameter('[Master]', 'maximize_library'))
      return false;
    engine.setParameter('[Master]', 'maximize_library', 0);
    return true;
  },
  toggleSelectedSidebarItem: function(channel, control, value, status, group) {
    if (!engine.getParameter('[Master]', 'maximize_library'))
      engine.setParameter('[Master]', 'maximize_library', 1);
    else
      XoneK2.clickButton(group, 'ToggleSelectedSidebarItem');
  },
  selectPlaylist: function(channel, control, value, status, group) {
    XoneK2.maximizeLibrary();
    XoneK2.turnKnob(group, 'SelectPlaylist', value);
  },
  selectTrackKnob: function(channel, control, value, status, group) {
    XoneK2.maximizeLibrary();
    XoneK2.turnKnob(group, 'SelectTrackKnob', value);
  },
  loadSelectedIntoFirstStopped: function(channel, control, value, status, group) {
    if (XoneK2.hideLibrary())
      XoneK2.clickButton(group, 'LoadSelectedIntoFirstStopped');
  },
  groupToDeck: function(group) {
    return parseInt(group.substring(8,9));
  },
  scratchEnable: function(channel, control, value, status, group) {
    engine.scratchEnable(XoneK2.groupToDeck(group), 128, 33+1/3, 1.0 / 8, 1.0 / 8 / 32);
  },
  scratchDisable: function(channel, control, value, status, group) {
    engine.scratchDisable(XoneK2.groupToDeck(group));
  },
  jogOrScratch: function(channel, control, value, status, group) {
    if (engine.isScratching(1))
      engine.scratchTick(XoneK2.groupToDeck(group), value > 63 ? value - 128 : value);
    else
      XoneK2.turnKnob(group, 'jog', value);
  },
  beatsAdjust: function(channel, control, value, status, group) {
    XoneK2.clickButton(group, value > 63 ? 'beats_adjust_faster' : 'beats_adjust_slower');
  },
  reverseToggle: function(channel, control, value, status, group) {
    engine.setParameter(group, 'reverse',
                        !engine.getParameter(group, 'reverse'));
  },
  loopLengths: [
    0.03125, 0.0625, 0.125, 0.25, 0.5, 1,
    2, 4, 8, 16, 32, 64,
  ],
  findLoopLength: function(group) {
    var s = engine.getValue(group, 'loop_start_position');
    var e = engine.getValue(group, 'loop_end_position');
    if (s == -1 || e == -1)
      return -1;
    var beats = ((e - s) /
        (engine.getValue(group, 'track_samples') /
          engine.getValue(group, 'duration'))) *
        engine.getValue(group, 'bpm') / 60;
    var best_i = -1, best_d;
    for (var i = 0; i < XoneK2.loopLengths.length; i++) {
      var d = Math.abs(beats - XoneK2.loopLengths[i]);
      if (best_i == -1 || best_d > d)
        best_i = i, best_d = d;
    }
    return best_i;
  },
  findLoopLengthDefault: function(group, control) {
    var i = XoneK2.findLoopLength(group);
    return i == -1 ? 7 : i;
  },
  beatloopToggle: function(channel, control, value, status, group) {
    engine.setParameter(group, 'beatloop_' +
                        XoneK2.loopLengths[XoneK2.findLoopLengthDefault(group, control)] +
                        '_toggle', 1);
  },
  beatloopSize: function(channel, control, value, status, group) {
    var i = Math.floor(XoneK2.loopLengths.length * value / 127);
    var c = XoneK2.findLoopLength(group);
    if (i != c) {
      if (c == -1) {
        engine.setParameter(group, 'beatloop_' + XoneK2.loopLengths[i] + '_activate', 0);
        engine.setParameter(group, 'beatloop_' + XoneK2.loopLengths[i] + '_toggle', 0);
      } else if (Math.abs(i - c) == 1) // TODO Do we want this semi-soft-takeover?
        XoneK2.clickButton(group, i > c ? 'loop_double' : 'loop_halve');
    }
  },
};

XoneK2.layer = XoneK2.unshiftedLayer;

for (var i = 1; i <= 4; i++)
  (function(i) {
    XoneK2['hotcue' + i + 'ActivateOrClear'] =
      function(channel, control, value, status, group) {
        XoneK2.layer.hotcueActivateOrClear(i, channel, control, value, status, group);
      };
  })(i);
