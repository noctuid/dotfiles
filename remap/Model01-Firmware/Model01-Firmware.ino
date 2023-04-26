// -*- mode: c++ -*-
// Copyright 2016 Keyboardio, inc. <jesse@keyboard.io>
// See "LICENSE" for license details

#ifndef BUILD_INFORMATION
#define BUILD_INFORMATION "locally built on " __DATE__ " at " __TIME__
#endif

// * Differences from Kyria
// - no caps word: https://github.com/keyboardio/Kaleidoscope/issues/1223
// - no function layer (though don't currently need; have VT keys bound)
// - no qwerty layer (though don't currently need)
// - other slight differences for keyboard-specific thigns (e.g. prog key,
//   keyboardio has palm keys, etc.)
// - kyria doesn't have lock layer (but it's pretty useless)

// * Includes
/**
 * These #include directives pull in the Kaleidoscope firmware core,
 * as well as the Kaleidoscope plugins we use in the Model 01's firmware
 */

// The Kaleidoscope core
#include "Kaleidoscope.h"

// Support for storing the keymap in EEPROM
#include "Kaleidoscope-EEPROM-Settings.h"
#include "Kaleidoscope-EEPROM-Keymap.h"

// Support for communicating with the host via a simple Serial protocol
#include "Kaleidoscope-FocusSerial.h"

// Support for keys that move the mouse
#include "Kaleidoscope-MouseKeys.h"

// Support for macros
#include "Kaleidoscope-Macros.h"

// Support for controlling the keyboard's LEDs
#include "Kaleidoscope-LEDControl.h"

// Support for "Numpad" mode, which is mostly just the Numpad specific LED mode
// #include "Kaleidoscope-NumPad.h"

// Support for the "Boot greeting" effect, which pulses the 'LED' button for 10s
// when the keyboard is connected to a computer (or that computer is powered on)
#include "Kaleidoscope-LEDEffect-BootGreeting.h"

// Support for LED modes that set all LEDs to a single color
#include "Kaleidoscope-LEDEffect-SolidColor.h"

// Support for an LED mode that makes all the LEDs 'breathe'
#include "Kaleidoscope-LEDEffect-Breathe.h"

// Support for an LED mode that makes a red pixel chase a blue pixel across the keyboard
// #include "Kaleidoscope-LEDEffect-Chase.h"

// Support for LED modes that pulse the keyboard's LED in a rainbow pattern
#include "Kaleidoscope-LEDEffect-Rainbow.h"

// Support for an LED mode that lights up the keys as you press them
#include "Kaleidoscope-LED-Stalker.h"

// Support for shared palettes for other plugins, like Colormap below
#include "Kaleidoscope-LED-Palette-Theme.h"

// Support for an LED mode that lets one configure per-layer color maps
// #include "Kaleidoscope-Colormap.h"

// Support for Keyboardio's internal keyboard testing mode
#include "Kaleidoscope-HardwareTestMode.h"

// Support for host power management (suspend & wakeup)
#include "Kaleidoscope-HostPowerManagement.h"

// Support for magic combos (key chords that trigger an action)
#include "Kaleidoscope-MagicCombo.h"

// Support for USB quirks, like changing the key state report protocol
#include "Kaleidoscope-USB-Quirks.h"

// ** Added Plugins
// Dual-role
#include <Kaleidoscope-Qukeys.h>

#include <Kaleidoscope-CharShift.h>

#include <Kaleidoscope-LED-Wavepool.h>

// * Macro Names
/** This 'enum' is a list of all the macros used by the Model 01's firmware
  * The names aren't particularly important. What is important is that each
  * is unique.
  *
  * These are the names of your macros. They'll be used in two places.
  * The first is in your keymap definitions. There, you'll use the syntax
  * `M(MACRO_NAME)` to mark a specific keymap position as triggering `MACRO_NAME`
  *
  * The second usage is in the 'switch' statement in the `macroAction` function.
  * That switch statement actually runs the code associated with a macro when
  * a macro key is pressed.
  */

enum {
  MACRO_VERSION_INFO,
  MACRO_ANY,
};

// * Aliases
#define Key_Version M(MACRO_VERSION_INFO)

#define Key_Tilde LSHIFT(Key_Backtick)
#define Key_Exclam LSHIFT(Key_1)
#define Key_At LSHIFT(Key_2)
#define Key_Octophorpe LSHIFT(Key_3)
#define Key_Dollar LSHIFT(Key_4)
#define Key_Percent LSHIFT(Key_5)
#define Key_Caret LSHIFT(Key_6)
#define Key_Ampersand LSHIFT(Key_7)
#define Key_Asterisk LSHIFT(Key_8)
#define Key_Underscore LSHIFT(Key_Minus)
#define Key_Plus LSHIFT(Key_Equals)

#define Key_VT1 LCTRL(LALT(Key_F1))
#define Key_VT2 LCTRL(LALT(Key_F2))
#define Key_VT3 LCTRL(LALT(Key_F3))
#define Key_VT4 LCTRL(LALT(Key_F4))
#define Key_VT5 LCTRL(LALT(Key_F5))
#define Key_VT6 LCTRL(LALT(Key_F6))
#define Key_VT7 LCTRL(LALT(Key_F7))
#define Key_VT8 LCTRL(LALT(Key_F8))
#define Key_VT9 LCTRL(LALT(Key_F9))
#define Key_VT10 LCTRL(LALT(Key_F10))

#define Key_BsWord LCTRL(Key_Backspace)

#define Key_Play Consumer_Play
#define Key_Next Consumer_ScanNextTrack
#define Key_Prev Consumer_ScanPreviousTrack
#define Key_BrUp Consumer_DisplayBrightnessIncrement
#define Key_BrDown Consumer_DisplayBrightnessDecrement

// * Keymaps
/** The Model 01's key layouts are defined as 'keymaps'. By default, there are three
  * keymaps: The standard QWERTY keymap, the "Function layer" keymap and the "Numpad"
  * keymap.
  *
  * Each keymap is defined as a list using the 'KEYMAP_STACKED' macro, built
  * of first the left hand's layout, followed by the right hand's layout.
  *
  * Keymaps typically consist mostly of `Key_` definitions. There are many, many keys
  * defined as part of the USB HID Keyboard specification. You can find the names
  * (if not yet the explanations) for all the standard `Key_` defintions offered by
  * Kaleidoscope in these files:
  *    https://github.com/keyboardio/Kaleidoscope/blob/master/src/kaleidoscope/key_defs_keyboard.h
  *    https://github.com/keyboardio/Kaleidoscope/blob/master/src/kaleidoscope/key_defs_consumerctl.h
  *    https://github.com/keyboardio/Kaleidoscope/blob/master/src/kaleidoscope/key_defs_sysctl.h
  *    https://github.com/keyboardio/Kaleidoscope/blob/master/src/kaleidoscope/key_defs_keymaps.h
  *
  * Additional things that should be documented here include
  *   using ___ to let keypresses fall through to the previously active layer
  *   using XXX to mark a keyswitch as 'blocked' on this layer
  *   using ShiftToLayer() and LockLayer() keys to change the active keymap.
  *   keeping NUM and FN consistent and accessible on all layers
  *
  * The PROG key is special, since it is how you indicate to the board that you
  * want to flash the firmware. However, it can be remapped to a regular key.
  * When the keyboard boots, it first looks to see whether the PROG key is held
  * down; if it is, it simply awaits further flashing instructions. If it is
  * not, it continues loading the rest of the firmware and the keyboard
  * functions normally, with whatever binding you have set to PROG. More detail
  * here: https://community.keyboard.io/t/how-the-prog-key-gets-you-into-the-bootloader/506/8
  *
  * The "keymaps" data structure is a list of the keymaps compiled into the firmware.
  * The order of keymaps in the list is important, as the ShiftToLayer(#) and LockLayer(#)
  * macros switch to key layers based on this list.
  *
  *

  * A key defined as 'ShiftToLayer(FUNCTION)' will switch to FUNCTION while held.
  * Similarly, a key defined as 'LockLayer(NUMPAD)' will switch to NUMPAD when tapped.
  */

/**
  * Layers are "0-indexed" -- That is the first one is layer 0. The second one is layer 1.
  * The third one is layer 2.
  * This 'enum' lets us use names like QWERTY, FUNCTION, and NUMPAD in place of
  * the numbers 0, 1 and 2.
  *
  */

enum { COLEMAK_DH, ALTGR, NAVIGATION, LOCKED }; // layers

/* This comment temporarily turns off astyle's indent enforcement
 *   so we can make the keymaps actually resemble the physical key layout better
 */
// *INDENT-OFF*

// keycodes:
// https://kaleidoscope.readthedocs.io/en/stable/customization/keycodes.html
// https://github.com/keyboardio/Kaleidoscope/blob/master/src/kaleidoscope/key_defs/keyboard.h
KEYMAPS(
  [COLEMAK_DH] = KEYMAP_STACKED
  (//<prog>,          Key_1, Key_2, Key_3, Key_4, Key_5, Key_LEDEffectNext,
   ___,               XXX,   XXX,   XXX,   XXX,   XXX,   Key_LEDEffectNext,
   // Key_Backtick
   Key_LEDEffectNext, Key_Q, Key_W, Key_F, Key_P, Key_B, Key_Tab,
   XXX,               Key_A, Key_R, Key_S, Key_T, Key_G,
   Key_LeftShift,     XXX,   Key_X, Key_C, Key_D, Key_V, Key_Z, // Key_Escape,
   // control, shift, _, alt+super
   Key_Tab, Key_Escape, Key_Spacebar, Key_LeftBracket,
   ShiftToLayer(NAVIGATION),


   //            Key_6, Key_7, Key_8,     Key_9,      Key_0,         LockLayer(NUMPAD),
   M(MACRO_ANY), XXX,   XXX,   XXX,       XXX,        XXX,           LockLayer(LOCKED),
   // Key_Enter, Key_J, Key_L, Key_U,     Key_Y,      Key_Semicolon, Key_Equals,
   Key_Enter,    Key_J, Key_L, Key_U,     Key_Y,      Key_Semicolon, Key_KeypadPlusSlashMinus,
                 Key_K, Key_N, Key_E,     Key_I,      Key_O,         Key_Quote,
   // Key_Minus,
   Key_Version,  Key_M, Key_H, Key_Comma, Key_Period, Key_Slash,     XXX,
   // super, nav, altgr, alt
   Key_LeftParen, Key_Backspace, Key_Enter, Key_LeftCurlyBracket,
   Key_RightShift),

  [ALTGR] =  KEYMAP_STACKED
  // virtual terminals
  (___, Key_VT1,      Key_VT2,    Key_VT3,          Key_VT4,        Key_VT5,       ___,
   // symbols
   ___, Key_Backtick, Key_Equals, Key_RightBracket, Key_Underscore, Key_Backslash, ___,
   ___, Key_1,        Key_2,      Key_3,            Key_4,          Key_Pipe,
   ___, ___,          Key_Tilde,  Key_Octophorpe,   Key_5,          Key_Caret,     ___,
   ___, ___,          ___,        ___,
   ___,


   // virtual terminals
   ___, Key_VT6,       Key_VT7,   Key_VT8,        Key_VT9,               Key_VT10,    ___,
   // symbols
   ___, Key_At,        Key_Minus, Key_RightParen, Key_RightCurlyBracket, Key_Exclam,  ___,
        Key_Ampersand, Key_7,     Key_8,          Key_9,                 Key_0,       ___,
   ___, Key_Dollar,    Key_6,     Key_Asterisk,   Key_Plus,          Key_Percent, ___,
   ___, ___,   ___,   ___,
   ___),

  [NAVIGATION] =  KEYMAP_STACKED
  (___, ___, ___,           ___,            ___,             ___, ___,
   ___, ___, Key_BsWord,    Key_mouseUp,    Key_PrintScreen, ___, ___,
   // probably won't use
   ___, ___, Key_mouseL,    Key_mouseDn,    Key_mouseR,      ___,
   ___, ___, Key_mouseBtnL, Key_mouseBtnM,  Key_mouseBtnR,   ___, ___,
   // NOTE C-S-Tab will still act as shift when held (qukeys is position based for any layer)
   // C-Tab on space for shortcuts in applications (e.g. browser fallback)
   LSHIFT(Key_Tab), LCTRL(LSHIFT(Key_Tab)), LCTRL(Key_Tab), ___,
   Key_mouseScrollDn,


   ___, ___,      ___,           ___,            ___,            ___,        ___,
   ___, Key_BrUp, Key_PageDown,  Key_Home,       Key_End,        Key_PageUp, Key_BrDown,
        CS(0),    Key_DownArrow, Key_UpArrow,    Key_RightArrow, Key_Play,   ___,
   ___, Key_Mute, Key_LeftArrow, Key_VolumeDown, Key_VolumeUp,   ___,        ___,
   ___, ___,          ___,           ___,
   Key_mouseScrollUp),

  [LOCKED] =  KEYMAP_STACKED
  (XXX, XXX, XXX, XXX, XXX, XXX, XXX,
   XXX, XXX, XXX, XXX, XXX, XXX, XXX,
   XXX, XXX, XXX, XXX, XXX, XXX,
   XXX, XXX, XXX, XXX, XXX, XXX, XXX,
   XXX, XXX, XXX, XXX,
   XXX,


   XXX, XXX, XXX, XXX, XXX, XXX, XXX,
   XXX, XXX, XXX, XXX, XXX, XXX, XXX,
        XXX, XXX, XXX, XXX, XXX, XXX,
   ___, XXX, XXX, XXX, XXX, XXX, XXX,
   XXX, XXX, XXX, XXX,
   XXX)
  )

/* Re-enable astyle's indent enforcement */
// *INDENT-ON*

// * Functions and Variables
/** versionInfoMacro handles the 'firmware version info' macro
 *  When a key bound to the macro is pressed, this macro
 *  prints out the firmware build information as virtual keystrokes
 */

static void versionInfoMacro(uint8_t key_state) {
  if (keyToggledOn(key_state)) {
    Macros.type(PSTR("Keyboardio Model 01 - Kaleidoscope "));
    Macros.type(PSTR(BUILD_INFORMATION));
  }
}

/** anyKeyMacro is used to provide the functionality of the 'Any' key.
 *
 * When the 'any key' macro is toggled on, a random alphanumeric key is
 * selected. While the key is held, the function generates a synthetic
 * keypress event repeating that randomly selected key.
 *
 */

static void anyKeyMacro(KeyEvent &event) {
  if (keyToggledOn(event.state)) {
    event.key.setKeyCode(Key_A.getKeyCode() + (uint8_t)(millis() % 36));
    event.key.setFlags(0);
  }
}


/** macroAction dispatches keymap events that are tied to a macro
    to that macro. It takes two uint8_t parameters.

    The first is the macro being called (the entry in the 'enum' earlier in this file).
    The second is the state of the keyswitch. You can use the keyswitch state to figure out
    if the key has just been toggled on, is currently pressed or if it's just been released.

    The 'switch' statement should have a 'case' for each entry of the macro enum.
    Each 'case' statement should call out to a function to handle the macro in question.

 */

const macro_t *macroAction(uint8_t macro_id, KeyEvent &event) {
  switch (macro_id) {

  case MACRO_VERSION_INFO:
    versionInfoMacro(event.state);
    break;

  case MACRO_ANY:
    anyKeyMacro(event);
    break;

  }
  return MACRO_NONE;
}



// These 'solid' color effect definitions define a rainbow of
// LED color modes calibrated to draw 500mA or less on the
// Keyboardio Model 01.


static kaleidoscope::plugin::LEDSolidColor solidRed(160, 0, 0);
static kaleidoscope::plugin::LEDSolidColor solidOrange(140, 70, 0);
static kaleidoscope::plugin::LEDSolidColor solidYellow(130, 100, 0);
static kaleidoscope::plugin::LEDSolidColor solidGreen(0, 160, 0);
static kaleidoscope::plugin::LEDSolidColor solidBlue(0, 70, 130);
static kaleidoscope::plugin::LEDSolidColor solidIndigo(0, 0, 170);
static kaleidoscope::plugin::LEDSolidColor solidViolet(130, 0, 120);

/** toggleLedsOnSuspendResume toggles the LEDs off when the host goes to sleep,
 * and turns them back on when it wakes up.
 */
void toggleLedsOnSuspendResume(kaleidoscope::plugin::HostPowerManagement::Event event) {
  switch (event) {
  case kaleidoscope::plugin::HostPowerManagement::Suspend:
    LEDControl.disable();
    break;
  case kaleidoscope::plugin::HostPowerManagement::Resume:
    LEDControl.enable();
    break;
  case kaleidoscope::plugin::HostPowerManagement::Sleep:
    break;
  }
}

/** hostPowerManagementEventHandler dispatches power management events (suspend,
 * resume, and sleep) to other functions that perform action based on these
 * events.
 */
void hostPowerManagementEventHandler(kaleidoscope::plugin::HostPowerManagement::Event event) {
  toggleLedsOnSuspendResume(event);
}

/** This 'enum' is a list of all the magic combos used by the Model 01's
 * firmware The names aren't particularly important. What is important is that
 * each is unique.
 *
 * These are the names of your magic combos. They will be used by the
 * `USE_MAGIC_COMBOS` call below.
 */
enum {
  // Toggle between Boot (6-key rollover; for BIOSes and early boot) and NKRO
  // mode.
  COMBO_TOGGLE_NKRO_MODE,
  // Enter test mode
  COMBO_ENTER_TEST_MODE
};

/** Wrappers, to be used by MagicCombo. **/

/**
 * This simply toggles the keyboard protocol via USBQuirks, and wraps it within
 * a function with an unused argument, to match what MagicCombo expects.
 */
static void toggleKeyboardProtocol(uint8_t combo_index) {
  USBQuirks.toggleKeyboardProtocol();
}

/**
 *  This enters the hardware test mode
 */
static void enterHardwareTestMode(uint8_t combo_index) {
  HardwareTestMode.runTests();
}

/** Magic combo list, a list of key combo and action pairs the firmware should
 * recognise.
 */
// https://github.com/keyboardio/Kaleidoscope/blob/9dea2a6083afa8d64f804dacd762601ba781b64f/doc/model01_coordinates.png
USE_MAGIC_COMBOS(
  {.action = toggleKeyboardProtocol,
   // Left Fn + Esc + Shift
   .keys = { R3C6, R2C6, R3C7 }

  }, {
    .action = enterHardwareTestMode,
    // Left Fn + Prog + LED
    .keys = { R3C6, R0C0, R0C6 }
  },
);

// ** Added Configuration
// void tapDanceAction(uint8_t tap_dance_index, byte row, byte col, uint8_t tap_count,
//                     kaleidoscope::plugin::TapDance::ActionType tap_dance_action) {
//   switch (tap_dance_index) {
//   case 0:
//     return tapDanceActionKeys(tap_count, tap_dance_action,
//                               LockLayer(NUMPAD), LockLayer(LOCKED));
//   }
// }

// * Plugins
// First, tell Kaleidoscope which plugins you want to use.
// The order can be important. For example, LED effects are
// added in the order they're listed here.
KALEIDOSCOPE_INIT_PLUGINS(
  // added; recommended to be first
  Qukeys,
  // The EEPROMSettings & EEPROMKeymap plugins make it possible to have an
  // editable keymap in EEPROM.
  EEPROMSettings,
  EEPROMKeymap,

  // Focus allows bi-directional communication with the host, and is the
  // interface through which the keymap in EEPROM can be edited.
  Focus,

  // FocusSettingsCommand adds a few Focus commands, intended to aid in
  // changing some settings of the keyboard, such as the default layer (via the
  // `settings.defaultLayer` command)
  FocusSettingsCommand,

  // FocusEEPROMCommand adds a set of Focus commands, which are very helpful in
  // both debugging, and in backing up one's EEPROM contents.
  FocusEEPROMCommand,

  // The boot greeting effect pulses the LED button for 10 seconds after the
  // keyboard is first connected
  BootGreetingEffect,

  // The hardware test mode, which can be invoked by tapping Prog, LED and the
  // left Fn button at the same time.
  HardwareTestMode,

  // LEDControl provides support for other LED modes
  LEDControl,

  // We start with the LED effect that turns off all the LEDs.
  LEDOff,

  // The rainbow effect changes the color of all of the keyboard's keys at the same time
  // running through all the colors of the rainbow.
  LEDRainbowEffect,

  // The rainbow wave effect lights up your keyboard with all the colors of a rainbow
  // and slowly moves the rainbow across your keyboard
  LEDRainbowWaveEffect,

  // The chase effect follows the adventure of a blue pixel which chases a red pixel across
  // your keyboard. Spoiler: the blue pixel never catches the red pixel
  // LEDChaseEffect,

  // These static effects turn your keyboard's LEDs a variety of colors
  solidRed, solidOrange, solidYellow, solidGreen, solidBlue, solidIndigo, solidViolet,

  // The breathe effect slowly pulses all of the LEDs on your keyboard
  LEDBreatheEffect,

  // The stalker effect lights up the keys you've pressed recently
  StalkerEffect,

  // The LED Palette Theme plugin provides a shared palette for other plugins,
  // like Colormap below
  LEDPaletteTheme,

  // The Colormap effect makes it possible to set up per-layer colormaps
  // ColormapEffect,

  // The numpad plugin is responsible for lighting up the 'numpad' mode
  // with a custom LED effect
  // NumPad,

  // The macros plugin adds support for macros
  Macros,

  // The MouseKeys plugin lets you add keys to your keymap which move the mouse.
  MouseKeys,

  // The HostPowerManagement plugin allows us to turn LEDs off when then host
  // goes to sleep, and resume them when it wakes up.
  HostPowerManagement,

  // The MagicCombo plugin lets you use key combinations to trigger custom
  // actions - a bit like Macros, but triggered by pressing multiple keys at the
  // same time.
  MagicCombo,

  // The USBQuirks plugin lets you do some things with USB that we aren't
  // comfortable - or able - to do automatically, but can be useful
  // nevertheless. Such as toggling the key report protocol between Boot (used
  // by BIOSes) and Report (NKRO).
  USBQuirks,

  // ** Added Plugins
  CharShift,
  WavepoolEffect
  );

// * Setup
/** The 'setup' function is one of the two standard Arduino sketch functions.
 * It's called when your keyboard first powers up. This is where you set up
 * Kaleidoscope and any plugins.
 */

void setup() {
  // First, call Kaleidoscope's internal setup function
  Kaleidoscope.setup();

  // While we hope to improve this in the future, the NumPad plugin
  // needs to be explicitly told which keymap layer is your numpad layer
  // NumPad.numPadLayer = NUMPAD;

  // We set the brightness of the rainbow effects to 150 (on a scale of 0-255)
  // This draws more than 500mA, but looks much nicer than a dimmer effect
  LEDRainbowEffect.brightness(150);
  LEDRainbowWaveEffect.brightness(150);

  // Set the action key the test mode should listen for to Left Fn
  HardwareTestMode.setActionKey(R3C6);

  // The LED Stalker mode has a few effects. The one we like is called
  // 'BlazingTrail'. For details on other options, see
  // https://github.com/keyboardio/Kaleidoscope/blob/master/docs/plugins/LED-Stalker.md
  StalkerEffect.variant = STALKER(BlazingTrail);

  // We want to make sure that the firmware starts with LED effects off
  // This avoids over-taxing devices that don't have a lot of power to share
  // with USB devices
  LEDOff.activate();

  // To make the keymap editable without flashing new firmware, we store
  // additional layers in EEPROM. For now, we reserve space for five layers. If
  // one wants to use these layers, just set the default layer to one in EEPROM,
  // by using the `settings.defaultLayer` Focus command, or by using the
  // `keymap.onlyCustom` command to use EEPROM layers only.
  EEPROMKeymap.setup(5);

  // We need to tell the Colormap plugin how many layers we want to have custom
  // maps for. To make things simple, we set it to five layers, which is how
  // many editable layers we have (see above).
  // ColormapEffect.max_layers(5);

  // ** Added Settings
  QUKEYS(
    // control/tab
    kaleidoscope::plugin::Qukey(0, KeyAddr(0, 7), Key_LeftControl),
    // shift/escape
    kaleidoscope::plugin::Qukey(0, KeyAddr(1, 7), Key_LeftShift),
    // necessary since something else is bound on this layer
    kaleidoscope::plugin::Qukey(NAVIGATION, KeyAddr(1, 7), Key_LeftShift),

    // alt+super/[
    kaleidoscope::plugin::Qukey(0, KeyAddr(3, 7), LCTRL(Key_LeftGui)),

    // super/(
    kaleidoscope::plugin::Qukey(0, KeyAddr(3, 8), Key_RightGui),
    // backspace/nav
    kaleidoscope::plugin::Qukey(0, KeyAddr(2, 8), ShiftToLayer(NAVIGATION)),
    // altgr/enter
    kaleidoscope::plugin::Qukey(0, KeyAddr(1, 8), ShiftToLayer(ALTGR)),
    // alt/{
    kaleidoscope::plugin::Qukey(0, KeyAddr(0, 8), Key_LeftAlt)
  )
  // Qukeys.setHoldTimeout(250);
  // decrease required overlap from 80%; favor modifiers like dumb xcape
  Qukeys.setOverlapThreshold(50);
  // Qukeys.setMinimumHoldTime(50);
  // Qukeys.setMinimumPriorInterval(75);
  // disable tap repeat; not disabling since occasionally repeat backspace
  // Qukeys.setMaxIntervalForTapRepeat(0);
  // Qukeys.activate();

  CS_KEYS(
    kaleidoscope::plugin::CharShift::KeyPair(Key_Next, Key_Prev),
  );

  MouseKeys.speed = 30;
  MouseKeys.accelSpeed = 0;
}

// * Loop
/** loop is the second of the standard Arduino sketch functions.
  * As you might expect, it runs in a loop, never exiting.
  *
  * For Kaleidoscope-based keyboard firmware, you usually just want to
  * call Kaleidoscope.loop(); and not do anything custom here.
  */

void loop() {
  Kaleidoscope.loop();
}

// Local Variables:
// aggressive-indent-excluded-modes: (c++-mode)
// fill-column: 200
// End:
