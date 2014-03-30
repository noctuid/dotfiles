-- my old config from when I used xmonad and xfce
-- I don't remember where I found the config for using a prefix key but will put link when I find it
import XMonad
import XMonad.Config.Xfce
-- for space in between windows
import XMonad.Layout.Spacing 
-- for prefix key
import XMonad.Actions.Submap
import XMonad.Util.Paste
import Data.Map as M
import qualified XMonad.StackSet as W

main = xmonad xfceConfig {
     -- for prefix key
     XMonad.keys = prefixKey myPrefix,
     XMonad.modMask = 0,
     XMonad.mouseBindings = myMouseBindings,
     borderWidth = myBorderWidth,
     layoutHook = myLayout,
     normalBorderColor = "#46CBF0",
     focusedBorderColor = "#F06B46"  
  }

myBorderWidth = 2

myLayout = tiled ||| Mirror tiled ||| Full  
     where  
     -- default tiling algorithm partitions the screen into two panes  
     tiled = spacing 3 $ Tall nmaster delta ratio  
  
     -- The default number of windows in the master pane  
     nmaster = 1  
  
     -- Default proportion of screen occupied by master pane  
     ratio = 5/9  
  
     -- Percent of screen to increment by when resizing panes  
     delta = 5/100  

-- Get it to not tile all the way past the panel
-- for prefix key
prefixKey pfx@(modifier, keycode) x = M.fromList $
     let oldKeys = XMonad.keys defaultConfig x
         mine = M.fromList (myKeys x)
         merged = M.union oldKeys mine
     in
        [ (pfx, submap merged) ]

myMouseMod = mod1Mask

-- won't work with Escape on the caps layer
myPrefix = (0, xK_Escape) 
myKeys x = [ (myPrefix, sendKey (fst myPrefix) (snd myPrefix)) ]

myMouseBindings x = M.fromList $
       -- mod-button1 %! Set the window to floating mode and move by dragging
    [ ((myMouseMod, button1), (\w -> focus w >> mouseMoveWindow w
                                          >> windows W.shiftMaster))
    -- mod-button2 %! Raise the window to the top of the stack
    , ((myMouseMod, button2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3 %! Set the window to floating mode and resize by dragging
    , ((myMouseMod, button3), (\w -> focus w >> mouseResizeWindow w
                                          >> windows W.shiftMaster))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
-- end prefix key stuff

-- modMask = mod4Mask
-- to reload: xmonad --recompile and xmonad --restart
-- set up alias as rldxmon

-- Width of the window border in pixels.
--
