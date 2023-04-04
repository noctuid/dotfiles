(ns wal.shared-paths
  "Shared globals for pywal/wallpaper related scripts.")

(def setroot-store-path
  "Path to save setroot arguments to."
  (str (System/getenv "HOME") "/.config/setroot/options"))
