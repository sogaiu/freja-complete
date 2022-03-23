(import freja/default-hotkeys :as dh)

(import ./freja-complete :as fc)

(dh/set-key dh/gb-binds
            [:alt :/]
            (comp dh/reset-blink fc/complete))

