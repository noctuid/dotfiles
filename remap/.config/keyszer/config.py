timeouts(
    multipurpose=0,
    # act as modifier immediately, don't wait
    suspend=0,
)

# it's not perfect
# multipurpose_modmap("default",
#     {
#         Key.BTN_LEFT: [Key.KPLEFTPAREN, ],
#         Key.BTN_RIGHT: [Key.KPLEFTPAREN, Key.RIGHT_META],

#      }
# )

Modifier("L_HYPER", aliases=["LHyper"], key=Key.F24)
LHyper = Key.F24

modmap(
    "default",
    {Key.BTN_LEFT: LHyper, Key.BTN_MIDDLE: Key.CAPSLOCK, Key.BTN_RIGHT: Key.LEFT_META},
)
