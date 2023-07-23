# STUFF THAT SHOULD PROLLY BE DONE

## refractor code

+ move more stuff into files besides `src/Lib.hs`
+ rename `elipse` and `boxish` to `ellipsewave` and `rectwave` or something

## handle state better

formalize how `sel` works better, maybe not just a `Selection` that gets passed
from function call to function call but rather a `State Selection {(), Bool}`
that gets passed from function call to function call. I don't really wanna
use monad transformers because I don't want to lift all IO calls. Is there 
a better way?

## handle fonts better

don't just hardlink one font, do some other way, like
+ find fonts on system and pick one
+ have user path to font if they want or something
