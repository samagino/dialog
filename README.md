# dialog

now you can talk to your computer in simple yet wonderful ways, look at that!

## dependencies

you're gonna need the haskell tool `stack` to build this guy, if/when you got
that you can clone this repo and `cd` to it and run

```
stack build
```

to build it and then you can run

```
stack exec prompt <command> <prompt>
```

to test it from that same project directory and

```
stack install
```

to install it so you can run

```
prompt <command> <prompt>
```

from whichever directory your heart desires

you're also gonna need `sdl2`, `sdl2-ttf`, and `sdl2-gfx`. so also install
those by whatever means you prefer according to your computer and operating
system and all that jazz.
 
## font

a path to a font is hardcoded in `src/PromptWindow.hs` so you will prolly have
to change this path to point to a font that is on your system to get
things to work. the relevant line of code looks like:

```
font <- F.load "/usr/share/fonts/TTF/Crimson-Roman.ttf" 42
```
