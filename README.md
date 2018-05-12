# Animate Preview

## Usage

```
Usage: animate-preview --target STRING [--image STRING] [--high-dpi] [--watch]

Available options:
  -h,--help                Show this help text
  --target STRING          File path with sprite information (YAML or JSON)
  --image STRING           Force sprite sheet's file path
  --high-dpi               Use high DPI (if available)
  --watch                  Watch target and image files, and automatically
                           reload files when changed
```

## Commands

### General

* `m`: Toggle modes - `Playback` and `Stepper`
* `r`: Reload sprite information and sprite sheet
* `escape`: Quit program

### Movement

* `j`/`down arrow`: Move sprite down
* `k`/`up arrow`: Move sprite up
* `h`/`left arrow`: Move sprite left
* `l`/`right arrow`: Move sprite right
* `mouse click` (and drag): Sprite follows mouse cursor 
* `c`: Center sprite

### Keyframe

* `n`: Next keyframe animation
* `p`: Previous keyframe animation

### Speed and Frame position

* `f`: (`Playback` Mode) Increase sprite animation speed. (`Stepper` Mode) Go to next frame.
* `d`: (`Playback` Mode) Decrease sprite animation speed. (`Stepper` Mode) Go to previous frame.
* `a`: Reset animation speed to 1x

### Scaling

* `s`/`mouse wheel click`: Reset sprite scale to 1x
* `+`/`scroll in`: Scale up sprite
* `-`/`scroll out`: Scale down sprite

### Visibility

* `t`: Iterate colors of the offset's crosshair (Red, Green, Blue, Cyan, Magenta, Yellow, None)  
* `b`: Iterate colors of the background (Gray checkered, Black checkered, White checkered)
* `o`: Iterate colors of the sprite's outline (Red, Green, Blue, Cyan, Magenta, Yellow, None)
* `i`: Toggle showing or hiding animation information


## Build

### OSX build

```
brew install sdl2
brew install sdl2_ttf
brew install sdl2_image
brew install sdl2_gfx
stack build
```

### Ubuntu build

```
sudo apt install libsdl2-dev libsdl2-ttf-dev libsdl2-image-dev libsdl2-gfx-dev
stack build
```

### Windows build

```
stack exec -- pacman -S mingw64/mingw-w64-x86_64-pkg-config mingw64/mingw-w64-x86_64-SDL2 mingw64/mingw-w64-x86_64-SDL2_ttf mingw64/mingw-w64-x86_64-SDL2_image mingw64/mingw-w64-x86_64-SDL2_gfx
stack build
```