# Animate Preview

## OSX build

```
brew install sdl2
brew install sdl2_ttf
brew install sdl2_image
brew install sdl2_gfx
stack build
```

## Ubuntu build

```
sudo apt install libsdl2-dev libsdl2-ttf-dev libsdl2-image-dev libsdl2-gfx-dev
stack build
```

## Windows build

```
stack exec -- pacman -S mingw64/mingw-w64-x86_64-pkg-config mingw64/mingw-w64-x86_64-SDL2 mingw64/mingw-w64-x86_64-SDL2_ttf mingw64/mingw-w64-x86_64-SDL2_image mingw64/mingw-w64-x86_64-SDL2_gfx
stack build
```