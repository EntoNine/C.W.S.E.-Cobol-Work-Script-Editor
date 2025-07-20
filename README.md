![FAWC Logo](logo.png)

# Cobol Work Script Editor (CWSE)

A simple ncurses-based editor for COBOL scripts.

## Preview

![Program Preview](preview.png)

## Terminal Compatibility

**Note:** CWSE may be unstable or have display issues with certain terminals, such as `gnome-terminal`.  
For best results, use [Sakura terminal](https://github.com/maehne/sakura) which has been tested and works reliably.

## Features

- Line-numbered editing for COBOL scripts
- Special comment column support
- Insert COBOL structure template
- Save, rename, and delete lines
- Keyboard navigation and shortcuts
- Visual highlighting for comments and code

## Installation

1. Ensure you have `gcc` and `libncurses` installed.
2. Clone this repository:
   ```sh
   git clone https://github.com/yourusername/FAWC.git
   cd FAWC
   ```
3. Build the program:
   ```sh
   gcc app.c -o cwse -lncurses
   ```

## Usage

Run the editor with an optional filename:
```sh
./cwse [filename]
```
If the file does not exist, it will be created.

## Keyboard Shortcuts

| Key         | Action                        |
|-------------|------------------------------|
| F2          | Save file                    |
| F3          | Rename file                  |
| F8          | Insert COBOL structure       |
| ESC         | Quit                         |
| ↑/↓         | Move cursor up/down          |
| ←/→         | Move cursor left/right       |
| Enter       | Insert new line              |
| Backspace   | Delete character/line        |
| Delete      | Delete character/line        |

## Contributing

Pull requests and suggestions are welcome! Please open an issue for bugs or feature requests.

## License

This project is licensed under the MIT License.

## Credits

Developed by Ento9.  
