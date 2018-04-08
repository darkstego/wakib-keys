# Wakib

Emacs minor mode that provides a modern, efficient and easy
to learn keybindings

## Features

### Proper Prefix Key Remapping

Using Ctrl+C and Ctrl+X as copy and paste respectively has always been tricky
in Emacs. CUA mode is fidly and most attempts to replace those keys end up breaking
down certain scenarios.

This mode converts those keys to copy/paste while providing new remapping keys
that all behave properly.

* C-c and C-x now implement proper Copy/Paste. Not Emacs CUA.
* New prefixes C-d and C-e work in all contexts.
* Pressing C-d shows up as C-d in the minibuffer
* These prefixes only act as the old key to start the prefix mode. So the vanilla
Emacs C-c C-c press becomes C-d C-c in this mode.


### Easy to learn

This mode makes it easy to pick up Emacs and start unlocking its potential without having
to sacrifice its power. The point of this mode it to leverage common shortcuts that
you are used to while making it easy to learn Emacs.

One of the ways this is done is by making individual shortcuts more powerful. So they give
access to many features without having to memorize individual keys for each.
Some keys perform non-repeatable tasks that can be utilized to do more. An example
is the Ctrl+A key that is typically mapped to Select All. Selecting they entire buffer is
not something that occurs often enough in emacs to merit occupying an entire shortcut, also
it is non-repeatable. In this mode Ctrl+A becomes select region, so on first press it selects
current line, on next press it selects current block(Paragraph). Press again and you get the Select
All. That makes it easy to memorize shortcuts as similar behaviour are grouped together.

## Bindings

The tables below show the bindings
This is just the start, I hope to expand on it very soon
(I explicitly mention the shift key so don't get thrown off by letter capitalization)

### Movement

| Key            | Binding                            |
| ---            | ---                                |
| Alt + I/J/K/L  | Inverse T movement by Char         |
| Alt+Shift+ I/K | Page Up/Down                       |
| Alt+ U/O       | Back/Forward Word                  |
| Alt+Shift+ U/O | Beginning/End of Line or Paragraph |

### Editing

| Key         | Binding                     |
| ---         | ---                         |
| Alt + E/R   | Delete Word Back/Forward    |
| Alt+ D/F    | Delete Char Back/Forward    |
| Alt + Space | Set/Stop Mark for Selection |


### CUA

| Key      | Binding               |
| ---      | ---                   |
| Ctrl + O | Open File             |
| Ctrl + P | Print                 |
| Ctrl + F | Search                |
| Ctrl + W | Close Buffer          |
| Ctrl + S | Save                  |
| Ctrl + Z | Undo                  |
| Ctrl + X | Cut                   |
| Ctrl + C | Copy                  |
| Ctrl + V | Paste                 |
| Ctrl + A | Select Line/Block/All |


### UI

| Key             | Binding            |
| ---             | ---                |
| Ctrl + =        | Increase Font Size |
| Ctrl + -        | Decrease Font Size |
| Alt + 4         | Split Window Right |
| Alt + Shift + 4 | Split Window Below |
| Alt + S         | Switch Window      |
| Ctrl + B        | Swith to Buffer    |
| Alt + 2         | Close Pane         |
| Alt + 3         | Close Other Panes  |

### Emacs Keys

Yes, those have finally moved

| Old Key                          | New Key  |
| ---                              | ---      |
| Ctrl + C (prefix only)           | Ctrl + D |
| Ctrl + X (prefix only)           | Ctrl + E |
| Alt + X  (not overwritten.. yet) | Alt + A  |



## Installation

Save the *wakib-mode.el* file anywhere in your emacs loadpath
then place

```
(require 'wakib-mode)
(wakib-mode 1)
```
in your init.el file



## Thanks

This minor mode is heavily influenced by Ergoemacs mode. So movement is by
using Alt + ijkl keys. I hope to finish up the keybindings soon. This is
modifier based mode. If you are looking for a modal keybinding in the same
vein, check out xah-fly-keys.

Also props to the general.el project for providing the solution to
dynamically bind prefix keys. If you simply want to copy the C-c key
to another location, while keeping C-c as is, check them out, they provide a
an easy way of doing it. Or just check out the Macro shown in my mode.

I am very very new to elisp, and emacs so any suggestions or advice is
greatly appreciated.
