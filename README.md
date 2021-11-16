# Wakib-keys

Emacs minor mode that provides a modern, efficient and easy
to learn keybindings. For the full starter kit that is based
on the concepts found here, please check out [wakib-emacs](https://github.com/darkstego/wakib-emacs).

## Changelog

* Move to beginning of line now stops at first Non-Whitespace, press again for beginning of line

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

The following images show the general ideas behind the bindings. Please note that some of the bindings only exist through seperate packages available through the starter kit.

CTRL
-----
![CTRL-KEYS](https://s15.postimg.cc/9bmeocmqz/Keyboard_CTRL_Layout.png "wakib-keys ctrl bindings")

ALT
---
![ALT-KEYS](https://i.postimg.cc/Fz0qq6DQ/Keyboard-ALT-Layout3.png "wakib-keys alt bindings")



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

| Key              | Binding               |
| ---              | ---                   |
| Ctrl + O         | Open File             |
| Ctrl + P         | Print                 |
| Ctrl + F         | Search                |
| Ctrl + Shift + F | Search Backwards      |
| Ctrl + W         | Close Buffer          |
| Ctrl + S         | Save                  |
| Ctrl + Z         | Undo                  |
| Ctrl + X         | Cut                   |
| Ctrl + C         | Copy                  |
| Ctrl + V         | Paste                 |
| Ctrl + A         | Select Line/Block/All |
| Ctrl + Q         | Quit                  |



### UI

| Key             | Binding            |
| ---             | ---                |
| Ctrl + =        | Increase Font Size |
| Ctrl + -        | Decrease Font Size |
| Alt + 4         | Split Window Right |
| Alt + Shift + 4 | Split Window Below |
| Alt + S         | Switch Window      |
| Ctrl + B        | Swith to Buffer    |
| Alt + Shift + 3 | Close Pane         |
| Alt + 3         | Close Other Panes  |

### Emacs Keys

Yes, those have finally moved

| Old Key                          | New Key  |
| ---                              | ---      |
| Ctrl + C (prefix only)           | Ctrl + D |
| Ctrl + X (prefix only)           | Ctrl + E |



## Installation


You can install wakib-keys from melpa or save the *wakib-keys.el* file
anywhere in your emacs loadpath then place

```
(require 'wakib-keys)
(wakib-keys 1)
```
in your init.el file

## Removing Unwanted Shortcuts

If there is a shortcut you would want to disable it can be achieved by removing it from the `wakib-keys-overriding-map`. 

E.g. if you want to remove the "C-q" binding just add the following to your init.el after requiring wakib-keys. Replace "C-q" with whatever key you wish to remove.

```
(define-key wakib-keys-overriding-map (kbd "C-q") nil)
```

## Contribution

If you have any issues, suggestions then please post them on the
project's github page. I would like to hear what is working and what
isn't with these keybindings.

## Other keybingings

Other projects that provide different keybindings for Emacs include

Modifier based: [Ergoemacs](https://ergoemacs.github.io/)
Modal: [Xah-Fly-Keys](https://github.com/xahlee/xah-fly-keys)
