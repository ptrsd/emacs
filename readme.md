# my emacs config

Cheatsheet

![Emacs + Tmux](/emacs+tmux.png?raw=true "Emacs + Tmux")

## evil
Key | Desc
--- | ---
`fd` | escape vi mode
`i` | insert text before cursor
`A` | append text at the end of the line
`o` | begin a new line below the cursor and insert text
`x` | delete character after the cursor
`dd` | delete line
`D` | delete the characters under the cursor until the end of the line
`:%s/old/new/g` | replace old with new in all lines
`u` | undo
`e` | forward to the end of word
`b` | backward to the end of word
`daw` | delete a word
`/pattern` | search

## emacs
Key | Desc
--- | ---
`C-x c` | exit
`C-g` | abort
`M-x` | command
`C-x f` | find a file
`C-x s` | save all file
`C-x 3` | split vertically
`C-x 2` | split horizontally
`C-x 0` | close buffer
`C-x o` | switch buffer
`C-x 1` | close all other buffers
`C-x b` | select another buffer
`C-x k` | kill a buffer
`C-x left` | previous buffer
`C-x right` | next buffer 

## projectile
Key | Desc
--- | --- 
`M-f` | search for a file in a project
`M-F` | grep in a project

## neotree
Key | Desc
--- | ---
`C-c n` | toggle neotree
`A` | max/min neotree
`H` | toggle display hidden files
`C-c C-n` | create a file or directory
`C-c C-d` | delete a file or directory
`C-c C-r` | rename a file or directory
`g` | refresh

## elscreen
Key | Desc
--- | ---
`C-z c` | create screen
`C-z k` | kill screen
`gt` | next screen
`gT` | previous screen

## ensime
Key | Desc
--- | ---
`M-RET` | jump to the definition
`M-,` | pop back to the previous position
`C-c C-v r` | list all references to the symbol
`C-c C-v t` | show the type of the symbol
`C-c C-r a` | add type to the symbol
`C-c C-r o` | organize imports in the current file
`C-c C-r t` | import the type
`C-c C-r r` | rename the symbol
`TAB` | autocomplete
