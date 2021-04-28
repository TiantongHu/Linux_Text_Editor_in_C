The project is a text editor in linux env, similar to vim in some aspects but probably has more similarities to other every-day editors. 

Please use 'make' to build it and use './LiTE <filename>' to run it. 

Its main features, other than viewing and editing files, include highlighting keywords in C files, and two-way incremental search.
There will be more filetypes that will have highlight features in upcoming versions, .py and .pas may be the first two.

This project does not use SDL but directly sends VT100 Escaping Sequences, exactly like what the ncurses lib does, this keeps the program lightweight and low.
Though, maybe it's really a good idea to rewrite the project with ncurses.

Document about VT100: https://en.wikipedia.org/wiki/VT100 
                      https://vt100.net/docs/vt100-ug/chapter3.html
Document about ANSI : https://en.wikipedia.org/wiki/ANSI_escape_code