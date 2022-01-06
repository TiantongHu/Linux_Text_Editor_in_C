/*** A text editor in linux ***/
/* use Make to build, ./LiTE <filename> to run it */

/* 
TODO: 
	1. Add moe filetypes
	2. Single & Multiline Copy / Paste Feature (Mouse Control?)
	3. Actually, consider use ncurses??
	4. Open TWO FILES (More Buffer?)
*/


/*** includes ***/

#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE
// define Feature Test Macros to help include parts that suits more env

#include <ctype.h>
#include <errno.h>
#include <fcntl.h> // for file control
#include <stdio.h>
#include <stdarg.h> // for variadic func
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h> // for getting windows size
#include <sys/types.h> // similar to ctype
#include <termios.h> // not available in windows, to control inputs and outputs in terminal, including enter raw mode 
#include <time.h>
#include <unistd.h> // unix standard, to access POSIX API - for file modification


/*** defines ***/

#define EDITOR_VERSION "3.0.2"
#define TAB_STOP 4 // tab length
#define QUIT_CONFIRM_TIMES 2 // confirmation needs to quit when unsaved

#define CTRL_KEY(k) ((k) & 0x1f)

enum editorKey{
	BACKSPACE = 127, // ASCII value of delete, for some reason not backspace
	ARROW_LEFT = 1000, // multi byte keys
	ARROW_RIGHT,
	ARROW_UP,
	ARROW_DOWN,
	DEL_KEY,
	HOME_KEY,
	END_KEY,
	PAGE_UP,
	PAGE_DOWN
}; // special keys, ignoring a lot other keys such as Fn

enum editorHighlight{
	HL_NORMAL = 0,
	HL_COMMENT,
	HL_MLCOMMENT,
	HL_KEYWORD1, // allow two types of keywords being defined, can totally null one
	HL_KEYWORD2,
	HL_STRING,
	HL_NUMBER,
	HL_MATCH // for search
}; // different highlights

#define HL_HIGHLIGHT_NUMBERS (1<<0) // flags to highlight 
#define HL_HIGHLIGHT_STRINGS (1<<1)

/*** data ***/

struct editorSyntax {
	char *filetype; // displayed
	char **filematch; // array of strs, containing filetype patterns to compare
	char **keywords; // array of strs, keywords
	char *singleline_comment_start; // two types of comments
	char *multiline_comment_start;
	char *multiline_comment_end;
	int flags; // a field for bit wise flags
}; // file opened

typedef struct erow{
	int idx; // the row's index
	int size; // actual size
	int rsize; // render size
	char *chars; // actual char input
	char *render; // what to display
	unsigned char *hl; // highlight, same len as render
	int hl_open_comment; // if end with an open multiline comment
} erow; // to store a row of text on screen

struct editorConfig{
	int cx, cy; // char pos
	int rx; // render x - deal for tabs
	int rowoff; // offsets, to track where the screen is scrolled to
	int coloff;
	int screenrows; // screen size
	int screencols;
	int numrows;
	erow *row; // array of raws
	int dirty; // whether modified, I always add 1 when changed, set to 1 is better, +1 can overload(unlikely)
	char *filename;
	char statusmsg[80];
	time_t statusmsg_time;
	struct editorSyntax *syntax; // for file syntax highlight
	struct termios orig_termios; // original terminal attributes 
};

struct editorConfig E; // editor config the program uses


/*** filetypes ***/

char *C_HL_extensions[] = {".c", ".h", ".cpp", NULL};
char *PASCAL_HL_extensions[] = {".pas", ".pp", ".inc", NULL};

char *C_HL_keywords[] = {
		"#include", "switch", "if", "while", "for", "break", "continue", "return", "else",
		"struct", "union", "typedef", "static", "enum", "class", "case", "#define",

		// second type terminates with '|', can have no second type
		"int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
		"void|", NULL
};
char *PASCAL_HL_keywords[] = {
		"PROGRAM", "DIV", "WRITE", "WRITELN", "PROCEDURE", "IF", "ELSE", "THEN", "BEGIN", "END",
		"ID", "INTEGER_CONST", "REAL_CONST", "EOF",

		"REAL|", "INTEGER|", "VAR|", "TRUE|", "FALSE|", "BOOLEAN|", "STRING|", NULL
};

struct editorSyntax HLDB[] = {
	{
		"c", // filetype
		C_HL_extensions, // filematch
		C_HL_keywords, // keywords
		"//", "/*", "*/", // comments
		HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS // flags
	}, // can add more like .py
	{
		"pascal", // filetype
		PASCAL_HL_extensions, // filematch
		PASCAL_HL_keywords, // keywords
		"{", "}", // comments
		HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
	},
}; // highlight database

#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0])) // len of HLDB


/*** function defines ***/

void editorSetStatusMessage(const char *fmt, ...);
void editorRefreshScreen();
char *editorPrompt(char *prompt, void(*callback)(char *, int));


/*** terminal ***/

void die(const char *s){
	/* to print out error message and kills the program */

	// clear screen and place cursor back
	write(STDOUT_FILENO, "\x1b[2J", 4);
	write(STDOUT_FILENO, "\x1b[H", 3);

	perror(s);
	exit(1);
}

void disableRawMode(){
	/* restore original attributes, discard unread inputs */
	if(tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1)
		die("tcsetattr"); 
}

void enableRawMode() {
	/* enter raw mode to avoid using enter to pass key presses 
	   pass the input byte by byte - non-canonical mode like vim
	   done this by modifying terminal's attributes (flags) 
	   special signals from ctrl+char are also turned off */

	if(tcgetattr(STDIN_FILENO, &E.orig_termios) == -1) die("tcgetattr"); // get original
	atexit(disableRawMode); // exit raw mode at exit

	struct termios raw = E.orig_termios;
	// input
	raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
	// output
	raw.c_oflag &= ~(OPOST); // note this turns off the auto translation of /n to /r/n
	// control
	raw.c_cflag |= (CS8); // old tradition to set char to 8bits per byte, if it's not already
	// others 
	raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
	// control chars
	raw.c_cc[VMIN] = 0; // min bytes need before return
	raw.c_cc[VTIME] = 1; // max time for write to wait before return, 0.1s

	if(tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) die("tcsetattr"); // set raw
}

int editorReadKey(){
	/* wait for keypress and return, handles escape sequences 
	   returns an int to adapt enum keys that were not chars */
	int nread;
	char c;
	while ((nread = read(STDIN_FILENO, &c, 1)) != 1){
		// Cygwin returns -1 not 0 when read() times out, so EAGAIN cannot be treat as error
		if(nread == -1 && errno != EAGAIN) die("read");
	}

	// deal with escape sequences 
	if(c == '\x1b'){
		char seq[3];

		if(read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
		if(read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';

		if(seq[0] == '['){
			if(seq[1] >= '0' && seq[1] <= '9'){
				if(read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
				if(seq[2] == '~'){
					switch (seq[1]){
						case '1': return HOME_KEY; // Home & Del have different reps on diff os, so multiples here
						case '3': return DEL_KEY;
						case '4': return END_KEY;
						case '5': return PAGE_UP;
						case '6': return PAGE_DOWN;
						case '7': return HOME_KEY;
						case '8': return END_KEY;
					}
				}
			} else{
				switch (seq[1]){
					case 'A': return ARROW_UP; // '\x1b[A'
					case 'B': return ARROW_DOWN;
					case 'C': return ARROW_RIGHT;
					case 'D': return ARROW_LEFT;
					case 'H': return HOME_KEY;
					case 'F': return END_KEY;
				}
			}
		} else if (seq[0] == '0'){
			switch (seq[1]){
				case 'H': return HOME_KEY;
				case 'F': return END_KEY;
			}
		}

		return '\x1b'; // esc key
	} else{
		return c; // not a special char, return it as what it is
	}
}

int getCursorPosition(int *rows, int *cols){
	char buf[32];
	unsigned int i = 0;

	// ask for the position using escape sequence
	if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4) return -1;

	// example reply in standard input:  \x1b[26;60R
	while(i < sizeof(buf) - 1){
		if(read(STDIN_FILENO, &buf[i], 1) != 1) break;
		if(buf[i] == 'R') break; // message ends
		i++;
	}
	buf[i] = '\0'; // change it into a proper str

	if(buf[0] != '\x1b' || buf[1] != '[') return -1;
	// pass the str of row : col
	if(sscanf(&buf[2], "%d;%d", rows, cols) != 2) return -1;

	return 0;
}

int getWindowSize(int *rows, int *cols){
	struct winsize ws;

	if(ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0){ 
		// if failed in getting windows size, move cursor to bottom right and read position
		if(write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) return -1;
		return getCursorPosition(rows, cols);
	}else{
		*cols = ws.ws_col;
		*rows = ws.ws_row;
		return 0;
	}
}


/*** syntax highlighting ***/

int is_separator(int c){
	/* return true if the char is considered as a separator */
	return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
}

void editorUpdateSyntax(erow *row){
	/* set the hl attribute for the row */
	row->hl = realloc(row->hl, row->rsize);
	memset(row->hl, HL_NORMAL, row->rsize); // set to default at first

	if(E.syntax == NULL) return; // no highlighting if the filetype is not recognizable 

	char **keywords = E.syntax->keywords;

	// comment alias
	char *scs = E.syntax->singleline_comment_start;
	char *mcs = E.syntax->multiline_comment_start;
	char *mce = E.syntax->multiline_comment_end;

	int scs_len = scs ? strlen(scs) : 0; 
	int mcs_len = mcs ? strlen(mcs) : 0;
	int mce_len = mce ? strlen(mce) : 0;

	int prev_sep = 1; // keeps track whether prev char was a separator
	int in_string = 0; // keeps track of whether in a string
	int in_comment = (row->idx > 0 && E.row[row->idx - 1].hl_open_comment); // if in multiline comments

	int i = 0;
	while(i < row->rsize){ // loop through chars
		char c = row->render[i];
		unsigned char prev_hl = (i > 0) ? row->hl[i-1] : HL_NORMAL; // record prev hl

		// single line comment
		if(scs_len && !in_string && !in_comment){ // check if scs is defined and not in other situations
			if(!strncmp(&row->render[i], scs, scs_len)){ //equal = 0 so !
				memset(&row->hl[i], HL_COMMENT, row->rsize - i); // set the entire line
				break; // the line is done, break
			}
		}


		//multi line comment
		if(mcs_len && mce_len && !in_string){ // if not in string & multiline comment defined properly
			if(in_comment){
				row->hl[i] = HL_MLCOMMENT;
				if(!strncmp(&row->render[i], mce, mce_len)){ // reach the end
					memset(&row->hl[i], HL_MLCOMMENT, mce_len);
					i += mce_len;
					in_comment = 0;
					prev_sep = 1; // multiline comments are considered separators
					continue;
				} else{
					i++;
					continue;
				}
			} else if(!strncmp(&row->render[i], mcs, mcs_len)){ // at the beginning
				memset(&row->hl[i], HL_MLCOMMENT, mcs_len);
				i += mcs_len;
				in_comment = 1;
				continue;
			}
		}

		// string
		if(E.syntax->flags & HL_HIGHLIGHT_STRINGS){ // check flag
			if(in_string){ // if in a string
				row->hl[i] = HL_STRING;
				// consider escape sequence, don't care what's after
				if(c == '\\' && i + 1 < row->rsize){ 
					row->hl[i+1] = HL_STRING;
					i += 2;
					continue;
				}
				if(c == in_string) in_string = 0; // string ends here
				i++;
				prev_sep = 1; // closing quote is a separator
				continue;
			} else{
				if(c == '"' || c == '\''){
					in_string = c; // set in_string to quotation marks, used for checking ending
					row->hl[i] = HL_STRING;
					i++;
					continue;
				}
			}
		}

		// nums
		if(E.syntax->flags & HL_HIGHLIGHT_NUMBERS){ // checking flag
			if((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) ||
				(c == '.' && prev_hl == HL_NUMBER)){ // avoid var names or int32 etc.
				row->hl[i] = HL_NUMBER;
				i++;
				prev_sep = 0; // not a separator
				continue;
			}
		}

		// keywords
		if(prev_sep){ // after checking all others, check keywords
			int j;
			for(j = 0; keywords[j]; j++){ // loop through the array
				int klen = strlen(keywords[j]);
				int kw2 = keywords[j][klen - 1] == '|';
				if(kw2) klen--; // remove '|'

				if(!strncmp(&row->render[i], keywords[j], klen) &&
						is_separator(row->render[i + klen])){ // after the keyword there must be a separator
					memset(&row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
					i += klen;
					break; // out for
				}
			}
			if(keywords[j] != NULL){ // if broken out, not looped out
				prev_sep = 0; // prev is end of the keyword
				continue;
			}
		}

		prev_sep = is_separator(c); // check whtether is a separator
		i++;
	}

	int changed = (row->hl_open_comment != in_comment);
	row->hl_open_comment = in_comment; // set it to true if the multiline comment is not ended
	if(changed && row->idx + 1 < E.numrows) // update next line's syntax if changed
		editorUpdateSyntax(&E.row[row->idx + 1]);
}

int editorSyntaxToColor(int hl){
	/* maps highlight value to actual ANSI color codes */
	switch(hl){
		case HL_COMMENT:
		case HL_MLCOMMENT:return 36; // cyan
		case HL_KEYWORD1: return 33; // yellow
		case HL_KEYWORD2: return 32; // green
		case HL_STRING:   return 35; // magenta
		case HL_NUMBER:   return 31; // red
		case HL_MATCH:    return 34; // blue
		default: 		  return 37; // white
	}
}

void editorSelectSyntaxHighlight(){
	/* determine filetype and do highlight */
	E.syntax = NULL; // reset syntax
	if(E.filename == NULL) return; // no file name = no file type

	char *ext = strrchr(E.filename, '.'); // pointer to last '.'

	// loop through HLDB to match file type
	for(unsigned int j = 0; j < HLDB_ENTRIES; j++){
		struct editorSyntax *s = &HLDB[j]; // struct a new syntax for each file type
		unsigned int i = 0;
		while (s->filematch[i]){ // loop through the filematch to check file type
			int is_ext = (s->filematch[i][0] == '.'); // validation
			if((is_ext && ext && !strcmp(ext, s->filematch[i])) || // use ! as it returns 0 when two str match
			    	(!is_ext && strstr(E.filename, s->filematch[i]))){ // or try to match file name directly for certian file hl
				E.syntax = s; // assign file syntax

				// highlight each row after syntax check
				int filerow;
				for(filerow = 0; filerow < E.numrows; filerow++){
					editorUpdateSyntax(&E.row[filerow]);
				}

				return;
			}
			i++;
		}
	}
}


/*** row operations ***/

int editorRowCxToRx(erow *row, int cx){
	 /* calc render index value from char index */
	int rx = 0;
	int j;
	for(j = 0; j < cx; j++){
		if(row->chars[j] == '\t')
			rx += (TAB_STOP - 1) - (rx % TAB_STOP);
		rx++;
	}
	return rx;
}

int editorRowRxToCx(erow *row, int rx){
	/* turns render index to char index */
	int cur_rx= 0;
	int cx;
	for(cx = 0; cx < row->size; cx++){
		if(row->chars[cx] == '\t')
			cur_rx += (TAB_STOP - 1) - (cur_rx % TAB_STOP);
		cur_rx++;

		if(cur_rx > rx) return cx; // cx has incremented again after ==, pointing to right pos
	}
	return cx; // in case rx given is invalid (out of range)
}

void editorUpdateRow(erow *row){
	/* updates a row after change */
	int tabs = 0;
	int j;

	// count tabs to know how much mem is needed
	for(j = 0; j < row->size; j++)
		if(row->chars[j] == '\t') tabs++;

	free(row->render);
	row->render = malloc(row->size + tabs*(TAB_STOP - 1) + 1);

	int idx = 0; // index
	for(j = 0; j< row->size; j++){
		if(row->chars[j] == '\t'){
			row->render[idx++] = ' ';
			// advance at least one col and reach a multiple of 4
			while(idx % TAB_STOP != 0) row->render[idx++] = ' ';
		} else{
			row->render[idx++] = row->chars[j];
		}
	}
	row->render[idx] = '\0';
	row->rsize = idx;

	editorUpdateSyntax(row);
}

void editorInsertRow(int at, char *s, size_t len){
	if(at < 0 || at > E.numrows) return; // at invalid line

	E.row = realloc(E.row, sizeof(erow) * (E.numrows + 1)); // ask for enough mem
	memmove(&E.row[at + 1], &E.row[at], sizeof(erow) * (E.numrows - at)); // move the rest to next row
	for(int j = at + 1; j <= E.numrows; j++) E.row[j].idx++; // increment index

	E.row[at].idx = at; // set index

	E.row[at].size = len;
	E.row[at].chars = malloc(len + 1); // allocate memory for the row
	memcpy(E.row[at].chars, s, len); // move the chars to the row
	E.row[at].chars[len] = '\0'; // proper string ending

	E.row[at].rsize = 0;
	E.row[at].render = NULL;
	E.row[at].hl = NULL;
	E.row[at].hl_open_comment = 0; // set to 0 for new row
	editorUpdateRow(&E.row[at]);

	E.numrows++;
	E.dirty++;
}

void editorFreeRow(erow *row){
	free(row->render);
	free(row->chars);
	free(row->hl);
}

void editorDelRow(int at){
	/* delete row and modify the pointer storing rows */
	if(at < 0 || at >= E.numrows) return;
	editorFreeRow(&E.row[at]);
	memmove(&E.row[at], &E.row[at + 1], sizeof(erow) * (E.numrows - at - 1));
	for (int j = at; j < E.numrows - 1; j++) E.row[j].idx--; // decrement index
	E.numrows--;
	E.dirty++;
}

void editorRowInsertChar(erow *row, int at, int c){
	if(at < 0 || at > row->size) at = row->size; // validation
	row->chars = realloc(row->chars, row->size + 2); // need one more char space for null

	// use memmove since the arrays overlap
	memmove(&row->chars[at+1], &row->chars[at], row->size - at + 1);
	row->size++;
	row->chars[at] = c;
	editorUpdateRow(row);
	E.dirty++;
}

void editorRowAppendString(erow *row, char *s, size_t len){
	row->chars = realloc(row->chars, row->size + len + 1); // including null
	memcpy(&row->chars[row->size], s, len);
	row->size += len;
	row->chars[row->size] = '\0';
	editorUpdateRow(row);
	E.dirty++;
}

void editorRowDelChar(erow *row, int at){
	if (at < 0 || at >= row->size) return;
	// deleting
	memmove(&row->chars[at], &row->chars[at+1], row->size - at);
	row->size--;
	editorUpdateRow(row);
	E.dirty++;
}


/*** editor operations ***/

void editorInsertChar(int c){
	/* takes care of cursor position */
	if(E.cy == E.numrows){ // insert new row at the end
		editorInsertRow(E.numrows, "", 0);
	}
	// erow modification
	editorRowInsertChar(&E.row[E.cy], E.cx, c);
	E.cx++;
}

void editorInsertNewLine(){
	/* handles enter key */
	if(E.cx == 0){
		editorInsertRow(E.cy, "", 0);
	} else{
		erow *row = &E.row[E.cy];
		// move the chars after cx to next row
		editorInsertRow(E.cy + 1, &row->chars[E.cx], row->size - E.cx);
		row = &E.row[E.cy]; // reassign as editorInsertRow() calls realloc() which may change mem location
		row->size = E.cx;
		row->chars[row->size] = '\0';
		editorUpdateRow(row);
	}
	E.cy++;
	E.cx = 0;
}

void editorDelChar(){
	// only delete when possible
	if(E.cx == 0 && E.cy == 0) return; // at the beginning
	if(E.cy == E.numrows) return; // passed the end of the file
	
	

	erow *row = &E.row[E.cy];
	if(E.cx > 0){
		editorRowDelChar(row, E.cx - 1);
		E.cx--;
	} else{
		// at the beginning of an empty line
		// append the content of the line to the previous line
		E.cx = E.row[E.cy - 1].size;
		editorRowAppendString(&E.row[E.cy - 1], row->chars, row->size);
		editorDelRow(E.cy);
		E.cy--;
	}
}


/*** file i/0 ***/

char *editorRowsToString(int *buflen){
	/* turns the erows into a single string for saving */

	// calculate for total len
	int totlen = 0;
	int j;
	for(j = 0; j < E.numrows; j++)
		totlen += E.row[j].size + 1; // at one for newline char
	*buflen = totlen;

	char *buf = malloc(totlen);
	char *p = buf;
	for(j = 0; j < E.numrows; j++){
		memcpy(p, E.row[j].chars, E.row[j].size);
		p += E.row[j].size; // point to the end
		*p = '\n'; // add newline char
		p++; // for next row
	}

	return buf;
}

void editorOpen(char *filename){
	/* open a file */
	free(E.filename);
	E.filename = strdup(filename); // saves filename for status bar

	editorSelectSyntaxHighlight(); // check if hl needed

	FILE *fp = fopen(filename, "r");
	if(!fp) die("fopen");

	char *line = NULL;
	size_t linecap = 0;
	ssize_t linelen; // can be -1 to indicate end

	while((linelen = getline(&line, &linecap, fp)) != -1){
		while (linelen > 0 && (line[linelen - 1] == '\n' ||
							   line[linelen - 1] == '\r'))
			linelen--; // remove line breaks
		editorInsertRow(E.numrows, line, linelen);
	}
	free(line);
	fclose(fp);
	E.dirty = 0;
}

void editorSave(){
	/* save the file */
	// if the file is new, ask for file name
	if(E.filename == NULL) {
		E.filename = editorPrompt("Save as: %s (ESC to cancel)", NULL);
		if(E.filename == NULL){
			editorSetStatusMessage("Save aborted");
			return;
		}
		editorSelectSyntaxHighlight(); // re-check whether type changes and hl changes
	}

	int len;
	char *buf = editorRowsToString(&len);

	// this will either open the file for read/write, or create new one
	// 0644 makes the owner to read/write, others can only read
	int fd = open(E.filename, O_RDWR |O_CREAT, 0644);
	if(fd != -1){
		// here ftruncate is used as it will not directly erase all data
		// can change to using O_TRUNC flag in open(), but it erases all data, 
		// and if write fails we will lose all data we previously have
		if(ftruncate(fd, len) != -1){ // set file size to len
			if(write(fd, buf, len) == len){
				close(fd);
				free(buf);
				E.dirty = 0; // reset
				editorSetStatusMessage("%d bytes written to disk", len);
				return;
			}
		}
		close(fd);
	}

	// if error
	free(buf);
	editorSetStatusMessage("Can't save! I/O error: %s", strerror(errno));
}


/*** find ***/

void editorFindCallback(char *query, int key){
	/* two direction incremental search */
	static int last_match = -1; // row the last match was on
	static int direction = 1; // 1 for forward, -1 for backward search

	static int saved_hl_line; // save searched string's highlighting, row num
	static char *saved_hl = NULL; // original color

	if(saved_hl){ // restore original highlighting 
		memcpy(E.row[saved_hl_line].hl, saved_hl, E.row[saved_hl_line].rsize);
		free(saved_hl);
		saved_hl = NULL;
	}

	if(key == '\r' || key == '\x1b'){
		// restore static var and end search
		last_match = -1;
		direction = 1;
		return;

	//set direction
	} else if(key == ARROW_RIGHT || key == ARROW_DOWN){
		direction = 1;
	} else if(key == ARROW_LEFT || key == ARROW_UP){
		direction = -1;
	} else{ // default setting when no input
		last_match = -1;
		direction = 1;
	}

	if(last_match == -1) direction = 1; // avoid error
	int current = last_match;
	int i;
	for(i = 0; i < E.numrows; i++){ // loop through all rows
		current += direction;
		if(current == -1) current = E.numrows - 1; // jump from beginning to end when -1 dir
		else if (current == E.numrows) current = 0; // jump from end to beginning when 1 dir

		erow *row = &E.row[current];
		char *match = strstr(row->render, query); // check whether a substring
		if(match){
			last_match = current; // start point for next search if arrow key pressed
			E.cy = current;
			E.cx = editorRowRxToCx(row, match - row->render); // real char index considering tab
			E.rowoff = E.numrows;

			saved_hl_line = current; // save highlighting row num
			saved_hl = malloc(row->rsize); // free will always happen after last enter/esc press
			memcpy(saved_hl, row->hl, row->rsize); // save original color
			memset(&row->hl[match - row->render], HL_MATCH, strlen(query)); // set color
			break;
		}
	}
}

void editorFind(){
	// save cursor position as these will be modified in incremental search process
	int saved_cx = E.cx;
	int saved_cy = E.cy;
	int saved_coloff = E.coloff;
	int saved_rowoff = E.rowoff;

	char *query = editorPrompt("Search: %s (Use ESC/Arrows/Enter)",
								editorFindCallback);

	if(query == NULL) {
		free(query); // no search done
	} else{ // restore saved data
		E.cx = saved_cx;
		E.cy = saved_cy;
		E.coloff = saved_coloff;
		E.rowoff = saved_rowoff;
	}
}


/*** append buffer ***/

struct abuf { 
	char *b;
	int len;
}; // dynamic string type
 
#define ABUF_INIT {NULL, 0} // empty buffer, acts as constructor 

void abAppend(struct abuf *ab, const char *s, int len){
	// allocate enough memory 
	char *new = realloc(ab->b, ab->len + len);

	if(new == NULL) return;
	// copy the new content to the end of new str
	memcpy(&new[ab->len], s, len);
	ab->b = new;
	ab->len += len;
}

void abFree(struct abuf *ab){
	free(ab->b);
}


/*** output ***/

void editorScroll(){
	/* adjust and update sreen position */
	// set rx to right value
	E.rx = 0;
	if (E.cy < E.numrows){
		E.rx = editorRowCxToRx(&E.row[E.cy], E.cx);
	}

	// adjust offsets based on whether cursor moved outside of the screen
	if(E.cy < E.rowoff){
		E.rowoff = E.cy;
	}
	if(E.cy >= E.rowoff + E.screenrows){
		E.rowoff = E.cy - E.screenrows +1;
	}
	if(E.rx < E.coloff){
		E.coloff = E.rx;
	}
	if(E.rx >= E.coloff + E.screencols){
		E.coloff = E.rx - E.screencols +1;
	}
}

void editorDrawRows(struct abuf *ab) {
	/* draw all rows, also draw '~' at the starting of each row */
	int y;
	for(y = 0; y < E.screenrows; y++){
		int filerow = y + E.rowoff; // position with offset
		if(filerow >= E.numrows){ // only draw additional things after text inputs
			// display welcome message when no text to show
			if(E.numrows == 0 && y == E.screenrows / 3){
				char welcome[80];
				int welcomelen = snprintf(welcome, sizeof(welcome),
						"Tiantong's Linux editor -- version %s", EDITOR_VERSION);
				if(welcomelen > E.screencols) welcomelen = E.screencols;
				// center the displayed message 
				int padding = (E.screencols - welcomelen) / 2;
				if(padding){
					abAppend(ab, "~", 1);
					padding--;
				}
				while(padding--) abAppend(ab, " ", 1);
				abAppend(ab, welcome, welcomelen);
			} else{
				abAppend(ab, "~", 1); // draw '~' at the start
			}
		} else{ // draw text input
			int len = E.row[filerow].rsize - E.coloff; // len of text to draw, considering offset
			if(len < 0) len = 0; // pass the end of the column
			if (len > E.screencols) len = E.screencols; // only draw till the end of screen
			char *c = &E.row[filerow].render[E.coloff]; // the char
			unsigned char *hl = &E.row[filerow].hl[E.coloff]; // the char's hl
			int current_color = -1; // keep track of currently color as most char will have same color as before
			int j;
			for(j = 0; j < len; j++){ // check color digit by digit
				if(iscntrl(c[j])){ // render control + char a representation, in inverted color
					char sym = (c[j] <= 26) ? '@' + c[j] : '?'; // '@' is used as ASCII value to map with capitals
					abAppend(ab, "\x1b[7m", 4);
					abAppend(ab, &sym, 1);
					abAppend(ab, "\x1b[m", 3);
					// since \x1b[m turns off all colors, we restore color if needed
					if(current_color != -1){
						char buf[16];
						int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", current_color);
						abAppend(ab, buf, clen);
					}
				}else if(hl[j] == HL_NORMAL){
					if(current_color != -1){
						abAppend(ab, "\x1b[39m", 5); // back to default color
						current_color = -1; // for default
					}
					abAppend(ab, &c[j], 1);
				} else{
					int color = editorSyntaxToColor(hl[j]); // check for highlight
					if(color != current_color){
						current_color = color;
						// write the escape sequence into buffer
						char buf[16];
						int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", color);
						abAppend(ab, buf, clen); // set color
					}
					abAppend(ab, &c[j], 1); // add char after properly setup color
				}
			}
			abAppend(ab, "\x1b[39m", 5); // back to default color
		}

		abAppend(ab, "\x1b[K", 3); // clear the row
		abAppend(ab, "\r\n", 2); // new row
	}
}

void editorDrawStatusBar(struct abuf *ab){
	/* draw the status bar */
	abAppend(ab, "\x1b[7m", 4); // invert color
	char status[80], rstatus[80];

	// status info
	int len = snprintf(status, sizeof(status), "%.20s - %d lines %s",
						E.filename ? E.filename : "[No Name]", E.numrows,
						E.dirty ? "(modified)" : "");
	int rlen = snprintf(rstatus, sizeof(rstatus), "%s | %d/%d",
						E.syntax ? E.syntax->filetype : "no ft",
						E.cy + 1, E.numrows);
	// not exceeding the screen
	if(len > E.screencols) len = E.screencols;
	abAppend(ab, status, len);
	while (len < E.screencols){
		// put rstatus at the right most pos
		if(E.screencols - len == rlen){
			abAppend(ab, rstatus, rlen);
			break;
		} else{
			abAppend(ab, " ", 1);
			len++;
		}
	}
	abAppend(ab, "\x1b[m", 3); // back to normal color
	abAppend(ab, "\r\n", 2); // status message space
}

void editorDrawMessageBar(struct abuf *ab){
	/* print the status message in the bar */
	abAppend(ab, "\x1b[k", 3); // clear the bar to ensure 
	int msglen = strlen(E.statusmsg);
	if(msglen > E.screencols) msglen = E.screencols; // no getting outside of the screen
	if(msglen && time(NULL) - E.statusmsg_time <5)
		abAppend(ab, E.statusmsg, msglen);
}

void editorRefreshScreen(){
	/* clean the screen and print content */

	editorScroll(); // adjust screen offsets

	struct abuf ab = ABUF_INIT; // construct new buf

	abAppend(&ab, "\x1b[?25l", 6); // hide the cursor before the refresh starts
	abAppend(&ab, "\x1b[H", 3); // reposition the cursor to top left

	editorDrawRows(&ab);
	editorDrawStatusBar(&ab);
	editorDrawMessageBar(&ab);

	// place cursor to the right position when refresh
	char buf[32];
	snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.rowoff) + 1,
											  (E.rx - E.coloff) + 1);
	abAppend(&ab, buf, strlen(buf));

	abAppend(&ab, "\x1b[?25h", 6); // re-show the cursor

	write(STDOUT_FILENO, ab.b, ab.len); // draw content in str onto screen
	abFree(&ab); // free the buffer
}

void editorSetStatusMessage(const char *fmt, ...){
	/* a variadic func so that one can pass a formated string */
	va_list ap;
	va_start(ap, fmt);
	vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
	va_end(ap);
	E.statusmsg_time = time(NULL);
}


/*** input ***/

char *editorPrompt(char *prompt, void(*callback)(char *, int)){
	/* prompt to ask user to input, 
	   take callback function to achieve incremental search,
	   besides searching, other cases will pass null */

	size_t bufsize = 128; 
	char *buf = malloc(bufsize);

	size_t buflen = 0;
	buf[0] = '\0';

	while(1){
		editorSetStatusMessage(prompt, buf);
		editorRefreshScreen();

		int c = editorReadKey();
		if(c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE){
			if(buflen != 0) buf[--buflen] = '\0';
		}else if(c == '\x1b'){ // press ESC to cancel
			editorSetStatusMessage("");
			if(callback) callback(buf, c);
			free(buf);
			return NULL;
		} else if(c == '\r'){
			if(buflen != 0){
				editorSetStatusMessage("");
				if(callback) callback(buf, c);
				return buf;
			}
		} else if(!iscntrl(c) && c < 128){ // ensure the key input is not a special key
			if(buflen == bufsize - 1){
				bufsize *= 2; // expand buf
				buf = realloc(buf, bufsize);
			}
			buf[buflen++] = c; // store input
			buf[buflen] = '\0';
		}

		if(callback) callback(buf, c);
	}
}

void editorMoveCursor(int key){
	/* maps arrow keys to cursor movements */

	// ensure cursor is not too much right and out of the file
	erow  *row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
	
	// moves cursor on arrow key presses
	switch(key){
		case ARROW_LEFT:
			if(E.cx != 0){
				E.cx--;
			} else if(E.cy > 0){ // up a line
				E.cy--;
				E.cx = E.row[E.cy].size;
			}
			break;
		case ARROW_RIGHT:
			if(row && E.cx < row->size){
				E.cx++;
			} else if(row && E.cx == row->size){ // next line
				E.cy++;
				E.cx--;
			}
			break;
		case ARROW_UP:
			if(E.cy != 0){
				E.cy--;
			}
			break;
		case ARROW_DOWN:
			if(E.cy < E.numrows){ // cannot pass the end of file
				E.cy++;
			}
			break;
	}

	row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy]; // update
	// make sure cursor is in the text region of the new row if cy is changed
	int rowlen = row ? row->size : 0;
	if(E.cx > rowlen){
		E.cx = rowlen;
	}
}

void editorProcessKeypress() {
	/* maps keypress */

	static int quit_times = QUIT_CONFIRM_TIMES; // static to extend lifetime

	int c = editorReadKey();

	switch(c) {
		case '\r':
			editorInsertNewLine();
			break;

		case CTRL_KEY('q'):
			if(E.dirty && quit_times > 0){ // to quit after confirmation if file is changed
				editorSetStatusMessage("WARNING!!! File has unsaved changes. "
										"Press Ctrl-Q %d more times to quit.",
										quit_times);
				quit_times--;
				return;
			}
			// clears screen and replace cursor back
			write(STDOUT_FILENO, "\x1b[2J", 4);
			write(STDOUT_FILENO, "\x1b[H", 3);
			exit(0);
			break;

		case CTRL_KEY('s'):
			editorSave();
			break;

		// may = Fn + arrow
		case HOME_KEY:
			E.cx = 0; // beginning of the line
			break;

		case END_KEY:
			if(E.cy < E.numrows)
				E.cx = E.row[E.cy].size; // end of the line
			break;

		case CTRL_KEY('f'):
			editorFind();
			break;

		case BACKSPACE:
		case CTRL_KEY('h'):
		case DEL_KEY:
			if(c == DEL_KEY) editorMoveCursor(ARROW_RIGHT); // del right of the cursor
			editorDelChar();
			break;

		// may = Fn + arrow key
		case PAGE_UP:
		case PAGE_DOWN:
			{
			if(c == PAGE_UP){
				E.cy = E.rowoff; // top of the page
			} else if(c == PAGE_DOWN){
				E.cy = E.rowoff + E.screenrows - 1; // next page
				if(E.cy > E.numrows) E.cy = E.numrows; // not passing the file
			}

			int times = E.screenrows;
			while (times--)
				editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
			}
			break;

		// mapping arrow keys
		case ARROW_UP:
		case ARROW_DOWN:
		case ARROW_LEFT:
		case ARROW_RIGHT:
			editorMoveCursor(c);
			break;

		case CTRL_KEY('l'):
		case '\x1b':
			break;

		default:
			editorInsertChar(c);
			break;
	}

	quit_times = QUIT_CONFIRM_TIMES; // reset
}


/*** init ***/

void initEditor(){
	E.cx = 0; // note 0 represents top & left
	E.cy = 0;
	E.rx = 0;
	E.rowoff = 0;
	E.coloff = 0;
	E.numrows = 0;
	E.row = NULL;
	E.dirty = 0;
	E.filename = NULL;
	E.statusmsg[0] = '\0';
	E.statusmsg_time = 0;
	E.syntax = NULL;

	if(getWindowSize(&E.screenrows, &E.screencols) == -1) die("getWindowSize");
	E.screenrows -= 2; // for status bar 
}

int main(int argc, char *argv[]) {
	enableRawMode();
	initEditor();

	if(argc >= 2){ // open file when arg is provided
		editorOpen(argv[1]);
	}

	editorSetStatusMessage("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");

	while (1){
		editorRefreshScreen();
		editorProcessKeypress();
	}
	return 0;
}
