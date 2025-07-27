#include <ncurses.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <signal.h>

#define MAX_LINES 999999
#define MAX_COLS 65
#define FIXED_COLS 7

typedef struct {
    char comment_col;
    char content[MAX_COLS + 1];
} Line;

Line lines[MAX_LINES];
int num_lines = 0;
int cur_line = 0;
int cur_col = FIXED_COLS;
int scroll_offset = 0;

char filename[256] = "";
char old_filename[256] = "";
int renaming = 0;
int confirm_structure = 0;
int modified = 0;

int is_special_comment(char c) {
    return c == '*' || c == '-' || c == '/';
}

void rtrim(char *str) {
    int len = strlen(str);
    while (len > 0 && (str[len - 1] == ' ' || str[len - 1] == '\t' || str[len - 1] == '\n'))
        str[--len] = '\0';
}

void load_file(const char *path) {
    FILE *f = fopen(path, "r");
    if (!f) return;
    strncpy(filename, path, 255);
    strncpy(old_filename, path, 255);
    filename[255] = old_filename[255] = '\0';

    num_lines = 0;
    char line[256];
    while (fgets(line, sizeof(line), f) && num_lines < MAX_LINES) {
        rtrim(line);
        int len = strlen(line);
        if (len < 7) {
            // Short or empty line -> empty comment column + empty content
            lines[num_lines].comment_col = ' ';
            lines[num_lines].content[0] = '\0';
        } else {
            lines[num_lines].comment_col = line[6];
            strncpy(lines[num_lines].content, &line[7], MAX_COLS);
            lines[num_lines].content[MAX_COLS] = '\0';
        }
        num_lines++;
    }
    fclose(f);
    modified = 0;
}

void init_lines() {
    if (num_lines > 0) return;
    lines[0].comment_col = ' ';
    strcpy(lines[0].content, "");
    num_lines = 1;
}

void insert_structure_now() {
    FILE *f = fopen(".structure", "r");
    if (!f) return;

    num_lines = 0;
    char line[256];
    while (fgets(line, sizeof(line), f) && num_lines < MAX_LINES) {
        rtrim(line);
        int len = strlen(line);
        if (len < 7) {
            // Short or empty line -> empty comment column + empty content
            lines[num_lines].comment_col = ' ';
            lines[num_lines].content[0] = '\0';
        } else {
            lines[num_lines].comment_col = line[6];
            strncpy(lines[num_lines].content, &line[7], MAX_COLS);
            lines[num_lines].content[MAX_COLS] = '\0';
        }
        num_lines++;
    }
    fclose(f);
    cur_line = 0;
    cur_col = FIXED_COLS;
    scroll_offset = 0;
    modified = 1;
}

void insert_line(int index) {
    if (num_lines >= MAX_LINES) return; // Maximum limit reached

    for (int i = num_lines; i > index; i--) {
        lines[i] = lines[i - 1];
    }
    lines[index].comment_col = ' ';
    lines[index].content[0] = '\0';

    num_lines++;
}

// delete_line returns the new cursor position
int delete_line(int index) {
    if (num_lines <= 1) return cur_line;  // Don't delete if only one line

    for (int i = index; i < num_lines - 1; i++) {
        lines[i] = lines[i + 1];
    }
    num_lines--;

    if (num_lines == 0) {
        // Safety: never empty, recreate an empty line
        lines[0].comment_col = ' ';
        lines[0].content[0] = '\0';
        num_lines = 1;
        return 0;
    }

    // If deleting the first line, stay on line 0
    if (index == 0) {
        return 0;
    } else {
        // Otherwise go back to the previous line
        return index - 1;
    }
}

void show_run_output(const char *output) {
    int win_height = LINES / 2;
    int win_width = COLS - 4;
    int starty = (LINES - win_height) / 2;
    int startx = 2;
    WINDOW *popup = newwin(win_height, win_width, starty, startx);
    wbkgd(popup, COLOR_PAIR(4));
    box(popup, 0, 0);
    mvwprintw(popup, 0, 2, " Output (F12:Run) - [UP/DOWN] Scroll, F4 to quit ");

    // Split output into lines
    char *lines[1024];
    int nlines = 0;
    char *buf = strdup(output);
    char *p = buf;
    while (p && nlines < 1024) {
        char *nl = strchr(p, '\n');
        if (nl) {
            *nl = '\0';
            lines[nlines++] = p;
            p = nl + 1;
        } else {
            lines[nlines++] = p;
            break;
        }
    }

    int scroll = 0;
    int max_scroll = (nlines > win_height - 2) ? nlines - (win_height - 2) : 0;
    int ch;
    keypad(popup, TRUE);
    do {
        werase(popup);
        box(popup, 0, 0);
        mvwprintw(popup, 0, 2, " Output (F12:Run) - [UP/DOWN] Scroll, F4 to quit ");
        for (int i = 0; i < win_height - 2; i++) {
            if (i + scroll < nlines)
                mvwprintw(popup, i + 1, 2, "%s", lines[i + scroll]);
        }
        wrefresh(popup);
        ch = wgetch(popup);
        if ((ch == KEY_DOWN || ch == 'j') && scroll < max_scroll) scroll++;
        if ((ch == KEY_UP || ch == 'k') && scroll > 0) scroll--;
    } while (ch != KEY_F(4));

    delwin(popup);
    free(buf);
}

void save_file(); // Forward declaration before run_script_with_gnu_cobol()

void run_script_with_gnu_cobol() {
    save_file();
    char safe_filename[236];
    strncpy(safe_filename, filename, 235);
    safe_filename[235] = '\0';
    const char *bin_name = "cwse_run_bin";
    char compile_cmd[512];
    snprintf(compile_cmd, sizeof(compile_cmd),
        "cobc -x -o %s \"%s\" 2>&1", bin_name, safe_filename);

    // Compilation with error capture
    FILE *fp = popen(compile_cmd, "r");
    if (!fp) {
        show_run_output("Failed to run compiler.");
        return;
    }
    char compile_output[4096] = {0};
    size_t offset = 0;
    while (fgets(compile_output + offset, sizeof(compile_output) - offset, fp)) {
        offset = strlen(compile_output);
        if (offset >= sizeof(compile_output) - 1) break;
    }
    int compile_status = pclose(fp);

    if (compile_status != 0) {
        show_run_output(compile_output[0] ? compile_output : "COBOL compilation error.");
        return;
    }

    // Exit ncurses to allow COBOL program interactivity
    endwin();
    printf("\033[2J\033[H"); // Clear screen and move cursor to top-left
    printf("\n--- COBOL program execution ---\n");
    printf("(Press Enter after the end to return to the editor)\n\n");
    char run_cmd[256];
    snprintf(run_cmd, sizeof(run_cmd), "./%s", bin_name);
    system(run_cmd);
    printf("\n--- End of COBOL execution ---\n");
    printf("Press Enter to return to the editor...");
    getchar();

    // Cleanup binary
    char rm_cmd[256];
    snprintf(rm_cmd, sizeof(rm_cmd), "rm -f %s", bin_name);
    system(rm_cmd);

    // Restart ncurses
    refresh();
    initscr();
    printf("\033[2J\033[H"); // Clear terminal after ncurses is re-initialized
    noecho();
    cbreak();
    keypad(stdscr, TRUE);
    curs_set(1);
    start_color();
    scrollok(stdscr, TRUE);
    idlok(stdscr, TRUE);

    // Reinitialize colors if needed
    init_pair(1, COLOR_YELLOW, COLOR_BLACK);
    init_pair(2, COLOR_CYAN, COLOR_BLACK);
    init_pair(3, COLOR_RED, COLOR_BLACK);
    init_pair(4, COLOR_WHITE, COLOR_BLACK);
    init_pair(5, COLOR_GREEN, COLOR_BLACK);
    init_pair(6, COLOR_MAGENTA, COLOR_BLACK); // Strings
    init_pair(7, COLOR_BLUE, COLOR_BLACK);    // Keywords
    init_pair(8, COLOR_GREEN, COLOR_BLACK);   // Numbers
}

void show_menu_popup() {
    int win_height = 10;
    int win_width = 40;
    int starty = (LINES - win_height) / 2;
    int startx = (COLS - win_width) / 2;
    WINDOW *menu = newwin(win_height, win_width, starty, startx);
    wbkgd(menu, COLOR_PAIR(4));
    box(menu, 0, 0);
    mvwprintw(menu, 1, 2, " CWSE Menu (F5 to close) ");
    mvwprintw(menu, 3, 4, "1. Future Functionality");
    mvwprintw(menu, 4, 4, "2. Future Functionality");
    mvwprintw(menu, 5, 4, "3. Future Functionality");
    mvwprintw(menu, 6, 4, "4. Future Functionality");
    mvwprintw(menu, 8, 4, "Press F5 to close.");
    wrefresh(menu);

    int ch;
    keypad(menu, TRUE);
    do {
        ch = wgetch(menu);
    } while (ch != KEY_F(5));
    delwin(menu);
}

void draw_header() {
    attron(COLOR_PAIR(4));
    mvprintw(0, 0, "C.W.S.E. V0.2 ");
    // F5:MENU and F12:Run top-right
    int run_label_col = COLS - 13;
    int menu_label_col = run_label_col - 13;
    attron(COLOR_PAIR(4) | A_REVERSE);
    mvprintw(0, menu_label_col, " F5: MENU ");
    attroff(A_REVERSE | COLOR_PAIR(4));
    attron(COLOR_PAIR(4) | A_REVERSE);
    mvprintw(0, run_label_col, " F12: Run ");
    attroff(A_REVERSE | COLOR_PAIR(4));
    clrtoeol();
    attroff(COLOR_PAIR(4));
    attron(A_BOLD | COLOR_PAIR(5));
    mvprintw(1, 0, "----+----1----+----2----+----3----+----4----+----5----+----6----+----7--");
    attroff(A_BOLD | COLOR_PAIR(5));
}

void draw_top_of_data() {
    attron(COLOR_PAIR(1));
    mvprintw(2, 0, "****** ******************************TOP OF DATA************************");
    attroff(COLOR_PAIR(1));
}

void draw_footer() {
    int row = LINES - 1;
    attron(A_REVERSE);
    mvprintw(row, 0, "F2:Save  F3:Rename  ESC:Quit  F8:Insert Structure");

    char short_name[256];
    size_t len = strlen(filename);
    int max_len = 22;

    if (len > max_len) {
        if (modified) {
            short_name[0] = '*';
            strncpy(short_name + 1, filename, max_len);
            memcpy(short_name + 1 + max_len, "...", 4);
        } else {
            strncpy(short_name, filename, max_len);
            memcpy(short_name + max_len, "...", 4);
        }
    } else {
        if (modified) {
            short_name[0] = '*';
            strcpy(short_name + 1, filename);
        } else {
            strcpy(short_name, filename);
        }
    }

    mvprintw(row, COLS - strlen(short_name) - 2, "%s", short_name);
    clrtoeol();
    attroff(A_REVERSE);
}

void draw_lines() {
    int visible = LINES - 5;
    for (int i = 0; i < visible; i++) {
        int actual = i + scroll_offset;
        if (actual >= num_lines) break;

        attron(COLOR_PAIR(4));
        mvprintw(3 + i, 0, "%06d", actual + 1);
        attroff(COLOR_PAIR(4));

        attron(COLOR_PAIR(3));
        mvaddch(3 + i, 6, lines[actual].comment_col);
        attroff(COLOR_PAIR(3));

        // Syntax highlighting for content
        int x = FIXED_COLS;
        const char *s = lines[actual].content;
        int in_string = 0;
        char string_char = 0;
        while (*s && x < FIXED_COLS + MAX_COLS) {
            // Check for string start/end
            if (!in_string && (*s == '"' || *s == '\'')) {
                in_string = 1;
                string_char = *s;
                attron(COLOR_PAIR(6));
                mvaddch(3 + i, x++, *s++);
                continue;
            }
            if (in_string) {
                attron(COLOR_PAIR(6));
                mvaddch(3 + i, x, *s);
                if (*s == string_char) {
                    in_string = 0;
                    attroff(COLOR_PAIR(6));
                }
                x++; s++;
                continue;
            }
            // Highlight keywords
            if (strncmp(s, "DIVISION", 8) == 0) {
                attron(COLOR_PAIR(7) | A_BOLD);
                for (int k = 0; k < 8; k++)
                    mvaddch(3 + i, x++, s[k]);
                attroff(COLOR_PAIR(7) | A_BOLD);
                s += 8;
                continue;
            }
            if (strncmp(s, "SECTION", 7) == 0) {
                attron(COLOR_PAIR(7) | A_BOLD);
                for (int k = 0; k < 7; k++)
                    mvaddch(3 + i, x++, s[k]);
                attroff(COLOR_PAIR(7) | A_BOLD);
                s += 7;
                continue;
            }
            // Highlight numbers
            if (*s >= '0' && *s <= '9') {
                attron(COLOR_PAIR(8) | A_BOLD);
                int start = x;
                while (*s >= '0' && *s <= '9' && x < FIXED_COLS + MAX_COLS) {
                    mvaddch(3 + i, x++, *s++);
                }
                attroff(COLOR_PAIR(8) | A_BOLD);
                continue;
            }
            // Default color
            attron(COLOR_PAIR(lines[actual].comment_col == '*' ? 2 : 1));
            mvaddch(3 + i, x++, *s++);
            attroff(COLOR_PAIR(1));
            attroff(COLOR_PAIR(2));
        }
        // Fill rest of line
        while (x < FIXED_COLS + MAX_COLS) {
            attron(COLOR_PAIR(lines[actual].comment_col == '*' ? 2 : 1));
            mvaddch(3 + i, x++, ' ');
            attroff(COLOR_PAIR(1));
            attroff(COLOR_PAIR(2));
        }

        // Cursor highlight
        if (actual == cur_line) {
            int cx = cur_col;
            if (cur_col == 6) {
                attron(A_REVERSE | COLOR_PAIR(3));
                mvaddch(3 + i, 6, lines[actual].comment_col);
                attroff(A_REVERSE | COLOR_PAIR(3));
            } else {
                int pos = cur_col - FIXED_COLS;
                char c = (pos < (int)strlen(lines[actual].content)) ? lines[actual].content[pos] : ' ';
                attron(A_REVERSE | COLOR_PAIR(4));
                mvaddch(3 + i, cx, c);
                attroff(A_REVERSE | COLOR_PAIR(4));
            }
        }
    }
}

void draw_rename_input() {
    attron(A_REVERSE);
    mvprintw(LINES - 3, 0, "File name: %s", filename);
    clrtoeol();
    move(LINES - 3, 11 + strlen(filename));
    attroff(A_REVERSE);
}

void draw_structure_confirmation() {
    attron(A_BOLD);
    mvprintw(LINES - 3, 0, "Insert COBOL structure? (y/n) This will overwrite everything.");
    clrtoeol();
    attroff(A_BOLD);
}

void save_file() {
    if (strlen(filename) == 0) {
        renaming = 1;
        return;
    }

    FILE *f = fopen(filename, "w");
    if (!f) return;

    for (int i = 0; i < num_lines; i++) {
        rtrim(lines[i].content);
        if (is_special_comment(lines[i].comment_col))
            fprintf(f, "%06d%c%s\n", i + 1, lines[i].comment_col, lines[i].content);
        else
            fprintf(f, "%06d %s\n", i + 1, lines[i].content);
    }
    fclose(f);
    modified = 0;
}

void handle_sigint(int sig) {
    endwin();
    printf("\033[2J\033[H");
    fflush(stdout);
    exit(0);
}

int main(int argc, char **argv) {
    signal(SIGINT, handle_sigint);

    FILE *structure_check = fopen(".structure", "r");
    if (!structure_check) {
        FILE *structure_file = fopen(".structure", "w");
        if (structure_file) {
            fprintf(structure_file,
                "000001 IDENTIFICATION DIVISION.\n"
                "000002 PROGRAM-ID. FILENAME.\n"
                "000003\n"
                "000004 ENVIRONMENT DIVISION.\n"
                "000005 CONFIGURATION SECTION.\n"
                "000006\n"
                "000007 DATA DIVISION.\n"
                "000008 WORKING-STORAGE SECTION. \n"
                "000009\n"
                "000010 PROCEDURE DIVISION. \n"
                "000011*    YOUR CODE HERE \n"
                "000012     STOP RUN.\n"
                "000013 END PROGRAM FILENAME.\n"
            );
            fclose(structure_file);
        }
    } else {
        fclose(structure_check);
    }

    if (argc > 1) {
        FILE *f = fopen(argv[1], "r");
        if (!f) {
            f = fopen(argv[1], "w");
            if (f) fclose(f);
        } else {
            fclose(f);
        }
        load_file(argv[1]);
    }

    initscr();
    noecho();
    cbreak();
    keypad(stdscr, TRUE);
    curs_set(1);
    start_color();
    scrollok(stdscr, TRUE);
    idlok(stdscr, TRUE);

    init_pair(1, COLOR_YELLOW, COLOR_BLACK);
    init_pair(2, COLOR_CYAN, COLOR_BLACK);
    init_pair(3, COLOR_RED, COLOR_BLACK);
    init_pair(4, COLOR_WHITE, COLOR_BLACK);
    init_pair(5, COLOR_GREEN, COLOR_BLACK);
    init_pair(6, COLOR_MAGENTA, COLOR_BLACK); // Strings
    init_pair(7, COLOR_BLUE, COLOR_BLACK);    // Keywords
    init_pair(8, COLOR_GREEN, COLOR_BLACK);   // Numbers

    init_lines();

    int ch;
    while (1) {
        erase();
        draw_header();
        draw_top_of_data();
        draw_lines();
        draw_footer();

        if (confirm_structure) {
            draw_structure_confirmation();
            refresh();
            ch = getch();
            if (ch == 'y' || ch == 'Y') insert_structure_now();
            confirm_structure = 0;
            modified = 1;
            continue;
        }

        if (renaming) {
            draw_rename_input();
            refresh();
            ch = getch();
            if (ch == 27) renaming = 0;
            else if (ch == KEY_BACKSPACE || ch == 127) {
                int len = strlen(filename);
                if (len > 0) filename[len - 1] = '\0';
            } else if (ch == '\n') {
                if (strcmp(old_filename, filename) != 0) {
                    remove(old_filename);
                    strncpy(old_filename, filename, 255);
                }
                renaming = 0;
                save_file();
            } else if (ch >= 32 && ch <= 126 && strlen(filename) < 255) {
                filename[strlen(filename) + 1] = '\0';
                filename[strlen(filename)] = ch;
            }
            continue;
        }

        move(3 + cur_line - scroll_offset, cur_col);
        refresh();
        ch = getch();

        switch (ch) {
            case 27:
                endwin();
                return 0;
            case KEY_F(2):
                save_file();
                break;
            case KEY_F(3):
                renaming = 1;
                break;
            case KEY_F(5):
                show_menu_popup();
                break;
            case KEY_F(8):
                confirm_structure = 1;
                break;
            case KEY_F(12):
                run_script_with_gnu_cobol();
                break;
            case KEY_DOWN:
                if (cur_line < num_lines - 1) {
                    cur_line++;
                    if (cur_line >= scroll_offset + LINES - 5)
                        scroll_offset++;
                }
                break;
            case KEY_UP:
                if (cur_line > 0) {
                    cur_line--;
                    if (cur_line < scroll_offset)
                        scroll_offset--;
                }
                break;
            case KEY_LEFT:
                if (cur_col > 6)
                    cur_col--;
                break;
            case KEY_RIGHT:
                if (cur_col < FIXED_COLS + MAX_COLS - 1)
                    cur_col++;
                break;
            case '\n':
                insert_line(cur_line + 1);
                cur_line++;
                cur_col = FIXED_COLS;
                modified = 1;
                if (cur_line >= scroll_offset + LINES - 5)
                    scroll_offset++;
                break;
            case KEY_BACKSPACE:
            case 127:
                if (cur_col == 6) {
                    if (lines[cur_line].comment_col != ' ') {
                        lines[cur_line].comment_col = ' ';
                        modified = 1;
                    } else if (strlen(lines[cur_line].content) == 0) {
                        cur_line = delete_line(cur_line);
                        cur_col = FIXED_COLS;
                        modified = 1;
                    }
                } else {
                    int pos = cur_col - FIXED_COLS;
                    int len = strlen(lines[cur_line].content);
                    if (pos > 0 && pos <= len) {
                        memmove(&lines[cur_line].content[pos - 1], &lines[cur_line].content[pos], len - pos + 1);
                        cur_col--;
                        modified = 1;
                    } else if (pos == 0 && len == 0) {
                        cur_line = delete_line(cur_line);
                        cur_col = FIXED_COLS;
                        modified = 1;
                    }
                }
                break;
            case KEY_DC:
            {
                int pos = cur_col - FIXED_COLS;
                int len = strlen(lines[cur_line].content);
                if (pos >= 0 && pos < len) {
                    memmove(&lines[cur_line].content[pos], &lines[cur_line].content[pos + 1], len - pos);
                    modified = 1;
                } else if (pos == len && len == 0) {
                    cur_line = delete_line(cur_line);
                    cur_col = FIXED_COLS;
                    modified = 1;
                }
                break;
            }
            case '\t': // TAB key
                if (cur_col >= FIXED_COLS) {
                    int pos = cur_col - FIXED_COLS;
                    int len = strlen(lines[cur_line].content);
                    // Check if we have enough space for 4 spaces
                    if (len + 4 <= MAX_COLS && pos <= len) {
                        // Shift content to the right
                        memmove(&lines[cur_line].content[pos + 4], &lines[cur_line].content[pos], len - pos + 1);
                        // Insert 4 spaces
                        memset(&lines[cur_line].content[pos], ' ', 4);
                        cur_col += 4;
                        // Make sure cursor doesn't exceed limit
                        if (cur_col > FIXED_COLS + MAX_COLS - 1) {
                            cur_col = FIXED_COLS + MAX_COLS - 1;
                        }
                        modified = 1;
                    }
                }
                break;
            default:
                if (cur_col == 6 && is_special_comment(ch)) {
                    if (lines[cur_line].comment_col != ch) {
                        lines[cur_line].comment_col = ch;
                        modified = 1;
                    }
                } else if (cur_col >= FIXED_COLS && ch >= 32 && ch <= 126) {
                    int pos = cur_col - FIXED_COLS;
                    int len = strlen(lines[cur_line].content);
                    
                    // MAIN FIX: Allow writing even beyond the end of existing content
                    if (pos < MAX_COLS && len < MAX_COLS) {
                        // If writing beyond the end, fill with spaces
                        if (pos > len) {
                            // Fill with spaces up to the desired position
                            memset(&lines[cur_line].content[len], ' ', pos - len);
                            lines[cur_line].content[pos] = '\0'; // Temporarily terminate
                            len = pos; // Update length
                        }
                        
                        // If inserting in the middle, shift content
                        if (pos < len) {
                            memmove(&lines[cur_line].content[pos + 1], &lines[cur_line].content[pos], len - pos + 1);
                        }
                        
                        // Insert the character
                        lines[cur_line].content[pos] = ch;
                        
                        // Ensure null termination
                        if (pos >= len) {
                            lines[cur_line].content[pos + 1] = '\0';
                        }
                        
                        cur_col++;
                        // Make sure cursor doesn't exceed limit
                        if (cur_col > FIXED_COLS + MAX_COLS - 1) {
                            cur_col = FIXED_COLS + MAX_COLS - 1;
                        }
                        modified = 1;
                    }
                }
                break;
        }
    }

    endwin();
    return 0;
}
