#include <ncurses.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <ctype.h>

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
int selected_menu_item = 0;

int is_special_comment(char c) {
    return c == '*' || c == '-' || c == '/';
}

// Generic menu display function with arrow navigation
typedef struct {
    char *label;
    int shortcut;
} MenuItem;

static void close_menu(WINDOW *menu, WINDOW *parent) {
    werase(menu);
    wrefresh(menu);
    delwin(menu);
    clearok(stdscr, TRUE);
    if (parent) {
        touchwin(parent);
        wrefresh(parent);
    } else {
        touchwin(stdscr);
        refresh();
    }
}

int show_interactive_menu(WINDOW *parent, const char *title, MenuItem items[], int item_count) {
    int win_height = item_count + 6;
    int win_width = 50;
    int starty = (LINES - win_height) / 2;
    int startx = (COLS - win_width) / 2;

    WINDOW *menu = newwin(win_height, win_width, starty, startx);
    if (!menu) return -1;

    wbkgd(menu, COLOR_PAIR(4));
    keypad(menu, TRUE);
    clearok(menu, TRUE);

    box(menu, 0, 0);

    wattron(menu, A_BOLD | COLOR_PAIR(5));
    mvwprintw(menu, 1, (win_width - (int)strlen(title)) / 2, "%s", title);
    wattroff(menu, A_BOLD | COLOR_PAIR(5));

    for (int x = 1; x < win_width - 1; x++) {
        mvwaddch(menu, 2, x, '-');
    }

    int selected = 0;
    int ch;

    while (1) {
        box(menu, 0, 0);

        for (int i = 0; i < item_count; i++) {
            mvwprintw(menu, 3 + i, 3, "%-*s", win_width - 6, "");
            if (i == selected) {
                wattron(menu, A_REVERSE | COLOR_PAIR(5));
                mvwprintw(menu, 3 + i, 3, " > %-*s", win_width - 8, items[i].label);
                wattroff(menu, A_REVERSE | COLOR_PAIR(5));
            } else {
                wattron(menu, COLOR_PAIR(1));
                mvwprintw(menu, 3 + i, 3, "   %-*s", win_width - 8, items[i].label);
                wattroff(menu, COLOR_PAIR(1));
            }
        }

        mvwprintw(menu, item_count + 4, 3, "ESC to cancel");
        wrefresh(menu);

        ch = wgetch(menu);

        if (ch == KEY_UP) {
            selected = (selected - 1 + item_count) % item_count;
        } else if (ch == KEY_DOWN) {
            selected = (selected + 1) % item_count;
        } else if (ch == '\n' || ch == ' ') {
            close_menu(menu, parent);
            return selected;
        } else if (ch == 27) {
            close_menu(menu, parent);
            return -1;
        } else {
            for (int i = 0; i < item_count; i++) {
                if (ch == items[i].shortcut) {
                    close_menu(menu, parent);
                    return i;
                }
            }
        }
    }
}

void rtrim(char *str) {
    int len = strlen(str);
    while (len > 0 && (str[len - 1] == ' ' || str[len - 1] == '\t' || str[len - 1] == '\n'))
        str[--len] = '\0';
}

void load_file(const char *path) {
    strncpy(filename, path, 255);
    strncpy(old_filename, path, 255);
    filename[255] = old_filename[255] = '\0';
    
    FILE *f = fopen(path, "r");
    if (!f) {
        // File doesn't exist, start with empty content
        num_lines = 1;
        lines[0].comment_col = ' ';
        lines[0].content[0] = '\0';
        modified = 0;
        return;
    }

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
    int win_height = LINES - 6;
    int win_width = COLS - 4;
    int starty = 3;
    int startx = 2;
    WINDOW *popup = newwin(win_height, win_width, starty, startx);
    wbkgd(popup, COLOR_PAIR(4));
    box(popup, 0, 0);
    
    // Draw title bar
    wattron(popup, A_BOLD | COLOR_PAIR(5));
    mvwprintw(popup, 0, 2, " === Resultat de l'Execution === ");
    wattroff(popup, A_BOLD | COLOR_PAIR(5));

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
    int max_scroll = (nlines > win_height - 4) ? nlines - (win_height - 4) : 0;
    int ch;
    keypad(popup, TRUE);
    do {
        werase(popup);
        box(popup, 0, 0);
        
        wattron(popup, A_BOLD | COLOR_PAIR(5));
        mvwprintw(popup, 0, 2, " === Execution Result === ");
        wattroff(popup, A_BOLD | COLOR_PAIR(5));
        
        wattron(popup, COLOR_PAIR(1));
        for (int i = 0; i < win_height - 4; i++) {
            if (i + scroll < nlines) {
                // Highlight error lines
                if (strstr(lines[i + scroll], "ERROR") || strstr(lines[i + scroll], "error")) {
                    wattron(popup, COLOR_PAIR(3) | A_BOLD);
                    mvwprintw(popup, i + 2, 2, "%s", lines[i + scroll]);
                    wattroff(popup, COLOR_PAIR(3) | A_BOLD);
                } else if (strstr(lines[i + scroll], "[OK]")) {
                    wattron(popup, COLOR_PAIR(5));
                    mvwprintw(popup, i + 2, 2, "%s", lines[i + scroll]);
                    wattroff(popup, COLOR_PAIR(5));
                } else {
                    mvwprintw(popup, i + 2, 2, "%s", lines[i + scroll]);
                }
            }
        }
        wattroff(popup, COLOR_PAIR(1));
        
        // Draw footer
        wattron(popup, COLOR_PAIR(6));
        mvwprintw(popup, win_height - 2, 2, "^v Scroll | ESC/F4 Close");
        wattroff(popup, COLOR_PAIR(6));
        
        wrefresh(popup);
        ch = wgetch(popup);
        if ((ch == KEY_DOWN || ch == 'j') && scroll < max_scroll) scroll++;
        if ((ch == KEY_UP || ch == 'k') && scroll > 0) scroll--;
    } while (ch != KEY_F(4) && ch != 27);

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
    set_escdelay(25);
    printf("\033[6 q"); // Set cursor to block
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

void show_compile_options() {
    MenuItem items[] = {
        {"Compile and Run (F12)", '1'},
        {"Compile Only", '2'},
        {"Compile with Debug", '3'}
    };
    
    int choice = show_interactive_menu(NULL, " === Compilation Options === ", items, 3);
    
    if (choice == -1) return;
    
    save_file();
    char safe_filename[236];
    strncpy(safe_filename, filename, 235);
    safe_filename[235] = '\0';
    const char *bin_name = "cwse_run_bin";
    
    if (choice == 0) {
        // Compile and Run
        run_script_with_gnu_cobol();
    } else if (choice == 1) {
        // Compile only
        char compile_cmd[512];
        snprintf(compile_cmd, sizeof(compile_cmd),
            "cobc -x -o %s \"%s\" 2>&1", bin_name, safe_filename);
        FILE *fp = popen(compile_cmd, "r");
        if (!fp) {
            show_run_output("Error: Failed to run compiler.");
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
            show_run_output(compile_output[0] ? compile_output : "ERROR: COBOL compilation failed.");
        } else {
            show_run_output("[OK] Compilation successful!");
        }
    } else if (choice == 2) {
        // Compile with debug
        char compile_cmd[512];
        snprintf(compile_cmd, sizeof(compile_cmd),
            "cobc -x -g -o %s \"%s\" 2>&1", bin_name, safe_filename);
        FILE *fp = popen(compile_cmd, "r");
        if (!fp) {
            show_run_output("Error: Failed to run compiler.");
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
            show_run_output(compile_output[0] ? compile_output : "ERROR: COBOL compilation failed.");
        } else {
            show_run_output("[OK] Debug compilation successful!");
        }
    }
}

void show_file_operations() {
    MenuItem items[] = {
        {"Save (F2)", '1'},
        {"Rename File (F3)", '2'}
    };
    
    int choice = show_interactive_menu(NULL, " === File Operations === ", items, 2);
    
    if (choice == -1) return;
    
    if (choice == 0) {
        save_file();
    } else if (choice == 1) {
        renaming = 1;
    }
}

void show_help_info() {
    int win_height = 18;
    int win_width = 60;
    int starty = (LINES - win_height) / 2;
    int startx = (COLS - win_width) / 2;
    WINDOW *help = newwin(win_height, win_width, starty, startx);
    wbkgd(help, COLOR_PAIR(5));
    box(help, ACS_VLINE, ACS_HLINE);
    
    // Draw title with colors
    wattron(help, A_BOLD | COLOR_PAIR(5));
    mvwprintw(help, 1, 2, "+======================================================+");
    mvwprintw(help, 2, 2, "|   C.W.S.E. - COBOL Work Script Editor v0.3           |");
    mvwprintw(help, 3, 2, "+======================================================+");
    wattroff(help, A_BOLD | COLOR_PAIR(5));
    
    wattron(help, COLOR_PAIR(1));
    mvwprintw(help, 5, 4, "Keyboard Shortcuts:");
    mvwprintw(help, 6, 6, "F2      - Save File");
    mvwprintw(help, 7, 6, "F3      - Rename File");
    mvwprintw(help, 8, 6, "F5      - Open main menu");
    mvwprintw(help, 9, 6, "F8      - Insert COBOL structure");
    mvwprintw(help, 10, 6, "F12     - Compile and run");
    mvwprintw(help, 11, 6, "ESC     - Quit editor");
    mvwprintw(help, 12, 6, "TAB     - Insert indentation");
    mvwprintw(help, 13, 6, "Arrows  - Navigate");
    
    wattron(help, COLOR_PAIR(6));
    mvwprintw(help, 15, 4, "[OK] Press any key to close");
    wattroff(help, COLOR_PAIR(6));
    
    wrefresh(help);
    
    int ch;
    keypad(help, TRUE);
    getch();
    delwin(help);
}

void show_menu_popup() {
    while (1) {
        MenuItem items[] = {
            {"File Operations", '1'},
            {"Compilation Options", '2'},
            {"Insert COBOL Structure (F8)", '3'},
            {"Keyboard Shortcuts", '4'}
        };
        int choice = show_interactive_menu(NULL, " === Main Menu CWSE === ", items, 4);
        if (choice == -1) return;

        if (choice == 0) {
            // File Operations
            while (1) {
                MenuItem file_items[] = {
                    {"Save (F2)", '1'},
                    {"Rename File (F3)", '2'},
                    {"< Back", '0'}
                };
                int fchoice = show_interactive_menu(NULL, " === File Operations === ", file_items, 3);
                if (fchoice == -1) return;  // ESC = ferme tout
                if (fchoice == 2) break;    // < Back = retour menu principal
                if (fchoice == 0) { save_file(); return; }
                if (fchoice == 1) { renaming = 1; return; }
            }

        } else if (choice == 1) {
            // Compilation Options
            while (1) {
                MenuItem comp_items[] = {
                    {"Compile and Run (F12)", '1'},
                    {"Compile Only", '2'},
                    {"Compile with Debug", '3'},
                    {"< Back", '0'}
                };
                int cchoice = show_interactive_menu(NULL, " === Compilation Options === ", comp_items, 4);
                if (cchoice == -1) return;  // ESC = ferme tout
                if (cchoice == 3) break;    // < Back = retour menu principal
                if (cchoice == 0) { run_script_with_gnu_cobol(); return; }
                if (cchoice == 1) {
                    save_file();
                    char compile_cmd[512];
                    snprintf(compile_cmd, sizeof(compile_cmd),
                        "cobc -x -o cwse_run_bin \"%s\" 2>&1", filename);
                    FILE *fp = popen(compile_cmd, "r");
                    char out[4096] = {0}; size_t off = 0;
                    while (fgets(out + off, sizeof(out) - off, fp)) off = strlen(out);
                    int st = pclose(fp);
                    show_run_output(st != 0 ? (out[0] ? out : "ERROR: Compilation failed.") : "[OK] Compilation successful!");
                }
                if (cchoice == 2) {
                    save_file();

                    char base[256];
                    strncpy(base, filename, 255);
                    base[255] = '\0';
                    char *slash = strrchr(base, '/');
                    if (slash) memmove(base, slash + 1, strlen(slash + 1) + 1);
                    char *dot = strrchr(base, '.');
                    if (dot) *dot = '\0';

                    char debug_dir[320];
                    snprintf(debug_dir, sizeof(debug_dir), "debug_%s", filename);
                    char mkdir_cmd[400];
                    snprintf(mkdir_cmd, sizeof(mkdir_cmd), "mkdir -p \"%s\"", debug_dir);
                    system(mkdir_cmd);

                    char bin_path[600];
                    snprintf(bin_path, sizeof(bin_path), "%s/%s", debug_dir, base);
                    char compile_cmd[900];
                    snprintf(compile_cmd, sizeof(compile_cmd),
                        "cobc -x -g -save-temps -o \"%s\" \"%s\" 2>&1",
                        bin_path, filename);
                    FILE *fp = popen(compile_cmd, "r");
                    char out[4096] = {0}; size_t off = 0;
                    while (fgets(out + off, sizeof(out) - off, fp)) off = strlen(out);
                    int st = pclose(fp);

                    char mv_cmd[2500];
                    snprintf(mv_cmd, sizeof(mv_cmd), "mv -f \"%s\".c \"%s/\" 2>/dev/null", base, debug_dir);
                    system(mv_cmd);
                    snprintf(mv_cmd, sizeof(mv_cmd), "mv -f \"%s\".c.h \"%s/\" 2>/dev/null", base, debug_dir);
                    system(mv_cmd);
                    snprintf(mv_cmd, sizeof(mv_cmd), "mv -f \"%s\".c.l.h \"%s/\" 2>/dev/null", base, debug_dir);
                    system(mv_cmd);
                    snprintf(mv_cmd, sizeof(mv_cmd), "mv -f \"%s\".i \"%s/\" 2>/dev/null", base, debug_dir);
                    system(mv_cmd);
                    snprintf(mv_cmd, sizeof(mv_cmd), "mv -f \"%s\".o \"%s/\" 2>/dev/null", base, debug_dir);
                    system(mv_cmd);

                    char result[4300];
                    if (st != 0) {
                        snprintf(result, sizeof(result), "%s",
                            out[0] ? out : "ERROR: Compilation failed.");
                    } else {
                        snprintf(result, sizeof(result),
                            "[OK] Debug compilation successful!\n\nFiles saved in: %s/\n  - %s (binary)\n  - %s.c\n  - %s.c.h\n  - %s.c.l.h\n  - %s.i\n  - %s.o",
                            debug_dir, base, base, base, base, base, base);
                    }
                    show_run_output(result);
                }
            }

        } else if (choice == 2) {
            confirm_structure = 1;
            return;
        } else if (choice == 3) {
            show_help_info();
        }
    }
}

void draw_header() {
    attron(COLOR_PAIR(4));
    mvprintw(0, 0, " C.W.S.E. - COBOL Editor v0.3 ");

    char status[50];
    snprintf(status, sizeof(status), "Line %d | Col %d", cur_line + 1, cur_col + 1);
    mvprintw(0, COLS - strlen(status) - 2, "%s", status);
    
    // Display current filename
    char short_name[30];
    size_t len = strlen(filename);
    if (len > 25) {
        strncpy(short_name, filename, 22);
        strcpy(short_name + 22, "...");
    } else {
        strcpy(short_name, filename);
    }
    
    clrtoeol();
    attroff(COLOR_PAIR(4));
    
    attron(A_BOLD | COLOR_PAIR(2));
    mvprintw(1, 0, "----+----1----+----2----+----3----+----4----+----5----+----6----+----7--");
    attroff(A_BOLD | COLOR_PAIR(2));
}

void draw_top_of_data() {
    attron(COLOR_PAIR(1));
    mvprintw(2, 0, "****** ******************************TOP OF DATA************************");
    attroff(COLOR_PAIR(1));
}

void draw_footer() {
    int row = LINES - 1;
    attron(A_REVERSE | COLOR_PAIR(4));
    mvprintw(row, 0, "[F2]Save  [F3]Rename  [F5]Menu  [F8]Struct  [F12]Run  [ESC]Quit");

    char short_name[256];
    size_t len = strlen(filename);
    int max_len = 20;

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
    attroff(A_REVERSE | COLOR_PAIR(4));
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
            // Highlight numbers (only if NOT part of identifier)
	        if (*s >= '0' && *s <= '9' &&
	            (s == lines[actual].content || !(isalnum(*(s - 1)) || *(s - 1) == '-'))) {
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
    printf("\033[6 q");
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
        clearok(stdscr, TRUE); 
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
