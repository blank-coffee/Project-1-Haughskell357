import tkinter as tk
from tkinter import filedialog, scrolledtext
from tkinter import ttk
import subprocess
import threading
import os

THEMES = {
    "dark": {
        "BG": "#1c1b2e",
        "PANEL": "#2a2740",
        "FG": "#d6cff0",
        "ACCENT": "#5e5086",
        "ACCENT2": "#8c75b8",
        "ERROR": "#ff6b6b",
        "DRY": "#d6cf70",
        "PROGRESS": "#6aa0ff",
        "STATUS_BG": "#11101a",
        "BROWN": "#4a3f32"
    },
    "light": {
        "BG": "#e7e6e1",
        "PANEL": "#f4f3ef",
        "FG": "#3a3a38",
        "ACCENT": "#b8a76f",
        "ACCENT2": "#d1c48a",
        "ERROR": "#b84a4a",
        "DRY": "#8c7a2e",
        "PROGRESS": "#4caf50",
        "STATUS_BG": "#dcd9d2",
        "BROWN": "#d2c4b2"
    }
}

MONO = ("Courier New", 10)
LABEL = ("Courier New", 10)

current_theme = THEMES["dark"]
tester_process = None
current_input = ""

def log(text, tag=None):
    if tag:
        output.insert(tk.END, text, tag)
    else:
        output.insert(tk.END, text)
    output.see(tk.END)

def cli_log(text):
    cli_terminal.insert(tk.END, text)
    cli_terminal.see(tk.END)

def set_status(text):
    status_var.set(text)

def update_progress_from_line(line):
    try:
        parts = line.strip().split()
        if len(parts) < 3:
            return
        phase = parts[1]
        cur, total = map(int, parts[2].split("/"))
        progress_bar["value"] = (cur / total) * 100
        set_status(f"{phase.capitalize()} {cur}/{total}")
    except:
        pass

def apply_theme():
    theme = current_theme
    root.configure(bg=theme["BG"])
    top_bar.configure(bg=theme["BG"])
    theme_frame.configure(bg=theme["BG"])
    content_frame.configure(bg=theme["BG"])
    gui_container.configure(bg=theme["BG"])
    flag_frame.configure(bg=theme["BG"])
    button_row.configure(bg=theme["BG"])
    progress_frame.configure(bg=theme["PANEL"])
    output.configure(bg=theme["PANEL"], fg=theme["FG"], insertbackground=theme["FG"])
    cli_terminal.configure(bg=theme["PANEL"], fg=theme["FG"], insertbackground=theme["FG"])
    center_spacer.configure(bg=theme["BG"])
    dev_safe_frame.configure(bg=theme["BG"])
    dev_safe_check.configure(bg=theme["BG"], fg=theme["ACCENT2"],
                             selectcolor=theme["PANEL"],
                             activebackground=theme["BG"],
                             activeforeground=theme["ACCENT2"])
    cli_button_row.configure(bg=theme["BG"])
    if theme_var.get() == "light":
        status_bar.configure(bg=theme["STATUS_BG"], fg="#2a2a2a")
    else:
        status_bar.configure(bg=theme["STATUS_BG"], fg=theme["ACCENT2"])
    if theme_var.get() == "light":
        panel_color = theme["BROWN"]
    else:
        panel_color = theme["PANEL"]
    frame.configure(bg=panel_color)
    entry_widget.configure(bg=panel_color,
                           fg=theme["FG"],
                           readonlybackground=panel_color,
                           insertbackground=theme["FG"])
    label_select.configure(bg=panel_color, fg=theme["FG"])
    for btn in (sort_btn, undo_btn, refresh_btn, browse_btn, start_btn, reset_btn):
        btn.config(bg=theme["ACCENT"],
                   fg=theme["FG"],
                   activebackground=theme["ACCENT2"],
                   activeforeground=theme["BG"])
    if theme_var.get() == "light":
        dark_btn.config(bg=theme["PANEL"], fg="#1e1e1e")
        light_btn.config(bg=theme["ACCENT"], fg="#1e1e1e")
    else:
        dark_btn.config(bg=theme["ACCENT"], fg=theme["FG"])
        light_btn.config(bg=theme["PANEL"], fg=theme["FG"])
    if mode_var.get() == "GUI":
        gui_btn.config(bg=theme["ACCENT"], fg=theme["FG"])
        cli_btn.config(bg=theme["PANEL"], fg=theme["FG"])
    else:
        gui_btn.config(bg=theme["PANEL"], fg=theme["FG"])
        cli_btn.config(bg=theme["ACCENT"], fg=theme["FG"])
    for child in flag_frame.winfo_children():
        child.config(bg=theme["BG"], fg=theme["FG"],
                     selectcolor=theme["PANEL"],
                     activebackground=theme["BG"],
                     activeforeground=theme["FG"])
    output.tag_config("error", foreground=theme["ERROR"])
    output.tag_config("dry", foreground=theme["DRY"])
    output.tag_config("progress", foreground=theme["PROGRESS"])
    output.tag_config("verbose", foreground=theme["ACCENT2"])
    output.tag_config("info", foreground=theme["ACCENT2"])
    style.configure("Thin.Horizontal.TProgressbar",
                    troughcolor=theme["PANEL"],
                    background=theme["PROGRESS"])

def set_theme(mode):
    theme_var.set(mode)
    global current_theme
    current_theme = THEMES[mode]
    apply_theme()

def toggle_progress_bar():
    if show_progress_var.get():
        progress_bar.pack(fill=tk.X, padx=8, pady=6)
    else:
        progress_bar.pack_forget()

def run_cmd(cmd, on_done):
    def task():
        process = subprocess.Popen(cmd, stdout=subprocess.PIPE,
                                   stderr=subprocess.STDOUT, text=True, bufsize=1)
        for line in process.stdout:
            tag = None
            if line.startswith("[progress]"):
                if show_progress_var.get():
                    tag = "progress"
                update_progress_from_line(line)
            elif line.startswith("[verbose]"):
                tag = "verbose"
            elif line.startswith("[dry-run]"):
                tag = "dry"
            log(line, tag)
        process.wait()
        root.after(0, on_done)
    threading.Thread(target=task, daemon=True).start()

def run_reset_sequence():
    def task():
        set_status("Resetting build...")
        def run_step(args):
            process = subprocess.Popen(args, stdout=subprocess.PIPE,
                                       stderr=subprocess.STDOUT, text=True, bufsize=1)
            for line in process.stdout:
                cli_log(line)
            process.wait()
        run_step(["stack", "clean"])
        run_step(["stack", "build"])
        run_step(["stack", "run", "tester"])
        root.after(0, lambda: set_status("Reset complete"))
    threading.Thread(target=task, daemon=True).start()

def show_reset_warning():
    if reset_warning_suppressed.get():
        if mode_var.get() != "CLI":
            _do_set_mode_cli()
        cli_terminal.delete("1.0", tk.END)
        run_reset_sequence()
        return
    warning_text = (
        "Reset will run:\n"
        "  stack clean\n"
        "  stack build\n"
        "  stack run tester\n\n"
        "Use this when you have changed Haskell logic (new commands, rules, parsing, "
        "or behavior) and need a fresh build. It is not required for normal preset "
        "or config changes."
    )
    win = tk.Toplevel(root)
    win.title("Reset build")
    win.transient(root)
    win.configure(bg=current_theme["BG"])
    win.grab_set()
    msg = tk.Label(win, text=warning_text, justify="left", wraplength=420, font=LABEL,
                   bg=current_theme["BG"], fg=current_theme["FG"])
    msg.pack(padx=16, pady=(16, 8))
    chk = tk.Checkbutton(win,
                         text="Do not show again this session",
                         variable=reset_warning_suppressed,
                         font=LABEL, anchor="w",
                         bg=current_theme["BG"], fg=current_theme["FG"],
                         selectcolor=current_theme["PANEL"],
                         activebackground=current_theme["BG"],
                         activeforeground=current_theme["FG"])
    chk.pack(fill=tk.X, padx=16, pady=(0, 12))
    btn_frame = tk.Frame(win, bg=current_theme["BG"])
    btn_frame.pack(padx=16, pady=(0, 16))
    def on_cancel():
        win.destroy()
    def on_continue():
        win.destroy()
        if mode_var.get() != "CLI":
            _do_set_mode_cli()
        cli_terminal.delete("1.0", tk.END)
        run_reset_sequence()
    cancel_btn = tk.Button(btn_frame, text="Cancel", width=10, font=LABEL,
                           command=on_cancel,
                           bg=current_theme["ACCENT"], fg=current_theme["FG"],
                           activebackground=current_theme["ACCENT2"],
                           activeforeground=current_theme["BG"],
                           relief=tk.FLAT)
    cancel_btn.pack(side=tk.LEFT, padx=6)
    continue_btn = tk.Button(btn_frame, text="Continue", width=10, font=LABEL,
                             command=on_continue,
                             bg=current_theme["ACCENT"], fg=current_theme["FG"],
                             activebackground=current_theme["ACCENT2"],
                             activeforeground=current_theme["BG"],
                             relief=tk.FLAT)
    continue_btn.pack(side=tk.LEFT, padx=6)

def show_files_recursively(folder):
    output.delete("1.0", tk.END)
    if not os.path.isdir(folder):
        log(f"-- folder does not exist: {folder}\n", "error")
        return
    set_status("Scanning...")
    progress_bar["value"] = 0
    cmd = ["stack", "run", "file-organizer", "--", "--scan"]
    if detailed_var.get():
        cmd.append("--verbose")
    cmd.append(folder)
    run_cmd(cmd, lambda: (set_status("Scan complete"),
                          progress_bar.config(value=100)))

def pick_folder():
    folder = filedialog.askdirectory()
    if folder:
        folder_var.set(folder)
        show_files_recursively(folder)

def run_sort():
    folder = folder_var.get()
    if not folder:
        log("-- please select a folder first\n", "error")
        return
    sort_btn.config(state=tk.DISABLED)
    undo_btn.config(state=tk.DISABLED)
    output.delete("1.0", tk.END)
    log(f"-- sorting {folder}...\n", "info")
    set_status("Sorting...")
    progress_bar["value"] = 0
    cmd = ["stack", "run", "file-organizer", "--", "--no-prompt"]
    if mock_run_var.get():
        cmd.append("--dry-run")
    if detailed_var.get() and not mock_run_var.get():
        cmd.append("--verbose")
    cmd.append(folder)
    def done():
        log("\n-- done.\n", "info")
        set_status("Sort complete")
        if not mock_run_var.get():
            undo_btn.config(state=tk.NORMAL)
        sort_btn.config(state=tk.NORMAL)
        progress_bar.config(value=100)
    run_cmd(cmd, done)

def undo_last_sort():
    folder = folder_var.get()
    if not folder:
        log("-- no folder selected\n", "error")
        return
    undo_btn.config(state=tk.DISABLED)
    log("-- undoing last sort...\n", "info")
    set_status("Undoing...")
    progress_bar["value"] = 0
    cmd = ["stack", "run", "file-organizer", "--", "--undo", folder]
    def done():
        log("-- undo complete.\n", "info")
        set_status("Undo complete")
        progress_bar.config(value=100)
    run_cmd(cmd, done)

def cleanup_backup():
    set_status("Cleaning backup...")
    progress_bar["value"] = 0
    cmd = ["stack", "run", "file-organizer", "--", "--cleanup"]
    def done():
        set_status("Cleanup complete")
        progress_bar.config(value=100)
    run_cmd(cmd, done)

def kill_tester_process():
    global tester_process
    if tester_process and tester_process.poll() is None:
        try:
            tester_process.terminate()
        except Exception:
            pass
    tester_process = None

def on_close():
    if undo_btn["state"] == tk.NORMAL:
        cleanup_backup()
    kill_tester_process()
    root.destroy()

def _do_set_mode_gui():
    mode_var.set("GUI")
    dev_safe_frame.pack_forget()
    cli_terminal.pack_forget()
    cli_button_row.pack_forget()
    gui_container.pack(fill=tk.BOTH, expand=True)
    apply_theme()

def _do_set_mode_cli():
    global current_input
    mode_var.set("CLI")
    gui_container.pack_forget()
    dev_safe_frame.pack(pady=(8, 4))
    cli_terminal.delete("1.0", tk.END)
    current_input = ""
    cli_terminal.pack(fill=tk.BOTH, expand=True, padx=16, pady=(0, 4))
    cli_button_row.pack(pady=(0, 8))
    apply_theme()

def set_mode_gui():
    global tester_process
    if mode_var.get() == "GUI":
        return
    if tester_process and tester_process.poll() is None:
        if not dev_safe_mode_var.get():
            kill_tester_process()
            _do_set_mode_gui()
            return
        win = tk.Toplevel(root)
        win.title("Tester running")
        win.transient(root)
        win.configure(bg=current_theme["BG"])
        win.grab_set()
        msg = tk.Label(win,
                       text="Tester is still running.\nStop it and switch to GUI, or stay in CLI?",
                       justify="left", wraplength=360, font=LABEL,
                       bg=current_theme["BG"], fg=current_theme["FG"])
        msg.pack(padx=16, pady=(16, 12))
        btn_frame = tk.Frame(win, bg=current_theme["BG"])
        btn_frame.pack(padx=16, pady=(0, 16))
        def stop_and_switch():
            kill_tester_process()
            win.destroy()
            _do_set_mode_gui()
        def stay_cli():
            win.destroy()
        stop_btn = tk.Button(btn_frame, text="Stop and switch", width=14, font=LABEL,
                             command=stop_and_switch,
                             bg=current_theme["ACCENT"], fg=current_theme["FG"],
                             activebackground=current_theme["ACCENT2"],
                             activeforeground=current_theme["BG"],
                             relief=tk.FLAT)
        stop_btn.pack(side=tk.LEFT, padx=6)
        stay_btn = tk.Button(btn_frame, text="Stay in CLI", width=12, font=LABEL,
                             command=stay_cli,
                             bg=current_theme["ACCENT"], fg=current_theme["FG"],
                             activebackground=current_theme["ACCENT2"],
                             activeforeground=current_theme["BG"],
                             relief=tk.FLAT)
        stay_btn.pack(side=tk.LEFT, padx=6)
    else:
        _do_set_mode_gui()

def set_mode_cli():
    if mode_var.get() == "CLI":
        return
    _do_set_mode_cli()

def on_mock_run_toggle():
    if mock_run_var.get():
        detailed_var.set(False)
    apply_theme()

def on_detailed_toggle():
    if detailed_var.get():
        mock_run_var.set(False)
    apply_theme()

def themed_check(parent, text, var, command=None):
    return tk.Checkbutton(parent,
                          text=text,
                          variable=var,
                          command=command,
                          bg=current_theme["BG"],
                          fg=current_theme["FG"],
                          selectcolor=current_theme["PANEL"],
                          activebackground=current_theme["BG"],
                          activeforeground=current_theme["FG"],
                          font=LABEL)

def run_tester_cli():
    global tester_process, current_input
    kill_tester_process()
    cli_terminal.delete("1.0", tk.END)
    current_input = ""
    set_status("Tester running...")
    def task():
        global tester_process
        proc = subprocess.Popen(
            ["stack", "run", "tester"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            bufsize=0
        )
        tester_process = proc
        while True:
            ch = proc.stdout.read(1)
            if ch == "" and proc.poll() is not None:
                break
            if ch:
                cli_log(ch)
        proc.wait()
        def done():
            set_status("Tester exited")
        root.after(0, done)
    threading.Thread(target=task, daemon=True).start()

def on_cli_key(event):
    global current_input
    if event.keysym == "BackSpace":
        if current_input:
            current_input = current_input[:-1]
    elif event.keysym == "Return":
        pass
    elif len(event.char) == 1 and event.char.isprintable():
        current_input += event.char
    return None

def send_tester_input(event):
    global tester_process, current_input
    if tester_process and tester_process.stdin:
        text = current_input.strip()
        if text:
            tester_process.stdin.write(text + "\n")
            tester_process.stdin.flush()
    current_input = ""
    return None

root = tk.Tk()
root.title("File Organizer :: Haughskell357")
root.geometry("600x720")
root.resizable(False, False)

folder_var = tk.StringVar()
mock_run_var = tk.BooleanVar(value=False)
detailed_var = tk.BooleanVar(value=False)
show_progress_var = tk.BooleanVar(value=True)
theme_var = tk.StringVar(value="dark")
mode_var = tk.StringVar(value="GUI")
reset_warning_suppressed = tk.BooleanVar(value=False)
dev_safe_mode_var = tk.BooleanVar(value=False)

style = ttk.Style()
style.theme_use("default")
style.configure("Thin.Horizontal.TProgressbar", thickness=4)

top_bar = tk.Frame(root, bg=current_theme["BG"])
top_bar.pack(fill=tk.X, pady=(10, 4), padx=10)

mode_pill = tk.Frame(top_bar, bg=current_theme["PANEL"], bd=1, relief=tk.FLAT)
mode_pill.pack(side=tk.LEFT)

gui_btn = tk.Button(mode_pill, text="GUI", font=LABEL,
                    command=set_mode_gui,
                    relief=tk.FLAT, padx=12, pady=4)
gui_btn.grid(row=0, column=0)

cli_btn = tk.Button(mode_pill, text="CLI", font=LABEL,
                    command=set_mode_cli,
                    relief=tk.FLAT, padx=12, pady=4)
cli_btn.grid(row=0, column=1)

center_spacer = tk.Frame(top_bar, bg=current_theme["BG"])
center_spacer.pack(side=tk.LEFT, expand=True)

theme_frame = tk.Frame(top_bar, bg=current_theme["BG"])
theme_frame.pack(side=tk.RIGHT)

pill = tk.Frame(theme_frame, bg=current_theme["PANEL"], bd=1, relief=tk.FLAT)
pill.pack()

dark_btn = tk.Button(pill, text="Dark", font=LABEL,
                     command=lambda: set_theme("dark"),
                     relief=tk.FLAT, padx=12, pady=4)
dark_btn.grid(row=0, column=0)

light_btn = tk.Button(pill, text="Light", font=LABEL,
                      command=lambda: set_theme("light"),
                      relief=tk.FLAT, padx=12, pady=4)
light_btn.grid(row=0, column=1)

content_frame = tk.Frame(root, bg=current_theme["BG"])
content_frame.pack(fill=tk.BOTH, expand=True)

gui_container = tk.Frame(content_frame, bg=current_theme["BG"])
gui_container.pack(fill=tk.BOTH, expand=True)

label_select = tk.Label(gui_container, text="select a folder to organise:",
                        bg=current_theme["BROWN"], fg=current_theme["FG"],
                        font=LABEL)
label_select.pack(pady=(16, 4))

frame = tk.Frame(gui_container, bg=current_theme["BROWN"], padx=6, pady=6)
frame.pack(padx=16, fill=tk.X)

entry_widget = tk.Entry(frame, textvariable=folder_var, state="readonly",
                        bg=current_theme["BROWN"], fg=current_theme["FG"],
                        readonlybackground=current_theme["BROWN"],
                        insertbackground=current_theme["FG"],
                        relief=tk.FLAT, font=MONO)
entry_widget.pack(side=tk.LEFT, expand=True, fill=tk.X)

browse_btn = tk.Button(frame, text="browse", command=pick_folder,
                       bg=current_theme["ACCENT"], fg=current_theme["FG"],
                       activebackground=current_theme["ACCENT2"],
                       activeforeground=current_theme["BG"],
                       relief=tk.FLAT, font=LABEL, padx=8)
browse_btn.pack(side=tk.LEFT, padx=(8, 0))

flag_frame = tk.Frame(gui_container, bg=current_theme["BG"])
flag_frame.pack(pady=(8, 4))

mock_run_cb = themed_check(flag_frame, "mock run", mock_run_var, on_mock_run_toggle)
mock_run_cb.pack(side=tk.LEFT, padx=10)

detailed_cb = themed_check(flag_frame, "detailed", detailed_var, on_detailed_toggle)
detailed_cb.pack(side=tk.LEFT, padx=10)

themed_check(flag_frame, "show progress", show_progress_var).pack(side=tk.LEFT, padx=10)

progress_frame = tk.Frame(gui_container, bg=current_theme["PANEL"])
progress_frame.pack(fill=tk.X, padx=16, pady=(0, 10))

progress_bar = ttk.Progressbar(progress_frame,
                               orient="horizontal",
                               mode="determinate",
                               style="Thin.Horizontal.TProgressbar")
progress_bar.pack(fill=tk.X, padx=8, pady=6)

button_row = tk.Frame(gui_container, bg=current_theme["BG"])
button_row.pack(pady=(10, 10))

sort_btn = tk.Button(button_row, text="Sort",
                     command=run_sort, width=12,
                     bg=current_theme["ACCENT"], fg=current_theme["FG"],
                     activebackground=current_theme["ACCENT2"],
                     activeforeground=current_theme["BG"],
                     relief=tk.FLAT,
                     font=("Courier New", 11, "bold"), pady=4)
sort_btn.pack(side=tk.LEFT, padx=6)

undo_btn = tk.Button(button_row, text="Undo",
                     command=undo_last_sort, width=12,
                     bg=current_theme["ACCENT"], fg=current_theme["FG"],
                     activebackground=current_theme["ACCENT2"],
                     activeforeground=current_theme["BG"],
                     relief=tk.FLAT,
                     font=("Courier New", 11, "bold"), pady=4,
                     state=tk.DISABLED)
undo_btn.pack(side=tk.LEFT, padx=6)

refresh_btn = tk.Button(button_row, text="Refresh",
                        command=lambda: show_files_recursively(folder_var.get()),
                        width=12,
                        bg=current_theme["ACCENT"], fg=current_theme["FG"],
                        activebackground=current_theme["ACCENT2"],
                        activeforeground=current_theme["BG"],
                        relief=tk.FLAT,
                        font=("Courier New", 11, "bold"), pady=4)
refresh_btn.pack(side=tk.LEFT, padx=6)

output = scrolledtext.ScrolledText(gui_container,
                                   wrap=tk.WORD,
                                   font=MONO,
                                   height=18,
                                   bg=current_theme["PANEL"],
                                   fg=current_theme["FG"],
                                   insertbackground=current_theme["FG"],
                                   relief=tk.FLAT)
output.pack(fill=tk.BOTH, expand=True, padx=16, pady=(0, 10))

dev_safe_frame = tk.Frame(content_frame, bg=current_theme["BG"])
dev_safe_check = tk.Checkbutton(dev_safe_frame,
                                text="Dev Safe Mode",
                                variable=dev_safe_mode_var,
                                bg=current_theme["BG"],
                                fg=current_theme["ACCENT2"],
                                selectcolor=current_theme["PANEL"],
                                activebackground=current_theme["BG"],
                                activeforeground=current_theme["ACCENT2"],
                                font=LABEL)
dev_safe_check.pack()

cli_terminal = scrolledtext.ScrolledText(content_frame,
                                         wrap=tk.WORD,
                                         font=MONO,
                                         height=24,
                                         bg=current_theme["PANEL"],
                                         fg=current_theme["FG"],
                                         insertbackground=current_theme["FG"],
                                         relief=tk.FLAT)
cli_terminal.pack_forget()
cli_terminal.bind("<Key>", on_cli_key)
cli_terminal.bind("<Return>", send_tester_input)

cli_button_row = tk.Frame(content_frame, bg=current_theme["BG"])
start_btn = tk.Button(cli_button_row, text="Start",
                      command=run_tester_cli, width=12,
                      bg=current_theme["ACCENT"], fg=current_theme["FG"],
                      activebackground=current_theme["ACCENT2"],
                      activeforeground=current_theme["BG"],
                      relief=tk.FLAT,
                      font=("Courier New", 11, "bold"), pady=4)
start_btn.pack(side=tk.LEFT, padx=6)
reset_btn = tk.Button(cli_button_row, text="Reset",
                      command=show_reset_warning, width=12,
                      bg=current_theme["ACCENT"], fg=current_theme["FG"],
                      activebackground=current_theme["ACCENT2"],
                      activeforeground=current_theme["BG"],
                      relief=tk.FLAT,
                      font=("Courier New", 11, "bold"), pady=4)
reset_btn.pack(side=tk.LEFT, padx=6)
cli_button_row.pack_forget()

status_var = tk.StringVar(value="Ready")
status_bar = tk.Label(root,
                      textvariable=status_var,
                      anchor="w",
                      bg=current_theme["STATUS_BG"],
                      fg=current_theme["ACCENT2"],
                      font=LABEL,
                      padx=10)
status_bar.pack(fill=tk.X, side=tk.BOTTOM)

apply_theme()
root.protocol("WM_DELETE_WINDOW", on_close)
root.mainloop()
