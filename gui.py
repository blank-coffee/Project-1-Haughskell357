import tkinter as tk
from tkinter import filedialog, scrolledtext, messagebox
from tkinter import ttk
import subprocess
import threading
import os
import sys
import shutil
import time

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
        "BROWN": "#4a3f32",
        "OP": "#c792ea",     
        "VERBOSE": "#82aaff",  
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
        "BROWN": "#d2c4b2",
        "OP": "#7b3fbf", 
        "VERBOSE": "#005fcc",  
    }
}

MONO = ("Courier New", 10)
LABEL = ("Courier New", 10)

current_theme = THEMES["dark"]
tester_process = None
gui_process = None  
current_input = ""
command_history = []
history_index = None
current_view = "logs"  

ANSI_FG_COLORS = {
    "30": "#000000",
    "31": "#ff6b6b",
    "32": "#4caf50",
    "33": "#d6cf70",
    "34": "#6aa0ff",
    "35": "#8c75b8",
    "36": "#4dd0e1",
    "37": "#d6cff0",
}


ansi_buffer = ""

def insert_ansi(widget, text):
    global ansi_buffer

   
    text = ansi_buffer + text
    ansi_buffer = ""

    i = 0
    current_tags = []
    n = len(text)

    while i < n:
        ch = text[i]

    
        if ch == "\x1b" and i + 1 < n and text[i + 1] == "[":
            j = i + 2

     
            while j < n and text[j] != "m":
                j += 1

         
            if j >= n:
                ansi_buffer = text[i:]
                return

            seq = text[i + 2:j]
            codes = seq.split(";") if seq else []

            if "0" in codes or not codes:
                current_tags = []
            else:
                for code in codes:
                    if code in ANSI_FG_COLORS:
                        current_tags = [t for t in current_tags if not t.startswith("ansi_fg_")]
                        current_tags.append(f"ansi_fg_{code}")
                    elif code == "1":
                        if "ansi_bold" not in current_tags:
                            current_tags.append("ansi_bold")

            i = j + 1
            continue

       
        if current_tags:
            widget.insert(tk.END, ch, tuple(current_tags))
        else:
            widget.insert(tk.END, ch)

        i += 1


import re
ANSI_RE = re.compile(r'\x1b\[[0-9;]*m')

def strip_ansi(s):
    return ANSI_RE.sub("", s)


def _clean_text(text: str) -> str:
    clean = text.replace("\r", "\n")
    clean = "\n".join([line for line in clean.split("\n") if line.strip() != ""])
    return clean

def log(text, tag=None):
    clean = strip_ansi(_clean_text(text))
    if not clean:
        return
    output.config(state="normal")
    if tag:
        output.insert(tk.END, clean + "\n", tag)
    else:
        output.insert(tk.END, clean + "\n")
    output.config(state="disabled")
    output.see(tk.END)




def cli_log(text):
    clean = _clean_text(text)
    if not clean:
        return
    insert_ansi(cli_terminal, clean + "\n")
    cli_terminal.see(tk.END)


def set_status(text):
    status_var.set(text)
    lower = text.lower()
    if "error" in lower or "fail" in lower:
        status_bar.config(fg=current_theme["ERROR"])
    elif any(k in lower for k in ["running", "sorting", "scanning", "undo", "cleaning", "reset"]):
        status_bar.config(fg=current_theme["PROGRESS"])
    else:
        status_bar.config(fg=current_theme["ACCENT2"])


def update_progress_from_line(line):
    try:
        parts = line.strip().split()
        if len(parts) < 3:
            return
        phase = parts[1]
        cur, total = map(int, parts[2].split("/"))
        progress_bar["value"] = (cur / total) * 100
        set_status(f"{phase.capitalize()} {cur}/{total}")
    except Exception:
        pass


def _update_toggle_button_label():
    if current_view == "logs":
        toggle_view_btn.config(text="Show structure")
    else:
        toggle_view_btn.config(text="Show logs")


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
    view_stack.configure(bg=theme["BG"])
    output.configure(bg=theme["PANEL"], fg=theme["FG"], insertbackground=theme["FG"])
    explorer_frame.configure(bg=theme["BG"])
    center_spacer.configure(bg=theme["BG"])
    dev_safe_frame.configure(bg=theme["BG"])
    dev_safe_check.configure(bg=theme["BG"], fg=theme["ACCENT2"],
                             selectcolor=theme["PANEL"],
                             activebackground=theme["BG"],
                             activeforeground=theme["ACCENT2"])
    cli_button_row.configure(bg=theme["BG"])

    if theme_var.get() == "light":
        status_bar.configure(bg=theme["STATUS_BG"], fg="#2a2a2a")
        panel_color = theme["BROWN"]
    else:
        status_bar.configure(bg=theme["STATUS_BG"], fg=theme["ACCENT2"])
        panel_color = theme["PANEL"]

    frame.configure(bg=panel_color)
    entry_widget.configure(bg=panel_color,
                           fg=theme["FG"],
                           readonlybackground=panel_color,
                           insertbackground=theme["FG"])
    label_select.configure(bg=panel_color, fg=theme["FG"])

    for btn in (
        sort_btn, undo_btn, refresh_btn, browse_btn,
        start_btn, reset_btn, toggle_view_btn,
        light_btn, dark_btn
    ):
        btn.config(bg=theme["ACCENT"],
                   fg=theme["FG"],
                   activebackground=theme["ACCENT2"],
                   activeforeground=theme["BG"])

    if mode_var.get() == "GUI":
        gui_btn.config(bg=theme["ACCENT"], fg=theme["FG"])
        cli_btn.config(bg=theme["PANEL"], fg=theme["FG"])
    else:
        gui_btn.config(bg=theme["PANEL"], fg=theme["FG"])
        cli_btn.config(bg=theme["ACCENT"], fg=theme["FG"])
    if theme_var.get() == "dark":
        dark_btn.config(bg=theme["ACCENT"], fg=theme["FG"])
        light_btn.config(bg=theme["PANEL"], fg=theme["FG"])
    else:
        light_btn.config(bg=theme["ACCENT"], fg=theme["FG"])
        dark_btn.config(bg=theme["PANEL"], fg=theme["FG"])


    for child in flag_frame.winfo_children():
        if isinstance(child, tk.Checkbutton):
            child.config(bg=theme["BG"], fg=theme["FG"],
                         selectcolor=theme["PANEL"],
                         activebackground=theme["BG"],
                         activeforeground=theme["FG"])
        else:
            child.config(bg=theme["BG"], fg=theme["FG"],
                         activebackground=theme["BG"],
                         activeforeground=theme["FG"])

    output.tag_config("error", foreground=theme["ERROR"])
    output.tag_config("dry", foreground=theme["DRY"])
    output.tag_config("progress", foreground=theme["PROGRESS"])
    output.tag_config("verbose", foreground=theme["VERBOSE"])
    output.tag_config("info", foreground=theme["ACCENT2"])
    output.tag_config("op", foreground=current_theme["OP"])

    toggle_view_btn.config(
    bg=theme["ACCENT"],
    fg=theme["FG"],
    activebackground=theme["ACCENT2"],
    activeforeground=theme["BG"]
    )

    style.configure("Thin.Horizontal.TProgressbar",
                    troughcolor=theme["PANEL"],
                    background=theme["PROGRESS"])

    style.configure("Treeview",
                    background=theme["PANEL"],
                    foreground=theme["FG"],
                    fieldbackground=theme["PANEL"],
                    bordercolor=theme["ACCENT"],
                    borderwidth=0)
    style.map("Treeview",
              background=[("selected", theme["ACCENT2"])],
              foreground=[("selected", theme["BG"])])

    cli_terminal.configure(bg=theme["PANEL"],
                           fg=theme["FG"],
                           insertbackground=theme["FG"])
    for code, color in ANSI_FG_COLORS.items():
        cli_terminal.tag_config(f"ansi_fg_{code}", foreground=color)
    cli_terminal.tag_config("ansi_bold", font=("Courier New", 10, "bold"))
    #for code, color in ANSI_FG_COLORS.items():
     #   output.tag_config(f"ansi_fg_{code}", foreground=color)

    #output.tag_config("ansi_bold", font=("Courier New", 10, "bold"))


    _update_toggle_button_label()
    cli_terminal.update_idletasks()


def set_theme(mode):
    theme_var.set(mode)
    global current_theme
    current_theme = THEMES[mode]
    apply_theme()


def kill_tester_process(block=False):
    global tester_process
    if tester_process and tester_process.poll() is None:
        try:
            tester_process.terminate()
            if block:
                try:
                    tester_process.wait(timeout=2)
                except subprocess.TimeoutExpired:
                    tester_process.kill()
                    tester_process.wait(timeout=1)
        except Exception:
            pass
    tester_process = None

def kill_stray_tester_exe():
    if sys.platform.startswith("win"):
        try:
            subprocess.run(
                ["taskkill", "/IM", "tester.exe", "/F"],
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
                check=False,
            )
        except Exception:
            pass


def kill_gui_process(block=False):
    global gui_process
    proc = gui_process
    if proc and proc.poll() is None:
        try:
            proc.terminate()
            if block:
                try:
                    proc.wait(timeout=2)
                except subprocess.TimeoutExpired:
                    proc.kill()
                    proc.wait(timeout=1)
        except Exception:
            pass
    gui_process = None


def run_cmd(cmd, on_done):
    def task():
        global gui_process
        creationflags = subprocess.CREATE_NEW_PROCESS_GROUP if sys.platform.startswith("win") else 0

        proc = subprocess.Popen(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            bufsize=1,
            start_new_session=True,
            creationflags=creationflags
        )
        gui_process = proc

        while True:
            line = proc.stdout.readline()

            if line:
                clean = strip_ansi(line)

                tag = None
                if clean.startswith("[progress]"):
                    tag = "progress"
                    update_progress_from_line(clean)
                elif clean.startswith("[verbose]"):
                    tag = "verbose"
                elif clean.startswith("[dry-run]"):
                    tag = "dry"
                elif clean.startswith("[error]"):
                    tag = "error"
                elif clean.startswith("[info]"):
                    tag = "info"
                elif clean.startswith("[op]"):
                    tag = "op"

                log(clean, tag)

            else:
                if proc.poll() is not None:
                    break
                time.sleep(0.01)
                continue

        try:
            proc.wait()
        except Exception:
            pass

        gui_process = None
        root.after(0, on_done)

    threading.Thread(target=task, daemon=True).start()







def run_reset_sequence():
    def task():
        kill_tester_process(block=True)
        kill_gui_process(block=True)
        kill_stray_tester_exe()
        set_status("Resetting build...")

        def run_step(args):
            creationflags = subprocess.CREATE_NEW_PROCESS_GROUP if sys.platform.startswith("win") else 0
            proc = subprocess.Popen(
                args,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                text=True,
                bufsize=1,
                start_new_session=True,
                creationflags=creationflags
            )
            for line in proc.stdout:
                cli_log(line)
            proc.wait()

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
    kill_gui_process()
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


def populate_tree(parent, path):
    try:
        entries = sorted(os.listdir(path))
    except Exception:
        return
    for name in entries:
        full = os.path.join(path, name)
        node = explorer_tree.insert(parent, "end", text=name, values=(full,))
        if os.path.isdir(full):
            populate_tree(node, full)


def refresh_tree():
    kill_gui_process()
    explorer_tree.delete(*explorer_tree.get_children())
    folder = folder_var.get()
    if folder and os.path.isdir(folder):
        root_node = explorer_tree.insert("", "end", text=folder, open=True, values=(folder,))
        populate_tree(root_node, folder)


def pick_folder():
    folder = filedialog.askdirectory()
    if folder:
        folder_var.set(folder)
        show_files_recursively(folder)
        refresh_tree()


def run_sort():
    kill_gui_process()
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
        refresh_tree()
        if not mock_run_var.get():
            undo_btn.config(state=tk.NORMAL)
        sort_btn.config(state=tk.NORMAL)
        progress_bar.config(value=100)

    run_cmd(cmd, done)


def undo_last_sort():
    kill_gui_process()
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
        refresh_tree()
        progress_bar.config(value=100)

    run_cmd(cmd, done)


def cleanup_backup():
    kill_gui_process()
    set_status("Cleaning backup...")
    progress_bar["value"] = 0
    cmd = ["stack", "run", "file-organizer", "--", "--cleanup"]

    def done():
        set_status("Cleanup complete")
        refresh_tree()
        progress_bar.config(value=100)

    run_cmd(cmd, done)


def on_close():
    if undo_btn["state"] == tk.NORMAL:
        cleanup_backup()
    kill_tester_process()
    kill_gui_process()
    root.destroy()


def _do_set_mode_gui():
    kill_gui_process(block=True)
    kill_tester_process()
    mode_var.set("GUI")
    dev_safe_frame.pack_forget()
    cli_terminal.pack_forget()
    cli_button_row.pack_forget()
    gui_container.pack(fill=tk.BOTH, expand=True)
    apply_theme()
    set_status("Ready")


def _do_set_mode_cli():
    kill_gui_process(block=True)
    kill_tester_process()
    global current_input, command_history, history_index
    mode_var.set("CLI")
    gui_container.pack_forget()
    dev_safe_frame.pack(pady=(8, 4))
    cli_terminal.delete("1.0", tk.END)
    current_input = ""
    command_history = []
    history_index = None
    cli_terminal.pack(fill=tk.BOTH, expand=True, padx=16, pady=(0, 4))
    cli_button_row.pack(pady=(0, 8))
    apply_theme()
    if not tester_process or tester_process.poll() is not None:
        run_tester_cli()


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
    global tester_process, current_input, command_history, history_index
    kill_gui_process()
    kill_tester_process()
    kill_stray_tester_exe()
    cli_terminal.delete("1.0", tk.END)
    current_input = ""
    command_history = []
    history_index = None
    set_status("Tester running...")

    def task():
        global tester_process
        proc = subprocess.Popen(
            ["stack", "run", "tester"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            bufsize=1
        )
        tester_process = proc
        for line in proc.stdout:
            if not line:
                break
            cli_log(line)
        proc.wait()

        def done():
            set_status("Tester exited")

        root.after(0, done)

    threading.Thread(target=task, daemon=True).start()


def send_tester_input():
    global tester_process, current_input, command_history, history_index
    text = current_input.strip()
    if tester_process and tester_process.stdin and text:
        tester_process.stdin.write(text + "\n")
        tester_process.stdin.flush()
        cli_terminal.insert("end", "\n")
        cli_terminal.see("end")
        if not command_history or command_history[-1] != text:
            command_history.append(text)
        history_index = None
    current_input = ""


def _replace_current_input(new_text: str):
    global current_input
    cli_terminal.mark_set("insert", "end-1c")
    if current_input:
        cli_terminal.delete(f"end-{len(current_input)+1}c", "end-1c")
    current_input = new_text
    cli_terminal.insert("end", new_text)
    cli_terminal.see("end")


def on_cli_key(event):
    global current_input, history_index
    cli_terminal.mark_set("insert", "end-1c")
    if event.keysym == "BackSpace":
        if current_input:
            current_input = current_input[:-1]
            cli_terminal.delete("end-2c", "end-1c")
        return "break"
    if event.keysym in ("Return", "KP_Enter"):
        send_tester_input()
        return "break"
    if event.keysym == "Up":
        if command_history:
            if history_index is None:
                history_index = len(command_history) - 1
            else:
                history_index = max(0, history_index - 1)
            _replace_current_input(command_history[history_index])
        return "break"
    if event.keysym == "Down":
        if command_history and history_index is not None:
            if history_index < len(command_history) - 1:
                history_index += 1
                _replace_current_input(command_history[history_index])
            else:
                history_index = None
                _replace_current_input("")
        return "break"
    if len(event.char) == 1 and event.char.isprintable():
        current_input += event.char
        cli_terminal.insert("end", event.char)
        cli_terminal.see("end")
        return "break"
    return "break"


def toggle_view():
    global current_view
    if current_view == "logs":
        output.pack_forget()
        explorer_frame.pack(fill=tk.BOTH, expand=True)
        current_view = "structure"
    else:
        explorer_frame.pack_forget()
        output.pack(fill=tk.BOTH, expand=True)
        current_view = "logs"
    _update_toggle_button_label()


def _get_selected_path():
    sel = explorer_tree.selection()
    if not sel:
        return None
    item = sel[0]
    vals = explorer_tree.item(item, "values")
    if not vals:
        return None
    return vals[0]


def _open_path(path):
    if not path:
        return
    try:
        if sys.platform.startswith("win"):
            os.startfile(path)
        elif sys.platform == "darwin":
            subprocess.Popen(["open", path])
        else:
            subprocess.Popen(["xdg-open", path])
    except Exception as e:
        messagebox.showerror("Open error", f"Could not open:\n{path}\n\n{e}")


def on_tree_double_click(event):
    path = _get_selected_path()
    if path:
        _open_path(path)


def _copy_to_clipboard(text):
    if not text:
        return
    root.clipboard_clear()
    root.clipboard_append(text)


def _rename_path(path):
    parent = os.path.dirname(path)
    old_name = os.path.basename(path)

    win = tk.Toplevel(root)
    win.title("Rename")
    win.transient(root)
    win.configure(bg=current_theme["BG"])
    win.grab_set()

    tk.Label(win, text="New name:", bg=current_theme["BG"],
             fg=current_theme["FG"], font=LABEL).pack(padx=12, pady=(12, 4))
    name_var = tk.StringVar(value=old_name)
    entry = tk.Entry(win, textvariable=name_var, font=MONO)
    entry.pack(padx=12, pady=(0, 8))
    entry.focus_set()

    btn_frame = tk.Frame(win, bg=current_theme["BG"])
    btn_frame.pack(padx=12, pady=(0, 12))

    def do_rename():
        new_name = name_var.get().strip()
        if not new_name or new_name == old_name:
            win.destroy()
            return
        new_path = os.path.join(parent, new_name)
        try:
            os.rename(path, new_path)
            refresh_tree()
            win.destroy()
        except Exception as e:
            messagebox.showerror("Rename error", f"Could not rename:\n{path}\n\n{e}")

    tk.Button(btn_frame, text="OK", width=8, command=do_rename,
              bg=current_theme["ACCENT"], fg=current_theme["FG"],
              activebackground=current_theme["ACCENT2"],
              activeforeground=current_theme["BG"],
              relief=tk.FLAT, font=LABEL).pack(side=tk.LEFT, padx=4)
    tk.Button(btn_frame, text="Cancel", width=8, command=win.destroy,
              bg=current_theme["ACCENT"], fg=current_theme["FG"],
              activebackground=current_theme["ACCENT2"],
              activeforeground=current_theme["BG"],
              relief=tk.FLAT, font=LABEL).pack(side=tk.LEFT, padx=4)


def _delete_path(path):
    if not messagebox.askyesno("Delete", f"Delete this item?\n\n{path}"):
        return
    try:
        if os.path.isdir(path):
            shutil.rmtree(path)
        else:
            os.remove(path)
        refresh_tree()
    except Exception as e:
        messagebox.showerror("Delete error", f"Could not delete:\n{path}\n\n{e}")


def on_tree_right_click(event):
    iid = explorer_tree.identify_row(event.y)
    if iid:
        explorer_tree.selection_set(iid)
    path = _get_selected_path()
    if not path:
        return
    menu = tk.Menu(root, tearoff=0)
    menu.add_command(label="Open", command=lambda: _open_path(path))
    menu.add_command(label="Copy path", command=lambda: _copy_to_clipboard(path))
    menu.add_command(label="Copy name", command=lambda: _copy_to_clipboard(os.path.basename(path)))
    menu.add_separator()
    menu.add_command(label="Rename", command=lambda: _rename_path(path))
    menu.add_command(label="Delete", command=lambda: _delete_path(path))
    menu.tk_popup(event.x_root, event.y_root)


root = tk.Tk()
root.title("File Organizer :: Haughskell357")
root.geometry("600x720")
root.resizable(False, False)

folder_var = tk.StringVar()
mock_run_var = tk.BooleanVar(value=False)
detailed_var = tk.BooleanVar(value=False)
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

toggle_view_btn = tk.Button(
    flag_frame,
    text="Show structure",
    command=toggle_view,
    relief=tk.FLAT,
    bg=current_theme["ACCENT"],
    fg=current_theme["FG"],
    activebackground=current_theme["ACCENT2"],
    activeforeground=current_theme["BG"],
    font=LABEL,
    padx=12,
    pady=4,
    borderwidth=0,
    highlightthickness=0
)
toggle_view_btn.pack(side=tk.LEFT, padx=10)

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
                        command=lambda: (show_files_recursively(folder_var.get()), refresh_tree()),
                        width=12,
                        bg=current_theme["ACCENT"], fg=current_theme["FG"],
                        activebackground=current_theme["ACCENT2"],
                        activeforeground=current_theme["BG"],
                        relief=tk.FLAT,
                        font=("Courier New", 11, "bold"), pady=4)
refresh_btn.pack(side=tk.LEFT, padx=6)

view_stack = tk.Frame(gui_container, bg=current_theme["BG"])
view_stack.pack(fill=tk.BOTH, expand=True, padx=16, pady=(0, 10))

output = scrolledtext.ScrolledText(view_stack,
                                   wrap=tk.WORD,
                                   font=MONO,
                                   height=18,
                                   bg=current_theme["PANEL"],
                                   fg=current_theme["FG"],
                                   insertbackground=current_theme["FG"],
                                   relief=tk.FLAT)
output.config(state="disabled")
output.pack(fill=tk.BOTH, expand=True)

explorer_frame = tk.Frame(view_stack, bg=current_theme["BG"])
explorer_tree = ttk.Treeview(explorer_frame, show="tree", columns=("fullpath",))
explorer_tree.heading("#0", text="Name", anchor="w")
explorer_tree.column("#0", anchor="w")
explorer_tree.column("fullpath", width=0, stretch=False)
explorer_tree.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
explorer_scroll = ttk.Scrollbar(explorer_frame, orient="vertical", command=explorer_tree.yview)
explorer_tree.configure(yscrollcommand=explorer_scroll.set)
explorer_scroll.pack(side=tk.RIGHT, fill=tk.Y)
explorer_frame.pack_forget()

explorer_tree.bind("<Double-1>", on_tree_double_click)
explorer_tree.bind("<Button-3>", on_tree_right_click)

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