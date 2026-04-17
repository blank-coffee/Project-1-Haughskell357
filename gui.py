import tkinter as tk
from tkinter import filedialog, scrolledtext
import subprocess
import threading

# Haskell brand palette
BG       = "#1c1b2e"   # deep dark purple
PANEL    = "#2a2740"   # slightly lighter for frames
PURPLE   = "#5e5086"   # Haskell logo purple
LAVENDER = "#8c75b8"   # lighter accent
FG       = "#d6cff0"   # soft lavender white
MONO     = ("Courier New", 10)
LABEL    = ("Courier New", 10)

def pick_folder():
    folder = filedialog.askdirectory()
    if folder:
        folder_var.set(folder)

def run_sort():
    folder = folder_var.get()
    if not folder:
        output.insert(tk.END, "-- please select a folder first\n")
        return

    sort_btn.config(state=tk.DISABLED)
    output.delete("1.0", tk.END)
    output.insert(tk.END, f"-- sorting {folder}...\n")

    def task():
        result = subprocess.run(
            ["stack", "run", "--", folder],
            capture_output=True, text=True
        )
        output.insert(tk.END, result.stdout or result.stderr)
        output.insert(tk.END, "\n-- done.\n")
        sort_btn.config(state=tk.NORMAL)

    threading.Thread(target=task, daemon=True).start()

root = tk.Tk()
root.title("File Organizer :: Haughskell357")
root.geometry("600x420")
root.resizable(False, False)
root.configure(bg=BG)

folder_var = tk.StringVar()

tk.Label(
    root, text="select a folder to organise:",
    bg=BG, fg=LAVENDER, font=LABEL
).pack(pady=(16, 4))

frame = tk.Frame(root, bg=PANEL, padx=6, pady=6)
frame.pack(padx=16, fill=tk.X)

tk.Entry(
    frame, textvariable=folder_var, state="readonly",
    bg=PANEL, fg=FG, readonlybackground=PANEL,
    insertbackground=FG, relief=tk.FLAT, font=MONO
).pack(side=tk.LEFT, expand=True, fill=tk.X)

tk.Button(
    frame, text="browse", command=pick_folder,
    bg=PURPLE, fg=FG, activebackground=LAVENDER, activeforeground=BG,
    relief=tk.FLAT, font=LABEL, padx=8
).pack(side=tk.LEFT, padx=(8, 0))

sort_btn = tk.Button(
    root, text="sort files", command=run_sort, width=20,
    bg=PURPLE, fg=FG, activebackground=LAVENDER, activeforeground=BG,
    relief=tk.FLAT, font=("Courier New", 11, "bold"), pady=6
)
sort_btn.pack(pady=12)

output = scrolledtext.ScrolledText(
    root, height=14,
    bg=PANEL, fg=FG, insertbackground=FG,
    font=MONO, relief=tk.FLAT, padx=6, pady=6
)
output.pack(padx=16, pady=(0, 16), fill=tk.BOTH, expand=True)

root.mainloop()
