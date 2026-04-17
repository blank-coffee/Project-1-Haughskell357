import tkinter as tk
from tkinter import filedialog, scrolledtext
import subprocess
import threading

def pick_folder():
    folder = filedialog.askdirectory()
    if folder:
        folder_var.set(folder)

def run_sort():
    folder = folder_var.get()
    if not folder:
        output.insert(tk.END, "Please select a folder first.\n")
        return

    sort_btn.config(state=tk.DISABLED)
    output.delete("1.0", tk.END)
    output.insert(tk.END, f"Sorting {folder}...\n")

    def task():
        result = subprocess.run(
            ["stack", "run", "--", folder],
            capture_output=True, text=True
        )
        output.insert(tk.END, result.stdout or result.stderr)
        output.insert(tk.END, "\nDone.\n")
        sort_btn.config(state=tk.NORMAL)

    threading.Thread(target=task, daemon=True).start()

root = tk.Tk()
root.title("File Organizer")
root.geometry("600x400")
root.resizable(False, False)

folder_var = tk.StringVar()

tk.Label(root, text="Select a folder to organize:").pack(pady=(16, 4))

frame = tk.Frame(root)
frame.pack(padx=16, fill=tk.X)
tk.Entry(frame, textvariable=folder_var, state="readonly").pack(side=tk.LEFT, expand=True, fill=tk.X)
tk.Button(frame, text="Browse", command=pick_folder).pack(side=tk.LEFT, padx=(8, 0))

sort_btn = tk.Button(root, text="Sort Files", command=run_sort, width=20)
sort_btn.pack(pady=12)

output = scrolledtext.ScrolledText(root, height=14, state=tk.NORMAL)
output.pack(padx=16, pady=(0, 16), fill=tk.BOTH, expand=True)

root.mainloop()
