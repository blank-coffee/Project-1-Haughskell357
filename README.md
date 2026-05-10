# Project-1-Haughskell357
Group project file organizer

A cross‑platform file‑organization system built around a Haskell backend and a Python/Tkinter frontend.  
This document describes the internal architecture, invariants, workflows, and implementation details required for development and maintenance.

---

## 1. Architecture Overview

The system consists of three major components:

### **1.1 Haskell Backend**
Located in `src/`:

- **FileOrganizer.hs**  
  Implements the file‑organization engine. Supports:
  - `--scan`
  - `--verbose`
  - `--dry-run`
  - `--undo`
  - `--cleanup`
  - `--no-prompt`

- **Tester.hs**  
  An interactive REPL‑style tester used for:
  - Creating test roots
  - Running vary prompts
  - Validating new logic before integration
  - Exercising organizer behavior in isolation

The backend emits structured tags such as:
[progress] scan 3/10
[verbose] ...
[dry-run] ...


The GUI parses these tags to drive progress bars and color‑coded output.

---

## 2. Python/Tkinter GUI

The GUI (`gui.py`) is a full application providing:

- GUI mode (folder selection, sort, undo, refresh)
- Integrated CLI mode (embedded tester)
- Theme system (light/dark)
- Progress bar with live updates
- Dev Safe Mode (prevents accidental tester termination)
- Themed modal dialogs
- Color‑coded output tags
- Automatic cleanup on exit

The GUI is intentionally **stateless** regarding file operations — all logic is delegated to the Haskell backend.

---

## 3. Mode System

The application supports two modes:

### **3.1 GUI Mode**
Primary interface for end‑users.

Features:
- Folder selection
- Sort execution
- Undo
- Refresh
- Progress bar
- Output panel
- Theme switching

### **3.2 CLI Mode**
Embeds the Haskell tester inside the GUI.

Features:
- Real‑time streaming output
- Keystroke‑based input buffer (no separate input widget)
- Terminal auto‑clears when switching into CLI mode
- Tester lifecycle is fully controlled by the GUI

---

## 4. Tester Lifecycle

The GUI manages the tester process with strict invariants:

### **4.1 Process Ownership**
- Only one tester process may exist at a time.
- `tester_process` is the single source of truth.
- All termination flows route through `kill_tester_process()`.

### **4.2 Startup**
`run_tester_cli()`:
- Kills any existing tester
- Clears terminal
- Resets input buffer
- Spawns tester with `bufsize=0` for unbuffered output
- Streams output one character at a time

### **4.3 Input Handling**
The GUI maintains a **separate input buffer**:

- Keystrokes update `current_input`
- Output is inserted into the terminal normally
- On Enter:
  - The buffer is sent to tester stdin
  - The buffer is cleared
  - The terminal visually shows the newline

This avoids scraping mixed output/input from the widget.

### **4.4 Shutdown**
Occurs when:
- User switches modes (Dev Safe Mode off)
- User confirms shutdown (Dev Safe Mode on)
- Tester exits naturally
- Application closes

---

## 5. Dev Safe Mode

A development‑focused safety feature.

### **Behavior**
When enabled:
- Switching from CLI → GUI while tester is running triggers a modal dialog:
  - **Stop and switch**
  - **Stay in CLI**

When disabled:
- Tester is terminated immediately on mode switch.

### **Rationale**
Prevents accidental termination of long‑running tester sessions during development.

---

## 6. Theme System

Two themes:

### **Dark Mode**

BG:      #1c1b2e
PANEL:   #2a2740
FG:      #d6cff0
ACCENT:  #5e5086
ACCENT2: #8c75b8
ERROR:   #ff6b6b


### **Light Mode**

BG:      #e7e6e1
PANEL:   #f4f3ef
FG:      #3a3a38
ACCENT:  #b8a76f
ACCENT2: #d1c48a
ERROR:   #b84a4a


### **Popup Theming**
All modal dialogs use:

bg = current_theme["BG"]
fg = current_theme["FG"]


This prevents white flashes in dark mode.

---

## 7. Progress Reporting

The GUI listens for backend tags:

[progress] <phase> <cur>/<total>


The parser:
- Extracts phase
- Computes percentage
- Updates progress bar
- Updates status bar

Progress bar uses a custom thin style for visual consistency.

---

## 8. Undo & Cleanup

The GUI exposes:

### **Undo**

stack run file-organizer -- --undo <folder>

### **Cleanup**

stack run file-organizer -- --cleanup


Undo is only enabled after a successful non‑dry‑run sort.

---

## 9. Reset Workflow

The **Reset** button runs:

stack clean
stack build
stack run tester


Used when:
- Haskell logic changes
- New commands are added
- Parsing or behavior changes require a rebuild

Not required for preset/config changes.

---


## 10. Development Notes

### **10.1 Process Safety**
- Never modify `tester_process` outside `kill_tester_process()` or tester startup.
- Always use local `proc` inside reader threads to avoid race conditions.

### **10.2 Terminal Behavior**
- The terminal is a single widget.
- Output and input coexist visually.
- Input is tracked separately to avoid scraping mixed content.

### **10.3 Mode Switching**
- `_do_set_mode_cli()` always clears the terminal.
- `_do_set_mode_gui()` hides CLI widgets cleanly.

### **10.4 Error Handling**
- Output tags (`error`, `dry`, `verbose`, etc.) are color‑coded.
- Backend errors are displayed directly in the terminal or output panel.

---

## 11. Current Status

The system now supports:

- Stable tester lifecycle  
- No duplicate processes  
- Clean mode switching  
- Reliable input handling  
- Themed popups  
- Accurate progress parsing  
- Fully functional GUI + CLI modes  
- Developer‑safe workflows  



