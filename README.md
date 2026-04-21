# emacs-config

My personal Emacs configuration. Minimal, fast, and built around the modern built-in stack (`use-package`, Eglot, Flymake, `project.el`) plus the Vertico/Consult/Embark/Corfu ecosystem. Evil-first editing, with first-class support for Guile Scheme and Guix.

## Highlights

- **Completion UI** — Vertico + Orderless + Marginalia + Consult + Embark for a fast, composable minibuffer.
- **In-buffer completion** — Corfu with history and popup info.
- **Editing** — Evil (with `evil-leader`, `evil-commentary`, `evil-org`), Smartparens in paredit mode, YASnippet.
- **LSP & diagnostics** — Eglot on demand, Flymake with the `flymake-flycheck` bridge, Eldoc-box for popup docs.
- **Notes** — Org-roam with a `daily/` journal and Consult integration.
- **Languages** — Scheme/Guile via Geiser, Go (`go-ts-mode`, format on save), JavaScript, PlantUML, plus generic tree-sitter.
- **Guix-friendly** — Picks up `emacs-guix`, sets the Guile load path for Geiser/Flycheck, and reads a local `guix-config.el` for system-specific paths (e.g. the PlantUML jar).
- **Clean UI** — `ef-winter` theme, no menu/tool/scroll bars, maximized undecorated frame, transparent background toggle, Noto Sans Mono 18.

## Requirements

- Emacs 29+ (uses `go-ts-mode`, built-in `use-package`, Eglot, etc.)
- `git`, `ripgrep`, and `fd` recommended for Consult
- Optional: Guix, Guile 3.0, PlantUML, a language server for any language you work in

## Install

```sh
git clone https://github.com/horellana/emacs-config ~/.emacs.d
```

On first launch Emacs will fetch the packages declared with `use-package`. If you're not on Guix, the missing `guix-config.el` load is guarded and will be skipped silently.

## Layout

```
.
├── init.el         # everything lives here
├── custom.el       # Customize-generated settings (auto-loaded if present)
└── guix-config.el  # optional, system-specific paths (not committed)
```

## Key bindings worth knowing

| Binding | Action |
|---|---|
| `<leader> w` | Save buffer (Evil leader) |
| `C-.` | `embark-act` |
| `C-x b` | `consult-buffer` |
| `M-y` | `consult-yank-pop` |
| `M-s r` / `M-s g` | `consult-ripgrep` / `consult-grep` |
| `M-g g` | `consult-goto-line` |
| `M-g i` | `consult-imenu` |
| `C-c n f` / `C-c n i` | `org-roam-node-find` / `-insert` |
| `C-c n d d` | Today's daily note |
| `C-c K` | `eldoc-box-help-at-point` |
| `C-M-i` | `completion-at-point` |

## Notes

- Backups go to `~/.emacs.d/saves`; lockfiles are disabled.
- `gc-cons-threshold` is bumped to 100 MB for snappier interaction.
- Background transparency is toggled on at startup via `horellana/toggle-frame-transparency`.
