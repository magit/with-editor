ELS = with-editor.el

DEPS = dash

TEXIPAGES = with-editor.texi
INFOPAGES = with-editor.info

ELCS    = $(ELS:.el=.elc)
DFLAGS  = $(addprefix -L ../,$(DEPS))
EFLAGS ?= $(DFLAGS)
EMACS  ?= emacs
BATCH   = $(EMACS) -batch -Q -L . $(EFLAGS)

MAKEINFO     ?= makeinfo
INSTALL_INFO ?= $(shell command -v ginstall-info || printf install-info)

WITH_EDITOR_VERSION = 2.5.2
ASYNC_VERSION       = 1.9
DASH_VERSION        = 2.13.0

.PHONY: help clean AUTHORS.md

help:
	$(info make all           - compile elisp and manual)
	$(info make lisp          - compile elisp)
	$(info make info          - generate info manual)
	$(info make clean         - remove generated files)
	$(info )
	$(info Release Managment)
	$(info =================)
	$(info )
	$(info make authors       - generate AUTHORS.md)
	$(info make bump-versions - bump versions for release)
	@printf "\n"

all: lisp info

lisp: $(ELCS)
%.elc: %.el
	@printf "Compiling %s\n" $<
	@$(BATCH)\
	  --eval '(setq with-editor-emacsclient-executable nil)'\
	  -f batch-byte-compile $<

texi: $(TEXIPAGES)

info: $(INFOPAGES) dir
%.info: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --no-split $< -o $@

dir: $(INFOPAGES)
	@printf "Generating dir\n"
	@echo $^ | xargs -n 1 $(INSTALL_INFO) --dir=$@

authors: AUTHORS.md
AUTHORS.md:
	@ printf "Authors\n=======\n\n" > $@
	@ ( printf "%s\n" "- Barak A. Pearlmutter <barak+git@pearlmutter.net>" && \
	    printf "%s\n" "- Lele Gaifax <lele@metapensiero.it>" && \
	    printf "%s\n" "- Rémi Vanicat <vanicat@debian.org>" && \
	    git log --pretty=format:'- %aN <%aE>' \
	  ) | sort -u >> $@

clean:
	@printf "Cleaning...\n"
	@rm -f $(ELCS)

define set_package_requires
(require 'dash)
(with-current-buffer (find-file-noselect "with-editor.el")
  (goto-char (point-min))
  (re-search-forward "^;; Package-Requires: ")
  (let ((s (read (buffer-substring (point) (line-end-position)))))
    (--when-let (assq 'async       s) (setcdr it (list async-version)))
    (--when-let (assq 'dash        s) (setcdr it (list dash-version)))
    (delete-region (point) (line-end-position))
    (insert (format "%S" s))
    (save-buffer)))
endef
export set_package_requires
#'

define set_manual_version
(let ((version (split-string "$(WITH_EDITOR_VERSION)" "\\.")))
  (setq version (concat (car version) "." (cadr version)))
  (with-current-buffer (find-file-noselect "with-editor.org")
    (goto-char (point-min))
    (re-search-forward "^#\\+SUBTITLE: for version ")
    (delete-region (point) (line-end-position))
    (insert version)
    (save-buffer)))
endef
export set_manual_version

bump-versions: bump-versions-1 texi
bump-versions-1:
	@$(BATCH) --eval "(progn\
        (setq async-version \"$(ASYNC_VERSION)\")\
        (setq dash-version \"$(DASH_VERSION)\")\
        $$set_package_requires\
        $$set_manual_version)"
