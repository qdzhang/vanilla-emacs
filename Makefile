DRONES_DIR = $(shell git config "borg.drones-directory" || echo "lib")
BATCH = emacs -Q --batch
ELC_REGEX := ^.*.elc$
FIND_AND_DELETE := find config -regex '$(ELC_REGEX)' -delete

-include $(DRONES_DIR)/borg/borg.mk

help::
	$(info make codespell-dry   = run codespell, dry run)
	$(info make codespell-fix   = run codespell, write fixes)

bootstrap-borg:
	@git submodule--helper clone --name borg --path $(DRONES_DIR)/borg \
	--url git@github.com:emacscollective/borg.git
	@cd $(DRONES_DIR)/borg; git symbolic-ref HEAD refs/heads/main
	@cd $(DRONES_DIR)/borg; git reset --hard HEAD

codespell-dry:
	@cd lib; codespell \
	  --ignore-words ~/.emacs.d/etc/codespell/ignore-words \
	  --exclude-file ~/.emacs.d/etc/codespell/ignore-lines \
	  --skip $(shell sed '/^\s*$$/d;/^\s*#.*$$/d;s/#.*//;s/\s//g' \
	  ~/.emacs.d/etc/codespell/ignore-files | tr "\\n" ",")

codespell-fix:
	@cd lib; codespell --write-changes \
	  --ignore-words ~/.emacs.d/etc/codespell/ignore-words \
	  --exclude-file ~/.emacs.d/etc/codespell/ignore-lines \
	  --skip $(shell sed '/^\s*$$/d;/^\s*#.*$$/d;s/#.*//;s/\s//g' \
	  ~/.emacs.d/etc/codespell/ignore-files | tr "\\n" ",")

autoload:
	@$(BATCH) --script config/init-autoload.el --eval \
	"(my/update-all-autoloads)"

compile:
	@$(BATCH) --script init.el --eval \
	"(my/byte-and-native-compile)"

clean-elc:
	rm init.elc
	@echo "init.elc removed."
	$(FIND_AND_DELETE)
	@echo "All .elc file in config dir removed."
