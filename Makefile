# -----------------------------------------------------------------------------
# quality

# linting
# same hlint version as used in ci pipeline
.PHONY: install-hlint
install-hlint:
	stack install hlint --resolver snapshot.yaml

# hlint check
# remark:
#	this check is part of the ci pipeline
.PHONY: hlint-check
hlint-check:
	hlint -j .

# formatting
# same ormolu version as used in ci pipeline
.PHONY: install-ormolu
install-ormolu:
	stack install ormolu --resolver snapshot.yaml

.PHONY: ormolu-format
ormolu-format:
	$(ORMOLU_FORMAT)

# ormolu format
ifeq ($(OS), Windows_NT)
ORMOLU_FORMAT = powershell -noprofile -executionpolicy remotesigned -File ./utils/ormolu-format.ps1
else
ORMOLU_FORMAT = bash ./utils/ormolu-format.sh
endif

# ormolu check; if nothing needs to be formated nothing is returned (quiet on success)
# remark:
#	this check is part of the ci pipeline
# 	execute the ormolu-format target in case of findings to reformat the code
.PHONY: ormolu-check
ormolu-check:
	$(ORMOLU_CHECK)

ifeq ($(OS), Windows_NT)
ORMOLU_CHECK = powershell -noprofile -executionpolicy remotesigned -File ./utils/ormolu-check.ps1
else
ORMOLU_CHECK = bash ./utils/ormolu-check.sh
endif

