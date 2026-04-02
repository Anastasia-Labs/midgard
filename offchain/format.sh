export LC_CTYPE=C.UTF-8
export LC_ALL=C.UTF-8
export LANG=C.UTF-8

check=""
if test "$#" -gt 0 && test "$1" = "check"
then
	check="yes"
	fourmolu_mode="check"
	cabalfmt_mode="-c"
else
	fourmolu_mode="inplace"
	cabalfmt_mode="-i"
fi

find . -type f -name '*.hs' ! -path '*/dist-newstyle/*' ! -path '*/tmp/*' -exec \
	fourmolu \
		-o-XTypeApplications \
		-o-XQualifiedDo \
		-o-XOverloadedRecordDot \
		-o-XNondecreasingIndentation \
		-o-XPatternSynonyms \
		-m "$fourmolu_mode" \
		{} +
find . -type f -name '*.cabal' -exec cabal-fmt "$cabalfmt_mode" {} +
find . -type f -name '*.nix' -exec nixpkgs-fmt ${check:+"--check"} {} +