.PHONY: build rebuild clean check watch deploy

build rebuild clean check watch:
	cabal new-run exe:site -- $@

deploy: rebuild
	mkdir -p mnt
	sshfs hackage-blog@www-origin.haskell.org:htdocs mnt
	rsync -rv _site/ mnt || (fusermount -u mnt && false)
	sleep 1
	fusermount -u mnt
