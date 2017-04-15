install:
	@echo "Updating Cabal.....\n"
	@cabal update
	@echo "Installing Control.Lens....\n"
	@cabal install lens
	@echo "Installing FEMD....\n
	@cabal install 
	@echo "Install Done!!! Enter \"make run\" to play now!!\n"
clean:
	cabal clean
play:
	@./dist/build/FEMD/FEMD
