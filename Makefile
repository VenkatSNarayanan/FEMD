install:
	@echo "Updating Cabal.....\n"
	@cabal update
	@echo "Installing Control.Lens....\n"
	@cabal install lens
	@echo "Installing FEMD....\n"
	@cabal install 
	@echo "Install Done!!! Enter \"make play\" to play now!!\n"
clean:
	@echo "Removing the dist directory for fresh install....\n"
	@cabal clean
	@echo "Done\n"
play:
	@echo "Ready to play???\n"
	@sleep 1
	@./dist/build/FEMD/FEMD
