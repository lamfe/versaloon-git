.PHONY: clean All

All:
	@echo ----------Building project:[ vsprog - Debug ]----------
	@cd "vsprog" && "mingw32-make.exe"  -j 2 -f "vsprog.mk"
clean:
	@echo ----------Cleaning project:[ vsprog - Debug ]----------
	@cd "vsprog" && "mingw32-make.exe"  -j 2 -f "vsprog.mk" clean
