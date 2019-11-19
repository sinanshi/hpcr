all:
	Rscript -e "devtools::document('.')"
	R CMD INSTALL .

clean:
	rm -rf src/*.o src/*.so 
