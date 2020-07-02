from setuptools import setup, find_packages
setup(name="min", 
	package_dir={'':'src'},
	version="0.1.0",
	packages=find_packages('src'),
	scripts=['src/imn'])
