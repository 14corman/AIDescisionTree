# Overview
This is the 4th project in the list given in the pdf. It will be of 2 types of decision trees that will be made in Ocaml.

# Labels in issues
If you are needing to assign someone to some task then create an issue and use the "assign" label.

If you need help with an issue, then comment on it so we can keep track of what has been asked.

If you have anything that could be added or changed to the project then create an issue and use the "enhanced" label.

# Getting ocamlgraph library
The recommended way to get the ocamlgraph library (we will be using version 1.8.8) is to use the code ```opam install ocamlgraph``` in whatever terminal you need to use to work on ocaml. In windows that should be Cygwin, in Mac I am not sure, and in Linux it should be just the normal terminal.

# Information for Dot files
You can find out more information on dot files [here](https://en.wikipedia.org/wiki/DOT_(graph_description_language)) if you have not worked with them before.

# Working with Merlin server in Visual Studio code
The ```.merlin``` file is what the merlin server looks at to see what should be loaded when parsing through code. You can look to [this](https://github.com/ocaml/merlin/wiki/project-configuration) to read more. Right now, the file only has ```PKG ocamlgraph``` in it because we want it to find and use the ocamlgraph library to help us work with all the graph code in the file.

# Running code in Visual studios
As of right now, we cannot run any test code in Visual Studios as the ocamlgraph library is not loaded from the TopDirs module which is what is what loads in all standard libraries when you run ```ocaml``` in the terminal to start up an ocaml session. We will need to figure out how to do this if we want to test our code as we go. Otherwise, we will need to run test as mentioned in the next section.

# To test code
You will need to run ```ocamlfind ocamlopt -o [name of executable] -linkpkg -package ocamlgraph [name of ocaml file to test 1].ml [name of ocaml file to test 2].ml [name of ocaml file to test 3].ml ...``` to build an executable that you will then be able to run through the command prompt to see if the file is working or not. The ocaml files 1, 2, 3, ... will be loaded in that order. So, if file 3 relies on file 2 then the order has to be 2 then 3 and not 3 then 2. If the files do not rely on each other then order does not matter. This will bring all the files together into an executable that can be run in 1 go. If you use 1 file then you will be testing just that file when it executes.
