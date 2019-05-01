# Overview
This is the 4th project in the list given in the pdf. It will be of 2 types of decision trees that will be made in Ocaml.

# Labels in issues
If you are needing to assign someone to some task then create an issue and use the "assign" label.

If you need help with an issue, then comment on it so we can keep track of what has been asked.

If you have anything that could be added or changed to the project then create an issue and use the "enhanced" label.

# Getting ocamlgraph library
You will only need to run ```opam install ocamlgraph```. Dune knows where opam installs the library, so we just tell Dune to use it.

# Information for Dot files
You can find out more information on dot files [here](https://en.wikipedia.org/wiki/DOT_(graph_description_language)) if you have not worked with them before. Dot files can be viewed [here](https://dreampuf.github.io/GraphvizOnline/) by copying and pasting the output in the command line into the the interpreter on that page.

# Working with Merlin server in Visual Studio code
The ```.merlin``` file is what the merlin server looks at to see what should be loaded when parsing through code. You can look to [this](https://github.com/ocaml/merlin/wiki/project-configuration) to read more. The file shows where the source files are to link them all together using an ```S```. The ```B``` is to tell it where all the built files are. These let it be able to use the files to help you write code. For example, when writing the tree test file it helps by knowing where the built build tree file is to allow you to write program using that file.

Dune builds the merlin files for us automatically when we run build, so the only reason to know the above is if something is still wrong with the merlin files when loaded in viual studio code.

# Running code in Visual studios
You can test parts of your code in visual studios by calling an ocaml session within the visual studio terminal using this command:
    ```ocaml -I "_build/default/bin/.main.eobjs" -I "data"```
    
This command needs to be run at the root of the repository directory where the .ocamlinit file is located. The ocamlinit file tells the TopDir module to load the ocamlgraph library, the Str library, and all the compiled modules from the sources. The first -I tells ocamlinit where the cmo files are to be loaded in. The second -I is used so you can test the data files in the code simply by using having the string ```"data/file name.csv```.

If new files are added to the bin directory, and dune build is run then you may need to update the .ocamlinit file with the new .cmo file.

As mentioned before, you will need to have dune build the repository first before you can run any code in the terminal. This also means that if you edit any of the source code then you have to rerun the build command of dune and restart your ocaml session in the terminal.

# To build and run code
The dune files in each directory make it so you only have to run ```dune build bin/main.exe```. This should be run at the root of the repository. You will be hopefully be able to run ```dune runtest``` in the root of the directory in the future to run all the tests, but right now the test files cannot find the source files. This makes it give unbound module.


# Example outputs
## CreateDot
The CreateDot.exe file should have this output in a command prompt:
```
digraph G {
  root;
  child_1;
  child_2;


  root -> child_1;
  root -> child_2;

  }
```

## ParseDot
The ParseDot.exe should be called as such:
``` cmd
ParseDot.exe [name of dot file].dot
```

There is 1 input that is the name of the dot file to be parsed. If we pass in the file SampleTree.dot, then we should get 
```
digraph G {
  "0";
  "1";
  "5";
  "7";
  "8";
  "9";
  "11";
  "12";
  "10";
  "6";
  "2";
  "3";
  "4";
  
  
  "0" -> "1" [label=<f&#36;oo>, ];
  "0" -> "2" [label=<f&#36;oo>, ];
  "1" -> "5" [label=<f&#36;oo>, ];
  "1" -> "6" [label=<f&#36;oo>, ];
  "5" -> "7" [label=<f&#36;oo>, ];
  "5" -> "8" [label=<f&#36;oo>, ];
  "8" -> "9" [label=<f&#36;oo>, ];
  "8" -> "10" [label=<f&#36;oo>, ];
  "9" -> "11" [label=<f&#36;oo>, ];
  "9" -> "12" [label=<f&#36;oo>, ];
  "2" -> "3" [label=<f&#36;oo>, ];
  "2" -> "4" [label=<f&#36;oo>, ];
  
  }

```

as output in a tmp.dot file.
