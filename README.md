# Overview
This is the 4th project in the list given in the pdf. It will be of 2 types of decision trees that will be made in Ocaml.

# Labels in issues
If you are needing to assign someone to some task then create an issue and use the "assign" label.

If you need help with an issue, then comment on it so we can keep track of what has been asked.

If you have anything that could be added or changed to the project then create an issue and use the "enhanced" label.

# Getting ocamlgraph library
Library is now included with the repository.

# Information for Dot files
You can find out more information on dot files [here](https://en.wikipedia.org/wiki/DOT_(graph_description_language)) if you have not worked with them before.

# Working with Merlin server in Visual Studio code
The ```.merlin``` file is what the merlin server looks at to see what should be loaded when parsing through code. You can look to [this](https://github.com/ocaml/merlin/wiki/project-configuration) to read more. The file shows where the source files are to link them all together using an ```S```. The ```B``` is to tell it where all the built files are. These let it be able to use the files to help you write code. For example, when writing the tree test file it helps by knowing where the built build tree file is to allow you to write program using that file. Will also put the given ocamlgraph library built files in this manner so we can program using ocamlgraph.

# Running code in Visual studios
As of right now, we cannot run any test code in Visual Studios as the ocamlgraph library is not loaded from the TopDirs module which is what is what loads in all standard libraries when you run ```ocaml``` in the terminal to start up an ocaml session. We will need to figure out how to do this if we want to test our code as we go. Otherwise, we will need to run test as mentioned in the next section.

# To test code
You will need to run ```make``` in the directory that this repository is in to create the testProgram.exe. You may need to run ```opam install ocaml-makefile``` to have it run correctly. If you need to add new files, or change what you want to build to be then you will need to change the make file.


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
