# vim-cpp-helper

A vim plugin to quickly generate c++ headers for you.

## Intro

This plugin takes the job of creating header+source files, a often boring
task especially with an architecture requiring a many classes, on its
shoulders. It provides a handful of commands for various jobs of creating and
transferring code between files, just what a lazy/sane person needs.

## Installation

Use your favourite plugin manager or just drop all the files into the vim
folder. Without plugin manager with Linux and Mac:
```
git clone https://github.com/d86leader/vim-cpp-helper.git && cp -r vim-cpp-helper/* ~/.vim/
```
For Pathogen:
```
cd ~/.vim/bundle && git clone https://github.com/d86leader/vim-cpp-helper.git
```
For vim-plug and similar, add the following line to your vimrc file after initializing the manager:
```
Plug 'd86leader/vim-cpp-helper'
```

## Commands

All the functionality of the plugin is in those commands, have a look.

Use to create an empty class and corresponding files:
```
	:Class path/classname
```
Path is used like a system path, it can be global or relative. Most useful
with relative paths to create classes in the project. As is everywhere in vim,
the path is relative to your vim cwd set with :cd. The examaple above with vim
started in the project root would create header and source files classname.h
and classname.cpp in folder path.

When you're writing a qt project in vim, it would be useful to create classes
with qt stuff in them already, and the following command will help you. Use it
like the command above:
```
	:QClass path/QClassName
```

To create methods use the following commands:
```
	:MethodPublic int my_method(int, std::unique_ptr<float>) override
	:MethodPrivate const int* my_method(int, std::unique_ptr<float>) const
	:MethodProtected int my_method(int, std::unique_ptr<float>)
```

For Qt you can also use commands for slots:
```
	:SlotPublic void MySlot(int)
	:SlotPrivate void MySlot(int)
	:SlotProtected void MySlot(int)
```

Warning: these commands are based on regexes so they won't be able to
recognize really complex c++ constructions, and they can't recognize macros in
the definition. You shouldn't write code like that anyway they say.

Warning #2: functions returning pointer written like that: `int *foo();` can't
be recognized either. If you can write the regex for me, please do and send me
a pull request, your name will forever be remembered by the hearts of
millions.

To create constructors you can use the similar commands:
```
	:Constructor int, int
	:Constructor (float, int)
	:Constructor ()
	:ConstructorCopy
	:ConstructorMove
```

When you already have a header written or want to write the header using vim
power, you can use the following command on the line with declaration of the
method to start the implementation in the source file:
```
	:Implement
```

When you have already written the implementation but forgot about the header,
position youself on the first line of the function (where its name is written)
and use the following commands to add it to the header:
```
	:DeclarePublic
	:DeclarePrivate
	:DeclareProtected
```

## Options

Use `g:cpp_helper_header_extension` and `g:cpp_helper_source_extension` to set the
file extensions of header and source files. Defaults:
```
	let g:cpp_helper_header_extension = ".h"
	let g:cpp_helper_source_extension = ".cpp"
```

Use `g:cpp_helper_inclusion_guard_flavour` to set how the include guards look.
Possible values: 0 for pragma once, 1 for ifndef. Default:
```
	let g:cpp_helper_inclusion_guard_flavour = 0
```

Use `g:cpp_helper_inclusion_guard_format` with guard flavour set to 1 to set how
the guard will look. The value is a format string and must contain %s which
will be substituted for the class name. Default value:
```
	let g:cpp_helper_inclusion_guard_format = "INCLUDED_%s"
```

Use `g:cpp_helper_bracket_style` to set how the braces around classes and
methods look. 0 for opening bracket on the same line as name, 1 for on the
following line. Default:
```
	let g:cpp_helper_bracket_style = 1
```

Use `g:cpp_helper_after_creation` to set where the cursor goes after creating
a new class. Possible values: 1 to go to the header of the new class, 2 to go
to where you started. Default:
```
	let g:cpp_helper_after_creation = 1
```

Use `g:cpp_helper_wipe_buffers` to set whether buffers created when making new
classes are deleted after the plugin finished working on them or not. Default:
```
	let g:cpp_helper_wipe_buffers = 1
```

Use `g:cpp_helper_scope_marker_indent` to set the indent for scope markers in
class declaration. Default:
```
	let g:cpp_helper_scope_marker_indent = 0
```

Use `g:cpp_helper_declaration_offset` and `g:cpp_helper_implementation_offset`
to set how many empty lines will be put between the declarations of methods
and between the implementations. Default:
```
	let g:cpp_helper_declaration_offset = 1
	let g:cpp_helper_implementation_offset = 2
```
