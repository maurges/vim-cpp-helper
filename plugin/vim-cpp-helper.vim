" Description: A collection of commands to create c++ and qt classes less
" painfully
" Author: d86leader (d86leader@github.com) 2018-2018


let s:save_cpo = &cpo
set cpo&vim

if exists('g:loaded_vim_cpp_helper') && !exists('g:force_reload_vim_cpp_helper')
	finish
endif
let g:loaded_vim_cpp_helper = 1


" Default variables


" Commands availible
command! -complete=file -nargs=1 Class  :call CppHelperClass("<args>", 0)
command! -complete=file -nargs=1 QClass :call CppHelperClass("<args>", 1)

command! -complete=tag -nargs=+ MethodPublic    :call CppHelperMethod("<args>", "public:")
command! -complete=tag -nargs=+ MethodProtected :call CppHelperMethod("<args>", "protected:")
command! -complete=tag -nargs=+ MethodPrivate   :call CppHelperMethod("<args>", "private:")

command! -complete=tag -nargs=+ SlotPublic      :call CppHelperMethod("<args>", "public slots:")
command! -complete=tag -nargs=+ SlotProtected   :call CppHelperMethod("<args>", "protected slots:")
command! -complete=tag -nargs=+ SlotPrivate     :call CppHelperMethod("<args>", "private slots:")

command! -nargs=* Constructor      :call CppHelperConstructor("public:", "basic", "<args>")
command! -nargs=0 ConstructorCopy  :call CppHelperConstructor("public:", "copy", "")
command! -nargs=0 ConstructorMove  :call CppHelperConstructor("public:", "move", "")

command! -nargs=0 Implement        :call CppHelperImplement()
command! -nargs=0 DeclarePublic    :call CppHelperDeclare("public:")
command! -nargs=0 DeclarePrivate   :call CppHelperDeclare("private:")
command! -nargs=0 DeclareProtected :call CppHelperDeclare("protected:")

command! -complete=file -nargs=1 EditClass    :call CppHelperEditClass("<args>", 0)
command! -complete=file -nargs=1 TabEditClass :call CppHelperEditClass("<args>", 1)



" Setting default variables


if !exists('g:cpp_helper_header_extension')
	let g:cpp_helper_header_extension = ".h"
endif
if !exists('g:cpp_helper_source_extension')
	let g:cpp_helper_source_extension = ".cpp"
endif

" 0 for pragma once, else for ifndef
if !exists('g:cpp_helper_inclusion_guard_flavour')
	let g:cpp_helper_inclusion_guard_flavour = 0
endif

" format string for ifndef inclusion guard
if !exists('g:cpp_helper_inclusion_guard_format')
	let g:cpp_helper_inclusion_guard_format = "INCLUDED_%s"
endif

" 1 for true, 0 for bad
if !exists('g:cpp_helper_bracket_style')
	let g:cpp_helper_bracket_style = 1
endif

" 1 to go to header, 2 to return to where you started
if !exists('g:cpp_helper_after_creation')
	let g:cpp_helper_after_creation = 1
endif

if !exists('g:cpp_helper_wipe_buffers')
	let g:cpp_helper_wipe_buffers = 1
endif

" sets the indent level of scope markers in class declaration
if !exists('g:cpp_helper_scope_marker_indent')
	let g:cpp_helper_scope_marker_indent = 0
endif

" the offset of function starts from other things
if !exists('g:cpp_helper_declaration_offset')
	let g:cpp_helper_declaration_offset = 1
endif
if !exists('g:cpp_helper_implementation_offset')
	let g:cpp_helper_implementation_offset = 2
endif


" Functions


" go to window containing file in current tab
" open in current window if it doesnt exist
fun! s:open_in_tab(name) abort
	" window number for first buffer assosiated with name in current tab
	let nr = bufwinnr(a:name)
	if nr != -1
		"go to window
		exec nr . "wincmd w"
	else
		"simply open it in current window
		exec "edit " . a:name
	endif
endfun


" check if files starting with path exist
fun! s:check_exist(path, extensions, action) abort
	for ext in a:extensions
		if filereadable(a:path . ext)
			let errmsg = "Can't " . a:action . ": file " . a:path .
				ext . " already exists"
			echoerr errmsg
			return 1
		endif
	endfor
	return 0
endfun


fun! s:add_inclusion_guard(name) abort
	if g:cpp_helper_inclusion_guard_flavour == 0
		" use pragma inclusion guard
		call append(0, "#pragma once")
	else
		" use ifndef guard
		let guard_name = printf(g:cpp_helper_inclusion_guard_format, a:name)
		call append(0, "#ifndef " . guard_name)
		call append(0, "#define " . guard_name)
		call append(line("$"), "#endif //" . guard_name)
	endif
endfun


" adds brackets according to g:bracket style or argument if present
fun! s:add_brackets(is_class, ...) abort
	if a:0 == 1
		let style = a:1
	else
		let style = g:cpp_helper_bracket_style
	endif

	let cmd = "normal! "

	"add opening bracket
	if style == 1
		let cmd .= "o{"
	elseif style == 0
		let cmd .= "A\<space>{"
	else
		echoerr "Unsupported style supplied to bracker adding: ", style
		throw "cpp-helper-error"
	endif

	" add closing bracket
	let cmd .= "\<cr>}"

	if a:is_class
		let cmd .= ";"
	endif

	" go to line with the opening bracket and finish
	let cmd .= "\<c-c>k"
	exec cmd
endfun


fun! s:new_line_indent(string, level) abort
	exec "normal! o" . a:string . "\<c-c>>>0dt" . a:string[0]
	let i = 0
	while i < a:level
		normal! >>
		let i += 1
	endwhile
endfun


fun! CppHelperClass(classpath, qt_flavour) abort
	let cwd = getcwd() . "/"

	" check if files for the class are not occupied. May raise an error
	let exts = [g:cpp_helper_header_extension, g:cpp_helper_source_extension]
	if s:check_exist(cwd . a:classpath, exts, "creare class " . a:classpath)
		echoerr "Could not create class"
		return
	endif

	" remember the current position as we're going to be jumping buffers
	let startpos = getpos(".")
	let startbuf = bufnr("%")

	" get class name from path
	let classname_re = '^\(.*\/\)\?\([^/]\+\)$'
	let classname = matchlist(a:classpath, classname_re)[2]

	" add header file

	" create file: may raise error if buffer is not saved, this is expected
	exec "edit " . cwd . a:classpath . g:cpp_helper_header_extension
	"remember the buffer for the file
	let header_buffer = bufnr("%")

	call s:add_inclusion_guard(classname)

	if a:qt_flavour
		"include the most basic qt thing
		exec "normal! o#include <QObject>\<CR>"
		echom "qt flavour"
	else
		echom "not qt flavour, its value " a:qt_flavour
	endif

	" add description to fill
	exec "normal! o// Description: \<c-c>"
	" remember position of description
	let descpos = getpos(".")
	let descbuf = bufnr("%")

	" add class declaration
	exec "normal! o\<cr>class " . classname
	if a:qt_flavour
		exec "normal! a : public QObject"
	endif

	call s:add_brackets(1)
	if a:qt_flavour
		exec "normal! oQ_OBJECT\<cr>"
	endif

	" add scope markers (does not add private slots and protected members)
	call s:new_line_indent("private:", g:cpp_helper_scope_marker_indent)
	normal! o
	if a:qt_flavour
		call s:new_line_indent("signals:", g:cpp_helper_scope_marker_indent)
		normal! o
		call s:new_line_indent("public slots:", g:cpp_helper_scope_marker_indent)
		normal! o
		call s:new_line_indent("public:", g:cpp_helper_scope_marker_indent)
	else
		call s:new_line_indent("public:", g:cpp_helper_scope_marker_indent)
	endif

	"save changes
	write


	" add object file

	" create file
	exec "edit " . cwd . a:classpath . g:cpp_helper_source_extension
	"remember the buffer for the file
	let object_buffer = bufnr("%")

	" include header file
	exec "normal! a#include \"" . a:classpath . g:cpp_helper_header_extension . "\""

	"save chages
	write

	if g:cpp_helper_after_creation == 2
		"return to where we started
		exec "buffer " . startbuf
		call setpos(".", startpos)

		if g:cpp_helper_wipe_buffers
			exec "bdelete" . header_buffer
			exec "bdelete" . object_buffer
		endif
	elseif g:cpp_helper_after_creation == 1
		"return to where Description: is written
		exec "buffer " . descbuf
		call setpos(".", descpos)

		if g:cpp_helper_wipe_buffers
			exec "bdelete" . object_buffer
		endif
	endif
endfun


" positions at the end of the scope block
" returns whether scope is empty
fun! s:find_scope_place(scope_name) abort
	let scopes_re = '\(public:\|private:\|protected:\|public slots:\|private slots:\|protected slots:\|signals:\|^}\)'
	let scope_begin = search(a:scope_name)
	if !scope_begin
		echoerr "Could not find scope: " a:scope_name
		throw "cpp-helper-error"
	endif
	let scope_end = search(scopes_re)
	if !scope_end
		echoerr "Could not find end of scope: " a:scope_name
		throw "cpp-helper-error"
	endif
	let pos = prevnonblank(scope_end-1)
	call setpos(".", [0, pos, 0, 0])

	return pos == scope_begin
endfun


fun! s:add_declaration(scope, funcarg) abort
	"find out path to header file: current file full path with correct extension
	let filename = expand("%:p:r") . g:cpp_helper_header_extension
	call s:open_in_tab(filename)

	let empty = s:find_scope_place(a:scope)
	if empty
		exec "normal! o" . a:funcarg . ";"
	else
		exec "normal! o" . repeat("\<cr>", g:cpp_helper_declaration_offset) . a:funcarg . ";"
	endif
	write
endfun


fun! s:add_implementation(return_type, other_declaration) abort
	"find out the class we're in: file name without extension
	let classname = expand("%:t:r")
	"find out path to header file: current file full path with correct extension
	let filename = expand("%:p:r") . g:cpp_helper_source_extension
	call s:open_in_tab(filename)

	"find the line where last function ends
	let l = line("$")
	while getline(l) !~ '}\|#include' | let l -= 1 | endwhile
	"set position to l, 0 for current buffer
	call setpos(".", [0, l, 0, 0])

	"add empty implementation
	exec "normal! o" . repeat("\<cr>", g:cpp_helper_implementation_offset)
	if len(a:return_type) != 0
		exec "normal! a" . a:return_type . " "
	endif
	exec "normal! a" . classname . "::" . a:other_declaration
	call s:add_brackets(0)
	write

	"set position to start of function
	call setpos(".", [0, l+4, 0, 0])
endfun


fun! CppHelperMethod(funcarg, scope) abort
	"                  return type            everything else
	let funcarg_re = '\([^(]\+\)' . '\s\+' . '\(\k\+\s*(.*\)'

	let parsed      = matchlist(a:funcarg, funcarg_re)

	if parsed == []
		echoerr "Could not parse the function!"
		throw "cpp-helper-error"
	endif

	let return_type = parsed[1]
	let other       = parsed[2]

	call s:add_declaration(a:scope, a:funcarg)
	call s:add_implementation(return_type, other)
endfun


fun! CppHelperImplement() abort
	"             indent   return type            everything else        semicolon
	let func_re = '\s*\%(' . '\([^(]\+\)' . '\s\+'. '\|\(\~\)' . '\|\)' . '\(\k\+\s*(.*\)' . ';'

	let line = getline(line("."))
	let parsed = matchlist(line, func_re)

	if parsed == []
		echoerr "Could not parse the function in the line!"
		throw "cpp-helper-error"
	endif

	let return_type     = parsed[1]
	let maybe_destuctor = parsed[2]
	let other           = parsed[3]
	"remove override from other
	let other = substitute(other, '\<override\>', "", "")

	if maybe_destuctor == '~'
		call s:implement_destructor(other)
	elseif return_type == ''
		call s:implement_constructor(other)
	elseif return_type == 'explicit'
		"constructors may start with explicit and we should implement them too
		call s:implement_constructor(other)
	else
		call s:add_implementation(return_type, other)
	endif
endfun


fun! CppHelperDeclare(scope) abort
	"                      return type              class name     everything else
	let func_re = '\s*' . '\([^(]\+\)' . '\s\+' . '\k\+\s*::\s*'. '\(\k\+\s*(.*\)'

	let line = getline(line("."))
	let parsed = matchlist(line, func_re)

	if parsed == []
		echoerr "Could not parse the function in the line!"
		throw "cpp-helper-error"
	endif

	let return_type = parsed[1]
	let other       = parsed[2]

	call s:add_declaration(a:scope, return_type.' '.other)
endfun


fun! s:implement_constructor(funcarg) abort
	call s:add_implementation('', a:funcarg)
endfun


fun! s:implement_destructor(funcarg) abort
	call s:add_implementation('', '~'.a:funcarg)
endfun


fun! CppHelperConstructor(scope, type, args) abort
	"classname taken from file name
	let classname = expand("%:t:r")

	"write correct argument for declaring and implementing
	if a:type == "copy"
		let args = "const " . classname . "&"
	elseif a:type == "move"
		let args = classname . "&&"
	else
		"take arguments supplied, strip them of braces (why did i do this?)
		if len(a:args) >= 2 && a:args[0] == "(" && a:args[-1:-1] == ")"
			let args = a:args[1:-2]
		else
			let args = a:args
		endif
	endif

	let funcarg = classname . "(" . args . ")"
	call s:add_declaration(a:scope, funcarg)
	call s:implement_constructor(funcarg)
endfun


fun! CppHelperEditClass(name, in_tab)
	let basename = substitute(a:name, '\.\w*$', '', '')
	if a:in_tab
		exec "tabedit " . basename . g:cpp_helper_header_extension
		exec "vsplit " . basename . g:cpp_helper_source_extension
	else
		exec "edit " . basename . g:cpp_helper_header_extension
		exec "vsplit " . basename . g:cpp_helper_source_extension
	endif
endfun

let &cpo = s:save_cpo
unlet s:save_cpo
