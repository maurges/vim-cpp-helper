" Description: A collection of fuctions to create c++ and qt classes less
" painfully
" Author: d86leader (d86leader@github.com) 2018-2018
" License: GNU GPLv2

let s:save_cpo = &cpo
set cpo&vim


"                       indent      return type
let s:header_func_re  = '\s*\%(' . '\([^(]\+\)\s\+'
"                        or tilde   or nothing
let s:header_func_re .= '\|\(\~\)' . '\|\)'
"                                 name    or opertaor decl
let s:header_func_re .= '\(\(' . '\k\+' . '\|operator\s*..\?.\?'
"                          brackets and inside
let s:header_func_re .= '\)\s*(.*' . '\)' . ';'


let s:source_func_re = ""
if g:cpp_helper_trailing_return_type
	"                                   class name
	let s:source_func_re  = '\s*auto\s\+' . '\k\+\s*::\s*' 
	"                       everything else                   return type
	let s:source_func_re .= '\(\k\+\s*(.*\)' . '\s*->\s*' . '\([^{]\+\)\s*\n\s*{'
else
	"                                 return type
	let s:source_func_re  = '\s*' . '\([^(]\+\)' . '\s\+' 
	"                     class name     everything else
	let s:source_func_re .= '\k\+\s*::\s*'. '\(\k\+\s*(.*\)\s*\n\s*{'
endif


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


fun! cpp_helper#class(classpath, qt_flavour) abort
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


fun! s:add_declaration(scope, ret_type, funcarg) abort
	let decl = ""
	if g:cpp_helper_trailing_return_type
		let decl = "auto " . a:funcarg . " -> " . a:ret_type
	else
		let decl = a:ret_type . " " . a:funcarg
	endif
	return s:add_to_scope(a:scope, decl)
endfun


fun! s:add_to_scope(scope, arg) abort
	"find out path to header file: current file full path with correct extension
	let filename = expand("%:p:r") . g:cpp_helper_header_extension
	call s:open_in_tab(filename)

	let empty = s:find_scope_place(a:scope)
	if empty
		exec "normal! o" . a:arg . ";"
	else
		exec "normal! o" . repeat("\<cr>", g:cpp_helper_declaration_offset) . a:arg . ";"
	endif
	write
endfun


fun! s:get_classname() abort
	let l = line(".")
	" level of {} blocks. When it reaches a new minimum, it means we found a new
	" nested class declaration
	let level = 0
	let minlevel = level
	let minlevel_lines = []
	while l > 0
		let l -= 1
		let cline = getline(l)
		if cline =~ '{' | let level -= 1 | endif
		if cline =~ '}' | let level += 1 | endif
		if level < minlevel
			let minlevel = level
			let minlevel_lines += [l]
		endif
	endwhile

	" parse a class name at each minimum we found
	let names = []
	for minlevel_line in minlevel_lines
		let l = minlevel_line
		while l > 0 && getline(l) !~ 'class\|struct\|namespace' | let l -= 1 | endwhile
		if getline(l) =~ 'namespace'
			"ignore scopes
			continue
		endif

		if l <= 0
			echoerr "Could not find class start!"
			throw "cpp-helper-error"
		endif

		let full_decl = s:joined_declaration_lines(l, '{')
		"          whitespace      declaration                  name     until end
		let decl_re = '\s*' . '\%(class\|struct\)' . '\s\+' . '\(\k\+\)' . '.*{'
		let parsed = matchlist(full_decl, decl_re)
		if parsed == []
			echoerr "Could not parse class declaration!"
			throw "cpp-helper-error"
		endif
		let names += [parsed[1]]
	endfor

	return join(reverse(names), "::")
endfun

fun! s:remove_default_args(str) abort
	"         after word   after eq     value     delim
	let arg_re = '\s*' . '='. '\s*' . '.\{-1,\}' . '\([,)]\)'
	return substitute(a:str, arg_re, '\1', "g")
endfun


fun! s:add_implementation(return_type, other_declaration) abort
	let classname = s:get_classname()
	"find out path to header file: current file full path with correct extension
	let filename = expand("%:p:r") . g:cpp_helper_source_extension
	"build pattern to delete indent
	let indent_re = ''
	if &expandtab
		let indent_re = '\n' . repeat(' ', indent(line(".")))
	else
		" tab after newline
		let indent_re = '\n	'
	endif
	"open new file to edit
	call s:open_in_tab(filename)

	"clean default arguments
	let other_declaration = s:remove_default_args(a:other_declaration)
	"clean excessive indentation
	let other_declaration = substitute(other_declaration, indent_re, "\n", "g")

	"find the line where last function ends
	let l = line("$")
	"                  simple          simple      brace not followed by namespace comment
	let lastfun_re = '#include' . '\|' . '{' . '\|' . '}\%(\s*//\s*namespace\)\@!'
	while getline(l) !~ lastfun_re && l > 1 | let l -= 1 | endwhile
	"set position to l, 0 for current buffer
	"	call setpos(".", [0, l, 0, 0])

	"build declaration line
	let decl_line = ""
	if a:return_type != ""
		let decl_line = a:return_type . " "
	endif
	if classname != ""
		let decl_line .= classname . "::"
	endif
	let other_decl_lines = split(other_declaration, "\n")
	let decl_line .= other_decl_lines[0]
	"build lines to append
	let lines  = repeat([""], g:cpp_helper_implementation_offset)
	let lines += [decl_line]
	let lines += other_decl_lines[1:]

	call append(l, lines)
	let l += len(lines)
	"set position to start of function
	call setpos(".", [0, l, 0, 0])

	call s:add_brackets(0)
	write
endfun


" joins the lines starting from linenr until it ends with end_char and comment
fun! s:joined_declaration_lines(linenr, end_char) abort
	"               any line          whitespace   comment, stuff, EOL
	let line_end_re = '^.*' . a:end_char . '\s' . '*\(//.*\|/\*.*\)\?$'
	" ^.*while\(\/\/\|\/\*\)\?

	let linenr = a:linenr
	let last_line = line("$")
	let text = getline(linenr)
	while text !~ line_end_re && linenr <= last_line
		let linenr += 1
		let text .= "\n" . getline(linenr)
	endwhile

	if linenr > last_line
		return ""
	else
		return text
	endif
endfun


" given the array of parsed declaration or implementation by re, return the
" return type and other arg. This is neccessary as they are on 1 and 2
" positions depending on whether the return type is trailing or not
fun! s:args_from_parse(parse) abort
	if a:parse == []
		echoerr "Could not parse the function!"
		throw "cpp-helper-error"
	endif

	let return_type = ""
	let other = ""

	if g:cpp_helper_trailing_return_type
		let return_type = a:parse[2]
		let other       = a:parse[1]
	else
		let return_type = a:parse[1]
		let other       = a:parse[2]
	endif

	return [return_type, other]
endfun


fun! cpp_helper#method(funcarg, scope) abort
	let parsed = []

	if g:cpp_helper_trailing_return_type
		"                                name and args                  return type
		let funcarg_re = 'auto\s\+ ' . '\(\k\+\s*(.*\)' . '\s*->\s*' . '\([^(]\+\)'
		let parsed = matchlist(a:funcarg, funcarg_re)
	else
		"                  return type            everything else
		let funcarg_re = '\([^(]\+\)' . '\s\+' . '\(\k\+\s*(.*\)'
		let parsed = matchlist(a:funcarg, funcarg_re)
	endif

	let [return_type, other] = s:args_from_parse(parsed)

	call s:add_declaration(a:scope, return_type, other)
	call s:add_implementation(return_type, other)
endfun


fun! cpp_helper#implement() abort
	" get lines until semicolon
	let line = s:joined_declaration_lines(line("."), ';')
	let parsed = matchlist(line, s:header_func_re)

	if parsed == []
		echoerr "Could not parse the function in the line!"
		throw "cpp-helper-error"
	endif

	let return_type     = parsed[1]
	let maybe_destuctor = parsed[2]
	let other           = parsed[3]
	"remove override from other
	let other = substitute(other, '\<override\>', "", "")
	"remove virtual from return_type
	let return_type = substitute(return_type, '\<virtual\>\s*', "", "")

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


fun! cpp_helper#reimplement() abort
	let ext = "." . expand("%:e") 
	if ext == g:cpp_helper_header_extension
		call s:reimplement_header()
	elseif ext == g:cpp_helper_source_extension
		call s:reimplement_source()
	else
		echom "is " . expand("%:e")
		echoerr "Could not determine module type. Is it not " .
			\ g:cpp_helper_header_extension . " or " .
			\ g:cpp_helper_source_extension . "?"
		throw "cpp-helper-error"
	endif
endfun


fun! cpp_helper#declare(scope) abort
	let line = s:joined_declaration_lines(line("."), '{')
	let parsed = matchlist(line, s:source_func_re)
	let [return_type, other] = s:args_from_parse(parsed)

	call s:add_declaration(a:scope, return_type, other)
endfun


fun! s:implement_constructor(funcarg) abort
	call s:add_implementation('', a:funcarg)
endfun


fun! s:implement_destructor(funcarg) abort
	call s:add_implementation('', '~'.a:funcarg)
endfun


fun! s:reimplement_header() abort
	let line = s:joined_declaration_lines(line("."), ';')
	let parsed = matchlist(line, s:header_func_re)

	if parsed == []
		echoerr "Could not parse the function in the line!"
		throw "cpp-helper-error"
	endif

	let return_type     = parsed[1]
	let maybe_destuctor = parsed[2]
	let other           = parsed[3]
	let name            = parsed[4]
	"remove override from other
	let other = substitute(other, '\<override\>', "", "")
	"remove virtual from return_type
	let return_type = substitute(return_type, '\<virtual\>\s*', "", "")
	let class_name = s:get_classname()

	if return_type == ""
		echoerr "Reimplementing constructors is not supported."
			\ . " Please use :Implement and manual deletion"
		return
	endif


	" find the old function in implementation file by its name
	let filename = expand("%:p:r") . g:cpp_helper_source_extension
	call s:open_in_tab(filename)
	"              ret type (or auto)
	let search_re = '\s*[^(]\+\s\+' . class_name . '\s*::\s*' . name . '('
	let g:search_re = search_re
	let pos = searchpos(search_re)
	echom "repositioning to " . pos[0] . ":" . pos[1]
	call cursor(pos)
endfun

fun! s:reimplement_source() abort
	let line = s:joined_declaration_lines(line("."), '{')
	let parsed = matchlist(line, s:source_func_re)
	let [return_type, other] = s:args_from_parse(parsed)
endfun


fun! cpp_helper#fill_q_property() abort
	"                     head                type(1)                     name(2)
	let prop_re  = '\s*Q_PROPERTY(' . '\(\k\+\%(<[^>]*>\)\?\)' . '\s\+' . '\(\k\+\)'
	"                         member decl    member name(3)   optional
	let prop_re .= '\%(\s\+' . 'MEMBER\s\+' . '\(\k\+\)' . '\)\?'
	"                        read decl      accessor(4)   optional
	let prop_re .= '\%(\s\+' . 'READ\s\+' . '\(\k\+\)' . '\)\?'
	"                        write decl      setter(5)   optional
	let prop_re .= '\%(\s\+' . 'WRITE\s\+' . '\(\k\+\)' . '\)\?'
	"                        reset decl      resetter(6)   optional
	let prop_re .= '\%(\s\+' . 'RESET\s\+' . '\(\k\+\)' . '\)\?'
	"                        notify decl      signal(7)   optional
	let prop_re .= '\%(\s\+' . 'NOTIFY\s\+' . '\(\k\+\)' . '\)\?'

	" get lines until closing bracket
	let line = s:joined_declaration_lines(line("."), ')')
	let parsed = matchlist(line, prop_re)

	if parsed == []
		echoerr "Could not parse the Q_PROPERTY in the line!"
		throw "cpp-helper-error"
	endif

	let type = parsed[1]
	let name = parsed[2]
	let member = parsed[3]
	let getter = parsed[4]
	let setter = parsed[5]
	let resetter = parsed[6]
	let signal   = parsed[7]

	if member != ""
		call s:add_to_scope('private:', type . ' ' . member)
	else
		"we still probably need to declare member. Or not.
	endif
	if getter != ""
		call s:add_declaration('public:', type, getter . '() const')
	endif
	if setter != ""
		call s:add_declaration('public:', 'void', setter . '(const ' . type . '&)')
	endif
	if resetter != ""
		call s:add_declaration('public:', 'void', resetter . '()')
	endif
	if signal != ""
		call s:add_declaration('signals:', 'void', signal . '(const ' . type . '&)')
	endif

	return parsed
endfun


fun! cpp_helper#constructor(scope, type, args) abort
	let classname = s:get_classname()

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
	call s:add_to_scope(a:scope, funcarg)
	call s:implement_constructor(funcarg)
endfun


fun! cpp_helper#editClass(name, in_tab)
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
