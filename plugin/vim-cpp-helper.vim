command! -complete=dir -nargs=1 Class  :call Class("<args>", 0)
command! -complete=dir -nargs=1 QClass :call Class("<args>", 1)

command! -complete=tag -nargs=+ MethodPublic    :call Method("<args>", "public:")
command! -complete=tag -nargs=+ MethodProtected :call Method("<args>", "protected:")
command! -complete=tag -nargs=+ MethodPrivate   :call Method("<args>", "private:")

command! -complete=tag -nargs=+ SlotPublic      :call Method("<args>", "public slots:")
command! -complete=tag -nargs=+ SlotProtected   :call Method("<args>", "protected slots:")
command! -complete=tag -nargs=+ SlotPrivate     :call Method("<args>", "private slots:")
command! -complete=tag -nargs=+ QSignal         :call Method("<args>", "signals:")

"command! -nargs=0               Constructor     :call 
"command! -nargs=0               ConstructorCopy :call 
"command! -nargs=0               ConstructorMove :call 

command! -nargs=0 Implement        :call Implement()
command! -nargs=0 DeclarePublic    :call Declare("public:")
command! -nargs=0 DeclarePrivate   :call Declare("private:")
command! -nargs=0 DeclareProtected :call Declare("protected:")

let g:header_extension = ".h"
let g:source_extension = ".cpp"

" 0 for pragma once, else for ifndef
let g:inclusion_guard_flavour = 0

let g:inclusion_guard_format = "INCLUDED_%s"

" 1 for true, 0 for bad
let g:bracket_style = 1

let g:wipe_buffers = 1

" 0 for not indented, else for indented
let g:indented_scope_markers = 0

let g:declaration_offset = 1
let g:implementation_offset = 2


fun! s:check_exists(path, extensions, action) abort
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
	if g:inclusion_guard_flavour == 0
		" use pragma inclusion guard
		call append(0, "#pragma once")
	else
		" use ifndef guard
		let guard_name = printf(g:inclusion_guard_format, a:name)
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
		let style = g:bracket_style
	endif

	let cmd = "normal! "

	"add opening bracket
	if style == 1
		let cmd .= "o{"
	elseif style == 0
		let cmd .= "A\<space>{"
	else
		echoerr "Unsupported style supplied to bracker adding: ", style
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


fun! Class(classpath, qt_flavour) abort
	let cwd = getcwd() . "/"

	" check if files for the class are not occupied. May raise an error
	let exts = [g:header_extension, g:source_extension]
	if s:check_exists(cwd . a:classpath, exts, "creare class " . a:classpath)
		echoerr "Could not create class"
		return
	endif

	" remember the current position as we're going to be jumping buffers
	let cpos = getpos(".")
	let cbuf = bufnr("%")

	" get class name from path
	let classname_re = '^\(.*\/\)\?\([^/]\+\)$'
	let classname = matchlist(a:classpath, classname_re)[2]

	" add header file

	" create file: may raise error if buffer is not saved, this is expected
	exec "edit " . cwd . a:classpath . g:header_extension
	"remember the buffer for the file
	let header_buffer = bufnr("%")

	call s:add_inclusion_guard(classname)

	" add description to fill
	exec "normal! o\<cr>// Description: \<c-c>"

	" add class declaration
	exec "normal! o\<cr>class " . classname

	call s:add_brackets(1)

	" add scope markers (does not add private slots and protected members)
	call s:new_line_indent("private:", g:indented_scope_markers)
	normal! o
	if a:qt_flavour
		call s:new_line_indent("signals:", g:indented_scope_markers)
		normal! o
		call s:new_line_indent("public slots:", g:indented_scope_markers)
		normal! o
		call s:new_line_indent("public:", g:indented_scope_markers)
		normal! o
	else
		call s:new_line_indent("public:", g:indented_scope_markers)
		normal! o
	endif

	"save changes
	write


	" add object file

	" create file
	exec "edit " . cwd . a:classpath . g:source_extension
	"remember the buffer for the file
	let object_buffer = bufnr("%")

	" include header file
	exec "normal! O#include \"" . a:classpath . g:header_extension . "\""

	"save chages
	write

	"return to where we started
	exec "buffer " . cbuf
	call setpos(".", cpos)

	if g:wipe_buffers
		exec "bdelete" . header_buffer
		exec "bdelete" . object_buffer
	endif
endfun


" positions at the end of the scope block
" returns whether scope is empty
fun! s:find_scope_place(scope_name) abort
	let scopes_re = '\(public:\|private:\|protected:\|public slots:\|private slots:\|protected slots:\|signals:\|^}\)'
	let scope_begin = search(a:scope_name)
	if !scope_begin
		echoerr "Could not find scope: " a:scope_name
	endif
	let scope_end = search(scopes_re)
	if !scope_end
		echoerr "Could not find end of scope: " a:scope_name
	endif
	normal! kk

	return scope_end - scope_begin == 2
endfun


fun! s:add_declaration(scope, funcarg) abort
	"find out path to header file: current file full path with correct extension
	let filename = expand("%:p:r") . g:header_extension

	exec "edit " . filename
	let empty = s:find_scope_place(a:scope)
	if empty
		exec "normal! o" . a:funcarg . ";"
	else
		exec "normal! o" . repeat("\<cr>", g:declaration_offset) . a:funcarg . ";"
	endif
	write
endfun


fun! s:add_implementation(return_type, other_declaration) abort
	"find out the class we're in: file name without extension
	let classname = expand("%:t:r")
	"find out path to header file: current file full path with correct extension
	let filename = expand("%:p:r") . g:source_extension
	exec "edit " . filename

	"find the line where last function ends
	let l = line("$")
	while getline(l) !~ '}\|#include' | let l -= 1 | endwhile
	"set position to l, 0 for current buffer
	call setpos(".", [0, l, 0, 0])

	"add empty implementation
	exec "normal! o" . repeat("\<cr>", g:implementation_offset) . a:return_type . " " . classname . "::" . a:other_declaration . "\<cr>{\<cr>}"
	write

	"set position to start of function
	call setpos(".", [0, l+4, 0, 0])
endfun


fun! Method(funcarg, scope) abort
	"                  return type            everything else
	let funcarg_re = '\([^(]\+\)' . '\s\+' . '\(\k\+\s*(.*\)'

	let parsed      = matchlist(a:funcarg, funcarg_re)

	if parsed == []
		echoerr "Could not parse the function!"
	endif

	let return_type = parsed[1]
	let other       = parsed[2]

	call s:add_declaration(a:scope, a:funcarg)
	call s:add_implementation(return_type, other)
endfun


fun! Implement() abort
	"             indent   return type            everything else
	let func_re = '\s*' . '\([^(]\+\)' . '\s\+' . '\(\k\+\s*(.*\)' . ';'

	let line = getline(line("."))
	let parsed = matchlist(line, func_re)

	if parsed == []
		echoerr "Could not parse the function in the line!"
	endif

	let return_type = parsed[1]
	let other       = parsed[2]

	call s:add_implementation(return_type, other)
endfun


fun! Declare(scope) abort
	"                      return type              class name     everything else
	let func_re = '\s*' . '\([^(]\+\)' . '\s\+' . '\k\+\s*::\s*'. '\(\k\+\s*(.*\)'

	let line = getline(line("."))
	let parsed = matchlist(line, func_re)

	if parsed == []
		echoerr "Could not parse the function in the line!"
	endif

	let return_type = parsed[1]
	let other       = parsed[2]

	call s:add_declaration(a:scope, return_type.' '.other)
endfun


fun! ArgType(funcarg)
	"                  return type              name                parameters                qualifiers
	let funcarg_re = '\([^(]\+\)' . '\s\+' . '\(\k\+\)' . '\s*(' . '\([^)]*\)' . ')\s*' . '\(\%(\k\|\s\)*\)'

	let parsed = matchlist(a:funcarg, funcarg_re)

	let return_type = parsed[1]
	let func_name   = parsed[2]
	let params      = parsed[3]
	let qualifiers  = parsed[4]
	
endfun
