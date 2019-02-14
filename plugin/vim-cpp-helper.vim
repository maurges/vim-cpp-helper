" Description: A collection of commands to create c++ and qt classes less
" painfully
" Author: d86leader (d86leader@github.com) 2018-2018
" License: GNU GPLv2


let s:save_cpo = &cpo
set cpo&vim

if exists('g:loaded_vim_cpp_helper') && !exists('g:force_reload_vim_cpp_helper')
	finish
endif
let g:loaded_vim_cpp_helper = 1


" Default variables


" Commands availible
command! -complete=file -nargs=1 Class  :call cpp_helper#class("<args>", 0)
command! -complete=file -nargs=1 QClass :call cpp_helper#class("<args>", 1)

command! -complete=tag -nargs=+ MethodPublic    :call cpp_helper#method("<args>", "public:")
command! -complete=tag -nargs=+ MethodProtected :call cpp_helper#method("<args>", "protected:")
command! -complete=tag -nargs=+ MethodPrivate   :call cpp_helper#method("<args>", "private:")

command! -complete=tag -nargs=+ SlotPublic      :call cpp_helper#method("<args>", "public slots:")
command! -complete=tag -nargs=+ SlotProtected   :call cpp_helper#method("<args>", "protected slots:")
command! -complete=tag -nargs=+ SlotPrivate     :call cpp_helper#method("<args>", "private slots:")

command! -nargs=* Constructor      :call cpp_helper#constructor("public:", "basic", "<args>")
command! -nargs=0 ConstructorCopy  :call cpp_helper#constructor("public:", "copy", "")
command! -nargs=0 ConstructorMove  :call cpp_helper#constructor("public:", "move", "")

command! -nargs=0 Implement        :call cpp_helper#implement()
command! -nargs=0 DeclarePublic    :call cpp_helper#declare("public:")
command! -nargs=0 DeclarePrivate   :call cpp_helper#declare("private:")
command! -nargs=0 DeclareProtected :call cpp_helper#declare("protected:")

command! -complete=file -nargs=1 EditClass    :call cpp_helper#editClass("<args>", 0)
command! -complete=file -nargs=1 TabEditClass :call cpp_helper#editClass("<args>", 1)



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



let &cpo = s:save_cpo
unlet s:save_cpo
