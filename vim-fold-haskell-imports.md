UPDATE: Now folds exports, too.

Put this in ~/.vim/after/ftplugin/haskell.vim and pray to $deity.

```vim

" Buffer global state to keep foldlevel calculations fast.
let b:importFirst = -1
let b:importLast = -1
let b:exportFirst = -1
let b:exportLast = -1

" Update the global state often.
augroup ImportRange
    autocmd! * <buffer>
    autocmd BufReadPost,CursorHold,CursorHoldI <buffer> let [b:importFirst, b:importLast] = s:ImportRange()
    autocmd BufReadPost,CursorHold,CursorHoldI <buffer> let [b:exportFirst, b:exportLast] = s:ExportRange()
augroup END

" Find the first and last lines that start with 'import' in the file.
func s:ImportRange()
    return s:ThingRange('^import', '^import')
endfunc

func s:ExportRange()
    return s:ThingRange('^module', 'where')
endfunc

func s:ThingRange(start, end)
    let lnum = 1
    let first = -1
    let last = -1

    let eob = line('$')

    while lnum <= l:eob
        let line = getline(lnum)
        let first = lnum
        let lnum += 1
        if line =~ a:start
            break
        endif
    endwhile

    while lnum <= eob
        let line = getline(lnum)
        if line =~ a:end
            let last = lnum
        elseif line =~ '^\S'
            break
        endif
        let lnum += 1
    endwhile

    return [first, last]
endfunc

" Fold all imports in a single level-1 fold.
func s:ImportFold(lnum)
    if b:importFirst == -1
        let [b:importFirst, b:importLast] = s:ImportRange()
    endif
    if b:exportFirst == -1
        let [b:exportFirst, b:exportLast] = s:ExportRange()
    endif

    if a:lnum >= b:importFirst && a:lnum <= b:importLast
        return '1'
    elseif a:lnum >= b:exportFirst && a:lnum <= b:exportLast
        return '1'
    elseif getline(a:lnum) =~ '^$'
        return '0'
    else
        return '='
    endif
endfunc

setl foldmethod=expr
setl foldexpr=s:ImportFold(v:lnum)
```
