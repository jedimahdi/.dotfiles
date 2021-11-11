vim.cmd([[
function! NetrwMapping()
  nmap <buffer> h -^
  nmap <buffer> l <CR>
  nmap <buffer> a %
  nmap <buffer> H u

  nmap <buffer> <leader>e <C-^>

  nmap <buffer> P <C-w>z

  nmap <buffer> <TAB> mf
  nmap <buffer> <S-TAB> mF
  nmap <buffer> <Leader><TAB> mu

  nmap <buffer> L <CR>:Lexplore<CR>
  nmap <buffer> <Leader>dd :Lexplore<CR>

  nmap <buffer> ff %:w<CR>:buffer #<CR>
  nmap <buffer> fe R
  nmap <buffer> fc mc
  nmap <buffer> fC mtmc
  nmap <buffer> fx mm
  nmap <buffer> fX mtmm
  nmap <buffer> f; mx

  nmap <buffer> fl :echo join(netrw#Expose("netrwmarkfilelist"), "\n")<CR>
  nmap <buffer> fq :echo 'Target:' . netrw#Expose("netrwmftgt")<CR>
  nmap <buffer> fd mtfq

endfunction

augroup netrw_mapping
  autocmd!
  autocmd filetype netrw call NetrwMapping()
augroup END


  nmap <leader>e -

  let g:netrw_fastbrowse = 0
  let g:netrw_altfile = 1
]])
