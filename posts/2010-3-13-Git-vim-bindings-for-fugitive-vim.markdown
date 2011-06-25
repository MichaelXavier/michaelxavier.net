---
title: Git.vim bindings for fugitive.vim
categories: vim,git
---

Learn how to set up bindings like git.vim for fugitive.vim, the new kid on the vim git plugin block.

I've been a long time user of vim and used to use the rather simple but quite
excellent Git.vim for committing to my repo from within vim. I have switched to
the godly TPope's [fugitive](http://github.com/tpope/vim-fugitive) and knew it
was probably better. I haven't gotten through all the features yet but I
definitely started missing my bindings. The bindings Git.vim all use the
mapleader key (\ key by default). The bindings I used most frequently were:

1. \gs Split the buffer and show the output of git status.
2. \gc Split the buffer and let you edit the commit message. Saving and writing would commit.
3. \ga git add the file in the buffer, staging it for commit.
4. \gl Split the buffer and show the output of git log.
5. \gd Split the buffer and show the output of git diff for the current file.

To get this functionality in fugitive, add the following to your vimrc

    #!sh_sh
    "rebind my favorite commands from Git.vim for Fugitive
    nmap <leader>gs :Gstatus<cr>
    nmap <leader>gc :Gcommit<cr>
    nmap <leader>ga :Gwrite<cr>
    nmap <leader>gl :Glog<cr>
    nmap <leader>gd :Gdiff<cr>

Thanks to TPope for yet another wonderful plugin. Check out more of his repos on his [github account](http://github.com/tpope).