# LSP_Jaguar
LSP player recoded for Atari Jaguar

LSP player created by Arnaud Carré : https://github.com/arnaud-carre/LSPlayer

modules need to be be converted using lspconvert.exe 

current LSP version handled by this code : 1.05

- lsp2.s : latest module only verison
- lsp_and_sound1.s : the same + 4 sound channels

compile and link : 
    rmac -fb -s -u lsp2.s
    rln -o lsp2.abs -w -rq -a 4000 x x lsp2.o -m 
