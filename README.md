# LSP_Jaguar
LSP player recoded for Atari Jaguar

LSP player created by Arnaud Carré : https://github.com/arnaud-carre/LSPlayer

modules need to be be converted using lspconvert.exe 

current LSP version handled by this code : 1.05

- lspv15.s : latest module only version
- lsp_and_sound1.s : the same + 4 sound channels

compile and link : 

    rmac -fb -s -u lsp2.s
    
    rln -o lsp2.abs -w -rq -a 4000 x x lsp2.o -m 


rmac : V2.1.13 

rln : V1.7.0 
