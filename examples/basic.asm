          .ORIG x3000                        ; this is the address in memory where the program will be loaded
          ADD R2, R3, R1                      ; clear R0
          ADD R5, R7, -1
          HALT                               ; halt the program
          .END
