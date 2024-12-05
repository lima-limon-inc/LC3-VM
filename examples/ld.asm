          .ORIG x3000                        ; this is the address in memory where the program will be loaded

          ADD R5, R5, 9
          ST R5, 5              ; ADDR = PC (3002) + 5 = 3007. Memory [ADDR] = R5 (9)
          LD R6, 4              ; ADDR = PC (3003) + 4 = 3007. R6 = memory [ADDR]
          HALT                  ; Halt
          .END
