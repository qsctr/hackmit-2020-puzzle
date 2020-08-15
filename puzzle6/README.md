# Puzzle 6

Copy the contents of [puzzle6.s](puzzle6.s) into the editor and run it. The program will calculate around 9 bytes of the private key until it hits the 2500 instruction limit and halts. The output is stored in registers `r15` to `r18`, with the bytes ordered from most significant to least significant within each register. Convert the nonzero bytes within those registers to ASCII to get the first 9 or so characters of the secret key. Then, look at the value of register `r4` and replace the address in the first line of code (`0x19800`) with that value. Run the program again to get the next 9 or so characters, then repeat the process. You should be able to obtain the whole private key in around three runs of the program.