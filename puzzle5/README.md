

# Puzzle 5

## Breakout Room 1: 
	+bbbbcyyyyya
The idea was to maximize 'y' to get the highest fibbonanci number. So the solution can be achieved by looping over many 'y' commands using 'b' and 'a' and using 'c' to offset the adding.
	  
## Breakout Room 2:
	 +bbbbbbbbbbbbbbbbbbycsfycsfsfsfa
We realized that the number 63245986 is the 39th fibbonanci number. So, using similar concepts from Breakout Room 1, constantly add the numbers using the 'y' commands and use the 'csf' to avoid adding to the wrong student (there are 3 students). Then loop 'ycsf' using 'b' commands that are a multiple of 3 (to match the student number).

## Breakout Room 3:
	sssssssssassssssssss+c+++++++++aayyyyyyyyyyyyy----------
First change both steps per register of the first two registers to 10. Then add the second register to 10, while changing directions using 'c' and go to the next register using 'a'. Use 'y' to add the 10 from the second register to the first register 10 times, resulting in [100, 10, 0, 0]. Finally use 'y' to add 100 to the other registers and subtract 10 from the second register.