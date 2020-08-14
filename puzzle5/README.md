# Psuedocode

	Init:
		scores = [0, …, 0]
		pc = 0
		pc_dir = 1
		step = 0
		unused_breaks = []
		used_breaks = []
		cur_reg = 0
		prev_reg = none
		reg_dir = 1
		steps_per_reg = [1, …, 1]
	cur_reg_steps = 0

	Def y <yes>:
		scores[cur_reg] += prev_reg

	Def n <no>:
		Print debug info

	Def f <go faster>:
		if steps_per_reg[cur_reg] == 1:
			Error
		steps_per_reg[cur_reg] -= 1

	Def s <go slower>:
		steps_per_reg[cur_reg] += 1

	Def + <thumbs up>:
		scores[cur_reg] += 1

	Def - <thumbs down>:
		scores[cur_reg] -= 1

	Def c <clap>:
	reg_dir *= -1

	Def b <take a break>:
		if pc not in used_breaks
		unused_breaks.push_right(pc)
		_step()

	Def a <away>:
		if unused_breaks != []:
			used_breaks.push_left(unused_breaks.pop_right())
			_step()

	Def _step():
		pc += pc_dir
		step += 1
		cur_reg_steps += 1
		if cur_reg_steps == steps_per_reg[cur_reg]:
		prev_reg = cur_reg
		cur_reg += reg_dir
		cur_reg_steps = 0

	Useful:
	skip register: sf

# Actual Solutions:

## Breakout Room 1: 
	+bbbbcyyyyya
The idea was to maximize 'y' to get the highest fibbonanci number. So the solution can be achieved by looping over many 'y' commands using 'b' and 'a' and using 'c' to offset the adding.
	  
## Breakout Room 2:
	 +bbbbbbbbbbbbbbbbbbycsfycsfsfsfa
We realized that the number 63245986 is the 39th fibbonanci number. So, using similar concepts from Breakout Room 1, constantly add the numbers using the 'y' commands and use the 'csf' to avoid adding to the wrong student (there are 3 students). Then loop 'ycsf' using 'b' commands that are a multiple of 3 (to match the student number).

## Breakout Room 3:
	sssssssssassssssssss+c+++++++++aayyyyyyyyyyyyy----------
First change both steps per register of the first two registers to 10. Then add the second register to 10, while changing directions using 'c' and go to the next register using 'a'. Use 'y' to add the 10 from the second register to the first register 10 times, resulting in [100, 10, 0, 0]. Finally use 'y' to add 100 to the other registers and subtract 10 from the second register.