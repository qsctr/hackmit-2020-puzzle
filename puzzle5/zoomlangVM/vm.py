class ZoomLangException(Exception):
  pass

class ZoomLangVM:
  def __init__(self, program, students=2):
    self.char_to_func = {'y': self.yes, 'f': self.faster, 's': self.slower, '+': self.up, '-': self.down, 'c': self.clap, 'b': self.take_break, 'a': self.away}
    self.program = list(program)
    self.students = students

    self.scores = [0] * students
    self.pc = 0
    self.pc_dir = 1
    self.step = 0
    self.unused_breaks = []
    self.used_breaks = []
    self.cur_reg = 0
    self.prev_reg = None
    self.reg_dir = 1
    self.steps_per_reg = [1] * students
    self.cur_reg_steps = 0

  def run(self):
    while self.pc < len(self.program):
      try:
        self.shouldSpecialStep = self.program[self.pc] == 'b' and self.pc in self.used_breaks
        self.char_to_func[self.program[self.pc]]()

        if (self.shouldSpecialStep):
          self._special_step()
        else: 
          self._step()

      except KeyError:
        if self.program[self.pc] == 'n':
          self.debug()
        else:
          print(f'scores: {self.scores}')
          print(f"invalid symbol '{self.program[self.pc]}' at position {self.pc}")
        return
      except ZoomLangException as e:
        print(f'scores: {self.scores}')
        print(e)
        return
    print(f'scores: {self.scores}')

  # y
  def yes(self):
    if self.prev_reg is None:
      raise ZoomLangException("cannot execute yes because no previous register")
    self.scores[self.cur_reg] += self.scores[self.prev_reg]

  # n
  def debug(self):
    print(f'scores: {self.scores}')
    print(f'pc: {self.pc}')
    print(f'pc_dir: {self.pc_dir}')
    print(f'step: {self.step}')
    print(f'unused_breaks: {self.unused_breaks}')
    print(f'used_breaks: {self.used_breaks}')
    print(f'cur_reg: {self.cur_reg}')
    print(f'prev_reg: {self.prev_reg}')
    print(f'reg_dir: {self.reg_dir}')
    print(f'steps_per_reg: {self.steps_per_reg}')
    print(f'cur_reg_steps: {self.cur_reg_steps}')

  # f
  def faster(self):
    if self.steps_per_reg[self.cur_reg] == 1:
      raise ZoomLangException(f"register {self.cur_reg} cannot go any faster")
    self.steps_per_reg[self.cur_reg] -= 1

  # s
  def slower(self):
    self.steps_per_reg[self.cur_reg] += 1

  # +
  def up(self):
    self.scores[self.cur_reg] += 1

  # -
  def down(self):
    self.scores[self.cur_reg] -= 1

  # c
  def clap(self):
    self.reg_dir *= -1

  # b
  def take_break(self):
    if self.pc not in self.used_breaks:
      self.unused_breaks.append(self.pc)

  # a
  def away(self):
    if self.unused_breaks:
      self.pc = self.unused_breaks.pop()
      self.used_breaks.insert(0, self.pc)

  # _step
  def _step(self):
    self.pc += self.pc_dir
    self.step += 1
    self.cur_reg_steps += 1
    if self.cur_reg_steps >= self.steps_per_reg[self.cur_reg]:
      self.prev_reg = self.cur_reg
      self.cur_reg = (self.cur_reg + self.reg_dir) % self.students
      self.cur_reg_steps = 0
  
  # special _step for 'b'
  def _special_step(self):
    self.pc += self.pc_dir
    if self.cur_reg_steps == self.steps_per_reg[self.cur_reg]:
      self.prev_reg = self.cur_reg
      self.cur_reg = (self.cur_reg + self.reg_dir) % self.students
      self.cur_reg_steps = 0