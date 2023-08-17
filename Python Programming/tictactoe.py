import random
import time

### Tic tac toe

def init():
  global board
  board = [
  ["1", "2", "3"],
  ["4", "5", "6"],
  ["7", "8", "9"]
  ]
  global p_c
  global c_c
  p_c = "" #player character; X or O
  c_c = "" #computer character; X or O
  global p_l
  global c_l
  p_l = [] #list of numbers player currently controls.
  c_l = [] #list of numbers computer currently controls.
  global win
  win = 0 # win == 1, player won. win == 2, computer won.

def pr_b():
  print(board[0])
  print("")
  print(board[1])
  print("")
  print(board[2])

def start():
  global p_c
  global c_c
  while p_c == "":
    p_c = input("Choose X or O ")
    if p_c.upper() == "X":
      p_c = "X"
      c_c = "O"
      time.sleep(0.5)
      print("You chose X")
    elif p_c.upper() =="O":
      p_c = "O"
      c_c = "X"
      time.sleep(0.5)
      print("You chose O")
    else:
      time.sleep(0.5)
      print("What is wrong with you")
      time.sleep(0.5)
      print("Choose again")
      p_c = ""

def placement():
  global p_l
  global p_c
  global board
  no = -1
  while no == -1:
    no = input("Choose where to place " )
    try:
      ino = int(no)
    except ValueError:
      time.sleep(0.5)
      print("What is wrong with you. Chooose again")
      no = -1
      continue
    if ino / 3 <=1:
      if no in board[0]:
        inno = board[0].index(no)
        board[0].insert(inno, p_c)
        board[0].remove(no)
        p_l.append(no)
      else:
        time.sleep(0.5)
        print("What is wrong with you. Choose again")
        no = -1
    elif ino / 3 <=2:
      if no in board[1]:
        inno = board[1].index(no)
        board[1].insert(inno, p_c)
        board[1].remove(no)
        p_l.append(no)
      else:
        time.sleep(0.5)
        print("What is wrong with you. Choose again")
        no = -1
    elif ino / 3 <= 3:
      if no in board[2]:
        inno = board[2].index(no)
        board[2].insert(inno, p_c)
        board[2].remove(no)
        p_l.append(no)
      else:
        time.sleep(0.5)
        print("What is wrong with you. Choose again")
        no = -1
    else :
      time.sleep(0.5)
      print("What is wrong with you. Choose again")
      no = -1

def c_placement():
  global c_c
  global c_l
  global board
  remain = []
  inno = ""
  for list in board:
    for item in list:
      try:
        int(item)
        remain.append(item)
      except ValueError:
        continue
  rand_remain = random.randint(1,len(remain))
  no = remain[rand_remain-1]
  if int(no)/3 <= 1:
    inno = board[0].index(no)
    board[0].insert(inno, c_c)
    board[0].remove(no)
  elif int(no)/3 <= 2:
    inno = board[1].index(no)
    board[1].insert(inno, c_c)
    board[1].remove(no)
  else:
    inno = board[2].index(no)
    board[2].insert(inno, c_c)
    board[2].remove(no)
  c_l.append(no)

## checking for a win

# 1, 2, 3
# 1, 4, 7
# 1, 5, 9
# 2, 5, 8
# 3, 6, 9
# 3, 5, 7
# 4, 5, 6
# 7, 8, 9

def check_win(x, y):
  global win
  if "1" in x:
    if ("2" in x) & ("3" in x):
      win = y
    elif ("4" in x) & ("7" in x):
      win = y
    elif ("5" in x) & ("9" in x):
      win = y
  if "2" in x:
    if ("5" in x) & ("8" in x):
      win = y
  if "3" in x:
    if ("5" in x) & ("7" in x):
      win = y
  if "4" in x:
    if ("5" in x) & ("6" in x):
      win = y
  if "7" in x:
    if ("8" in x) & ("9" in x):
      win = y

def play():
  global p_c
  global p_l
  global c_c
  global c_l
  global win
  global board
  init()
  start()
  time.sleep(0.5)
  pr_b()
  if p_c == "O":
    time.sleep(0.5)
    print("###############################")
    print("It is currently computer's turn")
    print("###############################")
    time.sleep(0.5)
    c_placement()
    time.sleep(0.5)
    pr_b()
    time.sleep(0.5)
  while win == 0:
    print("###############################")
    print("      It is now your turn      ")
    print("###############################")
    time.sleep(0.5)
    pr_b()
    time.sleep(0.5)
    placement()
    time.sleep(0.5)
    pr_b()
    time.sleep(0.5)
    check_win(p_l, 1)
    if win == 1:
      print("Congrats. You won")
      break
    print("###############################")
    print("It is currently computer's turn")
    print("###############################")
    time.sleep(0.5)
    c_placement()
    time.sleep(0.5)
    pr_b()
    time.sleep(0.5)
    check_win(c_l, 2)
    if win == 2:
      print("You lost. How?")
      break

play()
