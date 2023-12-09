import os
from time import sleep
import subprocess
from pynput import keyboard

import curses
import time

os.unlink("output")


def box(screen, width: int, height: int):
    screen.addstr(0, 0, "-" * WIDTH)
    for i in range(height):
        screen.addstr(i, 0, "|")
        screen.addstr(i, width, "|")

    screen.addstr(height, 0, "-" * width)


def player(screen, x: int, y: int, player_height: int):
    for i in range(player_height):
        screen.addstr(y + i, x, "#")


def ball(screen, x: int, y: int):
    screen.addstr(y, x, "O")


def draw_world(
    screen,
    width: int,
    height: int,
    ball_x: int,
    ball_y: int,
    velo_x: float,
    velo_y: float,
    player1y: int,
    player2y: int,
    player_height: int,
):
    box(screen, width, height)
    player(screen, 3, player1y, player_height)
    player(screen, width - 3, player2y, player_height)
    ball(screen, ball_x, ball_y)


WIDTH = 150
HEIGHT = 30
PLAYERHEIGHT = 10


player1y = int(HEIGHT / 2) - int(PLAYERHEIGHT / 2)
player2y = int(HEIGHT / 2) - int(PLAYERHEIGHT / 2)
ball_x = int(WIDTH / 2)
ball_y = int(HEIGHT / 2)
velo_x = 2.0
velo_y = 1.0

screen = curses.initscr()

curses.noecho()
curses.cbreak()
curses.curs_set(0)
screen.clear()
draw_world(screen, WIDTH, HEIGHT, ball_x, ball_y, velo_x, velo_y, player1y, player2y, PLAYERHEIGHT)


p1input = "-"
p2input = "-"
while True:
    with keyboard.Events() as events:
        event = events.get(0.05)
        # if not event:
        #     p1input = "-"
        #     p2input = "-"
        match event:
            case keyboard.Events.Press(key=keyboard.KeyCode(char="w")):
                p1input = "u"
            case keyboard.Events.Press(key=keyboard.Key.up):
                p2input = "u"
            case keyboard.Events.Release(key=keyboard.KeyCode(char="w")):
                p1input = "-"
            case keyboard.Events.Release(key=keyboard.Key.up):
                p2input = "-"
            case keyboard.Events.Press(key=keyboard.KeyCode(char="s")):
                p1input = "d"
            case keyboard.Events.Press(key=keyboard.Key.down):
                p2input = "d"
            case keyboard.Events.Release(key=keyboard.KeyCode(char="s")):
                p1input = "-"
            case keyboard.Events.Release(key=keyboard.Key.down):
                p2input = "-"

    print(f"{p1input=} {p2input=}")
    process = subprocess.Popen([".\\main"], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout, stderr = process.communicate(
        f"{p1input} {p2input} {WIDTH} {HEIGHT} {ball_x} {ball_y} {velo_x} {velo_y} {player1y} {player2y}".encode()
    )
    haskell_result = stdout.decode().strip().replace('"', "")
    with open("output", "a") as f:
        f.write(haskell_result)
        f.write("\n")

    if haskell_result.startswith("winner"):
        break

    width, height, ball_x, ball_y, velo_x, velo_y, player1y, player2y = haskell_result.split(" ")

    screen.clear()
    draw_world(
        screen,
        WIDTH,
        HEIGHT,
        int(ball_x),
        int(ball_y),
        float(velo_x),
        float(velo_y),
        int(player1y),
        int(player2y),
        PLAYERHEIGHT,
    )

    screen.refresh()
screen.clear()
message_player_1_wins = r"""
 ____     ___                                                  _            __      __                            
/\  _`\  /\_ \                                               /' \          /\ \  __/\ \    __                     
\ \ \L\ \\//\ \       __      __  __       __    _ __       /\_, \         \ \ \/\ \ \ \  /\_\     ___      ____  
 \ \ ,__/  \ \ \    /'__`\   /\ \/\ \    /'__`\ /\`'__\     \/_/\ \         \ \ \ \ \ \ \ \/\ \  /' _ `\   /',__\ 
  \ \ \/    \_\ \_ /\ \L\.\_ \ \ \_\ \  /\  __/ \ \ \/         \ \ \         \ \ \_/ \_\ \ \ \ \ /\ \/\ \ /\__, `\
   \ \_\    /\____\\ \__/.\_\ \/`____ \ \ \____\ \ \_\          \ \_\         \ `\___x___/  \ \_\\ \_\ \_\\/\____/
    \/_/    \/____/ \/__/\/_/  `/___/> \ \/____/  \/_/           \/_/          '\/__//__/    \/_/ \/_/\/_/ \/___/ 
                                  /\___/                                                                          
                                  \/__/                                                                           

"""
message_player_2_wins = r""""
 ____     ___                                                  ___           __      __                            
/\  _`\  /\_ \                                               /'___`\        /\ \  __/\ \    __                     
\ \ \L\ \\//\ \       __      __  __       __    _ __       /\_\ /\ \       \ \ \/\ \ \ \  /\_\     ___      ____  
 \ \ ,__/  \ \ \    /'__`\   /\ \/\ \    /'__`\ /\`'__\     \/_/// /__       \ \ \ \ \ \ \ \/\ \  /' _ `\   /',__\ 
  \ \ \/    \_\ \_ /\ \L\.\_ \ \ \_\ \  /\  __/ \ \ \/         // /_\ \       \ \ \_/ \_\ \ \ \ \ /\ \/\ \ /\__, `\
   \ \_\    /\____\\ \__/.\_\ \/`____ \ \ \____\ \ \_\        /\______/        \ `\___x___/  \ \_\\ \_\ \_\\/\____/
    \/_/    \/____/ \/__/\/_/  `/___/> \ \/____/  \/_/        \/_____/          '\/__//__/    \/_/ \/_/\/_/ \/___/ 
                                  /\___/                                                                           
                                  \/__/                                                                            
"""
if "1" in haskell_result:
    screen.addstr(0, 0, message_player_1_wins)
elif "2" in haskell_result:
    screen.addstr(0, 0, message_player_2_wins)
else:
    screen.addstr(0, 0, "something wrong")
screen.refresh()

curses.napms(3000)
screen.getch()
curses.endwin()
