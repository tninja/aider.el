
from fleet import Fleet, FleetGroup


class Player:

    def __init__(self, name, board_size=(10, 10)):
        self.name = name
        self.board_size = board_size
        self.fleet_group = FleetGroup()
        self.hit_history = []
        self.was_hit_history = []

    def random_init(self, fleet_names=['carrier', 'battleship', 'cruiser', 'submarine', 'destroyer']):
        board_size = self.board_size
        import random
        board = [[0] * board_size[1] for _ in range(board_size[0])]

        def is_valid_placement(x, y, size, direction):
            if direction == 'vertical':
                return all(board[x + i][y] == 0 for i in range(size))
            else:
                return all(board[x][y + i] == 0 for i in range(size))

        for name in fleet_names:
            fleet_sizes = {
                'carrier': 5,
                'battleship': 4,
                'cruiser': 3,
                'submarine': 3,
                'destroyer': 3,
                'patrol boat': 1,
                'boater': 2,
                'bomber': 6,
                'speed boat': 3,
                'smasher': 2,
                'f-1': 2
            }
            size = fleet_sizes[name]
            direction = random.choice(['vertical', 'horizontal'])
            placed = False

            while not placed:
                if direction == 'vertical':
                    x = random.randint(0, board_size[0] - size)
                    y = random.randint(0, board_size[1] - 1)
                else:
                    x = random.randint(0, board_size[0] - 1)
                    y = random.randint(0, board_size[1] - size)

                if is_valid_placement(x, y, size, direction):
                    placed = True
            fleet = Fleet(x, y, size, direction, name)
            self.fleet_group.add(fleet)
            for i in range(size):
                if direction == 'vertical':
                    board[x + i][y] = 1
                else:
                    board[x][y + i] = 1
            placed = True

    def hit(self, player, x, y):
        result = player.fleet_group.hit(x, y)
        self.hit_history.append((x, y, result is not None))
        return result

    def was_hit(self, x, y):
        self.was_hit_history.append((x, y))
        return self.fleet_group.hit(x, y)

    def all_sunk(self):
        return self.fleet_group.all_sunk()

    def show_myself(self):
        # show a x*y grid, which x=board_size[0], y=board_size[1]
        # if a cell is not being hit, show '.'
        # if a cell is occupied by a fleet and didn't got hit, show 'O'
        # if a cell is occupied by a fleet and got hit, show 'X'
        # if a cell is not occupied by a fleet but was hit before, show '*'
        board_size = self.board_size
        board = [['.'] * board_size[1] for _ in range(board_size[0])]
        for fleet in self.fleet_group.fleets:
            for coord in fleet.coordinates:
                if coord in fleet.hits:
                    board[coord[0]][coord[1]] = 'X'
                else:
                    board[coord[0]][coord[1]] = 'O'
        for hit in self.was_hit_history:
            if not board[hit[0]][hit[1]] == 'O':
                board[hit[0]][hit[1]] = '*'
        # print the board
        # print 1-10 on the top
        print('  ' + ' '.join([str(i) for i in range(1, board_size[1] + 1)]))
        for i in range(board_size[0]):
            row = board[i]
            # print A-J on the left
            ch = chr(ord('A') + i)
            print(ch + ' ' + ' '.join(row))

    def show_enemy(self):
        # draw the board given hit_history, similar to show_myself
        board_size = self.board_size
        board = [['.'] * board_size[1] for _ in range(board_size[0])]
        for hit in self.hit_history:
            if hit[2]:
                board[hit[0]][hit[1]] = 'X'
            else:
                board[hit[0]][hit[1]] = '*'
        # print the board
        # print 1-10 on the top
        print('  ' + ' '.join([str(i) for i in range(1, board_size[1] + 1)]))
        for i in range(board_size[0]):
            row = board[i]
            # print A-J on the left
            ch = chr(ord('A') + i)
            print(ch + ' ' + ' '.join(row))

    def log_hit(self, x, y, hit_result):
        loc = self.encodeLoc(x, y)
        print(f"{self.name} shoot on {loc}: ({x}, {y})")
        if hit_result:
            print(f"{self.name} hit on " + hit_result)
        else:
            print(f"{self.name} miss")

    def print_board(self):
        print(f"{self.name}'s hit history: ")
        self.show_enemy()
        print("-------------------------------------")
        print(f"{self.name}'s board: ")
        self.show_myself()

    def encodeLoc(self, x, y):
        return chr(x + ord('A')) + str(y + 1)


class HumanPlayer(Player):

    def input_and_hit(self, player):
        while True:
            loc = input(f"{self.name}, enter location to hit (A1 to J10): ")
            if loc == 'exit' or loc == 'quit':
                print("Game ended")
                ## quit the whole program and back to shell
                import sys
                sys.exit()
            else:
                try:
                    x, y = self.decodeLoc(loc)
                    break
                except:
                    print("Invalid location, please try again")
                    pass
        hit_result = self.hit(player, x, y)
        self.log_hit(x, y, hit_result)
        self.print_board()
        print("=====================================")
        return hit_result

    def decodeLoc(self, loc):
        x = ord(loc[0].upper()) - ord('A')
        y = int(loc[1:]) - 1
        return x, y


class ComputerPlayer(Player):

    def input_and_hit(self, player):
        import random
        board_size = self.board_size
        hit_history = self.hit_history
        
        # 获取未击沉的船只大小列表
        remaining_sizes = []
        for fleet in player.fleet_group.fleets:
            if not fleet.is_sunk():
                remaining_sizes.append(fleet.size)
        
        # 找出所有连续命中但未击沉的点
        hits_in_progress = []
        hits_only = [(x, y) for x, y, hit in hit_history if hit]
        
        # 分析连续命中点的方向
        for x, y in hits_only:
            # 检查是否已经是已击沉的船只的一部分
            is_sunk = False
            for fleet in player.fleet_group.fleets:
                if fleet.is_sunk() and (x, y) in fleet.coordinates:
                    is_sunk = True
                    break
            if not is_sunk:
                hits_in_progress.append((x, y))
        
        if hits_in_progress:
            # 如果有连续命中点,分析可能的方向
            if len(hits_in_progress) > 1:
                # 确定方向
                points = sorted(hits_in_progress)
                if points[0][0] == points[1][0]:  # 水平方向
                    direction = 'horizontal'
                    x = points[0][0]
                    possible_y = [points[0][1]-1, points[-1][1]+1]
                    candidates = [(x, y) for y in possible_y if 0 <= y < board_size[1]]
                else:  # 垂直方向
                    direction = 'vertical'
                    y = points[0][1]
                    possible_x = [points[0][0]-1, points[-1][0]+1]
                    candidates = [(x, y) for x in possible_x if 0 <= x < board_size[0]]
            else:
                # 单个命中点,尝试四个方向
                x, y = hits_in_progress[0]
                candidates = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
                candidates = [(x,y) for x,y in candidates 
                             if 0 <= x < board_size[0] and 0 <= y < board_size[1]]
            
            # 过滤掉已经打击过的位置
            candidates = [(x,y) for x,y in candidates 
                         if (x,y,True) not in hit_history and (x,y,False) not in hit_history]
            
            if candidates:
                x, y = random.choice(candidates)
            else:
                # 如果没有合适的相邻点,随机选择
                x, y = self._random_shot(board_size, hit_history)
        else:
            # 没有进行中的命中,使用概率分布选择
            x, y = self._random_shot(board_size, hit_history)
        
        hit_result = self.hit(player, x, y)
        self.log_hit(x, y, hit_result)
        print(f"{player.name}'s board: ")
        player.show_myself()
        print("=====================================")
        return hit_result

    def _random_shot(self, board_size, hit_history):
        import random
        # 创建所有可能的坐标
        possible_hits = [(x, y) for x in range(board_size[0]) 
                        for y in range(board_size[1])
                        if (x,y,True) not in hit_history and (x,y,False) not in hit_history]
        
        # 优先选择间隔为2的点位(因为最小的船是2格)
        priority_hits = [(x,y) for x,y in possible_hits if (x+y)%2 == 0]
        
        if priority_hits:
            return random.choice(priority_hits)
        return random.choice(possible_hits)


class Game:

    def __init__(self, fleet_names, board_size):
        self.fleet_names = fleet_names
        self.board_size = board_size
        self.players = []

    def add_players(self, player1, player2):
        self.players.append(player1)
        print(f"{player1.name} join the game")
        self.players.append(player2)
        print(f"{player2.name} join the game")

    def run(self):
        player1, player2 = self.players
        n = 1
        while not player1.all_sunk() and not player2.all_sunk():
            print(f"Round {n}")
            hit_result = player1.input_and_hit(player2)
            if player2.all_sunk():
                print(f"{player1.name} wins!")
                break
            player2.input_and_hit(player1)
            if player1.all_sunk():
                print(f"{player2.name} wins!")
                break

    def two_player_games(self):
        player1 = HumanPlayer("You", self.board_size)
        player2 = ComputerPlayer("Play Kang", self.board_size)
        player1.random_init(self.fleet_names)
        player2.random_init(self.fleet_names)
        self.add_players(player1, player2)
        self.run()


if __name__ == '__main__':
    game = Game(['carrier', 'battleship', 'cruiser', 'submarine', 'destroyer'
                 # ,'patrol boat', 'boater', 'bomber', 'speed boat', 'smasher', 'f-1'
                 ],
                # (10, 10)
                (7, 7)
                )
    game.two_player_games()
