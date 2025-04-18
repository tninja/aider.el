
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
        
        # get sizes of unsunk ships
        remaining_sizes = []
        for fleet in player.fleet_group.fleets:
            if not fleet.is_sunk():
                remaining_sizes.append(fleet.size)
        
        # get all consecutive hit points that are not sunk
        hits_in_progress = []
        hits_only = [(x, y) for x, y, hit in hit_history if hit]
        
        # Analyze the orientation of consecutive hit points
        for x, y in hits_only:
            # check if this hit is part of a sunk ship
            is_sunk = False
            for fleet in player.fleet_group.fleets:
                if fleet.is_sunk() and (x, y) in fleet.coordinates:
                    is_sunk = True
                    break
            if not is_sunk:
                hits_in_progress.append((x, y))
        
        if hits_in_progress:
            # If there are multiple consecutive hit points, analyze possible orientation
            if len(hits_in_progress) > 1:
                # Determine the orientation
                points = sorted(hits_in_progress)
                if points[0][0] == points[1][0]:  # horizontal direction
                    direction = 'horizontal'
                    x = points[0][0]
                    possible_y = [points[0][1]-1, points[-1][1]+1]
                    candidates = [(x, y) for y in possible_y if 0 <= y < board_size[1]]
                else:  # vertical direction
                    direction = 'vertical'
                    y = points[0][1]
                    possible_x = [points[0][0]-1, points[-1][0]+1]
                    candidates = [(x, y) for x in possible_x if 0 <= x < board_size[0]]
            else:
                # For a single hit point, try all four directions
                x, y = hits_in_progress[0]
                candidates = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
                candidates = [(x,y) for x,y in candidates 
                             if 0 <= x < board_size[0] and 0 <= y < board_size[1]]
            
            # Filter out positions that have already been targeted
            candidates = [(x,y) for x,y in candidates 
                         if (x,y,True) not in hit_history and (x,y,False) not in hit_history]
            
            if candidates:
                x, y = random.choice(candidates)
            else:
                # If no valid adjacent cell is available, choose randomly
                x, y = self._random_shot(board_size, hit_history)
        else:
            # If there are no ongoing hit sequences, use a random shot
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


class HardComputerPlayer(ComputerPlayer):
    """
    Hard mode: use probability density strategy for selecting the best move
    """

    def input_and_hit(self, player):
        # Get board size and hit history
        board_size = self.board_size
        hit_history = self.hit_history

        # get lengths of all opponent's unsunk ships
        remaining_ships = [fleet.size for fleet in player.fleet_group.fleets if not fleet.is_sunk()]

        # initialize probability grid with zeros
        prob_grid = [[0] * board_size[1] for _ in range(board_size[0])]

        # record shot coordinates; elements in hit_history are tuples (x, y, bool)
        shot_cells = {(x, y): True for x, y, _ in hit_history}

        # 对于每个剩余舰船的长度，尝试水平和垂直的放置，并累加概率分值
        for size in remaining_ships:
            for x in range(board_size[0]):
                for y in range(board_size[1]):
                    # horizontal placement: check if cells from (x, y) to (x, y+size-1) are within boundaries and not shot
                    if y + size <= board_size[1]:
                        if all((x, y + offset) not in shot_cells for offset in range(size)):
                            for offset in range(size):
                                prob_grid[x][y + offset] += size  # Increase weight based on ship size

                    # vertical placement: check if cells from (x, y) to (x+size-1, y) are within boundaries and not shot
                    if x + size <= board_size[0]:
                        if all((x + offset, y) not in shot_cells for offset in range(size)):
                            for offset in range(size):
                                prob_grid[x + offset][y] += size  # Increase weight based on ship size

        # Add bonus for cells adjacent to unsunk hit cells
        bonus = 3  # Bonus weight for adjacent cell
        for x, y, hit in hit_history:
            if hit:  # cell was a hit
                # Check if this hit cell belongs to an unsunk fleet
                unsunk = False
                for fleet in player.fleet_group.fleets:
                    if (x, y) in fleet.coordinates and not fleet.is_sunk():
                        unsunk = True
                        break
                if unsunk:
                    for dx, dy in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
                        nx, ny = x + dx, y + dy
                        if 0 <= nx < board_size[0] and 0 <= ny < board_size[1] and (nx, ny) not in shot_cells:
                            prob_grid[nx][ny] += bonus  # Boost adjacent cells near successful hit
        max_prob = -1
        candidates = []
        for x in range(board_size[0]):
            for y in range(board_size[1]):
                if (x, y) not in shot_cells:
                    cell_prob = prob_grid[x][y]
                    if cell_prob > max_prob:
                        max_prob = cell_prob
                        candidates = [(x, y)]
                    elif cell_prob == max_prob:
                        candidates.append((x, y))

        import random
        if candidates:
            x, y = random.choice(candidates)
        else:
            # 如果没有候选,退回到父类的随机策略
            x, y = self._random_shot(board_size, hit_history)

        hit_result = self.hit(player, x, y)
        self.log_hit(x, y, hit_result)
        print(f"{player.name}'s board: ")
        player.show_myself()
        print("=====================================")
        return hit_result

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

    def computer_vs_hard(self, normal_first=True):
        """
        # Start a game with ComputerPlayer vs HardComputerPlayer
        """
        player1 = ComputerPlayer("Normal CPU", self.board_size)
        player2 = HardComputerPlayer("Hard CPU", self.board_size)
        player1.random_init(self.fleet_names)
        player2.random_init(self.fleet_names)
        if normal_first:
            self.add_players(player1, player2)
        else:
            self.add_players(player2, player1)
        self.run()


if __name__ == '__main__':
    game = Game(['carrier', 'battleship', 'cruiser', 'submarine', 'destroyer'
                 # ,'patrol boat', 'boater', 'bomber', 'speed boat', 'smasher', 'f-1'
                 ],
                # (10, 10)
                (7, 7)
                )
    game.two_player_games()
