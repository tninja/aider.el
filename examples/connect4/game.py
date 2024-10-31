import random
from typing import List, Optional, Tuple

class Board:
    """Connect Four game board representation"""
    
    def __init__(self, rows: int = 6, cols: int = 7):
        self.rows = rows
        self.cols = cols
        self.board = [[0 for _ in range(cols)] for _ in range(rows)]
        
    def display(self):
        """Display the current board state"""
        symbols = {0: '.', 1: 'X', 2: 'O'}
        for row in self.board:
            print(' '.join(symbols[cell] for cell in row))
        print(' '.join(str(i+1) for i in range(self.cols)))
        print('===========================================')
        
    def is_valid_move(self, col: int) -> bool:
        """Check if a move is valid"""
        return 0 <= col < self.cols and self.board[0][col] == 0
        
    def drop_token(self, col: int, player: int) -> Optional[Tuple[int, int]]:
        """Drop a token in the specified column"""
        if not self.is_valid_move(col):
            return None
            
        # Find the lowest empty position
        for row in range(self.rows-1, -1, -1):
            if self.board[row][col] == 0:
                self.board[row][col] = player
                return (row, col)
        return None
        
    def check_win(self, row: int, col: int, player: int) -> bool:
        """Check if the last move at (row, col) created a winning line"""
        directions = [(0,1), (1,0), (1,1), (1,-1)]  # horizontal, vertical, diagonal
        
        for dr, dc in directions:
            count = 1
            # Check forward direction
            r, c = row + dr, col + dc
            while 0 <= r < self.rows and 0 <= c < self.cols and self.board[r][c] == player:
                count += 1
                r, c = r + dr, c + dc
                
            # Check backward direction
            r, c = row - dr, col - dc
            while 0 <= r < self.rows and 0 <= c < self.cols and self.board[r][c] == player:
                count += 1
                r, c = r - dr, c - dc
                
            if count >= 4:
                return True
        return False
        
    def is_full(self) -> bool:
        """Check if the board is full"""
        return all(cell != 0 for row in self.board for cell in row)

class Player:
    """Base class for players"""
    def __init__(self, number: int, name: str):
        self.number = number
        self.name = name
        
    def get_move(self, board: Board) -> int:
        raise NotImplementedError

class HumanPlayer(Player):
    """Human player implementation"""
    def get_move(self, board: Board) -> int:
        while True:
            try:
                col = int(input(f"{self.name}, choose column (1-{board.cols}): ")) - 1
                if board.is_valid_move(col):
                    return col
                print("Invalid move, try again.")
            except ValueError:
                print("Please enter a valid number.")

class ComputerPlayer(Player):
        
    def get_move(self, board: Board) -> int:
        valid_cols = [col for col in range(board.cols) if board.is_valid_move(col)]
        opponent = 1 if self.number == 2 else 2
        
        # 评估每个可能的移动
        moves = []  # 存储(列号,分数)对
        
        for col in valid_cols:
            board_copy = self._copy_board(board)
            row = self._simulate_move(board_copy, col, self.number)
            if row == -1:
                continue
                
            score = self._evaluate_move(board_copy, row, col, self.number)
            
            # 如果这步棋能赢，直接返回
            if board_copy.check_win(row, col, self.number):
                return col
                
            # 检查对手在这个位置是否能赢
            board_copy2 = self._copy_board(board)
            row2 = self._simulate_move(board_copy2, col, opponent)
            if row2 != -1 and board_copy2.check_win(row2, col, opponent):
                score += 100  # 高优先级阻止对手获胜
                
            # 检查这步棋是否会给对手创造获胜机会
            if row > 0:  # 如果上面还有空间
                board_copy.board[row-1][col] = opponent
                if board_copy.check_win(row-1, col, opponent):
                    score -= 80  # 避免给对手创造获胜机会
                board_copy.board[row-1][col] = 0
                
            # 优先选择中间列
            if col == board.cols // 2:
                score += 3
            elif col in [board.cols//2-1, board.cols//2+1]:
                score += 2
                
            # 添加一些随机性
            score += random.uniform(-1, 1)
            
            moves.append((col, score))
            
        # 选择最高分的移动
        if not moves:
            return valid_cols[0]
            
        # 找出得分最高的移动
        max_score = max(score for _, score in moves)
        best_moves = [col for col, score in moves if score >= max_score - 2]  # 允许差距2分以内的移动
        
        return random.choice(best_moves)
        
    def _copy_board(self, board: Board) -> Board:
        """创建棋盘的深拷贝"""
        new_board = Board(board.rows, board.cols)
        for i in range(board.rows):
            for j in range(board.cols):
                new_board.board[i][j] = board.board[i][j]
        return new_board
        
    def _simulate_move(self, board: Board, col: int, player: int) -> int:
        """模拟在指定列放置棋子，返回行号"""
        for row in range(board.rows-1, -1, -1):
            if board.board[row][col] == 0:
                board.board[row][col] = player
                return row
        return -1
        
    def _evaluate_move(self, board: Board, row: int, col: int, player: int) -> float:
        """评估一个位置的分数"""
        score = 0
        opponent = 1 if player == 2 else 2
        directions = [(0,1), (1,0), (1,1), (1,-1)]
        
        # 评估所有方向
        for dr, dc in directions:
            # 计算我方连续子数
            my_count = 1
            space_after = 0
            space_before = 0
            
            # 正向检查
            r, c = row + dr, col + dc
            while 0 <= r < board.rows and 0 <= c < board.cols:
                if board.board[r][c] == player:
                    my_count += 1
                elif board.board[r][c] == 0:
                    space_after += 1
                    break
                else:
                    break
                r, c = r + dr, c + dc
            
            # 反向检查
            r, c = row - dr, col - dc
            while 0 <= r < board.rows and 0 <= c < board.cols:
                if board.board[r][c] == player:
                    my_count += 1
                elif board.board[r][c] == 0:
                    space_before += 1
                    break
                else:
                    break
                r, c = r - dr, c - dc
            
            # 根据连续子数和空间评分
            if my_count >= 4:
                score += 1000
            elif my_count == 3 and (space_before > 0 or space_after > 0):
                score += 50
            elif my_count == 2 and space_before > 0 and space_after > 0:
                score += 10
                
            # 检查对手在这个方向的威胁
            opp_count = 1
            r, c = row + dr, col + dc
            while 0 <= r < board.rows and 0 <= c < board.cols and board.board[r][c] == opponent:
                opp_count += 1
                r, c = r + dr, c + dc
            
            r, c = row - dr, col - dc
            while 0 <= r < board.rows and 0 <= c < board.cols and board.board[r][c] == opponent:
                opp_count += 1
                r, c = r - dr, c - dc
                
            if opp_count >= 3:
                score += 60  # 高优先级阻止对手
        
        return score

class Game:
    """Connect Four game controller"""
    def __init__(self, player1, player2):
        self.board = Board()
        self.players = [player1, player2]
        self.current_player = 0
        
    def play(self):
        """Main game loop"""
        n = 0
        while True:
            if self.current_player == 0:
                n += 1
                print('round ' + str(n))
            self.board.display()
            player = self.players[self.current_player]
            
            # Get player's move
            col = player.get_move(self.board)
            print(f"{player.name} drop at column {col+1}")
            result = self.board.drop_token(col, player.number)
            
            if result is None:
                print("Invalid move!")
                continue
                
            row, col = result
            
            # Check for win
            if self.board.check_win(row, col, player.number):
                self.board.display()
                print(f"{player.name} wins!")
                break
                
            # Check for draw
            if self.board.is_full():
                self.board.display()
                print("Game is a draw!")
                break
                
            # Switch players
            self.current_player = 1 - self.current_player

if __name__ == "__main__":
    player1 = HumanPlayer(1, "You")
    player2 = ComputerPlayer(2, "Play Kang")
    game = Game(player1, player2)
    game.play()
