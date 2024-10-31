import unittest
from unittest.mock import patch
from io import StringIO
from game import Board, Player, HumanPlayer, ComputerPlayer, Game

class TestBoard(unittest.TestCase):
    def setUp(self):
        self.board = Board()
        
    def test_init(self):
        """Test board initialization"""
        self.assertEqual(self.board.rows, 6)
        self.assertEqual(self.board.cols, 7)
        self.assertEqual(len(self.board.board), 6)
        self.assertEqual(len(self.board.board[0]), 7)
        self.assertTrue(all(cell == 0 for row in self.board.board for cell in row))
        
    def test_is_valid_move(self):
        """Test valid move checking"""
        # Valid moves
        self.assertTrue(self.board.is_valid_move(0))
        self.assertTrue(self.board.is_valid_move(6))
        
        # Invalid moves
        self.assertFalse(self.board.is_valid_move(-1))
        self.assertFalse(self.board.is_valid_move(7))
        
        # Fill a column and test
        for _ in range(6):
            self.board.drop_token(0, 1)
        self.assertFalse(self.board.is_valid_move(0))
        
    def test_drop_token(self):
        """Test token dropping"""
        # Drop in empty column
        result = self.board.drop_token(0, 1)
        self.assertEqual(result, (5, 0))
        self.assertEqual(self.board.board[5][0], 1)
        
        # Drop on top of another token
        result = self.board.drop_token(0, 2)
        self.assertEqual(result, (4, 0))
        self.assertEqual(self.board.board[4][0], 2)
        
        # Drop in full column
        for _ in range(4):
            self.board.drop_token(0, 1)
        result = self.board.drop_token(0, 1)
        self.assertIsNone(result)
        
    def test_check_win(self):
        """Test win condition checking"""
        # Horizontal win
        for i in range(4):
            self.board.drop_token(i, 1)
        self.assertTrue(self.board.check_win(5, 3, 1))
        
        # Vertical win
        self.board = Board()  # Reset board
        for i in range(4):
            self.board.drop_token(0, 1)
        self.assertTrue(self.board.check_win(2, 0, 1))
        
        # Diagonal win
        self.board = Board()  # Reset board
        for i in range(4):
            for j in range(i):
                self.board.drop_token(i, 2)
            self.board.drop_token(i, 1)
        self.assertTrue(self.board.check_win(2, 2, 1))
        
    def test_is_full(self):
        """Test board full condition"""
        self.assertFalse(self.board.is_full())
        
        # Fill the board
        for col in range(7):
            for _ in range(6):
                self.board.drop_token(col, 1)
        self.assertTrue(self.board.is_full())

class TestHumanPlayer(unittest.TestCase):
    def setUp(self):
        self.board = Board()
        self.player = HumanPlayer(1)
        
    @patch('builtins.input', side_effect=['4'])
    def test_get_move_valid(self, mock_input):
        """Test valid human move input"""
        move = self.player.get_move(self.board)
        self.assertEqual(move, 3)  # Convert from 1-based to 0-based
        
    @patch('builtins.input', side_effect=['8', '0', 'a', '4'])
    def test_get_move_invalid(self, mock_input):
        """Test invalid human move inputs"""
        move = self.player.get_move(self.board)
        self.assertEqual(move, 3)

class TestComputerPlayer(unittest.TestCase):
    def setUp(self):
        self.board = Board()
        self.player = ComputerPlayer(2)
        
    def test_get_move(self):
        """Test computer move generation"""
        move = self.player.get_move(self.board)
        self.assertTrue(0 <= move < self.board.cols)
        self.assertTrue(self.board.is_valid_move(move))
        
    def test_winning_move(self):
        """Test computer finds winning move"""
        # Set up a winning position
        for i in range(3):
            self.board.drop_token(i, 2)
        move = self.player.get_move(self.board)
        self.assertEqual(move, 3)
        
    def test_blocking_move(self):
        """Test computer blocks opponent's winning move"""
        # Set up opponent's winning position
        for i in range(3):
            self.board.drop_token(i, 1)
        move = self.player.get_move(self.board)
        self.assertEqual(move, 3)

class TestGame(unittest.TestCase):
    def setUp(self):
        self.player1 = ComputerPlayer(1)
        self.player2 = ComputerPlayer(2)
        self.game = Game(self.player1, self.player2)
        
    def test_init(self):
        """Test game initialization"""
        self.assertIsInstance(self.game.board, Board)
        self.assertEqual(len(self.game.players), 2)
        self.assertEqual(self.game.current_player, 0)
        
    @patch('sys.stdout', new_callable=StringIO)
    def test_play_game(self, mock_stdout):
        """Test game play until win or draw"""
        self.game.play()
        output = mock_stdout.getvalue()
        self.assertTrue("wins!" in output or "draw!" in output)

if __name__ == '__main__':
    unittest.main()
