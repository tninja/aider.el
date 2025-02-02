
import unittest

from game import Player, Game, ComputerPlayer, HardComputerPlayer


class TestPlayer(unittest.TestCase):

    def test_player_random_init(self):
        player = Player("Play Kang")
        player.random_init()
        player.show_myself()
        print()
        player.show_enemy()
        # player.fleet_group.show()

    def test_computer_vs_hard(self):
        """
        # Unit test for battle between ComputerPlayer and HardComputerPlayer
        """
        fleet_names = ['carrier', 'battleship', 'cruiser', 'submarine', 'destroyer']
        board_size = (10, 10)
        normal_wins = hard_wins = 0
        for i in range(100):
            game = Game(fleet_names, board_size)
            if i % 2 == 0:
                game.computer_vs_hard(normal_first=True)
                if game.players[0].all_sunk():
                    normal_wins += 1
                if game.players[1].all_sunk():
                    hard_wins += 1
            else:
                game.computer_vs_hard(normal_first=False)
                if game.players[0].all_sunk():
                    hard_wins += 1
                if game.players[1].all_sunk():
                    normal_wins += 1
        print(f"Normal wins: {normal_wins}")
        print(f"Hard wins: {hard_wins}")

# Normal wins: 31
# Hard wins: 69
