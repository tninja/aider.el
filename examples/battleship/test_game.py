
import unittest

from game import Player


class TestPlayer(unittest.TestCase):

    def test_player_random_init(self):
        player = Player("Play Kang")
        player.random_init()
        player.show_myself()
        print()
        player.show_enemy()
        # player.fleet_group.show()
