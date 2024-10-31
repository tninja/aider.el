
import unittest

from fleet import FleetGroup, Fleet


class TestFleetGroup(unittest.TestCase):

    def test_hit_and_sunk(self):
        fleet = Fleet(0, 0, 2, 'horizontal', 'destroyer')
        self.assertFalse(fleet.is_sunk())
        fleet.hit(0, 0)
        self.assertFalse(fleet.is_sunk())
        fleet.hit(0, 1)
        self.assertTrue(fleet.is_sunk())

    def test_hit_and_sunk_battleship(self):
        fleet = Fleet(0, 0, 5, 'horizontal', 'battleship')
        self.assertFalse(fleet.is_sunk())
        fleet.hit(0, 0)
        self.assertFalse(fleet.is_sunk())
        fleet.hit(0, 1)
        self.assertFalse(fleet.is_sunk())
        fleet.hit(0, 2)
        self.assertFalse(fleet.is_sunk())
        fleet.hit(0, 3)
        self.assertFalse(fleet.is_sunk())
        fleet.hit(0, 4)
        self.assertTrue(fleet.is_sunk())
