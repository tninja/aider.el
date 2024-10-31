
class Fleet:
    def __init__(self, x, y, size, direction, name):
        self.coordinates = []
        self.name = name
        self.size = size
        self.direction = direction
        self.hits = []

        if direction == 'vertical':
            for i in range(size):
                self.coordinates.append((x + i, y))
        elif direction == 'horizontal':
            for i in range(size):
                self.coordinates.append((x, y + i))

    def hit(self, x, y):
        if (x, y) in self.coordinates and (x, y) not in self.hits:
            self.hits.append((x, y))
            print(f"The {self.name} has been hitted, total life is {self.size}, rest life is {self.size - len(self.hits)}.")
            if self.is_sunk():
                print(f"The {self.name} is destroyed.")
            return True
        return False

    def is_sunk(self):
        return len(self.hits) == self.size

    def show(self):
        print(f"Name: {self.name}")
        print(f"Size: {self.size}")
        print("Coordinates:")
        for coord in self.coordinates:
            status = "Hit" if coord in self.hits else "Not Hit"
            print(f"  {coord}: {status}")


class FleetGroup:
    def __init__(self):
        self.fleets = []

    def add(self, fleet):
        self.fleets.append(fleet)

    def hit(self, x, y):
        for fleet in self.fleets:
            if fleet.hit(x, y):
                return fleet.name
        return None

    def all_sunk(self):
        for fleet in self.fleets:
            if not fleet.is_sunk():
                return False
        return True

    def show(self):
        for fleet in self.fleets:
            fleet.show()
            print()  # 添加一个空行以分隔不同的舰队

