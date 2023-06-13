import math


def find_first_set_bit(n):
    return int(math.log2(n & -n))

lines = """
Contravariant 	2 	

The generic type parameter is contravariant. A contravariant type parameter can appear as a parameter type in method signatures.
Covariant 	1 	

The generic type parameter is covariant. A covariant type parameter can appear as the result type of a method, the type of a read-only field, a declared base type, or an implemented interface.
DefaultConstructorConstraint 	16 	

A type can be substituted for the generic type parameter only if it has a parameterless constructor.
None 	0 	

There are no special flags.
NotNullableValueTypeConstraint 	8 	

A type can be substituted for the generic type parameter only if it is a value type and is not nullable.
ReferenceTypeConstraint 	4 	

A type can be substituted for the generic type parameter only if it is a reference type.
SpecialConstraintMask 	28 	

Selects the combination of all special constraint flags. This value is the result of using logical OR to combine the following flags: DefaultConstructorConstraint, ReferenceTypeConstraint, and NotNullableValueTypeConstraint.
VarianceMask 	3 	

Selects the combination of all variance flags. This value is the result of using logical OR to combine the following flags: Contravariant and Covariant.
""".splitlines()

values = []

siz = 16

for line in lines:
    tokens = line.split()
    if len(tokens) == 2:
        try:
            name = tokens[0]
            mask = int(tokens[1])
            if mask == 0:
                continue
            if 'Reserved' in name:
                continue
            values.append((mask, name))
        except:
            pass

# Remove values that are under a mask
values = sorted(values)
masks = set()
for value, name in values:
    if 'Mask' not in name:
        continue

    for sub_value, sub_name in values:
        if name == sub_name:
            continue
        if (value & sub_value) == sub_value:
            first_bit = find_first_set_bit(value)
            print(f"REMOVING {sub_name} because it is under {name} with value {sub_value >> first_bit}")
            masks.add(sub_name)

last_bit_end = 0
for value, name in values:
    if name in masks:
        continue

    first_bit = find_first_set_bit(value)
    bit_count = (value >> first_bit).bit_count()
    bit_mask = (1 << bit_count) - 1
    assert (bit_mask << first_bit) == value, f"non contig bits in {bin(value)} != {bin(bit_mask << first_bit)}"
    if first_bit != last_bit_end:
        print(f"uint{siz}_t : {first_bit - last_bit_end};")
    print(f"uint{siz}_t {name} : {bit_count};")
    last_bit_end = first_bit + bit_count

assert last_bit_end <= siz
if last_bit_end != siz:
    print(f"uint{siz}_t : {siz - last_bit_end};")



# import math
# 
# 
# class Monster:
# 
#     def __init__(self, name, speed):
#         self.name = name
#         self.speed = speed
# 
#     def __repr__(self):
#         return f'Monster({repr(self.name)}, {repr(self.speed)})'
# 
# 
# A1 = Monster('A1', 500)
# A2 = Monster('A2', 100)
# A3 = Monster('A3', 300)
# 
# B1 = Monster('B1', 70)
# B2 = Monster('B2', 220)
# B3 = Monster('B3', 100)
# 
# monsters = [A1, A2, A3, B1, B2, B3]
# monsters = sorted(monsters, key=lambda x: x.speed, reverse=True)
# 
# # m1 = monsters[0]
# # m2 = monsters[1]
# # rest = monsters[2:]
# #
# 
# print(monsters)
# 
# 
# def do_map(factor):
#     if factor in [2, 3, 4]:
#         return 2
#     elif factor in [5, 6, 7, 8]:
#         return 3
#     else:
#         return math.isqrt(factor)
# 
# 
# for i in range(len(monsters)):
#     current = monsters[i]
# 
#     higher = monsters[:max(i, 0)]
#     for hi in higher:
#         factor = int(hi.speed / current.speed)
#         if factor <= 1:
#             break
#         print(f'{do_map(factor)} x {hi}')
# 
#     print('>>> ' + repr(current))
# 
# 
# [
#     {
#         'fo'
#     }
# ]
# 
# 
# print(monsters)
# 
# 
# 
# 
