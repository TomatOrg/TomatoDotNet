

class StringPrinter:

    def __init__(self, val):
        self._val = val
        self._children = None

    # NOTE: CLion is stupid, and when children exists it does not display to_string,
    #       so for now don't implement it for string, not that useful on it anyways
    # def children(self):
    #     if self._children is None:
    #         length = self._val['Length']
    #         chars = self._val['Chars']
    #         char = gdb.lookup_type('System_Char')
    #         char_pointer = char.pointer()
    #         char_array = char.array(length - 1)
    #         self._children = [
    #             ('Length', length),
    #             ('Chars', chars.cast(char_pointer).dereference().cast(char_array))
    #         ]
    #     return self._children

    def display_hint(self):
        return 'string'

    def to_string(self):
        s = ''
        chars = self._val['Chars']
        for i in range(self._val['Length']):
            s += chr(chars[i])
        return s


class ArrayPrinter:

    def __init__(self, val):
        self._val = val
        self._children = None

    def children(self):
        if self._children is None:
            base = self._val['']
            length = base['Length']
            entries = self._val['Elements']
            entry_pointer = entries.type.target().pointer()
            entry_array = entries.type.target().array(length - 1)
            self._children = [
                # TODO: object header nicely
                ('Length', length),
                ('Elements', entries.cast(entry_pointer).dereference().cast(entry_array))
            ]
        return self._children

    def display_hint(self):
        return 'array'


def my_pp_func(val):
    s = str(val.type)
    if s == 'System_String':
        if int(val) != 0:
            return StringPrinter(val)
    elif s.startswith('struct System_') and s.endswith('_Array'):
        return ArrayPrinter(val)
    return None


gdb.pretty_printers.append(my_pp_func)
