import subprocess
import os

CURRENT_DIR = os.path.dirname(os.path.abspath(__file__))
TDN_BINARY = os.path.join(CURRENT_DIR, '..', 'host', 'linux', 'out', 'bin', 'tdn.elf')
TDN_CORELIB = os.path.join(CURRENT_DIR, '..', 'TdnCoreLib', 'System.Private.CoreLib', 'bin', 'Release', 'net8.0', 'System.Private.CoreLib.dll')

IGNORE_DLLS = [
    'System.Private.CoreLib.dll'
]

SKIP = [
    # TODO: floating point support
    'FPAdd.csproj',
    'FPArea.csproj',
    'FPAvg2.csproj',
    'FPAvg6.csproj',
    'FPDist.csproj',
    'FPDiv.csproj',
    'FPMath.csproj',
    'FPMul.csproj',
    'FPNeg.csproj',
    'FPRem.csproj',
    'FPSub.csproj',
    'FPVar.csproj',

    # TODO: double support
    'DblAdd.csproj',
    'DblDiv.csproj',
    'DblMul.csproj',
    'DblNeg.csproj',
    'DblRem.csproj',
    'DblSub.csproj',
    'DblVar.csproj',

    # TODO: exception support
    'div2.csproj',
]


TEST_FOLDERS = set()


def build_test(dirpath: str, name: str, target: str):
    p = os.path.join(dirpath, 'bin', target, 'net8.0')
    TEST_FOLDERS.add(p)
    if not os.path.exists(os.path.join(p, name + '.dll')):
        subprocess.check_call(['dotnet', 'build', '--configuration', target, os.path.join(dirpath, name + '.csproj')])


def main():
    for dirpath, dirnames, filenames in os.walk(CURRENT_DIR):
        for filename in filenames:
            if filename in SKIP:
                continue


            if filename.endswith('.csproj'):
                name = filename[:-7]

                build_test(dirpath, name, 'Debug')
                build_test(dirpath, name, 'Release')

    for test_dir in TEST_FOLDERS:
        for filename in os.listdir(test_dir):
            if not filename.endswith('.dll') or filename in IGNORE_DLLS:
                continue

            test_path = os.path.join(test_dir, filename)
            print(f'Running test {test_path}')
            assert subprocess.call(
                [
                    TDN_BINARY, TDN_CORELIB, CURRENT_DIR, test_path
                ],
            ) == 100, f"Test {test_path} failed"


if __name__ == '__main__':
    main()
