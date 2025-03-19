import glob
import subprocess
import os

CURRENT_DIR = os.path.dirname(os.path.abspath(__file__))
TDN_BINARY = os.path.join(CURRENT_DIR, '..', 'host', 'linux', 'out', 'bin', 'tdn.elf')
TDN_CORELIB = os.path.join(CURRENT_DIR, '..', 'TdnCoreLib', 'System.Private.CoreLib', 'bin', 'Release', 'net8.0', 'System.Private.CoreLib.dll')

def main():
    # build everything
    assert os.system(f'cd {CURRENT_DIR} && dotnet build --configuration Release') == 0
    assert os.system(f'cd {CURRENT_DIR} && dotnet build --configuration Debug') == 0

    # get all the dlls to run
    debug_dlls = glob.glob('**/bin/Debug/net8.0/*.dll', root_dir=CURRENT_DIR, recursive=True)
    release_dlls = glob.glob('**/bin/Release/net8.0/*.dll', root_dir=CURRENT_DIR, recursive=True)

    dlls_to_run = []
    for dll in debug_dlls:
        if 'System.Private.CoreLib.dll' in dll:
            continue
        dlls_to_run.append(dll)
    for dll in release_dlls:
        if 'System.Private.CoreLib.dll' in dll:
            continue
        dlls_to_run.append(dll)

    for dll in dlls_to_run:
        print(f'Running test {dll}')
        assert subprocess.call(
            [
                TDN_BINARY, TDN_CORELIB, CURRENT_DIR, os.path.join(CURRENT_DIR, dll)
            ],
        ) == 100, f"Test {dll} failed"


if __name__ == '__main__':
    main()
