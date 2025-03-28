import argparse
from multiprocessing import Manager, Pool, Queue
from tempfile import mktemp
from typing import List
import subprocess
import glob
import time
import sys
import os


ILASM_PATH = '/home/tomato/Downloads/runtime.linux-x64.microsoft.netcore.ilasm.10.0.0-preview.2.25163.2/runtimes/linux-x64/native/ilasm'

CURRENT_DIR = os.path.dirname(os.path.abspath(__file__))
TDN_BINARY = os.path.join(CURRENT_DIR, '..', 'host', 'linux', 'out', 'bin', 'tdn.elf')


def build_tests():
    assert os.system(f'cd {CURRENT_DIR} && dotnet build --configuration Release') == 0
    assert os.system(f'cd {CURRENT_DIR} && dotnet build --configuration Debug') == 0


def gather_tests(dlls_to_include: List[str], il_verify: False):
    # get all the dlls to run
    debug_dlls = glob.glob('**/bin/Debug/net8.0/*.dll', root_dir=CURRENT_DIR, recursive=True)
    release_dlls = glob.glob('**/bin/Release/net8.0/*.dll', root_dir=CURRENT_DIR, recursive=True)
    ils = glob.glob('**/*.il', root_dir=CURRENT_DIR, recursive=True)

    # testse we never want to run
    dlls_to_ignore = [
        'System.Private.CoreLib.dll'
    ]

    if not il_verify:
        dlls_to_ignore.append('ilverify')

    # only tests we want to run
    if len(dlls_to_include) == 0:
        dlls_to_include = None

    # filter the tests
    dlls_to_run = []
    for dll in debug_dlls + release_dlls + ils:
        for ignore in dlls_to_ignore:
            if ignore in dll:
                break
        else:
            if dlls_to_include is not None:
                for include in dlls_to_include:
                    if include in dll:
                        dlls_to_run.append(dll)
                        break
            else:
                dlls_to_run.append(dll)

    return dlls_to_run


def compile_to_dll(path: str):
    if path.endswith('.dll'):
        return path

    path = os.path.join(CURRENT_DIR, path)
    dll_path = os.path.join(os.path.dirname(path), 'bin', os.path.basename(path)[:-2] + 'dll')
    os.makedirs(os.path.dirname(dll_path), exist_ok=True)
    assert os.system(f'{ILASM_PATH} -dll -output={dll_path} {path}') == 0
    return os.path.relpath(dll_path, CURRENT_DIR)


def run_single_test(dll: str, results: Queue) -> bool:
    try:
        dll = compile_to_dll(dll)

        timeout = False
        start_time = time.time()
        proc = subprocess.Popen(
            [
                TDN_BINARY,
                '--jit-verify-verbose',
                '--search-path', 'tests/JIT/CodeGenBringUpTests/bin/Release/net8.0',
                os.path.join(CURRENT_DIR, dll)
            ],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )

        try:
            stdout, stderr = proc.communicate()
        except subprocess.TimeoutExpired:
            proc.kill()
            stdout, stderr = proc.communicate()
            timeout = True

        elapsed_time = time.time() - start_time

        if proc.returncode == 100:
            results.put((True, dll, elapsed_time))
            return True
        else:
            results.put((False, dll, elapsed_time, stdout, stderr, timeout))
            return False

    except Exception as e:
        results.put((False, dll, 0, '', e, False))


def run_single_ilverify_test(dll: str, results: Queue) -> bool:
    try:
        dll = compile_to_dll(dll)

        timeout = False
        start_time = time.time()
        proc = subprocess.Popen(
            [
                TDN_BINARY,
                '--jit-verify-verbose',
                '--search-path', 'tests/JIT/CodeGenBringUpTests/bin/Release/net8.0',
                '--ilverify-test', os.path.join(CURRENT_DIR, dll)
            ],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )

        try:
            stdout, stderr = proc.communicate()
        except subprocess.TimeoutExpired:
            proc.kill()
            stdout, stderr = proc.communicate()
            timeout = True

        elapsed_time = time.time() - start_time

        if proc.returncode == 0:
            results.put((True, dll, elapsed_time))
            return True
        else:
            results.put((False, dll, elapsed_time, stdout, stderr, timeout))
            return False

    except Exception as e:
        results.put((False, dll, 0, '', e, False))


def run_tests(cases: List[str], parallelism: int) -> bool:
    pass_count = 0
    fail_count = 0
    start_time = time.time()

    def print_test_header(case: str, success: bool, timeout: bool, elapsed: float) -> None:
        status_str = "OK" if success else "TIMEOUT" if timeout else "FAILED"
        print(f"[{pass_count}/{len(cases)}] {case} {status_str} in {elapsed:.2f}s", flush=True)

    def format_output(data: str) -> str:
        return "\n".join(["\t" + line for line in data.split("\n")])

    with Pool(processes=parallelism) as pool:
        manager = Manager()
        result_queue = manager.Queue()

        pool.starmap_async(run_single_test, [(case, result_queue) for case in cases if 'ilverify' not in case])
        pool.starmap_async(run_single_ilverify_test, [(case, result_queue) for case in cases if 'ilverify' in case])

        while pass_count + fail_count < len(cases):
            success, case, elapsed_time, *args = result_queue.get()

            if success:
                pass_count += 1

                print_test_header(case, True, False, elapsed_time)
            else:
                fail_count += 1
                stdout, stderr, timeout = args

                print_test_header(case, False, timeout, elapsed_time)

                if isinstance(stderr, Exception):
                    raise stderr

                if isinstance(stdout, bytes):
                    stdout = stdout.decode('utf-8')

                if isinstance(stderr, bytes):
                    stderr = stderr.decode('utf-8')

                stdout_output = format_output(stdout)
                stderr_output = format_output(stderr)

                if stdout_output:
                    print(f"STDOUT FOR {case}:", flush=True)
                    print(stdout_output, flush=True)
                else:
                    print(f"NO STDOUT FROM TEST {case}", flush=True)

                if stderr_output:
                    print(f"STDERR FOR {case}:", flush=True)
                    print(stderr_output, flush=True)
                else:
                    print(f"NO STDERR FROM TEST {case}", flush=True)

        pool.close()
        pool.join()

    elapsed_time = time.time() - start_time

    print(f"SUMMARY: {pass_count}/{len(cases)} in {elapsed_time:.2f}s", end="")

    if not fail_count:
        print(" (ALL PASS!)")
    else:
        print(f" ({fail_count} FAILED)")

    return not fail_count


def main():
    parser = argparse.ArgumentParser(description="Run Tomato.NET tests")
    parser.add_argument('--ilverify', action='store_true',
                        default=False,
                        help="Run the ilverify tests")
    parser.add_argument('-p', '--parallelism', type=int,
                        default=os.cpu_count() or 1,
                        help="Number of test runners to run in parallel")
    parser.add_argument('cases', nargs='*',
                        help='Filter which tests to run')
    args = parser.parse_args()

    build_tests()
    run_tests(gather_tests(args.cases, args.ilverify), args.parallelism)


if __name__ == '__main__':
    main()
