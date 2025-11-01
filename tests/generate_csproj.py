import shlex
from pathlib import Path
import os

CURRENT_DIR = Path(__file__).resolve().parent

CSPROJ_TEMPLATE = """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net8.0</TargetFramework>
    <DebugType>Full</DebugType>
    <Optimize>True</Optimize>
    <UseAppHost>false</UseAppHost>
    <NoStdLib>true</NoStdLib>
    <DisableImplicitFrameworkReferences>true</DisableImplicitFrameworkReferences>
    <EnableDefaultCompileItems>false</EnableDefaultCompileItems>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="{}" />
    <ProjectReference Include="{}\\TdnCoreLib\\System.Private.CoreLib\\System.Private.CoreLib.csproj" />
  </ItemGroup>
</Project>
"""

def get_cs_from_csproj(f: Path):
    return f.read_text().split('Compile Include="', 1)[1].split('"', 1)[0]

def iter_tests(path: Path, suffix: str):
    for value in path.iterdir():
        if value.is_dir():
            if value.name in ['bin', 'obj']:
                continue
            yield from iter_tests(value, suffix)
        elif value.is_file(follow_symlinks=False):
            if value.suffix == suffix:
                yield value

EXISTING_PROJECTS: set[str] = set()
EXISTING_SOURCES: set[Path] = set()
PROJECTS: list[Path] = []

# start by creating the existing mappings
for csproj in iter_tests(CURRENT_DIR, suffix='.csproj'):
    cs = csproj.parent / get_cs_from_csproj(csproj)
    EXISTING_SOURCES.add(cs)
    EXISTING_PROJECTS.add(csproj.name.removesuffix('.csproj'))
    PROJECTS.append(csproj)

for cs in iter_tests(CURRENT_DIR, suffix='.cs'):
    if cs in EXISTING_SOURCES:
        continue

    # ensure the project name is unique
    csproj_name = cs.name.removesuffix('.cs').lower()
    while csproj_name in EXISTING_PROJECTS:
        csproj_name += '_'
    EXISTING_PROJECTS.add(csproj_name)

    dashes = str(CURRENT_DIR.relative_to(cs, walk_up=True)).replace("/", '\\')
    csproj = cs.parent / (csproj_name + '.csproj')
    print(f"Generating {csproj.relative_to(CURRENT_DIR)}")
    csproj.write_text(CSPROJ_TEMPLATE.format(cs.name, dashes))
    PROJECTS.append(csproj)

# build the solution
if not (CURRENT_DIR / 'tests.sln').exists():
    assert os.system(f'cd {CURRENT_DIR} && dotnet new sln') == 0
assert os.system(f'cd {CURRENT_DIR} && dotnet sln add {shlex.join([str(proj) for proj in PROJECTS])}') == 0
