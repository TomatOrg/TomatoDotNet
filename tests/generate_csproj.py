import shlex
import shutil
import tempfile
import os

CURRENT_DIR = os.path.dirname(os.path.abspath(__file__))

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


projects = []


def generate_csproj(path, depth=1):
    ticks = "\\".join(['..'] * depth)

    for filename in os.listdir(path):
        full_path = os.path.join(path, filename)

        if os.path.isdir(full_path):
            if filename in ['bin', 'obj']:
                continue
            generate_csproj(full_path, depth + 1)
            continue

        if not filename.endswith(".cs"):
            continue

        print('Generating {}proj'.format(os.path.relpath(full_path, CURRENT_DIR)))

        projects.append(os.path.relpath(full_path, CURRENT_DIR) + 'proj')

        # create a project file
        with open(full_path + 'proj', 'w') as f:
            f.write(CSPROJ_TEMPLATE.format(filename, ticks))

# generate all the csprojects
generate_csproj(CURRENT_DIR)

# build the solution
if not os.path.exists(os.path.join(CURRENT_DIR, 'tests.sln')):
    assert os.system(f'cd {CURRENT_DIR} && dotnet new sln') == 0
assert os.system(f'cd {CURRENT_DIR} && dotnet sln add {shlex.join(projects)}') == 0
