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
    <ProjectReference Include="..\\..\\..\\TdnCoreLib\\System.Private.CoreLib\\System.Private.CoreLib.csproj" />
  </ItemGroup>
</Project>
"""

for dirpath, dirnames, filenames in os.walk(CURRENT_DIR):
    # setup a temp dir for the test
    for filename in filenames:
        if not filename.endswith('.cs'):
            continue

        # create a project file
        with open(os.path.join(dirpath, filename + 'proj'), 'w') as f:
            f.write(CSPROJ_TEMPLATE.format(filename))


