# OpenCog Collection - Windows Build Script
# This script automates the build process for Windows using vcpkg

param(
    [string]$VcpkgRoot = "C:\vcpkg",
    [string]$InstallPrefix = "C:\OpenCog",
    [string]$BuildType = "Release",
    [switch]$SkipTests,
    [switch]$UseNinja,
    [int]$ParallelJobs = 0
)

$ErrorActionPreference = "Stop"

Write-Host "==================================================" -ForegroundColor Cyan
Write-Host "OpenCog Collection - Windows Build Script" -ForegroundColor Cyan
Write-Host "==================================================" -ForegroundColor Cyan
Write-Host ""

# Validate vcpkg installation
if (-not (Test-Path "$VcpkgRoot\vcpkg.exe")) {
    Write-Host "ERROR: vcpkg not found at $VcpkgRoot" -ForegroundColor Red
    Write-Host "Please install vcpkg first:" -ForegroundColor Yellow
    Write-Host "  git clone https://github.com/microsoft/vcpkg.git $VcpkgRoot" -ForegroundColor Yellow
    Write-Host "  cd $VcpkgRoot" -ForegroundColor Yellow
    Write-Host "  .\bootstrap-vcpkg.bat" -ForegroundColor Yellow
    exit 1
}

Write-Host "✓ vcpkg found at: $VcpkgRoot" -ForegroundColor Green

# Ensure vcpkg is integrated
Write-Host ""
Write-Host "Integrating vcpkg..." -ForegroundColor Cyan
& "$VcpkgRoot\vcpkg.exe" integrate install

# Determine generator
$Generator = if ($UseNinja) { "Ninja" } else { $null }

# Determine parallel jobs
if ($ParallelJobs -eq 0) {
    $ParallelJobs = (Get-CimInstance Win32_ComputerSystem).NumberOfLogicalProcessors
}

Write-Host ""
Write-Host "Build Configuration:" -ForegroundColor Cyan
Write-Host "  Build Type: $BuildType" -ForegroundColor White
Write-Host "  Install Prefix: $InstallPrefix" -ForegroundColor White
Write-Host "  Parallel Jobs: $ParallelJobs" -ForegroundColor White
Write-Host "  Generator: $(if ($Generator) { $Generator } else { 'Default (MSBuild)' })" -ForegroundColor White
Write-Host "  Skip Tests: $SkipTests" -ForegroundColor White
Write-Host ""

# Create build directory
$BuildDir = "build-windows"
if (Test-Path $BuildDir) {
    Write-Host "Cleaning existing build directory..." -ForegroundColor Yellow
    Remove-Item -Recurse -Force $BuildDir
}
New-Item -ItemType Directory -Path $BuildDir | Out-Null

# Configure CMake
Write-Host ""
Write-Host "==================================================" -ForegroundColor Cyan
Write-Host "Configuring CMake..." -ForegroundColor Cyan
Write-Host "==================================================" -ForegroundColor Cyan

$CMakeArgs = @(
    "-B", $BuildDir,
    "-S", ".",
    "-DCMAKE_TOOLCHAIN_FILE=$VcpkgRoot\scripts\buildsystems\vcpkg.cmake",
    "-DCMAKE_BUILD_TYPE=$BuildType",
    "-DCMAKE_INSTALL_PREFIX=$InstallPrefix",
    "-DBUILD_TESTING=$(if ($SkipTests) { 'OFF' } else { 'ON' })"
)

if ($Generator) {
    $CMakeArgs += "-G", $Generator
}

& cmake @CMakeArgs

if ($LASTEXITCODE -ne 0) {
    Write-Host ""
    Write-Host "ERROR: CMake configuration failed!" -ForegroundColor Red
    exit 1
}

Write-Host ""
Write-Host "✓ CMake configuration successful" -ForegroundColor Green

# Build
Write-Host ""
Write-Host "==================================================" -ForegroundColor Cyan
Write-Host "Building OpenCog Collection..." -ForegroundColor Cyan
Write-Host "==================================================" -ForegroundColor Cyan

& cmake --build $BuildDir --config $BuildType --parallel $ParallelJobs

if ($LASTEXITCODE -ne 0) {
    Write-Host ""
    Write-Host "ERROR: Build failed!" -ForegroundColor Red
    exit 1
}

Write-Host ""
Write-Host "✓ Build successful" -ForegroundColor Green

# Run tests
if (-not $SkipTests) {
    Write-Host ""
    Write-Host "==================================================" -ForegroundColor Cyan
    Write-Host "Running Tests..." -ForegroundColor Cyan
    Write-Host "==================================================" -ForegroundColor Cyan
    
    Push-Location $BuildDir
    & ctest -C $BuildType --output-on-failure --parallel $ParallelJobs
    $TestResult = $LASTEXITCODE
    Pop-Location
    
    if ($TestResult -ne 0) {
        Write-Host ""
        Write-Host "WARNING: Some tests failed!" -ForegroundColor Yellow
    } else {
        Write-Host ""
        Write-Host "✓ All tests passed" -ForegroundColor Green
    }
}

# Install
Write-Host ""
Write-Host "==================================================" -ForegroundColor Cyan
Write-Host "Installing to $InstallPrefix..." -ForegroundColor Cyan
Write-Host "==================================================" -ForegroundColor Cyan

& cmake --install $BuildDir --config $BuildType

if ($LASTEXITCODE -ne 0) {
    Write-Host ""
    Write-Host "ERROR: Installation failed!" -ForegroundColor Red
    exit 1
}

Write-Host ""
Write-Host "✓ Installation successful" -ForegroundColor Green

# Summary
Write-Host ""
Write-Host "==================================================" -ForegroundColor Cyan
Write-Host "Build Complete!" -ForegroundColor Cyan
Write-Host "==================================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "OpenCog Collection has been successfully built and installed to:" -ForegroundColor Green
Write-Host "  $InstallPrefix" -ForegroundColor White
Write-Host ""
Write-Host "To use OpenCog, add the following to your PATH:" -ForegroundColor Yellow
Write-Host "  $InstallPrefix\bin" -ForegroundColor White
Write-Host ""
Write-Host "You can do this by running:" -ForegroundColor Yellow
Write-Host "  `$env:PATH += `";$InstallPrefix\bin`"" -ForegroundColor White
Write-Host ""
Write-Host "To make this permanent, add it to your PowerShell profile:" -ForegroundColor Yellow
Write-Host "  notepad `$PROFILE" -ForegroundColor White
Write-Host ""
