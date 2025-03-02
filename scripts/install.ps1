$INSTALL_DIR = "C:\Windows\System32"
$EXECUTABLE_NAME = "cyrus.exe"
$SOURCE_PATH = ".\cyrus.exe"

$currentUser = [System.Security.Principal.WindowsIdentity]::GetCurrent()
$currentPrincipal = New-Object System.Security.Principal.WindowsPrincipal($currentUser)
$adminRole = [System.Security.Principal.WindowsBuiltInRole]::Administrator

if (-not $currentPrincipal.IsInRole($adminRole)) {
    Write-Host "Please run this script as Administrator."
    exit 1
}

if (-not (Test-Path $SOURCE_PATH)) {
    Write-Host "Error: '$SOURCE_PATH' not found. Ensure the Cyrus binary exists in this directory."
    exit 1
}

Copy-Item -Path $SOURCE_PATH -Destination "$INSTALL_DIR\$EXECUTABLE_NAME" -Force

if (Test-Path "$INSTALL_DIR\$EXECUTABLE_NAME") {
    Write-Host "Cyrus has been installed successfully!"
    Write-Host ""
    Write-Host "You can now run 'cyrus' from anywhere."
    Write-Host ""
    Write-Host "  Quick Usage:"
    Write-Host "   - Check version:       cyrus version"
    Write-Host "   - Show help:           cyrus help"
    Write-Host ""
    Write-Host "Happy coding with Cyrus Lang! 👾"
} else {
    Write-Host "Installation failed."
    exit 1
}
