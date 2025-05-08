function Create-SymbolicLink {
    param (
        [string]$symlinkPath,
        [string]$targetPath
    )
    if (Test-Path -Path $symlinkPath) {
        Remove-Item -Path $symlinkPath -Force -Recurse
    }
    New-Item -Path $symlinkPath -ItemType SymbolicLink -Value (Get-Item $targetPath).FullName
}

Create-SymbolicLink -symlinkPath "$env:LOCALAPPDATA\nvim\" -targetPath ".\nvim\"
Create-SymbolicLink -symlinkPath "$env:APPDATA\alacritty\" -targetPath ".\alacritty\"
Create-SymbolicLink -symlinkPath "$env:APPDATA\kitty\" -targetPath ".\kitty\"

New-Item -path "$env:APPDATA\.emacs.d\" -ItemType Directory -Force
Create-SymbolicLink -symlinkPath "$env:APPDATA\.emacs.d\early-init.el" -targetPath ".\.emacs.d\early-init.el"
Create-SymbolicLink -symlinkPath "$env:APPDATA\.emacs.d\lisp\" -targetPath ".\.emacs.d\lisp\"

Create-SymbolicLink -symlinkPath "$HOME\.ideavimrc" -targetPath ".\.ideavimrc"
Create-SymbolicLink -symlinkPath "$HOME\.editorconfig" -targetPath ".\.editorconfig"
Create-SymbolicLink -symlinkPath "$HOME\.fakevimrc" -targetPath ".\.fakevimrc"
Create-SymbolicLink -symlinkPath "$HOME\.clang-format" -targetPath ".\.clang-format"
Create-SymbolicLink -symlinkPath "$HOME\.zshrc" -targetPath ".\.zshrc"
