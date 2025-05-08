#!/usr/bin/env bash
cd "$(dirname "$0")"

mkdir -p "${HOME}/.emacs.d/lisp"

HOME_FILES=(
    ".fakevimrc"
    ".ideavimrc"
    ".zshrc"
    ".clang-format"
    ".emacs.d/early-init.el"
)

for file in "${HOME_FILES[@]}";do
    rm -rf "${HOME}/${file}"
    ln -s "${PWD}/${file}" "${HOME}/${file}"
done

HOME_FOLDERS=(
    ".emacs.d/lisp"
)

for folder in "${HOME_FOLDERS[@]}";do
    rm -rf "${HOME}/${folder}"
    ln -s "${PWD}/${folder}" "${HOME}/${folder}"
done

CONFIG_FOLDERS=(
    "foot"
    "kitty"
    "nvim"
    "alacritty"
    "Code - OSS"
)

for folder in "${CONFIG_FOLDERS[@]}";do
    rm -rf "${HOME}/.config/${folder}"
    ln -s "${PWD}/${folder}" "${HOME}/.config/${folder}"
done
