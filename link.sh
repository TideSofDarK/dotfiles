#!/usr/bin/env bash
cd "$(dirname "$0")"

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

HOME_FOLDERS=(
    ".emacs.d"
)

for folder in "${HOME_FOLDERS[@]}";do
    rm -rf "${HOME}/${folder}"
    ln -s "${PWD}/${folder}" "${HOME}/${folder}"
done

HOME_FILES=(
    ".fakevimrc"
    ".ideavimrc"
    ".zshrc"
    ".clang-format"
)

for file in "${HOME_FILES[@]}";do
    rm -rf "${HOME}/${file}"
    ln -s "${PWD}/${file}" "${HOME}/${file}"
done
