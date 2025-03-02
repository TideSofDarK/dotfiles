CONFIG_FOLDERS=(
    "nvim/"
    "alacritty/"
)

for folder in "${CONFIG_FOLDERS[@]}";do
    rm -rf "${HOME}/.config/${folder}"
    cp -lR "${folder}" "${HOME}/.config/"
done

HOME_FOLDERS=(
    ".emacs.d/"
)

for folder in "${HOME_FOLDERS[@]}";do
    rm -rf "${HOME}/${folder}"
    cp -lR "${folder}" "${HOME}/"
done

HOME_FILES=(
    ".fakevimrc"
    ".ideavimrc"
    ".zshrc"
)

for file in "${HOME_FILES[@]}";do
    rm -rf "${HOME}/${file}"
    cp -lR "${file}" "${HOME}/${file}"
done
