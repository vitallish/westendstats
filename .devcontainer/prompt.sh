# Project greeting and custom prompt
if [ -n "$PS1" ]; then
    PROJECT_NAME=$(ls /workspaces/ 2>/dev/null | head -1)
    echo "rstats-dev: ${RSTATS_DEV_TAG}"
    echo "project:    ${PROJECT_NAME}"

    _prompt_dir() {
        local wksp="/workspaces/${PROJECT_NAME}"
        case "$PWD" in
            "$wksp")    echo "wksp" ;;
            "$wksp/"*)  echo "wksp/${PWD#${wksp}/}" ;;
            *)          echo "$PWD" ;;
        esac
    }

    PS1='\[\033[01;32m\][$(_prompt_dir)]\[\033[00m\] \$ '
fi
