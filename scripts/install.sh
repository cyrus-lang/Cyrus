#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'

if [[ -t 1 ]]; then
    readonly C_RESET='\033[0m'
    readonly C_BOLD='\033[1m'
    readonly C_DIM='\033[2m'
    readonly C_RED='\033[38;5;196m'
    readonly C_GREEN='\033[38;5;46m'
    readonly C_YELLOW='\033[38;5;226m'
    readonly C_CYAN='\033[38;5;51m'
    readonly C_PURPLE='\033[38;5;129m'
else
    readonly C_RESET=''
    readonly C_BOLD=''
    readonly C_DIM=''
    readonly C_RED=''
    readonly C_GREEN=''
    readonly C_YELLOW=''
    readonly C_CYAN=''
    readonly C_PURPLE=''
fi

log_info()    { printf "%b[INFO]%b %s\n" "${C_CYAN}${C_BOLD}" "${C_RESET}" "$*"; }
log_success() { printf "%b[OK]%b %s\n" "${C_GREEN}${C_BOLD}" "${C_RESET}" "$*"; }
log_warn()    { printf "%b[WARN]%b %s\n" "${C_YELLOW}${C_BOLD}" "${C_RESET}" "$*" >&2; }
log_error()   { printf "%b[ERROR]%b %s\n" "${C_RED}${C_BOLD}" "${C_RESET}" "$*" >&2; }

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"

PREFIX="/usr"
BIN_DIR="${PREFIX}/bin"
DATA_DIR="/var/lib/cyrus"
STDLIB_TARGET_DIR="${DATA_DIR}/stdlib"
EXECUTABLE_NAME="cyrus"

SOURCE_BIN="${SCRIPT_DIR}/cyrus"
SOURCE_STDLIB="${SCRIPT_DIR}/stdlib"

show_help() {
    cat <<EOF
${C_BOLD}${C_PURPLE}CYRUS LANGUAGE COMPILER & TOOLCHAIN INSTALLER${C_RESET}

${C_BOLD}USAGE:${C_RESET}
  sudo bash $(basename "$0") [OPTIONS]

${C_BOLD}OPTIONS:${C_RESET}
  ${C_CYAN}-p, --prefix <DIR>${C_RESET}      Base installation prefix (Default: ${C_DIM}${PREFIX}${C_RESET})
                           Installs binary into: <DIR>/bin
  ${C_CYAN}-s, --stdlib <DIR>${C_RESET}      Custom target path for stdlib (Default: ${C_DIM}${STDLIB_TARGET_DIR}${C_RESET})
  ${C_CYAN}-u, --uninstall${C_RESET}         Remove Cyrus binary and standard library assets
  ${C_CYAN}-h, --help${C_RESET}              Display this CLI helper and exit

${C_BOLD}ENVIRONMENT VARIABLES EXPOSED:${C_RESET}
  ${C_GREEN}CYRUS_STDLIB_PATH${C_RESET}        Specifies search directory for standard modules.
                           Must be set in shell profile:
                           ${C_DIM}export CYRUS_STDLIB_PATH="${STDLIB_TARGET_DIR}"${C_RESET}

${C_BOLD}EXAMPLES:${C_RESET}
  sudo bash $(basename "$0")                      # Standard installation
  sudo bash $(basename "$0") --prefix /usr/local  # Install to /usr/local/bin
  sudo bash $(basename "$0") --uninstall          # Purge Cyrus from host system

EOF
}

uninstall_cyrus() {
    log_info "Initiating Cyrus deprovisioning..."

    if [[ -f "${BIN_DIR}/${EXECUTABLE_NAME}" ]]; then
        rm -f "${BIN_DIR}/${EXECUTABLE_NAME}"
        log_success "Removed binary: ${BIN_DIR}/${EXECUTABLE_NAME}"
    else
        log_warn "Binary not found at ${BIN_DIR}/${EXECUTABLE_NAME}"
    fi

    if [[ -d "${DATA_DIR}" ]]; then
        rm -rf "${DATA_DIR}"
        log_success "Removed data directory: ${DATA_DIR}"
    else
        log_warn "Data directory not found at ${DATA_DIR}"
    fi

    log_success "Cyrus has been cleanly uninstalled."
    exit 0
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        -h|--help)
            show_help
            exit 0
            ;;
        -u|--uninstall)
            if [[ ${EUID} -ne 0 ]]; then
                log_error "Uninstallation requires superuser privileges. Run with sudo."
                exit 1
            fi
            uninstall_cyrus
            ;;
        -p|--prefix)
            PREFIX="$2"
            BIN_DIR="${PREFIX}/bin"
            shift 2
            ;;
        -s|--stdlib)
            STDLIB_TARGET_DIR="$2"
            shift 2
            ;;
        *)
            log_error "Unknown option: $1"
            printf "Run '%s --help' for usage information.\n" "$0"
            exit 1
            ;;
    esac
done

if [[ ${EUID} -ne 0 ]]; then
    log_error "Superuser access required. Please re-run using sudo."
    exit 1
fi

log_info "Executing pre-flight validation checks..."

if [[ ! -f "${SOURCE_BIN}" ]]; then
    log_error "Payload missing: Binary '${SOURCE_BIN}' was not located."
    exit 1
fi

if [[ ! -d "${SOURCE_STDLIB}" ]]; then
    log_error "Payload missing: Stdlib folder '${SOURCE_STDLIB}' was not located."
    exit 1
fi

log_info "Deploying Cyrus executable to ${BIN_DIR}..."
install -d -m 0755 "${BIN_DIR}"
install -m 0755 "${SOURCE_BIN}" "${BIN_DIR}/${EXECUTABLE_NAME}"

log_info "Synchronizing standard library to ${STDLIB_TARGET_DIR}..."
install -d -m 0755 "${STDLIB_TARGET_DIR}"

cp -aT "${SOURCE_STDLIB}" "${STDLIB_TARGET_DIR}"

find "${STDLIB_TARGET_DIR}" -type d -exec chmod 0755 {} +
find "${STDLIB_TARGET_DIR}" -type f -exec chmod 0644 {} +

if command -v "${EXECUTABLE_NAME}" &>/dev/null; then
    printf "\n"
    printf "%b=================================================================%b\n" "${C_GREEN}${C_BOLD}" "${C_RESET}"
    printf "%b  Cyrus Toolchain Installed Successfully!%b\n" "${C_GREEN}${C_BOLD}" "${C_RESET}"
    printf "%b=================================================================%b\n\n" "${C_GREEN}${C_BOLD}" "${C_RESET}"

    printf "${C_BOLD}Required Environment Setup:${C_RESET}\n"
    printf "Export the standard library path in your active shell profile (~/.bashrc, ~/.zshrc, etc.):\n\n"
    printf "  %bexport CYRUS_STDLIB_PATH=\"%s\"%b\n\n" "${C_CYAN}${C_BOLD}" "${STDLIB_TARGET_DIR}" "${C_RESET}"

    printf "${C_BOLD}Quick Verification:${C_RESET}\n"
    printf "  • Verify path:       %bwhich cyrus%b\n" "${C_DIM}" "${C_RESET}"
    printf "  • Compiler version:  %bcyrus version%b\n" "${C_DIM}" "${C_RESET}"
    printf "  • Display manual:    %bcyrus help%b\n\n" "${C_DIM}" "${C_RESET}"
else
    log_warn "Cyrus was installed to '${BIN_DIR}/${EXECUTABLE_NAME}', but '${BIN_DIR}' is not in your current PATH."
    printf "Add it to your PATH by adding this line to your shell configuration:\n"
    printf "  export PATH=\"%s:\$PATH\"\n" "${BIN_DIR}"
fi
