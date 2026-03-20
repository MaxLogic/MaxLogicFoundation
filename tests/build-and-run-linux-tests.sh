#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"

if [ -z "${DAK_EXE:-}" ]; then
  echo "DAK_EXE is not set"
  exit 1
fi

"$DAK_EXE" build \
  --project "${SCRIPT_DIR}/MaxLogic.Linux.Tests.dproj" \
  --delphi 23.0 \
  --platform Linux64 \
  --config Debug \
  --ai

BIN_PATH="$(find "${SCRIPT_DIR}" -type f -name 'MaxLogic.Linux.Tests' | head -n 1)"

if [ -z "${BIN_PATH}" ]; then
  echo "Linux test binary not found after build"
  exit 1
fi

chmod +x "${BIN_PATH}"
cd "${ROOT_DIR}"
"${BIN_PATH}"
