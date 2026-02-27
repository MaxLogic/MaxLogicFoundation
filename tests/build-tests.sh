#!/bin/bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

"${SCRIPT_DIR}/build-delphi.sh" MaxLogic.Tests.dproj
"${SCRIPT_DIR}/build-delphi.sh" MaxLogic.Vcl.Tests.dproj
