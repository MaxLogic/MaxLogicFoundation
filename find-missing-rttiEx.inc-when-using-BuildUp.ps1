# Finds *.pas files (recursively) that:
#  1) contain "globalcontainer.buildup" (case-insensitive)
#  2) do NOT contain "maxlogic.RttiEx.inc" (case-insensitive)
# Outputs the final list, and cleans up temp files.

param(
  [string]$Root = (Get-Location).Path
)

$needle1 = 'globalcontainer.buildup'
$needle2 = 'maxlogic.RttiEx.inc'

# Temp files
$step1List = Join-Path $env:TEMP ("pas_step1_{0}.txt" -f ([guid]::NewGuid().ToString("N")))

try {
  # Step 1: find *.pas containing needle1, recursive
  # -List returns file names; -SimpleMatch treats needle as literal string; -CaseSensitive:$false enforces CI
  Get-ChildItem -Path $Root -Recurse -File -Filter *.pas -ErrorAction SilentlyContinue |
    Select-Object -ExpandProperty FullName |
    Where-Object {
      Select-String -Path $_ -Pattern $needle1 -SimpleMatch -CaseSensitive:$false -Quiet
    } |
    Set-Content -Path $step1List -Encoding UTF8

  # Step 2: from step1 results, keep only those that do NOT contain needle2
  $final = Get-Content -Path $step1List -ErrorAction Stop |
    Where-Object { $_ -and (Test-Path -LiteralPath $_) } |
    Where-Object {
      -not (Select-String -Path $_ -Pattern $needle2 -SimpleMatch -CaseSensitive:$false -Quiet)
    }

  # Step 3: output
  $final
}
finally {
  # Cleanup
  if (Test-Path -LiteralPath $step1List) {
    Remove-Item -LiteralPath $step1List -Force -ErrorAction SilentlyContinue
  }
}
