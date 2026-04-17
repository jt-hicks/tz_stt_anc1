#!/usr/bin/env pwsh
# Batch runner wrapper - Execute the batch summarise script

Write-Host ""
Write-Host "╔════════════════════════════════════════════════════════════════╗" -ForegroundColor Cyan
Write-Host "║   BATCH SUMMARISE: Extract Results for All 186 Councils       ║" -ForegroundColor Cyan
Write-Host "╚════════════════════════════════════════════════════════════════╝" -ForegroundColor Cyan
Write-Host ""

# Check if script exists
if (-not (Test-Path "batch_summarise_all.R")) {
    Write-Host "ERROR: batch_summarise_all.R not found!" -ForegroundColor Red
    exit 1
}

# Get R path
$RPath = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
if (-not (Test-Path $RPath)) {
    Write-Host "ERROR: R not found at $RPath" -ForegroundColor Red
    exit 1
}

Write-Host "This will run summarise_results for all 26 zone/region combinations"
Write-Host "and extract results for all 186 councils."
Write-Host ""
Write-Host "Estimated time: 5-10 minutes depending on system performance"
Write-Host ""
Write-Host "Press Enter to start, or Ctrl+C to cancel..." -ForegroundColor Yellow
Read-Host

Write-Host ""
Write-Host "Starting batch process..." -ForegroundColor Green
Write-Host ""

# Run the script
& $RPath "batch_summarise_all.R"

$exitCode = $LASTEXITCODE

if ($exitCode -eq 0) {
    Write-Host ""
    Write-Host "✓ Batch process completed successfully!" -ForegroundColor Green
    Write-Host ""
    Write-Host "Results are saved in:" -ForegroundColor Cyan
    Write-Host "  - batch_summarise_log.rds (R data format)"
    Write-Host "  - batch_summarise_log.csv (spreadsheet format)"
    Write-Host ""
    Write-Host "Global results:" -ForegroundColor Cyan
    Write-Host "  - archive/compile_results/[latest]/global_results.rds"
    Write-Host "  - archive/compile_results/[latest]/global_results.csv"
    Write-Host ""
} else {
    Write-Host ""
    Write-Host "✗ Process exited with error code: $exitCode" -ForegroundColor Red
}

exit $exitCode
