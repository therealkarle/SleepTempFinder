$cmd = Get-Command conda -ErrorAction SilentlyContinue
if ($cmd) { Write-Output $cmd.Source; exit 0 }
$paths = @("$env:USERPROFILE\\Miniconda3\\Scripts\\conda.exe","$env:USERPROFILE\\Anaconda3\\Scripts\\conda.exe","C:\\ProgramData\\Miniconda3\\Scripts\\conda.exe","C:\\ProgramData\\Anaconda3\\Scripts\\conda.exe")
foreach ($p in $paths) { if (Test-Path $p) { Write-Output $p; exit 0 } }
Write-Output "NOTFOUND"
