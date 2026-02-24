$paths=@()
try{ $p=(Get-ItemProperty 'HKLM:\SOFTWARE\R-core\R' -ErrorAction Stop).InstallPath; if($p){$paths+=$p} }catch{}
try{ $p=(Get-ItemProperty 'HKLM:\SOFTWARE\WOW6432Node\R-core\R' -ErrorAction Stop).InstallPath; if($p){$paths+=$p} }catch{}
$cmd=Get-Command R -ErrorAction SilentlyContinue
if($cmd){ $paths+=$cmd.Source }
if(-not $paths){
    $paths += (Get-ChildItem 'C:\Program Files' -Directory -Filter 'R*' -ErrorAction SilentlyContinue | Select-Object -ExpandProperty FullName)
    $paths += (Get-ChildItem 'C:\Program Files (x86)' -Directory -Filter 'R*' -ErrorAction SilentlyContinue | Select-Object -ExpandProperty FullName)
}
$paths = $paths | Select-Object -Unique
$found=@()
foreach($p in $paths){
    if(Test-Path (Join-Path $p 'bin\\R.exe')){ $found+=(Join-Path $p 'bin\\R.exe') }
    elseif(Test-Path (Join-Path $p 'bin\\x64\\R.exe')){ $found+=(Join-Path $p 'bin\\x64\\R.exe') }
    elseif(Test-Path $p){ $found+=$p }
}
if($found){
    $found | Select-Object -Unique | ForEach-Object { Write-Output $_ }
}else{
    Write-Output 'NOTFOUND'
}
