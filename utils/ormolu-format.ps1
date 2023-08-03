Get-ChildItem -Filter *.hs -Recurse $pwd |
    Where-Object { $_.FullName -notmatch 'stack' } |
    Foreach-Object {ormolu --mode inplace $_.FullName }
