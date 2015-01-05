echo off
echo attach %1 > extract.cmds
echo read * >> extract.cmds
echo quit >> extract.cmds
c1541.exe < extract.cmds