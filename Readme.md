# LazarusPackageManager
This project contains a command-line package manager for Lazarus.

Features:
- Handling of multiple lazarus versions
- Download and install packages from OPM
- Search OPM
- Download and install packages from GIT, SVN or HTTP downloads

## Purpose:

The goal of this program is not to manage packages for development lazarus version, as the OPM does a great job there, but to install packages for buildsystems, e.g. via docker, where no GUI (and therefore no OPM) is available.

An example for building a project that requires a package from OPM can be found in `example`. See `example/Readme.md` for further informations

## Usage:
The lpm executable saves all the information in the `~/.lpm` directory, to change this use the `--target` option (see `lpm -h` for further information).

To use the OPM functionality, first the package list must be synchronized via
```
$> ./lpm update
```
OPM packages can then be searched with `search` and downloaded with `fetch`
```
$> ./lpm search indy
...
$> ./lpm fetch Indy10
```
To download any non OPM packages, we can use `direct-download`
```
$> ./lpm direct-download indy https://packages.lazarus-ide.org/Indy10.zip Indy10/indylaz.lpk
```
If we don't add any lpk files after the URL, the lpm will automatically search for all lpk files in the target directory

To install any packages we first need to register a lazarus verions where we want to install the package to:
```
$> ./lpm lazarus add 2.0.6 /developer/lazarus/2.0.6
```
The LPM stores the current lazarus version to perform operations on. The first one added will be the default one, it can be switched via:
```
$> ./lpm lazarus select 2.0.6
```
We can then install packages to the currently selected lazarus version
```
# the OPM package and the downloaded package
$> ./lpm install Indy10 indy
```
If a package was not downloaded previously, but can be found with the OPM (like Indy10), it will be downloaded. Of course there is no way to find non OPM packages without downloading them beforehand with `direct-download`

To ease building of projects as well as package searching, the command `build` provides a wrapper that:
1. seaches all the required packages that where not previously downloaded in the OPM
2. installs missing packages that where not installed to the given lazarus version
3. compiles the project using lazbuild

It also supports lazarus build modes, so if you want to build the build mode Release and Release64 you can use the following
```
$> ./lpm build projectPath Release Release64
```
If no build mode is given, all will be build. Also, it will ask you before installing packages, to skip this pass the `-y` or `--yes` option

The last command is `upgrade`, which takes all packages installed via OPM, and downloads (and installs) the newest version of them, if a newer one is available than installed.

So to summarize, to build a program that requires packages the following has to be done:
```
$> lpm update
$> lpm lazarus add 2.0.6 /developer/lazarus/2.0.6
# manual install
$> lpm install Indy10
$> lazbuild project.lpi
# Or simply
$> lpm build -y project.lpi
```
