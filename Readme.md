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
We can then install packages
```
# the OPM package and the downloaded package
$> ./lpm install 2.0.6 Indy10 indy
```
If a package was not downloaded previously, but can be found with the OPM (like Indy10), it will be downloaded. Of course there is no way to find non OPM packages without downloading them beforehand with `direct-download`

The last command is `upgrade`, which takes all packages installed via OPM, and downloads (and installs) the newest version of them, if a newer one is available than installed.

So to summarize, to build a program that requires packages the following has to be done:
```
$> lpm update
$> lpm lazarus add 2.0.6 /developer/lazarus/2.0.6
$> lpm install 2.0.6 Indy10
$> lazbuild project.lpi
```
