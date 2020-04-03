import json
import os
from pathlib import Path
from os import makedirs
from subprocess import call
from shutil import rmtree

lazbuildName = "lazbuild" + (".exe" if os.name == "nt" else "")


class Package:
    def __init__(self, name, directory, date, package_files, installed=[]):
        self.name = name
        self.directory = directory
        self.date = date
        self.package_files = package_files
        self.installed = set(installed)
    def serialize(self):
        return {
            "name": self.name,
            "date": self.date,
            "directory": self.directory,
            "packages": self.package_files,
            "installed": list(self.installed)
        }
def deserializePackage(serialized):
    return Package(serialized["name"],
                         serialized["directory"],
                         serialized["date"],
                         serialized["packages"],
                         serialized["installed"])

class PackageManager:
    def __init__(self, target, logger):
        self.logger = logger
        self.target = target
        self.configFile = target/"lpm.json"
        self.packages = {}
        self.installations = {}
    def load(self):
        self.packages = {}
        self.installations = {}
        if not self.configFile.is_file():
            return
        with open(self.configFile, "r") as f:
            serialized = json.load(f)
        for p in  [deserializePackage(p) for p in serialized["packages"]]:
            self.packages[p.name] = p
        for n, p in serialized["installations"].items():
            self.installations[n] = Path(p)
    def save(self):
        installations = {}
        for n, p in self.installations.items():
            installations[n] = str(p)
        serialized = {
            "version": 1,
            "installations": installations,
            "packages": [p.serialize() for _, p in self.packages.items()]
        }
        with open(self.configFile, "w") as f:
            json.dump(serialized, f, indent=2)
    def fetchFromDownloader(self, name, downloader, date, packages):
        target = self.target/"packages"/name
        # remove old if there
        if target.is_dir():
            rmtree(target)
        # download file
        downloader.download(target)
        # if no package names are given, search for lpks
        if len(packages) == 0:
            pkgs = target.rglob("*.lpk")
            packages = [str(p.relative_to(target)) for p in pkgs]
            self.logger.log("Packages found:")
            for p in packages:
                self.logger.log(p)
        # create package
        pkg = Package(name, name, date, packages)
        # if previously installed, add install information
        if name in self.packages:
            pkg.installed = self.packages[pkg.name].installed
        self.packages[name] = pkg
        return True
    def fetchFromOPM(self, pkg):
        downloader = pkg.getDownloader(self.logger)
        packages = [f"{pkg.package_dir}/{fl.getFilename()}" for fl in pkg.files]
        return self.fetchFromDownloader(pkg.name, downloader, pkg.file_date, packages)
    def installPackage(self, lazarusName, packageName):
        # resolve package
        pkg = self.packages.get(packageName)
        if pkg is None:
            self.logger.error(f"Package {packageName} not found")
            return False
        pkgDir = self.target/"packages"/pkg.directory
        #resolve lazbuild
        lazarus = self.installations.get(lazarusName)
        if lazarus is None:
            print(f"No such lazarus installation found: {lazarusName}")
            return False
        lazbuild = lazarus/lazbuildName
        # call lazbuild for all lpks
        result = True
        for fl in pkg.package_files:
            self.logger.log(f"installing {fl}")
            pkgFile = pkgDir/fl
            result = result and call([lazbuild, "--add-package-link", pkgFile.resolve()]) == 0
        # add lazarus version to installed list
        pkg.installed.add(str(lazarusName))
        return result
    def addLazarus(self, name, path):
        self.installations[name] = path
        return True
    def removeLazarus(self, name):
        if name not in self.installations:
            self.logger.error(f"Installation {name} not found")
            return False
        del self.installations[name]
