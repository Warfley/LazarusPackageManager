from typing import List, Set, Dict, Any
import json
import os
from pathlib import Path
from os import makedirs
from subprocess import call
from shutil import rmtree
from logger import Logger
from download import Downloader
from opm import OPMPackage

LAZBUILD_EXEC_NAME = "lazbuild" + (".exe" if os.name == "nt" else "")


class LocalPackage:
    def __init__(self, name: str, date: int, package_files: List[Path], installed_in: List[str] = []):
        self.name: str = name
        self.date: int = date
        self.package_files: List[Path] = package_files.copy()
        self.installed_in: Set[str] = set(installed_in)

    def serialize(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "date": self.date,
            "packages": [str(fl) for fl in self.package_files],
            "installed": list(self.installed_in)
        }

    @staticmethod
    def deserialize(serialized: Dict[str, Any]) -> "LocalPackage":
        return LocalPackage(serialized["name"],
                            serialized["date"],
                            [Path(package) for package in serialized["packages"]],
                            serialized["installed"])

class PackageManager:
    def __init__(self, target: Path, logger: Logger):
        self.logger: Logger = logger
        self.target: Path = target
        self.configFile: Path = target/"lpm.json"
        self.packages: Dict[str, LocalPackage] = {}
        self.installations: Dict[str, Path] = {}
        self.selected_installation: str = None
    
    def _get_package_directory(self, package: LocalPackage) -> Path:
        return self.target/"packages"/package.name

    def load(self) -> None:
        if not self.configFile.is_file():
            return

        with open(self.configFile, "r") as f:
            serialized = json.load(f)
        self.packages = {package_data["name"]: LocalPackage.deserialize(package_data) for package_data in serialized["packages"]}
        self.installations = {name: Path(path) for name, path in serialized["installations"].items()}
        self.selected_installation = serialized.get("selected")

    def save(self) -> None:
        serialized = {
            "version": 1,
            "installations": {name: str(path) for name, path in self.installations.items()},
            "packages": [package.serialize() for package in self.packages.values()]
        }
        if self.selected_installation is not None:
            serialized["selected"] = self.selected_installation
        with open(self.configFile, "w") as f:
            json.dump(serialized, f, indent=2)

    def fetch_from_downloader(self, name: str, downloader: Downloader, date: int, packages: List[Path]) -> bool:
        package_target = self.target/"packages"/name
        # remove old if there
        if package_target.is_dir():
            rmtree(package_target)
        # download file
        downloader.download(package_target)
        # if no package names are given, search for lpks
        if len(packages) == 0:
            package_files = package_target.rglob("*.lpk")
            packages = [p.relative_to(package_target) for p in package_files]
            self.logger.log("Packages found:")
            for package in packages:
                self.logger.log(str(package))
        # create package
        package = LocalPackage(name, date, packages)
        # if previously installed, add install information
        if name in self.packages:
            package.installed_in = self.packages[package.name].installed_in
        self.packages[name] = package
        return True

    def fetch_opm_package(self, package: OPMPackage) -> bool:
        downloader = package.get_downloader(self.logger)
        packages = [package.package_dir/fl.get_file_name() for fl in package.files]
        return self.fetch_from_downloader(package.name, downloader, package.file_date, packages)

    def get_lazbuild_path(self) -> Path:
        installation_path = self.installations.get(self.selected_installation)
        if installation_path is None:
            self.logger.error(f"No such lazarus installation found: {self.selected_installation}")
            return None
        return installation_path/LAZBUILD_EXEC_NAME

    def install_package(self, package_name: str) -> bool:
        # resolve package
        local_package = self.packages.get(package_name)
        if local_package is None:
            self.logger.error(f"Package {package_name} not found")
            return False
        local_dir = self._get_package_directory(local_package)
        #resolve lazbuild
        lazbuild_path = self.get_lazbuild_path()
        if lazbuild_path is None:
            return False
        # call lazbuild for all lpks
        result = True
        for file_name in local_package.package_files:
            self.logger.log(f"installing {file_name}")
            lpk_file = local_dir/file_name
            result = result and call([lazbuild_path.resolve(), "--add-package-link", lpk_file.resolve()]) == 0
        # add lazarus version to installed list
        local_package.installed_in.add(str(self.selected_installation))
        return result

    def add_installation(self, name: str, path: Path) -> bool:
        self.installations[name] = str(path.resolve())
        if self.selected_installation is None:
            self.selected_installation = name
        return True

    def remove_installation(self, name: str) -> bool:
        if name not in self.installations:
            self.logger.error(f"Installation {name} not found")
            return False
        if self.selected_installation == name:
            self.selected_installation = None if len(self.installations) == 0 else self.installations.keys()[0]
        del self.installations[name]
        return True

    def select_installation(self, name: str) -> bool:
        if name is not None and name not in self.installations:
            return False
        self.selected_installation = name
        return True

    def package_by_lpk(self, lpk_name: str) -> LocalPackage:
        for package in self.packages.values():
            for package_file in package.package_files:
                if package_file.name == lpk_name:
                    return package
        return None

    def get_base_package_files(self) -> Set[str]:
        lazarus = self.installations.get(self.selected_installation)
        components_dir = lazarus/"components"
        result = {"fcl.lpk", "lcl.lpk", "lclbase.lpk"}
        if lazarus is None:
            self.logger.error(f"No such lazarus installation found: {self.selected_installation}")
            return None
        result.update({p.name.lower() for p in components_dir.rglob("*.lpk")})
        return result
