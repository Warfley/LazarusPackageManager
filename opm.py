from typing import List, Dict, Any, Tuple
import json
import re
from pathlib import Path
from os import makedirs
from download import HTTPDownloader, Downloader
from logger import Logger

OPM_REPOSITORY_URL = "https://packages.lazarus-ide.org"
OPM_FILE_NAME = "opm.json"

def normalize_file_name(file_name: str) -> Path:
    file_name = file_name.replace("\\/", "/")
    if len(file_name) > 1 and file_name[-1] == "/":
        file_name = file_name[:-1]
    return Path(file_name)

class PackageDependency:
    def __init__(self, dependencyString):
        dependency_expr = re.compile("([A-Za-z0-9,.-]+)(\\((.+)\\))?")
        match = dependency_expr.match(dependencyString.strip())
        assert match is not None
        self.name: str = match.group(1)
        self.version: str = match.group(3)

class OPMPackageFile:
    def __init__(self, package_file_data: Dict[str, Any]):
        self.name: str = package_file_data["Name"]
        self.description: str = package_file_data["Description"]
        self.author: str = package_file_data["Author"]
        self.license: str = package_file_data["License"]
        self.relative_path: Path = normalize_file_name(package_file_data["RelativeFilePath"])
        self.version: str = package_file_data["VersionAsString"]
        self.lazarus_versions: List[str] = [v.strip() for v in package_file_data["LazCompatibility"].split(",")]
        self.fpc_versions: List[str] = [v.strip() for v in package_file_data["FPCCompatibility"].split(",")]
        self.widgetsets: List[str] = [v.strip() for v in package_file_data["SupportedWidgetSet"].split(",")]
        self.package_type: str = package_file_data["PackageType"]
        self.dependencies: List[PackageDependency] = []
        dependency_string: str = package_file_data["DependenciesAsString"].strip()
        if dependency_string:
            self.dependencies = [PackageDependency(v) for v in package_file_data["DependenciesAsString"].split(",")]

    def get_file_name(self) -> Path:
        return self.relative_path/self.name

class OPMPackage:
    def __init__(self, package_data: Dict[str, Any]):
        self.name: str = package_data["Name"]
        self.display_name: str = package_data["DisplayName"]
        self.categories: List[str] = [c.strip() for c in package_data["Category"].split(",")]
        self.description: str = package_data["CommunityDescription"]
        self.file_name: str = package_data["RepositoryFileName"]
        self.file_size: int = package_data["RepositoryFileSize"]
        self.file_hash: str = package_data["RepositoryFileHash"]
        self.file_date: int = package_data["RepositoryDate"]
        self.package_dir: Path = normalize_file_name(package_data["PackageBaseDir"])
        self.homepage: str = package_data["HomePageURL"]
        self.download_url: str = package_data["DownloadURL"]
        self.svn_url: str = package_data["SVNURL"]
        self.files: List[OPMPackageFile] = []

    def get_file_names(self) -> List[str]:
        return [fl.name for fl in self.files]

    def get_downloader(self, logger: Logger) -> Downloader:
        return HTTPDownloader(f"{OPM_REPOSITORY_URL}/{self.file_name}", logger=logger)

def read_package_list(package_list_data: Dict[str, Dict[str, Any]]) -> Dict[str, OPMPackage]:
    packages: Dict[int, Tuple[str, str]] = {}
    name_expr = re.compile("Package(Data|Files)(\\d+)")
    for name, data in package_list_data.items():
        match = name_expr.match(name)
        assert match is not None
        package_number = int(match.group(2))
        package, files = packages.get(package_number, (None, None))
        if match.group(1) == "Data":
            package = OPMPackage(data)
        else:
            files = [OPMPackageFile(v) for v in data]
        packages[package_number] = (package, files)
    result = {}
    for _, (package, files) in packages.items():
        package.files = files
        result[package.name] = package
    return result

class OnlinePackageManager:
    def __init__(self, target: Path, logger: Logger):
        self.logger: Logger = logger
        self.target: Path = target
        self.packages: Dict[str, OPMPackage] = {}
        self.package_by_file: Dict[str, OPMPackage] = {}

    def __construct_package_file_map(self) -> Dict[str, OPMPackage]:
        result = {}
        for package in self.packages.values():
            for fl in package.files:
                result[fl.name.lower()] = package
        return result

    def update_package_list(self) -> bool:
        return HTTPDownloader(f"{OPM_REPOSITORY_URL}/packagelist.json", OPM_FILE_NAME, logger=self.logger).download(self.target)

    def load_package_data(self) -> None:
        package_file = self.target/OPM_FILE_NAME
        if not package_file.is_file():
            self.packages = {}
            self.package_by_file = {}
            return
        with open(package_file, "r") as f:
            opm_data = json.load(f)
            self.packages = read_package_list(opm_data)
        self.package_by_file = self.__construct_package_file_map()

    def search_packages(self, search_string: str) -> List[OPMPackage]:
        result = set()
        for package in self.packages.values():
            cat = ", ".join(package.categories)
            if (search_string in package.name) \
            or (search_string in package.display_name) \
            or (search_string in package.description) \
            or (search_string in cat):
                result.add(package)
                continue
            for fl in package.files:
                if (search_string in fl.name) \
                or (search_string in fl.description) \
                or (search_string in fl.author):
                    result.add(package)
                    break
        return list(result)
