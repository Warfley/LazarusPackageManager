import json
import re
from pathlib import Path
from os import makedirs
from download import HTTPDownload

repositoryURL = "https://packages.lazarus-ide.org"
targetFileName = "opm.json"

def normalizeFileName(fname):
    fname = fname.replace("\\/", "/")
    if len(fname) > 1 and fname[-1] == "/":
        fname = fname[:-1]
    return fname

class PackageDependency:
    def __init__(self, dependencyString):
        dependencyReg = re.compile("([A-Za-z0-9,.-]+)(\\((.+)\\))?")
        m = dependencyReg.match(dependencyString.strip())
        assert m is not None
        self.name = m.group(1)
        self.version = m.group(3)


from os import makedirs
from download import HTTPDownload
class OPMPackageFile:
    def __init__(self, dataObject):
        self.name = dataObject["Name"]
        self.description = dataObject["Description"]
        self.author = dataObject["Author"]
        self.license = dataObject["License"]
        self.relative_path = normalizeFileName(dataObject["RelativeFilePath"])
        self.version = dataObject["VersionAsString"]
        self.lazarus_versions = [v.strip() for v in dataObject["LazCompatibility"].split(",")]
        self.fpc_versions = [v.strip() for v in dataObject["FPCCompatibility"].split(",")]
        self.widgetsets = [v.strip() for v in dataObject["SupportedWidgetSet"].split(",")]
        self.package_type = dataObject["PackageType"]
        dependency_string = dataObject["DependenciesAsString"].strip()
        if dependency_string == "":
            self.dependencies = []
        else:
            self.dependencies = [PackageDependency(v) for v in dataObject["DependenciesAsString"].split(",")]
    def getFilename(self):
        if self.relative_path == "":
            return self.name
        return f"{self.relative_path}/{self.name}"

class OPMPackage:
    def __init__(self, dataObject):
        self.name = dataObject["Name"]
        self.display_name = dataObject["DisplayName"]
        self.categories = [c.strip() for c in dataObject["Category"].split(",")]
        self.description = dataObject["CommunityDescription"]
        self.file_name = dataObject["RepositoryFileName"]
        self.file_size = dataObject["RepositoryFileSize"]
        self.file_hash = dataObject["RepositoryFileHash"]
        self.file_date = dataObject["RepositoryDate"]
        self.package_dir = normalizeFileName(dataObject["PackageBaseDir"])
        self.homepage = dataObject["HomePageURL"]
        self.download_url = dataObject["DownloadURL"]
        self.svn_url = dataObject["SVNURL"]
        self.files = []
    def getPackageFilenames(self):
        return [fl.name for fl in self.files]
    def getDownloader(self, logger):
        return HTTPDownload(f"{repositoryURL}/{self.file_name}", logger=logger)

def readPackageList(packageListData):
    packages = {}
    nameMatcher = re.compile("Package(Data|Files)(\\d+)")
    for name, value in packageListData.items():
        m = nameMatcher.match(name)
        assert m is not None
        pkg_num = int(m.group(2))
        data, files = packages.get(pkg_num, (None, None))
        if m.group(1) == "Data":
            data = OPMPackage(value)
        else:
            files = [OPMPackageFile(v) for v in value]
        packages[pkg_num] = (data, files)
    result = {}
    for _, (data, files) in packages.items():
        data.files = files
        result[data.name] = data
    return result


class OnlinePackageManager:
    def __init__(self, target, logger):
        self.logger = logger
        self.packages = {}
        self.target = target
        self.packageMap = {}
    def __constructPackageNameMap(self):
        result = {}
        for _, pkg in self.packages.items():
            for fl in pkg.files:
                result[fl.name[:-4]] = pkg
        return result
    def downloadPackageList(self):
        HTTPDownload(f"{repositoryURL}/packagelist.json", targetFileName, logger=self.logger).download(self.target)
    def loadPackageList(self):
        destFile = self.target/targetFileName
        if not destFile.is_file():
            self.packages = {}
            self.packageMap = {}
            return
        with open(destFile, "r") as f:
            self.packages = readPackageList(json.load(f))
        self.__constructPackageNameMap()
    def searchPackages(self, searchString):
        result = set()
        for name, pkg in self.packages.items():
            cat = ", ".join(pkg.categories)
            if (searchString in name) \
            or (searchString in pkg.display_name) \
            or (searchString in pkg.description) \
            or (searchString in cat):
                result.add(pkg)
                continue
            for fl in pkg.files:
                if (searchString in fl.name) \
                or (searchString in fl.description) \
                or (searchString in fl.author):
                    result.add(pkg)
        return list(result)
