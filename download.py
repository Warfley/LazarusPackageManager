from urllib import request
from enum import Enum
from os import makedirs
from subprocess import Popen, call
from tempfile import mkdtemp
from shutil import rmtree
from pathlib import Path
from logger import noLogger


class Packtype(Enum):
    TAR = ".tar"
    TAR_GZ = ".tar.gz"
    TAR_BZ2 = ".tar.bz2"
    ZIP = ".zip"

def packtypeFromFilename(filename):
    for pt in Packtype:
        if filename.endswith(pt.value):
            return pt
    return None

def filenameFromUrl(url):
    return url.split("/")[-1]

class HTTPDownload:
    def __init__(self, url, filename=None, logger=noLogger):
        self.url = url
        self.logger=logger
        self.filename = filenameFromUrl(url) if filename is None else filename
        self.packtype = packtypeFromFilename(self.filename)
    def __downloadRaw(self, target):
        self.logger.log(f"Downloading {self.filename} from {self.url}")
        request.urlretrieve(self.url, target/self.filename)
        return True
    def __downloadTar(self, target, tarArgs):
        self.logger.log(f"Downloading and unpacking {self.filename} from {self.url}")
        with request.urlopen(self.url) as req:
            tar = Popen(["tar", tarArgs], cwd=target, stdin=req)
            tar.communicate()
            return tar.returncode == 0
    def __downloadZip(self, target):
        tmpDir = Path(mkdtemp())
        try:
            self.__downloadRaw(tmpDir)
            self.logger.log(f"Inflating {self.filename}")
            return call(["unzip", "-q", str(tmpDir/self.filename)], cwd=target) == 0
        finally:
            rmtree(tmpDir)
    def download(self, target):
        makedirs(target, exist_ok=True)
        if self.packtype == Packtype.TAR:
            return self.__downloadTar(target, "-x")
        if self.packtype == Packtype.TAR_GZ:
            return self.__downloadTar(target, "-xz")
        if self.packtype == Packtype.TAR_BZ2:
            return self.__downloadTar(target, "-xj")
        if self.packtype == Packtype.ZIP:
            return self.__downloadZip(target)
        return self.__downloadRaw(target)
    def serialize(self):
        return {
            "protocol": "HTTP",
            "url": self.url,
            "filename": self.filename
        }

class GITDownload:
    def __init__(self, url, branch="master", shallow=True, logger=noLogger):
        self.url = url
        self.logger = logger
        self.branch = branch
        self.shallow = shallow
    def download(self, target):
        self.logger.log(f"cloning {self.url} branch {self.branch} into {target}")
        callArgs = ["git", "clone" f"--branch={self.branch}"]
        if self.shallow:
            callArgs.append("--depth 1")
        callArgs.extend([self.url, target])
        return call(callArgs) == 0
    def serialize(self):
        return {
            "protocol": "GIT",
            "url": self.url,
            "branch": self.branch,
            "shallow": self.shallow
        }

class SVNDownload:
    def __init__(self, url, logger=noLogger):
        self.url = url
        self.logger = logger
    def download(self, target, print_action=None):
        self.logger.log(f"checking {self.url} out into {target}")
        return call(["svn", "co", "-q", self.url, target]) == 0
    def serialize(self):
        return {
            "protocol": "SVN",
            "url": self.url
        }

def deserializeDownload(serialized):
    proto = serialized["protocol"]
    if proto == "HTTP":
        return HTTPDownload(serialized["url"], serialized.get("filename", None))
    if proto == "GIT":
        return GITDownload(serialized["url"], serialized.get("branch", "master"), serialized.get("shallow", True))
    if proto == "SVN":
        return SVNDownload(serialized["url"])
    return None
