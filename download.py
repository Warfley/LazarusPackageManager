from typing import Dict, List, Any, Union
from urllib import request
from enum import Enum
from os import makedirs
from subprocess import Popen, call
from tempfile import mkdtemp
from shutil import rmtree
from pathlib import Path
from logger import Logger, noLogger
from abc import ABC, abstractmethod


class ArchiveType(Enum):
    TAR = ".tar"
    TAR_GZ = ".tar.gz"
    TAR_BZ2 = ".tar.bz2"
    ZIP = ".zip"
    
    @staticmethod
    def from_filename(filename: str) -> "ArchiveType":
        for archive_type in ArchiveType:
            if filename.endswith(archive_type.value):
                return archive_type
        return None

def get_filename_from_url(url: str):
    return url.split("/")[-1]

class Downloader(ABC):
    @abstractmethod
    def download(self, target: Path) -> bool():
        raise NotImplementedError()
    
    @abstractmethod
    def serialize(self) -> Dict[str, Any]:
        raise NotImplementedError()

    @staticmethod
    def deserialize(download_data: Dict[str, Any]) -> "Downloader":
        protocol = download_data["protocol"]
        if protocol == "HTTP":
            return HTTPDownloader(download_data["url"], download_data.get("filename", None))
        if protocol == "GIT":
            return GITDownloader(download_data["url"], download_data.get("branch", "master"), download_data.get("shallow", True))
        if protocol == "SVN":
            return SVNDownloader(download_data["url"])
        return None

class HTTPDownloader(Downloader):
    def __init__(self, url: str, filename: str = None, logger: Logger = noLogger):
        self.url: str = url
        self.logger: Logger = logger
        self.filename: str = get_filename_from_url(url) if filename is None else filename
        self.archive_type = ArchiveType.from_filename(self.filename)

    def __download_raw(self, target: Path) -> bool:
        self.logger.log(f"Downloading {self.filename} from {self.url}")
        request.urlretrieve(self.url, target/self.filename)
        return True

    def __download_tar(self, target: Path, tar_arguments: Union[str, List[str]]) -> bool:
        if isinstance(tar_arguments, str):
            tar_arguments = [tar_arguments]
        self.logger.log(f"Downloading and unpacking {self.filename} from {self.url}")
        with request.urlopen(self.url) as req:
            tar_process = Popen(["tar"] + tar_arguments, cwd=str(target), stdin=req)
            tar_process.communicate()
            return tar_process.returncode == 0

    def __download_zip(self, target: Path) -> bool:
        temporary_storage = Path(mkdtemp())
        try:
            self.__download_raw(temporary_storage)
            self.logger.log(f"Inflating {self.filename}")
            return call(["unzip", "-q", str(temporary_storage/self.filename)], cwd=target) == 0
        finally:
            rmtree(temporary_storage)

    def download(self, target: Path) -> bool:
        target.mkdir(parents=True, exist_ok=True)
        if self.archive_type == ArchiveType.TAR:
            return self.__download_tar(target, "-x")
        if self.archive_type == ArchiveType.TAR_GZ:
            return self.__download_tar(target, "-xz")
        if self.archive_type == ArchiveType.TAR_BZ2:
            return self.__download_tar(target, "-xj")
        if self.archive_type == ArchiveType.ZIP:
            return self.__download_zip(target)
        return self.__download_raw(target)

    def serialize(self) -> Dict[str, Any]:
        return {
            "protocol": "HTTP",
            "url": self.url,
            "filename": self.filename
        }

class GITDownloader(Downloader):
    def __init__(self, url: str, branch: str = "master", shallow: bool = True, logger: Logger = noLogger):
        self.url: str = url
        self.logger: Logger = logger
        self.branch: str = branch
        self.shallow: bool = shallow

    def download(self, target: Path) -> bool:
        self.logger.log(f"cloning {self.url} branch {self.branch} into {target}")
        git_command_args: List[str] = ["git", "clone" f"--branch={self.branch}"]
        if self.shallow:
            git_command_args.append("--depth 1")
        git_command_args.extend([self.url, str(target)])
        return call(git_command_args) == 0

    def serialize(self) -> Dict[str, Any]:
        return {
            "protocol": "GIT",
            "url": self.url,
            "branch": self.branch,
            "shallow": self.shallow
        }

class SVNDownloader(Downloader):
    def __init__(self, url: str, logger: Logger = noLogger):
        self.url: str = url
        self.logger: Logger = logger

    def download(self, target: Path) -> bool:
        self.logger.log(f"checking {self.url} out into {target}")
        return call(["svn", "co", "-q", self.url, str(target)]) == 0

    def serialize(self) -> Dict[str, Any]:
        return {
            "protocol": "SVN",
            "url": self.url
        }
