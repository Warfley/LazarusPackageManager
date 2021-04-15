from typing import List
from pathlib import Path
from xml.etree import ElementTree
from subprocess import call

class LazarusProject:
    def __init__(self, lpi_file: Path):
        self.lpi_file: Path = lpi_file
        self.lpi_data: ElementTree.ElementTree = ElementTree.parse(lpi_file)

    def build(self, lazbuild_path: Path, build_mode: str = None) -> bool:
        call_args: List[str] = [str(lazbuild_path.resolve())]
        if build_mode is not None:
            call_args.append(f"--build-mode={build_mode}")
        call_args.append(str(self.lpi_file.resolve()))
        return call(call_args) == 0

    def get_build_modes(self) -> List[str]:
        options_node: ElementTree.Element = self.lpi_data.getroot().find("ProjectOptions")
        mode_node: ElementTree.Element = options_node.find("BuildModes")
        return [item_node.attrib["Name"] for item_node in mode_node if item_node.tag.startswith("Item")]

    def get_dependencies(self) -> List[str]:
        options_node: ElementTree.Element = self.lpi_data.getroot().find("ProjectOptions")
        requirements_node: ElementTree.Element = options_node.find("RequiredPackages")
        if requirements_node is None:
            return []
        return [item_node[0].attrib["Value"] for item_node in requirements_node]
