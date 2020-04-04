from xml.etree import ElementTree
from subprocess import call

class LazarusProject:
    def __init__(self, lpiFile):
        self.lpiFile = lpiFile
        self.tree = ElementTree.parse(lpiFile)
    def build(self, lazbuild, mode=None):
        callArgs = [str(lazbuild.resolve())]
        if mode is not None:
            callArgs.append(f"--build-mode={mode}")
        callArgs.append(str(self.lpiFile.resolve()))
        return call(callArgs) == 0
    def getModes(self):
        optNode = self.tree.getroot().find("ProjectOptions")
        modeNode = optNode.find("BuildModes")
        return [item.attrib["Name"] for item in modeNode]
    def getDependencies(self):
        optNode = self.tree.getroot().find("ProjectOptions")
        reqNode = optNode.find("RequiredPackages")
        return [item[0].attrib["Value"] for item in reqNode]
