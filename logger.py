import sys
from enum import Enum


class LogLevel(Enum):
    DEBUG=0
    INFO=1
    WARNING=2
    ERROR=3
    NONE=10

class Logger:
    def __init__(self, loglevel):
        self.loglevel = loglevel
        self.files = {
            LogLevel.DEBUG: [sys.stdout],
            LogLevel.INFO: [sys.stdout],
            LogLevel.WARNING: [sys.stderr],
            LogLevel.ERROR: [sys.stderr]
        }
    def __write(self, level, pref, message):
        if self.loglevel.value > level.value:
            return
        for f in self.files[level]:
            f.write(pref)
            f.write(message)
            f.write("\n")
    def debug(self, message):
        self.__write(LogLevel.DEBUG, "[DEBUG] ", message)
    def log(self, message):
        self.__write(LogLevel.INFO, "[INFO] ", message)
    def warning(self, message):
        self.__write(LogLevel.WARNING, "[WARN] ", message)
    def error(self, message):
        self.__write(LogLevel.ERROR, "[ERROR] ", message)

noLogger = Logger(LogLevel.NONE)
