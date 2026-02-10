#!/usr/bin/env python3
# Runs the correct test-gadget client program in .test-gadget/test-gadget-client-$PLATFORM

import os
import platform
import sys
from pathlib import Path


def get_platform_binary() -> str:
    system = platform.system().lower()
    if system == "darwin":
        return "test-gadget-client-macos"
    elif system == "windows":
        return "test-gadget-client-windows.exe"
    elif system == "linux":
        return "test-gadget-client-linux"
    else:
        print(f"Unsupported platform: {system}", file=sys.stderr)
        sys.exit(1)


script_dir = Path(__file__).parent
dist_dir = script_dir / ".test-gadget"
binary = dist_dir / get_platform_binary()

if not binary.exists():
    print(f"Program not found: {binary}", file=sys.stderr)
    sys.exit(1)

os.execv(str(binary), [str(binary)] + sys.argv[1:])
