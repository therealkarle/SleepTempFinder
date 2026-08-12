"""Windows DPAPI-backed local Garmin credential storage."""

from __future__ import annotations

import base64
import ctypes
import ctypes.wintypes
import json
import os
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent
CREDENTIALS_PATH = ROOT / ".garmin-credentials.dpapi"


class DATA_BLOB(ctypes.Structure):
    _fields_ = [("cbData", ctypes.wintypes.DWORD), ("pbData", ctypes.POINTER(ctypes.c_char))]


def _protect(data: bytes, *, unprotect: bool = False) -> bytes:
    if os.name != "nt":
        raise RuntimeError("Lokale Passwortverschlüsselung ist derzeit nur unter Windows unterstützt.")
    source = DATA_BLOB(len(data), ctypes.cast(ctypes.create_string_buffer(data), ctypes.POINTER(ctypes.c_char)))
    target = DATA_BLOB()
    crypt = ctypes.windll.crypt32.CryptUnprotectData if unprotect else ctypes.windll.crypt32.CryptProtectData
    if not crypt(ctypes.byref(source), None, None, None, None, 0, ctypes.byref(target)):
        raise ctypes.WinError()
    try:
        return ctypes.string_at(target.pbData, target.cbData)
    finally:
        ctypes.windll.kernel32.LocalFree(target.pbData)


def save_credentials(email: str, password: str, path: Path = CREDENTIALS_PATH) -> None:
    payload = json.dumps({"email": email, "password": password}, ensure_ascii=False).encode("utf-8")
    path.write_text(base64.b64encode(_protect(payload)).decode("ascii"), encoding="ascii")
    try:
        os.chmod(path, 0o600)
    except OSError:
        pass


def load_credentials(path: Path = CREDENTIALS_PATH) -> tuple[str, str] | None:
    if not path.exists():
        return None
    encrypted = base64.b64decode(path.read_text(encoding="ascii"))
    payload = json.loads(_protect(encrypted, unprotect=True).decode("utf-8"))
    return str(payload["email"]), str(payload["password"])
