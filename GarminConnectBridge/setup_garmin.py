#!/usr/bin/env python3
"""Interactive Garmin Connect setup.

Creates a local token store and a project .env file. The Garmin password is
used only for login and is never written to disk. MFA is requested by the
python-garminconnect callback when Garmin asks for it.
"""

from __future__ import annotations

import getpass
import os
from pathlib import Path


PROJECT_ROOT = Path(__file__).resolve().parent.parent
DEFAULT_TOKEN_STORE = PROJECT_ROOT / ".garminconnect"
ENV_PATH = PROJECT_ROOT / ".env"


def prompt_mfa() -> str:
    return input("Garmin MFA-Code: ").strip()


def write_env(email: str, token_store: Path) -> None:
    lines = [
        "# Local Garmin Connect settings. Do not commit this file.",
        f"GARMIN_EMAIL={email}",
        f"GARMINTOKENS={token_store.as_posix()}",
        "",
    ]
    ENV_PATH.write_text("\n".join(lines), encoding="utf-8")


def main() -> int:
    try:
        from garminconnect import Garmin
    except ImportError:
        print("Fehlendes Paket. Installiere zuerst:")
        print("  python -m pip install -r GarminConnectBridge/requirements.txt")
        return 1

    print("Garmin-Connect-Setup")
    print("Das Passwort wird nur für den Login verwendet und nicht gespeichert.\n")
    email = input("Garmin E-Mail: ").strip()
    if not email:
        print("Keine E-Mail angegeben.")
        return 1
    password = getpass.getpass("Garmin Passwort: ")
    token_input = input(f"Token-Ordner [{DEFAULT_TOKEN_STORE}]: ").strip()
    token_store = Path(token_input).expanduser() if token_input else DEFAULT_TOKEN_STORE
    token_store.mkdir(parents=True, exist_ok=True)

    print("\nMelde bei Garmin an. Falls MFA aktiviert ist, wird gleich ein Code abgefragt ...")
    client = Garmin(email, password, prompt_mfa=prompt_mfa)
    try:
        client.login(str(token_store))
    except Exception as exc:
        print(f"Garmin-Login fehlgeschlagen: {exc}")
        return 1

    write_env(email, token_store.resolve())
    try:
        os.chmod(ENV_PATH, 0o600)
    except OSError:
        pass
    print(f"\nErfolgreich. Token-Cache: {token_store.resolve()}")
    print(f"Umgebungsdatei erstellt: {ENV_PATH}")
    print("Das Passwort wurde nicht gespeichert.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
