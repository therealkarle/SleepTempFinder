#!/usr/bin/env python3
"""Refresh Garmin authentication and notify when MFA/token authentication expires."""

from __future__ import annotations

import os
import sys
from pathlib import Path

from garmin_credentials import load_credentials


def notify(message: str) -> None:
    print(message, file=sys.stderr)
    if os.name != "nt":
        return
    try:
        import subprocess
        escaped = message.replace("'", "''")
        script = (
            "Add-Type -AssemblyName PresentationFramework; "
            f"[System.Windows.MessageBox]::Show('{escaped}','Garmin MFA') | Out-Null"
        )
        subprocess.run(["powershell", "-NoProfile", "-Command", script], check=False)
    except Exception:
        pass


def main() -> int:
    try:
        from garminconnect import Garmin
    except ImportError:
        notify("Garmin MFA kann nicht aktualisiert werden: garminconnect fehlt.")
        return 1
    credentials = load_credentials()
    if not credentials:
        notify("Keine verschlüsselten Garmin-Zugangsdaten gefunden. Erst setup_garmin.py ausführen.")
        return 1
    email, password = credentials
    token_store = Path(os.getenv("GARMINTOKENS", Path(__file__).resolve().parent.parent / ".garminconnect"))
    token_store.mkdir(parents=True, exist_ok=True)

    def prompt_mfa() -> str:
        return input("Neuer Garmin MFA-Code: ").strip()

    try:
        client = Garmin(email, password, prompt_mfa=prompt_mfa)
        client.login(str(token_store))
    except Exception as exc:
        notify(f"Garmin MFA/Token abgelaufen oder Login fehlgeschlagen: {exc}")
        return 1
    print(f"Garmin-Token erfolgreich aktualisiert: {token_store.resolve()}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
