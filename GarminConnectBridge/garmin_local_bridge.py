#!/usr/bin/env python3
"""Read the garmin-local-mcp SQLite warehouse without write access."""

from __future__ import annotations

import argparse
import json
import sqlite3
from pathlib import Path
from typing import Any


def _value(row: dict[str, Any], name: str) -> Any:
    return row.get(name)


def _minutes_to_hours(value: Any) -> float | None:
    if value is None or value == "":
        return None
    try:
        return float(value) / 60.0
    except (TypeError, ValueError):
        return None


def _minutes_to_seconds(value: Any) -> float | None:
    if value is None or value == "":
        return None
    try:
        return float(value) * 60.0
    except (TypeError, ValueError):
        return None


def _table_columns(conn: sqlite3.Connection, table: str) -> set[str]:
    return {row[1] for row in conn.execute(f"PRAGMA table_info({table})")}


def _read_table(conn: sqlite3.Connection, table: str) -> list[dict[str, Any]]:
    return [dict(row) for row in conn.execute(f"SELECT * FROM {table} ORDER BY date")]


def read_history(db_path: str | Path) -> list[dict[str, Any]]:
    path = Path(db_path).expanduser()
    if not path.is_file():
        raise FileNotFoundError(f"Garmin Local MCP database not found: {path}")

    # `file:C:/...` is parsed incorrectly by SQLite on Windows. `as_uri()`
    # produces the portable `file:///C:/...` form while keeping URI mode.
    uri = f"{path.resolve().as_uri()}?mode=ro"
    try:
        conn = sqlite3.connect(uri, uri=True, timeout=5)
        conn.row_factory = sqlite3.Row
        conn.execute("PRAGMA query_only = ON")
        tables = {
            row[0]
            for row in conn.execute(
                "SELECT name FROM sqlite_master WHERE type = 'table'"
            )
        }
        if "sleep" not in tables:
            raise RuntimeError("Garmin Local MCP database has no required 'sleep' table")

        sleep_columns = _table_columns(conn, "sleep")
        if "date" not in sleep_columns:
            raise RuntimeError("Garmin Local MCP 'sleep' table has no required 'date' column")

        sleep_rows = _read_table(conn, "sleep")
        hrv_rows = _read_table(conn, "hrv") if "hrv" in tables else []
        wellness_rows = _read_table(conn, "daily_wellness") if "daily_wellness" in tables else []
        conn.close()
    except sqlite3.Error as exc:
        raise RuntimeError(f"Could not read Garmin Local MCP database read-only: {exc}") from exc

    hrv_by_date = {row.get("date"): row for row in hrv_rows}
    wellness_by_date = {row.get("date"): row for row in wellness_rows}
    output: list[dict[str, Any]] = []
    for sleep in sleep_rows:
        day = sleep.get("date")
        hrv = hrv_by_date.get(day, {})
        wellness = wellness_by_date.get(day, {})
        row: dict[str, Any] = {
            "Date": day,
            "Sleep_Score": _value(sleep, "score"),
            "Sleep_Duration": _minutes_to_hours(_value(sleep, "duration_min")),
            "bedtime": _value(sleep, "start_ts"),
            "waketime": _value(sleep, "end_ts"),
            "Deep_Sleep_Seconds": _minutes_to_seconds(_value(sleep, "deep_min")),
            "REM_Seconds": _minutes_to_seconds(_value(sleep, "rem_min")),
            "Wake_Time": _minutes_to_hours(_value(sleep, "awake_min")),
            "Restless_Moments": _value(sleep, "restless_moments"),
            "Stress": _value(sleep, "avg_stress"),
            "HRV": _value(hrv, "last_night_avg"),
            "RHR": _value(wellness, "resting_hr"),
            "Quality_Flags": _value(sleep, "quality_flags"),
            "Source_File": "garmin-local://garmin.db",
            "Source_Name": "Garmin Local MCP SQLite warehouse",
            "Sleep_Source": "garmin_local",
        }
        output.append(row)
    return output


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--db-path", required=True)
    parser.add_argument("--output", required=True)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    rows = read_history(args.db_path)
    output = Path(args.output)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(json.dumps(rows, ensure_ascii=False), encoding="utf-8")
    print(f"Garmin Local MCP: loaded {len(rows)} sleep rows", flush=True)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as exc:
        print(f"Garmin Local MCP bridge failed: {exc}")
        raise SystemExit(1)
