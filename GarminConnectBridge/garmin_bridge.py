#!/usr/bin/env python3
"""Small read-only bridge around python-garminconnect.

The R application calls this script instead of importing Python packages from
R.  Responses are cached as JSON envelopes so the raw Garmin response remains
available for later mappings and offline analysis.
"""

from __future__ import annotations

import argparse
import getpass
import hashlib
import json
import os
import sys
from datetime import date, datetime, timedelta, timezone
from pathlib import Path
from typing import Any


def load_project_env() -> None:
    """Load the setup script's simple KEY=value .env without a dependency."""
    env_path = Path(__file__).resolve().parent.parent / ".env"
    if not env_path.exists():
        return
    for raw_line in env_path.read_text(encoding="utf-8").splitlines():
        line = raw_line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        key, value = line.split("=", 1)
        os.environ.setdefault(key.strip(), value.strip().strip('"').strip("'"))


load_project_env()


BASE_ALIASES = {
    "Date": ("calendarDate", "date", "sleepDate", "sleepDay"),
    "bedtime": (
        "dailySleepDTO.sleepStartTimestampLocal",
        "sleepStartTimestampLocal",
        "sleepStartTimeLocal",
        "sleepStart",
    ),
    "waketime": (
        "dailySleepDTO.sleepEndTimestampLocal",
        "sleepEndTimestampLocal",
        "sleepEndTimeLocal",
        "sleepEnd",
    ),
    "Sleep_Score": (
        "dailySleepDTO.sleepScores.overall.value",
        "sleepScores.overall.value",
        "sleepScore",
        "overallSleepScore",
    ),
    "HRV": ("avgOvernightHrv", "averageOvernightHrv", "overnightHrv"),
    "RHR": ("restingHeartRate", "restingHr", "restingHeartRateValue"),
    "Sleep_Duration": (
        "dailySleepDTO.sleepTimeSeconds",
        "totalSleepTimeSeconds",
        "sleepDurationSeconds",
        "totalSleepTime",
    ),
    "Deep_Sleep_Seconds": (
        "dailySleepDTO.deepSleepSeconds", "deepSleepSeconds", "deepSleepTimeSeconds",
    ),
    "REM_Seconds": (
        "dailySleepDTO.remSleepSeconds", "remSleepSeconds", "remSleepTimeSeconds",
    ),
    "Wake_Time": (
        "dailySleepDTO.awakeSleepSeconds", "awakeSleepSeconds", "awakeTimeSeconds",
    ),
    "Restless_Moments": (
        "dailySleepDTO.restlessMomentsCount", "restlessMomentsCount", "restlessMoments",
    ),
    "Stress": (
        "dailySleepDTO.avgSleepStress", "dailySleepDTO.averageStressLevel",
        "averageStressLevel", "avgStressLevel",
    ),
}

ENDPOINTS = {
    "hrv": "get_hrv_data",
    "respiration": "get_respiration_data",
    "spo2": "get_spo2_data",
    "stress": "get_all_day_stress",
    "body_battery": "get_body_battery",
    "heart_rate": "get_heart_rates",
    "lifestyle_logging": "get_lifestyle_logging_data",
}


def norm(value: Any) -> str:
    return "".join(ch.lower() for ch in str(value) if ch.isalnum())


def flatten(value: Any, prefix: str = "") -> dict[str, Any]:
    out: dict[str, Any] = {}
    if isinstance(value, dict):
        for key, child in value.items():
            path = f"{prefix}.{key}" if prefix else str(key)
            out.update(flatten(child, path))
    elif isinstance(value, list):
        # Endpoint responses often contain one daily summary plus samples. Keep
        # scalar lists as JSON; nested samples are handled by their caller.
        if value and all(not isinstance(item, (dict, list)) for item in value):
            out[prefix] = value
    else:
        out[prefix] = value
    return out


def pick(flat: dict[str, Any], aliases: tuple[str, ...]) -> Any:
    by_norm = {norm(key): value for key, value in flat.items()}
    for alias in aliases:
        if norm(alias) in by_norm:
            return by_norm[norm(alias)]
    return None


def as_seconds(value: Any) -> float | None:
    if value is None or value == "":
        return None
    try:
        number = float(value)
    except (TypeError, ValueError):
        return None
    # Garmin duration fields are normally seconds; small values are already
    # likely hours when supplied by a compatible API fixture.
    return number / 3600 if number > 24 * 60 else number / 3600


def as_datetime(value: Any) -> str | None:
    if value is None or value == "":
        return None
    if isinstance(value, (int, float)):
        seconds = float(value) / (1000 if float(value) > 10_000_000_000 else 1)
        return datetime.fromtimestamp(seconds, timezone.utc).isoformat()
    text = str(value).strip()
    if text.isdigit():
        return as_datetime(int(text))
    return text


def cache_key(source: str, endpoint: str, day: str, identity: str) -> str:
    raw = f"{source}|{endpoint}|{day}|{identity}".encode()
    return hashlib.sha256(raw).hexdigest()[:24]


def read_cache(path: Path, ttl_seconds: int) -> tuple[Any, bool] | None:
    if not path.exists():
        return None
    try:
        envelope = json.loads(path.read_text(encoding="utf-8"))
        fetched = datetime.fromisoformat(envelope["fetched_at"])
        fresh = (datetime.now(timezone.utc) - fetched).total_seconds() <= ttl_seconds
        return envelope["payload"], fresh
    except (OSError, ValueError, KeyError, json.JSONDecodeError):
        return None


def write_cache(path: Path, source: str, endpoint: str, payload: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    envelope = {
        "source": source,
        "endpoint": endpoint,
        "fetched_at": datetime.now(timezone.utc).isoformat(),
        "wrapper": "python-garminconnect",
        "payload": payload,
    }
    temp = path.with_suffix(path.suffix + ".tmp")
    temp.write_text(json.dumps(envelope, ensure_ascii=False), encoding="utf-8")
    temp.replace(path)


def fetch_cached(client: Any, method_name: str, day: str, args: list[Any], *, cache_dir: Path, ttl: int, identity: str) -> Any:
    key = cache_key("garmin", method_name, day, identity)
    path = cache_dir / f"{key}.json"
    cached = read_cache(path, ttl)
    if cached and cached[1]:
        return cached[0]
    try:
        payload = getattr(client, method_name)(*args)
        write_cache(path, "garmin", method_name, payload)
        return payload
    except Exception as exc:
        message = str(exc).lower()
        auth_error = any(token in message for token in ("401", "403", "authentication", "unauthorized", "forbidden"))
        if cached and not auth_error:
            print(f"Warning: using stale Garmin cache for {method_name} {day}", file=sys.stderr)
            return cached[0]
        raise


def make_sleep_row(day: str, payload: Any) -> dict[str, Any]:
    flat = flatten(payload)
    row: dict[str, Any] = {"Date": day}
    for name, aliases in BASE_ALIASES.items():
        if name == "Date":
            continue
        value = pick(flat, aliases)
        if name in ("bedtime", "waketime"):
            value = as_datetime(value)
        elif name in ("Sleep_Duration", "Wake_Time"):
            value = as_seconds(value)
        row[name] = value
    return row


def merge_endpoint_metric(row: dict[str, Any], name: str, payload: Any) -> None:
    flat = flatten(payload)
    aliases = {
        "hrv": ("weeklyAvg", "lastNightAvg", "avgOvernightHrv", "value"),
        "respiration": ("avgWakingRespirationValue", "avgSleepRespirationValue", "average"),
        "spo2": ("averageSpO2", "avgSpO2", "value"),
        "stress": ("averageStressLevel", "avgStressLevel", "value"),
        "body_battery": ("charged", "highestValue", "value"),
        "heart_rate": ("restingHeartRate", "minHeartRate", "value"),
    }.get(name, ())
    value = pick(flat, aliases)
    if value is not None:
        row[f"Garmin_{name}"] = value


def build_client() -> Any:
    try:
        from garminconnect import Garmin
    except ImportError as exc:
        raise RuntimeError("python-garminconnect is required; install garminconnect and curl_cffi") from exc
    from garmin_credentials import load_credentials
    credentials = load_credentials()
    email = os.getenv("GARMIN_EMAIL") or (credentials[0] if credentials else input("Garmin email: ").strip())
    password = os.getenv("GARMIN_PASSWORD") or (credentials[1] if credentials else getpass.getpass("Garmin password: "))
    client = Garmin(email, password, prompt_mfa=lambda: input("Garmin MFA code: ").strip())
    token_store = os.getenv("GARMINTOKENS")
    client.login(token_store) if token_store else client.login()
    return client


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--start", required=True)
    parser.add_argument("--end", required=True)
    parser.add_argument("--cache-dir", default=".cache/garmin")
    parser.add_argument("--ttl", type=int, default=86400)
    parser.add_argument("--metrics", default="")
    parser.add_argument("--identity", default="default")
    parser.add_argument("--lifestyle-output-dir", default="")
    parser.add_argument("--output", required=True)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    start = date.fromisoformat(args.start)
    end = date.fromisoformat(args.end)
    cache_dir = Path(args.cache_dir)
    metrics = {item.strip() for item in args.metrics.split(",") if item.strip()}
    client = build_client()
    rows: list[dict[str, Any]] = []
    day = start
    while day <= end:
        day_text = day.isoformat()
        print(f"Garmin: loading {day_text}", file=sys.stderr, flush=True)
        sleep = fetch_cached(client, "get_sleep_data", day_text, [day_text], cache_dir=cache_dir, ttl=args.ttl, identity=args.identity)
        row = make_sleep_row(day_text, sleep)
        for metric in sorted(metrics):
            method = ENDPOINTS.get(metric)
            if not method:
                continue
            payload = fetch_cached(client, method, day_text, [day_text], cache_dir=cache_dir, ttl=args.ttl, identity=args.identity)
            if metric == "lifestyle_logging":
                row["Garmin_LifestyleLogging"] = payload
                if args.lifestyle_output_dir:
                    output_dir = Path(args.lifestyle_output_dir)
                    output_dir.mkdir(parents=True, exist_ok=True)
                    (output_dir / f"{day_text}_LifestyleLogging.json").write_text(
                        json.dumps(payload, ensure_ascii=False), encoding="utf-8"
                    )
            else:
                merge_endpoint_metric(row, metric, payload)
        rows.append(row)
        day += timedelta(days=1)
    output_path = Path(args.output)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(json.dumps(rows, ensure_ascii=False), encoding="utf-8")
    print(f"Garmin result written: {output_path}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as exc:
        print(f"Garmin bridge failed: {exc}", file=sys.stderr)
        raise SystemExit(1)
