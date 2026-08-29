import json
import tempfile
import unittest
from pathlib import Path

from garmin_bridge import make_sleep_row, read_cache, write_cache


class GarminBridgeTests(unittest.TestCase):
    def test_sleep_mapping_and_units(self):
        row = make_sleep_row(
            "2026-08-10",
            {
                "sleepStartTimestampLocal": 1780000000000,
                "sleepEndTimestampLocal": 1780025200000,
                "totalSleepTimeSeconds": 25200,
                "sleepScores": {"overall": {"value": 88}},
                "restingHeartRate": 49,
            },
        )
        self.assertEqual(row["Date"], "2026-08-10")
        self.assertEqual(row["Sleep_Score"], 88)
        self.assertEqual(row["RHR"], 49)
        self.assertEqual(row["Sleep_Duration"], 7)
        self.assertTrue(row["bedtime"])

    def test_sleep_mapping_matches_nested_garmin_response(self):
        row = make_sleep_row(
            "2026-08-10",
            {
                "dailySleepDTO": {
                    "sleepStartTimestampLocal": 1780000000000,
                    "sleepEndTimestampLocal": 1780025200000,
                    "sleepTimeSeconds": 25200,
                    "sleepScores": {"overall": {"value": 88}},
                },
                "avgOvernightHrv": 52,
                "restingHeartRate": 49,
            },
        )
        self.assertEqual(row["Sleep_Score"], 88)
        self.assertEqual(row["HRV"], 52)
        self.assertEqual(row["RHR"], 49)
        self.assertEqual(row["Sleep_Duration"], 7)
        self.assertTrue(row["bedtime"])
        self.assertTrue(row["waketime"])

    def test_cache_envelope_and_staleness(self):
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "cache.json"
            write_cache(path, "garmin", "get_sleep_data", {"value": 1})
            payload, fresh = read_cache(path, 3600)
            self.assertTrue(fresh)
            self.assertEqual(payload["value"], 1)
            envelope = json.loads(path.read_text(encoding="utf-8"))
            self.assertEqual(envelope["source"], "garmin")
            self.assertEqual(envelope["wrapper"], "python-garminconnect")


if __name__ == "__main__":
    unittest.main()
