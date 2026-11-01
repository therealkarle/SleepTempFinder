import sqlite3
import tempfile
import unittest
from pathlib import Path

from garmin_local_bridge import read_history


class GarminLocalBridgeTests(unittest.TestCase):
    def test_garmin_local_reads_complete_history_and_joins_tables_read_only(self):
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "garmin.db"
            conn = sqlite3.connect(path)
            conn.executescript(
                """
                CREATE TABLE sleep (
                    date TEXT PRIMARY KEY, score INTEGER, duration_min REAL,
                    deep_min INTEGER, rem_min INTEGER, awake_min INTEGER,
                    start_ts TEXT, end_ts TEXT, avg_stress INTEGER,
                    restless_moments INTEGER, quality_flags TEXT
                );
                CREATE TABLE hrv (date TEXT PRIMARY KEY, last_night_avg INTEGER);
                CREATE TABLE daily_wellness (date TEXT PRIMARY KEY, resting_hr INTEGER);
                INSERT INTO sleep VALUES
                  ('2024-01-01', 80, 420, 90, 100, 30, '2023-12-31T23:00:00', '2024-01-01T06:00:00', 20, 4, NULL),
                  ('2026-09-10', 90, 450, 100, 110, 25, '2026-09-09T22:30:00', '2026-09-10T06:00:00', 15, 2, 'ok');
                INSERT INTO hrv VALUES ('2024-01-01', 42), ('2026-09-10', 55);
                INSERT INTO daily_wellness VALUES ('2024-01-01', 51), ('2026-09-10', 48);
                """
            )
            conn.commit()
            conn.close()

            rows = read_history(path)

            self.assertEqual([row["Date"] for row in rows], ["2024-01-01", "2026-09-10"])
            self.assertEqual(rows[0]["Sleep_Duration"], 7)
            self.assertEqual(rows[0]["Deep_Sleep_Seconds"], 5400)
            self.assertEqual(rows[0]["HRV"], 42)
            self.assertEqual(rows[0]["RHR"], 51)
            self.assertEqual(rows[1]["Sleep_Source"], "garmin_local")

            read_only = sqlite3.connect(f"file:{path.as_posix()}?mode=ro", uri=True)
            with self.assertRaises(sqlite3.OperationalError):
                read_only.execute("INSERT INTO sleep(date) VALUES ('2026-09-11')")
            read_only.close()

    def test_garmin_local_allows_missing_optional_tables(self):
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "garmin.db"
            conn = sqlite3.connect(path)
            conn.execute("CREATE TABLE sleep (date TEXT PRIMARY KEY, score INTEGER)")
            conn.execute("INSERT INTO sleep VALUES ('2026-01-01', 70)")
            conn.commit()
            conn.close()

            rows = read_history(path)
            self.assertEqual(rows[0]["Sleep_Score"], 70)
            self.assertIsNone(rows[0]["HRV"])
            self.assertIsNone(rows[0]["RHR"])


if __name__ == "__main__":
    unittest.main()
