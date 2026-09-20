import json
import tempfile
import unittest
import zipfile
from pathlib import Path

from lifestyle_sleep_analysis import ExportReader, analyse, write_outputs


class LifestyleSleepAnalysisTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name) / "GarminUserData" / "2026.01"
        wellness = self.root / "DI_CONNECT" / "DI-Connect-Wellness"
        wellness.mkdir(parents=True)
        (wellness / "123_LifestyleLogging.json").write_text(json.dumps({"dailyLogList": [
            {"calendarDate": [2026, 1, 1], "behaviourName": "Walk", "status": "done"},
            {"calendarDate": [2026, 1, 2], "behaviourName": "Walk", "status": "no"},
            {"calendarDate": [2026, 1, 2], "behaviourName": "Meditation", "status": True},
        ]}), encoding="utf-8")
        (self.root / "Sleep.csv").write_text(
            "Datum,Score,Dauer,HRV,Ruheherzfrequenz\n"
            "2026-01-01,90,8h 00min,50,40\n"
            "2026-01-02,70,6h 00min,30,50\n"
            "2026-01-03,80,7h 00min,40,45\n", encoding="utf-8")

    def tearDown(self):
        self.temp.cleanup()

    def config(self):
        return {"start_date": "2026-01-01", "end_date": "2026-01-03", "missing_activity_is_no": True,
                "missing_activity_is_no_by_activity": {"Meditation": False}, "sleep_metrics": ["Sleep_Score", "Sleep_Duration"],
                "value_interval": 0.8, "confidence_level": 0.95, "significance_level": 0.05}

    def test_folder_import_missing_activity_and_duration(self):
        reader = ExportReader(self.root)
        result = analyse(self.config(), reader)
        walk = next(r for r in result["results"] if r["activity"] == "Walk" and r["metric"] == "Sleep_Score")
        self.assertEqual(walk["done_n"], 1)
        self.assertEqual(walk["not_done_n"], 2)
        self.assertEqual(walk["done_mean"], 90)
        duration = next(r for r in result["results"] if r["activity"] == "Walk" and r["metric"] == "Sleep_Duration")
        self.assertEqual(duration["done_mean"], 8)
        reader.close()

    def test_zip_import_and_outputs(self):
        archive = Path(self.temp.name) / "export.zip"
        with zipfile.ZipFile(archive, "w") as zf:
            for path in self.root.rglob("*"):
                if path.is_file():
                    zf.write(path, path.relative_to(self.root.parent.parent))
        reader = ExportReader(archive)
        result = analyse(self.config(), reader)
        output = Path(self.temp.name) / "out"
        write_outputs(result, output, self.config())
        self.assertTrue((output / "significant_positive.csv").exists())
        self.assertTrue((output / "significant_negative.csv").exists())
        self.assertTrue((output / "not_significant.csv").exists())
        self.assertTrue((output / "lifestyle_sleep_analysis.json").exists())
        reader.close()


if __name__ == "__main__":
    unittest.main()
