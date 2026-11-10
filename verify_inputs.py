from pathlib import Path
import csv, hashlib
p = Path(__file__).resolve().parent
rows = list(csv.DictReader((p / 'source_manifest.csv').open(encoding='utf-8-sig')))
for row in rows:
    file = p / row['path']
    if hashlib.sha256(file.read_bytes()).hexdigest() != row['sha256']:
        raise SystemExit('Checksum mismatch: ' + row['path'])
print(f'{len(rows)} frozen inputs verified.')
