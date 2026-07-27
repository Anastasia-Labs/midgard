import assert from "node:assert/strict";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";

import {
  parseValidatorTitles,
  readValidatorMeasurements,
  restoreBlueprint,
  snapshotBlueprint,
} from "./run-diagnostic-build.mjs";

const withTemporaryDirectory = (callback) => {
  const directory = mkdtempSync(join(tmpdir(), "midgard-diagnostic-test-"));
  try {
    return callback(directory);
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
};

test("restores the exact existing blueprint bytes", () => {
  withTemporaryDirectory((directory) => {
    const blueprintPath = join(directory, "plutus.json");
    writeFileSync(blueprintPath, Buffer.from([0, 1, 2, 255]));
    const snapshotRoot = mkdtempSync(join(directory, "snapshot-"));

    try {
      const snapshot = snapshotBlueprint(blueprintPath, snapshotRoot);
      writeFileSync(blueprintPath, "generated");
      restoreBlueprint(snapshot);
      assert.deepEqual(
        readFileSync(blueprintPath),
        Buffer.from([0, 1, 2, 255]),
      );
    } finally {
      rmSync(snapshotRoot, { recursive: true, force: true });
    }
  });
});

test("restores an absent blueprint by removing generated output", () => {
  withTemporaryDirectory((directory) => {
    const blueprintPath = join(directory, "plutus.json");
    const snapshotRoot = mkdtempSync(join(directory, "snapshot-"));
    const snapshot = snapshotBlueprint(blueprintPath, snapshotRoot);
    writeFileSync(blueprintPath, "{}");

    restoreBlueprint(snapshot);
    assert.throws(() => readFileSync(blueprintPath), { code: "ENOENT" });
  });
});

test("reports exact raw bytes for requested validators", () => {
  withTemporaryDirectory((directory) => {
    const blueprintPath = join(directory, "plutus.json");
    writeFileSync(
      blueprintPath,
      JSON.stringify({
        validators: [
          {
            title: "fraud_proofs/example.main.spend",
            compiledCode: "00aaff",
            parameters: [{ title: "policy" }],
          },
        ],
      }),
    );

    assert.deepEqual(
      readValidatorMeasurements(
        blueprintPath,
        new Set(["fraud_proofs/example.main.spend"]),
      ),
      [
        {
          title: "fraud_proofs/example.main.spend",
          rawBytes: 3,
          parameterCount: 1,
        },
      ],
    );
  });
});

test("rejects invalid, duplicate, and missing validator titles", () => {
  assert.throws(() => parseValidatorTitles([]), /usage/u);
  assert.throws(
    () =>
      parseValidatorTitles([
        "fraud_proofs/example.main.spend",
        "fraud_proofs/example.main.spend",
      ]),
    /unique/u,
  );
  assert.throws(
    () => parseValidatorTitles(["../example.main.spend"]),
    /usage/u,
  );

  withTemporaryDirectory((directory) => {
    const blueprintPath = join(directory, "plutus.json");
    writeFileSync(blueprintPath, JSON.stringify({ validators: [] }));
    assert.throws(
      () =>
        readValidatorMeasurements(
          blueprintPath,
          new Set(["fraud_proofs/example.main.spend"]),
        ),
      /missing/u,
    );
  });
});
