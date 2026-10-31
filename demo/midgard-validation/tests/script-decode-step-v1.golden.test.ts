import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { midgardBlake2b } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  scriptDecodeRecordData,
  scriptDecodeRunData,
  serialiseScriptDecodeData,
} from "../src/script-decode.step.js";
import { planScriptDecodeWalk } from "../src/script-decode.step-walk.js";
import * as vectors from "./fixtures/script-decode-step-v1.vectors.mjs";

/**
 * The TypeScript half of the script-decode step golden channel.
 *
 * The walks are rebuilt here through `src/` from the same definitions the
 * generator drives through `dist/`, and compared with the checked-in fixture.
 * The generated Aiken module
 * (`onchain/aiken/lib/midgard/script-decode-v1-golden.test.ak`) replays every
 * walk with `step_with_frames_v1` and checks the same run-datum hashes.
 * Regenerate both with `pnpm run fixtures:script-decode-step-v1:sync`.
 */

const golden = JSON.parse(
  readFileSync(
    fileURLToPath(
      new URL(
        "./fixtures/script-decode-step-v1.generated.json",
        import.meta.url,
      ),
    ),
    "utf8",
  ),
) as { readonly rows: readonly vectors.ScriptDecodeStepRowV1[] };

const rows = vectors.buildScriptDecodeStepRows({
  planWalk: planScriptDecodeWalk,
  runData: scriptDecodeRunData,
  recordData: scriptDecodeRecordData,
  serialise: serialiseScriptDecodeData,
  blake2b256: (bytes) => midgardBlake2b(bytes, { dkLen: 32 }),
});

describe("script-decode step golden vectors", () => {
  it("declares exactly the exports the fixture module has", () => {
    expect(Object.keys(vectors).sort()).toEqual([
      "SCRIPT_DECODE_BLUEPRINT_SCRIPTS",
      "buildScriptDecodeStepRows",
      "scriptDecodeStepScripts",
    ]);
  });

  it("match what src/ walks today", () => {
    expect(rows).toEqual(golden.rows);
  });

  it("give each blueprint validator the script hash the blueprint gave it", () => {
    for (const [title, hash] of vectors.SCRIPT_DECODE_BLUEPRINT_SCRIPTS) {
      const row = rows.find((candidate) => candidate.label === title);
      expect(row?.scriptHash, title).toBe(hash);
      expect(row?.record, title).toMatchObject({
        k: "decoded",
        phase2Class: 0,
      });
    }
  });

  it("walk each row over more than one Walk, unless the Bind refuses it", () => {
    for (const row of rows) {
      if (row.steps.length === 0) {
        expect(row.record, row.label).toMatchObject({ k: "refused" });
      } else {
        expect(row.steps.length, row.label).toBeGreaterThan(1);
      }
    }
    expect(rows.filter((row) => row.steps.length === 0)).toHaveLength(1);
  });
});
