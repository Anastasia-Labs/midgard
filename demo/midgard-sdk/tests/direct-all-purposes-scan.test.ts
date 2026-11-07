import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  admitAllPurposesExecution,
  allPurposesExpectedScriptIntegrityHash,
  allPurposesFacts,
  allPurposesScriptLanguages,
  DirectAllPurposesFacts,
} from "../src/fraud-proof/direct-all-purposes-scan.js";

const a = "a1".repeat(28);
const b = "b2".repeat(28);
const c = "c3".repeat(28);

type Row = readonly [
  executions: readonly (readonly [string, bigint])[],
  probe: string | null,
  bitmap: bigint,
  reached: boolean,
];

const facts = (
  executions: Row[0],
  probe: string | null,
): DirectAllPurposesFacts =>
  allPurposesFacts(
    executions.map(([scriptHash, languageTag]) => ({
      scriptHash,
      languageTag,
    })),
    probe,
  );

describe("all-purposes scan twin", () => {
  // The same rows are pinned on-chain in all-purposes-scan.test.ak
  // (twin_table); change both together.
  it("agrees with the on-chain twin table", () => {
    const rows: readonly Row[] = [
      [[], null, 0n, false],
      [[[a, 0n]], a, 0n, true],
      [[[a, 3n]], null, 1n, false],
      [[[a, 128n]], b, 2n, false],
      [
        [
          [a, 3n],
          [b, 128n],
          [c, 3n],
        ],
        c,
        3n,
        true,
      ],
      [
        [
          [a, 128n],
          [a, 128n],
        ],
        b,
        2n,
        false,
      ],
      [
        [
          [b, 0n],
          [a, 3n],
        ],
        b,
        1n,
        true,
      ],
      [
        [
          [c, 0n],
          [c, 0n],
        ],
        null,
        0n,
        false,
      ],
    ];
    for (const [executions, probe, bitmap, reached] of rows)
      expect(facts(executions, probe)).toEqual({
        language_bitmap: bitmap,
        probe,
        probe_reached: reached,
      });
  });

  it("does not depend on the order of the executions", () => {
    const executions = [
      [a, 3n],
      [b, 0n],
      [c, 128n],
    ] as const;
    const forward = facts(executions, b);
    expect(facts([...executions].reverse(), b)).toEqual(forward);
    expect(facts([executions[1], executions[2], executions[0]], b)).toEqual(
      forward,
    );
  });

  it("keeps a reached probe reached", () => {
    const reached = admitAllPurposesExecution(
      { language_bitmap: 0n, probe: a, probe_reached: false },
      { scriptHash: a, languageTag: 0n },
    );
    expect(
      admitAllPurposesExecution(reached, { scriptHash: b, languageTag: 3n }),
    ).toEqual({ language_bitmap: 1n, probe: a, probe_reached: true });
  });

  it("round-trips the facts through their wire form", () => {
    const value = facts(
      [
        [a, 3n],
        [b, 128n],
      ],
      a,
    );
    expect(
      Data.from(Data.to(value, DirectAllPurposesFacts), DirectAllPurposesFacts),
    ).toEqual(value);
  });
});

describe("all-purposes expected script integrity hash", () => {
  const redeemerTxWitsHash = Buffer.alloc(32, 0x11);

  // Pinned on-chain in script-language-views-v1.test.ak.
  it("matches the on-chain vectors for every bitmap", () => {
    expect(
      [0n, 1n, 2n, 3n].map((bitmap) =>
        allPurposesExpectedScriptIntegrityHash(redeemerTxWitsHash, bitmap),
      ),
    ).toEqual([
      "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53",
      "b37aff1034628d20a78418d2c2cf20796ac08c83467b6f8558e6cea542de4c26",
      "74c606847a40bed5f147cd6cca208fa1b0db6a130f7d86d7c478fd5838d6d878",
      "ceab70e09654e1f35ddf1566b15861055566fda429e056107de070c58c8b3991",
    ]);
  });

  it("names each bit's language", () => {
    expect(allPurposesScriptLanguages(0n)).toEqual([]);
    expect(allPurposesScriptLanguages(1n)).toEqual(["PlutusV3"]);
    expect(allPurposesScriptLanguages(2n)).toEqual(["MidgardV1"]);
    expect(allPurposesScriptLanguages(3n)).toEqual(["PlutusV3", "MidgardV1"]);
  });

  it("refuses a bitmap outside the known languages", () => {
    for (const bitmap of [-1n, 4n])
      expect(() =>
        allPurposesExpectedScriptIntegrityHash(redeemerTxWitsHash, bitmap),
      ).toThrow(`all-purposes language bitmap ${bitmap.toString()} differs`);
  });
});
