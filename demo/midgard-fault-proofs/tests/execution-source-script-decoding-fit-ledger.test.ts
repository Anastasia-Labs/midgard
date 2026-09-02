import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

const path = new URL(
  "../../../docs/fault-proofs/size-plans/execution-source-script-decoding-v1-fit-ledger.json",
  import.meta.url,
);

describe("executionSourceScriptDecoding fit ledger", () => {
  it("reproduces the maximum shape and positive signed/ex-unit margins", async () => {
    const bytes = await readFile(path);
    const ledger = JSON.parse(bytes.toString("utf8")) as {
      categoryId: string;
      maximum: {
        scriptItemBytes: number;
        boundedChunks: number;
        nodeAndDepthBoundary: number;
      };
      referencePublications: {
        signedBytes: number;
        reserveMarginBytes: number;
      }[];
      focusedAikenMaximum: { memoryMargin: number; cpuMargin: number };
      forcedLifecycle: {
        stage: string;
        signedBytes: number;
        byteMargin: number;
        memory: number;
        cpu: number;
      }[];
      acceptedMalformedLifecycle: {
        stage: string;
        signedBytes: number;
        byteMargin: number;
        memory: number;
        cpu: number;
      }[];
      evidenceDigest: string;
    };
    expect(ledger.categoryId).toBe("00000031");
    expect(ledger.maximum).toEqual({
      scriptItemBytes: 32_768,
      boundedChunks: 9,
      nodeAndDepthBoundary: 16_384,
    });
    expect(
      ledger.referencePublications.map(({ signedBytes }) => signedBytes),
    ).toEqual([15_032, 15_730, 6_777, 12_217, 2_990]);
    expect(
      ledger.referencePublications.every(
        ({ reserveMarginBytes }) => reserveMarginBytes > 0,
      ),
    ).toBe(true);
    expect(ledger.focusedAikenMaximum.memoryMargin).toBeGreaterThan(0);
    expect(ledger.focusedAikenMaximum.cpuMargin).toBeGreaterThan(0);
    expect(ledger.forcedLifecycle.map(({ stage }) => stage)).toEqual([
      "init",
      "step01-forced",
      "step02-authenticate",
      "step03-open-item",
      "step04-scan-0",
      "step05-mint",
      "remove",
    ]);
    expect(
      ledger.forcedLifecycle.every(
        ({ byteMargin, memory, cpu }) =>
          byteMargin > 0 && memory > 0 && cpu > 0,
      ),
    ).toBe(true);
    expect(ledger.acceptedMalformedLifecycle.map(({ stage }) => stage)).toEqual(
      [
        "init",
        "step01-accepted",
        "step02-authenticate",
        "step03-open-item",
        "step04-scan-0",
        "step05-mint",
        "remove",
      ],
    );
    expect(
      ledger.acceptedMalformedLifecycle.every(
        ({ byteMargin, memory, cpu }) =>
          byteMargin > 0 && memory > 0 && cpu > 0,
      ),
    ).toBe(true);
    const { evidenceDigest, ...evidence } = ledger;
    expect(
      createHash("sha256").update(JSON.stringify(evidence)).digest("hex"),
    ).toBe(evidenceDigest);
  });
});
