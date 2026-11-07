import { createHash } from "node:crypto";
import {
  mkdir,
  mkdtemp,
  readFile,
  rm,
  stat,
  writeFile,
} from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { submitCapturedTransaction } from "@al-ft/midgard-fault-proofs";
import { makeGenuineFundingInitCaptureForTest } from "@al-ft/midgard-fault-proofs/test-support/funding-measurement";
import { describe, expect, it } from "vitest";

// This catches source/package instance drift between the genuine recorder and
// the operational writer; no existing writer test uses actual proof actions.
describe("declared genuine funding measurement producer output", () => {
  it("writes exact private signed SDK Init evidence and confirms the actual action", async () => {
    // The guarded build must produce this actual public entry. A source resolver
    // cannot substitute for this explicit dist URL.
    const compiledProducer: typeof import("../src/funding-measurements.js") =
      await import(
        /* @vite-ignore */ new URL(
          "../dist/funding-measurements.js",
          import.meta.url,
        ).href
      );

    const retainedDirectory =
      process.env.MIDGARD_FUNDING_MEASUREMENT_OUTPUT_DIR;
    if (retainedDirectory !== undefined)
      await mkdir(retainedDirectory, { recursive: true, mode: 0o700 });
    const directory = await mkdtemp(
      join(retainedDirectory ?? tmpdir(), "midgard-funding-producer-"),
    );
    try {
      const { fixture, contextInput, transaction } =
        await makeGenuineFundingInitCaptureForTest();
      const outputFile = join(directory, "init.json");
      const receipt =
        await compiledProducer.captureAndWriteComputationThreadInitFunding({
          identity: contextInput,
          lucid: fixture.proverLucid,
          transaction,
          category: "doubleSpend",
          outputFile,
        });
      await expect(
        compiledProducer.captureAndWriteComputationThreadInitFunding({
          identity: contextInput,
          lucid: fixture.proverLucid,
          transaction,
          category: "doubleSpend",
          outputFile,
        }),
      ).rejects.toMatchObject({ code: "EEXIST" });
      const bytes = await readFile(outputFile);
      expect(createHash("sha256").update(bytes).digest("hex")).toBe(
        receipt.sha256,
      );
      expect(bytes.length).toBe(receipt.bytes);
      expect((await stat(outputFile)).mode & 0o777).toBe(0o600);
      const artifact = JSON.parse(bytes.toString("utf8"));
      expect(artifact.status).toBe("partial");
      expect(artifact.transactionStatus).toBe("locally_evaluated_prepared");
      expect(artifact.action.transactionHash).toBe(transaction.txHash);
      expect(artifact.action.signedTransactionCborHex).toBe(
        transaction.signed.toCBOR(),
      );
      expect(artifact.missingCoverage).toContain("maximum-reachable-shapes");
      expect(await submitCapturedTransaction(transaction)).toBe(
        transaction.txHash,
      );
      expect(await fixture.proverLucid.awaitTx(transaction.txHash)).toBe(true);
      // Confirmation is a separate driver receipt, never a relabelled prepared
      // artifact. Optional retention keeps genuine raw evidence outside git.
      await writeFile(
        join(directory, "confirmation.json"),
        JSON.stringify({
          environment: "emulator",
          status: "confirmed",
          transactionHash: transaction.txHash,
          measurementArtifactSha256: receipt.sha256,
        }),
        { flag: "wx", mode: 0o600 },
      );
      // The retained file stays the prepared artifact after confirmation.
      expect(await readFile(outputFile)).toEqual(bytes);
    } finally {
      if (retainedDirectory === undefined)
        await rm(directory, { recursive: true, force: true });
    }
  }, 600_000);
});
