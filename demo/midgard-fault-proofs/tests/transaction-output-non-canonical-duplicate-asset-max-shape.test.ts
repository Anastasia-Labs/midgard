/**
 * transactionOutputNonCanonical with the repeated key as late as it can sit.
 * The scanner reads one token per step-03 transaction, so the costliest
 * repeat is the last asset name of an output filled with assets.
 *
 * Accepted: the output is exactly the family's 16,384-byte bound, holding as
 * many assets as fit, its last name repeating the one before it. The block is
 * convicted from Init and removed; the twin whose last name is the next one
 * instead, the same length and one byte apart, is refused at step 04 in the
 * same block.
 *
 * Forced: the L1 admission screen bounds an output's value at 5,000 bytes, so
 * the forced maximum is that value filled with assets the same way. The node's
 * `OutputNonCanonical` rejection is refused at step 04, and a leaf accepting
 * it is convicted and removed.
 */
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core";
import { acceptedVerdictSubject } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { maximumAssetOutput } from "./support/duplicate-asset-output.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import {
  forcedOutputTransaction,
  runAcceptedBlock,
  runForcedLeaf,
} from "./transaction-output-non-canonical-duplicate-asset.scenario.js";
import {
  evidenceOf,
  MAXIMUM_OUTPUT_BYTES,
} from "./transaction-output-non-canonical-lifecycle.registered-contracts.js";

/** `{0: address, 1: value}` around the value: map head, two keys, address. */
const OUTPUT_BYTES_AROUND_VALUE = 34;
const FORCED_MAXIMUM_OUTPUT_BYTES =
  MIDGARD_CONSENSUS_LIMITS.maxOutputValueCborBytes + OUTPUT_BYTES_AROUND_VALUE;

const repeatedAt = (targetBytes: number) =>
  maximumAssetOutput({ targetBytes, duplicateLast: true });
const twinAt = (targetBytes: number) =>
  maximumAssetOutput({ targetBytes, duplicateLast: false });

const scanStepsOf = (output: Buffer) =>
  evidenceOf(
    acceptedVerdictSubject("00".repeat(32)),
    makeNativeTx({ spendInputCbors: [], fee: 1n, outputCbors: [output] }),
    0,
  );

const report = (
  label: string,
  log: Awaited<ReturnType<typeof runForcedLeaf>>,
) =>
  console.info(
    `${label}: ${log.transactions.toString()} transactions, ${log.scans.toString()} step-03 scans; peak ${log.peakMemory.toString()} memory, ${log.peakSteps.toString()} CPU, ${log.peakBytes.toString()} signed bytes`,
  );

describe("transactionOutputNonCanonical repeated key at the maximum shape", () => {
  it("fills the family bound and the forced value bound with assets, the repeat at the very last name", () => {
    for (const targetBytes of [
      MAXIMUM_OUTPUT_BYTES,
      FORCED_MAXIMUM_OUTPUT_BYTES,
    ]) {
      const repeated = repeatedAt(targetBytes);
      const twin = twinAt(targetBytes);
      expect(repeated).toHaveLength(targetBytes);
      expect(twin).toHaveLength(targetBytes);
      const differing = [...repeated.keys()].filter(
        (index) => repeated[index] !== twin[index],
      );
      // One byte apart: the last asset's one-byte name, then its quantity.
      expect(differing).toStrictEqual([targetBytes - 2]);
      const repeatedEvidence = scanStepsOf(repeated);
      const twinEvidence = scanStepsOf(twin);
      expect(repeatedEvidence.canonical).toBe(false);
      expect(twinEvidence.canonical).toBe(true);
      // The repeat is found on the scan of the last asset, the twin's terminal
      // two scans later: past that asset and across the empty optional tail.
      expect(repeatedEvidence.scanControls.length).toBe(
        twinEvidence.scanControls.length - 2,
      );
    }
    // The forced shape sits exactly at the admission bound.
    expect(
      forcedOutputTransaction(
        repeatedAt(FORCED_MAXIMUM_OUTPUT_BYTES),
        2_000_000n,
      ).admission,
    ).toBeNull();
    expect(
      forcedOutputTransaction(
        repeatedAt(FORCED_MAXIMUM_OUTPUT_BYTES + 1),
        2_000_000n,
      ).admission,
    ).toStrictEqual({
      check: "outputs_are_admissible(outputs)",
      detail: `item 0: value is ${(MIDGARD_CONSENSUS_LIMITS.maxOutputValueCborBytes + 1).toString()} bytes`,
    });
  });

  it("convicts an accepted output repeating its last asset name at the family maximum, refuses the twin, then removes the block", async () => {
    const log = await runAcceptedBlock([
      {
        label: "repeated last name",
        output: repeatedAt(MAXIMUM_OUTPUT_BYTES),
        repeated: true,
      },
      {
        label: "distinct last name",
        output: twinAt(MAXIMUM_OUTPUT_BYTES),
        repeated: false,
      },
    ]);
    report("accepted maximum", log);
  }, 3_600_000);

  it("admits the forced maximum; the node's rejection is refused at step 04 and a leaf accepting it is convicted and removed", async () => {
    const forced = forcedOutputTransaction(
      repeatedAt(FORCED_MAXIMUM_OUTPUT_BYTES),
      2_000_000n,
    );
    expect(forced.admission).toBeNull();
    expect(await forced.verdict()).toStrictEqual({
      ForcedTxInvalid: {
        reason: { OutputNonCanonical: { output_index: 0n } },
      },
    });
    report(
      "forced maximum rejection",
      await runForcedLeaf({ transaction: forced.transaction, accepted: false }),
    );
    report(
      "forced maximum acceptance",
      await runForcedLeaf({ transaction: forced.transaction, accepted: true }),
    );
  }, 3_600_000);
});
