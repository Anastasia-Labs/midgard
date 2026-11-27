import { materializeMidgardForcedTxFromCanonical } from "@al-ft/midgard-core/codec/forced";
import { forcedVerdictSubject } from "@al-ft/midgard-sdk/fraud-proof";
import { describe, expect, it } from "vitest";

import { SIGNER_WITNESS_SCAN_BATCH } from "../src/signer-witness-scan/index.js";
import { makeScenario } from "./protected-output-signer-missing-lifecycle.make-scenario.js";
import {
  decoyWitness,
  evidenceFor,
  FORCED_ORDER_KEY,
  nativeTxWith,
  validWitness,
} from "./protected-output-signer-missing-lifecycle.registered-contracts.js";
import { scanToTerminal } from "./protected-output-signer-missing-lifecycle.with-swapped-certificate-slot.js";

describe("signer-witness scan early exit", () => {
  it.each([
    { position: "first", signerAt: 0, turns: 1 },
    { position: "second", signerAt: SIGNER_WITNESS_SCAN_BATCH + 7, turns: 2 },
  ])(
    "ends the scan at the batch where the signer is found ($position batch of three)",
    async ({ signerAt, turns }) => {
      // A wrongful forced rejection over a field of three batches: the scan
      // writes the verdict at the end of the batch holding the valid signer,
      // before the field is exhausted, so a signer in the first batch takes
      // one scan transaction and a signer in the second takes two.
      const witnessCount = 2 * SIGNER_WITNESS_SCAN_BATCH + 8;
      const nativeTx = nativeTxWith({
        fee: 37n,
        witnesses: (txId) =>
          Array.from({ length: witnessCount }, (_unused, index) =>
            index === signerAt ? validWitness(txId) : decoyWitness(index),
          ),
      });
      const forcedReason = {
        ProtectedOutputSignerMissing: { output_index: 0n },
      } as const;
      const s = await makeScenario({ nativeTx, forcedReason });
      const adjudicated = materializeMidgardForcedTxFromCanonical(nativeTx);
      const evidence = evidenceFor(
        forcedVerdictSubject({
          transactionId: s.block.nativeTxId,
          sourceKey: FORCED_ORDER_KEY,
          rejectionReason: forcedReason,
        }),
        adjudicated,
      );
      expect(evidence.signerPresent).toBe(true);
      const bound = await s.forced01(await s.init(), evidence);
      const credential = await s.step02(
        bound.nextThreadOutRef,
        evidence,
        adjudicated,
      );
      const terminal = await scanToTerminal(
        s,
        credential.nextThreadOutRef,
        evidence,
        adjudicated,
      );
      expect(terminal.turns).toBe(turns);
      const minted = await s.terminal(terminal.threadOutRef, evidence);
      expect(minted.fraudProofUnit).toContain(s.category.categoryId);
    },
    900_000,
  );
});
