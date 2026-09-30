import {
  encodeMidgardForcedTxCanonical,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import { missingSignatureVkeyHash } from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { afterAll, describe, expect, it } from "vitest";

import type { VanRossemFitMeasurement } from "../src/proof-fit/van-rossem-fit-ledger.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { nodeForcedVerdict } from "./support/emulator/node-forced-verdict.js";
import { setupMissingSignatureForcedScenario } from "./support/missing-signature-forced-scenario.js";
import { buildMissingSignatureForcedTransaction as transactionFor } from "./support/missing-signature-forced-shapes.js";

/**
 * A forced RequiredSignerUnsigned reason names a required signer by its field
 * position, and missingSignature reopens exactly that position. The verdict
 * here is the one the node's classifier writes, so the suite fails if the
 * writer and the proof disagree on how signers are counted: one position
 * early names a signer that did sign and convicts; the written position is
 * refused on chain.
 */

/** A key the transaction requires but never carries a signature from. */
const absentKey = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 32));
afterAll(() => absentKey.free());
const absentVkey = Buffer.from(absentKey.to_public().to_raw_bytes()).toString(
  "hex",
);

const shape = {
  spendInputCbors: [
    encodeMidgardSpendInputItem({
      txId: Buffer.alloc(32, 0x5a),
      outputIndex: 0,
    }),
  ],
  // Required signers are canonically ordered; this key's hash sorts after
  // the signed one's, so the unsigned signer is position 1.
  unsignedSignerHashes: [missingSignatureVkeyHash(absentVkey)],
};

const writtenSignerIndex = async (): Promise<bigint> => {
  const transaction = transactionFor(shape);
  const verdict = await nodeForcedVerdict({
    transactionId: Buffer.from(transaction.transactionId, "hex"),
    forcedCanonicalCbor: encodeMidgardForcedTxCanonical(
      transaction.transaction,
    ),
  });
  expect(verdict).toStrictEqual({
    ForcedTxInvalid: {
      reason: { RequiredSignerUnsigned: { signer_index: 1n } },
    },
  });
  return 1n;
};

const fitMeasurements: VanRossemFitMeasurement[] = [];

describe("forced RequiredSignerUnsigned coordinate the node writes", () => {
  it("convicts a coordinate one position early, where the signer signed", async () => {
    const written = await writtenSignerIndex();
    const s = await setupMissingSignatureForcedScenario(
      fitMeasurements,
      shape,
      written - 1n,
    );
    let thread = await s.init();
    for (let i = 0; i < 3; i++) thread = await s.advance(thread, i);
    expect((await s.action(thread, 3)).kind).toBe("proven");
    await s.remove();
  }, 600_000);

  it("refuses the written coordinate on chain", async () => {
    const s = await setupMissingSignatureForcedScenario(
      fitMeasurements,
      shape,
      await writtenSignerIndex(),
    );
    // The builder refuses an honest rejection locally, so the steps before
    // the terminal one run on evidence claiming a signature the transaction
    // does not carry; none of those steps opens the address witnesses.
    const claimed = {
      ...s.prepared,
      evidence: {
        ...s.prepared.evidence,
        addrTxWits: [
          ...s.prepared.evidence.addrTxWits,
          {
            verification_key: absentVkey,
            signature: Buffer.from(
              absentKey
                .sign(Buffer.from(s.prepared.transactionId, "hex"))
                .to_raw_bytes(),
            ).toString("hex"),
          },
        ],
      },
    };
    let thread = await s.init();
    for (let i = 0; i < 3; i++) thread = await s.advance(thread, i, claimed);
    // The committed witnesses hold one signature, which is signer 0's.
    await expectOnchainRefusal(() => s.finalizeRaw(thread, 0n));
  }, 600_000);
});
