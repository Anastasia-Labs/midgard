import { sign } from "node:crypto";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  encodeCbor,
  encodeMidgardAddressWitnessItem,
  encodeMidgardNativeTxCanonical,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { acceptedVerdictSubject, Proof } from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterial } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { SPEND_INPUT_SIGNER_MISSING_ID } from "../src/spend-input-signer-missing/index.js";
import {
  commitForcedBlock,
  publishReferences,
} from "./spend-input-signer-missing-lifecycle.commit-forced-block.js";
import { familyDriver } from "./spend-input-signer-missing-lifecycle.family-driver.js";
import {
  ed25519Keypair,
  FAMILY,
  newHarness,
  prepareSpendInputSignerMissingEvidence,
  registeredContracts,
} from "./spend-input-signer-missing-lifecycle.registered-contracts.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { nodeForcedVerdict } from "./support/emulator/node-forced-verdict.js";

/**
 * A forced SpendInputSignerMissing reason names a spend input by its field
 * position, and spendInputSignerMissing reopens exactly that input. Spend
 * input 0 is locked by the key that signs the transaction; spend input 1 by a
 * key that never signs. The verdict is the one the node's classifier writes
 * over that ledger, so the suite fails if the writer and the proof disagree
 * on how spend inputs are counted: one position early names an input whose
 * signer signed and convicts; the written position is refused on chain.
 */

const signerKey = ed25519Keypair(21);
const absentKey = ed25519Keypair(22);
const priorTxId = Buffer.alloc(32, 0x58);

/** Spend input `i` and the pub-key output it resolves to in the ledger. */
const entries = [signerKey, absentKey].map((key, outputIndex) => ({
  outRefBytes: encodeMidgardSpendInputItem({ txId: priorTxId, outputIndex }),
  outputIndex,
  outputCbor: encodeMidgardTxOutput({
    address: Buffer.concat([
      Buffer.from([0x60]),
      Buffer.from(key.keyHash, "hex"),
    ]),
    value: { lovelace: 2_000_000n, assets: new Map() },
  }),
}));

const nativeTx = (() => {
  const spendInputCbors = entries.map((entry) => entry.outRefBytes);
  const txId = computeMidgardNativeTxId(
    makeNativeTx({ spendInputCbors, fee: 7n }),
  );
  return makeNativeTx({
    spendInputCbors,
    fee: 7n,
    addrTxWitsPreimageCbor: encodeCbor([
      encodeMidgardAddressWitnessItem({
        verificationKey: signerKey.verificationKey,
        signature: sign(null, txId, signerKey.privateKey),
      }),
    ]),
  });
})();

const writtenInputIndex = async (): Promise<bigint> => {
  const forced = materializeMidgardForcedTxFromCanonical(nativeTx);
  const verdict = await nodeForcedVerdict({
    transactionId: computeMidgardNativeTxId(forced),
    forcedCanonicalCbor: encodeMidgardForcedTxCanonical(forced),
    ledger: entries.map((entry) => [entry.outRefBytes, entry.outputCbor]),
  });
  expect(verdict).toStrictEqual({
    ForcedTxInvalid: {
      reason: { SpendInputSignerMissing: { input_index: 1n } },
    },
  });
  return 1n;
};

/** The prior ledger holding both spend inputs, with each one's opening. */
const priorLedger = async () => {
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  const descriptors = entries.map(
    (entry) =>
      buildCanonicalMidgardLedgerOutputMaterial({
        outputIndex: entry.outputIndex,
        outputCbor: entry.outputCbor,
      }).descriptorCbor,
  );
  for (const [index, entry] of entries.entries())
    await trie.insert(entry.outRefBytes, descriptors[index]!);
  const priorRoot = Buffer.from(trie.hash).toString("hex");
  const resolved = async (index: number) => {
    const entry = entries[index]!;
    const proof = await trie.prove(entry.outRefBytes);
    return {
      priorRoot,
      transactionId: priorTxId.toString("hex"),
      outputIndex: entry.outputIndex,
      descriptorCborHex: descriptors[index]!.toString("hex"),
      outputCborHex: entry.outputCbor.toString("hex"),
      membershipProofCborHex: proof.toCBOR().toString("hex"),
      membershipProof: Data.from(proof.toCBOR().toString("hex"), Proof),
    };
  };
  return { priorRoot, resolved };
};

/**
 * A block committing the transaction under `SpendInputSignerMissing {
 * inputIndex }`, and the family driver over it.
 */
const setupScenario = async (inputIndex: bigint) => {
  const harness = await newHarness();
  const family = await registeredContracts(harness);
  const prior = await priorLedger();
  const block = await commitForcedBlock(
    harness,
    family,
    nativeTx,
    prior.priorRoot,
    { SpendInputSignerMissing: { input_index: inputIndex } },
    "e3",
  );
  const { references, certificateReference } = await publishReferences(
    harness,
    family,
    `${FAMILY}-coordinate`,
    false,
  );
  const run = familyDriver(harness, family, references, certificateReference);
  const resolved = await prior.resolved(Number(inputIndex));
  return { block, run, resolved };
};

describe("forced SpendInputSignerMissing coordinate the node writes", () => {
  it("convicts a coordinate one position early, where the input's signer signed", async () => {
    const { block, run, resolved } = await setupScenario(
      (await writtenInputIndex()) - 1n,
    );
    const evidence = prepareSpendInputSignerMissingEvidence({
      subject: block.subject,
      inputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
      resolved,
    });
    expect(evidence.signerMissing).toBe(false);
    const thread = await run.initThread(block.blockOutRef);
    const step01 = await run.step01Forced(
      thread.threadOutRef,
      evidence,
      block.forcedSource,
    );
    const step02 = await run.step02(
      step01.nextThreadOutRef,
      evidence,
      block.compactCbor,
      block.witnessSetCompactCbor,
    );
    const step03 = await run.step03(
      step02.nextThreadOutRef,
      evidence,
      block.compactCbor,
      block.witnessSetCompactCbor,
    );
    const step04 = await run.step04(
      step03.nextThreadOutRef,
      evidence,
      block.compactCbor,
      block.witnessSetCompactCbor,
    );
    expect(step04.stage).toBe("step05");
    expect(
      (await run.step05(step04.nextThreadOutRef, evidence)).fraudProofUnit,
    ).toBeTruthy();
    const removal = await run.removal(block.headerHash);
    expect(removal.result.fraudCategoryId).toBe(SPEND_INPUT_SIGNER_MISSING_ID);
  }, 600_000);

  it("refuses the written coordinate on chain", async () => {
    const written = await writtenInputIndex();
    const { block, run, resolved } = await setupScenario(written);
    // The prover's builder refuses evidence that agrees with the verdict, so
    // the evidence is prepared as a wrongful-acceptance claim and then bound
    // to the forced leaf: the thread carries the written coordinate honestly.
    const contradicting = prepareSpendInputSignerMissingEvidence({
      subject: acceptedVerdictSubject(block.nativeTxId),
      inputIndex: Number(written),
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
      resolved,
    });
    expect(contradicting.signerMissing).toBe(true);
    const honest = {
      ...contradicting,
      subject: block.subject,
      canonicalTransactionCborHex: encodeMidgardForcedTxCanonical(
        materializeMidgardForcedTxFromCanonical(nativeTx),
      ).toString("hex"),
    };
    const thread = await run.initThread(block.blockOutRef);
    const step01 = await run.step01Forced(
      thread.threadOutRef,
      honest,
      block.forcedSource,
    );
    const step02 = await run.step02(
      step01.nextThreadOutRef,
      honest,
      block.compactCbor,
      block.witnessSetCompactCbor,
    );
    const step03 = await run.step03(
      step02.nextThreadOutRef,
      honest,
      block.compactCbor,
      block.witnessSetCompactCbor,
    );
    const step04 = await run.step04(
      step03.nextThreadOutRef,
      honest,
      block.compactCbor,
      block.witnessSetCompactCbor,
    );
    expect(step04.stage).toBe("step05");
    // Input 1's signer is missing, as the leaf says. Step 05 authenticates
    // the thread and the terminal rule, which convicts a forced rejection
    // only when the signer is present, returns false.
    await expectOnchainRefusal(
      () => run.finalizeDirect(step04.nextThreadOutRef),
      /^Validator returned false$/u,
    );
  }, 600_000);
});
