import {
  computeMidgardNativeTxId,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { forcedVerdictSubject } from "@al-ft/midgard-sdk";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  submitTransactionOutputNonCanonicalStep01Forced,
  submitTransactionOutputNonCanonicalStep02,
  submitTransactionOutputNonCanonicalStep03,
  submitTransactionOutputNonCanonicalStep04,
} from "../src/transaction-output-non-canonical/index.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { nodeForcedVerdict } from "./support/emulator/node-forced-verdict.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";
import {
  buildForcedOutputFixture,
  MALFORMED_OUTPUT,
  submitOutputStep04Raw,
} from "./support/transaction-output-non-canonical-emulator.js";
import {
  evidenceOf,
  registeredContracts,
} from "./transaction-output-non-canonical-lifecycle.registered-contracts.js";

/**
 * A forced OutputNonCanonical reason names an output by its field position,
 * and transactionOutputNonCanonical reopens exactly that output item. The
 * fixture puts a canonical output before the malformed one. The verdict is
 * the one the node's classifier writes, so the suite fails if the writer and
 * the proof disagree on how outputs are counted: one position early names the
 * canonical output and convicts; the written position is refused on chain.
 */

const canonicalOutput = encodeMidgardTxOutput({
  address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x47)]),
  value: { lovelace: 2_000_000n, assets: new Map() },
});

/** A canonical output, then a malformed one; no other field is filled. */
const transaction = makeNativeTx({
  spendInputCbors: [],
  fee: 0n,
  referenceByte: "b1",
  outputCbors: [canonicalOutput, MALFORMED_OUTPUT],
});

const writtenOutputIndex = async (): Promise<bigint> => {
  const forced = materializeMidgardForcedTxFromCanonical(transaction);
  const verdict = await nodeForcedVerdict({
    transactionId: computeMidgardNativeTxId(forced),
    forcedCanonicalCbor: encodeMidgardForcedTxCanonical(forced),
  });
  expect(verdict).toStrictEqual({
    ForcedTxInvalid: { reason: { OutputNonCanonical: { output_index: 1n } } },
  });
  return 1n;
};

/**
 * A block whose forced leaf rejects the two-output transaction with
 * `OutputNonCanonical { written + offset }`, with a thread bound to that
 * output and its field opened.
 */
const setupScenario = async (offset: bigint) => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realTransactionOutputNonCanonical: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const registered = await registeredContracts(harness);
  const { contracts, category, references } = registered;
  const funderCredential = getAddressDetails(
    await harness.funderLucid.wallet().address(),
  ).paymentCredential;
  if (funderCredential?.type !== "Key")
    throw new Error("forced fixture funder key absent");
  const outputIndex = (await writtenOutputIndex()) + offset;
  const forced = await buildForcedOutputFixture({
    operatorVkey: funderCredential.hash,
    now:
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
    transaction,
    outputIndex,
  });
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: registered.catalogue,
    header: forced.header,
  });
  await registered.publishReferences();
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: forced.reconstruction,
    eventKey: forced.eventKey,
  });
  const evidence = evidenceOf(
    forcedVerdictSubject({
      transactionId: forced.transaction.tx_id,
      sourceKey: membership.key,
      rejectionReason: forced.rejectionReason,
    }),
    forced.nativeTx,
    Number(outputIndex),
  );
  const source = {
    nativeTxCompactCbor: forced.transaction.submitted_source.compact_cbor,
    witnessSetCompactCbor:
      forced.transaction.submitted_source.witness_set_compact_cbor,
  };
  const step = {
    lucid: harness.proverLucid,
    contracts,
    categoryId: category.categoryId,
    signer: harness.proverSigner,
  };
  const init = await registered.init(setup.fraudulentBlockOutRef);
  const bound = await submitTransactionOutputNonCanonicalStep01Forced({
    ...step,
    threadOutRef: `${init.result.txHash}#${init.result.firstStepOutputIndex.toString()}`,
    finding: evidence,
    forcedSource: { header: forced.header, membership, direction: 1n },
    referenceScriptUtxo: references[0]!,
  });
  const authenticated = await submitTransactionOutputNonCanonicalStep02({
    ...step,
    ...source,
    threadOutRef: bound.nextThreadOutRef,
    evidence,
    referenceScriptUtxo: references[1]!,
  });
  /** Step 03 run to its terminal checkpoint. */
  const scanToTerminal = async (): Promise<string> => {
    let threadOutRef = authenticated.nextThreadOutRef;
    for (;;) {
      const scanned = await submitTransactionOutputNonCanonicalStep03({
        ...step,
        ...source,
        threadOutRef,
        evidence,
        referenceScriptUtxo: references[2]!,
      });
      threadOutRef = scanned.nextThreadOutRef;
      if (scanned.terminal) return threadOutRef;
    }
  };
  return {
    harness,
    registered,
    setup,
    evidence,
    step,
    references,
    scanToTerminal,
  };
};

describe("forced OutputNonCanonical coordinate the node writes", () => {
  it("convicts a coordinate one position early, where the output is canonical", async () => {
    const s = await setupScenario(-1n);
    expect(s.evidence.canonical).toBe(true);
    const minted = await submitTransactionOutputNonCanonicalStep04({
      ...s.step,
      threadOutRef: await s.scanToTerminal(),
      evidence: s.evidence,
      referenceScriptUtxo: s.references[3]!,
      witnessReferenceScripts: s.harness.witnessReferenceScripts,
    });
    expect(minted.fraudProofUnit).toBeTruthy();
    await s.registered.removal(s.setup.headerHash);
    const [txHash, outputIndex] = s.setup.fraudulentBlockOutRef.split("#");
    expect(
      await s.harness.proverLucid.utxosByOutRef([
        { txHash: txHash!, outputIndex: Number(outputIndex) },
      ]),
    ).toHaveLength(0);
  }, 900_000);

  it("refuses the written coordinate on chain", async () => {
    const s = await setupScenario(0n);
    // The written output is the malformed one: the rejection holds.
    expect(s.evidence.canonical).toBe(false);
    expect(s.evidence.decisiveFaultHolds).toBe(true);
    const terminal = await s.scanToTerminal();
    await expect(
      submitTransactionOutputNonCanonicalStep04({
        ...s.step,
        threadOutRef: terminal,
        evidence: s.evidence,
        referenceScriptUtxo: s.references[3]!,
        witnessReferenceScripts: s.harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/does not contradict/u);
    // The step authenticates the non-canonical terminal and the rule, which
    // convicts only an outcome the reason contradicts, returns false.
    await expectOnchainRefusal(
      async () =>
        await submitOutputStep04Raw({
          ...s.registered.common(terminal, 3),
          witnessReferenceScripts: s.harness.witnessReferenceScripts,
        }),
      /^Validator returned false$/u,
    );
  }, 900_000);
});
