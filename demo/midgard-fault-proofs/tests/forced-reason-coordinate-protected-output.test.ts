import {
  computeMidgardNativeTxId,
  encodeCbor,
  encodeMidgardNativeTxCanonical,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { prepareProtectedOutputSignerMissingEvidence } from "../src/protected-output-signer-missing/index.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { makeScenario } from "./protected-output-signer-missing-lifecycle.make-scenario.js";
import {
  FORCED_ORDER_KEY,
  protectedOutputCbor,
  signerCredentialHex,
  validWitness,
} from "./protected-output-signer-missing-lifecycle.registered-contracts.js";
import { scanToTerminal } from "./protected-output-signer-missing-lifecycle.with-swapped-certificate-slot.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { nodeForcedVerdict } from "./support/emulator/node-forced-verdict.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import {
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

/**
 * A forced ProtectedOutputSignerMissing reason names an output by its field
 * position, and protectedOutputSignerMissing reopens exactly that output.
 * Output 0 is protected for the key that signs the transaction; output 1 is
 * protected for a key that never signs. The verdict is the one the node's
 * classifier writes, so the suite fails if the writer and the proof disagree
 * on how outputs are counted: one position early names an output whose signer
 * signed and convicts; the written position is refused on chain.
 */

/** A pub-key credential nobody signs for. */
const absentCredentialHex = "4c".repeat(28);

/** The one spend input, locked by the signing key, and its ledger output. */
const spent = encodeMidgardSpendInputItem({
  txId: Buffer.alloc(32, 0x59),
  outputIndex: 0,
});
const spentOutput = protectedOutputCbor(signerCredentialHex, 0x60);

const nativeTx = (() => {
  const body = {
    spendInputCbors: [spent],
    fee: 0n,
    outputCbors: [
      protectedOutputCbor(signerCredentialHex),
      protectedOutputCbor(absentCredentialHex),
    ],
  };
  const txId = computeMidgardNativeTxId(makeNativeTx(body));
  return makeNativeTx({
    ...body,
    addrTxWitsPreimageCbor: encodeCbor([validWitness(txId)]),
  });
})();
const adjudicated = materializeMidgardForcedTxFromCanonical(nativeTx);

const writtenOutputIndex = async (): Promise<bigint> => {
  const verdict = await nodeForcedVerdict({
    transactionId: computeMidgardNativeTxId(adjudicated),
    forcedCanonicalCbor: encodeMidgardForcedTxCanonical(adjudicated),
    ledger: [[spent, spentOutput]],
  });
  expect(verdict).toStrictEqual({
    ForcedTxInvalid: {
      reason: { ProtectedOutputSignerMissing: { output_index: 1n } },
    },
  });
  return 1n;
};

/**
 * A block committing the transaction under `ProtectedOutputSignerMissing {
 * outputIndex }`, the forced subject a thread carries for it, and removal of
 * the block once a proof token exists.
 */
const setupScenario = async (outputIndex: bigint) => {
  const reason = {
    ProtectedOutputSignerMissing: { output_index: outputIndex },
  };
  const s = await makeScenario({ nativeTx, forcedReason: reason });
  const subject = forcedVerdictSubject({
    transactionId: s.block.nativeTxId,
    sourceKey: FORCED_ORDER_KEY,
    rejectionReason: reason,
  });
  const remove = async () => {
    const references = await publishRemovalReferenceScripts({
      lucid: s.harness.proverLucid,
      contracts: s.harness.contracts,
    });
    const now = BigInt(s.harness.emulator.now());
    return submitRemoveFraudulentBlock({
      lucid: s.harness.proverLucid,
      blueprint: s.harness.realBlueprint,
      deploymentInfo: buildRemovalDeploymentInfo(
        s.harness.contracts,
        s.harness.catalogue,
        { removalReferenceScripts: references.published },
      ),
      network,
      signer: s.harness.proverSigner,
      fraudCategory: "protectedOutputSignerMissing",
      fraudulentHeaderHash: s.setup.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => ({
          token: "protected-output-coordinate-lease",
          source: "emulator",
          renew: async () => {},
          release: async () => {},
          fail: async () => {},
        }),
      },
      validFrom: now > 120_000n ? now - 120_000n : 0n,
      validTo: now + 300_000n,
    });
  };
  return { s, subject, remove };
};

describe("forced ProtectedOutputSignerMissing coordinate the node writes", () => {
  it("convicts a coordinate one position early, where the output's signer signed", async () => {
    const { s, subject, remove } = await setupScenario(
      (await writtenOutputIndex()) - 1n,
    );
    const evidence = prepareProtectedOutputSignerMissingEvidence({
      subject,
      outputIndex: 0,
      canonicalTransactionCbor: encodeMidgardForcedTxCanonical(adjudicated),
    });
    expect(evidence.signerPresent).toBe(true);
    const bound = await s.forced01(await s.init(), evidence);
    const credential = await s.step02(
      bound.nextThreadOutRef,
      evidence,
      adjudicated,
    );
    const carriage = await s.step03(
      credential.nextThreadOutRef,
      evidence,
      adjudicated,
    );
    const terminal = await scanToTerminal(
      s,
      carriage.nextThreadOutRef,
      evidence,
      adjudicated,
      carriage,
    );
    expect((await s.step05(terminal, evidence)).fraudProofUnit).toContain(
      s.category.categoryId,
    );
    expect((await remove()).fraudCategoryId).toBe(s.category.categoryId);
  }, 600_000);

  it("refuses the written coordinate on chain", async () => {
    const written = await writtenOutputIndex();
    const { s, subject } = await setupScenario(written);
    // The prover's builder refuses evidence that agrees with the verdict, so
    // the evidence is prepared as a wrongful-acceptance claim and then bound
    // to the forced leaf: the thread carries the written coordinate honestly.
    const evidence = Object.freeze({
      ...prepareProtectedOutputSignerMissingEvidence({
        subject: acceptedVerdictSubject(s.block.nativeTxId),
        outputIndex: Number(written),
        canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
      }),
      subject,
      canonicalTransactionCborHex:
        encodeMidgardForcedTxCanonical(adjudicated).toString("hex"),
    });
    expect(evidence.signerPresent).toBe(false);
    const bound = await s.forced01(await s.init(), evidence);
    const credential = await s.step02(
      bound.nextThreadOutRef,
      evidence,
      adjudicated,
    );
    const carriage = await s.step03(
      credential.nextThreadOutRef,
      evidence,
      adjudicated,
    );
    const terminal = await scanToTerminal(
      s,
      carriage.nextThreadOutRef,
      evidence,
      adjudicated,
      carriage,
    );
    // Output 1's signer is missing, as the leaf says. Step 05 authenticates
    // the thread and the terminal rule, which convicts a forced rejection
    // only when the signer is present, returns false.
    await expectOnchainRefusal(() => s.rawStep05(terminal), {
      refusedBy: "fraud_proofs/protected_output_signer_missing/step_05",
      check: /^Validator returned false$/u,
    });
  }, 600_000);
});
