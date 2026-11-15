import {
  computeMidgardNativeTxId,
  encodeCbor,
  encodeMidgardFieldPreimage,
  encodeMidgardNativeTxCanonical,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  MIDGARD_CONSENSUS_LIMITS,
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

import {
  VAN_ROSSEM_MAX_CPU_UNITS,
  VAN_ROSSEM_MAX_MEMORY_UNITS,
} from "../src/proof-fit/van-rossem.js";
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
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { nodeForcedVerdict } from "./support/emulator/node-forced-verdict.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { deployHarnessFieldGrammarCertificate } from "./support/field-grammar-certificate-deployment-fixture.js";
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
const setupScenario = async (outputIndex: bigint, forcedTx = nativeTx) => {
  const reason = {
    ProtectedOutputSignerMissing: { output_index: outputIndex },
  };
  const s = await makeScenario({ nativeTx: forcedTx, forcedReason: reason });
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

/** Protected outputs, the first padded so field 2 is exactly at its cap. */
const MAXIMUM_OUTPUT_COUNT = 700;
const maximumOutputs = (): Buffer[] => {
  const rest = Array.from({ length: MAXIMUM_OUTPUT_COUNT - 1 }, () =>
    protectedOutputCbor(signerCredentialHex),
  );
  let pad = 0;
  const first = () =>
    encodeMidgardTxOutput({
      address: Buffer.concat([
        Buffer.from([0x68]),
        Buffer.from(absentCredentialHex, "hex"),
      ]),
      value: { lovelace: 2_000_000n, assets: new Map() },
      script_ref: { language: "PlutusV3", scriptBytes: Buffer.alloc(pad) },
    });
  while (
    encodeMidgardFieldPreimage([first(), ...rest]).length <
    MIDGARD_CONSENSUS_LIMITS.maxOutputsPreimageBytes
  )
    pad++;
  const outputs = [first(), ...rest];
  expect(encodeMidgardFieldPreimage(outputs).length).toBe(
    MIDGARD_CONSENSUS_LIMITS.maxOutputsPreimageBytes,
  );
  return outputs;
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

  it("convicts a negative coordinate, which names no output", async () => {
    const { s, subject, remove } = await setupScenario(-1n);
    const evidence = prepareProtectedOutputSignerMissingEvidence({
      subject,
      outputIndex: -1,
      canonicalTransactionCbor: encodeMidgardForcedTxCanonical(adjudicated),
    });
    expect(evidence.route).toBe("coordinate_out_of_range");
    const bound = await s.forced01(await s.init(), evidence);
    const direct = await s.step02(
      bound.nextThreadOutRef,
      evidence,
      adjudicated,
    );
    expect(direct.stage).toBe("step05");
    expect(
      (await s.step05(direct.nextThreadOutRef, evidence)).fraudProofUnit,
    ).toContain(s.category.categoryId);
    expect((await remove()).fraudCategoryId).toBe(s.category.categoryId);
  }, 600_000);

  it("refuses on chain a claim that a held output is absent", async () => {
    const written = await writtenOutputIndex();
    const { s, subject } = await setupScenario(written);
    const held = Object.freeze({
      ...prepareProtectedOutputSignerMissingEvidence({
        subject: acceptedVerdictSubject(s.block.nativeTxId),
        outputIndex: Number(written),
        canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
      }),
      subject,
      canonicalTransactionCborHex:
        encodeMidgardForcedTxCanonical(adjudicated).toString("hex"),
    });
    const bound = await s.forced01(await s.init(), held);
    await expectOnchainRefusal(
      () =>
        s.step02(
          bound.nextThreadOutRef,
          Object.freeze({
            ...held,
            route: "coordinate_out_of_range" as const,
            signerRequired: false,
          }),
          adjudicated,
        ),
      {
        refusedBy: "fraud_proofs/protected_output_signer_missing/step_02",
        check: /^Validator returned false$/u,
      },
    );
  }, 600_000);

  it("convicts a negative coordinate over field 2 at its byte cap", async () => {
    const outputs = maximumOutputs();
    expect(outputs).toHaveLength(MAXIMUM_OUTPUT_COUNT);
    const maximum = makeNativeTx({
      spendInputCbors: [spent],
      fee: 0n,
      outputCbors: outputs,
    });
    const forced = materializeMidgardForcedTxFromCanonical(maximum);
    const { s, subject, remove } = await setupScenario(-1n, maximum);
    const evidence = prepareProtectedOutputSignerMissingEvidence({
      subject,
      outputIndex: -1,
      canonicalTransactionCbor: encodeMidgardForcedTxCanonical(forced),
    });
    expect(evidence.outputCarriage).toBe("Certified");
    const grammar = await deployHarnessFieldGrammarCertificate(
      s.harness,
      "protected-output negative coordinate",
    );
    const initial = await s.init();
    const bound = await captureEmulatorSubmission(s.harness.emulator, () =>
      s.forced01(initial, evidence),
    );
    const direct = await captureEmulatorSubmission(s.harness.emulator, () =>
      s.step02(
        bound.result.nextThreadOutRef,
        evidence,
        forced,
        grammar.fieldGrammar,
      ),
    );
    expect(direct.result.stage).toBe("step05");
    const minted = await captureEmulatorSubmission(s.harness.emulator, () =>
      s.step05(direct.result.nextThreadOutRef, evidence),
    );
    for (const { measurement } of [bound, direct, minted]) {
      expect(measurement.executionMemory).toBeLessThanOrEqual(
        VAN_ROSSEM_MAX_MEMORY_UNITS,
      );
      expect(measurement.executionSteps).toBeLessThanOrEqual(
        VAN_ROSSEM_MAX_CPU_UNITS,
      );
    }
    console.info(
      `max-shape negative output coordinate (${MAXIMUM_OUTPUT_COUNT.toString()} outputs, ${encodeMidgardFieldPreimage(outputs).length.toString()} bytes): step-01 mem=${bound.measurement.executionMemory.toString()} cpu=${bound.measurement.executionSteps.toString()}; step-02 mem=${direct.measurement.executionMemory.toString()} cpu=${direct.measurement.executionSteps.toString()}; step-05 mem=${minted.measurement.executionMemory.toString()} cpu=${minted.measurement.executionSteps.toString()}`,
    );
    expect((await remove()).fraudCategoryId).toBe(s.category.categoryId);
  }, 1_800_000);
});
