import "node:crypto";
import "node:fs/promises";
import "node:url";
import "@aiken-lang/merkle-patricia-forestry";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec/forced";
import "@al-ft/midgard-sdk";
import "@al-ft/midgard-validation";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/committed-field-shape/submit-committed-field-shape-init.js";
import "../src/field-opening.js";
import "../src/linear-fault-family.js";
import "../src/linear-fault-finalize.js";
import "../src/linear-fault-submit.js";
import "../src/proof-fit/van-rossem-fit-ledger.js";
import "../src/remove-fraudulent-block.js";
import "../src/spend-input-signer-missing/field-plans.js";
import "../src/spend-input-signer-missing/index.js";
import "../src/spend-input-signer-missing/schemas.js";
import "../src/step-support.js";
import "../src/testing/complete-lifecycle.js";
import "../src/transition-trace/witnesses.js";
import "../src/tx-layout.js";
import "./support/emulator/blueprints.js";
import "./support/emulator/harness.js";
import "./support/emulator/measurement.js";
import "./support/emulator/native-tx.js";
import "./support/emulator/reference-scripts.js";
import "./support/emulator/registered-chain.js";
import "./support/emulator/removal-deployment.js";
import "./support/lifecycle-coverage.js";
import "./support/native-script-decoding-emulator.js";
import "./support/submit-init-emulator-fixtures.js";
import "./support/submit-init-emulator-shared.js";
import "./spend-input-signer-missing-lifecycle.registered-contracts.js";
import "./spend-input-signer-missing-lifecycle.commit-forced-block.js";
import "./spend-input-signer-missing-lifecycle.family-driver.js";
import "./spend-input-signer-missing-lifecycle.submit-step03-with-foreign-certificate.js";

import { createHash, sign } from "node:crypto";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";

import {
  computeMidgardNativeTxId,
  encodeCbor,
  encodeMidgardAddressWitnessItem,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
} from "@al-ft/midgard-core";
import { encodeMidgardForcedTxCanonical } from "@al-ft/midgard-core/codec/forced";
import { materializeMidgardForcedTxFromCanonical } from "@al-ft/midgard-core/codec/forced";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import {
  classifySpendInputSignerMissingFinding,
  SPEND_INPUT_SIGNER_MISSING_ID,
  type SpendInputSignerMissingEvidence,
} from "../src/spend-input-signer-missing/index.js";
import { assertCompleteLifecycleCoverage } from "../src/testing/complete-lifecycle.js";
import {
  commitAcceptedBlock,
  commitForcedBlock,
  publishReferences,
} from "./spend-input-signer-missing-lifecycle.commit-forced-block.js";
import { familyDriver } from "./spend-input-signer-missing-lifecycle.family-driver.js";
import {
  AUTHENTICATION_SEAMS,
  coverage,
  ed25519Keypair,
  expectRefusedOnChain,
  FAMILY,
  garbageWitness,
  HONEST_ACCEPTED_WITNESSES,
  MAXIMUM_SHAPE,
  MAXIMUM_WITNESSES,
  measurements,
  newHarness,
  PHYSICAL_STEPS,
  prepareSpendInputSignerMissingEvidence,
  priorLedgerFor,
  REASON,
  recordMeasurements,
  registeredContracts,
  signedNativeTx,
  witnessSetCompactHex,
} from "./spend-input-signer-missing-lifecycle.registered-contracts.js";
import { submitStep03WithForeignCertificate } from "./spend-input-signer-missing-lifecycle.submit-step03-with-foreign-certificate.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { transitionTraceOutRef } from "./support/submit-init-emulator-shared.js";

describe("spendInputSignerMissing registered-chain lifecycle", () => {
  it("runs the accepted 318-witness maximum from Init through proof mint, cancelling every physical step", async () => {
    const harness = await newHarness();
    const family = await registeredContracts(harness);
    const paymentCredential = Buffer.alloc(28, 0x51).toString("hex");
    const prior = await priorLedgerFor(paymentCredential, "ab".repeat(32));
    const nativeTx = makeNativeTx({
      spendInputCbors: [prior.outRefBytes],
      fee: 7n,
      addrTxWitsPreimageCbor: encodeCbor(
        Array.from({ length: MAXIMUM_WITNESSES }, (_unused, index) =>
          garbageWitness(index),
        ),
      ),
    });
    const block = await commitAcceptedBlock(
      harness,
      family,
      nativeTx,
      prior.priorRoot,
    );
    const evidence = prepareSpendInputSignerMissingEvidence({
      subject: acceptedVerdictSubject(block.nativeTxId),
      inputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
      resolved: prior.resolved,
    });
    expect(evidence.witnessCarriage).toBe("Certified");
    expect(evidence.checkpoints).toHaveLength(20);
    coverage.scenario("maximum_supported_evidence");

    const { references, certificateReference } = await publishReferences(
      harness,
      family,
      FAMILY,
      true,
    );
    const run = familyDriver(harness, family, references, certificateReference);
    const measured = async <T>(
      name: string,
      operation: () => Promise<T>,
    ): Promise<T> => {
      const captured = await captureEmulatorSubmission(
        harness.emulator,
        operation,
      );
      recordMeasurements(name, "lifecycle", MAXIMUM_SHAPE, captured);
      return captured.result;
    };

    const thread = await measured("accepted-init", () =>
      run.initThread(block.blockOutRef),
    );
    const step01 = await measured("accepted-step01", () =>
      run.step01Accepted(
        thread,
        evidence,
        block.blockOutRef,
        block.txInclusion,
      ),
    );
    const step02 = await measured("accepted-step02", () =>
      run.step02(
        step01.nextThreadOutRef,
        evidence,
        block.compactCbor,
        block.witnessSetCompactCbor,
      ),
    );
    const step03 = await measured("accepted-step03", () =>
      run.step03(
        step02.nextThreadOutRef,
        evidence,
        block.compactCbor,
        block.witnessSetCompactCbor,
      ),
    );
    const { scans, threadOutRef } = await run.scanToTerminal(
      step03.nextThreadOutRef,
      evidence,
      block.compactCbor,
      block.witnessSetCompactCbor,
    );
    expect(scans).toHaveLength(20);
    scans.forEach((scan, index) =>
      recordMeasurements(
        `accepted-step04-${index.toString().padStart(2, "0")}`,
        "lifecycle",
        MAXIMUM_SHAPE,
        scan,
      ),
    );
    // Nineteen resumptions from nothing but the checkpoint digest the previous
    // transaction committed and the redeemer's re-supplied checkpoint bytes.
    coverage.resumed();
    const step05 = await measured("accepted-step05-proof-mint", () =>
      run.step05(threadOutRef, evidence),
    );
    expect(step05.fraudProofUnit).toBeTruthy();
    coverage.reason(REASON, "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");

    // Cancel from every physical step, including the resumed scan position,
    // on fresh threads over the same block. The chunks the maximum run
    // published above are reused, so only the cancels themselves are measured.
    for (const cancelTarget of [
      "step01",
      "step02",
      "step03",
      "step04-initial",
      "step04-resumed",
      "step05",
    ] as const) {
      const thread = await run.initThread(block.blockOutRef);
      let outRef = thread.threadOutRef;
      if (cancelTarget !== "step01") {
        outRef = (
          await run.step01Accepted(
            thread,
            evidence,
            block.blockOutRef,
            block.txInclusion,
          )
        ).nextThreadOutRef;
      }
      if (cancelTarget !== "step01" && cancelTarget !== "step02") {
        outRef = (
          await run.step02(
            outRef,
            evidence,
            block.compactCbor,
            block.witnessSetCompactCbor,
          )
        ).nextThreadOutRef;
      }
      if (
        cancelTarget === "step04-initial" ||
        cancelTarget === "step04-resumed" ||
        cancelTarget === "step05"
      ) {
        outRef = (
          await run.step03(
            outRef,
            evidence,
            block.compactCbor,
            block.witnessSetCompactCbor,
          )
        ).nextThreadOutRef;
      }
      if (cancelTarget === "step04-resumed" || cancelTarget === "step05") {
        for (;;) {
          const scan = await run.step04(
            outRef,
            evidence,
            block.compactCbor,
            block.witnessSetCompactCbor,
          );
          outRef = scan.nextThreadOutRef;
          if (cancelTarget === "step04-resumed" || scan.stage === "step05")
            break;
        }
      }
      const referenceIndex =
        cancelTarget === "step01"
          ? 0
          : cancelTarget === "step02"
            ? 1
            : cancelTarget === "step03"
              ? 2
              : cancelTarget === "step05"
                ? 4
                : 3;
      await measured(`cancel-${cancelTarget}`, () =>
        run.cancel(outRef, referenceIndex),
      );
      coverage.cancelled(`step-0${(referenceIndex + 1).toString()}`);
    }
    const removal = await run.removal(block.headerHash);
    expect(removal.result.fraudCategoryId).toBe(SPEND_INPUT_SIGNER_MISSING_ID);
    recordMeasurements("accepted-remove", "lifecycle", MAXIMUM_SHAPE, removal);
    coverage.scenario("permanent_proof_token_and_descendant_removal");
  }, 900_000);

  it("runs a forced wrongful rejection with a valid matching signature through removal", async () => {
    const harness = await newHarness();
    const family = await registeredContracts(harness);
    const signerKey = ed25519Keypair(7);
    const prior = await priorLedgerFor(signerKey.keyHash, "cd".repeat(32));
    const nativeTx = signedNativeTx({
      outRefBytes: prior.outRefBytes,
      fee: 7n,
      witnesses: (txId) => [
        encodeMidgardAddressWitnessItem({
          verificationKey: signerKey.verificationKey,
          signature: sign(null, txId, signerKey.privateKey),
        }),
      ],
    });
    const reason = { SpendInputSignerMissing: { input_index: 0n } } as const;
    const block = await commitForcedBlock(
      harness,
      family,
      nativeTx,
      prior.priorRoot,
      reason,
      "f7",
    );
    const evidence = prepareSpendInputSignerMissingEvidence({
      subject: block.subject,
      inputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
      resolved: prior.resolved,
    });
    expect(evidence.signerMissing).toBe(false);
    expect(evidence.validSignerHashes).toEqual([signerKey.keyHash]);
    const shape =
      "1 valid matching address witness; Inline field; one scan batch";
    const { references, certificateReference } = await publishReferences(
      harness,
      family,
      `${FAMILY}-forced`,
      false,
    );
    const run = familyDriver(harness, family, references, certificateReference);
    const measured = async <T>(
      name: string,
      operation: () => Promise<T>,
    ): Promise<T> => {
      const captured = await captureEmulatorSubmission(
        harness.emulator,
        operation,
      );
      recordMeasurements(name, "lifecycle", shape, captured);
      return captured.result;
    };
    const thread = await measured("forced-init", () =>
      run.initThread(block.blockOutRef),
    );
    const step01 = await measured("forced-step01", () =>
      run.step01Forced(thread.threadOutRef, evidence, block.forcedSource),
    );
    const step02 = await measured("forced-step02", () =>
      run.step02(
        step01.nextThreadOutRef,
        evidence,
        block.compactCbor,
        block.witnessSetCompactCbor,
      ),
    );
    const step03 = await measured("forced-step03", () =>
      run.step03(
        step02.nextThreadOutRef,
        evidence,
        block.compactCbor,
        block.witnessSetCompactCbor,
      ),
    );
    const step04 = await measured("forced-step04", () =>
      run.step04(
        step03.nextThreadOutRef,
        evidence,
        block.compactCbor,
        block.witnessSetCompactCbor,
      ),
    );
    expect(step04.stage).toBe("step05");
    const step05 = await measured("forced-step05-proof-mint", () =>
      run.step05(step04.nextThreadOutRef, evidence),
    );
    expect(step05.fraudProofUnit).toBeTruthy();
    coverage.reason(REASON, "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");
    const removal = await run.removal(block.headerHash);
    expect(removal.result.fraudCategoryId).toBe(SPEND_INPUT_SIGNER_MISSING_ID);
    recordMeasurements("forced-remove", "lifecycle", shape, removal);
  }, 600_000);

  it("refuses an honest accepted block, every substituted accepted seam, and a mutated spend coordinate on chain", async () => {
    const harness = await newHarness();
    const family = await registeredContracts(harness);
    // The credential's key signs the transaction from the last position of a
    // certified 160-witness field: the block is honest, so no acceptance
    // thread may close, however far the scan has to walk to learn it.
    const signerKey = ed25519Keypair(9);
    const prior = await priorLedgerFor(signerKey.keyHash, "ef".repeat(32));
    const nativeTx = signedNativeTx({
      outRefBytes: prior.outRefBytes,
      fee: 9n,
      witnesses: (txId) => [
        ...Array.from({ length: HONEST_ACCEPTED_WITNESSES - 1 }, (_u, index) =>
          garbageWitness(index),
        ),
        encodeMidgardAddressWitnessItem({
          verificationKey: signerKey.verificationKey,
          signature: sign(null, txId, signerKey.privateKey),
        }),
      ],
    });
    const block = await commitAcceptedBlock(
      harness,
      family,
      nativeTx,
      prior.priorRoot,
    );
    const accepted = acceptedVerdictSubject(block.nativeTxId);
    // Off chain the package refuses to prepare evidence that does not
    // contradict the verdict; the honest run below carries the authenticated
    // material under the accepted subject anyway, so the refusal is the
    // chain's.
    expect(() =>
      prepareSpendInputSignerMissingEvidence({
        subject: accepted,
        inputIndex: 0,
        canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
        resolved: prior.resolved,
      }),
    ).toThrow(/agrees with the operator verdict/u);
    const contradicting = prepareSpendInputSignerMissingEvidence({
      subject: forcedVerdictSubject({
        transactionId: block.nativeTxId,
        sourceKey: transitionTraceOutRef("f9"),
        rejectionReason: { SpendInputSignerMissing: { input_index: 0n } },
      }),
      inputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
      resolved: prior.resolved,
    });
    expect(contradicting.signerMissing).toBe(false);
    expect(contradicting.witnessCarriage).toBe("Certified");
    expect(contradicting.checkpoints).toHaveLength(10);
    const honest: SpendInputSignerMissingEvidence = {
      ...contradicting,
      subject: accepted,
      canonicalTransactionCborHex:
        encodeMidgardNativeTxCanonical(nativeTx).toString("hex"),
    };
    const { references, certificateReference } = await publishReferences(
      harness,
      family,
      `${FAMILY}-honest`,
      false,
    );
    const run = familyDriver(harness, family, references, certificateReference);

    const thread = await run.initThread(block.blockOutRef);
    // Transaction membership: a foreign transactions root cannot bind the
    // header's counted root, whatever proof rides with it.
    await expectRefusedOnChain(() =>
      run.step01Accepted(thread, honest, block.blockOutRef, {
        ...block.txInclusion,
        transactionsPhasRoot: "11".repeat(32),
      }),
    );
    coverage.seamMutated("tx_membership");
    const bound = await run.step01Accepted(
      thread,
      honest,
      block.blockOutRef,
      block.txInclusion,
    );
    // Prior-output membership: a descriptor for another output under the same
    // out-ref is not a member of the bound prior root.
    const foreign = await priorLedgerFor(
      Buffer.alloc(28, 0x77).toString("hex"),
      "ef".repeat(32),
    );
    await expectRefusedOnChain(() =>
      run.step02(
        bound.nextThreadOutRef,
        {
          ...honest,
          resolved: {
            ...honest.resolved!,
            descriptorCborHex: foreign.resolved.descriptorCborHex,
            outputCborHex: foreign.resolved.outputCborHex,
          },
        },
        block.compactCbor,
        block.witnessSetCompactCbor,
      ),
    );
    coverage.seamMutated("prior_output_membership");
    const authenticated = await run.step02(
      bound.nextThreadOutRef,
      honest,
      block.compactCbor,
      block.witnessSetCompactCbor,
    );
    // Field certificate: a certificate honestly minted over another
    // transaction's witness field, presented under this transaction's compact
    // structure and witness set, wears the wrong field hash and is refused at
    // the door.
    const foreignTx = makeNativeTx({
      spendInputCbors: [prior.outRefBytes],
      fee: 11n,
      addrTxWitsPreimageCbor: encodeCbor(
        Array.from({ length: HONEST_ACCEPTED_WITNESSES }, (_u, index) =>
          garbageWitness(index + 1_000),
        ),
      ),
    });
    const foreignEvidence = prepareSpendInputSignerMissingEvidence({
      subject: acceptedVerdictSubject(
        computeMidgardNativeTxId(foreignTx).toString("hex"),
      ),
      inputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(foreignTx),
      resolved: prior.resolved,
    });
    expect(foreignEvidence.witnessCarriage).toBe("Certified");
    await expectRefusedOnChain(() =>
      submitStep03WithForeignCertificate({
        harness,
        family,
        threadOutRef: authenticated.nextThreadOutRef,
        evidence: honest,
        nativeTxCompactCbor: block.compactCbor,
        witnessSetCompactCbor: block.witnessSetCompactCbor,
        foreign: {
          evidence: foreignEvidence,
          nativeTxCompactCbor: encodeMidgardNativeTxCompact(
            foreignTx.compact,
          ).toString("hex"),
          witnessSetCompactCbor: witnessSetCompactHex(foreignTx),
        },
        referenceScriptUtxo: references[2]!,
        certificateReference,
      }),
    );
    coverage.seamMutated("field_certificate");
    const scanning = await run.step03(
      authenticated.nextThreadOutRef,
      honest,
      block.compactCbor,
      block.witnessSetCompactCbor,
    );
    const { scans, threadOutRef } = await run.scanToTerminal(
      scanning.nextThreadOutRef,
      honest,
      block.compactCbor,
      block.witnessSetCompactCbor,
    );
    // The valid signature sits in the tenth batch: nine real resumptions
    // before the frontier admits it and the scan terminates.
    expect(scans).toHaveLength(10);
    // The family builder refuses to finalize a terminal that agrees with the
    // block, and the generic finalizer is refused by the validator itself.
    await expect(run.step05(threadOutRef, honest)).rejects.toThrow(
      /does not contradict verdict/u,
    );
    await expectRefusedOnChain(() => run.finalizeDirect(threadOutRef));
    coverage.scenario("honest_accepted_block_refusal");
    coverage.reason(REASON);

    // Spend coordinate: step 01 binds whatever coordinate the redeemer names;
    // step 02 selects it from the authenticated field and a one-input
    // transaction has no item 1.
    const mutated = await run.initThread(block.blockOutRef);
    const boundOutOfRange = await run.step01Accepted(
      mutated,
      { ...honest, inputIndex: 1 },
      block.blockOutRef,
      block.txInclusion,
    );
    await expectRefusedOnChain(() =>
      run.step02(
        boundOutOfRange.nextThreadOutRef,
        { ...honest, inputIndex: 1 },
        block.compactCbor,
        block.witnessSetCompactCbor,
      ),
    );
    coverage.scenario("reason_or_subject_coordinate_mutation");
  }, 900_000);

  it("refuses an honest forced rejection, a mutated forced reason coordinate, and a substituted forced leaf on chain", async () => {
    const harness = await newHarness();
    const family = await registeredContracts(harness);
    // Both Wave 3 mutations in one witness field: a valid signature from the
    // wrong key, and the right key with an invalid signature. Neither enters
    // the frontier, so the operator's rejection is exactly right.
    const signerKey = ed25519Keypair(11);
    const strangerKey = ed25519Keypair(12);
    const prior = await priorLedgerFor(signerKey.keyHash, "1a".repeat(32));
    const nativeTx = signedNativeTx({
      outRefBytes: prior.outRefBytes,
      fee: 13n,
      witnesses: (txId) => [
        encodeMidgardAddressWitnessItem({
          verificationKey: strangerKey.verificationKey,
          signature: sign(null, txId, strangerKey.privateKey),
        }),
        encodeMidgardAddressWitnessItem({
          verificationKey: signerKey.verificationKey,
          signature: Buffer.alloc(64, 0xff),
        }),
      ],
    });
    const reason = { SpendInputSignerMissing: { input_index: 0n } } as const;
    const block = await commitForcedBlock(
      harness,
      family,
      nativeTx,
      prior.priorRoot,
      reason,
      "fb",
    );
    expect(() =>
      prepareSpendInputSignerMissingEvidence({
        subject: block.subject,
        inputIndex: 0,
        canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
        resolved: prior.resolved,
      }),
    ).toThrow(/agrees with the operator verdict/u);
    const contradicting = prepareSpendInputSignerMissingEvidence({
      subject: acceptedVerdictSubject(block.nativeTxId),
      inputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
      resolved: prior.resolved,
    });
    expect(contradicting.signerMissing).toBe(true);
    // The stranger's signature verifies, so it is a signer — of the wrong
    // credential; the credential's own witness never verifies.
    expect(contradicting.validSignerHashes).toEqual([strangerKey.keyHash]);
    const honest: SpendInputSignerMissingEvidence = {
      ...contradicting,
      subject: block.subject,
      canonicalTransactionCborHex: encodeMidgardForcedTxCanonical(
        materializeMidgardForcedTxFromCanonical(nativeTx),
      ).toString("hex"),
    };
    // Classification refuses another family's typed reason outright.
    expect(() =>
      classifySpendInputSignerMissingFinding({
        subject: forcedVerdictSubject({
          transactionId: block.nativeTxId,
          sourceKey: block.sourceKey,
          rejectionReason: "ObserversForbiddenOnUntaggedNetwork",
        }),
        inputIndex: 0,
      }),
    ).toThrow(/wrong typed rejection reason/u);
    const { references, certificateReference } = await publishReferences(
      harness,
      family,
      `${FAMILY}-honest-forced`,
      false,
    );
    const run = familyDriver(harness, family, references, certificateReference);
    const thread = await run.initThread(block.blockOutRef);
    // Forced leaf: a leaf carrying another verdict is not in the forced root.
    await expectRefusedOnChain(() =>
      run.step01Forced(thread.threadOutRef, honest, {
        ...block.forcedSource,
        membership: {
          ...block.membership,
          value: { ...block.membership.value, verdict: "ForcedTxValid" },
        },
      }),
    );
    coverage.seamMutated("forced_leaf");
    // Reason coordinate: the leaf rejects input 0; a thread claiming the same
    // reason at input 1 is refused by the exact-reason bind.
    const shifted: VerdictSubject = forcedVerdictSubject({
      transactionId: block.nativeTxId,
      sourceKey: block.sourceKey,
      rejectionReason: { SpendInputSignerMissing: { input_index: 1n } },
    });
    await expectRefusedOnChain(() =>
      run.step01Forced(
        thread.threadOutRef,
        { ...honest, subject: shifted, inputIndex: 1 },
        block.forcedSource,
      ),
    );
    coverage.scenario("reason_or_subject_coordinate_mutation");
    const bound = await run.step01Forced(
      thread.threadOutRef,
      honest,
      block.forcedSource,
    );
    // Credential seam: a pub-key coordinate whose signer really is missing
    // cannot skip the witness scan through the direct exit; the validator
    // classifies the resolved credential itself.
    await expectRefusedOnChain(() =>
      run.step02(
        bound.nextThreadOutRef,
        {
          ...honest,
          route: "script_credential",
          signerRequired: false,
          signerMissing: false,
        },
        block.compactCbor,
        block.witnessSetCompactCbor,
      ),
    );
    coverage.seamMutated("credential");
    const authenticated = await run.step02(
      bound.nextThreadOutRef,
      honest,
      block.compactCbor,
      block.witnessSetCompactCbor,
    );
    const scanning = await run.step03(
      authenticated.nextThreadOutRef,
      honest,
      block.compactCbor,
      block.witnessSetCompactCbor,
    );
    const scan = await run.step04(
      scanning.nextThreadOutRef,
      honest,
      block.compactCbor,
      block.witnessSetCompactCbor,
    );
    expect(scan.stage).toBe("step05");
    await expect(run.step05(scan.nextThreadOutRef, honest)).rejects.toThrow(
      /does not contradict verdict/u,
    );
    await expectRefusedOnChain(() => run.finalizeDirect(scan.nextThreadOutRef));
    coverage.scenario("honest_forced_rejection_refusal");
  }, 600_000);

  it("proves a forced rejection over a script-locked spend input through the direct terminal route and refuses the scan door for it", async () => {
    const harness = await newHarness();
    const family = await registeredContracts(harness);
    // The operator rejected a transaction whose spend input is script-locked:
    // canonical validation authorizes a script credential with no signer, so
    // the rejection is wrong without any witness and step 02 closes at the
    // terminal directly.
    const scriptHash = Buffer.alloc(28, 0x5c).toString("hex");
    const prior = await priorLedgerFor(scriptHash, "e1".repeat(32), 0x70);
    const nativeTx = signedNativeTx({
      outRefBytes: prior.outRefBytes,
      fee: 17n,
      witnesses: () => [],
    });
    const reason = { SpendInputSignerMissing: { input_index: 0n } } as const;
    const block = await commitForcedBlock(
      harness,
      family,
      nativeTx,
      prior.priorRoot,
      reason,
      "f9",
    );
    const evidence = prepareSpendInputSignerMissingEvidence({
      subject: block.subject,
      inputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
      resolved: prior.resolved,
    });
    expect(evidence.route).toBe("script_credential");
    expect(evidence.signerRequired).toBe(false);
    const shape = "script-locked spend input; direct terminal route; no scan";
    const { references, certificateReference } = await publishReferences(
      harness,
      family,
      `${FAMILY}-forced-direct`,
      false,
    );
    const run = familyDriver(harness, family, references, certificateReference);
    const measured = async <T>(
      name: string,
      operation: () => Promise<T>,
    ): Promise<T> => {
      const captured = await captureEmulatorSubmission(
        harness.emulator,
        operation,
      );
      recordMeasurements(name, "lifecycle", shape, captured);
      return captured.result;
    };
    const thread = await measured("forced-direct-init", () =>
      run.initThread(block.blockOutRef),
    );
    const step01 = await measured("forced-direct-step01", () =>
      run.step01Forced(thread.threadOutRef, evidence, block.forcedSource),
    );
    // Credential seam: the same forced claim on this coordinate cannot take
    // the witness-scan door; the validator classifies the credential itself.
    await expectRefusedOnChain(() =>
      run.step02(
        step01.nextThreadOutRef,
        {
          ...evidence,
          route: "witness_scan",
          signerRequired: true,
          signerMissing: false,
          paymentCredentialHex: scriptHash,
        },
        block.compactCbor,
        block.witnessSetCompactCbor,
      ),
    );
    const step02 = await measured("forced-direct-step02-terminal-verdict", () =>
      run.step02(
        step01.nextThreadOutRef,
        evidence,
        block.compactCbor,
        block.witnessSetCompactCbor,
      ),
    );
    expect(step02.stage).toBe("step05");
    expect(step02.route).toBe("script_credential");
    const step05 = await measured("forced-direct-step05-proof-mint", () =>
      run.step05(step02.nextThreadOutRef, evidence),
    );
    expect(step05.fraudProofUnit).toBeTruthy();
    coverage.reason(REASON, "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");
    const removal = await run.removal(block.headerHash);
    expect(removal.result.fraudCategoryId).toBe(SPEND_INPUT_SIGNER_MISSING_ID);
    recordMeasurements("forced-direct-remove", "lifecycle", shape, removal);
  }, 600_000);

  it("declares the complete lifecycle coverage it exercised and writes the fit ledger it measured", async () => {
    // Recorded while the suites above ran, never pre-filled. The aggregate
    // field bound (32,768 bytes, 318 witnesses) is this family's consensus
    // bound, but no lifecycle transaction can present the adjacent 319-witness
    // field: the L2 codec refuses to lay it out and the door refuses its
    // certified view (`step_04_refuses_an_adjacent_over_bound_witness_field`),
    // so the suite claims no adjacent bound of its own.
    assertCompleteLifecycleCoverage({
      coverage: coverage.snapshot(),
      expectedReasonArms: [REASON],
      authenticationSeams: [...AUTHENTICATION_SEAMS],
      cancellablePhysicalSteps: [...PHYSICAL_STEPS],
      resumable: true,
      hasAdjacentConsensusBound: false,
    });
    const blueprintBytes = await readFile(realBlueprintPath);
    const blueprint = JSON.parse(blueprintBytes.toString("utf8")) as {
      readonly preamble?: { readonly compiler?: { readonly version?: string } };
    };
    const ledger = buildVanRossemFitLedger({
      category: "spendInputSignerMissing:00000027:testnet",
      blueprintSha256: createHash("sha256")
        .update(blueprintBytes)
        .digest("hex"),
      compilerVersion:
        blueprint.preamble?.compiler?.version ?? "unknown-aiken-compiler",
      measurements,
    });
    expect(ledger.entries).toHaveLength(measurements.length);
    expect(
      ledger.entries.every(
        (entry) =>
          entry.signedByteMargin > 0 &&
          BigInt(entry.memoryUnitMargin) > 0n &&
          BigInt(entry.cpuUnitMargin) > 0n,
      ),
    ).toBe(true);
    expect(
      ledger.entries
        .filter((entry) => entry.kind === "publication")
        .every((entry) => (entry.publicationReserveMargin ?? -1) >= 0),
    ).toBe(true);
    console.info(
      `[spend-input-signer-missing-fit-ledger] ${JSON.stringify(ledger)}`,
    );
    if (process.env.MIDGARD_WRITE_FIT_LEDGER === "1")
      await writeVanRossemFitLedger(
        fileURLToPath(
          new URL(
            "../../../docs/fault-proofs/size-plans/spend-input-signer-missing-v1-fit-ledger.json",
            import.meta.url,
          ),
        ),
        ledger,
      );
  });
});
