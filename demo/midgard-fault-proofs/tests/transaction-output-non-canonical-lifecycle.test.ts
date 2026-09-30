import "node:fs/promises";
import "@aiken-lang/merkle-patricia-forestry";
import "@al-ft/midgard-core";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/committed-field-shape/submit-committed-field-shape-init.js";
import "../src/remove-fraudulent-block.js";
import "../src/step-support.js";
import "../src/testing/complete-lifecycle.js";
import "../src/transaction-output-non-canonical/index.js";
import "../src/transition-trace/witnesses.js";
import "./support/emulator/expect-onchain-refusal.js";
import "./support/emulator/harness.js";
import "./support/emulator/measurement.js";
import "./support/emulator/native-tx.js";
import "./support/emulator/reference-scripts.js";
import "./support/emulator/registered-chain.js";
import "./support/emulator/removal-deployment.js";
import "./support/lifecycle-coverage.js";
import "./support/measured-fit-ledger.js";
import "./support/submit-init-emulator-fixtures.js";
import "./support/submit-init-emulator-shared.js";
import "./support/transaction-output-non-canonical-emulator.js";
import "./transaction-output-non-canonical-lifecycle.registered-contracts.js";
import "./transaction-output-non-canonical-lifecycle.scan-to-terminal.js";

import { writeFile } from "node:fs/promises";

import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { assertCompleteLifecycleCoverage } from "../src/testing/complete-lifecycle.js";
import {
  submitTransactionOutputNonCanonicalStep01Accepted,
  submitTransactionOutputNonCanonicalStep01Forced,
  submitTransactionOutputNonCanonicalStep02,
  submitTransactionOutputNonCanonicalStep03,
  submitTransactionOutputNonCanonicalStep04,
} from "../src/transaction-output-non-canonical/index.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { setupFraudulentBlock } from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  submitSetupTx,
  transitionTraceOutRef,
} from "./support/submit-init-emulator-shared.js";
import {
  buildForcedOutputFixture,
  canonicalOutputOfLength,
  initialScanStateOfItem,
  MALFORMED_OUTPUT,
  outputFieldOpening,
  type PublishedOutputFieldCarriage,
  publishOutputFieldCarriage,
  readOutputScanState,
  scanStateOf,
  scanWindowAt,
  submitOutputStep01ForcedRaw,
  submitOutputStep02Raw,
  submitOutputStep03Raw,
  submitOutputStep04Raw,
} from "./support/transaction-output-non-canonical-emulator.js";
import {
  committedInclusions,
  coverage,
  type Evidence,
  evidenceOf,
  fit,
  type Inclusion,
  MAXIMUM_OUTPUT_BYTES,
  type NativeTx,
  network,
  record,
  recordAll,
  registeredContracts,
  witnessSetCborOf,
} from "./transaction-output-non-canonical-lifecycle.registered-contracts.js";
import { scanToTerminal } from "./transaction-output-non-canonical-lifecycle.scan-to-terminal.js";

describe("transactionOutputNonCanonical registered-chain lifecycle", () => {
  it("convicts an accepted malformed output at the maximum shape, refuses every accepted seam, the honest twin and the adjacent width, cancels every step, then mints and removes", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realTransactionOutputNonCanonical: true,
        alwaysFraudProofCatalogue: true,
      },
    });
    const registered = await registeredContracts(harness);
    const { contracts, category, references, common } = registered;

    // A: the selected raw CBOR item is exactly the family maximum and its
    // leading byte is not an output map, so the scanner rejects it at the
    // first window. A second unselected item takes the authenticated field to
    // exactly the 32,768-byte Certified ceiling.
    const malformedTx = makeNativeTx({
      spendInputCbors: [],
      fee: 7n,
      outputCbors: [Buffer.alloc(MAXIMUM_OUTPUT_BYTES), Buffer.alloc(16_377)],
    });
    // B: the honest twin at the same maximum shape, canonical through the
    // longest admissible scan frontier.
    const canonicalTx = makeNativeTx({
      spendInputCbors: [],
      fee: 8n,
      outputCbors: [canonicalOutputOfLength(MAXIMUM_OUTPUT_BYTES)],
    });
    // C: the adjacent width, one byte over the family bound.
    const overBoundTx = makeNativeTx({
      spendInputCbors: [],
      fee: 9n,
      outputCbors: [Buffer.alloc(MAXIMUM_OUTPUT_BYTES + 1)],
    });
    // D: a transaction the block never committed.
    const foreignTx = makeNativeTx({
      spendInputCbors: [],
      fee: 10n,
      outputCbors: [MALFORMED_OUTPUT],
    });
    const committed = await committedInclusions([
      malformedTx,
      canonicalTx,
      overBoundTx,
    ]);
    const malformedInclusion = committed.inclusions[0]!;
    const canonicalInclusion = committed.inclusions[1]!;
    const overBoundInclusion = committed.inclusions[2]!;
    const foreign = await committedInclusions([foreignTx]);
    const setup = await setupFraudulentBlock({
      funderLucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      catalogue: registered.catalogue,
      fixture: {
        transactionsRoot: committed.transactionsRoot,
        l2TransactionCount: 3n,
      },
    });
    const certificateReference = await registered.publishReferences();

    const malformedEvidence = evidenceOf(
      acceptedVerdictSubject(malformedInclusion.nativeTxId),
      malformedTx,
      0,
    );
    expect(malformedEvidence.canonical).toBe(false);
    expect(malformedEvidence.itemLength).toBe(MAXIMUM_OUTPUT_BYTES);
    expect(Buffer.from(malformedEvidence.fieldPreimageHex, "hex")).toHaveLength(
      32_768,
    );
    expect(malformedEvidence.carriage).toBe("Certified");
    coverage.scenario("maximum_supported_evidence");
    // Cross-language descriptor reconstruction: the TypeScript scan agrees
    // with the rule's own maximum selector that this shape is canonical and
    // reaches its exact terminal only after several bounded windows.
    const canonicalEvidence = evidenceOf(
      acceptedVerdictSubject(canonicalInclusion.nativeTxId),
      canonicalTx,
      0,
    );
    expect(canonicalEvidence.canonical).toBe(true);
    expect(canonicalEvidence.itemLength).toBe(MAXIMUM_OUTPUT_BYTES);
    expect(canonicalEvidence.scanControls.length).toBeGreaterThan(3);
    expect(() =>
      evidenceOf(
        acceptedVerdictSubject(overBoundInclusion.nativeTxId),
        overBoundTx,
        0,
      ),
    ).toThrow(/fieldItemWidthIllegal/u);

    const publishCarriage = async (
      inclusion: Inclusion,
      nativeTx: NativeTx,
      items: readonly Buffer[],
    ) =>
      await captureEmulatorSubmission(harness.emulator, () =>
        publishOutputFieldCarriage({
          lucid: harness.proverLucid,
          network,
          signer: harness.proverSigner,
          contracts,
          anchorTxId: inclusion.nativeTxId,
          nativeTxCompactCbor: inclusion.nativeTxCompactCbor,
          witnessSetCompactCbor: witnessSetCborOf(nativeTx),
          items,
          certificateReferenceScriptUtxo: certificateReference,
        }),
      );
    const malformedCarriage = await publishCarriage(
      malformedInclusion,
      malformedTx,
      [Buffer.alloc(MAXIMUM_OUTPUT_BYTES), Buffer.alloc(16_377)],
    );
    expect(malformedCarriage.result.planned.plan.tier).toBe("Certified");
    malformedCarriage.measurements.forEach((measurement, index) => {
      record(
        index === malformedCarriage.measurements.length - 1
          ? "accepted-carriage-certificate"
          : `accepted-carriage-chunk${(index + 1).toString().padStart(2, "0")}`,
        measurement,
      );
    });
    const canonicalCarriage = (
      await publishCarriage(canonicalInclusion, canonicalTx, [
        canonicalOutputOfLength(MAXIMUM_OUTPUT_BYTES),
      ])
    ).result;

    const bindAccepted = async (inclusion: Inclusion, evidence: Evidence) => {
      const init = await registered.init(setup.fraudulentBlockOutRef);
      const threadUtxo = await registered.threadUtxoOf(
        init.result.txHash,
        init.result.firstStepOutputIndex,
      );
      const bound = await captureEmulatorSubmission(harness.emulator, () =>
        submitTransactionOutputNonCanonicalStep01Accepted({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          network,
          contracts,
          signer: harness.proverSigner,
          finding: evidence,
          threadUtxo,
          threadToken: {
            unit: init.result.computationThreadUnit,
            fraudulentHeaderHash: init.result.fraudulentHeaderHash,
          },
          stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
          txInclusion: inclusion,
          referenceScriptUtxo: references[0]!,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      );
      return { init, bound, threadOutRef: bound.result.nextThreadOutRef };
    };
    const refuseStep01 = async (
      seam: string,
      inclusion: Inclusion,
      evidence: Evidence,
    ) => {
      const init = await registered.init(setup.fraudulentBlockOutRef);
      const threadUtxo = await registered.threadUtxoOf(
        init.result.txHash,
        init.result.firstStepOutputIndex,
      );
      await expectOnchainRefusal(
        async () =>
          await submitTransactionOutputNonCanonicalStep01Accepted({
            lucid: harness.proverLucid,
            blueprint: harness.realBlueprint,
            network,
            contracts,
            signer: harness.proverSigner,
            finding: evidence,
            threadUtxo,
            threadToken: {
              unit: init.result.computationThreadUnit,
              fraudulentHeaderHash: init.result.fraudulentHeaderHash,
            },
            stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
            txInclusion: inclusion,
            referenceScriptUtxo: references[0]!,
            witnessReferenceScripts: harness.witnessReferenceScripts,
          }),
      );
      coverage.seamMutated(seam);
    };
    const step02 = async (
      threadOutRef: string,
      evidence: Evidence,
      inclusion: Inclusion,
      nativeTx: NativeTx,
      carriage: PublishedOutputFieldCarriage,
    ) =>
      await captureEmulatorSubmission(harness.emulator, () =>
        submitTransactionOutputNonCanonicalStep02({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef,
          evidence,
          nativeTxCompactCbor: inclusion.nativeTxCompactCbor,
          witnessSetCompactCbor: witnessSetCborOf(nativeTx),
          publishedCarriageUtxos: carriage.carriageUtxos,
          certificateUtxo: carriage.certificateUtxo,
          referenceScriptUtxo: references[1]!,
          certificateReferenceScriptUtxo: certificateReference,
        }),
      );
    const step03 = async (threadOutRef: string, evidence: Evidence) =>
      await captureEmulatorSubmission(harness.emulator, () =>
        submitTransactionOutputNonCanonicalStep03({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef,
          evidence,
          nativeTxCompactCbor: "",
          witnessSetCompactCbor: "",
          referenceScriptUtxo: references[2]!,
        }),
      );

    // ## Wrongful acceptance at the maximum shape.
    const conviction = await bindAccepted(
      malformedInclusion,
      malformedEvidence,
    );
    record("accepted-init", conviction.init.measurement);
    record("accepted-step01", conviction.bound.measurement);
    const authenticated = await step02(
      conviction.threadOutRef,
      malformedEvidence,
      malformedInclusion,
      malformedTx,
      malformedCarriage.result,
    );
    record("accepted-step02", authenticated.measurement);
    const scanned = await step03(
      authenticated.result.nextThreadOutRef,
      malformedEvidence,
    );
    expect(scanned.result.terminal).toBe(true);
    record("accepted-step03-scan", scanned.measurement);
    const minted = await captureEmulatorSubmission(harness.emulator, () =>
      submitTransactionOutputNonCanonicalStep04({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: scanned.result.nextThreadOutRef,
        evidence: malformedEvidence,
        referenceScriptUtxo: references[3]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expect(minted.result.fraudProofUnit).toBeTruthy();
    record("accepted-step04-proof-mint", minted.measurement);
    coverage.reason("OutputNonCanonical", "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");

    // ## Cancel from every physical step.
    for (const targetStep of [0, 1, 2, 3] as const) {
      const init = await registered.init(setup.fraudulentBlockOutRef);
      let thread = `${init.result.txHash}#${init.result.firstStepOutputIndex.toString()}`;
      if (targetStep >= 1) {
        thread = (
          await submitTransactionOutputNonCanonicalStep01Accepted({
            lucid: harness.proverLucid,
            blueprint: harness.realBlueprint,
            network,
            contracts,
            signer: harness.proverSigner,
            finding: malformedEvidence,
            threadUtxo: await registered.threadUtxoOf(
              init.result.txHash,
              init.result.firstStepOutputIndex,
            ),
            threadToken: {
              unit: init.result.computationThreadUnit,
              fraudulentHeaderHash: init.result.fraudulentHeaderHash,
            },
            stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
            txInclusion: malformedInclusion,
            referenceScriptUtxo: references[0]!,
            witnessReferenceScripts: harness.witnessReferenceScripts,
          })
        ).nextThreadOutRef;
      }
      if (targetStep >= 2) {
        thread = (
          await step02(
            thread,
            malformedEvidence,
            malformedInclusion,
            malformedTx,
            malformedCarriage.result,
          )
        ).result.nextThreadOutRef;
      }
      if (targetStep >= 3) {
        const scan = await step03(thread, malformedEvidence);
        expect(scan.result.terminal).toBe(true);
        thread = scan.result.nextThreadOutRef;
      }
      const cancelled = await registered.cancel(thread, targetStep);
      record(
        `accepted-cancel-step0${(targetStep + 1).toString()}`,
        cancelled.measurement,
      );
      coverage.cancelled(`step-0${(targetStep + 1).toString()}`);
    }

    // ## Step-01 transaction-membership seams.
    await refuseStep01(
      "tx_membership",
      { ...malformedInclusion, transactionsPhasRoot: foreign.transactionsRoot },
      malformedEvidence,
    );
    await refuseStep01(
      "tx_membership",
      {
        ...malformedInclusion,
        txMembershipProof: canonicalInclusion.txMembershipProof,
        txMembershipProofCbor: canonicalInclusion.txMembershipProofCbor,
      },
      malformedEvidence,
    );
    // Substituted source: a transaction the block never committed, carrying
    // its own consistent root and proof.
    await refuseStep01(
      "substituted_source",
      foreign.inclusions[0]!,
      evidenceOf(
        acceptedVerdictSubject(foreign.inclusions[0]!.nativeTxId),
        foreignTx,
        0,
      ),
    );

    // ## Coordinate mutation: the thread binds an out-of-range field-2 index.
    const genuineOpening = outputFieldOpening({
      anchorCompactCbor: malformedInclusion.nativeTxCompactCbor,
      carriage: malformedCarriage.result,
      stepReference: references[1]!,
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    });
    const initialState = scanStateOf(malformedEvidence, 0, 0n);
    const mutated = await bindAccepted(malformedInclusion, {
      ...malformedEvidence,
      itemIndex: 2,
    });
    await expectOnchainRefusal(
      async () =>
        await submitOutputStep02Raw({
          ...common(mutated.threadOutRef, 1),
          opening: genuineOpening,
          nextState: { ...initialState, output_index: 2n },
          carriageUtxos: malformedCarriage.result.carriageUtxos,
          extraReferenceInputs: [malformedCarriage.result.certificateUtxo!],
        }),
    );
    coverage.scenario("reason_or_subject_coordinate_mutation");

    // ## Step-02 seams against a thread bound to the malformed coordinate.
    const seamThread = await bindAccepted(
      malformedInclusion,
      malformedEvidence,
    );
    const refuseStep02 = async (
      seam: string,
      input: Partial<Parameters<typeof submitOutputStep02Raw>[0]>,
    ) => {
      await expectOnchainRefusal(
        async () =>
          await submitOutputStep02Raw({
            ...common(seamThread.threadOutRef, 1),
            opening: genuineOpening,
            nextState: initialState,
            carriageUtxos: malformedCarriage.result.carriageUtxos,
            extraReferenceInputs: [malformedCarriage.result.certificateUtxo!],
            ...input,
          }),
      );
      coverage.seamMutated(seam);
    };
    // Own-output substitution: the honest twin's certified field-2 bytes
    // presented under the malformed transaction's anchor.
    await refuseStep02("field_certificate", {
      opening: outputFieldOpening({
        anchorCompactCbor: malformedInclusion.nativeTxCompactCbor,
        carriage: canonicalCarriage,
        stepReference: references[1]!,
        certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      }),
      nextState: scanStateOf(canonicalEvidence, 0, 0n),
      carriageUtxos: canonicalCarriage.carriageUtxos,
      extraReferenceInputs:
        canonicalCarriage.certificateUtxo === undefined
          ? []
          : [canonicalCarriage.certificateUtxo],
    });
    await refuseStep02("initial_checkpoint", {
      nextState: {
        ...initialState,
        item_length: initialState.item_length + 1n,
      },
    });
    await refuseStep02("initial_checkpoint", {
      nextState: {
        ...initialState,
        chunk_hashes: ["ff".repeat(32), ...initialState.chunk_hashes.slice(1)],
      },
    });
    await refuseStep02("step_02_successor", { nextStepIndex: 3 });

    // ## Step-03 seams at the malformed item's single terminal window.
    const seamScan = await step02(
      seamThread.threadOutRef,
      malformedEvidence,
      malformedInclusion,
      malformedTx,
      malformedCarriage.result,
    );
    const terminalState = scanStateOf(malformedEvidence, 0, 2n);
    const window = scanWindowAt(Buffer.from(malformedEvidence.itemHex, "hex"), {
      cursor: 0n,
      stage: 0n,
    });
    const refuseStep03 = async (
      seam: string,
      input: Partial<Parameters<typeof submitOutputStep03Raw>[0]>,
    ) => {
      await expectOnchainRefusal(
        async () =>
          await submitOutputStep03Raw({
            ...common(seamScan.result.nextThreadOutRef, 2),
            window,
            nextState: terminalState,
            nextStepIndex: 3,
            ...input,
          }),
      );
      coverage.seamMutated(seam);
    };
    const flipped = Buffer.from(window);
    flipped[1] ^= 0x01;
    await refuseStep03("scan_window", { window: flipped });
    await refuseStep03("scan_window", { window: window.subarray(0, 4_095) });
    await refuseStep03("scan_checkpoint", {
      nextState: { ...terminalState, outcome: 1n },
    });
    await refuseStep03("step_03_successor", { nextStepIndex: 2 });
    await registered.cancel(seamScan.result.nextThreadOutRef, 2);

    // ## Honest accepted block: the canonical twin scans to its exact
    // terminal through real checkpoints and the proof mint refuses on chain.
    const honest = await bindAccepted(canonicalInclusion, canonicalEvidence);
    const honestAuthenticated = await step02(
      honest.threadOutRef,
      canonicalEvidence,
      canonicalInclusion,
      canonicalTx,
      canonicalCarriage,
    );
    record("accepted-canonical-step02", honestAuthenticated.measurement);
    const honestScan = await scanToTerminal({
      registered,
      threadOutRef: honestAuthenticated.result.nextThreadOutRef,
      evidence: canonicalEvidence,
      rederive: () =>
        evidenceOf(
          acceptedVerdictSubject(canonicalInclusion.nativeTxId),
          canonicalTx,
          0,
        ),
      label: "accepted-canonical-step03",
      nativeTxCompactCbor: "",
      witnessSetCompactCbor: "",
    });
    expect(honestScan.scans).toBe(canonicalEvidence.scanControls.length - 1);
    const honestTerminal = await readOutputScanState(
      common(honestScan.threadOutRef, 3),
      3,
    );
    expect(honestTerminal.outcome).toBe(1n);
    await expect(
      submitTransactionOutputNonCanonicalStep04({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: honestScan.threadOutRef,
        evidence: canonicalEvidence,
        referenceScriptUtxo: references[3]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/does not contradict/u);
    await expectOnchainRefusal(
      async () =>
        await submitOutputStep04Raw({
          ...common(honestScan.threadOutRef, 3),
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
    );
    coverage.scenario("honest_accepted_block_refusal");
    await registered.cancel(honestScan.threadOutRef, 3);

    // ## Adjacent over-bound width: the 16,385-byte item is authenticated
    // and then refused by the scan initializer, never scanned.
    const overBoundCarriage = (
      await publishCarriage(overBoundInclusion, overBoundTx, [
        Buffer.alloc(MAXIMUM_OUTPUT_BYTES + 1),
      ])
    ).result;
    const overBoundSubject = acceptedVerdictSubject(
      overBoundInclusion.nativeTxId,
    );
    const overBound = await bindAccepted(overBoundInclusion, {
      ...malformedEvidence,
      subject: overBoundSubject,
      itemIndex: 0,
    });
    await expectOnchainRefusal(
      async () =>
        await submitOutputStep02Raw({
          ...common(overBound.threadOutRef, 1),
          opening: outputFieldOpening({
            anchorCompactCbor: overBoundInclusion.nativeTxCompactCbor,
            carriage: overBoundCarriage,
            stepReference: references[1]!,
            certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
          }),
          nextState: initialScanStateOfItem({
            subject: overBoundSubject,
            itemIndex: 0,
            item: Buffer.alloc(MAXIMUM_OUTPUT_BYTES + 1),
          }),
          carriageUtxos: overBoundCarriage.carriageUtxos,
          extraReferenceInputs:
            overBoundCarriage.certificateUtxo === undefined
              ? []
              : [overBoundCarriage.certificateUtxo],
        }),
    );
    coverage.adjacentOverBoundRefused();

    // ## Permanent proof token, then state-queue target and descendant removal.
    const removed = await registered.removal(setup.headerHash);
    record("accepted-remove-fraudulent-block", removed.measurement);
    coverage.scenario("permanent_proof_token_and_descendant_removal");
  }, 900_000);

  it("convicts a forced rejection of the maximum canonical output through real checkpoints, refuses every forced seam, then mints and removes", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realTransactionOutputNonCanonical: true,
        alwaysFraudProofCatalogue: true,
      },
    });
    const registered = await registeredContracts(harness);
    const { contracts, category, references, common } = registered;
    const funderCredential = getAddressDetails(
      await harness.funderLucid.wallet().address(),
    ).paymentCredential;
    if (funderCredential?.type !== "Key")
      throw new Error("forced fixture funder key absent");
    const forced = await buildForcedOutputFixture({
      operatorVkey: funderCredential.hash,
      now:
        alignUnixTimeToEmulatorSlotBoundary(
          harness.funderLucid,
          harness.emulator.now() + 120_000,
        ) - 1,
      outputCbor: canonicalOutputOfLength(MAXIMUM_OUTPUT_BYTES),
    });
    const setup = await submitSetupTx({
      lucid: harness.funderLucid,
      contracts: harness.contracts,
      nonceUtxo: harness.nonceUtxo,
      catalogue: registered.catalogue,
      header: forced.header,
    });
    const certificateReference = await registered.publishReferences();
    const membership = await buildForcedTransactionLeafMembershipProof({
      reconstruction: forced.reconstruction,
      eventKey: forced.eventKey,
    });
    const subject = forcedVerdictSubject({
      transactionId: forced.transaction.tx_id,
      sourceKey: membership.key,
      rejectionReason: forced.rejectionReason,
    });
    const evidence = evidenceOf(subject, forced.nativeTx, 0);
    expect(evidence.canonical).toBe(true);
    expect(evidence.itemLength).toBe(MAXIMUM_OUTPUT_BYTES);
    expect(evidence.scanControls.length).toBeGreaterThan(3);
    const initThread = async () => {
      const init = await registered.init(setup.fraudulentBlockOutRef);
      return {
        init,
        threadOutRef: `${init.result.txHash}#${init.result.firstStepOutputIndex.toString()}`,
      };
    };

    // ## Forced-leaf seams and the exact reason coordinate, each on a fresh thread.
    const refuseStep01 = async (
      seam: string,
      input: Partial<Parameters<typeof submitOutputStep01ForcedRaw>[0]>,
    ) => {
      const { threadOutRef } = await initThread();
      await expectOnchainRefusal(
        async () =>
          await submitOutputStep01ForcedRaw({
            ...common(threadOutRef, 0),
            header: forced.header,
            membership,
            direction: 1n,
            outputIndex: 0n,
            ...input,
          }),
      );
      coverage.seamMutated(seam);
    };
    await refuseStep01("forced_leaf", {
      membership: { ...membership, key: transitionTraceOutRef("f2") },
    });
    await refuseStep01("forced_leaf", {
      header: { ...forced.header, utxosRoot: "ff".repeat(32) },
    });
    await refuseStep01("forced_leaf", { direction: 0n });
    // Reason/coordinate mutation: the redeemer names output 1 while the
    // committed reason is `OutputNonCanonical { output_index: 0 }`.
    await refuseStep01("reason_coordinate", { outputIndex: 1n });
    coverage.scenario("reason_or_subject_coordinate_mutation");

    // ## Wrongful forced rejection at the maximum canonical shape.
    const { init, threadOutRef } = await initThread();
    record("forced-init", init.measurement);
    const bound = await captureEmulatorSubmission(harness.emulator, () =>
      submitTransactionOutputNonCanonicalStep01Forced({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        finding: evidence,
        forcedSource: { header: forced.header, membership, direction: 1n },
        referenceScriptUtxo: references[0]!,
      }),
    );
    record("forced-step01", bound.measurement);
    const authenticated = await captureEmulatorSubmission(
      harness.emulator,
      () =>
        submitTransactionOutputNonCanonicalStep02({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: bound.result.nextThreadOutRef,
          evidence,
          nativeTxCompactCbor: forced.transaction.submitted_source.compact_cbor,
          witnessSetCompactCbor:
            forced.transaction.submitted_source.witness_set_compact_cbor,
          referenceScriptUtxo: references[1]!,
          certificateReferenceScriptUtxo: certificateReference,
        }),
    );
    recordAll(
      "forced-step02",
      authenticated.measurements,
      authenticated.result.carriageTier === "Certified",
    );
    const scanned = await scanToTerminal({
      registered,
      threadOutRef: authenticated.result.nextThreadOutRef,
      evidence,
      rederive: () => evidenceOf(subject, forced.nativeTx, 0),
      label: "forced-step03",
      nativeTxCompactCbor: forced.transaction.submitted_source.compact_cbor,
      witnessSetCompactCbor:
        forced.transaction.submitted_source.witness_set_compact_cbor,
    });
    expect(scanned.scans).toBe(evidence.scanControls.length - 1);
    const minted = await captureEmulatorSubmission(harness.emulator, () =>
      submitTransactionOutputNonCanonicalStep04({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: scanned.threadOutRef,
        evidence,
        referenceScriptUtxo: references[3]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expect(minted.result.fraudProofUnit).toBeTruthy();
    record("forced-step04-proof-mint", minted.measurement);
    coverage.reason("OutputNonCanonical", "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");
    const removed = await registered.removal(setup.headerHash);
    record("forced-remove-fraudulent-block", removed.measurement);
  }, 900_000);

  it("refuses to mint against an honest forced rejection: the malformed output reaches its non-canonical terminal and step 04 refuses on chain", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realTransactionOutputNonCanonical: true,
        alwaysFraudProofCatalogue: true,
      },
    });
    const registered = await registeredContracts(harness);
    const { contracts, category, references, common } = registered;
    const funderCredential = getAddressDetails(
      await harness.funderLucid.wallet().address(),
    ).paymentCredential;
    if (funderCredential?.type !== "Key")
      throw new Error("forced fixture funder key absent");
    const forced = await buildForcedOutputFixture({
      operatorVkey: funderCredential.hash,
      now:
        alignUnixTimeToEmulatorSlotBoundary(
          harness.funderLucid,
          harness.emulator.now() + 120_000,
        ) - 1,
      outputCbor: MALFORMED_OUTPUT,
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
    const subject = forcedVerdictSubject({
      transactionId: forced.transaction.tx_id,
      sourceKey: membership.key,
      rejectionReason: forced.rejectionReason,
    });
    const evidence = evidenceOf(subject, forced.nativeTx, 0);
    expect(evidence.canonical).toBe(false);
    expect(evidence.decisiveFaultHolds).toBe(true);
    const init = await registered.init(setup.fraudulentBlockOutRef);
    const bound = await submitTransactionOutputNonCanonicalStep01Forced({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: `${init.result.txHash}#${init.result.firstStepOutputIndex.toString()}`,
      finding: evidence,
      forcedSource: { header: forced.header, membership, direction: 1n },
      referenceScriptUtxo: references[0]!,
    });
    const authenticated = await submitTransactionOutputNonCanonicalStep02({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: bound.nextThreadOutRef,
      evidence,
      nativeTxCompactCbor: forced.transaction.submitted_source.compact_cbor,
      witnessSetCompactCbor:
        forced.transaction.submitted_source.witness_set_compact_cbor,
      referenceScriptUtxo: references[1]!,
    });
    const scanned = await submitTransactionOutputNonCanonicalStep03({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: authenticated.nextThreadOutRef,
      evidence,
      nativeTxCompactCbor: forced.transaction.submitted_source.compact_cbor,
      witnessSetCompactCbor:
        forced.transaction.submitted_source.witness_set_compact_cbor,
      referenceScriptUtxo: references[2]!,
    });
    expect(scanned.terminal).toBe(true);
    const terminal = await readOutputScanState(
      common(scanned.nextThreadOutRef, 3),
      3,
    );
    expect(terminal.outcome).toBe(2n);
    await expect(
      submitTransactionOutputNonCanonicalStep04({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: scanned.nextThreadOutRef,
        evidence,
        referenceScriptUtxo: references[3]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/does not contradict/u);
    await expectOnchainRefusal(
      async () =>
        await submitOutputStep04Raw({
          ...common(scanned.nextThreadOutRef, 3),
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
    );
    coverage.scenario("honest_forced_rejection_refusal");
    await registered.cancel(scanned.nextThreadOutRef, 3);
  }, 600_000);

  it("declares the complete lifecycle coverage it exercised", async () => {
    // Recorded while the suites above ran, never pre-filled.
    assertCompleteLifecycleCoverage({
      coverage: coverage.snapshot(),
      expectedReasonArms: ["OutputNonCanonical"],
      authenticationSeams: [
        "tx_membership",
        "substituted_source",
        "forced_leaf",
        "reason_coordinate",
        "field_certificate",
        "initial_checkpoint",
        "scan_window",
        "scan_checkpoint",
        "step_02_successor",
        "step_03_successor",
      ],
      cancellablePhysicalSteps: ["step-01", "step-02", "step-03", "step-04"],
      resumable: true,
      hasAdjacentConsensusBound: true,
    });
    for (const [name, measurement] of fit) {
      expect(measurement.l1ByteMargin, name).toBeGreaterThan(0);
    }
    const output = process.env.MIDGARD_FIT_OUT;
    if (output !== undefined) {
      await writeFile(
        output,
        JSON.stringify(
          fit.map(([name, measurement]) => ({
            name,
            signedBytes: measurement.completeSignedBytes,
            memoryUnits: measurement.executionMemory.toString(),
            cpuUnits: measurement.executionSteps.toString(),
          })),
          null,
          2,
        ),
      );
    }
  });
});
