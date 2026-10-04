import "node:crypto";
import "node:fs";
import "node:url";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec/forced";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/committed-field-shape/submit-committed-field-shape-init.js";
import "../src/field-opening.js";
import "../src/missing-native-script-tx/staged-walk.js";
import "../src/observer-order-invalid/actuator.js";
import "../src/observer-order-invalid/artifact.js";
import "../src/observer-order-invalid/contracts.js";
import "../src/observer-order-invalid/family.js";
import "../src/observer-order-invalid/staged-plan.js";
import "../src/observer-order-invalid/submit-cancel.js";
import "../src/observer-order-invalid/submit-step-01.js";
import "../src/observer-order-invalid/submit-step-02.js";
import "../src/observer-order-invalid/submit-step-03.js";
import "../src/observer-order-invalid/submit-step-04.js";
import "../src/proof-fit/van-rossem-fit-ledger.js";
import "../src/remove-fraudulent-block.js";
import "../src/testing/complete-lifecycle.js";
import "../src/workflow/transaction-boundary.js";
import "./support/emulator/blueprints.js";
import "./support/emulator/emulator-context.js";
import "./support/emulator/expect-onchain-refusal.js";
import "./support/emulator/harness.js";
import "./support/emulator/measurement.js";
import "./support/emulator/reference-scripts.js";
import "./support/emulator/registered-chain.js";
import "./support/emulator/removal-deployment.js";
import "./support/emulator/setup-tx.js";
import "./support/lifecycle-coverage.js";
import "./support/observer-order-invalid-raw.js";
import "./support/submit-init-emulator-fixtures.js";
import "./support/submit-init-emulator-shared.js";
import "./observer-order-invalid-lifecycle.authentication-seams.js";
import "./observer-order-invalid-lifecycle.make-harness.js";
import "./observer-order-invalid-lifecycle.forced-success.js";
import "./observer-order-invalid-lifecycle.accepted-success.js";

import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";

import { midgardFieldCommitment } from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { describe, expect, it, vi } from "vitest";

import { advanceMissingNativeScriptTxSemanticCheckpoint } from "../src/missing-native-script-tx/staged-walk.js";
import { createObserverOrderInvalidActuator } from "../src/observer-order-invalid/actuator.js";
import { buildObserverOrderInvalidArtifact } from "../src/observer-order-invalid/artifact.js";
import {
  classifyObserverOrderInvalidFinding,
  OBSERVER_ORDER_INVALID_ITEM_BUDGET,
  type ObserverOrderInvalidEvidence,
  observerOrderInvalidEvidenceCloses,
  type ObserverOrderInvalidFinding,
} from "../src/observer-order-invalid/family.js";
import {
  encodeObserverOrderWalkCheckpoint,
  hashObserverOrderWalkCheckpoint,
  type ObserverOrderInvalidStagedPlan,
} from "../src/observer-order-invalid/staged-plan.js";
import {
  buildVanRossemFitLedger,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { assertCompleteLifecycleCoverage } from "../src/testing/complete-lifecycle.js";
import { submitCapturedTransaction } from "../src/workflow/transaction-boundary.js";
import { acceptedSuccess } from "./observer-order-invalid-lifecycle.accepted-success.js";
import {
  AUTHENTICATION_SEAMS,
  CANCELLABLE_STEPS,
  CATEGORY_ID,
  coverage,
  LAST_ORDINAL,
  ledgerPath,
  MAXIMUM_FIELD_BYTES,
  MAXIMUM_OBSERVERS,
  MAXIMUM_SCANS,
  measurements,
  network,
  REASON_ARM,
  record,
  scanLabel,
  type Seam,
} from "./observer-order-invalid-lifecycle.authentication-seams.js";
import {
  acceptedBlock,
  acceptedFinding,
  duplicateFirstShape,
  earlierViolationShape,
  emptyShape,
  evidenceOf,
  firstPairDescendingShape,
  forcedBlock,
  forcedFinding,
  forcedSuccess,
  maximumLastViolationShape,
  maximumOrderedShape,
  middleDuplicateShape,
  reasonAt,
  singleShape,
  smallOrderedShape,
  stagedOf,
  twoOrderedShape,
} from "./observer-order-invalid-lifecycle.forced-success.js";
import { makeHarness } from "./observer-order-invalid-lifecycle.make-harness.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import {
  ascendingObservers,
  compactCborHex,
  mutateCertifiedCarriage,
  mutateCompactSource,
  mutateRawUtxoCarriage,
  observerAt,
  observerFieldShape,
  transactionIdOf,
  witnessSetCompactCborHex,
} from "./support/observer-order-invalid-raw.js";

describe("observerOrderInvalid registered-chain lifecycle", () => {
  it("convicts the maximum accepted field at its last ordinal through the production actuator, cancels every step, refuses every accepted seam and the honest ordered field, then mints and removes", async () => {
    const h = await makeHarness();
    const maximum = maximumLastViolationShape();
    const honest = smallOrderedShape();
    expect(maximum.fieldPreimage).toHaveLength(MAXIMUM_FIELD_BYTES);
    // Adjacent over the §5.4 aggregate field bound: the field has no carriage
    // tier, so this family can neither prepare evidence for it nor open it;
    // the shared field door refuses its length on chain, and the fault it
    // carries belongs to the committed-field-shape families.
    const overBound = observerFieldShape({
      label: "over bound",
      observers: ascendingObservers(MAXIMUM_OBSERVERS + 1),
    });
    expect(() => evidenceOf(overBound, acceptedFinding(overBound, 1))).toThrow(
      /aggregate bound/u,
    );
    const { setup, inclusions } = await acceptedBlock(h, [maximum, honest]);
    const maximumInclusion = inclusions[0]!;
    const finding = acceptedFinding(maximum, LAST_ORDINAL);
    const evidence = evidenceOf(maximum, finding);
    const staged = stagedOf(maximum, LAST_ORDINAL);
    expect(evidence.carriage).toBe("Certified");
    expect(evidence.violation).toBe(true);
    expect(staged.walk).toHaveLength(MAXIMUM_SCANS);

    const published = await h.publishField(maximum);
    h.recordCarriage("accepted", maximum, published);
    const artifact = buildObserverOrderInvalidArtifact({
      headerHash: setup.headerHash,
      detectionId: `${transactionIdOf(maximum)}:accepted:${LAST_ORDINAL.toString()}`,
      position: 0n,
      evidence,
      nativeTxCompactCbor: maximumInclusion.nativeTxCompactCbor,
      witnessSetCompactCbor: witnessSetCompactCborHex(maximum.nativeTx),
      l2TransactionSourceCbor: maximumInclusion.l2TransactionSourceCbor,
      transactionsPhasRoot: maximumInclusion.transactionsPhasRoot,
      transactionMembershipCbor: maximumInclusion.txMembershipProofCbor,
    });
    const actuator = (deploymentInfo: unknown) =>
      createObserverOrderInvalidActuator({
        binding: {
          definition: { headerHash: setup.headerHash },
          resolvedContracts: {
            category: { categoryId: h.category.categoryId },
            contracts: {
              fraudProof: {
                spendingScriptHash:
                  h.harness.contracts.fraudProof.spendingScriptHash,
              },
            },
          },
          network,
          blueprint: h.harness.realBlueprint,
          deploymentInfo,
          releaseEconomics: {
            policy: { fraudProverRewardLovelace: "400000000" },
          },
        } as never,
        lucid: h.harness.proverLucid,
        signer: h.harness.proverSigner,
        contracts: h.contracts,
        references: {
          steps: h.stepReferences(),
          witnesses: h.harness.witnessReferenceScripts as never,
          fieldPreimageCertificateMint: h.requireCertificateReference(),
        },
        stateQueueMutationLeaseCoordinator: {
          acquire: async () => ({
            token: "observer-order-emulator",
            source: "emulator",
            renew: async () => {},
            release: async () => {},
            fail: async () => {},
          }),
        },
      });
    const proofActuator = actuator({});
    const drive = async (
      action: Parameters<typeof proofActuator.capture>[0]["action"],
      nextAddress: string,
    ) =>
      captureEmulatorSubmission(h.harness.emulator, async () => {
        const captured = await proofActuator.capture({ action, artifact });
        const txHash = await submitCapturedTransaction(captured.transaction);
        expect(txHash).toBe(captured.transaction.txHash);
        await h.harness.proverLucid.awaitTx(txHash);
        const next = (await h.harness.proverLucid.utxosAt(nextAddress)).find(
          (utxo) => utxo.txHash === txHash,
        );
        if (next === undefined)
          throw new Error("actuator omitted its next thread output");
        return {
          txHash,
          nextThreadOutRef: `${next.txHash}#${next.outputIndex.toString()}`,
          fraudProofUnit: Object.keys(next.assets).find(
            (unit) => unit !== "lovelace" && next.assets[unit] === 1n,
          ),
        };
      });

    // Cancel from the first three physical steps, on production threads.
    const cancelAt01 = await h.init(
      setup.fraudulentBlockOutRef,
      setup.headerHash,
    );
    record(
      "accepted-cancel-step01",
      maximum.label,
      (await h.cancel(h.threadOf(cancelAt01.result), 0)).measurement,
    );
    const cancelAt02 = await h.step01Accepted(
      (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
      finding,
      maximumInclusion,
      setup.fraudulentBlockOutRef,
    );
    record(
      "accepted-cancel-step02",
      maximum.label,
      (await h.cancel(cancelAt02.result.nextThreadOutRef, 1)).measurement,
    );
    const cancelAt03 = await h.step02(
      (
        await h.step01Accepted(
          (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
          finding,
          maximumInclusion,
          setup.fraudulentBlockOutRef,
        )
      ).result.nextThreadOutRef,
      evidence,
      maximum,
      staged,
    );
    record(
      "accepted-cancel-step03",
      maximum.label,
      (await h.cancel(cancelAt03.result.nextThreadOutRef, 2)).measurement,
    );

    // The restarted thread is driven by the production actuator: the same
    // artifact a fresh process would admit from its journal.
    const restarted = await h.init(
      setup.fraudulentBlockOutRef,
      setup.headerHash,
    );
    record("accepted-init", maximum.label, restarted.measurement);
    const step01 = await drive(
      {
        stage: "step_01",
        threadOutRef: h.threadOf(restarted.result),
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      },
      h.contracts.steps[1].spendingScriptAddress,
    );
    record("accepted-step01", maximum.label, step01.measurement);
    const step02 = await drive(
      {
        stage: "step_02",
        threadOutRef: step01.result.nextThreadOutRef,
        action: { kind: "authenticate" },
      },
      h.contracts.steps[2].spendingScriptAddress,
    );
    record("accepted-step02", maximum.label, step02.measurement);
    let cursor = step02.result.nextThreadOutRef;
    for (let ordinal = 0; ordinal < staged.walk.length; ordinal += 1) {
      const terminal = ordinal === staged.walk.length - 1;
      const scanned = await drive(
        { stage: "step_03", threadOutRef: cursor, walkOrdinal: ordinal },
        h.contracts.steps[terminal ? 3 : 2].spendingScriptAddress,
      );
      record(
        scanLabel("accepted", ordinal),
        maximum.label,
        scanned.measurement,
      );
      cursor = scanned.result.nextThreadOutRef;
    }
    coverage.resumed();
    const proven = await drive(
      { stage: "step_04", threadOutRef: cursor },
      h.harness.contracts.fraudProof.spendingScriptAddress,
    );
    expect(proven.result.fraudProofUnit).toBeTruthy();
    record("accepted-step04-proof-mint", maximum.label, proven.measurement);
    coverage.reason(REASON_ARM, "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");
    coverage.scenario("maximum_supported_evidence");

    const seam = async (name: Seam, build: () => Promise<unknown>) => {
      await expectOnchainRefusal(build);
      coverage.seamMutated(name);
    };

    // Step-01 seam: the transaction's membership in the committed block.
    const membershipThread = await h.init(
      setup.fraudulentBlockOutRef,
      setup.headerHash,
    );
    await seam("tx_membership", () =>
      h.step01Accepted(
        membershipThread.result,
        finding,
        { ...maximumInclusion, transactionsPhasRoot: "ff".repeat(32) },
        setup.fraudulentBlockOutRef,
      ),
    );
    await h.cancel(h.threadOf(membershipThread.result), 0);

    // Step-02 seams against one bound thread; a refused spend leaves it bound.
    const seamThread = (
      await h.step01Accepted(
        (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
        finding,
        maximumInclusion,
        setup.fraudulentBlockOutRef,
      )
    ).result.nextThreadOutRef;
    await seam("native_tx_source", () =>
      h.step02Raw(seamThread, evidence, maximum, staged, (opening) =>
        mutateCompactSource(opening, compactCborHex(honest.nativeTx)),
      ),
    );
    await seam("field_certificate", () =>
      h.step02Raw(seamThread, evidence, maximum, staged, (opening) =>
        mutateCertifiedCarriage(opening, (carriage) => ({
          ...carriage,
          cert_ref_input_index: carriage.chunk_ref_input_indices[0]!,
        })),
      ),
    );
    await seam("field_chunks", () =>
      h.step02Raw(seamThread, evidence, maximum, staged, (opening) =>
        mutateCertifiedCarriage(opening, (carriage) => ({
          ...carriage,
          chunk_ref_input_indices: [
            ...carriage.chunk_ref_input_indices,
          ].reverse(),
        })),
      ),
    );
    await h.cancel(seamThread, 1);

    // Step-03 seams against a thread that already holds one real checkpoint:
    // the resume is what every mutation below must break.
    const scanThread = (
      await h.step03(
        (
          await h.step02(
            (
              await h.step01Accepted(
                (await h.init(setup.fraudulentBlockOutRef, setup.headerHash))
                  .result,
                finding,
                maximumInclusion,
                setup.fraudulentBlockOutRef,
              )
            ).result.nextThreadOutRef,
            evidence,
            maximum,
            staged,
          )
        ).result.nextThreadOutRef,
        evidence,
        maximum,
        staged,
        0,
      )
    ).result.nextThreadOutRef;
    await seam("scan_budget", () =>
      h.step03Raw(scanThread, evidence, maximum, staged, 1, {
        itemBudget: BigInt(OBSERVER_ORDER_INVALID_ITEM_BUDGET + 1),
      }),
    );
    // Checkpoint bytes that do not hash to the committed checkpoint.
    const priorBytes = encodeObserverOrderWalkCheckpoint(staged.walk[0]!);
    priorBytes[priorBytes.length - 1] = priorBytes[priorBytes.length - 1]! ^ 1;
    await seam("scan_checkpoint", () =>
      h.step03Raw(scanThread, evidence, maximum, staged, 1, {
        checkpointBytesHex: priorBytes.toString("hex"),
      }),
    );
    // A successor accumulator the engine never produced.
    const second = staged.walk[1]!;
    await seam("scan_successor_state", () =>
      h.step03Raw(scanThread, evidence, maximum, staged, 1, {
        successor: {
          kind: "scan",
          checkpointHash: hashObserverOrderWalkCheckpoint(second),
          seen: BigInt(second.nextItemIndex + 1),
          previousObserver: observerAt(second.nextItemIndex).toString("hex"),
        },
      }),
    );
    // The right scanning state sent to the terminal script.
    await seam("scan_wrong_successor", () =>
      h.step03Raw(scanThread, evidence, maximum, staged, 1, {
        nextStepIndex: 3,
      }),
    );
    // Deciding before the walk reaches the cited ordinal.
    await seam("premature_decision", () =>
      h.step03Raw(scanThread, evidence, maximum, staged, 1, {
        successor: { kind: "decision", violation: true },
      }),
    );
    await h.cancel(scanThread, 2);

    // Honest accepted block: a strictly ordered field cited at ordinal 1.
    // Step 01 binds it, the scan decides `ordered`, and the terminal step
    // must refuse to convict.
    const honestFinding = acceptedFinding(honest, 1);
    const honestEvidence = evidenceOf(honest, honestFinding);
    const honestStaged = stagedOf(honest, 1);
    expect(honestEvidence.violation).toBe(false);
    expect(observerOrderInvalidEvidenceCloses(honestEvidence)).toBe(false);
    await h.publishField(honest);
    const honestBound = (
      await h.step01Accepted(
        (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
        honestFinding,
        inclusions[1]!,
        setup.fraudulentBlockOutRef,
      )
    ).result.nextThreadOutRef;
    // A published small field rides a RawUtxo carriage: naming the next
    // reference input (the step's own reference script) as the carriage
    // substitutes the bytes the door commits against the body.
    await seam("field_raw_utxo", () =>
      h.step02Raw(
        honestBound,
        honestEvidence,
        honest,
        honestStaged,
        (opening) => mutateRawUtxoCarriage(opening, 1n),
      ),
    );
    const honestOpened = await h.step02(
      honestBound,
      honestEvidence,
      honest,
      honestStaged,
    );
    const honestDecided = await h.step03(
      honestOpened.result.nextThreadOutRef,
      honestEvidence,
      honest,
      honestStaged,
      0,
    );
    await expectOnchainRefusal(() =>
      h.step04Raw(honestDecided.result.nextThreadOutRef),
    );
    coverage.scenario("honest_accepted_block_refusal");
    record(
      "accepted-cancel-step04",
      honest.label,
      (await h.cancel(honestDecided.result.nextThreadOutRef, 3)).measurement,
    );
    // Removal last, through the actuator's mutation-leased stage: it consumes
    // the fraudulent block every thread above bound.
    const removalActuator = actuator(await h.removalDeploymentInfo());
    vi.setSystemTime(h.harness.emulator.now());
    const removal = await captureEmulatorSubmission(
      h.harness.emulator,
      async () => {
        const captured = await removalActuator.capture({
          action: {
            stage: "remove",
            stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
            nextRemovalOutRef: setup.fraudulentBlockOutRef,
            fraudProofOutRef: proven.result.nextThreadOutRef,
          },
          artifact,
        });
        const txHash = await submitCapturedTransaction(captured.transaction);
        expect(txHash).toBe(captured.transaction.txHash);
        await h.harness.proverLucid.awaitTx(txHash);
        return { txHash };
      },
    );
    record("accepted-remove", maximum.label, removal.measurement);
    coverage.scenario("permanent_proof_token_and_descendant_removal");
  }, 1_800_000);

  it("convicts an accepted field whose first adjacent pair descends", async () => {
    await acceptedSuccess("accepted-first", firstPairDescendingShape(), 1);
  }, 900_000);

  it("convicts an accepted field with a duplicate at a middle ordinal", async () => {
    await acceptedSuccess("accepted-duplicate", middleDuplicateShape(), 2);
  }, 900_000);

  it("contradicts a wrongful forced rejection of the maximum ordered field at its last ordinal, refusing every forced-door seam and the flipped decision first", async () => {
    await forcedSuccess(
      "forced-maximum",
      maximumOrderedShape(),
      LAST_ORDINAL,
      async (h) => {
        const {
          setup,
          leaf,
          sourceKey,
          finding,
          source,
          header,
          shape,
          evidence,
          staged,
        } = h;
        const thread = h.threadOf(
          (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
        );
        const door = async (
          seam: Seam,
          mutatedFinding: ObserverOrderInvalidFinding,
          patch: Partial<typeof source>,
        ) => {
          await expectOnchainRefusal(() =>
            h.step01Forced(thread, mutatedFinding, { ...source, ...patch }),
          );
          coverage.seamMutated(seam);
        };
        await door("forced_leaf_header", finding, {
          header: { ...header, validationTracesRoot: "ff".repeat(32) },
        });
        await door("forced_leaf_membership", finding, {
          membership: { ...leaf.membership, root: "ee".repeat(32) },
        });
        // The subject is bound from the authenticated leaf, never the finding.
        await door(
          "forced_subject_transaction",
          {
            ...finding,
            subject: forcedVerdictSubject({
              transactionId: "dd".repeat(32),
              sourceKey,
              rejectionReason: reasonAt(LAST_ORDINAL),
            }),
          },
          {},
        );
        // The leaf names ordinal 1091; a finding naming 1090 carries a
        // consistent reason of its own and is refused only by the exact
        // typed-reason binding.
        await door(
          "forced_reason_coordinate",
          forcedFinding(leaf, sourceKey, LAST_ORDINAL - 1),
          {},
        );
        coverage.scenario("reason_or_subject_coordinate_mutation");
        await door(
          "forced_direction",
          { ...finding, subject: acceptedVerdictSubject(leaf.transactionId) },
          { direction: 0n },
        );
        await expectOnchainRefusal(() =>
          h.step01ForcedRaw(thread, finding, source, 0),
        );
        coverage.seamMutated("successor_script");
        await h.cancel(thread, 0);

        // The terminal scan must carry the engine's decision, not the
        // prover's: flipping it to `violation` is refused on chain.
        const bound = await h.step01Forced(
          h.threadOf(
            (await h.init(setup.fraudulentBlockOutRef, setup.headerHash))
              .result,
          ),
          finding,
          source,
        );
        const opened = await h.step02(
          bound.result.nextThreadOutRef,
          evidence,
          shape,
          staged,
        );
        let cursor = opened.result.nextThreadOutRef;
        for (let ordinal = 0; ordinal < staged.walk.length - 1; ordinal += 1)
          cursor = (await h.step03(cursor, evidence, shape, staged, ordinal))
            .result.nextThreadOutRef;
        await expectOnchainRefusal(() =>
          h.step03Raw(cursor, evidence, shape, staged, staged.walk.length - 1, {
            successor: { kind: "decision", violation: true },
          }),
        );
        coverage.seamMutated("decision_polarity");
        await h.cancel(cursor, 2);
      },
    );
  }, 1_800_000);

  it("contradicts a wrongful forced rejection at a middle ordinal of a small ordered field", async () => {
    await forcedSuccess("forced-middle", smallOrderedShape(), 2);
  }, 900_000);

  it("contradicts a wrongful forced rejection naming an ordinal past the field's end", async () => {
    await forcedSuccess("forced-past-end", twoOrderedShape(), 7);
  }, 900_000);

  it("contradicts a wrongful forced rejection of the empty observer field", async () => {
    await forcedSuccess("forced-empty", emptyShape(), 1);
  }, 900_000);

  it("contradicts a wrongful forced rejection naming ordinal 0", async () => {
    await forcedSuccess("forced-zero", singleShape(), 0);
  }, 900_000);

  it("refuses to contradict an honest forced rejection of a duplicate observer", async () => {
    const shape = duplicateFirstShape();
    const h = await makeHarness();
    const { setup, finding, source } = await forcedBlock(h, shape, 1);
    const evidence = evidenceOf(shape, finding);
    const staged = stagedOf(shape, 1);
    expect(evidence.violation).toBe(true);
    expect(observerOrderInvalidEvidenceCloses(evidence)).toBe(false);
    const bound = await h.step01Forced(
      h.threadOf(
        (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
      ),
      finding,
      source,
    );
    await h.publishField(shape);
    const opened = await h.step02(
      bound.result.nextThreadOutRef,
      evidence,
      shape,
      staged,
    );
    const decided = await h.step03(
      opened.result.nextThreadOutRef,
      evidence,
      shape,
      staged,
      0,
    );
    await expectOnchainRefusal(() =>
      h.step04Raw(decided.result.nextThreadOutRef),
    );
    coverage.scenario("honest_forced_rejection_refusal");
    await h.cancel(decided.result.nextThreadOutRef, 3);
  }, 900_000);

  it("refuses to walk past an earlier offending pair toward a later cited ordinal", async () => {
    // The leaf names ordinal 2 of a field whose ordinal 1 already descends:
    // the rejection is inexact, the transaction is invalid, and the family
    // must not convict. The off-chain twin refuses to prepare evidence; the
    // hand-built plan reaches the scan validator, which refuses at item 1.
    const shape = earlierViolationShape();
    const h = await makeHarness();
    const { setup, finding, source } = await forcedBlock(h, shape, 2);
    expect(() => evidenceOf(shape, finding)).toThrow(/earlier/u);
    const evidence: ObserverOrderInvalidEvidence = {
      ...classifyObserverOrderInvalidFinding(finding),
      violation: false,
      previousObserverHex: observerAt(0).toString("hex"),
      observerHex: observerAt(2).toString("hex"),
      fieldPreimageHex: Buffer.from(shape.fieldPreimage).toString("hex"),
      fieldCommitmentHex: midgardFieldCommitment(shape.fieldPreimage).toString(
        "hex",
      ),
      carriage: "Inline",
    };
    // Ordinal 1 is a legitimate plan over the same field; only the walk
    // length differs from the cited ordinal's.
    const base = stagedOf(shape, 1);
    const staged: ObserverOrderInvalidStagedPlan = {
      ...base,
      walk: [
        {
          ...advanceMissingNativeScriptTxSemanticCheckpoint({
            checkpoint: { ...base.initialWalk, fieldIndex: 6 },
            txId: transactionIdOf(shape),
            items: base.items,
            budget: 3,
          }),
          fieldIndex: 3 as const,
        },
      ],
      violation: false,
    };
    const bound = await h.step01Forced(
      h.threadOf(
        (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
      ),
      finding,
      source,
    );
    await h.publishField(shape);
    const opened = await h.threadAfter(
      await h.step02Raw(bound.result.nextThreadOutRef, evidence, shape, staged),
      h.contracts.steps[2].spendingScriptAddress,
    );
    await expectOnchainRefusal(() =>
      h.step03Raw(opened, evidence, shape, staged, 0, {
        successor: { kind: "decision", violation: false },
      }),
    );
    await h.cancel(opened, 2);
  }, 900_000);

  it("refuses to bind a forced rejection whose authenticated leaf carries the sibling observer reason", async () => {
    const shape = smallOrderedShape();
    const h = await makeHarness();
    // The leaf is typed ObserversForbiddenOnUntaggedNetwork, the other reason
    // the machine maps to the same rejection code; the prover claims this
    // family's coordinate for the same transaction.
    const { setup, finding, source } = await forcedBlock(
      h,
      shape,
      2,
      "ObserversForbiddenOnUntaggedNetwork",
    );
    const thread = h.threadOf(
      (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
    );
    await expectOnchainRefusal(() => h.step01Forced(thread, finding, source));
    coverage.seamMutated("forced_leaf_reason");
    coverage.scenario("reason_or_subject_coordinate_mutation");
    await h.cancel(thread, 0);
  }, 600_000);

  it("closes the coverage gate and the Van Rossem fit ledger", async () => {
    assertCompleteLifecycleCoverage({
      coverage: coverage.snapshot(),
      expectedReasonArms: [REASON_ARM],
      authenticationSeams: [...AUTHENTICATION_SEAMS],
      cancellablePhysicalSteps: [...CANCELLABLE_STEPS],
      resumable: true,
      // The decisive predicate has no numeric bound of its own. The only
      // bound on field 3 is the §5.4 aggregate field bound, which makes the
      // adjacent 1,093-observer field unencodable (asserted above) and is
      // owned by the shared field door and its own families.
      hasAdjacentConsensusBound: false,
    });
    const blueprintBytes = readFileSync(realBlueprintPath);
    const preamble = JSON.parse(blueprintBytes.toString("utf8")) as {
      readonly preamble?: { readonly compiler?: { readonly version?: string } };
    };
    const ledger = buildVanRossemFitLedger({
      category: `observerOrderInvalid:${CATEGORY_ID}:testnet`,
      blueprintSha256: createHash("sha256")
        .update(blueprintBytes)
        .digest("hex"),
      compilerVersion: `aiken ${preamble.preamble?.compiler?.version ?? "unknown"}`,
      measurements,
    });
    for (const entry of ledger.entries) {
      expect(entry.signedByteMargin, entry.name).toBeGreaterThan(0);
      expect(BigInt(entry.memoryUnitMargin), entry.name).toBeGreaterThan(0n);
      expect(BigInt(entry.cpuUnitMargin), entry.name).toBeGreaterThan(0n);
      if (entry.kind === "publication")
        expect(
          entry.publicationReserveMargin,
          entry.name,
        ).toBeGreaterThanOrEqual(0);
    }
    if (process.env.MIDGARD_WRITE_FIT_LEDGER === "1") {
      await writeVanRossemFitLedger(ledgerPath, ledger);
      console.info(`[observer-order-invalid-fit-ledger] wrote ${ledgerPath}`);
    }
    console.info(
      `[observer-order-invalid-fit-ledger] ${JSON.stringify(ledger.entries)}`,
    );
  });
});
