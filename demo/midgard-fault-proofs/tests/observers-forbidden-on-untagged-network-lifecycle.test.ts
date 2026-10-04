import "node:crypto";
import "node:fs";
import "node:url";
import "@al-ft/midgard-core";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/committed-field-shape/submit-committed-field-shape-init.js";
import "../src/field-opening.js";
import "../src/observers-forbidden-on-untagged-network/actuator.js";
import "../src/observers-forbidden-on-untagged-network/artifact.js";
import "../src/observers-forbidden-on-untagged-network/contracts.js";
import "../src/observers-forbidden-on-untagged-network/family.js";
import "../src/observers-forbidden-on-untagged-network/submit-cancel.js";
import "../src/observers-forbidden-on-untagged-network/submit-step-01.js";
import "../src/observers-forbidden-on-untagged-network/submit-step-02.js";
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
import "./support/observers-forbidden-on-untagged-network-raw.js";
import "./support/submit-init-emulator-fixtures.js";
import "./support/submit-init-emulator-shared.js";
import "./observers-forbidden-on-untagged-network-lifecycle.record.js";
import "./observers-forbidden-on-untagged-network-lifecycle.make-harness.js";
import "./observers-forbidden-on-untagged-network-lifecycle.forced-block.js";

import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";

import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { describe, expect, it, vi } from "vitest";

import { createObserversForbiddenActuator } from "../src/observers-forbidden-on-untagged-network/actuator.js";
import { buildObserversForbiddenArtifact } from "../src/observers-forbidden-on-untagged-network/artifact.js";
import {
  observersForbiddenEvidenceCloses,
  type ObserversForbiddenFinding,
} from "../src/observers-forbidden-on-untagged-network/family.js";
import {
  buildVanRossemFitLedger,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { assertCompleteLifecycleCoverage } from "../src/testing/complete-lifecycle.js";
import { submitCapturedTransaction } from "../src/workflow/transaction-boundary.js";
import {
  acceptedBlock,
  acceptedEvidence,
  acceptedFinding,
  emptyUntaggedShape,
  forcedBlock,
  forcedFinding,
  forcedSuccess,
  honestForcedShape,
  maximumTaggedShape,
  maximumUntaggedShape,
  nativeOnlyShape,
} from "./observers-forbidden-on-untagged-network-lifecycle.forced-block.js";
import { makeHarness } from "./observers-forbidden-on-untagged-network-lifecycle.make-harness.js";
import {
  AUTHENTICATION_SEAMS,
  CANCELLABLE_STEPS,
  CATEGORY_ID,
  coverage,
  ledgerPath,
  MAXIMUM_FIELD_BYTES,
  MAXIMUM_OBSERVERS,
  measurements,
  network,
  REASON_ARM,
  record,
} from "./observers-forbidden-on-untagged-network-lifecycle.record.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import {
  compactCborHex,
  mutateCertifiedCarriage,
  mutateCompactSource,
  mutateRawUtxoCarriage,
  transactionIdOf,
  witnessSetCompactCborHex,
} from "./support/observers-forbidden-on-untagged-network-raw.js";

describe("observersForbiddenOnUntaggedNetwork registered-chain lifecycle", () => {
  it("convicts the maximum accepted observer field on scalar 255 through the production actuator, cancels both steps, refuses every accepted seam and both honest accepted polarities, then mints and removes", async () => {
    const h = await makeHarness();
    const maximum = maximumUntaggedShape();
    const honestNative = nativeOnlyShape();
    const honestEmpty = emptyUntaggedShape();
    const { setup, inclusions } = await acceptedBlock(h, [
      maximum,
      honestNative,
      honestEmpty,
    ]);
    const maximumInclusion = inclusions[0]!;
    const evidence = acceptedEvidence(maximum);
    expect(evidence.observerFieldPreimageCbor).toHaveLength(
      MAXIMUM_FIELD_BYTES * 2,
    );
    expect(evidence.observerCount).toBe(MAXIMUM_OBSERVERS);
    expect(evidence.carriage).toBe("Certified");
    expect(observersForbiddenEvidenceCloses(evidence)).toBe(true);

    const published = await h.publishField(maximum);
    h.recordCarriage("accepted", maximum, published);
    const artifact = buildObserversForbiddenArtifact({
      headerHash: setup.headerHash,
      detectionId: `${transactionIdOf(maximum)}:accepted`,
      position: 0n,
      evidence,
      nativeTxCompactCbor: maximumInclusion.nativeTxCompactCbor,
      witnessSetCompactCbor: witnessSetCompactCborHex(maximum.nativeTx),
      l2TransactionSourceCbor: maximumInclusion.l2TransactionSourceCbor,
      transactionsPhasRoot: maximumInclusion.transactionsPhasRoot,
      transactionMembershipCbor: maximumInclusion.txMembershipProofCbor,
    });
    const actuator = (deploymentInfo: unknown) =>
      createObserversForbiddenActuator({
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
            token: "observers-forbidden-emulator",
            source: "emulator",
            renew: async () => {},
            release: async () => {},
            fail: async () => {},
          }),
        },
      });
    const proofActuator = actuator({});

    // Cancel from every nonterminal physical step.
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
      evidence,
      maximumInclusion,
      setup.fraudulentBlockOutRef,
    );
    record(
      "accepted-cancel-step02",
      maximum.label,
      (await h.cancel(cancelAt02.result.nextThreadOutRef, 1)).measurement,
    );

    // The restarted thread is driven by the production actuator: the same
    // artifact a fresh process would admit from its journal.
    const restarted = await h.init(
      setup.fraudulentBlockOutRef,
      setup.headerHash,
    );
    record("accepted-init", maximum.label, restarted.measurement);
    const step01 = await captureEmulatorSubmission(
      h.harness.emulator,
      async () => {
        const captured = await proofActuator.capture({
          action: {
            stage: "step_01",
            threadOutRef: h.threadOf(restarted.result),
            stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
          },
          artifact,
        });
        const txHash = await submitCapturedTransaction(captured.transaction);
        expect(txHash).toBe(captured.transaction.txHash);
        await h.harness.proverLucid.awaitTx(txHash);
        const next = (
          await h.harness.proverLucid.utxosAt(
            h.contracts.steps[1].spendingScriptAddress,
          )
        ).find((utxo) => utxo.txHash === txHash);
        if (next === undefined)
          throw new Error("actuator step-01 output absent");
        return {
          txHash,
          nextThreadOutRef: `${next.txHash}#${next.outputIndex.toString()}`,
        };
      },
    );
    record("accepted-step01", maximum.label, step01.measurement);
    const proven = await captureEmulatorSubmission(
      h.harness.emulator,
      async () => {
        const captured = await proofActuator.capture({
          action: {
            stage: "step_02",
            threadOutRef: step01.result.nextThreadOutRef,
          },
          artifact,
        });
        const txHash = await submitCapturedTransaction(captured.transaction);
        expect(txHash).toBe(captured.transaction.txHash);
        await h.harness.proverLucid.awaitTx(txHash);
        const proof = (
          await h.harness.proverLucid.utxosAt(
            h.harness.contracts.fraudProof.spendingScriptAddress,
          )
        ).find((utxo) => utxo.txHash === txHash);
        if (proof === undefined)
          throw new Error("actuator proof output absent");
        return {
          txHash,
          fraudProofOutRef: `${proof.txHash}#${proof.outputIndex.toString()}`,
          fraudProofUnit: Object.keys(proof.assets).find(
            (unit) => unit !== "lovelace" && proof.assets[unit] === 1n,
          ),
        };
      },
    );
    expect(proven.result.fraudProofUnit).toBeTruthy();
    record("accepted-step02-proof-mint", maximum.label, proven.measurement);
    coverage.reason(REASON_ARM, "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");
    coverage.scenario("maximum_supported_evidence");

    // Step-01 seam: the transaction's membership in the committed block.
    const membershipThread = await h.init(
      setup.fraudulentBlockOutRef,
      setup.headerHash,
    );
    await expectOnchainRefusal(() =>
      h.step01Accepted(
        membershipThread.result,
        evidence,
        { ...maximumInclusion, transactionsPhasRoot: "ff".repeat(32) },
        setup.fraudulentBlockOutRef,
      ),
    );
    coverage.seamMutated("tx_membership");
    await h.cancel(h.threadOf(membershipThread.result), 0);

    // Step-01 seam: the bound scalar must be the compact body's, not the
    // prover's. Scalar 1 is a canonical value, so only the validator refuses.
    const scalarThread = await h.init(
      setup.fraudulentBlockOutRef,
      setup.headerHash,
    );
    await expectOnchainRefusal(() =>
      h.step01Accepted(
        scalarThread.result,
        acceptedFinding(maximum, { networkId: 1 }),
        maximumInclusion,
        setup.fraudulentBlockOutRef,
      ),
    );
    coverage.seamMutated("accepted_network_scalar");
    coverage.scenario("reason_or_subject_coordinate_mutation");
    await h.cancel(h.threadOf(scalarThread.result), 0);

    // Step-02 seams against one bound thread; a refused spend leaves it bound.
    const seamThread = (
      await h.step01Accepted(
        (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
        evidence,
        maximumInclusion,
        setup.fraudulentBlockOutRef,
      )
    ).result.nextThreadOutRef;
    await expectOnchainRefusal(() =>
      h.step02Raw(seamThread, evidence, maximum, (opening) =>
        mutateCompactSource(opening, compactCborHex(honestNative.nativeTx)),
      ),
    );
    coverage.seamMutated("native_tx_source");
    await expectOnchainRefusal(() =>
      h.step02Raw(seamThread, evidence, maximum, (opening) =>
        mutateCertifiedCarriage(opening, (carriage) => ({
          ...carriage,
          cert_ref_input_index: carriage.chunk_ref_input_indices[0]!,
        })),
      ),
    );
    coverage.seamMutated("field_certificate");
    await expectOnchainRefusal(() =>
      h.step02Raw(seamThread, evidence, maximum, (opening) =>
        mutateCertifiedCarriage(opening, (carriage) => ({
          ...carriage,
          chunk_ref_input_indices: [
            ...carriage.chunk_ref_input_indices,
          ].reverse(),
        })),
      ),
    );
    coverage.seamMutated("field_chunks");
    await h.cancel(seamThread, 1);

    // Honest accepted block, native-only polarity: observers on scalar 255
    // under the absent integrity hash need no Plutus evaluation, so the
    // machine accepts the transaction. Step 01 binds it; the terminal step
    // must refuse to convict.
    const honestNativeEvidence = acceptedEvidence(honestNative);
    expect(honestNativeEvidence.carriage).toBe("Inline");
    expect(observersForbiddenEvidenceCloses(honestNativeEvidence)).toBe(false);
    await h.publishField(honestNative);
    const honestNativeThread = (
      await h.step01Accepted(
        (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
        honestNativeEvidence,
        inclusions[1]!,
        setup.fraudulentBlockOutRef,
      )
    ).result.nextThreadOutRef;
    await expectOnchainRefusal(() =>
      h.step02Raw(honestNativeThread, honestNativeEvidence, honestNative),
    );
    coverage.scenario("honest_accepted_block_refusal");
    await h.cancel(honestNativeThread, 1);

    // Honest accepted block, empty polarity: no observers on scalar 255.
    const honestEmptyEvidence = acceptedEvidence(honestEmpty);
    expect(honestEmptyEvidence.observerCount).toBe(0);
    expect(observersForbiddenEvidenceCloses(honestEmptyEvidence)).toBe(false);
    await h.publishField(honestEmpty);
    const honestEmptyThread = (
      await h.step01Accepted(
        (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
        honestEmptyEvidence,
        inclusions[2]!,
        setup.fraudulentBlockOutRef,
      )
    ).result.nextThreadOutRef;
    await expectOnchainRefusal(() =>
      h.step02Raw(honestEmptyThread, honestEmptyEvidence, honestEmpty),
    );
    await h.cancel(honestEmptyThread, 1);
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
            fraudProofOutRef: proven.result.fraudProofOutRef,
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
  }, 900_000);

  it("contradicts a wrongful forced rejection of the empty observer field on scalar 255, refusing every forced-door seam and a substituted published carriage first", async () => {
    const shape = emptyUntaggedShape();
    await forcedSuccess("forced-empty", shape, async (h) => {
      const { setup, leaf, sourceKey, finding, evidence, source, header } = h;
      expect(evidence.carriage).toBe("Inline");
      const thread = h.threadOf(
        (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
      );
      const door = async (
        seam: (typeof AUTHENTICATION_SEAMS)[number],
        mutatedFinding: ObserversForbiddenFinding,
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
            rejectionReason: REASON_ARM,
          }),
        },
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

      // A published small field rides a RawUtxo carriage: naming the next
      // reference input (the step's own reference script) as the carriage
      // substitutes the bytes the door commits against the body.
      const bytesThread = (
        await h.step01Forced(
          h.threadOf(
            (await h.init(setup.fraudulentBlockOutRef, setup.headerHash))
              .result,
          ),
          evidence,
          source,
        )
      ).result.nextThreadOutRef;
      await expectOnchainRefusal(() =>
        h.step02Raw(bytesThread, evidence, shape, (opening) =>
          mutateRawUtxoCarriage(opening, 1n),
        ),
      );
      coverage.seamMutated("field_raw_utxo");
      await h.cancel(bytesThread, 1);
    });
  }, 900_000);

  it("contradicts a wrongful forced rejection of the maximum observer field on a tagged scalar", async () => {
    await forcedSuccess("forced-tagged", maximumTaggedShape());
  }, 900_000);

  it("contradicts a wrongful forced rejection of a native-only transaction carrying observers on scalar 255", async () => {
    await forcedSuccess("forced-native", nativeOnlyShape());
  }, 900_000);

  it("refuses to contradict an honest forced rejection of observers on scalar 255 under a present integrity hash", async () => {
    const shape = honestForcedShape();
    const h = await makeHarness();
    const { setup, evidence, source } = await forcedBlock(h, shape);
    expect(observersForbiddenEvidenceCloses(evidence)).toBe(false);
    const bound = await h.step01Forced(
      h.threadOf(
        (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
      ),
      evidence,
      source,
    );
    await h.publishField(shape, 1n);
    await expectOnchainRefusal(() =>
      h.step02Raw(bound.result.nextThreadOutRef, evidence, shape),
    );
    coverage.scenario("honest_forced_rejection_refusal");
    await h.cancel(bound.result.nextThreadOutRef, 1);
  }, 900_000);

  it("refuses to bind a forced rejection whose authenticated leaf carries a sibling tx-global reason", async () => {
    const shape = honestForcedShape();
    const h = await makeHarness();
    // The leaf is typed NetworkIdMismatch; the prover claims this family's
    // reason for the same transaction. Both are tx-global with no
    // coordinates, so only the exact typed-reason binding separates them.
    const { setup, leaf, sourceKey, source } = await forcedBlock(
      h,
      shape,
      "NetworkIdMismatch",
    );
    const claimed = forcedFinding(shape, leaf, sourceKey);
    const thread = h.threadOf(
      (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
    );
    await expectOnchainRefusal(() => h.step01Forced(thread, claimed, source));
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
      // Two physical steps: the family closes in one step-02 transaction and
      // declares no checkpoint to resume from.
      resumable: false,
      // The decisive predicate has no numeric bound of its own; the observer
      // count bound belongs to the shared field door and its own families.
      hasAdjacentConsensusBound: false,
    });
    const blueprintBytes = readFileSync(realBlueprintPath);
    const preamble = JSON.parse(blueprintBytes.toString("utf8")) as {
      readonly preamble?: { readonly compiler?: { readonly version?: string } };
    };
    const ledger = buildVanRossemFitLedger({
      category: `observersForbiddenOnUntaggedNetwork:${CATEGORY_ID}:testnet`,
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
      console.info(
        `[observers-forbidden-on-untagged-network-fit-ledger] wrote ${ledgerPath}`,
      );
    }
    console.info(
      `[observers-forbidden-on-untagged-network-fit-ledger] ${JSON.stringify(ledger.entries)}`,
    );
  });
});
