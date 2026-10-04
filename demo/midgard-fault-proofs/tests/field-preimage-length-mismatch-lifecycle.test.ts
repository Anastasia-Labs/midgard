import "@aiken-lang/merkle-patricia-forestry";
import "effect";
import "../src/prepare-double-spend.js";
import "../src/remove-fraudulent-block.js";
import "./support/committed-field-shape-emulator.js";
import "./support/emulator/emulator-context.js";
import "./support/emulator/harness.js";
import "./support/emulator/registered-chain.js";
import "./support/emulator/setup-tx.js";
import "./support/lifecycle-coverage.js";
import "./support/measured-fit-ledger.js";
import "./support/submit-init-emulator-shared.js";

import {
  encodeMidgardNativeTxProofFieldLengths,
  planMidgardFieldCarriage,
} from "@al-ft/midgard-core";
import { L2TransactionSourceSchema } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  certifyFaultProofFieldCarriage,
  type FaultProofFieldOpeningPlan,
  faultProofRawFieldCarriage,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import {
  fieldPreimageLengthCommittedClaim,
  prepareAcceptedFieldPreimageLengthMismatch,
} from "../src/field-preimage-length-mismatch/prepare-accepted.js";
import {
  submitFieldPreimageLengthAcceptedAuthentication,
  submitFieldPreimageLengthAcceptedDispatch,
  submitFieldPreimageLengthCancel,
  submitFieldPreimageLengthForcedAuthentication,
  submitFieldPreimageLengthForcedDispatch,
  submitFieldPreimageLengthInit,
  submitFieldPreimageLengthTerminal,
} from "../src/field-preimage-length-mismatch/submit-lucid.js";
import {
  type PreparedFieldPreimageLengthWorkflow,
  prepareFieldPreimageLengthWorkflow,
} from "../src/field-preimage-length-mismatch/workflow.js";
import { parseSubmitStep01TxInclusion } from "../src/step-support.js";
import { assertCompleteLifecycleCoverage } from "../src/testing/complete-lifecycle.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import {
  forcedPrepared,
  forcedSetup,
  inlineBodyClaim,
} from "./field-preimage-length-mismatch-lifecycle.forced-prepared.js";
import {
  coverage,
  emitFit,
  expectOnChainRefusal,
  fitRows,
  flipFirstByte,
  REASON,
  successorSwappedConfig,
  WORKFLOW,
} from "./field-preimage-length-mismatch-lifecycle.registered-contracts.js";
import {
  removeFraudulentBlock,
  setup,
} from "./field-preimage-length-mismatch-lifecycle.setup.js";
import {
  appendFieldRecoverySuccessor,
  createFieldRecoveryRecorder,
  removeThroughSharedFieldRecovery,
} from "./field-preimage-length-mismatch-lifecycle.shared-removal.js";
import { network } from "./support/emulator/blueprints.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import { FIELD_PREIMAGE_LENGTH_FORCED_FIELD_INDEX } from "./support/field-preimage-length-mismatch-forced-fixture.js";
import { countedTransactionsRoot } from "./support/submit-init-emulator-fixtures.js";

describe("field-preimage-length-mismatch registered-chain lifecycle", () => {
  it.each([
    [0, "step-01"],
    [1, "step-02-accepted"],
    [3, "step-03"],
  ] as const)(
    "cancels the accepted path from physical step %s",
    async (stepIndex, physicalStep) => {
      const fixture = await setup();
      if (fixture.acceptedPrepared === undefined)
        throw new Error("missing directly prepared accepted evidence");
      const init = await submitFieldPreimageLengthInit({
        config: fixture.config,
        fraudulentBlockOutRef: fixture.fraudulent.fraudulentBlockOutRef,
      });
      let threadOutRef = init.nextThreadOutRef;
      if (stepIndex >= 1) {
        const dispatch = await submitFieldPreimageLengthAcceptedDispatch({
          config: fixture.config,
          threadOutRef,
          stateQueueBlockOutRef: fixture.fraudulent.fraudulentBlockOutRef,
          inclusion: parseSubmitStep01TxInclusion(
            fixture.acceptedPrepared.inclusion,
          ),
          claim: fixture.acceptedPrepared.claim,
        });
        threadOutRef = dispatch.nextThreadOutRef;
      }
      if (stepIndex === 3) {
        const authentication =
          await submitFieldPreimageLengthAcceptedAuthentication({
            config: fixture.config,
            threadOutRef,
            claim: fixture.acceptedPrepared.claim,
            prepared: fixture.acceptedPrepared.prepared,
          });
        threadOutRef = authentication.nextThreadOutRef;
      }
      const cancel = await captureEmulatorSubmission(
        fixture.harness.emulator,
        () =>
          submitFieldPreimageLengthCancel({
            config: fixture.config,
            threadOutRef,
            stepIndex,
          }),
      );
      emitFit(`accepted-cancel-${physicalStep}`, cancel.measurement);
      coverage.cancelled(physicalStep);
    },
    120_000,
  );

  it("executes certified maximum evidence, refuses a substituted chunk order, and refuses the adjacent actual length", async () => {
    const fixture = await setup({ acceptedPreimageBytes: 32_768 });
    const plan = planMidgardFieldCarriage({
      owner: Buffer.from(fixture.harness.proverSigner.paymentKeyHash, "hex"),
      txId: Buffer.from(fixture.scenario.nativeTxId, "hex"),
      fieldIndex: fixture.scenario.fieldIndex,
      preimage: fixture.scenario.committedPreimage,
      publish: false,
    });
    expect(plan.tier).toBe("Certified");
    const planned = {
      sourceKind: 0n,
      fieldIndex: fixture.scenario.fieldIndex,
      nativeTxId: fixture.scenario.nativeTxId,
      nativeTxCompactCbor: fixture.scenario.inclusion.nativeTxCompactCbor,
      preimage: fixture.scenario.committedPreimage,
      itemCount: 0,
      commitment: Buffer.from(plan.commitment).toString("hex"),
      plan,
    } as FaultProofFieldOpeningPlan;
    const rawPlan = planMidgardFieldCarriage({
      owner: Buffer.from(fixture.harness.proverSigner.paymentKeyHash, "hex"),
      txId: Buffer.from(fixture.scenario.nativeTxId, "hex"),
      fieldIndex: fixture.scenario.fieldIndex,
      preimage: Buffer.alloc(14_337, 0xa4),
      publish: false,
    });
    expect(rawPlan.tier).toBe("RawUtxo");
    const rawPublication = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        publishFaultProofFieldCarriage({
          lucid: fixture.harness.proverLucid,
          signer: fixture.harness.proverSigner,
          planned: {
            ...planned,
            preimage: Buffer.alloc(14_337, 0xa4),
            commitment: Buffer.from(rawPlan.commitment).toString("hex"),
            plan: rawPlan,
          },
          publisherAddress: fixture.harness.proverSigner.address,
          label: "field-preimage-length raw tier boundary",
        }),
    );
    expect(rawPublication.measurements).toHaveLength(1);
    emitFit("raw-utxo-14337-publication", rawPublication.measurement);
    const chunkPublication = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        publishFaultProofFieldCarriage({
          lucid: fixture.harness.proverLucid,
          signer: fixture.harness.proverSigner,
          planned,
          publisherAddress: fixture.harness.proverSigner.address,
          label: "field-preimage-length maximum",
        }),
    );
    const chunks = chunkPublication.result;
    chunkPublication.measurements.forEach((measurement, index) =>
      emitFit(
        `certified-32768-chunk${(index + 1).toString().padStart(2, "0")}`,
        measurement,
      ),
    );
    const certificateReference = (
      await publishPlainReferenceScriptUtxo({
        lucid: fixture.harness.proverLucid,
        script: fixture.config.contracts.fieldPreimageCertificate.mintingScript,
        label: "field-preimage-length certificate mint",
      })
    ).utxo;
    const certificateMint = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        certifyFaultProofFieldCarriage({
          lucid: fixture.harness.proverLucid,
          network,
          signer: fixture.harness.proverSigner,
          planned,
          certificatePolicyId:
            fixture.config.contracts.fieldPreimageCertificate.policyId,
          certificateMintingScript:
            fixture.config.contracts.fieldPreimageCertificate.mintingScript,
          certificateReferenceScriptUtxo: certificateReference,
          chunkUtxos: chunks,
          compactCbor: fixture.scenario.inclusion.nativeTxCompactCbor,
          witnessSetCompactCbor: (
            Data.from(
              fixture.sourceCbor,
              L2TransactionSourceSchema as never,
            ) as {
              source: { witness_set_compact_cbor: string };
            }
          ).source.witness_set_compact_cbor,
        }),
    );
    const certificate = certificateMint.result;
    emitFit("accepted-certified-certificate", certificateMint.measurement);
    const allAuthenticationReferences = [
      fixture.config.referenceScripts.step02Accepted,
      certificate.certificateUtxo,
      ...chunks,
    ];
    const carriage = faultProofRawFieldCarriage({
      plan,
      referenceInputs: allAuthenticationReferences,
      certificatePolicyId:
        fixture.config.contracts.fieldPreimageCertificate.policyId,
      label: "field-preimage-length maximum",
    });
    const direct = await prepareAcceptedFieldPreimageLengthMismatch({
      headerHash: fixture.fraudulent.headerHash,
      committedTransactionsRoot: await countedTransactionsRoot(
        fixture.transactionsRoot,
        1n,
      ),
      l2TransactionCount: 1n,
      entries: [[fixture.scenario.nativeTxId, fixture.sourceCbor]],
      transactionId: fixture.scenario.nativeTxId,
      canonicalTransactionCbor: fixture.canonicalTransactionCbor,
      fieldIndex: fixture.scenario.fieldIndex,
      carriage,
    });
    expect(direct.prepared.actualLength).toBe(32_768);
    expect(direct.prepared.carriage).toBe("Certified");
    const init = await captureEmulatorSubmission(fixture.harness.emulator, () =>
      submitFieldPreimageLengthInit({
        config: fixture.config,
        fraudulentBlockOutRef: fixture.fraudulent.fraudulentBlockOutRef,
      }),
    );
    emitFit("accepted-certified-init", init.measurement);
    const dispatch = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthAcceptedDispatch({
          config: fixture.config,
          threadOutRef: init.result.nextThreadOutRef,
          stateQueueBlockOutRef: fixture.fraudulent.fraudulentBlockOutRef,
          inclusion: parseSubmitStep01TxInclusion(direct.inclusion),
          claim: direct.claim,
        }),
    );
    emitFit("accepted-certified-dispatch", dispatch.measurement);
    // The certificate seam: the same certificate and chunks, concatenated in
    // a substituted order, no longer hash to the committed field.
    if (!("Certified" in carriage))
      throw new Error("maximum carriage is not certified");
    const reordered = [...carriage.Certified.chunk_ref_input_indices];
    [reordered[0], reordered[1]] = [reordered[1]!, reordered[0]!];
    await expectOnChainRefusal(
      () =>
        submitFieldPreimageLengthAcceptedAuthentication({
          config: fixture.config,
          threadOutRef: dispatch.result.nextThreadOutRef,
          claim: {
            BodyFieldClaim: {
              field_index: BigInt(fixture.scenario.fieldIndex),
              carriage: {
                Certified: {
                  ...carriage.Certified,
                  chunk_ref_input_indices: reordered,
                },
              },
            },
          },
          prepared: direct.prepared,
          carriageReferenceInputs: [certificate.certificateUtxo, ...chunks],
        }),
      "reordered certified chunks",
    );
    coverage.seamMutated("field_certificate");
    const authentication = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthAcceptedAuthentication({
          config: fixture.config,
          threadOutRef: dispatch.result.nextThreadOutRef,
          claim: direct.claim,
          prepared: direct.prepared,
          carriageReferenceInputs: [certificate.certificateUtxo, ...chunks],
        }),
    );
    emitFit("accepted-certified-authenticate", authentication.measurement);
    const terminal = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthTerminal({
          config: fixture.config,
          threadOutRef: authentication.result.nextThreadOutRef,
        }),
    );
    emitFit("accepted-certified-final-mint", terminal.measurement);
    const removal = await removeFraudulentBlock(fixture);
    expect(removal.result.fraudCategoryId).toBe("00000020");
    emitFit("accepted-certified-remove", removal.measurement);
    coverage.reason(REASON, "accepted_invalid");
    coverage.scenario("maximum_supported_evidence");
    // The adjacent shape has no admissible carriage: the family refuses it
    // before any transaction exists, and the applied reducer's own bound is
    // exercised by the `authenticated.{..}` Aiken selectors.
    expect(() =>
      prepareFieldPreimageLengthWorkflow({
        headerHash: fixture.fraudulent.headerHash,
        transactionId: fixture.scenario.nativeTxId,
        direction: "wrongfulAcceptance",
        fieldIndex: fixture.scenario.fieldIndex,
        fieldPreimageLengthsCbor: encodeMidgardNativeTxProofFieldLengths([
          32_768, 0, 0, 0, 0, 0, 0, 0, 0,
        ]),
        fieldPreimage: Buffer.alloc(32_769),
      }),
    ).toThrow(/consensus bound/u);
    expect(() =>
      planMidgardFieldCarriage({
        owner: Buffer.from(fixture.harness.proverSigner.paymentKeyHash, "hex"),
        txId: Buffer.from(fixture.scenario.nativeTxId, "hex"),
        fieldIndex: fixture.scenario.fieldIndex,
        preimage: Buffer.alloc(32_769, 0xa5),
        publish: false,
      }),
    ).toThrow();
    coverage.adjacentOverBoundRefused();
  }, 180_000);

  it("starts at generic Init, refuses every mutated accepted seam on chain, convicts the accepted source, mints proof, and removes the descendant chain", async () => {
    const recorder = createFieldRecoveryRecorder();
    const fixture = await setup();
    if (fixture.scenario.canonicalTx === null) {
      throw new Error("accepted fixture is not canonical");
    }
    if (fixture.acceptedPrepared === undefined)
      throw new Error("missing directly prepared accepted evidence");
    const prepared = fixture.acceptedPrepared;
    const claim = prepared.claim;
    const targetOutRef = await appendFieldRecoverySuccessor(fixture);
    const init = await captureEmulatorSubmission(fixture.harness.emulator, () =>
      submitFieldPreimageLengthInit({
        config: fixture.config,
        fraudulentBlockOutRef: targetOutRef,
      }),
    );
    emitFit("accepted-init", init.measurement);
    const inclusion = parseSubmitStep01TxInclusion(prepared.inclusion);
    // Transaction-membership seam: the same transaction re-keyed under the
    // honest length vector is not the leaf the header's PHAS commits.
    await expectOnChainRefusal(
      () =>
        submitFieldPreimageLengthAcceptedDispatch({
          config: fixture.config,
          threadOutRef: init.result.nextThreadOutRef,
          stateQueueBlockOutRef: targetOutRef,
          inclusion: {
            ...inclusion,
            l2TransactionSourceCbor: fixture.scenario.substitutedSourceCbor,
          },
          claim,
        }),
      "substituted source leaf",
    );
    coverage.seamMutated("tx_membership");
    // Successor seam: step 01 fixes the accepted authenticator's applied hash.
    await expectOnChainRefusal(
      () =>
        submitFieldPreimageLengthAcceptedDispatch({
          config: successorSwappedConfig(fixture.config),
          threadOutRef: init.result.nextThreadOutRef,
          stateQueueBlockOutRef: targetOutRef,
          inclusion,
          claim,
        }),
      "forced authenticator as the accepted successor",
    );
    coverage.seamMutated("successor_script");
    const dispatch = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthAcceptedDispatch({
          config: fixture.config,
          threadOutRef: init.result.nextThreadOutRef,
          stateQueueBlockOutRef: targetOutRef,
          inclusion,
          claim,
        }),
    );
    emitFit("accepted-dispatch", dispatch.measurement);
    // Field-preimage seam: one flipped byte no longer hashes to the field the
    // committed body carries, even though its length would match the claim.
    await expectOnChainRefusal(
      () =>
        submitFieldPreimageLengthAcceptedAuthentication({
          config: fixture.config,
          threadOutRef: dispatch.result.nextThreadOutRef,
          claim: inlineBodyClaim(
            fixture.scenario.fieldIndex,
            flipFirstByte(fixture.scenario.committedPreimage),
          ),
          prepared: prepared.prepared,
        }),
      "flipped preimage byte",
    );
    coverage.seamMutated("field_preimage");
    // Coordinate mutation: a claim naming field 1 while carrying field 0's
    // bytes opens nothing the body committed at that position.
    await expectOnChainRefusal(
      () =>
        submitFieldPreimageLengthAcceptedAuthentication({
          config: fixture.config,
          threadOutRef: dispatch.result.nextThreadOutRef,
          claim: inlineBodyClaim(1, fixture.scenario.committedPreimage),
          prepared: { ...prepared.prepared, fieldIndex: 1 },
        }),
      "mutated field coordinate",
    );
    // Subject mutation: the terminal state must carry the subject step 01
    // bound, not one the prover names.
    await expectOnChainRefusal(
      () =>
        submitFieldPreimageLengthAcceptedAuthentication({
          config: fixture.config,
          threadOutRef: dispatch.result.nextThreadOutRef,
          claim,
          prepared: { ...prepared.prepared, transactionId: "ff".repeat(32) },
        }),
      "mutated subject transaction id",
    );
    coverage.scenario("reason_or_subject_coordinate_mutation");
    const authentication = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthAcceptedAuthentication({
          config: fixture.config,
          threadOutRef: dispatch.result.nextThreadOutRef,
          claim,
          prepared: prepared.prepared,
        }),
    );
    emitFit("accepted-inline-authenticate", authentication.measurement);
    const terminal = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthTerminal({
          config: fixture.config,
          threadOutRef: authentication.result.nextThreadOutRef,
        }),
    );
    emitFit("accepted-inline-final-mint", terminal.measurement);
    const removal = await removeThroughSharedFieldRecovery(fixture, recorder, [
      true,
      false,
      false,
    ]);
    expect(removal.result.transactions.map(({ kind }) => kind)).toEqual([
      "remove-successor",
      "remove-successor",
      "remove-target",
    ]);
    expect(removal.result.fraudCategoryId).toBe("00000020");
    emitFit("accepted-inline-remove", removal.measurement);
    coverage.reason(REASON, "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");
    coverage.scenario("permanent_proof_token_and_descendant_removal");
  }, 180_000);

  it("refuses an honest accepted block at the terminal after authenticating its field on chain", async () => {
    const fixture = await setup({ honestAccepted: true });
    const preimage = fixture.scenario.committedPreimage;
    const declaredLength =
      fixture.scenario.lengths[fixture.scenario.fieldIndex]!;
    expect(declaredLength).toBe(preimage.length);
    const claim = fieldPreimageLengthCommittedClaim({
      fieldIndex: fixture.scenario.fieldIndex,
      witnessSetCompactCbor: fixture.scenario.witnessSetCompactCbor,
      carriage: { Inline: { preimage: preimage.toString("hex") } },
    });
    // The family's own preparer already refuses to build this evidence.
    expect(() =>
      prepareFieldPreimageLengthWorkflow({
        headerHash: fixture.fraudulent.headerHash,
        transactionId: fixture.scenario.nativeTxId,
        direction: "wrongfulAcceptance",
        fieldIndex: fixture.scenario.fieldIndex,
        fieldPreimageLengthsCbor: encodeMidgardNativeTxProofFieldLengths(
          fixture.scenario.lengths,
        ),
        fieldPreimage: preimage,
      }),
    ).toThrow(/does not contradict/u);
    const prepared: PreparedFieldPreimageLengthWorkflow = {
      schemaVersion: WORKFLOW,
      headerHash: fixture.fraudulent.headerHash,
      transactionId: fixture.scenario.nativeTxId,
      direction: "wrongfulAcceptance",
      fieldIndex: fixture.scenario.fieldIndex,
      declaredLength,
      actualLength: preimage.length,
      preimageHex: preimage.toString("hex"),
      carriage: "Inline",
      evidenceDigest: "00".repeat(32),
    };
    const init = await submitFieldPreimageLengthInit({
      config: fixture.config,
      fraudulentBlockOutRef: fixture.fraudulent.fraudulentBlockOutRef,
    });
    const dispatch = await submitFieldPreimageLengthAcceptedDispatch({
      config: fixture.config,
      threadOutRef: init.nextThreadOutRef,
      stateQueueBlockOutRef: fixture.fraudulent.fraudulentBlockOutRef,
      inclusion: parseSubmitStep01TxInclusion(fixture.scenario.inclusion),
      claim,
    });
    // The authenticator has no opinion on polarity: it binds the honest
    // lengths into terminal state, and the terminal rule refuses to close a
    // wrongful-acceptance thread over equal lengths.
    const authentication =
      await submitFieldPreimageLengthAcceptedAuthentication({
        config: fixture.config,
        threadOutRef: dispatch.nextThreadOutRef,
        claim,
        prepared,
      });
    await expectOnChainRefusal(
      () =>
        submitFieldPreimageLengthTerminal({
          config: fixture.config,
          threadOutRef: authentication.nextThreadOutRef,
        }),
      "honest accepted block at the terminal",
    );
    coverage.scenario("honest_accepted_block_refusal");
    const cancel = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthCancel({
          config: fixture.config,
          threadOutRef: authentication.nextThreadOutRef,
          stepIndex: 3,
        }),
    );
    emitFit("honest-accepted-cancel-step-03", cancel.measurement);
    coverage.cancelled("step-03");
  }, 120_000);

  it("starts at generic Init, refuses the mutated forced seams on chain, and resolves wrongful forced rejection", async () => {
    const recorder = createFieldRecoveryRecorder();
    const fixture = await setup({ forced: true });
    const targetOutRef = await appendFieldRecoverySuccessor(fixture);
    if (fixture.forcedFixture === undefined)
      throw new Error("missing forced fixture");
    const forcedFixture = fixture.forcedFixture;
    const init = await captureEmulatorSubmission(fixture.harness.emulator, () =>
      submitFieldPreimageLengthInit({
        config: fixture.config,
        fraudulentBlockOutRef: targetOutRef,
      }),
    );
    emitFit("forced-init", init.measurement);
    const dispatch = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthForcedDispatch({
          config: fixture.config,
          threadOutRef: init.result.nextThreadOutRef,
          direction: 1n,
        }),
    );
    emitFit("forced-dispatch", dispatch.measurement);
    const membership = await buildForcedTransactionLeafMembershipProof({
      reconstruction: forcedFixture.reconstruction,
      eventKey: forcedFixture.eventKey,
    });
    const preimage = Buffer.from(
      forcedFixture.forcedNativeTx.body.spendInputsPreimageCbor,
    );
    const referencePreimage = Buffer.from(
      forcedFixture.forcedNativeTx.body.referenceInputsPreimageCbor,
    );
    const prepared = forcedPrepared({
      headerHash: fixture.fraudulent.headerHash,
      transactionId: forcedFixture.forcedTransaction.tx_id,
      direction: "wrongfulRejection",
      declaredLength: preimage.length,
      preimage,
    });
    const claim = inlineBodyClaim(
      FIELD_PREIMAGE_LENGTH_FORCED_FIELD_INDEX,
      preimage,
    );
    // Forced-leaf seam: a leaf whose committed length vector differs from the
    // one the header's forced-transactions root commits does not open.
    const substitutedLeaf = {
      ...membership,
      value: {
        ...membership.value,
        submitted_source: {
          ...membership.value.submitted_source,
          field_preimage_lengths_cbor: encodeMidgardNativeTxProofFieldLengths([
            preimage.length + 1,
            ...fixture.scenario.honestLengths.slice(1),
          ]).toString("hex"),
        },
      },
    };
    await expectOnChainRefusal(
      () =>
        submitFieldPreimageLengthForcedAuthentication({
          config: fixture.config,
          threadOutRef: dispatch.result.nextThreadOutRef,
          header: forcedFixture.header,
          membership: substitutedLeaf,
          claim,
          prepared: { ...prepared, declaredLength: preimage.length + 1 },
        }),
      "substituted forced leaf",
    );
    coverage.seamMutated("forced_leaf");
    // Reason-coordinate mutation: the leaf rejects field 0; authenticating
    // field 1, with field 1's real bytes, is refused by the exact bind.
    await expectOnChainRefusal(
      () =>
        submitFieldPreimageLengthForcedAuthentication({
          config: fixture.config,
          threadOutRef: dispatch.result.nextThreadOutRef,
          header: forcedFixture.header,
          membership,
          claim: inlineBodyClaim(1, referencePreimage),
          prepared: {
            ...prepared,
            fieldIndex: 1,
            declaredLength: referencePreimage.length,
            actualLength: referencePreimage.length,
            preimageHex: referencePreimage.toString("hex"),
          },
        }),
      "mutated forced reason coordinate",
    );
    coverage.scenario("reason_or_subject_coordinate_mutation");
    const authentication = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthForcedAuthentication({
          config: fixture.config,
          threadOutRef: dispatch.result.nextThreadOutRef,
          header: forcedFixture.header,
          membership,
          claim,
          prepared,
        }),
    );
    emitFit("forced-authenticate", authentication.measurement);
    const terminal = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthTerminal({
          config: fixture.config,
          threadOutRef: authentication.result.nextThreadOutRef,
        }),
    );
    emitFit("forced-final-mint", terminal.measurement);
    const removal = await removeThroughSharedFieldRecovery(fixture, recorder, [
      true,
      false,
      false,
    ]);
    expect(removal.result.transactions.map(({ kind }) => kind)).toEqual([
      "remove-successor",
      "remove-successor",
      "remove-target",
    ]);
    expect(removal.result.fraudCategoryId).toBe("00000020");
    emitFit("forced-remove", removal.measurement);
    coverage.reason(REASON, "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");
    coverage.scenario("permanent_proof_token_and_descendant_removal");
  }, 180_000);

  it("refuses direction 0 against a rejected leaf before construction and cancels the forced authenticator", async () => {
    const fixture = await setup({ forced: true });
    if (fixture.forcedFixture === undefined)
      throw new Error("missing forced fixture");
    const init = await submitFieldPreimageLengthInit({
      config: fixture.config,
      fraudulentBlockOutRef: fixture.fraudulent.fraudulentBlockOutRef,
    });
    const dispatch = await submitFieldPreimageLengthForcedDispatch({
      config: fixture.config,
      threadOutRef: init.nextThreadOutRef,
      direction: 0n,
    });
    const honestMembership = await buildForcedTransactionLeafMembershipProof({
      reconstruction: fixture.forcedFixture.reconstruction,
      eventKey: fixture.forcedFixture.eventKey,
    });
    const honestPreimage = Buffer.from(
      fixture.forcedFixture.forcedNativeTx.body.spendInputsPreimageCbor,
    );
    await expect(
      submitFieldPreimageLengthForcedAuthentication({
        config: fixture.config,
        threadOutRef: dispatch.nextThreadOutRef,
        header: fixture.forcedFixture.header,
        membership: honestMembership,
        claim: inlineBodyClaim(
          FIELD_PREIMAGE_LENGTH_FORCED_FIELD_INDEX,
          honestPreimage,
        ),
        prepared: forcedPrepared({
          headerHash: fixture.fraudulent.headerHash,
          transactionId: fixture.forcedFixture.forcedTransaction.tx_id,
          direction: "wrongfulAcceptance",
          declaredLength: honestPreimage.length,
          preimage: honestPreimage,
        }),
      }),
    ).rejects.toThrow(/forced leaf differs/u);
    const cancel = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthCancel({
          config: fixture.config,
          threadOutRef: dispatch.nextThreadOutRef,
          stepIndex: 2,
        }),
    );
    emitFit("forced-cancel-step-02-forced", cancel.measurement);
    coverage.cancelled("step-02-forced");
  }, 120_000);

  it("refuses an honest forced rejection at the terminal after authenticating the mismatched leaf on chain", async () => {
    // The operator rightly rejected: the leaf's committed vector overstates
    // field 0 by one byte.
    const fixture = await forcedSetup("rejected", true);
    expect(fixture.forced.declaredLength).toBe(
      fixture.forced.preimage.length + 1,
    );
    const init = await submitFieldPreimageLengthInit({
      config: fixture.config,
      fraudulentBlockOutRef: fixture.fraudulent.fraudulentBlockOutRef,
    });
    const dispatch = await submitFieldPreimageLengthForcedDispatch({
      config: fixture.config,
      threadOutRef: init.nextThreadOutRef,
      direction: 1n,
    });
    const authentication = await submitFieldPreimageLengthForcedAuthentication({
      config: fixture.config,
      threadOutRef: dispatch.nextThreadOutRef,
      header: fixture.forced.header,
      membership: fixture.forced.membership,
      claim: inlineBodyClaim(
        FIELD_PREIMAGE_LENGTH_FORCED_FIELD_INDEX,
        fixture.forced.preimage,
      ),
      prepared: forcedPrepared({
        headerHash: fixture.fraudulent.headerHash,
        transactionId: fixture.forced.forcedTransaction.tx_id,
        direction: "wrongfulRejection",
        declaredLength: fixture.forced.declaredLength,
        preimage: fixture.forced.preimage,
      }),
    });
    await expectOnChainRefusal(
      () =>
        submitFieldPreimageLengthTerminal({
          config: fixture.config,
          threadOutRef: authentication.nextThreadOutRef,
        }),
      "honest forced rejection at the terminal",
    );
    coverage.scenario("honest_forced_rejection_refusal");
    const cancel = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthCancel({
          config: fixture.config,
          threadOutRef: authentication.nextThreadOutRef,
          stepIndex: 3,
        }),
    );
    emitFit("honest-forced-cancel-step-03", cancel.measurement);
    coverage.cancelled("step-03");
  }, 120_000);

  it("convicts a wrongfully accepted forced transaction through the forced authenticator", async () => {
    const fixture = await forcedSetup("valid", true);
    const init = await captureEmulatorSubmission(fixture.harness.emulator, () =>
      submitFieldPreimageLengthInit({
        config: fixture.config,
        fraudulentBlockOutRef: fixture.fraudulent.fraudulentBlockOutRef,
      }),
    );
    emitFit("forced-accepted-init", init.measurement);
    const dispatch = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthForcedDispatch({
          config: fixture.config,
          threadOutRef: init.result.nextThreadOutRef,
          direction: 0n,
        }),
    );
    emitFit("forced-accepted-dispatch", dispatch.measurement);
    const authentication = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthForcedAuthentication({
          config: fixture.config,
          threadOutRef: dispatch.result.nextThreadOutRef,
          header: fixture.forced.header,
          membership: fixture.forced.membership,
          claim: inlineBodyClaim(
            FIELD_PREIMAGE_LENGTH_FORCED_FIELD_INDEX,
            fixture.forced.preimage,
          ),
          prepared: forcedPrepared({
            headerHash: fixture.fraudulent.headerHash,
            transactionId: fixture.forced.forcedTransaction.tx_id,
            direction: "wrongfulAcceptance",
            declaredLength: fixture.forced.declaredLength,
            preimage: fixture.forced.preimage,
          }),
        }),
    );
    emitFit("forced-accepted-authenticate", authentication.measurement);
    const terminal = await captureEmulatorSubmission(
      fixture.harness.emulator,
      () =>
        submitFieldPreimageLengthTerminal({
          config: fixture.config,
          threadOutRef: authentication.result.nextThreadOutRef,
        }),
    );
    emitFit("forced-accepted-final-mint", terminal.measurement);
    const removal = await removeFraudulentBlock(fixture);
    expect(removal.result.transactions.map(({ kind }) => kind)).toEqual([
      "remove-target",
    ]);
    emitFit("forced-accepted-remove", removal.measurement);
    coverage.reason(REASON, "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");
  }, 120_000);

  it("declares the complete lifecycle coverage it exercised", () => {
    console.info(
      `[field-preimage-length-fit-rows] ${JSON.stringify(
        fitRows.map(({ stage, measurement }) => ({
          stage,
          signedBytes: measurement.completeSignedBytes,
          memory: measurement.executionMemory.toString(),
          cpu: measurement.executionSteps.toString(),
        })),
      )}`,
    );
    // Recorded while the suites above ran, never pre-filled. The family has
    // no resumable scan: one bounded whole-field opening authenticates the
    // maximum preimage, so no checkpoint exists to resume from.
    assertCompleteLifecycleCoverage({
      coverage: coverage.snapshot(),
      expectedReasonArms: [REASON],
      authenticationSeams: [
        "tx_membership",
        "successor_script",
        "field_preimage",
        "field_certificate",
        "forced_leaf",
      ],
      cancellablePhysicalSteps: [
        "step-01",
        "step-02-accepted",
        "step-02-forced",
        "step-03",
      ],
      resumable: false,
      hasAdjacentConsensusBound: true,
    });
  });
});
