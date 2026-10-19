/**
 * Pure production-workflow coverage for the §5.2 network-id wrongful-rejection
 * (forced) direction: durable artifact round trip, deterministic action
 * selection from journal plus simulated chain state at every physical step,
 * and the classification refusals that keep another family's typed rejection
 * reason out of this thread.
 */
import { fieldPreimagePublicationDatumCbor } from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import {
  hashNetworkIdForcedScanWalkCheckpoint,
  type NetworkIdForcedScanPlan,
  planNetworkIdForcedScan,
} from "../src/network-id/forced-scan-plan.js";
import { planNetworkIdOutputsOpening } from "../src/network-id/prepare.js";
import { submitNetworkIdInit } from "../src/network-id/submit-network-id-init.js";
import {
  admitNetworkIdForcedArtifact,
  admitNetworkIdWorkflowArtifact,
  networkIdForcedArtifactFromPrepared,
} from "../src/network-id/workflow-adapter.js";
import { NETWORK_ID_WRONGFUL_REJECTION_VIOLATION_ID } from "../src/network-id/wrongful-rejection.js";
import { WorkflowActionChangedError } from "../src/workflow/action-changed.js";
import {
  type CanonicalViolationDetection,
  classifyCanonicalBlockViolations,
} from "../src/workflow/classification.js";
import { BLOCK_SUBJECT } from "../src/workflow/detection-subject.js";
import { type JournalJsonObject } from "../src/workflow/journal.js";
import type { FraudProofRawL1FamilyStage } from "../src/workflow/raw-l1-family-derivation.js";
import {
  type LocallyEvaluatedTransaction,
  workflowPreflightTransaction,
} from "../src/workflow/transaction-boundary.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  h28,
  h32,
} from "./helpers/canonical-block-evidence-fixture.js";
import { signedRecoveryObservationFixture } from "./helpers/signed-recovery-observation-fixture.js";
import {
  forcedHarness,
  forcedPrepared,
  scanDatum,
  SIGNER,
  STATE_QUEUE_OUT_REF,
  utxo,
  workflowIdentity,
} from "./support/network-id-forced-harness.js";

// The forced preflight must reach a real builder's pre-submit boundary; only
// the init builder itself is replaced, so everything the adapter does with the
// captured body stays under test.
vi.mock(
  "../src/network-id/submit-network-id-init.js",
  async (importOriginal) => ({
    ...(await importOriginal<
      typeof import("../src/network-id/submit-network-id-init.js")
    >()),
    submitNetworkIdInit: vi.fn(),
  }),
);

describe("network-id forced (§5.2) production workflow", () => {
  it("round-trips the prepared forced contradiction through its durable artifact", async () => {
    const prepared = await forcedPrepared();
    const artifact = networkIdForcedArtifactFromPrepared(prepared);
    expect(artifact.direction).toBe("forced");
    const admitted = admitNetworkIdForcedArtifact(artifact);
    expect(admitted.headerHash).toBe(prepared.headerHash);
    expect(admitted.subject).toEqual(prepared.subject);
    expect(admitted.evidence).toEqual(prepared.evidence);
    expect(admitted.forcedSource.membership).toEqual(
      prepared.forcedSource.membership,
    );
    // Byte equality of the re-encoded artifact is the membership/leaf identity:
    // `forcedSourceCbor` carries the header, counted root and forced leaf.
    expect(networkIdForcedArtifactFromPrepared(admitted)).toEqual(artifact);
    expect(
      admitNetworkIdWorkflowArtifact(artifact as unknown as JournalJsonObject),
    ).toEqual({ direction: "forced", prepared: admitted });
  });

  it("refuses another family's typed reason, an honest rejection, and substituted evidence", async () => {
    const otherReason = await forcedPrepared({ reason: "EmptyInputs" }).catch(
      (cause: unknown) => cause,
    );
    // The detector never yields a forced network-id detection for another
    // family's typed reason, so such a leaf cannot even be prepared; the
    // artifact admission re-binds the exact reason a second time.
    expect(otherReason).toBeInstanceOf(Error);
    expect(String(otherReason)).toMatch(/no authenticated wrongful rejection/u);

    const honest = await forcedPrepared();
    const artifact = networkIdForcedArtifactFromPrepared(honest);
    const single = networkIdForcedArtifactFromPrepared(
      await forcedPrepared({ outputNetworkIds: [0] }),
    );
    expect(() =>
      admitNetworkIdForcedArtifact({
        ...artifact,
        outputsPreimageCbor: single.outputsPreimageCbor,
      }),
    ).toThrow(
      /outputs differ from their authenticated preimage|does not bind its authenticated forced leaf/u,
    );
    expect(() =>
      admitNetworkIdForcedArtifact({ ...artifact, headerHash: h28(0x66) }),
    ).toThrow(/does not bind its authenticated header/u);
    expect(() =>
      admitNetworkIdForcedArtifact(
        networkIdForcedArtifactFromPrepared({
          ...honest,
          forcedSource: {
            ...honest.forcedSource,
            membership: {
              ...honest.forcedSource.membership,
              phas_root: h32(0x99),
            },
          },
        }),
      ),
    ).toThrow(/membership proof opens another root/u);
  });

  // Every selected action also carries `actionKind`: the production funding
  // reservation permit labels reservations by `actionKind` (or `stage`) and
  // refuses an action that offers only the adapter-private `kind`.
  it("selects init, forced step-01, the forced door, and step-02 from chain state alone", async () => {
    const prepared = await forcedPrepared();
    const notStarted = await forcedHarness({
      prepared,
      stage: {
        kind: "not_started",
        stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
      },
      forcedDoorOccupied: false,
    }).observe();
    expect(notStarted).toMatchObject({
      kind: "action_required",
      action: { input: { kind: "init", actionKind: "init" } },
    });

    const atStep01 = await forcedHarness({
      prepared,
      stage: {
        kind: "step",
        step: 1,
        threadOutRef: `${h32(0x72)}#0`,
        stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
      },
      forcedDoorOccupied: false,
    }).observe();
    expect(atStep01).toMatchObject({
      kind: "action_required",
      action: {
        input: {
          kind: "forced_step01",
          actionKind: "forced_step01",
          threadOutRef: `${h32(0x72)}#0`,
        },
      },
    });

    // Resume at the forced door: the raw-L1 family walks only the two linear
    // steps, so the door is invisible to it and must be read directly.
    const atDoor = await forcedHarness({
      prepared,
      stage: {
        kind: "not_started",
        stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
      },
      forcedDoorOccupied: true,
    }).observe();
    expect(atDoor).toMatchObject({
      kind: "action_required",
      action: {
        input: {
          kind: "forced_bind",
          actionKind: "forced_bind",
          threadOutRef: `${h32(0x77)}#0`,
        },
      },
    });

    // Step 02 no longer opens the field in this direction, so the thread that
    // reaches it is finalised directly: the scan already certified field 2.
    const atStep02 = await forcedHarness({
      prepared,
      stage: {
        kind: "step",
        step: 2,
        threadOutRef: `${h32(0x74)}#0`,
        stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
      },
      forcedDoorOccupied: false,
    }).observe();
    expect(atStep02).toMatchObject({
      kind: "action_required",
      action: {
        input: {
          kind: "step02",
          actionKind: "step02",
          threadOutRef: `${h32(0x74)}#0`,
        },
      },
    });
  });

  it("drives the scan stages from the live scan datum and its committed checkpoint", async () => {
    const prepared = await forcedPrepared();
    const opening = planNetworkIdOutputsOpening({
      prepared,
      owner: SIGNER.paymentKeyHash,
      publish: true,
    });
    expect(opening.plan.tier).not.toBe("Inline");
    const plan: NetworkIdForcedScanPlan = planNetworkIdForcedScan({
      outputsCarriagePlan: opening,
      outputCount: opening.itemCount,
    });
    const carriage = opening.plan.publications.map((publication, index) =>
      utxo({
        txHash: h32(0x20 + index),
        datum: fieldPreimagePublicationDatumCbor(publication.bytes),
      }),
    );
    const scanStage: FraudProofRawL1FamilyStage = {
      kind: "not_started",
      stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
    };
    // The scan is the field's consumer now, so the carriage it needs is
    // published against the scan thread, not against step 02.
    const unpublished = await forcedHarness({
      prepared,
      stage: scanStage,
      forcedDoorOccupied: false,
      forcedScanDatum: scanDatum(prepared, "ready"),
    }).observe();
    expect(unpublished).toMatchObject({
      kind: "action_required",
      action: {
        input: {
          kind: "publish_field",
          fieldCommitment: opening.commitment,
          threadOutRef: `${h32(0x78)}#0`,
        },
      },
    });

    const ready = await forcedHarness({
      prepared,
      stage: scanStage,
      forcedDoorOccupied: false,
      forcedScanDatum: scanDatum(prepared, "ready"),
      walletUtxos: carriage,
    }).observe();
    expect(ready).toMatchObject({
      kind: "action_required",
      action: {
        input: {
          kind: "forced_scan_open",
          scanAction: "open",
          scanOrdinal: "0",
          threadOutRef: `${h32(0x78)}#0`,
        },
      },
    });

    const walking = await forcedHarness({
      prepared,
      stage: scanStage,
      forcedDoorOccupied: false,
      forcedScanDatum: scanDatum(prepared, {
        scanningAt: hashNetworkIdForcedScanWalkCheckpoint(plan.initialWalk),
      }),
      walletUtxos: carriage,
    }).observe();
    expect(walking).toMatchObject({
      kind: "action_required",
      action: {
        input: {
          kind: "forced_scan_advance",
          scanAction: "advance",
          scanOrdinal: "0",
        },
      },
    });

    // A checkpoint the plan never produced is fail-closed: the adapter refuses
    // to guess a walk position rather than resuming at the wrong item.
    await expect(
      forcedHarness({
        prepared,
        stage: scanStage,
        forcedDoorOccupied: false,
        forcedScanDatum: scanDatum(prepared, { scanningAt: h32(0x99) }),
        walletUtxos: carriage,
      }).observe(),
    ).rejects.toThrow(/on no batch of the planned walk/u);
  });

  it("yields stale forced scan selections while rejecting duplicate or malformed fresh state", async () => {
    const prepared = await forcedPrepared();
    const opening = planNetworkIdOutputsOpening({
      prepared,
      owner: SIGNER.paymentKeyHash,
      publish: true,
    });
    const carriage = opening.plan.publications.map((publication, index) =>
      utxo({
        txHash: h32(0x20 + index),
        datum: fieldPreimagePublicationDatumCbor(publication.bytes),
      }),
    );
    const harness = forcedHarness({
      prepared,
      stage: {
        kind: "not_started",
        stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
      },
      forcedDoorOccupied: false,
      forcedScanDatum: scanDatum(prepared, "ready"),
      walletUtxos: carriage,
    });
    const selected = await harness.observe();
    if (selected.kind !== "action_required")
      throw new Error("expected forced scan action");
    const preflight = () =>
      harness.adapter.preflight({
        identity: workflowIdentity(prepared.headerHash),
        workflowId: h32(0x51),
        artifact: harness.artifact,
        entries: [],
        action: selected.action,
      });
    const original = harness.scanUtxos[0]!;
    harness.scanUtxos[0] = { ...original, txHash: h32(0x79) };
    await expect(preflight()).rejects.toBeInstanceOf(
      WorkflowActionChangedError,
    );
    harness.scanUtxos.splice(0);
    await expect(preflight()).rejects.toBeInstanceOf(
      WorkflowActionChangedError,
    );
    const plan = planNetworkIdForcedScan({
      outputsCarriagePlan: opening,
      outputCount: opening.itemCount,
    });
    harness.scanUtxos.push({
      ...original,
      datum: scanDatum(prepared, {
        scanningAt: hashNetworkIdForcedScanWalkCheckpoint(plan.initialWalk),
      }),
    });
    await expect(preflight()).rejects.toBeInstanceOf(
      WorkflowActionChangedError,
    );
    harness.scanUtxos[0] = { ...original, datum: "00" };
    await expect(preflight()).rejects.not.toBeInstanceOf(
      WorkflowActionChangedError,
    );
    harness.scanUtxos.splice(0, 1, original, { ...original, outputIndex: 1 });
    await expect(preflight()).rejects.toThrow("duplicate forced-scan UTxOs");
    await expect(preflight()).rejects.not.toBeInstanceOf(
      WorkflowActionChangedError,
    );
  });

  it("yields changed authenticated certification chunks but preserves publication authentication failures", async () => {
    const prepared = await forcedPrepared({
      outputNetworkIds: Array.from({ length: 600 }, () => 0),
    });
    const opening = planNetworkIdOutputsOpening({
      prepared,
      owner: SIGNER.paymentKeyHash,
      publish: true,
    });
    expect(opening.plan.tier).toBe("Certified");
    const carriage = opening.plan.publications.map((publication, index) =>
      utxo({
        txHash: h32(0x20 + index),
        datum: fieldPreimagePublicationDatumCbor(publication.bytes),
      }),
    );
    const harness = forcedHarness({
      prepared,
      stage: {
        kind: "not_started",
        stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
      },
      forcedDoorOccupied: false,
      forcedScanDatum: scanDatum(prepared, "ready"),
      walletUtxos: carriage,
    });
    const selected = await harness.observe();
    if (selected.kind !== "action_required")
      throw new Error("expected certification action");
    expect(selected.action.input.kind).toBe("certify_field");
    const preflight = () =>
      harness.adapter.preflight({
        identity: workflowIdentity(prepared.headerHash),
        workflowId: h32(0x51),
        artifact: harness.artifact,
        entries: [],
        action: selected.action,
      });
    carriage[0] = { ...carriage[0]!, txHash: h32(0x70) };
    await expect(preflight()).rejects.toBeInstanceOf(
      WorkflowActionChangedError,
    );
    expect(harness.publicationObserver.observeExact).toHaveBeenCalledWith(
      expect.objectContaining({ expectedOutRef: `${h32(0x70)}#0` }),
    );
    const failure = new Error("publication authentication failed");
    vi.mocked(harness.publicationObserver.observeExact).mockRejectedValueOnce(
      failure,
    );
    await expect(preflight()).rejects.toBe(failure);
  });

  // Regression: the production funding reservation permit reads the signed
  // body back off the in-memory preflight, so a preflight that forgets to bind
  // its captured transaction fails the whole run at reservation time.
  it("binds the captured signed transaction to the preflight it returns", async () => {
    const prepared = await forcedPrepared();
    const capturedTxHash = h32(0x41);
    const capturedSigned = {
      toHash: () => capturedTxHash,
      submit: async () => capturedTxHash,
      toTransaction: () => ({
        witness_set: () => ({
          native_scripts: () => undefined,
          plutus_v1_scripts: () => undefined,
          plutus_v2_scripts: () => undefined,
          plutus_v3_scripts: () => undefined,
        }),
        body: () => ({
          inputs: { len: () => 0, get: () => undefined },
          reference_inputs: () => undefined,
        }),
      }),
    } as unknown as LocallyEvaluatedTransaction["signed"];
    vi.mocked(submitNetworkIdInit).mockImplementation(
      async ({ preSubmitBoundary }) => {
        await preSubmitBoundary?.({
          txHash: capturedTxHash,
          signed: capturedSigned,
          referenceScripts: [
            {
              role: "V1 fraud-proof network-id init",
              outRef: `${h32(0x19)}#0`,
              scriptHash: h28(0x11),
            },
          ],
        });
        throw new Error("init builder must not run past its boundary");
      },
    );

    const harness = forcedHarness({
      prepared,
      stage: {
        kind: "not_started",
        stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
      },
      forcedDoorOccupied: false,
    });
    const observation = await harness.observe();
    if (observation.kind !== "action_required") {
      throw new Error("expected the init action");
    }
    const preflight = await harness.adapter.preflight({
      identity: workflowIdentity(prepared.headerHash),
      workflowId: h32(0x51),
      artifact: harness.artifact,
      entries: [],
      action: observation.action,
    });
    expect(workflowPreflightTransaction(preflight)).toBeDefined();
    expect(workflowPreflightTransaction(preflight)).toBe(capturedSigned);
    expect(preflight).toMatchObject({
      actionId: observation.action.actionId,
      txHash: capturedTxHash,
      scriptExecution: "reference_scripts",
      localUplcEvaluation: { status: "passed" },
    });
  });

  it("reconciles each forced stage from chain state and the journaled tx hash", async () => {
    const prepared = await forcedPrepared();
    const reconcile = async ({
      kind,
      stage,
      forcedDoorOccupied,
      forcedScanDatum,
      threadOutRef = `${h32(0x72)}#0`,
    }: {
      readonly kind:
        | "init"
        | "forced_step01"
        | "forced_bind"
        | "forced_scan_open"
        | "forced_scan_advance";
      readonly stage: FraudProofRawL1FamilyStage;
      readonly forcedDoorOccupied: boolean;
      readonly forcedScanDatum?: string;
      readonly threadOutRef?: string;
    }) => {
      const harness = forcedHarness({
        prepared,
        stage,
        forcedDoorOccupied,
        ...(forcedScanDatum === undefined ? {} : { forcedScanDatum }),
      });
      return await harness.adapter.reconcile({
        identity: workflowIdentity(prepared.headerHash),
        workflowId: h32(0x51),
        artifact: harness.artifact,
        entries: [],
        action: {
          actionId: `network-id:${kind}`,
          input: { kind, threadOutRef },
        },
        txHash: h32(0x41),
      });
    };
    const notStarted: FraudProofRawL1FamilyStage = {
      kind: "not_started",
      stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
    };
    const atStep02: FraudProofRawL1FamilyStage = {
      kind: "step",
      step: 2,
      threadOutRef: `${h32(0x74)}#0`,
      stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
    };
    // init advances the moment the door is occupied, even though the linear
    // family still reports `not_started`.
    await expect(
      reconcile({ kind: "init", stage: notStarted, forcedDoorOccupied: true }),
    ).resolves.toEqual({ kind: "confirmed", txHash: h32(0x41) });
    await expect(
      reconcile({
        kind: "forced_step01",
        stage: notStarted,
        forcedDoorOccupied: true,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash: h32(0x41) });
    await expect(
      reconcile({
        kind: "forced_step01",
        stage: notStarted,
        forcedDoorOccupied: false,
      }),
    ).resolves.toEqual({ kind: "not_found" });
    await expect(
      reconcile({
        kind: "forced_bind",
        stage: atStep02,
        forcedDoorOccupied: false,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash: h32(0x41) });
    // The bind is not confirmed while the thread is still parked at the door.
    await expect(
      reconcile({
        kind: "forced_bind",
        stage: atStep02,
        forcedDoorOccupied: true,
      }),
    ).resolves.toEqual({ kind: "not_found" });
    // The bind now hands the thread to the scan, which the linear family
    // cannot see at all: occupancy of the scan address is what confirms it.
    await expect(
      reconcile({
        kind: "forced_bind",
        stage: notStarted,
        forcedDoorOccupied: false,
        forcedScanDatum: scanDatum(prepared, "ready"),
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash: h32(0x41) });
    // A scan batch is confirmed exactly when the self-looping thread has left
    // the out-ref its action named, and pending while it still sits there.
    await expect(
      reconcile({
        kind: "forced_scan_open",
        stage: notStarted,
        forcedDoorOccupied: false,
        forcedScanDatum: scanDatum(prepared, "ready"),
        threadOutRef: `${h32(0x78)}#0`,
      }),
    ).resolves.toEqual({ kind: "not_found" });
    await expect(
      reconcile({
        kind: "forced_scan_open",
        stage: notStarted,
        forcedDoorOccupied: false,
        forcedScanDatum: scanDatum(prepared, "ready"),
        threadOutRef: `${h32(0x79)}#0`,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash: h32(0x41) });
    // The completing batch leaves the scan address entirely.
    await expect(
      reconcile({
        kind: "forced_scan_advance",
        stage: atStep02,
        forcedDoorOccupied: false,
        threadOutRef: `${h32(0x78)}#0`,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash: h32(0x41) });
  });

  // Before the chain advances, shallow absence must retain the exact signed body;
  // abandon the funding transition and rebuild the identical transaction while
  // the original is still landing. An unconfirmed submission now resolves
  // through canonical signed-transaction recovery.
  it("keeps an unconfirmed submission pending until signed recovery reports expiry", async () => {
    const prepared = await forcedPrepared();
    const inputs = CML.TransactionInputList.new();
    inputs.add(
      CML.TransactionInput.new(CML.TransactionHash.from_hex(h32(0x11)), 0n),
    );
    const body = CML.TransactionBody.new(
      inputs,
      CML.TransactionOutputList.new(),
      0n,
    );
    const signed = CML.Transaction.new(
      body,
      CML.TransactionWitnessSet.new(),
      true,
    );
    const transactionHash = CML.hash_transaction(body).to_hex();
    const signedTransactionCborHex = signed.to_cbor_hex();
    const observation = signedRecoveryObservationFixture({
      transactionHash,
      signedTransactionCborHex,
    });
    const observe = vi.fn(async () => observation("pending"));
    const notStarted: FraudProofRawL1FamilyStage = {
      kind: "not_started",
      stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
    };
    const reconcile = async (
      harness: ReturnType<typeof forcedHarness>,
      overrides: {
        readonly signedTransactionCborHex?: string;
        readonly retirementOnly?: boolean;
      } = {
        signedTransactionCborHex,
      },
    ) => {
      const observed = await harness.observe();
      if (observed.kind !== "action_required") {
        throw new Error("expected the init action");
      }
      return await harness.adapter.reconcile({
        identity: workflowIdentity(prepared.headerHash),
        workflowId: h32(0x51),
        artifact: harness.artifact,
        entries: [],
        action: observed.action,
        txHash: transactionHash,
        ...overrides,
      });
    };
    const recovering = forcedHarness({
      prepared,
      stage: notStarted,
      forcedDoorOccupied: false,
      observeSignedTransaction: observe,
    });
    await expect(reconcile(recovering)).resolves.toEqual({
      kind: "pending",
      txHash: transactionHash,
    });
    expect(observe).toHaveBeenCalledWith({
      transactionHash,
      signedTransactionCborHex,
    });
    // Whichever lands wins: expired within the horizon is superseded at once.
    observe.mockResolvedValueOnce(observation("expired"));
    await expect(reconcile(recovering)).resolves.toEqual({ kind: "not_found" });
    observe.mockResolvedValueOnce(observation("expired", true));
    await expect(
      reconcile(recovering, { signedTransactionCborHex, retirementOnly: true }),
    ).resolves.toMatchObject({
      kind: "not_found",
      retirement: { transactionHash, reason: "expired" },
    });
    observe.mockResolvedValueOnce(observation("conflict"));
    await expect(reconcile(recovering)).resolves.toEqual({
      kind: "conflict",
      reason: "conflict",
    });
    // Without the durable signed bytes recovery cannot establish absence.
    await expect(reconcile(recovering, {})).resolves.toMatchObject({
      kind: "unknown",
    });
    // The bare raw-L1 stub has no signed recovery, so the legacy answer stands.
    await expect(
      reconcile(
        forcedHarness({
          prepared,
          stage: notStarted,
          forcedDoorOccupied: false,
        }),
      ),
    ).resolves.toEqual({ kind: "not_found" });
  });

  it("routes the forced violation to networkId and refuses another category or a generic fallback", async () => {
    const fixture = await buildCanonicalBlockFixture({ transactions: [] });
    const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(fixture),
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "libp2p/peer-forced",
        grade: "security",
      },
    });
    const detection = (violationId: string): CanonicalViolationDetection => ({
      ...BLOCK_SUBJECT,
      detectionId: `${violationId}:0:${h32(0x33)}`,
      headerHash: evidence.headerHash,
      violationId,
      position: 0n,
      diagnostic: "forced rejection fixture",
    });
    await expect(
      classifyCanonicalBlockViolations({
        evidence,
        detections: [detection(NETWORK_ID_WRONGFUL_REJECTION_VIOLATION_ID)],
      }),
    ).resolves.toMatchObject({
      decision: "fault_detected",
      category: "networkId",
      selected: { violationId: NETWORK_ID_WRONGFUL_REJECTION_VIOLATION_ID },
    });
    // Another family's forced rejection never reaches the network-id thread.
    await expect(
      classifyCanonicalBlockViolations({
        evidence,
        detections: [detection("input-set-uniqueness-wrongful-rejection")],
      }),
    ).resolves.toMatchObject({
      decision: "fault_detected",
      category: "inputSetUniqueness",
    });
    // A `NetworkIdMismatch` argument is never coerced into a generic family:
    // an unregistered id stays an unprovable gap instead of a validation
    // dispute.
    await expect(
      classifyCanonicalBlockViolations({
        evidence,
        detections: [detection("network-id-mismatch-dispute")],
      }),
    ).resolves.toMatchObject({
      decision: "unprovable_gap",
      selected: { reason: "unregistered_violation" },
    });
  });
});
