/**
 * Pure production-workflow coverage for the §5.2 network-id wrongful-rejection
 * (forced) direction: durable artifact round trip, deterministic action
 * selection from journal plus simulated chain state at every physical step,
 * and the classification refusals that keep another family's typed rejection
 * reason out of this thread.
 */
import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  adjudicateMidgardNativeTxFullValidity,
  computeHash28,
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSource,
  encodeMidgardNativeTxCanonical,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import {
  encodeHeaderCbor,
  fieldPreimagePublicationDatumCbor,
  ForcedInclusionTxV1Schema,
  type NetworkIdFault,
  NetworkIdForcedScanDatumSchema,
  OutputReference,
  Proof,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type MintingPolicy,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import {
  hashNetworkIdForcedScanWalkCheckpoint,
  type NetworkIdForcedScanPlan,
  planNetworkIdForcedScan,
} from "../src/network-id/forced-scan-plan.js";
import { planNetworkIdOutputsOpening } from "../src/network-id/prepare.js";
import {
  admitNetworkIdForcedArtifact,
  admitNetworkIdWorkflowArtifact,
  createNetworkIdWorkflowAdapter,
  networkIdForcedArtifactFromPrepared,
  type NetworkIdWorkflowAdapterConfig,
} from "../src/network-id/workflow-adapter.js";
import {
  detectNetworkIdWrongfulRejections,
  NETWORK_ID_WRONGFUL_REJECTION_VIOLATION_ID,
  type PreparedNetworkIdWrongfulRejection,
} from "../src/network-id/wrongful-rejection.js";
import type { ResolvedProverSigner } from "../src/runtime.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import {
  type CanonicalViolationDetection,
  classifyCanonicalBlockViolations,
} from "../src/workflow/classification.js";
import {
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type JournalJsonObject,
} from "../src/workflow/journal.js";
import type { FraudProofRawL1FamilyStage } from "../src/workflow/raw-l1-family-derivation.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  h28,
  h32,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import { makeHeader } from "./support/emulator/header-fixtures.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";

const POLICY_ID = "91".repeat(28);
const THREAD_POLICY_ID = h28(0x13);
const CATEGORY_ID = "0000001c";
const SCRIPT: MintingPolicy = { type: "Native", script: "8200" };
const SIGNER: ResolvedProverSigner = {
  source: "network-id-forced-test",
  address: "addr_test1_network_id_forced",
  paymentKeyHash: "81".repeat(28),
  selectWallet: () => undefined,
};
const STEP_01_ADDRESS = "network-step-01";
const FORCED_STEP_ADDRESS = "network-forced-step";
const FORCED_SCAN_ADDRESS = "network-forced-scan";
const STEP_02_ADDRESS = "network-step-02";
const STATE_QUEUE_ADDRESS = "state-queue";
const STATE_QUEUE_OUT_REF = `${h32(0x73)}#0`;

const utxo = ({
  txHash,
  outputIndex = 0,
  address = SIGNER.address,
  datum,
}: {
  readonly txHash: string;
  readonly outputIndex?: number;
  readonly address?: string;
  readonly datum?: string;
}): UTxO => ({
  txHash,
  outputIndex,
  address,
  assets: { lovelace: 5_000_000n },
  ...(datum === undefined ? {} : { datum }),
});

const output = (networkId: number) =>
  encodeMidgardTxOutput({
    address: Buffer.concat([
      Buffer.from([0x60 | networkId]),
      Buffer.alloc(28, 0x44),
    ]),
    value: { lovelace: 2_000_000n, assets: new Map() },
  });

/**
 * One authenticated forced leaf, its counted-root membership proof, and the
 * header that commits it — the exact material the planner would derive from a
 * reconstructed block.
 */
const forcedPrepared = async ({
  outputNetworkIds = [0, 0],
  reason = "NetworkIdMismatch",
}: {
  readonly outputNetworkIds?: readonly number[];
  readonly reason?: "NetworkIdMismatch" | "EmptyInputs";
} = {}): Promise<PreparedNetworkIdWrongfulRejection> => {
  const invalid = adjudicateMidgardNativeTxFullValidity(
    makeNativeTx({
      spendInputCbors: [outRefCbor(0x11, 0n)],
      fee: 0n,
      outputCbors: outputNetworkIds.map(output),
    }),
    "TxIsInvalid",
  );
  const txId = computeMidgardNativeTxId(invalid).toString("hex");
  const proofSource = deriveMidgardNativeTxProofSource(invalid);
  const key = { transactionId: h32(0x05), outputIndex: 0n };
  const leaf = {
    tx_id: txId,
    source: {
      compact_cbor: proofSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        proofSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        proofSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: { ForcedTxInvalid: { reason } },
  } as const;
  const keyBytes = Buffer.from(Data.to(key, OutputReference), "hex");
  const valueBytes = Buffer.from(
    Data.to(leaf as never, ForcedInclusionTxV1Schema as never),
    "hex",
  );
  const root = await buildCountedRoot(ROOT_DOMAINS.forcedTransactionsV1, [
    { key: keyBytes, value: valueBytes },
  ]);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(keyBytes, valueBytes);
  const proof = await trie.prove(keyBytes);
  const header = {
    ...makeHeader(h28(0x44), 1_000),
    forcedTransactionsRoot: root.root,
    forcedTransactionCount: root.count,
  };
  const headerHash = computeHash28(encodeHeaderCbor(header)).toString("hex");
  const block = {
    headerHash,
    header: { expectedNetworkId: 0n },
    reconstruction: {
      forcedTransactions: [
        {
          key,
          value: leaf,
          fullTransactionCbor: encodeMidgardNativeTxCanonical(invalid),
        },
      ],
    },
  } as never;
  const detection = detectNetworkIdWrongfulRejections({
    block,
    expectedNetworkId: 0n,
  })[0];
  if (detection === undefined) {
    throw new Error("fixture produced no authenticated wrongful rejection");
  }
  return {
    headerHash,
    expectedNetworkId: 0n,
    badTxId: txId,
    nativeTxCompactCbor: leaf.source.compact_cbor,
    outputsItemCbors: detection.evidence.outputsItemCbors,
    faultClaim: { kind: "forced-network-mismatch" },
    fault: "ForcedNetworkIdMismatch" as NetworkIdFault,
    subject: detection.evidence.subject,
    forcedSource: {
      header,
      membership: {
        domain: root.domain,
        root: root.root,
        phas_root: root.phasRoot,
        count: root.count,
        key,
        value: leaf,
        proof: Data.from(proof.toCBOR().toString("hex"), Proof),
      } as PreparedNetworkIdWrongfulRejection["forcedSource"]["membership"],
      direction: 1n,
    },
    evidence: detection.evidence,
  };
};

const workflowIdentity = (headerHash: string): FraudProofWorkflowIdentity => ({
  schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  deploymentFingerprint: "d1".repeat(32),
  category: "networkId",
  target: { kind: "state_queue_header", headerHash },
});

/** The scan-thread datum the forced door writes, and the ones it becomes. */
const scanDatum = (
  prepared: PreparedNetworkIdWrongfulRejection,
  state: "ready" | { readonly scanningAt: string },
): string => {
  const bound = {
    bad_tx_id: prepared.badTxId,
    committed_tx_network_id: prepared.evidence.committedNetworkId,
    expected_network_id: prepared.expectedNetworkId,
    forced_source_key: prepared.subject.source_key,
  };
  return Data.to(
    {
      fraud_prover: SIGNER.paymentKeyHash,
      data:
        state === "ready"
          ? { Ready: { bound } }
          : { Scanning: { bound, checkpoint_hash: state.scanningAt } },
    } as never,
    NetworkIdForcedScanDatumSchema as never,
  );
};

const forcedHarness = ({
  prepared,
  stage,
  forcedDoorOccupied,
  forcedScanDatum,
  walletUtxos = [],
}: {
  readonly prepared: PreparedNetworkIdWrongfulRejection;
  readonly stage: FraudProofRawL1FamilyStage;
  readonly forcedDoorOccupied: boolean;
  /** Inline datum of the live scan thread, absent when the scan is empty. */
  readonly forcedScanDatum?: string;
  readonly walletUtxos?: readonly UTxO[];
}) => {
  const forcedUtxo = utxo({
    txHash: h32(0x77),
    address: FORCED_STEP_ADDRESS,
  });
  const scanUtxo = utxo({
    txHash: h32(0x78),
    address: FORCED_SCAN_ADDRESS,
    ...(forcedScanDatum === undefined ? {} : { datum: forcedScanDatum }),
  });
  const lucid = {
    utxosAtWithUnit: async (address: string) =>
      address === FORCED_STEP_ADDRESS && forcedDoorOccupied
        ? [forcedUtxo]
        : address === FORCED_SCAN_ADDRESS && forcedScanDatum !== undefined
          ? [scanUtxo]
          : address === STATE_QUEUE_ADDRESS
            ? [utxo({ txHash: h32(0x73), address: STATE_QUEUE_ADDRESS })]
            : [],
    utxosAt: async (address: string) =>
      address === SIGNER.address ? walletUtxos : [],
  } as unknown as LucidEvolution;
  const step = {
    spendingScript: SCRIPT,
    spendingScriptHash: h28(0x11),
    spendingScriptAddress: STEP_01_ADDRESS,
  };
  const config = {
    lucid,
    blueprint: {},
    network: "Preview",
    contracts: {
      steps: [step, { ...step, spendingScriptAddress: STEP_02_ADDRESS }],
      forcedStep: { ...step, spendingScriptAddress: FORCED_STEP_ADDRESS },
      forcedScan: { ...step, spendingScriptAddress: FORCED_SCAN_ADDRESS },
      expectedNetworkId: 0n,
      computationThread: { policyId: THREAD_POLICY_ID, mintingScript: SCRIPT },
      fraudProof: {
        policyId: h28(0x14),
        mintingScript: SCRIPT,
        spendingScriptAddress: "proof",
      },
      hubOraclePolicyId: h28(0x15),
      stateQueuePolicyId: h28(0x16),
      fieldPreimageCertificatePolicyId: POLICY_ID,
      fieldPreimageCertificateMintingScript: SCRIPT,
    },
    stateQueueAddress: STATE_QUEUE_ADDRESS,
    category: { categoryId: CATEGORY_ID, label: "network-id" },
    catalogue: {
      policyId: h28(0x17),
      spendingScriptAddress: "catalogue",
      root: h32(0x18),
    },
    signer: SIGNER,
    stepReferenceScripts: [
      utxo({ txHash: h32(0x19) }),
      utxo({ txHash: h32(0x1a) }),
    ],
    forcedStepReferenceScript: utxo({ txHash: h32(0x1b) }),
    forcedScanReferenceScript: utxo({ txHash: h32(0x1d) }),
    fieldPreimageCertificateReferenceScript: utxo({ txHash: h32(0x1c) }),
    witnessReferenceScripts: {},
    removal: {
      deploymentInfo: {},
      category: "networkId",
      isCurrentHead: async () => true,
    },
    rawL1: {
      publications: {
        observerVersion:
          "midgard-fraud-proof-authenticated-publication-observer-v1",
        observeExact: vi.fn(
          async (input: { readonly expectedOutRef: string }) => ({
            kind: "confirmed" as const,
            outRef: input.expectedOutRef,
          }),
        ),
      },
      transactionConfirmed: async () => true,
      observe: async () => stage,
    },
    terminalFacts: async () => {
      throw new Error("terminal state is outside this test");
    },
  } as unknown as NetworkIdWorkflowAdapterConfig;
  const artifact = networkIdForcedArtifactFromPrepared(
    prepared,
  ) as unknown as JournalJsonObject;
  return {
    artifact,
    adapter: createNetworkIdWorkflowAdapter(config),
    observe: async () =>
      await createNetworkIdWorkflowAdapter(config).observe({
        identity: workflowIdentity(prepared.headerHash),
        workflowId: h32(0x51),
        artifact,
        entries: [],
      }),
  };
};

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
      action: { input: { kind: "init" } },
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
        input: { kind: "forced_step01", threadOutRef: `${h32(0x72)}#0` },
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
        input: { kind: "forced_bind", threadOutRef: `${h32(0x77)}#0` },
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
      action: { input: { kind: "step02", threadOutRef: `${h32(0x74)}#0` } },
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
