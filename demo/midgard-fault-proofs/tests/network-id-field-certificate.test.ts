/**
 * The network-id outputs opening at Certified tier: how the production adapter
 * settles its own field-certificate transaction and binds the certificate the
 * final step references, when the certificate address holds more than one
 * output that carries the same certification datum, and how it treats carriage
 * that changed after an action was scheduled.
 */
import { encodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import {
  deriveFieldPreimageCertification,
  fieldPreimagePublicationDatumCbor,
} from "@al-ft/midgard-sdk";
import { CML, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  planNetworkIdOutputsOpening,
  type PreparedNetworkIdProof,
} from "../src/network-id/prepare.js";
import { deriveL2TransactionSourceCbor } from "../src/prepare-double-spend.js";
import { WorkflowActionChangedError } from "../src/workflow/action-changed.js";
import type { JournalJsonObject } from "../src/workflow/journal.js";
import {
  buildFixtureTransaction,
  h28,
  h32,
} from "./helpers/canonical-block-evidence-fixture.js";
import { signedRecoveryObservationFixture } from "./helpers/signed-recovery-observation-fixture.js";
import {
  CERTIFICATE_ADDRESS,
  CERTIFICATE_UNIT,
  forcedHarness,
  forcedPrepared,
  scanDatum,
  SIGNER,
  STATE_QUEUE_OUT_REF,
  utxo,
  workflowIdentity,
} from "./support/network-id-forced-harness.js";
import { repeatedFirstChunkItems } from "./support/repeated-first-chunk-items.js";

/** A Certified-tier opening (600 outputs), its carriage, and the harness. */
const certifiedOpening = async ({
  certificateUtxos,
  observeSignedTransaction,
}: Pick<
  Parameters<typeof forcedHarness>[0],
  "certificateUtxos" | "observeSignedTransaction"
>) => {
  const prepared = await forcedPrepared({
    outputNetworkIds: Array.from({ length: 600 }, () => 0),
  });
  const opening = planNetworkIdOutputsOpening({
    prepared,
    owner: SIGNER.paymentKeyHash,
    publish: true,
  });
  expect(opening.plan.tier).toBe("Certified");
  const harness = forcedHarness({
    prepared,
    stage: { kind: "not_started", stateQueueBlockOutRef: STATE_QUEUE_OUT_REF },
    forcedDoorOccupied: false,
    forcedScanDatum: scanDatum(prepared, "ready"),
    walletUtxos: opening.plan.publications.map((publication, index) =>
      utxo({
        txHash: h32(0x20 + index),
        datum: fieldPreimagePublicationDatumCbor(publication.bytes),
      }),
    ),
    certificateUtxos,
    observeSignedTransaction,
  });
  const certificate = (
    txHash: string,
    assets: UTxO["assets"] = { lovelace: 5_000_000n, [CERTIFICATE_UNIT]: 1n },
  ): UTxO => ({
    txHash,
    outputIndex: 0,
    address: CERTIFICATE_ADDRESS,
    assets,
    datum: deriveFieldPreimageCertification(opening.plan).datumCbor,
  });
  return { prepared, harness, certificate };
};

/**
 * An output-network fault whose outputs field (440 items unless `outputs`
 * names others) opens at Certified tier through the final step rather than
 * the forced scan.
 */
const outputNetworkOpening = (
  outputs: readonly Buffer[] = Array.from({ length: 440 }, () =>
    encodeMidgardTxOutput({
      address: Buffer.concat([Buffer.from([0x61]), Buffer.alloc(28, 0x55)]),
      value: { lovelace: 1n, assets: new Map() },
    }),
  ),
) => {
  const transaction = buildFixtureTransaction({
    spendInputs: [],
    outputs: [...outputs],
    fee: 1n,
    networkId: 0n,
  });
  const headerHash = h28(0x71);
  const l2TransactionSourceCbor = deriveL2TransactionSourceCbor(
    transaction.canonicalCbor,
  );
  const prepared = {
    headerHash,
    expectedNetworkId: 0n,
    badTxId: transaction.txId,
    nativeTxCanonicalCbor: transaction.canonicalCbor.toString("hex"),
    nativeTxCompactCbor: transaction.compactCbor.toString("hex"),
    outputsItemCbors: outputs.map((item) => item.toString("hex")),
    faultClaim: { kind: "output-network", outputIndex: 0n },
    fault: { OutputNetwork: { output_index: 0n } },
    txInclusion: {
      nativeTxId: transaction.txId,
      nativeTx: {} as never,
      nativeTxCompactCbor: transaction.compactCbor.toString("hex"),
      l2TransactionSourceCbor,
      transactionsPhasRoot: h32(0x31),
      txMembershipProofCbor: "d87980",
    },
  } satisfies PreparedNetworkIdProof;
  const opening = planNetworkIdOutputsOpening({
    prepared,
    owner: SIGNER.paymentKeyHash,
  });
  expect(opening.plan.tier).toBe("Certified");
  const artifact = {
    schemaVersion: "midgard-network-id-workflow-artifact-v1",
    headerHash,
    expectedNetworkId: "0",
    badTxId: transaction.txId,
    nativeTxCanonicalCbor: prepared.nativeTxCanonicalCbor,
    nativeTxCompactCbor: prepared.nativeTxCompactCbor,
    l2TransactionSourceCbor,
    outputsItemCbors: prepared.outputsItemCbors,
    faultKind: "output-network",
    outputIndex: "0",
    transactionsPhasRoot: h32(0x31),
    txMembershipProofCbor: "d87980",
  } as JournalJsonObject;
  return { headerHash, artifact, opening };
};

/**
 * 800 outputs whose outputs field repeats its first chunk as its second. Only
 * address payload bytes vary, so the first output still names network 1.
 */
const repeatedFirstChunkOutputs = () =>
  repeatedFirstChunkItems({
    count: 800,
    item: (fill) =>
      encodeMidgardTxOutput({
        address: Buffer.concat([Buffer.from([0x61]), Buffer.alloc(28, fill)]),
        value: { lovelace: 1n, assets: new Map() },
      }),
  });

/** A signed transaction the canonical recovery port can authenticate. */
const signedTransaction = () => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex(h32(0x11)), 0n),
  );
  const body = CML.TransactionBody.new(
    inputs,
    CML.TransactionOutputList.new(),
    0n,
  );
  return {
    transactionHash: CML.hash_transaction(body).to_hex(),
    signedTransactionCborHex: CML.Transaction.new(
      body,
      CML.TransactionWitnessSet.new(),
      true,
    ).to_cbor_hex(),
  };
};

describe("network-id Certified-tier field certificate", () => {
  it("confirms its own certificate transaction by its exact output, whatever else sits at the certificate address", async () => {
    const signed = signedTransaction();
    const observation = signedRecoveryObservationFixture(signed);
    const included = observation("included");
    const observeSignedTransaction = vi.fn(async () => included);
    const certificates: UTxO[] = [];
    const { prepared, harness, certificate } = await certifiedOpening({
      certificateUtxos: certificates,
      observeSignedTransaction,
    });
    const selected = await harness.observe();
    if (selected.kind !== "action_required")
      throw new Error("expected the certification action");
    expect(selected.action.input.kind).toBe("certify_field");
    const reconcile = async () =>
      await harness.adapter.reconcile({
        identity: workflowIdentity(prepared.headerHash),
        workflowId: h32(0x51),
        artifact: harness.artifact,
        entries: [],
        action: selected.action,
        txHash: signed.transactionHash,
        signedTransactionCborHex: signed.signedTransactionCborHex,
      });
    const confirmed = { kind: "confirmed", txHash: signed.transactionHash };

    certificates.push(certificate(signed.transactionHash));
    await expect(reconcile()).resolves.toEqual(confirmed);

    // Another valid certificate for the same opening, ordered first.
    const other = certificate("00".repeat(32));
    certificates.unshift(other);
    await expect(reconcile()).resolves.toEqual(confirmed);
    observeSignedTransaction.mockResolvedValue({
      ...observation("included", true),
      inclusionPoint: included.releaseFinalPoint,
    });
    await expect(reconcile()).resolves.toEqual(confirmed);

    // Without its own output the transaction is not confirmed by the other.
    observeSignedTransaction.mockResolvedValue(included);
    certificates.splice(0, certificates.length, other);
    await expect(reconcile()).resolves.toEqual({
      kind: "pending",
      txHash: signed.transactionHash,
    });
  });

  it("treats a certificate other than the scheduled one as a changed action", async () => {
    const { headerHash, artifact, opening } = outputNetworkOpening();
    const certification = deriveFieldPreimageCertification(opening.plan);
    const certificate = (txHash: string): UTxO => ({
      txHash,
      outputIndex: 0,
      address: CERTIFICATE_ADDRESS,
      assets: { lovelace: 5_000_000n, [CERTIFICATE_UNIT]: 1n },
      datum: certification.datumCbor,
    });
    const scheduled = certificate(h32(0x61));
    const certificates: UTxO[] = [scheduled];
    const harness = forcedHarness({
      prepared: { headerHash, artifact },
      stage: {
        kind: "step",
        step: 2,
        threadOutRef: `${h32(0x72)}#0`,
        stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
      },
      forcedDoorOccupied: false,
      walletUtxos: opening.plan.publications.map((publication, index) =>
        utxo({
          txHash: h32(0x20 + index),
          datum: fieldPreimagePublicationDatumCbor(publication.bytes),
        }),
      ),
      certificateUtxos: certificates,
    });
    const selected = await harness.observe();
    if (selected.kind !== "action_required")
      throw new Error("expected the final step");
    expect(selected.action.input).toMatchObject({
      kind: "step02",
      certificateOutRef: `${h32(0x61)}#0`,
    });
    const preflight = () =>
      harness.adapter.preflight({
        identity: workflowIdentity(headerHash),
        workflowId: h32(0x51),
        artifact,
        entries: [],
        action: selected.action,
      });
    certificates.unshift(certificate("00".repeat(32)));
    await expect(preflight()).rejects.toBeInstanceOf(
      WorkflowActionChangedError,
    );
    certificates.splice(0, certificates.length);
    await expect(preflight()).rejects.toBeInstanceOf(
      WorkflowActionChangedError,
    );
  });

  it("treats a certificate other than the scheduled one as a changed forced-scan action", async () => {
    const certificates: UTxO[] = [];
    const { prepared, harness, certificate } = await certifiedOpening({
      certificateUtxos: certificates,
    });
    certificates.push(certificate(h32(0x61)));
    const selected = await harness.observe();
    if (selected.kind !== "action_required")
      throw new Error("expected the forced scan action");
    expect(selected.action.input.certificateOutRef).toBe(`${h32(0x61)}#0`);
    const preflight = () =>
      harness.adapter.preflight({
        identity: workflowIdentity(prepared.headerHash),
        workflowId: h32(0x51),
        artifact: harness.artifact,
        entries: [],
        action: selected.action,
      });
    certificates.unshift(certificate("00".repeat(32)));
    await expect(preflight()).rejects.toBeInstanceOf(
      WorkflowActionChangedError,
    );
  });

  it("schedules only a certificate output that holds nothing but ada and the certificate token", async () => {
    const certificates: UTxO[] = [];
    const { harness, certificate } = await certifiedOpening({
      certificateUtxos: certificates,
    });
    certificates.push(
      certificate("00".repeat(32), {
        lovelace: 5_000_000n,
        [CERTIFICATE_UNIT]: 1n,
        [`${"ab".repeat(28)}01`]: 1n,
      }),
      certificate(h32(0x61)),
    );
    const selected = await harness.observe();
    if (selected.kind !== "action_required")
      throw new Error("expected the forced scan action");
    expect(selected.action.input.certificateOutRef).toBe(`${h32(0x61)}#0`);
  });

  it("schedules only an output that holds the certificate token", async () => {
    const certificates: UTxO[] = [];
    const { harness, certificate } = await certifiedOpening({
      certificateUtxos: certificates,
    });
    certificates.push(
      certificate("00".repeat(32), { lovelace: 5_000_000n }),
      certificate(h32(0x61)),
    );
    const selected = await harness.observe();
    if (selected.kind !== "action_required")
      throw new Error("expected the forced scan action");
    expect(selected.action.input.certificateOutRef).toBe(`${h32(0x61)}#0`);
  });

  it("treats a field chunk published since scheduling as a changed publication action", async () => {
    const { headerHash, artifact, opening } = outputNetworkOpening();
    const [first, ...rest] = opening.plan.publications.map(
      (publication, index) =>
        utxo({
          txHash: h32(0x20 + index),
          datum: fieldPreimagePublicationDatumCbor(publication.bytes),
        }),
    );
    const wallet: UTxO[] = [...rest];
    const harness = forcedHarness({
      prepared: { headerHash, artifact },
      stage: {
        kind: "step",
        step: 2,
        threadOutRef: `${h32(0x72)}#0`,
        stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
      },
      forcedDoorOccupied: false,
      walletUtxos: wallet,
      certificateUtxos: [],
    });
    const selected = await harness.observe();
    if (selected.kind !== "action_required")
      throw new Error("expected a field publication");
    expect(selected.action.input).toMatchObject({
      kind: "publish_field",
      publicationDatumCbor: first!.datum,
    });

    // The scheduled chunk is on chain before the action runs.
    wallet.unshift(first!);
    await expect(
      harness.adapter.preflight({
        identity: workflowIdentity(headerHash),
        workflowId: h32(0x51),
        artifact,
        entries: [],
        action: selected.action,
      }),
    ).rejects.toBeInstanceOf(WorkflowActionChangedError);
  });

  it("publishes every field chunk when two chunks are identical", async () => {
    const { headerHash, artifact, opening } = outputNetworkOpening(
      repeatedFirstChunkOutputs(),
    );
    const chunks = opening.plan.publications.map(
      (publication, index): UTxO =>
        utxo({
          txHash: h32(0x20 + index),
          datum: fieldPreimagePublicationDatumCbor(publication.bytes),
        }),
    );
    expect(chunks.map((chunk) => chunk.datum)).toEqual([
      chunks[0]!.datum,
      chunks[0]!.datum,
      chunks[2]!.datum,
    ]);
    // One output carries the repeated chunk's bytes; the plan needs two.
    const harness = forcedHarness({
      prepared: { headerHash, artifact },
      stage: {
        kind: "step",
        step: 2,
        threadOutRef: `${h32(0x72)}#0`,
        stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
      },
      forcedDoorOccupied: false,
      walletUtxos: [chunks[0]!, chunks[2]!],
      certificateUtxos: [],
    });
    const selected = await harness.observe();
    if (selected.kind !== "action_required")
      throw new Error("expected a field publication");
    expect(selected.action.input).toMatchObject({
      kind: "publish_field",
      publicationDatumCbor: chunks[1]!.datum,
    });
    // The stub chain holds no funding, so building the publication is as far
    // as the action can get here.
    await expect(
      harness.adapter.preflight({
        identity: workflowIdentity(headerHash),
        workflowId: h32(0x51),
        artifact,
        entries: [],
        action: selected.action,
      }),
    ).rejects.toThrow("Failed to publish field-preimage carriage");
  });

  it("certifies two identical field chunks from two different outputs", async () => {
    const { headerHash, artifact, opening } = outputNetworkOpening(
      repeatedFirstChunkOutputs(),
    );
    const chunks = opening.plan.publications.map(
      (publication, index): UTxO =>
        utxo({
          txHash: h32(0x20 + index),
          datum: fieldPreimagePublicationDatumCbor(publication.bytes),
        }),
    );
    const harness = forcedHarness({
      prepared: { headerHash, artifact },
      stage: {
        kind: "step",
        step: 2,
        threadOutRef: `${h32(0x72)}#0`,
        stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
      },
      forcedDoorOccupied: false,
      walletUtxos: chunks,
      certificateUtxos: [],
    });
    const selected = await harness.observe();
    if (selected.kind !== "action_required")
      throw new Error("expected the certification action");
    expect(selected.action.input).toMatchObject({
      kind: "certify_field",
      chunkOutRefs: chunks
        .map((chunk) => `${chunk.txHash}#${chunk.outputIndex.toString()}`)
        .join(","),
    });
  });
});
