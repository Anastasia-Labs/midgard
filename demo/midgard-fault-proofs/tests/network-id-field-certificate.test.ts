/**
 * The network-id outputs opening at Certified tier: how the production adapter
 * settles its own field-certificate transaction and binds the certificate the
 * final step references, when the certificate address holds more than one
 * output that carries the same certification datum.
 */
import {
  deriveFieldPreimageCertification,
  fieldPreimagePublicationDatumCbor,
} from "@al-ft/midgard-sdk";
import { CML, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { planNetworkIdOutputsOpening } from "../src/network-id/prepare.js";
import { h32 } from "./helpers/canonical-block-evidence-fixture.js";
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
});
