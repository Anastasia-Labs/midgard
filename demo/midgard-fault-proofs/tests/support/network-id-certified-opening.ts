/**
 * Network-id outputs openings at Certified tier for the adapter suites: the
 * forced direction over the forced harness, and an output-network fault that
 * opens through the final step.
 */
import { encodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import {
  deriveFieldPreimageCertification,
  fieldPreimagePublicationDatumCbor,
} from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  planNetworkIdOutputsOpening,
  type PreparedNetworkIdProof,
} from "../../src/network-id/prepare.js";
import { deriveL2TransactionSourceCbor } from "../../src/prepare-double-spend.js";
import type { JournalJsonObject } from "../../src/workflow/journal.js";
import {
  buildFixtureTransaction,
  h28,
  h32,
} from "../helpers/canonical-block-evidence-fixture.js";
import {
  CERTIFICATE_ADDRESS,
  CERTIFICATE_UNIT,
  forcedHarness,
  forcedPrepared,
  scanDatum,
  SIGNER,
  STATE_QUEUE_OUT_REF,
  utxo,
} from "./network-id-forced-harness.js";

/** A Certified-tier opening (600 outputs), its carriage, and the harness. */
export const certifiedOpening = async ({
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
  const wallet = opening.plan.publications.map((publication, index) =>
    utxo({
      txHash: h32(0x20 + index),
      datum: fieldPreimagePublicationDatumCbor(publication.bytes),
    }),
  );
  const harness = forcedHarness({
    prepared,
    stage: { kind: "not_started", stateQueueBlockOutRef: STATE_QUEUE_OUT_REF },
    forcedDoorOccupied: false,
    forcedScanDatum: scanDatum(prepared, "ready"),
    walletUtxos: wallet,
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
  return { prepared, harness, certificate, wallet };
};

/**
 * An output-network fault whose outputs field (440 items unless `outputs`
 * names others) opens at Certified tier through the final step rather than
 * the forced scan.
 */
export const outputNetworkOpening = (
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
