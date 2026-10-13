/** Authenticated retained-DA preparation for the Q35 network-id family. */
import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import { type NetworkIdFault } from "@al-ft/midgard-sdk";

import {
  admitCanonicalEvidenceForProofBuild,
  type CanonicalEvidenceBuilderInput,
} from "../evidence/prepare-from-evidence.js";
import type { FaultProofFieldOpeningPlan } from "../field-opening.js";
import { planFaultProofFieldOpening } from "../field-opening.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  type PreparedTxInclusionJson,
  requireProof,
  requireTransactionsRootMatch,
  transactionSourceTrieItem,
} from "../prepare-double-spend.js";
import type { PreparedNetworkIdAcceptedForced } from "./accepted-forced.js";
import {
  findNetworkIdFaults,
  type NetworkIdFaultClaim,
  type RetainedDaNetworkIdEvidence,
} from "./evidence.js";
import type { PreparedNetworkIdWrongfulRejection } from "./wrongful-rejection.js";

export type PreparedNetworkIdProof = {
  readonly headerHash: string;
  readonly expectedNetworkId: 0n | 1n;
  readonly badTxId: string;
  readonly nativeTxCanonicalCbor: string;
  readonly nativeTxCompactCbor: string;
  readonly outputsItemCbors: readonly string[];
  readonly faultClaim: NetworkIdFaultClaim;
  readonly fault: NetworkIdFault;
  readonly txInclusion: PreparedTxInclusionJson;
};

const wireFault = (claim: NetworkIdFaultClaim): NetworkIdFault =>
  claim.kind === "transaction-network"
    ? ("TransactionNetwork" as NetworkIdFault)
    : ({
        OutputNetwork: { output_index: claim.outputIndex },
      } as NetworkIdFault);

/**
 * Selects the first deterministic Q35 violation from authenticated public
 * block evidence and emits the exact native inclusion proof consumed by step
 * 01. A requested transaction must both exist and convict.
 */
export const prepareNetworkIdFromCanonicalEvidence = async ({
  evidence,
  expectedNetworkId,
  badTxId,
}: CanonicalEvidenceBuilderInput & {
  readonly expectedNetworkId: 0n | 1n;
  readonly badTxId?: string;
}): Promise<PreparedNetworkIdProof> => {
  const admitted = admitCanonicalEvidenceForProofBuild(evidence);
  const decoded = await Promise.all(
    admitted.transactions.map(decodeTransactionMaterial),
  );
  const requested = badTxId?.toLowerCase();
  if (requested !== undefined && !/^[0-9a-f]{64}$/u.test(requested)) {
    throw new Error("network-id badTxId must be 32-byte lowercase hex");
  }
  const candidates = decoded.flatMap((tx) => {
    if (requested !== undefined && tx.nodeTxId !== requested) return [];
    const retained: RetainedDaNetworkIdEvidence = {
      source: "retained-da",
      evidenceSourceId: evidence.provenance.da.sourceId,
      nativeTxCanonicalCbor: tx.txCbor,
    };
    return findNetworkIdFaults({
      evidence: retained,
      expectedNetworkId,
    }).map((faultClaim) => ({ tx, faultClaim }));
  });
  const selected = candidates[0];
  if (selected === undefined) {
    throw new Error(
      requested === undefined
        ? "authenticated retained DA contains no accepted network-id violation"
        : `transaction ${requested} does not contain an accepted network-id violation`,
    );
  }
  const nativeTrie = await buildTrieView(
    decoded.map(transactionSourceTrieItem),
  );
  await requireTransactionsRootMatch({
    sourceRoot: nativeTrie.root,
    expectedTransactionsRoot: admitted.expectedTransactionsRoot,
    count: BigInt(decoded.length),
  });
  const proofCbor = requireProof(
    nativeTrie,
    transactionSourceTrieItem(selected.tx).key,
    "network-id transaction",
  );
  return {
    headerHash: admitted.headerHash,
    expectedNetworkId,
    badTxId: selected.tx.nodeTxId,
    nativeTxCanonicalCbor: selected.tx.txCbor,
    nativeTxCompactCbor: selected.tx.nativeCompactCbor,
    outputsItemCbors: decodeMidgardFieldPreimage(
      selected.tx.nativeTx.body.outputsPreimageCbor,
    ).map((item) => Buffer.from(item).toString("hex")),
    faultClaim: selected.faultClaim,
    fault: wireFault(selected.faultClaim),
    txInclusion: {
      nativeTxId: selected.tx.nodeTxId,
      nativeTx: selected.tx.nativeTxCompact,
      nativeTxCompactCbor: selected.tx.nativeCompactCbor,
      l2TransactionSourceCbor: selected.tx.l2TransactionSourceCbor,
      transactionsPhasRoot: nativeTrie.root,
      txMembershipProofCbor: proofCbor,
    },
  };
};

/** Plans field-2's canonical tier for an output-network claim. */
export const planNetworkIdOutputsOpening = ({
  prepared,
  owner,
  publish = false,
}: {
  readonly prepared:
    | PreparedNetworkIdProof
    | PreparedNetworkIdWrongfulRejection
    | PreparedNetworkIdAcceptedForced;
  readonly owner: string;
  readonly publish?: boolean;
}): FaultProofFieldOpeningPlan => {
  if (
    prepared.faultClaim.kind !== "output-network" &&
    prepared.faultClaim.kind !== "forced-network-mismatch"
  ) {
    throw new Error(
      "transaction-network claims do not carry an outputs field opening",
    );
  }
  return planFaultProofFieldOpening({
    // A forced leaf of either verdict opens under the forced source.
    anchorSourceKind: "forcedSource" in prepared ? 1n : 0n,
    fieldIndex: 2,
    anchorTxId: prepared.badTxId,
    nativeTxCompactCbor: prepared.nativeTxCompactCbor,
    itemCbors: prepared.outputsItemCbors.map((item) =>
      Buffer.from(item, "hex"),
    ),
    owner,
    publish,
    label: "network-id outputs",
  });
};
