/** Authenticated retained-DA preparation for the Q35 network-id family. */
import {
  decodeMidgardAddressBytes,
  decodeMidgardFieldPreimage,
  decodeMidgardLedgerOutputCommitment,
  decodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import {
  EMPTY_MERKLE_TREE_ROOT,
  type NetworkIdFault,
  type NetworkIdPostUtxoPredecessor,
  type OutputReference,
  Proof,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import {
  blockTransactionsFromCanonicalEvidence,
  type CanonicalBlockEvidence,
} from "../evidence/canonical-block-evidence-v1.js";
import {
  admitCanonicalEvidenceForProofBuild,
  type CanonicalEvidenceBuilderInput,
} from "../evidence/prepare-from-evidence-v1.js";
import type { FaultProofFieldOpeningPlan } from "../field-opening-v1.js";
import { planFaultProofFieldOpening } from "../field-opening-v1.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  type PreparedTxInclusionJson,
  requireProof,
  requireTransactionsRootMatch,
  transactionSourceTrieItem,
} from "../prepare-double-spend.js";
import {
  keyValuePhasNonMembershipProof,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../transition-trace/phas.js";
import {
  findNetworkIdFaults,
  type NetworkIdFaultClaim,
  type RetainedDaNetworkIdEvidence,
} from "./evidence-v1.js";
import type { PreparedNetworkIdWrongfulRejection } from "./wrongful-rejection-v1.js";

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

export type NetworkIdPostUtxoFaultClaim = {
  readonly kind: "post-utxo-network";
  readonly outRef: OutputReference;
  readonly observedNetworkId: bigint;
};

/**
 * A post-block UTxO claim authenticates the compact descriptor committed by
 * `header.utxos_root`; it never carries or reconstructs the full output in the
 * proof transaction.
 */
export type PreparedNetworkIdPostUtxoProof = {
  readonly headerHash: string;
  readonly expectedNetworkId: 0n | 1n;
  readonly outRef: OutputReference;
  readonly outRefKeyCbor: string;
  readonly descriptorCbor: string;
  readonly postUtxosRoot: string;
  readonly prevUtxosRoot: string;
  readonly membershipProofCbor: string;
  readonly membershipProof: Proof;
  readonly predecessor: NetworkIdPostUtxoPredecessor;
  readonly predecessorProof: Proof;
  readonly predecessorProofCbor: string;
  readonly faultClaim: NetworkIdPostUtxoFaultClaim;
  readonly fault: NetworkIdFault;
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

const sameOutRef = (left: OutputReference, right: OutputReference): boolean =>
  left.transactionId === right.transactionId &&
  left.outputIndex === right.outputIndex;

/**
 * Selects a wrong-network member of the authenticated post-block ledger. This
 * covers zero-transaction fabricated post-state entries and entries whose
 * network was changed by this transition. An unchanged inherited violation is
 * deliberately excluded: descendants are corrected with their fraudulent
 * ancestor, but are not blamed for introducing its fault.
 */
export const prepareNetworkIdPostUtxoFromCanonicalEvidence = async ({
  evidence,
  previousBlockEvidence,
  expectedNetworkId,
  outRef,
}: CanonicalEvidenceBuilderInput & {
  readonly previousBlockEvidence?: CanonicalBlockEvidence;
  readonly expectedNetworkId: 0n | 1n;
  readonly outRef?: OutputReference;
}): Promise<PreparedNetworkIdPostUtxoProof> => {
  // Reuses the canonical evidence admission without imposing the transaction
  // inclusion convention, which this independent post-state route does not use.
  blockTransactionsFromCanonicalEvidence(evidence);
  if (previousBlockEvidence !== undefined) {
    blockTransactionsFromCanonicalEvidence(previousBlockEvidence);
    if (previousBlockEvidence.headerHash !== evidence.header.prevHeaderHash) {
      throw new Error(
        "network-id previous block evidence is not the predecessor named by the challenged header",
      );
    }
    if (
      previousBlockEvidence.header.utxosRoot !== evidence.header.prevUtxosRoot
    ) {
      throw new Error(
        "network-id previous block evidence does not authenticate the challenged prev_utxos_root",
      );
    }
  } else if (evidence.header.prevUtxosRoot !== EMPTY_MERKLE_TREE_ROOT) {
    throw new Error(
      "network-id post-UTxO preparation requires authenticated predecessor evidence for a non-empty prev_utxos_root",
    );
  }
  const descriptorMembers = (
    entries: CanonicalBlockEvidence["reconstruction"]["utxos"],
  ) =>
    entries.map((entry) => {
      const decodedOutRef = decodeMidgardSpendInputItem(entry.key);
      const material = buildCanonicalMidgardLedgerEntryOutputMaterial({
        outRef: entry.key,
        outputCbor: entry.value,
      });
      const descriptor = decodeMidgardLedgerOutputCommitment(
        material.descriptorCbor,
      );
      if (descriptor.outputIndex !== decodedOutRef.outputIndex) {
        throw new Error(
          "network-id post-UTxO descriptor index does not match its ledger out-ref",
        );
      }
      const memberOutRef: OutputReference = {
        transactionId: Buffer.from(decodedOutRef.txId).toString("hex"),
        outputIndex: BigInt(decodedOutRef.outputIndex),
      };
      return {
        key: Buffer.from(entry.key),
        value: Buffer.from(material.descriptorCbor),
        outRef: memberOutRef,
        observedNetworkId: BigInt(
          decodeMidgardAddressBytes(descriptor.address).networkId,
        ),
      };
    });
  const postMembers = descriptorMembers(evidence.reconstruction.utxos);
  const priorMembers = descriptorMembers(
    previousBlockEvidence?.reconstruction.utxos ?? [],
  );
  const postTrie = await keyValuePhasRootWithCount(postMembers);
  const priorTrie = await keyValuePhasRootWithCount(priorMembers);
  if (postTrie.root !== evidence.header.utxosRoot) {
    throw new Error(
      `network-id post-UTxO ledger root ${postTrie.root} does not match authenticated header.utxos_root ${evidence.header.utxosRoot}`,
    );
  }
  if (priorTrie.root !== evidence.header.prevUtxosRoot) {
    throw new Error(
      `network-id predecessor ledger root ${priorTrie.root} does not match authenticated header.prev_utxos_root ${evidence.header.prevUtxosRoot}`,
    );
  }
  const selected = postMembers
    .filter(
      (member) =>
        (outRef === undefined || sameOutRef(member.outRef, outRef)) &&
        member.observedNetworkId !== expectedNetworkId,
    )
    .map((post) => ({
      post,
      previous: priorMembers.find((prior) => prior.key.equals(post.key)),
    }))
    .find(
      ({ post, previous }) =>
        previous === undefined ||
        (previous.observedNetworkId === expectedNetworkId &&
          !previous.value.equals(post.value)),
    );
  if (selected === undefined) {
    throw new Error(
      outRef === undefined
        ? "authenticated post-block ledger contains no network-id violation introduced by this transition"
        : `post-block UTxO ${outRef.transactionId}#${outRef.outputIndex.toString()} is absent, valid, or inherited unchanged from its predecessor`,
    );
  }
  const membershipProof = await keyValuePhasProof(
    postTrie,
    selected.post.key,
    selected.post.value,
  );
  let predecessor: NetworkIdPostUtxoPredecessor;
  let predecessorProof: Proof;
  let predecessorProofCbor: string;
  if (selected.previous === undefined) {
    const proof = await keyValuePhasNonMembershipProof(
      priorTrie,
      selected.post.key,
    );
    predecessor = "Introduced";
    predecessorProof = proof;
    predecessorProofCbor = Data.to(proof, Proof);
  } else {
    const proof = await keyValuePhasProof(
      priorTrie,
      selected.previous.key,
      selected.previous.value,
    );
    predecessor = {
      NetworkChanged: {
        previous_descriptor_cbor: selected.previous.value.toString("hex"),
      },
    };
    predecessorProof = proof;
    predecessorProofCbor = Data.to(proof, Proof);
  }
  const faultClaim: NetworkIdPostUtxoFaultClaim = {
    kind: "post-utxo-network",
    outRef: selected.post.outRef,
    observedNetworkId: selected.post.observedNetworkId,
  };
  const fault = {
    OutputNetworkUtxo: {
      observed_network_id: selected.post.observedNetworkId,
    },
  } as NetworkIdFault;
  return {
    headerHash: evidence.headerHash,
    expectedNetworkId,
    outRef: selected.post.outRef,
    outRefKeyCbor: selected.post.key.toString("hex"),
    descriptorCbor: selected.post.value.toString("hex"),
    postUtxosRoot: postTrie.root,
    prevUtxosRoot: priorTrie.root,
    membershipProofCbor: Data.to(membershipProof, Proof),
    membershipProof,
    predecessor,
    predecessorProof,
    predecessorProofCbor,
    faultClaim,
    fault,
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
    | PreparedNetworkIdWrongfulRejection;
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
