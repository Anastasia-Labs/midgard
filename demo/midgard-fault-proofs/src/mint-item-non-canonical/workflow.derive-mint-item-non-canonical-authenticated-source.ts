import { deriveMidgardNativeTxFaultEvidenceMaterial } from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import {
  type FraudProofCatalogueCategoryName,
  OutputReferenceSchema,
  PROOF_THREAD_SOURCE_KIND_ACCEPTED,
  PROOF_THREAD_SOURCE_KIND_FORCED,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { requireLinearFaultThreadUtxo } from "../linear-fault-family.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  requireProof,
  transactionSourceTrieItem,
} from "../prepare-double-spend.js";
import { parseSubmitStep01TxInclusion } from "../step-support.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import {
  type MintItemEvidence,
  type MintItemJournal,
  type MintItemStage,
} from "./mint-item-non-canonical.js";
import {
  type LoadManifestBoundMintItemNonCanonicalConfig,
  type ManifestBoundMintItemNonCanonicalConfig,
  type MintItemNonCanonicalAuthenticatedSource,
  type MintItemNonCanonicalStage,
} from "./workflow.load-manifest-bound-mint-item-non-canonical-config.js";

/** Rebuilds all accepted/forced submitter material from the authenticated block. */
export const deriveMintItemNonCanonicalAuthenticatedSource = async ({
  block,
  evidence,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly evidence: MintItemEvidence;
}): Promise<MintItemNonCanonicalAuthenticatedSource> => {
  if (evidence.subject.source_kind === PROOF_THREAD_SOURCE_KIND_ACCEPTED) {
    const decoded = await Promise.all(
      block.transactions.map(decodeTransactionMaterial),
    );
    const selected = decoded.find(
      ({ nodeTxId }) => nodeTxId === evidence.subject.transaction_id,
    );
    if (selected === undefined) {
      throw new Error(
        "mintItemNonCanonical accepted subject disappeared from retained DA",
      );
    }
    const trie = await buildTrieView(decoded.map(transactionSourceTrieItem));
    if (
      trie.root !== block.reconstruction.rootData.transactions.phasRoot ||
      trie.root !== block.inclusionRootAuthentication.sourceValuePhasRoot
    ) {
      throw new Error(
        "mintItemNonCanonical accepted source trie differs from authenticated reconstruction",
      );
    }
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
      Buffer.from(selected.txCbor, "hex"),
    );
    return Object.freeze({
      nativeTxCompactCbor: material.proofSource.compactCbor.toString("hex"),
      witnessSetCompactCbor:
        material.proofSource.witnessSetCompactCbor.toString("hex"),
      acceptedInclusion: parseSubmitStep01TxInclusion({
        nativeTxId: selected.nodeTxId,
        nativeTx: selected.nativeTxCompact,
        nativeTxCompactCbor: selected.nativeCompactCbor,
        l2TransactionSourceCbor: selected.l2TransactionSourceCbor,
        transactionsPhasRoot: trie.root,
        txMembershipProofCbor: requireProof(
          trie,
          Buffer.from(selected.nodeTxId, "hex"),
          "mintItemNonCanonical accepted transaction",
        ),
      }),
    });
  }
  const forced = block.reconstruction.forcedTransactions.find(
    ({ key, value }) =>
      value.tx_id === evidence.subject.transaction_id &&
      Data.to(key as never, OutputReferenceSchema as never) ===
        evidence.subject.source_key,
  );
  if (forced === undefined || forced.value.verdict !== "ForcedTxValid") {
    throw new Error(
      "mintItemNonCanonical forced subject disappeared from retained DA",
    );
  }
  if (evidence.subject.rejection_reason !== null)
    throw new Error(
      "mintItemNonCanonical does not adjudicate rejected transactions",
    );
  const material = deriveMidgardForcedTxFaultEvidenceMaterial(
    forced.fullTransactionCbor,
  );
  if (
    material.proofSource.compactCbor.toString("hex") !==
      forced.value.submitted_source.compact_cbor ||
    material.proofSource.witnessSetCompactCbor.toString("hex") !==
      forced.value.submitted_source.witness_set_compact_cbor ||
    material.proofSource.fieldPreimageLengthsCbor.toString("hex") !==
      forced.value.submitted_source.field_preimage_lengths_cbor
  ) {
    throw new Error(
      "mintItemNonCanonical forced source material differs from authenticated leaf",
    );
  }
  const eventKey = {
    ForcedTransactionEventKey: { tx_order_id: forced.key },
  } as const;
  return Object.freeze({
    nativeTxCompactCbor: material.proofSource.compactCbor.toString("hex"),
    witnessSetCompactCbor:
      material.proofSource.witnessSetCompactCbor.toString("hex"),
    forcedHeader: block.header,
    forcedMembership: await buildForcedTransactionLeafMembershipProof({
      reconstruction: block.reconstruction,
      eventKey,
    }),
    forcedDirection: 0n,
  });
};

export type MintItemNonCanonicalRuntimeLoader = Readonly<{
  config: LoadManifestBoundMintItemNonCanonicalConfig;
  journal: MintItemJournal;
  observe: (identity: string) => Promise<MintItemStage>;
  resolveStage: (input: {
    readonly action:
      | "submitInit"
      | "submitStep01"
      | "submitStep02"
      | "submitStep03"
      | "submitStep04"
      | "removeDescendants"
      | "cancel";
    readonly evidence: MintItemEvidence;
  }) => Promise<MintItemNonCanonicalStage>;
}>;

export const createMintItemNonCanonicalRawL1StageResolver =
  ({
    config,
    l1,
    source,
  }: {
    readonly config: ManifestBoundMintItemNonCanonicalConfig;
    readonly l1: FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>;
    readonly source: MintItemNonCanonicalAuthenticatedSource;
  }): MintItemNonCanonicalRuntimeLoader["resolveStage"] =>
  async ({ action, evidence }) => {
    const observed = await l1.observe({
      headerHash: config.binding.definition.headerHash,
    });
    const stage = observed.stage;
    if (action === "submitInit") {
      if (stage.kind !== "not_started") {
        throw new Error(
          "mintItemNonCanonical init requires raw-L1 not_started",
        );
      }
      return { fraudulentBlockOutRef: stage.stateQueueBlockOutRef };
    }
    if (action === "removeDescendants") {
      if (stage.kind !== "proof_token") {
        throw new Error(
          "mintItemNonCanonical removal requires raw-L1 proof token",
        );
      }
      return {
        fraudulentBlockOutRef: stage.stateQueueBlockOutRef,
        nextRemovalOutRef: stage.nextRemovalOutRef,
        fraudProofOutRef: stage.fraudProofOutRef,
      };
    }
    const expectedStep =
      action === "submitStep01"
        ? 1
        : action === "submitStep02"
          ? 2
          : action === "submitStep03"
            ? 3
            : 4;
    if (stage.kind !== "step" || stage.step !== expectedStep) {
      throw new Error(
        `mintItemNonCanonical ${action} differs from authenticated raw-L1 stage`,
      );
    }
    const common = {
      fraudulentBlockOutRef: stage.stateQueueBlockOutRef,
      threadOutRef: stage.threadOutRef,
      nativeTxCompactCbor: source.nativeTxCompactCbor,
      witnessSetCompactCbor: source.witnessSetCompactCbor,
    };
    if (action !== "submitStep01") return common;
    if (evidence.subject.source_kind === PROOF_THREAD_SOURCE_KIND_FORCED) {
      return {
        ...common,
        forcedHeader: required(
          source.forcedHeader,
          "authenticated forced header",
        ),
        forcedMembership: required(
          source.forcedMembership,
          "authenticated forced membership",
        ),
        forcedDirection: required(
          source.forcedDirection,
          "authenticated forced direction",
        ),
      };
    }
    const thread = await requireLinearFaultThreadUtxo({
      lucid: config.lucid,
      contracts: config.contracts,
      categoryId: config.binding.resolvedContracts.category.categoryId,
      family: "mint-item-non-canonical",
      stepIndex: 0,
      threadOutRef: stage.threadOutRef,
    });
    return {
      ...common,
      threadUtxo: thread.threadUtxo,
      threadToken: thread.threadToken,
      stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
      acceptedInclusion: required(
        source.acceptedInclusion,
        "authenticated accepted inclusion",
      ),
    };
  };

export const required = <T>(value: T | undefined, label: string): T => {
  if (value === undefined)
    throw new Error(`mintItemNonCanonical missing ${label}`);
  return value;
};
