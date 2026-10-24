import {
  decodeMidgardAddressBytes,
  decodeMidgardFieldPreimage,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import {
  forcedVerdictSubject,
  isAnyNetworkIdMismatch,
  type NetworkIdFault,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { deriveTransactionOutputScan } from "../transaction-output-non-canonical/transaction-output-non-canonical.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import { verdictDetectionSubject } from "../workflow/detection-subject.js";

export const NETWORK_ID_MISMATCH_REASON = "NetworkIdMismatch" as const;
export const NETWORK_ID_WRONGFUL_REJECTION_VIOLATION_ID =
  "network-id-wrongful-rejection" as const;

export type NetworkIdWrongfulRejectionEvidence = Readonly<{
  subject: VerdictSubject;
  expectedNetworkId: 0n | 1n;
  committedNetworkId: bigint;
  outputNetworkIds: readonly (bigint | null)[];
  outputsItemCbors: readonly string[];
  outputsPreimageCbor: string;
}>;

export type PreparedNetworkIdWrongfulRejection = Readonly<{
  headerHash: string;
  expectedNetworkId: 0n | 1n;
  badTxId: string;
  nativeTxCompactCbor: string;
  outputsItemCbors: readonly string[];
  faultClaim: Readonly<{ kind: "forced-network-mismatch" }>;
  fault: NetworkIdFault;
  subject: VerdictSubject;
  forcedSource: Readonly<{
    header: CanonicalBlockEvidence["header"];
    membership: Awaited<
      ReturnType<typeof buildForcedTransactionLeafMembershipProof>
    >;
  }>;
  evidence: NetworkIdWrongfulRejectionEvidence;
}>;

type ForcedEntry =
  CanonicalBlockEvidence["reconstruction"]["forcedTransactions"][number];

/**
 * Opens a forced leaf's retained full transaction after checking it is the
 * one the leaf authenticates, returning the committed body network and every
 * output's network in field-2 order.
 */
export const openAuthenticatedForcedNetworks = (
  forced: Pick<ForcedEntry, "value" | "fullTransactionCbor">,
) => {
  const material = deriveMidgardForcedTxFaultEvidenceMaterial(
    forced.fullTransactionCbor,
  );
  if (
    material.transactionId.toString("hex") !== forced.value.tx_id ||
    material.proofSource.compactCbor.toString("hex") !==
      forced.value.submitted_source.compact_cbor ||
    material.proofSource.witnessSetCompactCbor.toString("hex") !==
      forced.value.submitted_source.witness_set_compact_cbor ||
    material.proofSource.fieldPreimageLengthsCbor.toString("hex") !==
      forced.value.submitted_source.field_preimage_lengths_cbor
  )
    throw new Error(
      "networkId: forced preimage differs from authenticated leaf",
    );
  const { canonical } = material;
  const outputs = decodeMidgardFieldPreimage(
    canonical.body.outputsPreimageCbor,
  );
  return Object.freeze({
    committedNetworkId: canonical.body.networkId,
    outputNetworkIds: Object.freeze(outputs.map(networkIdOfOutput)),
    outputsItemCbors: Object.freeze(
      outputs.map((item) => Buffer.from(item).toString("hex")),
    ),
    outputsPreimageCbor: canonical.body.outputsPreimageCbor.toString("hex"),
  });
};

const evidenceFor = ({
  block,
  forcedIndex,
  expectedNetworkId,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly forcedIndex: number;
  readonly expectedNetworkId: 0n | 1n;
}): NetworkIdWrongfulRejectionEvidence | null => {
  const forced = block.reconstruction.forcedTransactions[forcedIndex];
  if (
    forced === undefined ||
    forced.value.verdict === "ForcedTxValid" ||
    forced.value.verdict.ForcedTxInvalid.reason !== NETWORK_ID_MISMATCH_REASON
  )
    return null;
  const opened = openAuthenticatedForcedNetworks(forced);
  const subject = forcedVerdictSubject({
    transactionId: forced.value.tx_id,
    sourceKey: forced.key,
    rejectionReason: NETWORK_ID_MISMATCH_REASON,
  });
  return Object.freeze({
    subject,
    expectedNetworkId,
    ...opened,
  });
};

export const networkIdWrongfulRejectionCloses = (
  evidence: NetworkIdWrongfulRejectionEvidence,
): boolean =>
  evidence.outputNetworkIds.every(
    (network): network is bigint => network !== null,
  ) &&
  !isAnyNetworkIdMismatch({
    committedNetworkId: evidence.committedNetworkId,
    outputNetworkIds: evidence.outputNetworkIds,
    expectedNetworkId: evidence.expectedNetworkId,
  });

export const detectNetworkIdWrongfulRejections = ({
  block,
  expectedNetworkId,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly expectedNetworkId: 0n | 1n;
}) =>
  Object.freeze(
    block.reconstruction.forcedTransactions.flatMap((_, forcedIndex) => {
      const evidence = evidenceFor({ block, forcedIndex, expectedNetworkId });
      return evidence !== null && networkIdWrongfulRejectionCloses(evidence)
        ? [
            Object.freeze({
              ...verdictDetectionSubject(evidence.subject),
              detectionId: `${NETWORK_ID_WRONGFUL_REJECTION_VIOLATION_ID}:${forcedIndex.toString()}:${evidence.subject.transaction_id}`,
              headerHash: block.headerHash,
              violationId: NETWORK_ID_WRONGFUL_REJECTION_VIOLATION_ID,
              position: BigInt(forcedIndex),
              forcedIndex,
              transactionId: evidence.subject.transaction_id,
              evidence,
            }),
          ]
        : [];
    }),
  );

/**
 * Binds deployment configuration once.  The runtime-facing planner then has
 * the strict authority surface `{ block }`: verdict, reason, transaction,
 * evidence, membership, and actuator inputs are all derived internally.
 */
export const createNetworkIdWrongfulRejectionPlanner =
  (expectedNetworkId: 0n | 1n) =>
  async ({
    block,
  }: Readonly<{
    block: CanonicalBlockEvidence;
  }>): Promise<PreparedNetworkIdWrongfulRejection> => {
    const detection = detectNetworkIdWrongfulRejections({
      block,
      expectedNetworkId,
    })[0];
    if (detection === undefined)
      throw new Error("networkId: no authenticated wrongful rejection");
    const forced =
      block.reconstruction.forcedTransactions[detection.forcedIndex]!;
    const membership = await buildForcedTransactionLeafMembershipProof({
      reconstruction: block.reconstruction,
      eventKey: {
        ForcedTransactionEventKey: { tx_order_id: forced.key },
      },
    });
    return Object.freeze({
      headerHash: block.headerHash,
      expectedNetworkId,
      badTxId: forced.value.tx_id,
      nativeTxCompactCbor: forced.value.submitted_source.compact_cbor,
      outputsItemCbors: detection.evidence.outputsItemCbors,
      faultClaim: Object.freeze({ kind: "forced-network-mismatch" as const }),
      fault: "ForcedNetworkIdMismatch" as NetworkIdFault,
      subject: detection.evidence.subject,
      forcedSource: Object.freeze({
        header: block.header,
        membership,
      }),
      evidence: detection.evidence,
    });
  };

/** An unreadable output remains the output-shape family's claim. */
export const networkIdOfOutput = (item: Buffer): bigint | null => {
  const scan = deriveTransactionOutputScan(item);
  return scan.canonical
    ? BigInt(
        decodeMidgardAddressBytes(
          scan.controls[scan.controls.length - 1]!.address,
        ).networkId,
      )
    : null;
};
