import {
  decodeMidgardAddressBytes,
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardOutputFieldPreimage,
  deriveMidgardNativeTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core";
import {
  forcedVerdictSubject,
  isAnyNetworkIdMismatch,
  type NetworkIdFault,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";

export const NETWORK_ID_MISMATCH_REASON = "NetworkIdMismatch" as const;
export const NETWORK_ID_WRONGFUL_REJECTION_VIOLATION_ID =
  "network-id-wrongful-rejection" as const;

export type NetworkIdWrongfulRejectionEvidence = Readonly<{
  subject: VerdictSubject;
  expectedNetworkId: 0n | 1n;
  committedNetworkId: bigint;
  outputNetworkIds: readonly bigint[];
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
    direction: 1n;
  }>;
  evidence: NetworkIdWrongfulRejectionEvidence;
}>;

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
  const material = deriveMidgardNativeTxFaultEvidenceMaterial(
    forced.fullTransactionCbor,
  );
  if (
    material.transactionId.toString("hex") !== forced.value.tx_id ||
    material.proofSource.compactCbor.toString("hex") !==
      forced.value.source.compact_cbor ||
    material.proofSource.witnessSetCompactCbor.toString("hex") !==
      forced.value.source.witness_set_compact_cbor ||
    material.proofSource.fieldPreimageLengthsCbor.toString("hex") !==
      forced.value.source.field_preimage_lengths_cbor
  )
    throw new Error(
      "networkId: forced preimage differs from authenticated leaf",
    );
  const decoded = decodeMidgardNativeTxFullFromCanonicalCbor(
    forced.fullTransactionCbor,
  );
  const outputs = decodeMidgardOutputFieldPreimage(
    decoded.body.outputsPreimageCbor,
  );
  const outputNetworkIds = outputs.map((output) =>
    BigInt(decodeMidgardAddressBytes(output.address).networkId),
  );
  const subject = forcedVerdictSubject({
    transactionId: forced.value.tx_id,
    sourceKey: forced.key,
    rejectionReason: NETWORK_ID_MISMATCH_REASON,
  });
  return Object.freeze({
    subject,
    expectedNetworkId,
    committedNetworkId: decoded.body.networkId,
    outputNetworkIds: Object.freeze(outputNetworkIds),
    outputsItemCbors: Object.freeze(
      decodeMidgardFieldPreimage(decoded.body.outputsPreimageCbor).map((item) =>
        Buffer.from(item).toString("hex"),
      ),
    ),
    outputsPreimageCbor: decoded.body.outputsPreimageCbor.toString("hex"),
  });
};

export const networkIdWrongfulRejectionCloses = (
  evidence: NetworkIdWrongfulRejectionEvidence,
): boolean =>
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
      nativeTxCompactCbor: forced.value.source.compact_cbor,
      outputsItemCbors: detection.evidence.outputsItemCbors,
      faultClaim: Object.freeze({ kind: "forced-network-mismatch" as const }),
      fault: "ForcedNetworkIdMismatch" as NetworkIdFault,
      subject: detection.evidence.subject,
      forcedSource: Object.freeze({
        header: block.header,
        membership,
        direction: 1n,
      }),
      evidence: detection.evidence,
    });
  };
