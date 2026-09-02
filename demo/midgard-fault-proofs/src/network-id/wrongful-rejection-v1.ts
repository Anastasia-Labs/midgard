import {
  decodeMidgardAddressBytes,
  decodeMidgardFieldPreimageV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  decodeMidgardOutputFieldPreimageV1,
  deriveMidgardNativeTxFaultEvidenceMaterialV1,
} from "@al-ft/midgard-core";
import {
  forcedVerdictSubjectV1,
  isAnyNetworkIdMismatchV1,
  type NetworkIdFaultV1,
  type VerdictSubjectV1,
} from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";

export const NETWORK_ID_MISMATCH_REASON_V1 = "NetworkIdMismatch" as const;
export const NETWORK_ID_WRONGFUL_REJECTION_VIOLATION_ID_V1 =
  "network-id-wrongful-rejection" as const;

export type NetworkIdWrongfulRejectionEvidenceV1 = Readonly<{
  subject: VerdictSubjectV1;
  expectedNetworkId: 0n | 1n;
  committedNetworkId: bigint;
  outputNetworkIds: readonly bigint[];
  outputsItemCbors: readonly string[];
  outputsPreimageCbor: string;
}>;

export type PreparedNetworkIdWrongfulRejectionV1 = Readonly<{
  headerHash: string;
  expectedNetworkId: 0n | 1n;
  badTxId: string;
  nativeTxCompactCbor: string;
  outputsItemCbors: readonly string[];
  faultClaim: Readonly<{ kind: "forced-network-mismatch" }>;
  fault: NetworkIdFaultV1;
  subject: VerdictSubjectV1;
  forcedSource: Readonly<{
    header: CanonicalBlockEvidenceV1["header"];
    membership: Awaited<
      ReturnType<typeof buildForcedTransactionLeafMembershipProof>
    >;
    direction: 1n;
  }>;
  evidence: NetworkIdWrongfulRejectionEvidenceV1;
}>;

const evidenceFor = ({
  block,
  forcedIndex,
  expectedNetworkId,
}: {
  readonly block: CanonicalBlockEvidenceV1;
  readonly forcedIndex: number;
  readonly expectedNetworkId: 0n | 1n;
}): NetworkIdWrongfulRejectionEvidenceV1 | null => {
  const forced = block.reconstruction.forcedTransactions[forcedIndex];
  if (
    forced === undefined ||
    forced.value.verdict === "ForcedTxValid" ||
    forced.value.verdict.ForcedTxInvalid.reason !==
      NETWORK_ID_MISMATCH_REASON_V1
  )
    return null;
  const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
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
  const decoded = decodeMidgardNativeTxFullV1FromCanonicalCbor(
    forced.fullTransactionCbor,
  );
  const outputs = decodeMidgardOutputFieldPreimageV1(
    decoded.body.outputsPreimageCbor,
  );
  const outputNetworkIds = outputs.map((output) =>
    BigInt(decodeMidgardAddressBytes(output.address).networkId),
  );
  const subject = forcedVerdictSubjectV1({
    transactionId: forced.value.tx_id,
    sourceKey: forced.key,
    rejectionReason: NETWORK_ID_MISMATCH_REASON_V1,
  });
  return Object.freeze({
    subject,
    expectedNetworkId,
    committedNetworkId: decoded.body.networkId,
    outputNetworkIds: Object.freeze(outputNetworkIds),
    outputsItemCbors: Object.freeze(
      decodeMidgardFieldPreimageV1(decoded.body.outputsPreimageCbor).map(
        (item) => Buffer.from(item).toString("hex"),
      ),
    ),
    outputsPreimageCbor: decoded.body.outputsPreimageCbor.toString("hex"),
  });
};

export const networkIdWrongfulRejectionClosesV1 = (
  evidence: NetworkIdWrongfulRejectionEvidenceV1,
): boolean =>
  !isAnyNetworkIdMismatchV1({
    committedNetworkId: evidence.committedNetworkId,
    outputNetworkIds: evidence.outputNetworkIds,
    expectedNetworkId: evidence.expectedNetworkId,
  });

export const detectNetworkIdWrongfulRejectionsV1 = ({
  block,
  expectedNetworkId,
}: {
  readonly block: CanonicalBlockEvidenceV1;
  readonly expectedNetworkId: 0n | 1n;
}) =>
  Object.freeze(
    block.reconstruction.forcedTransactions.flatMap((_, forcedIndex) => {
      const evidence = evidenceFor({ block, forcedIndex, expectedNetworkId });
      return evidence !== null && networkIdWrongfulRejectionClosesV1(evidence)
        ? [
            Object.freeze({
              detectionId: `${NETWORK_ID_WRONGFUL_REJECTION_VIOLATION_ID_V1}:${forcedIndex.toString()}:${evidence.subject.transaction_id}`,
              headerHash: block.headerHash,
              violationId: NETWORK_ID_WRONGFUL_REJECTION_VIOLATION_ID_V1,
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
export const createNetworkIdWrongfulRejectionPlannerV1 =
  (expectedNetworkId: 0n | 1n) =>
  async ({
    block,
  }: Readonly<{
    block: CanonicalBlockEvidenceV1;
  }>): Promise<PreparedNetworkIdWrongfulRejectionV1> => {
    const detection = detectNetworkIdWrongfulRejectionsV1({
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
      fault: "ForcedNetworkIdMismatch" as NetworkIdFaultV1,
      subject: detection.evidence.subject,
      forcedSource: Object.freeze({
        header: block.header,
        membership,
        direction: 1n,
      }),
      evidence: detection.evidence,
    });
  };
