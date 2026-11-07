import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { stringifyJson } from "./json-file.js";
import { buildTrieView, requireProof } from "./prepare-double-spend.js";
import {
  classifyFabricatedWithdrawalFault,
  decodeCommittedWithdrawalLeaf,
  FABRICATED_WITHDRAWAL_EVIDENCE_SCHEMA_VERSION,
  FabricatedWithdrawalRejection,
  hexOf,
  type PreparedFabricatedWithdrawalInclusionJson,
  type PreparedFabricatedWithdrawalOutput,
  type PrepareFabricatedWithdrawalFromCommittedLeavesOptions,
} from "./prepare-fabricated-withdrawal.classify-fabricated-withdrawal-fault.js";
import {
  commitCountedRoot,
  keyValuePhasRootWithCount,
} from "./transition-trace/phas.js";

/**
 * Core builder: authenticates the raw committed withdrawal leaves against the
 * header's counted `withdrawals_root` **and** `withdrawal_count`, classifies one
 * leaf against the authenticated L1 witness, then emits the membership proof, the
 * retained content opening, and the exact step-02 state.
 */
export const prepareFabricatedWithdrawalFromCommittedLeaves = async ({
  headerHash,
  committedWithdrawalsRoot,
  withdrawalCount,
  headerStartTime,
  headerEndTime,
  entries,
  witness,
  committedWithdrawalIdCbor,
  minimumConfirmationDepth,
  outputDir,
}: PrepareFabricatedWithdrawalFromCommittedLeavesOptions): Promise<PreparedFabricatedWithdrawalOutput> => {
  const phasEntries = entries.map(([keyHex, valueHex], index) => ({
    key: hexOf(keyHex, `withdrawals[${index.toString()}].key`),
    value: hexOf(valueHex, `withdrawals[${index.toString()}].value`),
  }));
  const phas = await keyValuePhasRootWithCount(phasEntries);
  const countedRoot = await commitCountedRoot({
    domain: SDK.ROOT_DOMAINS.withdrawals,
    phasRoot: phas.root,
    count: phas.count,
  });
  if (
    countedRoot !== committedWithdrawalsRoot.toLowerCase() ||
    phas.count !== withdrawalCount
  ) {
    throw new FabricatedWithdrawalRejection(
      "withdrawals_root_mismatch",
      `header_withdrawals_root=${committedWithdrawalsRoot.toLowerCase()} derived=${countedRoot} header_count=${withdrawalCount.toString()} derived_count=${phas.count.toString()}`,
    );
  }

  const leaves = await Promise.all(
    entries.map(async ([keyHex, valueHex], index) =>
      decodeCommittedWithdrawalLeaf(keyHex, valueHex, index),
    ),
  );
  if (leaves.length === 0) {
    throw new FabricatedWithdrawalRejection(
      "no_committed_withdrawal_leaf",
      `header_hash=${headerHash.toLowerCase()} commits an empty withdrawal source set`,
    );
  }
  const challengedLeaf =
    committedWithdrawalIdCbor === undefined
      ? leaves[0]
      : leaves.find(
          (leaf) =>
            leaf.committedWithdrawalIdCbor ===
            committedWithdrawalIdCbor.toLowerCase(),
        );
  if (challengedLeaf === undefined) {
    throw new FabricatedWithdrawalRejection(
      "leaf_not_committed",
      `committed_withdrawal_id=${committedWithdrawalIdCbor?.toLowerCase() ?? ""} is not a committed leaf of header_hash=${headerHash.toLowerCase()} (leaf_count=${leaves.length.toString()})`,
    );
  }

  const classification = await classifyFabricatedWithdrawalFault({
    leaf: challengedLeaf,
    headerStartTime,
    headerEndTime,
    witness,
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
  });

  const trie = await buildTrieView(phasEntries);
  const withdrawalInclusion: PreparedFabricatedWithdrawalInclusionJson = {
    committedWithdrawalIdCbor: challengedLeaf.committedWithdrawalIdCbor,
    committedWithdrawalInfoCbor: challengedLeaf.committedWithdrawalInfoCbor,
    withdrawalsPhasRoot: phas.root,
    withdrawalMembershipProofCbor: requireProof(
      trie,
      Buffer.from(challengedLeaf.committedWithdrawalIdCbor, "hex"),
      "committed withdrawal leaf",
    ),
  };
  const output: PreparedFabricatedWithdrawalOutput = {
    schemaVersion: FABRICATED_WITHDRAWAL_EVIDENCE_SCHEMA_VERSION,
    violationId: SDK.FABRICATED_WITHDRAWAL_VIOLATION_ID,
    fraudCategoryId: SDK.FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID,
    headerHash: headerHash.toLowerCase(),
    threadTokenAssetName: SDK.fabricatedWithdrawalThreadTokenAssetName(
      headerHash.toLowerCase(),
    ),
    withdrawalCount: Number(withdrawalCount),
    withdrawalsPhasRoot: phas.root,
    committedWithdrawalsRoot: countedRoot,
    headerStartTime,
    headerEndTime,
    leaves,
    challengedLeaf,
    classification,
    withdrawalInclusion,
    authenticContent: {
      openingCbor: classification.openingCbor,
    },
    step02State: {
      stateQueuePolicyId: classification.stateQueuePolicyId,
      challengedHeaderHash: headerHash.toLowerCase(),
      headerStartTime: headerStartTime.toString(),
      headerEndTime: headerEndTime.toString(),
      committedWithdrawalIdCbor: challengedLeaf.committedWithdrawalIdCbor,
      committedWithdrawalContentHash:
        challengedLeaf.committedWithdrawalContentHash,
    },
  };
  if (outputDir === undefined) {
    return output;
  }
  await mkdir(outputDir, { recursive: true });
  const paths = {
    withdrawalInclusionPath: join(outputDir, "withdrawal-inclusion.json"),
    authenticContentPath: join(outputDir, "authentic-content.json"),
    planPath: join(outputDir, "plan.json"),
  };
  await Promise.all([
    writeFile(
      paths.withdrawalInclusionPath,
      stringifyJson(output.withdrawalInclusion),
    ),
    writeFile(
      paths.authenticContentPath,
      stringifyJson(output.authenticContent),
    ),
    writeFile(
      paths.planPath,
      stringifyJson({
        schemaVersion: output.schemaVersion,
        violationId: output.violationId,
        fraudCategoryId: output.fraudCategoryId,
        headerHash: output.headerHash,
        threadTokenAssetName: output.threadTokenAssetName,
        withdrawalsPhasRoot: output.withdrawalsPhasRoot,
        committedWithdrawalsRoot: output.committedWithdrawalsRoot,
        withdrawalCount: output.withdrawalCount,
        step02State: output.step02State,
      }),
    ),
  ]);
  return { ...output, files: paths };
};

export type FabricatedWithdrawalBlockEvidence = {
  readonly grade: SDK.EvidenceGrade;
  readonly provenance: {
    readonly l1: SDK.EvidenceProvenance;
    readonly da: SDK.EvidenceProvenance;
  };
  readonly headerHash: string;
  readonly payloadEnvelopeSha256: string;
  readonly payloadSha256: string;
  readonly committedWithdrawalsRoot: string;
  readonly withdrawalCount: bigint;
  readonly headerStartTime: bigint;
  readonly headerEndTime: bigint;
  readonly entries: readonly (readonly [string, string])[];
};

/**
 * Extracts the committed `withdrawals` leaves from public retained-DA bytes
 * without requiring the block to be well-formed. Only the payload envelope,
 * canonical `DaPayload` framing and the embedded-header identity are enforced
 * here; leaf authenticity is what the family adjudicates.
 */
export const fabricatedWithdrawalBlockEvidenceFromVerifiedPayload = async ({
  observation,
  payloadEnvelopeCbor,
  daProvenance,
  minimumConfirmationDepth,
}: {
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservation;
  readonly payloadEnvelopeCbor: Uint8Array;
  readonly daProvenance: SDK.EvidenceProvenance;
  readonly minimumConfirmationDepth?: number;
}): Promise<FabricatedWithdrawalBlockEvidence> => {
  const admittedObservation =
    await SDK.admitAuthenticatedStateQueueHeaderObservation({
      observation,
      ...(minimumConfirmationDepth === undefined
        ? {}
        : { minimumConfirmationDepth }),
    });
  const admittedDa = SDK.assertSecurityGradeEvidence(daProvenance);
  if (admittedDa.trustClass !== "public_or_permissionless_da") {
    throw new SDK.CanonicalEvidenceRejection(
      "da_evidence_wrong_trust_class",
      `expected=public_or_permissionless_da actual=${admittedDa.trustClass}`,
    );
  }

  let payloadCbor: Buffer;
  try {
    payloadCbor = Buffer.from(
      (
        await unwrapDaPayload(payloadEnvelopeCbor, {
          maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
        })
      ).innerBytes,
    );
  } catch (cause) {
    throw new FabricatedWithdrawalRejection(
      "malformed_da_payload",
      `failed to decode the mandatory DaPayloadEnvelopeV1: ${String(cause)}`,
    );
  }
  let payload: SDK.DaPayload;
  try {
    payload = SDK.decodeDaPayload(payloadCbor);
  } catch (cause) {
    throw new FabricatedWithdrawalRejection(
      "malformed_da_payload",
      `failed to decode DaPayloadV1 canonical CBOR: ${String(cause)}`,
    );
  }
  if (!SDK.encodeDaPayload(payload).equals(payloadCbor)) {
    throw new FabricatedWithdrawalRejection(
      "non_canonical_da_payload",
      "DA payload CBOR is not canonical for DaPayloadV1",
    );
  }
  if (payload.version !== SDK.DA_PAYLOAD_VERSION) {
    throw new FabricatedWithdrawalRejection(
      "wrong_da_payload_version",
      `expected=${SDK.DA_PAYLOAD_VERSION.toString()} actual=${payload.version.toString()}`,
    );
  }
  const body = payload.block_body;
  const embeddedHeaderHash = await Effect.runPromise(
    SDK.hashBlockHeader(body.header),
  );
  if (
    embeddedHeaderHash !== body.header_hash.toLowerCase() ||
    embeddedHeaderHash !== admittedObservation.headerHash
  ) {
    throw new FabricatedWithdrawalRejection(
      "header_hash_mismatch",
      `embedded=${embeddedHeaderHash} payload=${body.header_hash.toLowerCase()} observed=${admittedObservation.headerHash}`,
    );
  }

  return {
    grade: SDK.combineEvidenceGrade([
      admittedObservation.provenance,
      admittedDa,
    ]),
    provenance: { l1: admittedObservation.provenance, da: admittedDa },
    headerHash: admittedObservation.headerHash,
    payloadEnvelopeSha256: computeDaSha256Hash(
      Buffer.from(payloadEnvelopeCbor),
    ).toString("hex"),
    payloadSha256: computeDaSha256Hash(payloadCbor).toString("hex"),
    committedWithdrawalsRoot: admittedObservation.header.withdrawalsRoot,
    withdrawalCount: admittedObservation.header.withdrawalCount,
    headerStartTime: admittedObservation.header.startTime,
    headerEndTime: admittedObservation.header.endTime,
    entries: body.withdrawals.map(
      ([keyHex, valueHex]) => [keyHex, valueHex] as const,
    ),
  };
};
