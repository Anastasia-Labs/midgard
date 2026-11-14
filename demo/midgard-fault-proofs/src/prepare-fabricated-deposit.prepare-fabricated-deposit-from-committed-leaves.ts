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
  classifyFabricatedDepositFault,
  decodeCommittedDepositLeaf,
  FABRICATED_DEPOSIT_EVIDENCE_SCHEMA_VERSION,
  FabricatedDepositRejection,
  hexOf,
  type PreparedFabricatedDepositInclusionJson,
  type PreparedFabricatedDepositOutput,
  type PrepareFabricatedDepositFromCommittedLeavesOptions,
} from "./prepare-fabricated-deposit.classify-fabricated-deposit-fault.js";
import {
  commitCountedRoot,
  keyValuePhasRootWithCount,
} from "./transition-trace/phas.js";

/**
 * Core builder: authenticates the raw committed deposit leaves against the
 * header's counted `deposits_root` **and** `deposit_count`, classifies one leaf
 * against the authenticated L1 witness, then emits the membership proof, the
 * retained content opening, and the exact step-02 state.
 */
export const prepareFabricatedDepositFromCommittedLeaves = async ({
  headerHash,
  committedDepositsRoot,
  depositCount,
  headerStartTime,
  headerEndTime,
  entries,
  witness,
  committedDepositIdCbor,
  minimumConfirmationDepth,
  outputDir,
}: PrepareFabricatedDepositFromCommittedLeavesOptions): Promise<PreparedFabricatedDepositOutput> => {
  const phasEntries = entries.map(([keyHex, valueHex], index) => ({
    key: hexOf(keyHex, `deposits[${index.toString()}].key`),
    value: hexOf(valueHex, `deposits[${index.toString()}].value`),
  }));
  const phas = await keyValuePhasRootWithCount(phasEntries);
  const countedRoot = await commitCountedRoot({
    domain: SDK.ROOT_DOMAINS.deposits,
    phasRoot: phas.root,
    count: phas.count,
  });
  if (
    countedRoot !== committedDepositsRoot.toLowerCase() ||
    phas.count !== depositCount
  ) {
    throw new FabricatedDepositRejection(
      "deposits_root_mismatch",
      `header_deposits_root=${committedDepositsRoot.toLowerCase()} derived=${countedRoot} header_count=${depositCount.toString()} derived_count=${phas.count.toString()}`,
    );
  }

  const leaves = await Promise.all(
    entries.map(async ([keyHex, valueHex], index) =>
      decodeCommittedDepositLeaf(keyHex, valueHex, index),
    ),
  );
  if (leaves.length === 0) {
    throw new FabricatedDepositRejection(
      "no_committed_deposit_leaf",
      `header_hash=${headerHash.toLowerCase()} commits an empty deposit source set`,
    );
  }
  const challengedLeaf =
    committedDepositIdCbor === undefined
      ? leaves[0]
      : leaves.find(
          (leaf) =>
            leaf.committedDepositIdCbor ===
            committedDepositIdCbor.toLowerCase(),
        );
  if (challengedLeaf === undefined) {
    throw new FabricatedDepositRejection(
      "leaf_not_committed",
      `committed_deposit_id=${committedDepositIdCbor?.toLowerCase() ?? ""} is not a committed leaf of header_hash=${headerHash.toLowerCase()} (leaf_count=${leaves.length.toString()})`,
    );
  }

  const classification = await classifyFabricatedDepositFault({
    leaf: challengedLeaf,
    headerStartTime,
    headerEndTime,
    witness,
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
  });

  const trie = await buildTrieView(phasEntries);
  const depositInclusion: PreparedFabricatedDepositInclusionJson = {
    committedDepositIdCbor: challengedLeaf.committedDepositIdCbor,
    committedDepositInfoCbor: challengedLeaf.committedDepositInfoCbor,
    depositsPhasRoot: phas.root,
    depositMembershipProofCbor: requireProof(
      trie,
      Buffer.from(challengedLeaf.committedDepositIdCbor, "hex"),
      "committed deposit leaf",
    ),
  };
  const output: PreparedFabricatedDepositOutput = {
    schemaVersion: FABRICATED_DEPOSIT_EVIDENCE_SCHEMA_VERSION,
    violationId: SDK.FABRICATED_DEPOSIT_VIOLATION_ID,
    fraudCategoryId: SDK.FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID,
    headerHash: headerHash.toLowerCase(),
    threadTokenAssetName: SDK.fabricatedDepositThreadTokenAssetName(
      headerHash.toLowerCase(),
    ),
    depositCount: Number(depositCount),
    depositsPhasRoot: phas.root,
    committedDepositsRoot: countedRoot,
    headerStartTime,
    headerEndTime,
    leaves,
    challengedLeaf,
    classification,
    depositInclusion,
    authenticContent: {
      openingCbor: classification.openingCbor,
    },
    step02State: {
      stateQueuePolicyId: classification.stateQueuePolicyId,
      challengedHeaderHash: headerHash.toLowerCase(),
      headerStartTime: headerStartTime.toString(),
      headerEndTime: headerEndTime.toString(),
      committedDepositIdCbor: challengedLeaf.committedDepositIdCbor,
      committedDepositInfoHash: challengedLeaf.committedDepositInfoHash,
    },
  };
  if (outputDir === undefined) {
    return output;
  }
  await mkdir(outputDir, { recursive: true });
  const paths = {
    depositInclusionPath: join(outputDir, "deposit-inclusion.json"),
    authenticContentPath: join(outputDir, "authentic-content.json"),
    planPath: join(outputDir, "plan.json"),
  };
  await Promise.all([
    writeFile(
      paths.depositInclusionPath,
      stringifyJson(output.depositInclusion),
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
        depositsPhasRoot: output.depositsPhasRoot,
        committedDepositsRoot: output.committedDepositsRoot,
        depositCount: output.depositCount,
        step02State: output.step02State,
      }),
    ),
  ]);
  return { ...output, files: paths };
};

export type FabricatedDepositBlockEvidence = {
  readonly grade: SDK.EvidenceGrade;
  readonly provenance: {
    readonly l1: SDK.EvidenceProvenance;
    readonly da: SDK.EvidenceProvenance;
  };
  readonly headerHash: string;
  readonly payloadEnvelopeSha256: string;
  readonly payloadSha256: string;
  readonly committedDepositsRoot: string;
  readonly depositCount: bigint;
  readonly headerStartTime: bigint;
  readonly headerEndTime: bigint;
  readonly entries: readonly (readonly [string, string])[];
};

/**
 * Extracts the committed `deposits` leaves from public retained-DA bytes without
 * requiring the block to be well-formed. Only the payload envelope, canonical
 * `DaPayload` framing and the embedded-header identity are enforced here; leaf
 * authenticity is what the family adjudicates.
 */
export const fabricatedDepositBlockEvidenceFromVerifiedPayload = async ({
  observation,
  payloadEnvelopeCbor,
  daProvenance,
  minimumConfirmationDepth,
}: {
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservation;
  readonly payloadEnvelopeCbor: Uint8Array;
  readonly daProvenance: SDK.EvidenceProvenance;
  readonly minimumConfirmationDepth?: number;
}): Promise<FabricatedDepositBlockEvidence> => {
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
    throw new FabricatedDepositRejection(
      "malformed_da_payload",
      `failed to decode the mandatory DaPayloadEnvelopeV1: ${String(cause)}`,
    );
  }
  let payload: SDK.DaPayload;
  try {
    payload = SDK.decodeDaPayload(payloadCbor);
  } catch (cause) {
    throw new FabricatedDepositRejection(
      "malformed_da_payload",
      `failed to decode DaPayloadV1 canonical CBOR: ${String(cause)}`,
    );
  }
  if (!SDK.encodeDaPayload(payload).equals(payloadCbor)) {
    throw new FabricatedDepositRejection(
      "non_canonical_da_payload",
      "DA payload CBOR is not canonical for DaPayloadV1",
    );
  }
  if (payload.version !== SDK.DA_PAYLOAD_VERSION) {
    throw new FabricatedDepositRejection(
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
    throw new FabricatedDepositRejection(
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
    committedDepositsRoot: admittedObservation.header.depositsRoot,
    depositCount: admittedObservation.header.depositCount,
    headerStartTime: admittedObservation.header.startTime,
    headerEndTime: admittedObservation.header.endTime,
    entries: body.deposits.map(
      ([keyHex, valueHex]) => [keyHex, valueHex] as const,
    ),
  };
};
