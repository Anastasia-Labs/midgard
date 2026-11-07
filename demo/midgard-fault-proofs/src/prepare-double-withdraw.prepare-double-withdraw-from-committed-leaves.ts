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
  decodeLeaf,
  DOUBLE_WITHDRAW_EVIDENCE_SCHEMA_VERSION,
  type DoubleWithdrawCommittedLeaf,
  DoubleWithdrawRejection,
  hex,
  type PreparedDoubleWithdrawInclusion,
  type PreparedDoubleWithdrawOutput,
  requireSelectedPair,
} from "./prepare-double-withdraw.require-selected-pair.js";
import {
  fetchRetainedDaPayloadByHeaderHash,
  type RetainedDaPayloadSource,
} from "./transition-trace/fetch.js";
import {
  commitCountedRoot,
  keyValuePhasRootWithCount,
} from "./transition-trace/phas.js";

/** Authenticate the committed set, select a deterministic fault pair, and prove both leaves. */
export const prepareDoubleWithdrawFromCommittedLeaves = async ({
  headerHash,
  committedWithdrawalsRoot,
  withdrawalCount,
  entries,
  firstWithdrawalIdCbor,
  secondWithdrawalIdCbor,
  outputDir,
}: {
  readonly headerHash: string;
  readonly committedWithdrawalsRoot: string;
  readonly withdrawalCount: bigint;
  readonly entries: readonly (readonly [string, string])[];
  readonly firstWithdrawalIdCbor?: string;
  readonly secondWithdrawalIdCbor?: string;
  readonly outputDir?: string;
}): Promise<PreparedDoubleWithdrawOutput> => {
  const normalizedHeaderHash = hex(headerHash, "headerHash", 28);
  const normalizedRoot = hex(
    committedWithdrawalsRoot,
    "committedWithdrawalsRoot",
    32,
  );
  const entriesBytes = entries.map(([key, value], index) => ({
    key: Buffer.from(hex(key, `withdrawals[${index.toString()}].key`), "hex"),
    value: Buffer.from(
      hex(value, `withdrawals[${index.toString()}].value`),
      "hex",
    ),
  }));
  const phas = await keyValuePhasRootWithCount(entriesBytes);
  const countedRoot = await commitCountedRoot({
    domain: SDK.ROOT_DOMAINS.withdrawals,
    phasRoot: phas.root,
    count: phas.count,
  });
  if (countedRoot !== normalizedRoot || phas.count !== withdrawalCount) {
    throw new DoubleWithdrawRejection(
      "withdrawals_root_mismatch",
      `header_root=${normalizedRoot} derived_root=${countedRoot} header_count=${withdrawalCount.toString()} derived_count=${phas.count.toString()}`,
    );
  }
  const leaves = entries.map(decodeLeaf);
  const [firstLeaf, secondLeaf] = requireSelectedPair({
    leaves,
    ...(firstWithdrawalIdCbor === undefined ? {} : { firstWithdrawalIdCbor }),
    ...(secondWithdrawalIdCbor === undefined ? {} : { secondWithdrawalIdCbor }),
  });
  const trie = await buildTrieView(entriesBytes);
  const inclusion = (
    leaf: DoubleWithdrawCommittedLeaf,
  ): PreparedDoubleWithdrawInclusion => ({
    withdrawalIdCbor: leaf.withdrawalIdCbor,
    withdrawalInfoCbor: leaf.withdrawalInfoCbor,
    withdrawalsPhasRoot: phas.root,
    withdrawalMembershipProofCbor: requireProof(
      trie,
      Buffer.from(leaf.withdrawalIdCbor, "hex"),
      `double-withdraw leaf ${leaf.index.toString()}`,
    ),
  });
  const output: PreparedDoubleWithdrawOutput = {
    schemaVersion: DOUBLE_WITHDRAW_EVIDENCE_SCHEMA_VERSION,
    violationId: SDK.DOUBLE_WITHDRAW_VIOLATION_ID,
    headerHash: normalizedHeaderHash,
    withdrawalCount: Number(withdrawalCount),
    withdrawalsPhasRoot: phas.root,
    committedWithdrawalsRoot: countedRoot,
    leaves,
    firstLeaf,
    secondLeaf,
    firstInclusion: inclusion(firstLeaf),
    secondInclusion: inclusion(secondLeaf),
    step02State: SDK.doubleWithdrawStep02State({
      challengedHeaderHash: normalizedHeaderHash,
      committedWithdrawal: {
        domain: SDK.ROOT_DOMAINS.withdrawals,
        root: countedRoot,
        phas_root: phas.root,
        count: phas.count,
        key: firstLeaf.withdrawalId,
        value: firstLeaf.withdrawalInfo,
        proof: [],
      },
    }),
  };
  if (outputDir === undefined) return output;
  await mkdir(outputDir, { recursive: true });
  const files = {
    firstInclusionPath: join(outputDir, "first-withdrawal-inclusion.json"),
    secondInclusionPath: join(outputDir, "second-withdrawal-inclusion.json"),
    planPath: join(outputDir, "plan.json"),
  };
  await Promise.all([
    writeFile(files.firstInclusionPath, stringifyJson(output.firstInclusion)),
    writeFile(files.secondInclusionPath, stringifyJson(output.secondInclusion)),
    writeFile(
      files.planPath,
      stringifyJson({
        schemaVersion: output.schemaVersion,
        violationId: output.violationId,
        headerHash: output.headerHash,
        withdrawalCount: output.withdrawalCount,
        withdrawalsPhasRoot: output.withdrawalsPhasRoot,
        committedWithdrawalsRoot: output.committedWithdrawalsRoot,
        step02State: output.step02State,
      }),
    ),
  ]);
  return { ...output, files };
};

export type DoubleWithdrawBlockEvidence = {
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
  readonly entries: readonly (readonly [string, string])[];
};

/** Admit retained DA against an authenticated state-queue header. */
export const doubleWithdrawBlockEvidenceFromVerifiedPayload = async ({
  observation,
  payloadEnvelopeCbor,
  daProvenance,
  minimumConfirmationDepth,
}: {
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservation;
  readonly payloadEnvelopeCbor: Uint8Array;
  readonly daProvenance: SDK.EvidenceProvenance;
  readonly minimumConfirmationDepth?: number;
}): Promise<DoubleWithdrawBlockEvidence> => {
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
    throw new DoubleWithdrawRejection(
      "malformed_da_payload",
      `failed to decode DaPayloadEnvelopeV1: ${String(cause)}`,
    );
  }
  let payload: SDK.DaPayload;
  try {
    payload = SDK.decodeDaPayload(payloadCbor);
  } catch (cause) {
    throw new DoubleWithdrawRejection(
      "malformed_da_payload",
      `failed to decode DaPayloadV1: ${String(cause)}`,
    );
  }
  if (!SDK.encodeDaPayload(payload).equals(payloadCbor)) {
    throw new DoubleWithdrawRejection(
      "non_canonical_da_payload",
      "payload CBOR is not canonical DaPayloadV1",
    );
  }
  if (payload.version !== SDK.DA_PAYLOAD_VERSION) {
    throw new DoubleWithdrawRejection(
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
    throw new DoubleWithdrawRejection(
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
    entries: body.withdrawals.map(([key, value]) => [key, value] as const),
  };
};

/** Security-grade watcher entrypoint: L1 header + public DA -> proof plan. */
export const prepareDoubleWithdrawFromRetainedDa = async ({
  observation,
  sources,
  retries,
  minimumConfirmationDepth,
  firstWithdrawalIdCbor,
  secondWithdrawalIdCbor,
  outputDir,
}: {
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservation;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly retries?: number;
  readonly minimumConfirmationDepth?: number;
  readonly firstWithdrawalIdCbor?: string;
  readonly secondWithdrawalIdCbor?: string;
  readonly outputDir?: string;
}): Promise<PreparedDoubleWithdrawOutput> => {
  if (sources.length === 0) {
    throw new SDK.CanonicalEvidenceRejection(
      "da_evidence_wrong_trust_class",
      "no public DA source was configured",
    );
  }
  const admitted = await SDK.admitAuthenticatedStateQueueHeaderObservation({
    observation,
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
  });
  const fetched = await fetchRetainedDaPayloadByHeaderHash({
    headerHash: admitted.headerHash,
    sources,
    ...(retries === undefined ? {} : { retries }),
  });
  const evidence = await doubleWithdrawBlockEvidenceFromVerifiedPayload({
    observation: admitted,
    payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
    daProvenance: SDK.assertSecurityGradeEvidence(
      SDK.admitEvidenceProvenance({ provenance: fetched.provenance }),
    ),
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
  });
  return prepareDoubleWithdrawFromCommittedLeaves({
    headerHash: evidence.headerHash,
    committedWithdrawalsRoot: evidence.committedWithdrawalsRoot,
    withdrawalCount: evidence.withdrawalCount,
    entries: evidence.entries,
    ...(firstWithdrawalIdCbor === undefined ? {} : { firstWithdrawalIdCbor }),
    ...(secondWithdrawalIdCbor === undefined ? {} : { secondWithdrawalIdCbor }),
    ...(outputDir === undefined ? {} : { outputDir }),
  });
};
