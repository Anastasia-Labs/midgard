/**
 * Security-grade evidence preparation for the same-block `double-withdraw`
 * family. The bare same-outref predicate is deliberately insufficient: a
 * pair is submittable only when both distinct committed leaves are payable.
 */
import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { stringifyJson } from "./json-file.js";
import { buildTrieView, requireProof } from "./prepare-double-spend.js";
import {
  fetchRetainedDaPayloadByHeaderHash,
  type RetainedDaPayloadSource,
} from "./transition-trace/fetch.js";
import {
  commitCountedRoot,
  keyValuePhasRootWithCount,
} from "./transition-trace/phas.js";

export const DOUBLE_WITHDRAW_EVIDENCE_SCHEMA_VERSION =
  "midgard-double-withdraw-evidence-v1" as const;

export type DoubleWithdrawRejectionCode =
  | "malformed_da_payload"
  | "non_canonical_da_payload"
  | "wrong_da_payload_version"
  | "header_hash_mismatch"
  | "withdrawals_root_mismatch"
  | "malformed_withdrawal_leaf"
  | "non_canonical_withdrawal_leaf"
  | "selected_leaf_not_committed"
  | "same_leaf_twice"
  | "first_leaf_not_payable"
  | "second_leaf_not_payable"
  | "distinct_l2_outrefs"
  | "no_payable_duplicate_pair";

/** Deterministic rejection whose detail contains committed/public facts only. */
export class DoubleWithdrawRejection extends Error {
  readonly code: DoubleWithdrawRejectionCode;

  constructor(code: DoubleWithdrawRejectionCode, detail: string) {
    super(`${code}: ${detail}`);
    this.name = "DoubleWithdrawRejectionV1";
    this.code = code;
  }
}

const hex = (value: string, label: string, bytes?: number): string => {
  const normalized = value.toLowerCase();
  const exact = bytes === undefined ? "*" : `{${(bytes * 2).toString()}}`;
  if (!new RegExp(`^[0-9a-f]${exact}$`, "u").test(normalized)) {
    throw new DoubleWithdrawRejection(
      "malformed_da_payload",
      `${label} is not ${bytes === undefined ? "even-length" : bytes.toString() + "-byte"} hexadecimal`,
    );
  }
  return normalized;
};

const sameOutRef = (a: SDK.OutputReference, b: SDK.OutputReference): boolean =>
  a.transactionId.toLowerCase() === b.transactionId.toLowerCase() &&
  a.outputIndex === b.outputIndex;

export type DoubleWithdrawCommittedLeaf = {
  readonly index: number;
  readonly withdrawalIdCbor: string;
  readonly withdrawalInfoCbor: string;
  readonly withdrawalId: SDK.OutputReference;
  readonly withdrawalInfo: SDK.WithdrawalInfo;
};

const decodeLeaf = (
  entry: readonly [string, string],
  index: number,
): DoubleWithdrawCommittedLeaf => {
  const withdrawalIdCbor = hex(
    entry[0],
    `withdrawals[${index.toString()}].key`,
  );
  const withdrawalInfoCbor = hex(
    entry[1],
    `withdrawals[${index.toString()}].value`,
  );
  let withdrawalId: SDK.OutputReference;
  let withdrawalInfo: SDK.WithdrawalInfo;
  try {
    withdrawalId = Data.from(withdrawalIdCbor, SDK.OutputReference);
    withdrawalInfo = Data.from(withdrawalInfoCbor, SDK.WithdrawalInfo);
  } catch (cause) {
    throw new DoubleWithdrawRejection(
      "malformed_withdrawal_leaf",
      `withdrawals[${index.toString()}] does not decode as (WithdrawalId, WithdrawalInfo): ${String(cause)}`,
    );
  }
  if (
    SDK.committedWithdrawalKeyBytes(withdrawalId) !== withdrawalIdCbor ||
    SDK.committedWithdrawalValueBytes(withdrawalInfo) !== withdrawalInfoCbor
  ) {
    throw new DoubleWithdrawRejection(
      "non_canonical_withdrawal_leaf",
      `withdrawals[${index.toString()}] is not in serialiseData form`,
    );
  }
  return {
    index,
    withdrawalIdCbor,
    withdrawalInfoCbor,
    withdrawalId,
    withdrawalInfo,
  };
};

export type PreparedDoubleWithdrawInclusion = {
  readonly withdrawalIdCbor: string;
  readonly withdrawalInfoCbor: string;
  readonly withdrawalsPhasRoot: string;
  readonly withdrawalMembershipProofCbor: string;
};

export type PreparedDoubleWithdrawOutput = {
  readonly schemaVersion: typeof DOUBLE_WITHDRAW_EVIDENCE_SCHEMA_VERSION;
  readonly violationId: typeof SDK.DOUBLE_WITHDRAW_VIOLATION_ID;
  readonly headerHash: string;
  readonly withdrawalCount: number;
  readonly withdrawalsPhasRoot: string;
  readonly committedWithdrawalsRoot: string;
  readonly leaves: readonly DoubleWithdrawCommittedLeaf[];
  readonly firstLeaf: DoubleWithdrawCommittedLeaf;
  readonly secondLeaf: DoubleWithdrawCommittedLeaf;
  readonly firstInclusion: PreparedDoubleWithdrawInclusion;
  readonly secondInclusion: PreparedDoubleWithdrawInclusion;
  readonly step02State: SDK.DoubleWithdrawStep02State;
  readonly files?: {
    readonly firstInclusionPath: string;
    readonly secondInclusionPath: string;
    readonly planPath: string;
  };
};

const requireSelectedPair = ({
  leaves,
  firstWithdrawalIdCbor,
  secondWithdrawalIdCbor,
}: {
  readonly leaves: readonly DoubleWithdrawCommittedLeaf[];
  readonly firstWithdrawalIdCbor?: string;
  readonly secondWithdrawalIdCbor?: string;
}): readonly [DoubleWithdrawCommittedLeaf, DoubleWithdrawCommittedLeaf] => {
  if (
    (firstWithdrawalIdCbor === undefined) !==
    (secondWithdrawalIdCbor === undefined)
  ) {
    throw new DoubleWithdrawRejection(
      "selected_leaf_not_committed",
      "both selected withdrawal ids must be supplied together",
    );
  }
  if (
    firstWithdrawalIdCbor !== undefined &&
    secondWithdrawalIdCbor !== undefined
  ) {
    const firstId = hex(firstWithdrawalIdCbor, "firstWithdrawalIdCbor");
    const secondId = hex(secondWithdrawalIdCbor, "secondWithdrawalIdCbor");
    const first = leaves.find((leaf) => leaf.withdrawalIdCbor === firstId);
    const second = leaves.find((leaf) => leaf.withdrawalIdCbor === secondId);
    if (first === undefined || second === undefined) {
      throw new DoubleWithdrawRejection(
        "selected_leaf_not_committed",
        "one or both selected withdrawal ids are absent from the committed set",
      );
    }
    if (sameOutRef(first.withdrawalId, second.withdrawalId)) {
      throw new DoubleWithdrawRejection(
        "same_leaf_twice",
        "the selected withdrawal identities are equal",
      );
    }
    if (!SDK.isPayableWithdrawalLeaf(first.withdrawalInfo)) {
      throw new DoubleWithdrawRejection(
        "first_leaf_not_payable",
        "the selected first leaf is not WithdrawalIsValid",
      );
    }
    if (!SDK.isPayableWithdrawalLeaf(second.withdrawalInfo)) {
      throw new DoubleWithdrawRejection(
        "second_leaf_not_payable",
        "the selected second leaf is not WithdrawalIsValid",
      );
    }
    if (
      !sameOutRef(
        first.withdrawalInfo.body.l2_outref,
        second.withdrawalInfo.body.l2_outref,
      )
    ) {
      throw new DoubleWithdrawRejection(
        "distinct_l2_outrefs",
        "the selected leaves drain different L2 output references",
      );
    }
    return [first, second];
  }

  for (let left = 0; left < leaves.length; left += 1) {
    const first = leaves[left]!;
    if (!SDK.isPayableWithdrawalLeaf(first.withdrawalInfo)) continue;
    for (let right = left + 1; right < leaves.length; right += 1) {
      const second = leaves[right]!;
      if (
        SDK.isPayableWithdrawalLeaf(second.withdrawalInfo) &&
        !sameOutRef(first.withdrawalId, second.withdrawalId) &&
        sameOutRef(
          first.withdrawalInfo.body.l2_outref,
          second.withdrawalInfo.body.l2_outref,
        )
      ) {
        return [first, second];
      }
    }
  }
  throw new DoubleWithdrawRejection(
    "no_payable_duplicate_pair",
    `the committed ${leaves.length.toString()}-leaf withdrawal set contains no distinct both-payable same-outref pair`,
  );
};

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
