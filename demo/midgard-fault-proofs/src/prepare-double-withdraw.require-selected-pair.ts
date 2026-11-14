import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

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

export const hex = (value: string, label: string, bytes?: number): string => {
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

export const decodeLeaf = (
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

export const requireSelectedPair = ({
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
