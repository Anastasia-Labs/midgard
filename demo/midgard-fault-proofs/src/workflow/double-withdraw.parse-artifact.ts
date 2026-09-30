import { DOUBLE_WITHDRAW_VIOLATION_ID } from "@al-ft/midgard-sdk";

import { parseSubmitDoubleWithdrawInclusion } from "../double-withdraw/submit-double-withdraw-step-01.js";
import {
  type PreparedDoubleWithdrawOutput,
  prepareDoubleWithdrawFromCommittedLeaves,
} from "../prepare-double-withdraw.js";
import type { CanonicalBlockClassification } from "./classification.js";
import { type JournalJsonObject } from "./journal.js";

export const DOUBLE_WITHDRAW_ARTIFACT =
  "midgard-production-double-withdraw-artifact-v1" as const;

type DoubleWithdrawArtifactEntry = Readonly<{
  keyCbor: string;
  valueCbor: string;
}>;

export type DoubleWithdrawArtifact = JournalJsonObject & {
  readonly schemaVersion: typeof DOUBLE_WITHDRAW_ARTIFACT;
  readonly headerHash: string;
  readonly committedWithdrawalsRoot: string;
  readonly withdrawalCount: number;
  readonly firstLeafIndex: number;
  readonly secondLeafIndex: number;
  readonly entries: readonly DoubleWithdrawArtifactEntry[];
};

const HEX_28 = /^[0-9a-f]{56}$/u;

const HEX_32 = /^[0-9a-f]{64}$/u;

export const EVEN_HEX = /^(?:[0-9a-f]{2})+$/u;

export const record = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype
  ) {
    throw new Error(`${label} must be a plain object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

const exactKeys = (
  value: Readonly<Record<string, unknown>>,
  expected: readonly string[],
  label: string,
): void => {
  const actual = Object.keys(value).sort();
  const canonical = [...expected].sort();
  if (
    actual.length !== canonical.length ||
    actual.some((key, index) => key !== canonical[index])
  ) {
    throw new Error(`${label} has unknown or missing fields`);
  }
};

const canonicalHex = (
  value: unknown,
  pattern: RegExp,
  label: string,
): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    throw new Error(`${label} is not canonical lowercase hex`);
  }
  return value;
};

const natural = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} is not a non-negative safe integer`);
  }
  return value as number;
};

const parseArtifact = (value: unknown): DoubleWithdrawArtifact => {
  const artifact = record(value, "double-withdraw artifact");
  exactKeys(
    artifact,
    [
      "schemaVersion",
      "headerHash",
      "committedWithdrawalsRoot",
      "withdrawalCount",
      "firstLeafIndex",
      "secondLeafIndex",
      "entries",
    ],
    "double-withdraw artifact",
  );
  if (artifact.schemaVersion !== DOUBLE_WITHDRAW_ARTIFACT) {
    throw new Error("double-withdraw artifact version changed");
  }
  if (!Array.isArray(artifact.entries) || artifact.entries.length === 0) {
    throw new Error("double-withdraw artifact has no withdrawal leaves");
  }
  const entries = Object.freeze(
    artifact.entries.map((value, index) => {
      const entry = record(value, `double-withdraw entry ${index.toString()}`);
      exactKeys(
        entry,
        ["keyCbor", "valueCbor"],
        `double-withdraw entry ${index.toString()}`,
      );
      return Object.freeze({
        keyCbor: canonicalHex(
          entry.keyCbor,
          EVEN_HEX,
          `double-withdraw entry ${index.toString()} key`,
        ),
        valueCbor: canonicalHex(
          entry.valueCbor,
          EVEN_HEX,
          `double-withdraw entry ${index.toString()} value`,
        ),
      });
    }),
  );
  const withdrawalCount = natural(
    artifact.withdrawalCount,
    "double-withdraw withdrawal count",
  );
  if (withdrawalCount !== entries.length) {
    throw new Error(
      "double-withdraw artifact count differs from its withdrawal leaves",
    );
  }
  return Object.freeze({
    schemaVersion: DOUBLE_WITHDRAW_ARTIFACT,
    headerHash: canonicalHex(
      artifact.headerHash,
      HEX_28,
      "double-withdraw header",
    ),
    committedWithdrawalsRoot: canonicalHex(
      artifact.committedWithdrawalsRoot,
      HEX_32,
      "double-withdraw withdrawals root",
    ),
    withdrawalCount,
    firstLeafIndex: natural(
      artifact.firstLeafIndex,
      "double-withdraw first leaf index",
    ),
    secondLeafIndex: natural(
      artifact.secondLeafIndex,
      "double-withdraw second leaf index",
    ),
    entries,
  });
};

type AdmittedDoubleWithdrawArtifact = Readonly<{
  artifact: DoubleWithdrawArtifact;
  prepared: PreparedDoubleWithdrawOutput;
  firstInclusion: ReturnType<typeof parseSubmitDoubleWithdrawInclusion>;
  secondInclusion: ReturnType<typeof parseSubmitDoubleWithdrawInclusion>;
}>;

/** Rebuilds the counted root, deterministic pair, and both MPF proofs. */
export const admitDoubleWithdrawArtifact = async (
  value: unknown,
): Promise<AdmittedDoubleWithdrawArtifact> => {
  const artifact = parseArtifact(value);
  const first = artifact.entries[artifact.firstLeafIndex];
  const second = artifact.entries[artifact.secondLeafIndex];
  if (
    first === undefined ||
    second === undefined ||
    artifact.firstLeafIndex >= artifact.secondLeafIndex
  ) {
    throw new Error("double-withdraw artifact selected an invalid leaf pair");
  }
  const prepared = await prepareDoubleWithdrawFromCommittedLeaves({
    headerHash: artifact.headerHash,
    committedWithdrawalsRoot: artifact.committedWithdrawalsRoot,
    withdrawalCount: BigInt(artifact.withdrawalCount),
    entries: artifact.entries.map(({ keyCbor, valueCbor }) => [
      keyCbor,
      valueCbor,
    ]),
    firstWithdrawalIdCbor: first.keyCbor,
    secondWithdrawalIdCbor: second.keyCbor,
  });
  if (
    prepared.firstLeaf.index !== artifact.firstLeafIndex ||
    prepared.secondLeaf.index !== artifact.secondLeafIndex
  ) {
    throw new Error("double-withdraw artifact pair changed on re-derivation");
  }
  return Object.freeze({
    artifact,
    prepared,
    firstInclusion: parseSubmitDoubleWithdrawInclusion(prepared.firstInclusion),
    secondInclusion: parseSubmitDoubleWithdrawInclusion(
      prepared.secondInclusion,
    ),
  });
};

export const detectionIdForPrepared = (
  prepared: PreparedDoubleWithdrawOutput,
): string =>
  `${DOUBLE_WITHDRAW_VIOLATION_ID}:${prepared.firstLeaf.index.toString()}:${prepared.secondLeaf.index.toString()}:${prepared.firstLeaf.withdrawalIdCbor}:${prepared.secondLeaf.withdrawalIdCbor}`;

export const selectedPairFromClassification = (
  classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  > & { readonly category: "doubleWithdraw" },
): Readonly<{
  firstLeafIndex: number;
  secondLeafIndex: number;
  firstKeyCbor: string;
  secondKeyCbor: string;
}> => {
  const [violationId, first, second, firstKeyCbor, secondKeyCbor, ...surplus] =
    classification.selected.detectionId.split(":");
  if (
    violationId !== DOUBLE_WITHDRAW_VIOLATION_ID ||
    surplus.length !== 0 ||
    !/^(?:0|[1-9][0-9]*)$/u.test(first ?? "") ||
    !/^(?:0|[1-9][0-9]*)$/u.test(second ?? "") ||
    !EVEN_HEX.test(firstKeyCbor ?? "") ||
    !EVEN_HEX.test(secondKeyCbor ?? "")
  ) {
    throw new Error("double-withdraw classification has a malformed pair id");
  }
  const firstLeafIndex = Number(first);
  const secondLeafIndex = Number(second);
  if (
    !Number.isSafeInteger(firstLeafIndex) ||
    !Number.isSafeInteger(secondLeafIndex) ||
    firstLeafIndex >= secondLeafIndex ||
    classification.selected.position !== BigInt(secondLeafIndex)
  ) {
    throw new Error("double-withdraw classification has an invalid pair order");
  }
  return {
    firstLeafIndex,
    secondLeafIndex,
    firstKeyCbor: firstKeyCbor!,
    secondKeyCbor: secondKeyCbor!,
  };
};
