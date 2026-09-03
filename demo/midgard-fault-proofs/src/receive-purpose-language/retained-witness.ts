import type { EventKey } from "@al-ft/midgard-sdk";

import { buildExecutionSourceMachineAuthenticationFromRetainedDa } from "../execution-source-script-decoding/retained-witness.js";
import type { ReceivePurposeLanguageDescriptor } from "./family.js";
import type { ReceivePurposeLanguageAuthentication } from "./submit-step-02.js";

type EncodedEntry = Readonly<{ key: Uint8Array; value: Uint8Array }>;

/** Strict retained-DA reconstruction for category 34's purpose/language bind. */
export const buildReceivePurposeLanguageAuthenticationFromRetainedDa =
  async (args: {
    readonly eventKey: EventKey;
    readonly executionIndex: number;
    readonly authenticatedValidationTraceEntries: readonly EncodedEntry[];
    readonly retainedValidationWitnessEntries: readonly EncodedEntry[];
    readonly expectedValidationTracesRoot: string;
    readonly expectedLanguageTag: 0 | 3 | 128;
  }): Promise<
    Readonly<{
      validationTracesRoot: string;
      validationTraceCount: bigint;
      authentication: ReceivePurposeLanguageAuthentication;
    }>
  > => {
    const rebuilt =
      await buildExecutionSourceMachineAuthenticationFromRetainedDa({
        ...args,
        expectedPurposeKind: 3,
      });
    const { purpose_kind, ...authentication } = rebuilt.authentication;
    if (purpose_kind !== 3n)
      throw new Error("receivePurposeLanguage retained purpose is not receive");
    return Object.freeze({ ...rebuilt, authentication });
  };

const exactNumber = (value: bigint, label: string): number => {
  const converted = Number(value);
  if (!Number.isSafeInteger(converted) || converted < 0)
    throw new Error(`receivePurposeLanguage ${label} is not a safe index`);
  return converted;
};

/** Converts retained authentication into the pure evidence descriptor twin. */
export const receivePurposeLanguageDescriptorFromAuthentication = (
  authentication: ReceivePurposeLanguageAuthentication,
  executionIndex: number,
): ReceivePurposeLanguageDescriptor => {
  const frontier = (
    count: bigint,
    peaks: readonly Readonly<{ height: bigint; hash: string }>[],
  ) => ({
    count: exactNumber(count, "frontier count"),
    peaks: peaks.map(({ height, hash }) => ({
      height: exactNumber(height, "frontier height"),
      hash: Buffer.from(hash, "hex"),
    })),
  });
  const membership = (
    count: bigint,
    peaks: readonly Readonly<{ height: bigint; hash: string }>[],
    leafIndex: number,
    siblings: readonly string[],
  ) => ({
    frontier: frontier(count, peaks),
    leafIndex,
    leafHash: Buffer.alloc(32),
    siblings: siblings.map((sibling) => Buffer.from(sibling, "hex")),
  });
  const languageTag = exactNumber(authentication.language_tag, "language tag");
  if (languageTag !== 0 && languageTag !== 3 && languageTag !== 128)
    throw new Error("receivePurposeLanguage language tag is unsupported");
  const purposeIndex = exactNumber(
    authentication.purpose_index,
    "purpose index",
  );
  const sourceIndex = exactNumber(authentication.source_index, "source index");
  const originKind = exactNumber(authentication.origin_kind, "origin kind");
  if (originKind !== 0 && originKind !== 1)
    throw new Error("receivePurposeLanguage origin kind is unsupported");
  return Object.freeze({
    sourceIndex,
    originKind,
    sourceKeyHex: authentication.source_key,
    languageTag,
    scriptHashHex: authentication.script_hash,
    scriptTotalLength: exactNumber(
      authentication.total_length,
      "script length",
    ),
    scriptItemCommitmentHex: authentication.item_commitment,
    purposeKind: 3,
    purposeIndex,
    purposeSubjectHex: authentication.purpose_subject,
    redeemerLeafHex: authentication.redeemer_leaf,
    purposeMembership: membership(
      authentication.control.purpose_count,
      authentication.control.purpose_peaks,
      executionIndex,
      authentication.purpose_siblings,
    ),
    sourceMembership: membership(
      authentication.control.source_count,
      authentication.control.source_peaks,
      sourceIndex,
      authentication.source_siblings,
    ),
    executionMembership: membership(
      authentication.control.execution_count,
      authentication.control.execution_peaks,
      executionIndex,
      authentication.execution_siblings,
    ),
  });
};
