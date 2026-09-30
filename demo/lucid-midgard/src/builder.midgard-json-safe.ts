import {
  encodeMidgardNativeTxCanonical,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core/codec";
import { isMidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";
import { validateMidgardConsensusTxCbor } from "@al-ft/midgard-core/consensus-validation";
import {
  LedgerColumns,
  type PhaseAResult,
  type PhaseBResultWithPatch,
  type RejectedTx,
} from "@al-ft/midgard-validation";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { LucidMidgardConfigSnapshot } from "./builder/context.js";
import { type CompleteTxMetadata } from "./builder/metadata.js";
import {
  addrWitnessMetadata,
  decodeAddrWitnesses,
  type PartialWitnessBundleInput,
} from "./builder/witness-bundle.js";
import { BuilderInvariantError, LucidMidgardError } from "./core/errors.js";
import {
  type LocalValidationPreState,
  type LocalValidationPreStateSource,
  type LocalValidationReport,
  type MidgardResult,
  type TxStatus,
} from "./core/types.js";
import type { MidgardProvider } from "./provider.js";
import { type PrivateKey } from "./wallet.js";

export type TxStatusKind = TxStatus["kind"];

export type SubmitOptions = {
  readonly provider?: MidgardProvider;
};

export type AwaitTxOptions = {
  readonly provider?: MidgardProvider;
  readonly until?: TxStatusKind | readonly TxStatusKind[];
  readonly pollIntervalMs?: number;
  readonly timeoutMs?: number;
  readonly signal?: AbortSignal;
};

export type LocalPreflightPhase = "phase-a" | "phase-b";

export type LocalPreflightOptions = {
  readonly provider?: MidgardProvider;
  readonly localPreState?: LocalValidationPreState;
  readonly localPreStateSource?: LocalValidationPreStateSource;
  readonly nowCardanoSlotNo?: bigint | number;
  readonly validationConcurrency?: number;
  readonly enforceScriptBudget?: boolean;
};

export type AssemblePartialWitnessOptions = {
  readonly allowPartial?: boolean;
};

export type MidgardEffect<T> = Effect.Effect<T, LucidMidgardError>;

export const midgardSafe = async <T>(
  operation: () => Promise<T>,
): Promise<MidgardResult<T, LucidMidgardError>> => {
  try {
    return { ok: true, value: await operation() };
  } catch (error) {
    if (error instanceof LucidMidgardError) {
      return { ok: false, error };
    }
    throw error;
  }
};

export const midgardProgram = <T>(
  operation: () => Promise<T>,
): MidgardEffect<T> =>
  Effect.tryPromise({
    try: () => operation(),
    catch: (error) => {
      if (error instanceof LucidMidgardError) {
        return error;
      }
      throw error;
    },
  });

export const txBuilderConstructorToken = Symbol("TxBuilder.constructor");

export const assertConsensusTransaction = (
  txCbor: Uint8Array,
  consensusProfile: LucidMidgardConfigSnapshot["consensusProfile"],
): void => {
  if (!isMidgardConsensusProfile(consensusProfile)) {
    throw new BuilderInvariantError(
      "Transaction uses an unsupported consensus profile",
    );
  }
  const violation = validateMidgardConsensusTxCbor(txCbor);
  if (violation !== null) {
    throw new BuilderInvariantError(
      `Transaction violates the V1 consensus profile: ${violation.code}`,
      `${violation.featureId}: ${violation.detail}`,
    );
  }
};

export const submittedTxConstructorToken = Symbol("SubmittedTx.constructor");

export const partiallySignedTxConstructorToken = Symbol(
  "PartiallySignedTx.constructor",
);

export const assertTxNetworkMatchesExpected = (
  tx: MidgardNativeTxFull,
  expectedNetworkId: bigint | undefined,
  context: string,
): void => {
  if (expectedNetworkId === undefined) {
    return;
  }
  if (tx.body.networkId !== expectedNetworkId) {
    throw new BuilderInvariantError(
      `${context} network id mismatch`,
      `expected=${expectedNetworkId.toString()} actual=${tx.body.networkId.toString()}`,
    );
  }
};

export const trustedCompleteTxs = new WeakSet<object>();

export const midgardJsonSafe = (value: unknown): unknown => {
  if (typeof value === "bigint") {
    return value.toString(10);
  }
  if (Buffer.isBuffer(value) || value instanceof Uint8Array) {
    return Buffer.from(value).toString("hex");
  }
  if (Array.isArray(value)) {
    return value.map(midgardJsonSafe);
  }
  if (typeof value === "object" && value !== null) {
    return Object.fromEntries(
      Object.entries(value).map(([key, child]) => [
        key,
        midgardJsonSafe(child),
      ]),
    );
  }
  return value;
};

export type MidgardTxJson = {
  readonly txId: string;
  readonly txCbor: string;
  readonly metadata: unknown;
};

export const completeTxMetadataWithAddrWitnesses = (
  tx: MidgardNativeTxFull,
  metadata: CompleteTxMetadata,
): CompleteTxMetadata => {
  const witnesses = decodeAddrWitnesses(tx.witnessSet.addrTxWitsPreimageCbor);
  return {
    ...metadata,
    txByteLength: encodeMidgardNativeTxCanonical(tx).length,
    ...addrWitnessMetadata(witnesses),
  };
};

export const privateKeyFromInput = (
  privateKey: PrivateKey | string,
): PrivateKey =>
  typeof privateKey === "string"
    ? CML.PrivateKey.from_bech32(privateKey)
    : privateKey;

export const partialBundleInputs = (
  bundles: PartialWitnessBundleInput | readonly PartialWitnessBundleInput[],
): readonly PartialWitnessBundleInput[] =>
  Array.isArray(bundles) ? bundles : [bundles];

export const completeWitnessSet = (
  actual: readonly string[],
  metadata: CompleteTxMetadata,
): boolean => {
  const expected = metadata.expectedAddrWitnessKeyHashes;
  if (
    expected === undefined ||
    metadata.expectedAddrWitnessesComplete === false
  ) {
    return false;
  }
  const actualSet = new Set(actual);
  return expected.every((keyHash) => actualSet.has(keyHash));
};

export const normalizeValidationConcurrency = (
  value: number | undefined,
  fieldName: string,
): number => {
  if (value === undefined) {
    return 1;
  }
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new BuilderInvariantError(
      `${fieldName} must be a non-negative safe integer`,
    );
  }
  return value;
};

export const normalizeLocalPreflightPhase = (
  phase: LocalPreflightPhase,
): LocalPreflightPhase => {
  if (phase === "phase-a" || phase === "phase-b") {
    return phase;
  }
  throw new BuilderInvariantError(
    'local preflight phase must be "phase-a" or "phase-b"',
    String(phase),
  );
};

const localValidationRejected = (
  rejected: readonly RejectedTx[],
): LocalValidationReport["rejected"] =>
  rejected.map((item) => ({
    txId: item.txId.toString("hex"),
    code: item.code,
    detail: item.detail,
  }));

export const localValidationReportFromPhaseA = (
  result: PhaseAResult,
  preStateSource?: LocalValidationPreStateSource,
): LocalValidationReport => ({
  phase: "phase-a",
  acceptedTxIds: result.accepted.map((item) =>
    item.ledgerTx.txId.toString("hex"),
  ),
  rejected: localValidationRejected(result.rejected),
  ...(preStateSource === undefined
    ? {}
    : { preStateSource, preStateAuthoritative: false as const }),
});

export const localValidationReportFromPhaseB = (
  result: PhaseBResultWithPatch,
  preStateSource?: LocalValidationPreStateSource,
): LocalValidationReport => ({
  phase: "phase-b",
  acceptedTxIds: result.accepted.map((item) =>
    item.ledgerTx.txId.toString("hex"),
  ),
  rejected: localValidationRejected(result.rejected),
  ...(preStateSource === undefined
    ? {}
    : { preStateSource, preStateAuthoritative: false as const }),
  statePatch: {
    deletedOutRefs: result.statePatch.deletedOutRefs,
    upsertedOutRefs: result.statePatch.upsertedOutRefs.map(
      ([outRefHex, output]) => [outRefHex, Buffer.from(output).toString("hex")],
    ),
  },
});

export const materializeLocalPreState = (
  preState: LocalValidationPreState,
): Map<string, Buffer> => {
  if (Array.isArray(preState)) {
    return new Map(
      preState.map((entry) => [
        entry[LedgerColumns.OUTREF].toString("hex"),
        Buffer.from(entry[LedgerColumns.OUTPUT]),
      ]),
    );
  }
  const mapPreState = preState as ReadonlyMap<string, Uint8Array>;
  return new Map(
    [...mapPreState.entries()].map(([outRefHex, output]) => [
      outRefHex,
      Buffer.from(output),
    ]),
  );
};
