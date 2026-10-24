import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";

import type { Header } from "../domain.js";
import { normalizeHex } from "../utils/hex.js";
import { type DaPayloadCountSet, type DaPayloadRootSet } from "./payload.js";
import {
  buildCountedRoot,
  type CountedRoot,
  type DataSchema,
  type DecodedRootEntry,
  decodeTypedEntries,
  eventKeyFingerprint,
  type KeyValuePhasRoot,
  NON_MEMBERSHIP_DUMMY_VALUE,
  normalizeRoot,
  type ProofCountSetInput,
  type ProofRootSetInput,
  rawEntries,
  type TraceProofReconstruction,
  trieFromEntries,
} from "./proof-artifacts.da-proof-artifact-reason-code.js";

export const reconstructTraceProofs = async (
  payload: SDK.DaPayload,
  context: {
    readonly headerHash: Buffer;
    readonly payloadHash: Buffer;
    readonly rootSummary: DaPayloadRootSet;
    readonly countSummary: DaPayloadCountSet;
  },
): Promise<TraceProofReconstruction> => {
  const { block_body: body } = payload;
  const transitionTraceEntries = decodeTypedEntries<bigint, SDK.TransitionStep>(
    {
      fieldName: "transition_trace",
      entries: body.transition_trace,
      keySchema: LucidData.Integer() as never,
      valueSchema: SDK.TransitionStepSchema,
    },
  );
  const eventToStepEntries = decodeTypedEntries<
    SDK.EventKey,
    SDK.EventToStepValue
  >({
    fieldName: "event_to_step",
    entries: body.event_to_step,
    keySchema: SDK.EventKeySchema,
    valueSchema: SDK.EventToStepValueSchema,
  });
  return {
    headerHash: Buffer.from(context.headerHash),
    payloadHash: Buffer.from(context.payloadHash),
    rootSummary: normalizeRootSet(context.rootSummary, "proof bundle roots"),
    countSummary: context.countSummary,
    transitionTrace: new Map(
      transitionTraceEntries.map((entry) => [entry.key, entry] as const),
    ),
    eventToStep: new Map(
      eventToStepEntries.map(
        (entry) => [eventKeyFingerprint(entry.key), entry] as const,
      ),
    ),
    rootData: {
      transitionTrace: await buildCountedRoot(
        SDK.ROOT_DOMAINS.transitionTrace,
        rawEntries("transition_trace", body.transition_trace),
      ),
      eventToStep: await buildCountedRoot(
        SDK.ROOT_DOMAINS.eventToStep,
        rawEntries("event_to_step", body.event_to_step),
      ),
    },
  };
};

export const encodeData = <A>(value: A, schema: DataSchema): Buffer =>
  Buffer.from(LucidData.to(value as never, schema as never), "hex");

const countedPhasView = (root: CountedRoot): KeyValuePhasRoot => ({
  root: root.phasRoot,
  count: root.count,
  entries: root.entries,
});

const sdkProofFromCbor = (proofCbor: Uint8Array): SDK.Proof =>
  LucidData.from(
    Buffer.from(proofCbor).toString("hex"),
    SDK.Proof as never,
  ) as SDK.Proof;

const keyValuePhasProof = async (
  root: KeyValuePhasRoot,
  key: Buffer,
  value: Buffer,
): Promise<SDK.Proof> => {
  if (root.entries.length === 0) {
    throw new Error("cannot prove membership for empty tree");
  }
  const trie = await trieFromEntries(root.entries);
  const proof = await trie.prove(Buffer.from(key));
  if (normalizeRoot(proof.verify(true)) !== root.root) {
    throw new Error("generated membership proof root mismatch");
  }
  if (
    !root.entries.some(
      (entry) => entry.key.equals(key) && entry.value.equals(value),
    )
  ) {
    throw new Error("cannot prove membership for absent key/value");
  }
  return sdkProofFromCbor(proof.toCBOR());
};

const keyValuePhasNonMembershipProof = async (
  root: KeyValuePhasRoot,
  key: Buffer,
): Promise<SDK.Proof> => {
  if (root.entries.some((entry) => entry.key.equals(key))) {
    throw new Error("cannot prove non-membership for present key");
  }
  const trie = await trieFromEntries(root.entries);
  await trie.insert(Buffer.from(key), NON_MEMBERSHIP_DUMMY_VALUE);
  const proof = await trie.prove(Buffer.from(key));
  if (normalizeRoot(proof.verify(false)) !== root.root) {
    throw new Error("generated non-membership proof root mismatch");
  }
  return sdkProofFromCbor(proof.toCBOR());
};

export const membershipProof = async <K, V>(
  root: CountedRoot,
  entry: DecodedRootEntry<K, V>,
): Promise<SDK.RootMembershipProof<K, V>> => ({
  domain: root.domain,
  root: root.root,
  phas_root: root.phasRoot,
  count: root.count,
  key: entry.key,
  value: entry.value,
  proof: await keyValuePhasProof(
    countedPhasView(root),
    entry.keyBytes,
    entry.valueBytes,
  ),
});

const nonMembershipProof = async <K>({
  root,
  key,
  keyBytes,
}: {
  readonly root: CountedRoot;
  readonly key: K;
  readonly keyBytes: Buffer;
}): Promise<SDK.RootNonMembershipProof<K>> => ({
  domain: root.domain,
  root: root.root,
  phas_root: root.phasRoot,
  count: root.count,
  key,
  proof: await keyValuePhasNonMembershipProof(countedPhasView(root), keyBytes),
});

export const buildEventToStepMembershipProof = async (
  root: CountedRoot,
  entry: DecodedRootEntry<SDK.EventKey, SDK.EventToStepValue>,
): Promise<SDK.EventToStepProof> => ({
  EventToStepMembership: {
    membership: await membershipProof(root, entry),
  },
});

export const buildEventToStepNonMembershipProof = async (
  root: CountedRoot,
  eventKey: SDK.EventKey,
): Promise<SDK.EventToStepProof> => ({
  EventToStepNonMembership: {
    non_membership: await nonMembershipProof({
      root,
      key: eventKey,
      keyBytes: encodeData(eventKey, SDK.EventKeySchema),
    }),
  },
});

export const normalizeRootSet = (
  roots: ProofRootSetInput,
  fieldName: string,
): DaPayloadRootSet => ({
  utxosRoot: normalizeHex(roots.utxosRoot, {
    fieldName: `${fieldName}.utxos_root`,
    byteLength: 32,
  }),
  withdrawalsRoot: normalizeHex(roots.withdrawalsRoot, {
    fieldName: `${fieldName}.withdrawals_root`,
    byteLength: 32,
  }),
  forcedTransactionsRoot: normalizeHex(roots.forcedTransactionsRoot, {
    fieldName: `${fieldName}.forced_transactions_root`,
    byteLength: 32,
  }),
  transactionsRoot: normalizeHex(roots.transactionsRoot, {
    fieldName: `${fieldName}.transactions_root`,
    byteLength: 32,
  }),
  depositsRoot: normalizeHex(roots.depositsRoot, {
    fieldName: `${fieldName}.deposits_root`,
    byteLength: 32,
  }),
  transitionTraceRoot: normalizeHex(roots.transitionTraceRoot, {
    fieldName: `${fieldName}.transition_trace_root`,
    byteLength: 32,
  }),
  eventToStepRoot: normalizeHex(roots.eventToStepRoot, {
    fieldName: `${fieldName}.event_to_step_root`,
    byteLength: 32,
  }),
  validationTracesRoot: normalizeHex(roots.validationTracesRoot, {
    fieldName: `${fieldName}.validation_traces_root`,
    byteLength: 32,
  }),
});

export const rootMismatches = (
  expected: ProofRootSetInput,
  actual: ProofRootSetInput,
): readonly string[] => {
  const normalizedExpected = normalizeRootSet(expected, "expected roots");
  const normalizedActual = normalizeRootSet(actual, "actual roots");
  return [
    normalizedExpected.utxosRoot === normalizedActual.utxosRoot
      ? null
      : "utxos_root",
    normalizedExpected.withdrawalsRoot === normalizedActual.withdrawalsRoot
      ? null
      : "withdrawals_root",
    normalizedExpected.forcedTransactionsRoot ===
    normalizedActual.forcedTransactionsRoot
      ? null
      : "forced_transactions_root",
    normalizedExpected.transactionsRoot === normalizedActual.transactionsRoot
      ? null
      : "transactions_root",
    normalizedExpected.depositsRoot === normalizedActual.depositsRoot
      ? null
      : "deposits_root",
    normalizedExpected.transitionTraceRoot ===
    normalizedActual.transitionTraceRoot
      ? null
      : "transition_trace_root",
    normalizedExpected.eventToStepRoot === normalizedActual.eventToStepRoot
      ? null
      : "event_to_step_root",
    normalizedExpected.validationTracesRoot ===
    normalizedActual.validationTracesRoot
      ? null
      : "validation_traces_root",
  ].filter((field): field is string => field !== null);
};

export const countMismatches = (
  expected: ProofCountSetInput,
  actual: ProofCountSetInput,
): readonly string[] =>
  [
    expected.withdrawalCount === actual.withdrawalCount
      ? null
      : "withdrawal_count",
    expected.forcedTransactionCount === actual.forcedTransactionCount
      ? null
      : "forced_transaction_count",
    expected.l2TransactionCount === actual.l2TransactionCount
      ? null
      : "l2_transaction_count",
    expected.depositCount === actual.depositCount ? null : "deposit_count",
    expected.totalEventCount === actual.totalEventCount
      ? null
      : "total_event_count",
    expected.transitionStepCount === actual.transitionStepCount
      ? null
      : "transition_step_count",
    expected.validationTraceCount === actual.validationTraceCount
      ? null
      : "validation_trace_count",
  ].filter((field): field is string => field !== null);

export const headerRoots = (header: Header): DaPayloadRootSet => ({
  utxosRoot: header.utxosRoot,
  withdrawalsRoot: header.withdrawalsRoot,
  forcedTransactionsRoot: header.forcedTransactionsRoot,
  transactionsRoot: header.transactionsRoot,
  depositsRoot: header.depositsRoot,
  transitionTraceRoot: header.transitionTraceRoot,
  eventToStepRoot: header.eventToStepRoot,
  validationTracesRoot: header.validationTracesRoot,
});

export const headerCounts = (header: Header): DaPayloadCountSet => ({
  withdrawalCount: header.withdrawalCount,
  forcedTransactionCount: header.forcedTransactionCount,
  l2TransactionCount: header.l2TransactionCount,
  depositCount: header.depositCount,
  totalEventCount: header.totalEventCount,
  transitionStepCount: header.transitionStepCount,
  validationTraceCount: header.validationTraceCount,
});

export const headerCborHex = (header: Header): string =>
  LucidData.to(header, SDK.Header);
