import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildTransactionsSourceRoot,
  buildTransitionTraceResult,
  configureMpfPathHydration,
  getMpfPathHydrationConfig,
  keyValuePhasNonMembershipProof,
  keyValuePhasProof,
  keyValuePhasRoot,
  MidgardMpf,
  type MpfBatchOp,
  type MpfReplayCorpusBlock,
  setMpfScratchBuild,
  type TransitionTraceSourceEvent,
  verifyKeyValuePhasMembershipProof,
  verifyKeyValuePhasNonMembershipProof,
} from "../mpf/index.js";
import { buildAuthenticatedRootFromEncodedEntries } from "../workers/commit-block-header/transition-roots.js";

export type ReplayRoots = MpfReplayCorpusBlock["expected"];

export type MpfReplaySummary = {
  readonly corpusPath: string;
  readonly blocks: number;
  readonly runs: number;
  readonly proofChecks: number;
  readonly implementations: readonly ["typescript_reference", "architecture_g"];
  readonly runsByImplementation: {
    readonly typescript_reference: number;
    readonly architecture_g: number;
  };
  readonly scratchBuilds: readonly ["insert", "fromlist"];
  readonly nativeOwner: {
    readonly binaryPath: string;
    readonly binarySha256: string;
  };
  readonly adversarialCoverage: {
    readonly emptyEvents: number;
    readonly deleteReinsertEvents: number;
    readonly collapseResplitSequences: number;
    readonly longestHashedPrefixNibbles: number;
  };
};

export type MpfReplayOptions = {
  readonly nativeOwnerBinaryPath?: string;
};

export const IMPLEMENTATIONS = [
  "typescript_reference",
  "architecture_g",
] as const;

export const SCRATCH_BUILDS = ["insert", "fromlist"] as const;

export const DEFAULT_NATIVE_OWNER_BINARY_PATH =
  "native/mpf-event-flat-wasm/target/release/architecture-g-owner";

export const decodeEntry = (entry: {
  readonly key: string;
  readonly value: string;
}) => ({
  key: Buffer.from(entry.key, "hex"),
  value: Buffer.from(entry.value, "hex"),
});

const decodeOp = (
  op:
    | { readonly type: "insert"; readonly key: string; readonly value: string }
    | { readonly type: "delete"; readonly key: string },
): MpfBatchOp =>
  op.type === "insert"
    ? {
        type: "insert",
        key: Buffer.from(op.key, "hex"),
        value: Buffer.from(op.value, "hex"),
      }
    : { type: "delete", key: Buffer.from(op.key, "hex") };

export const assertEqual = (
  label: string,
  expected: unknown,
  actual: unknown,
): void => {
  if (JSON.stringify(expected) !== JSON.stringify(actual)) {
    throw new Error(
      `MPF replay divergence for ${label}: expected=${JSON.stringify(expected)},actual=${JSON.stringify(actual)}`,
    );
  }
};

export const decodeSourceEvents = (
  block: MpfReplayCorpusBlock,
): readonly TransitionTraceSourceEvent[] =>
  block.sourceEvents.map((event) => ({
    phase: event.phase,
    eventKey: LucidData.from(
      event.eventKeyCbor,
      SDK.EventKeySchema as never,
    ) as SDK.EventKey,
    ledgerOps: event.ledgerOps.map(decodeOp),
  }));

export const verifyIndependentFinalProofs = (
  root: string,
  finalEntries: readonly { readonly key: Buffer; readonly value: Buffer }[],
): Effect.Effect<number, unknown> =>
  Effect.gen(function* () {
    let proofChecks = 0;
    const keys = finalEntries.map((entry) => entry.key);
    const values = finalEntries.map((entry) => entry.value);
    const sample = finalEntries[0];
    if (sample !== undefined) {
      const proof = yield* keyValuePhasProof(keys, values, sample.key);
      yield* verifyKeyValuePhasMembershipProof({
        root,
        key: sample.key,
        value: sample.value,
        proof,
      });
      proofChecks += 1;
    }
    const absentKey = Buffer.alloc(32, 0xff);
    if (!finalEntries.some((entry) => entry.key.equals(absentKey))) {
      const proof = yield* keyValuePhasNonMembershipProof(
        keys,
        values,
        absentKey,
      );
      yield* verifyKeyValuePhasNonMembershipProof({
        root,
        key: absentKey,
        proof,
      });
      proofChecks += 1;
    }
    return proofChecks;
  });

/**
 * Replays a block through the TypeScript MPF store, the independent reference
 * the native owner's roots are checked against.
 */
export const replayTypeScriptReference = (
  block: MpfReplayCorpusBlock,
  scratchBuild: "insert" | "fromlist",
): Effect.Effect<
  { readonly roots: ReplayRoots; readonly proofChecks: number },
  unknown
> => {
  const previousPathHydration = getMpfPathHydrationConfig();
  return Effect.gen(function* () {
    setMpfScratchBuild(scratchBuild);
    configureMpfPathHydration({
      mode: "whole_block",
      chunkOps: 512,
      retainDepth: 2,
    });
    const initial = block.initialLedgerEntries.map(decodeEntry);
    const ledger = yield* MidgardMpf.createScratch(
      `${block.label}-reference-${scratchBuild}-ledger`,
    );
    yield* ledger.applyBatch(
      initial.map((entry) => ({
        type: "insert" as const,
        key: entry.key,
        value: entry.value,
      })),
    );
    const sourceEvents = decodeSourceEvents(block);
    const count = (phase: SDK.TransitionPhase): number =>
      sourceEvents.filter((event) => event.phase === phase).length;
    const trace = yield* buildTransitionTraceResult({
      ledgerMpf: ledger,
      sourceEvents,
      withdrawalCount: count("Withdrawal"),
      forcedTransactionCount: count("ForcedTransaction"),
      l2TransactionCount: count("L2Transaction"),
      depositCount: count("Deposit"),
    });

    const transactionOps = block.transactionOps.map(decodeEntry);
    const transactions = yield* MidgardMpf.createScratch(
      `${block.label}-reference-${scratchBuild}-transactions`,
    );
    const transactionBatch = transactionOps.map((entry) => ({
      type: "insert" as const,
      ...entry,
    }));
    yield* transactions.applyBatch(transactionBatch);
    const rawTxRoot = yield* transactions.rootHex();
    const txRoot = yield* buildTransactionsSourceRoot(transactionBatch);
    const counted = (
      domain: SDK.RootDomain,
      entries: readonly { readonly key: string; readonly value: string }[],
    ) =>
      buildAuthenticatedRootFromEncodedEntries(
        domain,
        entries.map(decodeEntry),
      ).pipe(Effect.map((result) => result.root));
    const [depositsRoot, withdrawalsRoot, forcedTransactionsRoot] =
      yield* Effect.all(
        [
          counted(SDK.ROOT_DOMAINS.deposits, block.deposits),
          counted(SDK.ROOT_DOMAINS.withdrawals, block.withdrawals),
          counted(
            SDK.ROOT_DOMAINS.forcedTransactionsV1,
            block.forcedTransactions,
          ),
        ],
        { concurrency: "unbounded" },
      );
    const finalEntries = block.finalUtxoEntries.map(decodeEntry);
    const payloadRoot = yield* keyValuePhasRoot(
      finalEntries.map((entry) => entry.key),
      finalEntries.map((entry) => entry.value),
    );
    const utxoRoot = yield* ledger.rootHex();
    if (payloadRoot !== utxoRoot) {
      throw new Error(
        `Final payload root mismatch: payload=${payloadRoot},ledger=${utxoRoot}`,
      );
    }

    let proofChecks = 0;
    const sample = finalEntries[0];
    if (sample !== undefined) {
      const proof = yield* ledger.prove(sample.key);
      yield* verifyKeyValuePhasMembershipProof({
        root: utxoRoot,
        key: sample.key,
        value: sample.value,
        proof: LucidData.from(
          proof.cbor.toString("hex"),
          SDK.Proof as never,
        ) as SDK.Proof,
      });
      proofChecks += 1;
    }
    const absentKey = Buffer.alloc(32, 0xff);
    if (!finalEntries.some((entry) => entry.key.equals(absentKey))) {
      const proof = yield* keyValuePhasNonMembershipProof(
        finalEntries.map((entry) => entry.key),
        finalEntries.map((entry) => entry.value),
        absentKey,
      );
      yield* verifyKeyValuePhasNonMembershipProof({
        root: utxoRoot,
        key: absentKey,
        proof,
      });
      proofChecks += 1;
    }

    yield* ledger.close();
    yield* transactions.close();
    return {
      roots: {
        utxoRoot,
        rawTxRoot,
        txRoot,
        transitionTraceRoot: trace.transitionTraceRoot,
        eventToStepRoot: trace.eventToStepRoot,
        depositsRoot,
        withdrawalsRoot,
        forcedTransactionsRoot,
        transitionRoots: trace.transitionTraceMembers.map((member) => ({
          pre: member.value.pre_utxos_root,
          post: member.value.post_utxos_root,
        })),
      },
      proofChecks,
    };
  }).pipe(
    Effect.ensuring(
      Effect.sync(() => configureMpfPathHydration(previousPathHydration)),
    ),
  );
};
