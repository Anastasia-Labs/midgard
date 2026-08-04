import { createHash } from "node:crypto";
import * as FS from "node:fs";
import { mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import blake2b from "blake2b";
import { Effect } from "effect";
import { Level } from "level";

import * as Ledger from "@/database/utils/ledger.js";
import { ProductionNativeMpfOwnerService } from "@/services/mpf-native-owner/index.js";
import { buildAuthenticatedRootFromEncodedEntries } from "@/workers/commit-block-header/transition-roots.js";
import {
  buildNativeProductionRootProbe,
  buildTransactionsSourceRoot,
  buildTransitionTraceResult,
  computeUtxoPayloadRoot,
  configureMpfPathHydration,
  getMpfPathHydrationConfig,
  hydrateLedgerMpfFromLedgerEntries,
  keyValuePhasNonMembershipProof,
  keyValuePhasProof,
  MidgardMpf,
  type MpfBatchOp,
  type MpfReplayCorpusBlock,
  type NativeMpfBuildContext,
  setMpfScratchBuild,
  type TransitionTraceSourceEvent,
  verifyKeyValuePhasMembershipProof,
  verifyKeyValuePhasNonMembershipProof,
} from "@/workers/utils/mpf.js";

type ReplayRoots = MpfReplayCorpusBlock["expected"];

export type MpfReplaySummary = {
  readonly corpusPath: string;
  readonly blocks: number;
  readonly runs: number;
  readonly proofChecks: number;
  readonly engines: readonly [
    "legacy",
    "overlay",
    "event_flat",
    "architecture_g",
  ];
  readonly runsByEngine: {
    readonly legacy: number;
    readonly overlay: number;
    readonly event_flat: number;
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

const ENGINES = ["legacy", "overlay", "event_flat", "architecture_g"] as const;
const SCRATCH_BUILDS = ["insert", "fromlist"] as const;
const DEFAULT_NATIVE_OWNER_BINARY_PATH =
  "native/mpf-event-flat-wasm/target/release/architecture-g-owner";

const decodeEntry = (entry: {
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

const assertEqual = (
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

const decodeSourceEvents = (
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

const verifyIndependentFinalProofs = (
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

const replayOne = (
  block: MpfReplayCorpusBlock,
  engine: "legacy" | "overlay" | "event_flat",
  scratchBuild: "insert" | "fromlist",
): Effect.Effect<
  { readonly roots: ReplayRoots; readonly proofChecks: number },
  unknown
> => {
  const previousPathHydration = getMpfPathHydrationConfig();
  return Effect.gen(function* () {
    setMpfScratchBuild(scratchBuild);
    configureMpfPathHydration({
      mode: engine === "event_flat" ? "chunked_arena" : "whole_block",
      chunkOps: 512,
      retainDepth: 2,
    });
    const initial = block.initialLedgerEntries.map(decodeEntry);
    const ledger = yield* MidgardMpf.createScratch(
      `${block.label}-${engine}-${scratchBuild}-ledger`,
      { engine },
    );
    yield* hydrateLedgerMpfFromLedgerEntries(
      ledger,
      initial.map((entry) => ({
        [Ledger.Columns.OUTREF]: entry.key,
        [Ledger.Columns.OUTPUT]: entry.value,
      })),
    );
    if (engine !== "legacy") yield* ledger.beginBlockOverlay();
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
      `${block.label}-${engine}-${scratchBuild}-transactions`,
      { engine },
    );
    const transactionBatch = transactionOps.map((entry) => ({
      type: "insert" as const,
      ...entry,
    }));
    if (engine !== "legacy") yield* transactions.beginBlockOverlay();
    if (engine === "event_flat") {
      yield* transactions.primeBlockPathArena(transactionBatch, 2, false);
    }
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
            SDK.ROOT_DOMAINS.forcedTransactions,
            block.forcedTransactions,
          ),
        ],
        { concurrency: "unbounded" },
      );
    const finalEntries = block.finalUtxoEntries.map(decodeEntry);
    const payloadRoot = yield* computeUtxoPayloadRoot(
      finalEntries.map((entry) => ({
        outref: entry.key,
        output: entry.value,
      })),
    );
    const utxoRoot = yield* ledger.rootHex();
    if (payloadRoot !== utxoRoot) {
      throw new Error(
        `Final payload root mismatch: payload=${payloadRoot},ledger=${utxoRoot}`,
      );
    }

    if (engine !== "legacy") {
      yield* ledger.flushBlockOverlay(yield* ledger.root());
      yield* transactions.flushBlockOverlay(yield* transactions.root());
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

const seedNativeOwnerFixture = async ({
  block,
  scratchBuild,
  levelPath,
}: {
  readonly block: MpfReplayCorpusBlock;
  readonly scratchBuild: "insert" | "fromlist";
  readonly levelPath: string;
}): Promise<string> => {
  const initial = block.initialLedgerEntries.map(decodeEntry);
  const trieName = `${block.label}-architecture-g-${scratchBuild}-fixture`;
  const ledger =
    scratchBuild === "fromlist"
      ? await Effect.runPromise(
          MidgardMpf.createLevelFromListForBenchmark(
            trieName,
            levelPath,
            initial,
            { engine: "legacy" },
          ),
        )
      : await (async () => {
          const empty = new Level<string, unknown>(levelPath, {
            valueEncoding: "json",
          });
          try {
            await empty.open();
            await empty.put("__root__", SDK.EMPTY_MERKLE_TREE_ROOT);
          } finally {
            await empty.close();
          }
          const created = await Effect.runPromise(
            MidgardMpf.create(trieName, levelPath, { engine: "legacy" }),
          );
          try {
            await Effect.runPromise(
              hydrateLedgerMpfFromLedgerEntries(
                created,
                initial.map((entry) => ({
                  [Ledger.Columns.OUTREF]: entry.key,
                  [Ledger.Columns.OUTPUT]: entry.value,
                })),
              ),
            );
            return created;
          } catch (error) {
            await Effect.runPromise(created.close()).catch(() => undefined);
            throw error;
          }
        })();
  try {
    return await Effect.runPromise(ledger.rootHex());
  } finally {
    await Effect.runPromise(ledger.close());
  }
};

const replayArchitectureGOne = (
  block: MpfReplayCorpusBlock,
  scratchBuild: "insert" | "fromlist",
  nativeOwner: MpfReplaySummary["nativeOwner"],
): Effect.Effect<
  { readonly roots: ReplayRoots; readonly proofChecks: number },
  unknown
> =>
  Effect.tryPromise({
    try: async () => {
      setMpfScratchBuild(scratchBuild);
      const temporaryRoot = await mkdtemp(
        join(tmpdir(), `midgard-mpf-differential-${scratchBuild}-`),
      );
      const levelPath = join(temporaryRoot, "ledger");
      const sidecarPath = join(temporaryRoot, "ledger.sidecar");
      let service: ProductionNativeMpfOwnerService | undefined;
      let handle:
        | Awaited<ReturnType<ProductionNativeMpfOwnerService["fork"]>>
        | undefined;
      try {
        const baseRoot = await seedNativeOwnerFixture({
          block,
          scratchBuild,
          levelPath,
        });
        service = await ProductionNativeMpfOwnerService.create({
          levelPath,
          sidecarPath,
          binaryPath: nativeOwner.binaryPath,
          binarySha256: nativeOwner.binarySha256,
        });
        handle = await service.fork(baseRoot);
        const nativeMpf: NativeMpfBuildContext = {
          client: service,
          handle,
          ownerBinarySha256: nativeOwner.binarySha256,
        };
        const sourceEvents = decodeSourceEvents(block);
        const productionRoots = await Effect.runPromise(
          buildNativeProductionRootProbe({
            nativeMpf,
            sourceEvents,
            transactionOps: block.transactionOps
              .map(decodeEntry)
              .map((entry) => ({
                type: "insert" as const,
                ...entry,
              })),
            deposits: block.deposits.map(decodeEntry).map((entry) => ({
              type: "insert" as const,
              ...entry,
            })),
            withdrawals: block.withdrawals.map(decodeEntry).map((entry) => ({
              type: "insert" as const,
              ...entry,
            })),
            forcedTransactions: block.forcedTransactions
              .map(decodeEntry)
              .map((entry) => ({ type: "insert" as const, ...entry })),
          }),
        );
        const roots: ReplayRoots = {
          utxoRoot: productionRoots.utxoRoot,
          rawTxRoot: productionRoots.rawTxRoot,
          txRoot: productionRoots.txRoot,
          transitionTraceRoot: productionRoots.transitionTraceRoot,
          eventToStepRoot: productionRoots.eventToStepRoot,
          depositsRoot: productionRoots.depositsRoot,
          withdrawalsRoot: productionRoots.withdrawalsRoot,
          forcedTransactionsRoot: productionRoots.forcedTransactionsRoot,
          transitionRoots: productionRoots.transitionRoots,
        };
        const proofChecks = await Effect.runPromise(
          verifyIndependentFinalProofs(
            roots.utxoRoot,
            block.finalUtxoEntries.map(decodeEntry),
          ),
        );
        await service.discard(handle);
        handle = undefined;
        const diagnostics = await service.diagnostics();
        if (
          diagnostics.durableRoot !== baseRoot ||
          diagnostics.activeGenerations !== 0
        ) {
          throw new Error(
            `Architecture G fixture lifecycle mismatch: durable=${diagnostics.durableRoot},base=${baseRoot},active_generations=${diagnostics.activeGenerations.toString()}`,
          );
        }
        return { roots, proofChecks };
      } finally {
        try {
          if (service !== undefined && handle !== undefined) {
            await service.discard(handle).catch(() => undefined);
          }
          await service?.close();
        } finally {
          await rm(temporaryRoot, { recursive: true, force: true });
        }
      }
    },
    catch: (cause) =>
      new Error(
        `Architecture G replay failed for ${block.label}:${scratchBuild}`,
        { cause },
      ),
  });

const mpfKeyDigestHex = (keyHex: string): string =>
  Buffer.from(blake2b(32).update(Buffer.from(keyHex, "hex")).digest()).toString(
    "hex",
  );

const commonPrefixLength = (left: string, right: string): number => {
  let length = 0;
  while (length < left.length && left[length] === right[length]) length += 1;
  return length;
};

const inspectAdversarialCoverage = (
  block: MpfReplayCorpusBlock,
): MpfReplaySummary["adversarialCoverage"] => {
  const emptyEvents = block.sourceEvents.filter(
    (event) => event.ledgerOps.length === 0,
  ).length;
  const deleteReinsertEvents = block.sourceEvents.filter((event) => {
    const deleted = new Set(
      event.ledgerOps.filter((op) => op.type === "delete").map((op) => op.key),
    );
    return event.ledgerOps.some(
      (op) => op.type === "insert" && deleted.has(op.key),
    );
  }).length;
  const initialKeys = new Set(
    block.initialLedgerEntries.map((entry) => entry.key),
  );
  let collapseResplitSequences = 0;
  for (const [index, event] of block.sourceEvents.entries()) {
    for (const op of event.ledgerOps) {
      if (op.type !== "delete" || !initialKeys.has(op.key)) continue;
      if (
        block.sourceEvents
          .slice(index + 1)
          .some((later) =>
            later.ledgerOps.some(
              (laterOp) => laterOp.type === "insert" && laterOp.key === op.key,
            ),
          )
      ) {
        collapseResplitSequences += 1;
      }
    }
  }
  const digests = block.initialLedgerEntries.map((entry) =>
    mpfKeyDigestHex(entry.key),
  );
  let longestHashedPrefixNibbles = 0;
  for (let left = 0; left < digests.length; left += 1) {
    for (let right = left + 1; right < digests.length; right += 1) {
      longestHashedPrefixNibbles = Math.max(
        longestHashedPrefixNibbles,
        commonPrefixLength(digests[left]!, digests[right]!),
      );
    }
  }
  return {
    emptyEvents,
    deleteReinsertEvents,
    collapseResplitSequences,
    longestHashedPrefixNibbles,
  };
};

const assertSeededAdversarialCoverage = (
  block: MpfReplayCorpusBlock,
  coverage: MpfReplaySummary["adversarialCoverage"],
): void => {
  if (!block.label.startsWith("seeded-adversarial-")) return;
  const completeRoot = (value: string): boolean => /^[0-9a-f]{64}$/.test(value);
  const completeRoots = [
    block.expected.utxoRoot,
    block.expected.rawTxRoot,
    block.expected.txRoot,
    block.expected.transitionTraceRoot,
    block.expected.eventToStepRoot,
    block.expected.depositsRoot,
    block.expected.withdrawalsRoot,
    block.expected.forcedTransactionsRoot,
  ].every(completeRoot);
  if (
    coverage.emptyEvents < 1 ||
    coverage.deleteReinsertEvents < 1 ||
    coverage.collapseResplitSequences < 1 ||
    coverage.longestHashedPrefixNibbles < 6 ||
    !completeRoots ||
    block.expected.transitionRoots.length !== block.sourceEvents.length ||
    block.expected.transitionRoots.some(
      (root) => !completeRoot(root.pre) || !completeRoot(root.post),
    )
  ) {
    throw new Error(
      `Seeded adversarial corpus coverage is incomplete for ${block.label}: ${JSON.stringify(coverage)}`,
    );
  }
};

const h32 = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(32);

const encodeCorpusOp = (op: MpfBatchOp) =>
  op.type === "insert"
    ? {
        type: "insert" as const,
        key: op.key.toString("hex"),
        value: op.value.toString("hex"),
      }
    : { type: "delete" as const, key: op.key.toString("hex") };

export const makeSeededAdversarialMpfCorpusBlock = (
  seed = 1_337,
): Effect.Effect<MpfReplayCorpusBlock, unknown> =>
  Effect.gen(function* () {
    const key = (suffix: number): Buffer => {
      const value = Buffer.alloc(32, seed % 251);
      value.writeUInt32BE(suffix, 28);
      return value;
    };
    const byHashedPrefix = new Map<string, Buffer>();
    let longPrefixPair: readonly [Buffer, Buffer] | undefined;
    for (
      let suffix = 0;
      suffix < 100_000 && longPrefixPair === undefined;
      suffix += 1
    ) {
      const candidate = key(suffix);
      const prefix = mpfKeyDigestHex(candidate.toString("hex")).slice(0, 6);
      const previous = byHashedPrefix.get(prefix);
      if (previous === undefined) byHashedPrefix.set(prefix, candidate);
      else longPrefixPair = [previous, candidate];
    }
    if (longPrefixPair === undefined) {
      return yield* Effect.fail(
        new Error("Unable to construct seeded long-prefix MPF fixture"),
      );
    }
    const [a, b] = longPrefixPair;
    const pairPrefix = mpfKeyDigestHex(a.toString("hex")).slice(0, 6);
    const unrelated = (start: number): Buffer => {
      for (let suffix = start; suffix < start + 100_000; suffix += 1) {
        const candidate = key(suffix);
        if (
          !candidate.equals(a) &&
          !candidate.equals(b) &&
          !mpfKeyDigestHex(candidate.toString("hex")).startsWith(pairPrefix)
        ) {
          return candidate;
        }
      }
      throw new Error("Unable to construct unrelated seeded MPF key");
    };
    const d = unrelated(100_000);
    const neighbor = unrelated(200_000);
    const eventKey = (index: number): SDK.EventKey => ({
      L2TransactionEventKey: { tx_id: h32((seed + index) % 251) },
    });
    const sources: readonly {
      readonly phase: SDK.TransitionPhase;
      readonly eventKey: SDK.EventKey;
      readonly ledgerOps: readonly MpfBatchOp[];
    }[] = [
      {
        phase: "Withdrawal",
        eventKey: {
          WithdrawalEventKey: {
            withdrawal_id: {
              transactionId: h32(seed % 251),
              outputIndex: 0n,
            },
          },
        },
        ledgerOps: [],
      },
      {
        phase: "L2Transaction",
        eventKey: eventKey(1),
        ledgerOps: [
          { type: "delete", key: a },
          { type: "insert", key: a, value: Buffer.from("02", "hex") },
        ],
      },
      {
        phase: "L2Transaction",
        eventKey: eventKey(2),
        ledgerOps: [{ type: "delete", key: b }],
      },
      {
        phase: "L2Transaction",
        eventKey: eventKey(3),
        ledgerOps: [
          { type: "insert", key: b, value: Buffer.from("03", "hex") },
        ],
      },
      {
        phase: "Deposit",
        eventKey: {
          DepositEventKey: {
            deposit_id: {
              transactionId: h32((seed + 4) % 251),
              outputIndex: 0n,
            },
          },
        },
        ledgerOps: [
          { type: "insert", key: d, value: Buffer.from("05", "hex") },
        ],
      },
    ];
    const placeholder = "";
    const block: MpfReplayCorpusBlock = {
      version: 1,
      label: `seeded-adversarial-${seed.toString()}`,
      initialLedgerEntries: [
        { key: a.toString("hex"), value: "01" },
        { key: b.toString("hex"), value: "ff" },
        { key: neighbor.toString("hex"), value: "ff" },
      ],
      sourceEvents: sources.map((source) => ({
        phase: source.phase,
        eventKeyCbor: LucidData.to(
          source.eventKey as never,
          SDK.EventKeySchema as never,
        ),
        ledgerOps: source.ledgerOps.map(encodeCorpusOp),
      })),
      transactionOps: [1, 2, 3].map((index) => ({
        key: Buffer.alloc(32, index).toString("hex"),
        value: Buffer.alloc(8, seed % (index + 17)).toString("hex"),
      })),
      deposits: [{ key: d.toString("hex"), value: "05" }],
      withdrawals: [{ key: key(9).toString("hex"), value: "00" }],
      forcedTransactions: [],
      finalUtxoEntries: [
        { key: a.toString("hex"), value: "02" },
        { key: b.toString("hex"), value: "03" },
        { key: d.toString("hex"), value: "05" },
        { key: neighbor.toString("hex"), value: "ff" },
      ],
      expected: {
        utxoRoot: placeholder,
        rawTxRoot: placeholder,
        txRoot: placeholder,
        transitionTraceRoot: placeholder,
        eventToStepRoot: placeholder,
        depositsRoot: placeholder,
        withdrawalsRoot: placeholder,
        forcedTransactionsRoot: placeholder,
        transitionRoots: [],
      },
    };
    const result = yield* replayOne(block, "legacy", "insert");
    return { ...block, expected: result.roots };
  });

export const replayMpfCorpusBlocks = (
  blocks: readonly MpfReplayCorpusBlock[],
  corpusPath: string,
  nativeOwner: MpfReplaySummary["nativeOwner"],
): Effect.Effect<MpfReplaySummary, unknown> =>
  Effect.gen(function* () {
    let runs = 0;
    let proofChecks = 0;
    const runsByEngine = {
      legacy: 0,
      overlay: 0,
      event_flat: 0,
      architecture_g: 0,
    };
    const adversarialCoverage = {
      emptyEvents: 0,
      deleteReinsertEvents: 0,
      collapseResplitSequences: 0,
      longestHashedPrefixNibbles: 0,
    };
    for (const block of blocks) {
      if (block.version !== 1) {
        throw new Error(
          `Unsupported MPF corpus version: ${String(block.version)}`,
        );
      }
      const blockCoverage = inspectAdversarialCoverage(block);
      assertSeededAdversarialCoverage(block, blockCoverage);
      adversarialCoverage.emptyEvents += blockCoverage.emptyEvents;
      adversarialCoverage.deleteReinsertEvents +=
        blockCoverage.deleteReinsertEvents;
      adversarialCoverage.collapseResplitSequences +=
        blockCoverage.collapseResplitSequences;
      adversarialCoverage.longestHashedPrefixNibbles = Math.max(
        adversarialCoverage.longestHashedPrefixNibbles,
        blockCoverage.longestHashedPrefixNibbles,
      );
      let baseline: ReplayRoots | undefined;
      for (const engine of ["legacy", "overlay", "event_flat"] as const) {
        for (const scratchBuild of SCRATCH_BUILDS) {
          const result = yield* replayOne(block, engine, scratchBuild);
          baseline ??= result.roots;
          assertEqual(
            `${block.label}:${engine}:${scratchBuild}`,
            baseline,
            result.roots,
          );
          assertEqual(`${block.label}:recorded`, block.expected, result.roots);
          runs += 1;
          runsByEngine[engine] += 1;
          proofChecks += result.proofChecks;
        }
      }
      for (const scratchBuild of SCRATCH_BUILDS) {
        const result = yield* replayArchitectureGOne(
          block,
          scratchBuild,
          nativeOwner,
        );
        assertEqual(
          `${block.label}:architecture_g:${scratchBuild}`,
          baseline,
          result.roots,
        );
        assertEqual(`${block.label}:recorded`, block.expected, result.roots);
        runs += 1;
        runsByEngine.architecture_g += 1;
        proofChecks += result.proofChecks;
      }
    }
    return {
      corpusPath,
      blocks: blocks.length,
      runs,
      proofChecks,
      engines: ENGINES,
      runsByEngine,
      scratchBuilds: SCRATCH_BUILDS,
      nativeOwner,
      adversarialCoverage,
    };
  });

export const mpfReplayProgram = (
  corpusPath: string,
  options: MpfReplayOptions = {},
): Effect.Effect<MpfReplaySummary, unknown> =>
  Effect.gen(function* () {
    const binaryPath = resolve(
      options.nativeOwnerBinaryPath ??
        process.env.MPF_NATIVE_OWNER_BINARY_PATH ??
        DEFAULT_NATIVE_OWNER_BINARY_PATH,
    );
    const binarySha256 = yield* Effect.tryPromise({
      try: async () =>
        createHash("sha256")
          .update(await readFile(binaryPath))
          .digest("hex"),
      catch: (cause) =>
        new Error(`Failed to identify Architecture G owner ${binaryPath}`, {
          cause,
        }),
    });
    const text = yield* Effect.try({
      try: () => FS.readFileSync(corpusPath, "utf8"),
      catch: (cause) =>
        new Error(`Failed to read MPF corpus ${corpusPath}`, { cause }),
    });
    const blocks = text
      .split(/\r?\n/u)
      .map((line) => line.trim())
      .filter((line) => line.length > 0)
      .map((line) => JSON.parse(line) as MpfReplayCorpusBlock);
    return yield* replayMpfCorpusBlocks(blocks, corpusPath, {
      binaryPath,
      binarySha256,
    });
  });
