import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import blake2b from "blake2b";
import { Effect } from "effect";
import { Level } from "level";

import {
  buildNativeRootProbe,
  MidgardMpf,
  type MpfBatchOp,
  type MpfReplayCorpusBlock,
  type NativeMpfBuildContext,
  setMpfScratchBuild,
} from "../mpf/index.js";
import { ProductionNativeMpfOwnerService } from "../services/mpf-native-owner/index.js";
import {
  decodeEntry,
  decodeSourceEvents,
  type MpfReplaySummary,
  type ReplayRoots,
  verifyIndependentFinalProofs,
} from "./mpf-replay.replay-type-script-reference.js";

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
            { mode: "direct" },
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
            MidgardMpf.create(trieName, levelPath),
          );
          try {
            await Effect.runPromise(
              created.applyBatch(
                initial.map((entry) => ({
                  type: "insert" as const,
                  key: entry.key,
                  value: entry.value,
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

export const replayArchitectureGOne = (
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
          buildNativeRootProbe({
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

export const mpfKeyDigestHex = (keyHex: string): string =>
  Buffer.from(blake2b(32).update(Buffer.from(keyHex, "hex")).digest()).toString(
    "hex",
  );

const commonPrefixLength = (left: string, right: string): number => {
  let length = 0;
  while (length < left.length && left[length] === right[length]) length += 1;
  return length;
};

export const inspectAdversarialCoverage = (
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

export const assertSeededAdversarialCoverage = (
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

export const h32 = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(32);

export const encodeCorpusOp = (op: MpfBatchOp) =>
  op.type === "insert"
    ? {
        type: "insert" as const,
        key: op.key.toString("hex"),
        value: op.value.toString("hex"),
      }
    : { type: "delete" as const, key: op.key.toString("hex") };
