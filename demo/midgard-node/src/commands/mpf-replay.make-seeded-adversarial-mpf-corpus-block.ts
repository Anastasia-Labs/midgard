import { createHash } from "node:crypto";
import * as FS from "node:fs";
import { readFile } from "node:fs/promises";
import { resolve } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { trimmedEnvironmentValue } from "../environment.js";
import { type MpfBatchOp, type MpfReplayCorpusBlock } from "../mpf/index.js";
import {
  assertSeededAdversarialCoverage,
  encodeCorpusOp,
  h32,
  inspectAdversarialCoverage,
  mpfKeyDigestHex,
  replayArchitectureGOne,
} from "./mpf-replay.replay-architecture-gone.js";
import {
  assertEqual,
  DEFAULT_NATIVE_OWNER_BINARY_PATH,
  IMPLEMENTATIONS,
  type MpfReplayOptions,
  type MpfReplaySummary,
  type ReplayRoots,
  replayTypeScriptReference,
  SCRATCH_BUILDS,
} from "./mpf-replay.replay-type-script-reference.js";

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
    const result = yield* replayTypeScriptReference(block, "insert");
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
    const runsByImplementation = {
      typescript_reference: 0,
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
      for (const scratchBuild of SCRATCH_BUILDS) {
        const result = yield* replayTypeScriptReference(block, scratchBuild);
        baseline ??= result.roots;
        assertEqual(
          `${block.label}:typescript_reference:${scratchBuild}`,
          baseline,
          result.roots,
        );
        assertEqual(`${block.label}:recorded`, block.expected, result.roots);
        runs += 1;
        runsByImplementation.typescript_reference += 1;
        proofChecks += result.proofChecks;
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
        runsByImplementation.architecture_g += 1;
        proofChecks += result.proofChecks;
      }
    }
    return {
      corpusPath,
      blocks: blocks.length,
      runs,
      proofChecks,
      implementations: IMPLEMENTATIONS,
      runsByImplementation,
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
        trimmedEnvironmentValue("MPF_NATIVE_OWNER_BINARY_PATH") ??
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
