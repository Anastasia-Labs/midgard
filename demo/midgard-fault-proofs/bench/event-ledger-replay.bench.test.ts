/**
 * Wall-clock cost of reconstructing a block's event ledgers, run by
 * `pnpm bench`, not by `test`. Over `eventLedgerBlock` it times:
 *
 * - `scan`: the ledger-relative spend scan, which needs the ledger before
 *   every event of the block;
 * - `prove`: for a few events, the ledger before that event from the
 *   block's one walk and the resolution proof a validation trace takes of its
 *   input against it, the work a Plutus event's replay adds;
 * - `union`: the installed catalogue's replay of the block, but for the
 *   transition-trace family, which needs raw L1 events admitted for the
 *   header and reads no event ledger;
 * - `families`: each of those families' replays alone, to attribute the
 *   union's time.
 *
 * `EVENT_LEDGER_BENCH_EVENTS` (default 1000) and `EVENT_LEDGER_BENCH_ENTRIES`
 * (default 10000) size the block, and `EVENT_LEDGER_BENCH_SCENARIOS` (default
 * all) picks scenarios by name, comma-separated. Each scenario prints one
 * JSON line with its milliseconds and the process's peak RSS so far, so run
 * one scenario per process to read its own peak.
 *
 * Wall-clock time depends on the machine and its load, so this is not a
 * gate: `tests/predecessor-ledger-replay-cost.test.ts` gates the same replays
 * on the trie work they do.
 */
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  credentialToAddress,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { eventLedgerBefore } from "../src/event-ledger.js";
import { checkedLedgerPreState } from "../src/ledger-at.js";
import { detectLedgerRelativeMissingInputs } from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import * as completeReplay from "../src/workflow/complete-replay.js";
import { createFabricatedDepositEvidenceAuthority } from "../src/workflow/fabricated-deposit-evidence.js";
import { createFabricatedWithdrawalEvidenceAuthority } from "../src/workflow/fabricated-withdrawal-evidence.js";
import { INSTALLED_FRAUD_PROOF_CATEGORY_ORDER } from "../src/workflow/installed-categories.js";
import { eventLedgerBlock } from "./event-ledger-block.js";

const sizeFromEnv = (name: string, fallback: number): number => {
  const raw = process.env[name];
  if (raw === undefined) return fallback;
  const parsed = Number(raw);
  if (!Number.isSafeInteger(parsed) || parsed < 1)
    throw new Error(`${name} must be a positive integer, got ${raw}`);
  return parsed;
};

const EVENTS = sizeFromEnv("EVENT_LEDGER_BENCH_EVENTS", 1000);
const ENTRIES = sizeFromEnv("EVENT_LEDGER_BENCH_ENTRIES", 10_000);
const SCENARIOS = new Set(
  (
    process.env.EVENT_LEDGER_BENCH_SCENARIOS ?? "scan,prove,union,families"
  ).split(","),
);
/** How many events, evenly spaced and ending at the last, `prove` times. */
const PROVED_EVENTS = 4;

const report = (scenario: string, timings: Record<string, number>) =>
  console.log(
    JSON.stringify({
      scenario,
      events: EVENTS,
      entries: ENTRIES,
      ...timings,
      peakRssMb: Math.round(process.resourceUsage().maxRSS / 1024),
    }),
  );

const timed = async <T>(run: () => Promise<T>) => {
  const started = performance.now();
  const result = await run();
  return { result, ms: Math.round(performance.now() - started) };
};

// The block holds only normal transactions: no fabricated-event authority is
// asked for chain history.
const unusedEventProvider = new Proxy(
  {},
  {
    get: () => () => {
      throw new Error("the benchmark block has no deposits or withdrawals");
    },
  },
) as LucidEvolution;

const retention = {
  inlineLimitBytes: 512n,
  maxPayloadBytes: 5000n,
  maxPayloadNodes: 512n,
  retentionAddress: credentialToAddress("Preprod", {
    type: "Script",
    hash: "ee".repeat(28),
  }),
};

const authority = {
  lucid: unusedEventProvider,
  network: "Preprod" as const,
  hubOraclePolicyId: "c1".repeat(28),
  minimumConfirmationDepth: 1,
  history: retention,
};

/** Every installed family's replay in catalogue order, as the catalogue
 * replay builds it, without the transition-trace family. */
const installedFamiliesWithoutTransitionTrace = () => {
  const owner = "b1".repeat(28);
  const replays = [
    ...Object.values(completeReplay),
    completeReplay.createValidationTraceDisputeCompleteCanonicalReplay(
      NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
    ),
    completeReplay.createFabricatedDepositCompleteCanonicalReplay({
      authority: createFabricatedDepositEvidenceAuthority(authority),
      owner,
    }),
    completeReplay.createFabricatedWithdrawalCompleteCanonicalReplay({
      authority: createFabricatedWithdrawalEvidenceAuthority(authority),
      owner,
    }),
  ];
  const byCategory = new Map<string, completeReplay.CompleteCanonicalReplay>();
  for (const value of replays)
    if (
      typeof value === "object" &&
      value !== null &&
      "replayVersion" in value &&
      value.replayVersion === completeReplay.COMPLETE_CANONICAL_REPLAY &&
      (value as completeReplay.CompleteCanonicalReplay).launchScope.length === 1
    ) {
      const replay = value as completeReplay.CompleteCanonicalReplay;
      byCategory.set(replay.launchScope[0]!, replay);
    }
  return INSTALLED_FRAUD_PROOF_CATEGORY_ORDER.filter(
    (category) => category !== "transitionTrace",
  ).map((category) => {
    const member = byCategory.get(category);
    if (member === undefined)
      throw new Error(`no complete replay installs ${category}`);
    return { category, member };
  });
};

describe(`event-ledger replay over ${EVENTS} events and ${ENTRIES} entries`, () => {
  let block: Awaited<ReturnType<typeof eventLedgerBlock>>;

  beforeAll(async () => {
    const { result, ms } = await timed(() =>
      eventLedgerBlock({ events: EVENTS, entries: ENTRIES }),
    );
    block = result;
    report("fixture", { ms });
  });

  it.runIf(SCENARIOS.has("scan"))("scan", async () => {
    const { result, ms } = await timed(() =>
      detectLedgerRelativeMissingInputs({
        evidence: block.evidence,
        context: block.context,
        kind: "spend",
      }),
    );
    expect(result).toStrictEqual([]);
    report("scan", { ms });
  });

  it.runIf(SCENARIOS.has("prove"))("prove", async () => {
    let ledgerMs = 0;
    let proveMs = 0;
    for (let sample = 1; sample <= PROVED_EVENTS; sample += 1) {
      const step = Math.ceil((EVENTS * sample) / PROVED_EVENTS) - 1;
      const input = block.predecessorEntries[step]!.outRef;
      const ledger = await timed(() =>
        eventLedgerBefore({
          block: block.evidence,
          eventKey: { L2TransactionEventKey: { tx_id: block.txs[step]!.txId } },
          stepIndex: BigInt(step),
          predecessor:
            completeReplay.completeCanonicalReplayPredecessorLedger(block),
        }),
      );
      const proof = await timed(() =>
        checkedLedgerPreState(ledger.result).proveResolution([input]),
      );
      expect(proof.result).toHaveLength(1);
      ledgerMs += ledger.ms;
      proveMs += proof.ms;
    }
    report("prove", {
      ledgerMsPerEvent: Math.round(ledgerMs / PROVED_EVENTS),
      proveMsPerEvent: Math.round(proveMs / PROVED_EVENTS),
    });
  });

  it.runIf(SCENARIOS.has("union"))("union", async () => {
    const replay = completeReplay.createCompleteCanonicalReplayUnion(
      installedFamiliesWithoutTransitionTrace().map(({ member }) => member),
    );
    const { result, ms } = await timed(() =>
      replay.replay(block.evidence, block.context),
    );
    report("union", { ms, detections: result.detections.length });
    const byViolation: Record<string, number> = {};
    for (const { violationId } of result.detections)
      byViolation[violationId] = (byViolation[violationId] ?? 0) + 1;
    console.log(JSON.stringify({ byViolation }));
  });

  it.runIf(SCENARIOS.has("families"))("families", async () => {
    const msByFamily: Record<string, number> = {};
    for (const {
      category,
      member,
    } of installedFamiliesWithoutTransitionTrace())
      msByFamily[category] = (
        await timed(() => member.replay(block.evidence, block.context))
      ).ms;
    report("families", msByFamily);
  });
});
