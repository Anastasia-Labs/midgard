/** The restart wait in the runner: a transaction that spends only wallet
 * coins is held, with nothing journaled or prepared for it, until the
 * caller's wait is over; one spending a specific output of ours is sent at
 * once; and a wallet-only transaction with no upper validity bound, or one
 * later than one window past the run's clock, other than the field-preimage
 * carriage publication, is refused. */
import { CML, type TxSigned, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { proofMaterialsAddressOfFundingAddress } from "../src/funding/proof-materials-address.js";
import { WALLET_ONLY_VALIDITY_MS } from "../src/wallet-only-validity.js";
import { runFraudProofWorkflowCli } from "../src/workflow/cli.js";
import { bindWorkflowFundingReservationJournal } from "../src/workflow/funding-reservation-permit.begin-workflow-funding-reservation-action.js";
import { unsafeWorkflowFundingReservationSelectedOutRefsForTest } from "../src/workflow/funding-reservation-permit.restrict-workflow-funding-signer.js";
import { MemoryFraudProofWorkflowJournalStore } from "../src/workflow/journal.directory-fraud-proof-workflow-journal-store.js";
import type { FraudProofWorkflowJournalEntry } from "../src/workflow/journal.fraud-proof-workflow-journal-event.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowPreflight,
} from "../src/workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import { createFraudProofWorkflowRegistry } from "../src/workflow/orchestrator.immutable-fraud-proof-workflow-registry.js";
import { runAdmittedFraudProofWorkflow } from "../src/workflow/orchestrator.run-admitted-fraud-proof-workflow.js";
import { readWorkflowRuntimeFundingPolicy } from "../src/workflow/runtime-funding-policy.js";
import { bindWorkflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import {
  clockOnlyWalletOnlySend,
  WALLET_ONLY_BOUND_ROUNDING_SLOTS,
  type WalletOnlySend,
} from "../src/workflow/wallet-only-send-gate.js";
import { releaseFinality } from "./support/raw-l1-terminal-fixture.output.js";
import { WALLET_ONLY_SEND_ALWAYS_OPEN } from "./support/wallet-only-send-always-open.js";
import {
  makeAdapter,
  REFERENCE_OUT_REF,
  REFERENCE_SCRIPT_HASH,
  terminalVerifier,
} from "./workflow.make-adapter.js";
import {
  admittedActuation,
  DEPLOYMENT,
  fundingAddress,
} from "./workflow-runtime.admitted-actuation.js";
import {
  runtimeFunding,
  signedFundingTransaction,
} from "./workflow-runtime.runtime-funding.js";

const FEE = 200_000n;
/** The run's journal clock, and the caller's clock now: one-second slots
 * from 0. */
const NOW_MS = 1_000_000;
/** The caller's slot one window past its clock now. */
const LATEST_VALID_TO_SLOT = Math.floor(
  (NOW_MS + WALLET_ONLY_VALIDITY_MS) / 1_000,
);
/** The latest upper bound a wallet-only transaction may carry: the caller's
 * latest slot, with the rounding allowance. */
const LATEST_TTL =
  BigInt(LATEST_VALID_TO_SLOT) + WALLET_ONLY_BOUND_ROUNDING_SLOTS;
const TTL = LATEST_TTL - 10n;
const RESTART_WAIT = { name: "fault_proof_restart_wait", alarm: false };
const TOKEN_POLICY = "e1".repeat(28);
const THREAD_LOVELACE = 3_000_000n;
/** The reserved coin the proof-materials scene resolves at the
 * proof-materials address, and its value. */
const MATERIALS_OUT_REF = `${"73".repeat(32)}#0`;
const MATERIALS_LOVELACE = 10_000_000n;

class RecordingJournal extends MemoryFraudProofWorkflowJournalStore {
  readonly kinds: string[] = [];

  override async append(
    entry: FraudProofWorkflowJournalEntry,
    expectedSequence: number,
  ): Promise<void> {
    await super.append(entry, expectedSequence);
    this.kinds.push(entry.event.kind);
  }
}

type Spend =
  /** The reservation's wallet coins alone. */
  | "wallet"
  /** Wallet coins and a thread of ours at a governed contract. */
  | "thread"
  /** Wallet coins and a reserved output at the proof-materials address. */
  | "proof_materials";

/** A funded run whose one action builds the transaction `spend` names. */
const scene = async ({
  spend = "wallet",
  ttl = TTL,
  unboundedCarriagePublication = false,
}: {
  readonly spend?: Spend;
  readonly ttl?: bigint | null;
  readonly unboundedCarriagePublication?: boolean;
} = {}) => {
  const sample = await runtimeFunding("step-one");
  const contract = readWorkflowRuntimeFundingPolicy(sample.policy).contracts[0]!
    .address;
  const thread: UTxO = {
    txHash: "91".repeat(32),
    outputIndex: 0,
    address: contract,
    assets: { lovelace: THREAD_LOVELACE, [`${TOKEN_POLICY}aa`]: 1n },
    datum: "00",
  };
  const threadRef = `${thread.txHash}#0`;
  const materials: UTxO = {
    txHash: "73".repeat(32),
    outputIndex: 0,
    address: proofMaterialsAddressOfFundingAddress(fundingAddress),
    assets: { lovelace: MATERIALS_LOVELACE },
  };
  const journal = new RecordingJournal();
  const runtime = await runtimeFunding("step-one", {
    begin: false,
    journal,
    ...(spend === "thread"
      ? { additionalInputs: [thread], confirmedInputs: [thread] }
      : spend === "proof_materials"
        ? { additionalInputs: [materials] }
        : {}),
  });
  const action = { actionId: "step-one", input: { actionKind: "step-one" } };
  const sent: string[] = [];
  const built: TxSigned[] = [];
  const base = makeAdapter();
  const adapter: FraudProofFamilyWorkflowAdapter = {
    ...base,
    observe: async ({ entries }) =>
      entries.some(({ event }) => event.kind === "submitted")
        ? { kind: "pending" as const, reason: "awaiting inclusion" }
        : { kind: "action_required" as const, action },
    preflight: async () => {
      const funding = unsafeWorkflowFundingReservationSelectedOutRefsForTest(
        runtime.permit,
      ).fundingOutRefs;
      const fundingLovelace = funding
        .map((outRef) =>
          BigInt(
            runtime.snapshot.activeInputs.find(
              (input) => input.outRef === outRef,
            )!.lovelace,
          ),
        )
        .reduce((total, lovelace) => total + lovelace, 0n);
      const burn = CML.Mint.new();
      burn.set(
        CML.ScriptHash.from_hex(TOKEN_POLICY),
        CML.AssetName.from_hex("aa"),
        -1n,
      );
      const signed = signedFundingTransaction({
        inputOutRefs: [
          ...funding,
          ...(spend === "thread" ? [threadRef] : []),
          ...(spend === "proof_materials" ? [MATERIALS_OUT_REF] : []),
        ],
        outputLovelace:
          fundingLovelace -
          FEE +
          (spend === "thread" ? THREAD_LOVELACE : 0n) +
          (spend === "proof_materials" ? MATERIALS_LOVELACE : 0n),
        fee: FEE,
        ...(spend === "thread" ? { mint: burn } : {}),
        ...(ttl === null ? {} : { ttl }),
      });
      built.push(signed);
      const preflight: FraudProofWorkflowPreflight = Object.freeze({
        actionId: action.actionId,
        txHash: signed.toHash(),
        scriptExecution: "reference_scripts" as const,
        localUplcEvaluation: {
          status: "passed" as const,
          evaluator: "uplc-v1",
        },
        referenceScripts: [
          {
            role: "family-step",
            outRef: REFERENCE_OUT_REF,
            scriptHash: REFERENCE_SCRIPT_HASH,
          },
        ] as const,
        ...(unboundedCarriagePublication
          ? { unboundedCarriagePublication: true as const }
          : {}),
      });
      return bindWorkflowPreflightTransaction(preflight, signed);
    },
    submit: async ({ preflight }) => {
      sent.push(preflight.txHash);
      return { kind: "submitted" as const, txHash: preflight.txHash };
    },
    reconcile: async ({ txHash }) => ({
      kind: "pending" as const,
      txHash: txHash!,
    }),
  };
  const { headerHash } = runtime.actuation;
  const run = (open: WalletOnlySend["open"]) =>
    runAdmittedFraudProofWorkflow({
      deploymentFingerprint: DEPLOYMENT,
      category: "doubleSpend",
      headerHash,
      evidenceBinding: {
        route: "canonical_block",
        headerHash,
        payloadEnvelopeSha256: "77".repeat(32),
        payloadSha256: "78".repeat(32),
        l1BlockHash: "79".repeat(32),
        l1Slot: "1",
      },
      prepareFamilyArtifact: async () => ({}),
      registry: createFraudProofWorkflowRegistry({
        adapters: [adapter],
        launchScope: ["doubleSpend"],
      }),
      journal,
      terminalVerifier,
      releaseFinality: {
        ...releaseFinality,
        deploymentIdentityDigest: DEPLOYMENT,
      },
      now: () => new Date(NOW_MS),
      walletOnlySend: { open, latestValidToSlot: () => LATEST_VALID_TO_SLOT },
    });
  return { journal, runtime, sent, built, run };
};

const closed = () => vi.fn<WalletOnlySend["open"]>(async () => false);
const open = () => vi.fn<WalletOnlySend["open"]>(async () => true);

describe("workflow: the restart wait of a wallet-only transaction", () => {
  it("holds a wallet-only action before the wait is over, leaving nothing journaled, prepared or sent", async () => {
    const { journal, runtime, sent, run } = await scene();
    const reader = closed();
    const held = await run(reader);
    expect(held).toMatchObject({
      kind: "pending",
      resumeOnObservation: true,
      namedState: RESTART_WAIT,
      reason: expect.stringContaining(
        "fault_proof_restart_wait: step-one spends only wallet coins",
      ),
    });
    expect(reader).toHaveBeenCalled();
    expect(journal.kinds).not.toContain("preflight_passed");
    expect(journal.kinds).not.toContain("submission_intent");
    expect(journal.kinds).not.toContain("stalled");
    // No reservation transition holds the coins for it.
    expect(runtime.prepare).not.toHaveBeenCalled();
    expect(sent).toEqual([]);
  });

  it("holds it while the wait is still closed, and sends it at the first run after the wait is over", async () => {
    const { journal, runtime, sent, built, run } = await scene();
    // The caller's wait opens on the clock and on its observed chain; the
    // runner reads only the caller's answer.
    const wait = { clockPast: true, chainPast: false };
    const reader = () => wait.clockPast && wait.chainPast;
    expect(await run(reader)).toMatchObject({ namedState: RESTART_WAIT });
    expect(sent).toEqual([]);
    wait.chainPast = true;
    const sending = await run(reader);
    expect(sending).not.toHaveProperty("namedState");
    expect(sent).toEqual([built.at(-1)!.toHash()]);
    expect(journal.kinds).toContain("preflight_passed");
    expect(journal.kinds).toContain("submission_intent");
    expect(runtime.prepare).toHaveBeenCalledOnce();
  });

  it("sends an action spending a thread of ours at once: its own input excludes a duplicate", async () => {
    const { sent, built, run } = await scene({ spend: "thread" });
    const reader = closed();
    const sending = await run(reader);
    expect(sending).not.toHaveProperty("namedState");
    expect(reader).not.toHaveBeenCalled();
    expect(sent).toEqual([built.at(-1)!.toHash()]);
  });

  it("does not read a reserved output at the proof-materials address as a wallet coin", async () => {
    const { run } = await scene({ spend: "proof_materials" });
    const reader = closed();
    // The run goes past the gate without asking whether the wait is over.
    const result = await run(reader);
    expect(result).not.toHaveProperty("namedState");
    expect(reader).not.toHaveBeenCalled();
  });

  it("holds the field-preimage carriage publication too, and sends it unbounded once the wait is over", async () => {
    const { sent, built, run } = await scene({
      ttl: null,
      unboundedCarriagePublication: true,
    });
    expect(await run(closed())).toMatchObject({ namedState: RESTART_WAIT });
    expect(sent).toEqual([]);
    await run(open());
    expect(sent).toEqual([built.at(-1)!.toHash()]);
    expect(built.at(-1)!.toTransaction().body().ttl()).toBeUndefined();
  });

  it("refuses any other wallet-only transaction with no upper validity bound, naming the action", async () => {
    const { journal, runtime, sent, run } = await scene({ ttl: null });
    const refused = await run(open());
    expect(refused).toMatchObject({
      kind: "stalled",
      reason: expect.stringContaining(
        "preflight failed for step-one: WalletOnlyValidityBoundError: wallet-only transaction of step-one carries no upper validity bound",
      ),
    });
    expect(journal.kinds).toContain("stalled");
    expect(journal.kinds).not.toContain("submission_intent");
    expect(runtime.prepare).not.toHaveBeenCalled();
    expect(sent).toEqual([]);
  });

  it("sends a wallet-only transaction whose upper bound is exactly the latest one window past the run's clock allows", async () => {
    const { sent, built, run } = await scene({ ttl: LATEST_TTL });
    await run(open());
    expect(built.at(-1)!.toTransaction().body().ttl()).toBe(LATEST_TTL);
    expect(sent).toEqual([built.at(-1)!.toHash()]);
  });

  it("refuses a wallet-only transaction bound one slot past the rounding allowance, naming the action", async () => {
    const { journal, runtime, sent, run } = await scene({
      ttl: LATEST_TTL + 1n,
    });
    const refused = await run(open());
    expect(refused).toMatchObject({
      kind: "stalled",
      reason: expect.stringContaining(
        `preflight failed for step-one: WalletOnlyValidityBoundError: wallet-only transaction of step-one carries upper validity bound slot ${(LATEST_TTL + 1n).toString()}, later than slot ${LATEST_TTL.toString()}, one window past the caller's clock`,
      ),
    });
    expect(journal.kinds).not.toContain("submission_intent");
    expect(runtime.prepare).not.toHaveBeenCalled();
    expect(sent).toEqual([]);
  });
});

describe("workflow: no run without a funding reservation", () => {
  // The wallet-coin set comes from the run's funding reservation; with none
  // bound the runner would read no coin as a wallet coin. Production never
  // runs without one.
  it("refuses a CLI run without a funding reservation permit before any family runs", async () => {
    const actuation = await admittedActuation();
    await expect(
      runFraudProofWorkflowCli({
        mode: "run",
        category: "doubleSpend",
        deploymentFingerprint: DEPLOYMENT,
        headerHash: actuation.headerHash,
        decisionDigest: actuation.decisionDigest,
        actuationPermit: actuation.actuationPermit,
        journal: new MemoryFraudProofWorkflowJournalStore(),
        runtimeConfigPath: "/etc/midgard/runtime.json",
        walletOnlySend: WALLET_ONLY_SEND_ALWAYS_OPEN,
      }),
    ).rejects.toThrow(
      "production workflow actuation requires admitted decision and funding reservation permits from the application supervisor",
    );
  });

  it("refuses to bind a funding reservation permit it did not admit", () => {
    expect(() =>
      bindWorkflowFundingReservationJournal({
        journal: new MemoryFraudProofWorkflowJournalStore(),
        permit: Object.freeze({
          permitVersion:
            "midgard-production-workflow-funding-reservation-permit-v1",
        }) as never,
      }),
    ).toThrow("production funding reservation permit was not admitted");
  });
});

describe("workflow: the restart wait of a process with no L1 follower", () => {
  it("opens once the clock is one window past the start it was given", () => {
    vi.useFakeTimers({ toFake: ["Date"] });
    try {
      const startedAtMs = 1_000_000;
      const { open, latestValidToSlot } = clockOnlyWalletOnlySend(startedAtMs);
      vi.setSystemTime(startedAtMs + WALLET_ONLY_VALIDITY_MS - 1);
      expect(open()).toBe(false);
      vi.setSystemTime(startedAtMs + WALLET_ONLY_VALIDITY_MS);
      expect(open()).toBe(true);
      // It binds no funding reservation, so it checks no wallet-only bound.
      expect(() => latestValidToSlot()).toThrow("no slot clock");
    } finally {
      vi.useRealTimers();
    }
  });
});
