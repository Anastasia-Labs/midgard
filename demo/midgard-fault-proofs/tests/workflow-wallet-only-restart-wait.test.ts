/** The restart wait in the runner: a transaction that spends only wallet
 * coins is held, with nothing sent for it, until the caller's wait is over,
 * while the moves ranked after it still go; one spending a specific output
 * of ours is sent at once; and a wallet-only transaction with no upper
 * validity bound, or one later than one window past the run's clock, other
 * than the field-preimage carriage publication, is refused. */
import { CML, type TxSigned } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { WALLET_ONLY_VALIDITY_MS } from "../src/wallet-only-validity.js";
import { runFraudProofWorkflowCli } from "../src/workflow/cli.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowPreflight,
} from "../src/workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import { createFraudProofWorkflowRegistry } from "../src/workflow/orchestrator.immutable-fraud-proof-workflow-registry.js";
import { runAdmittedFraudProofWorkflow } from "../src/workflow/orchestrator.run-admitted-fraud-proof-workflow.js";
import { bindWorkflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import {
  clockOnlyWalletOnlySend,
  WALLET_ONLY_BOUND_ROUNDING_SLOTS,
  type WalletOnlySend,
} from "../src/workflow/wallet-only-send-gate.js";
import { releaseFinality } from "./support/raw-l1-terminal-fixture.output.js";
import { WALLET_ONLY_SEND_ALWAYS_OPEN } from "./support/wallet-only-send-always-open.js";
import { createTestWorkflowProcess } from "./support/workflow-process.js";
import {
  chainOf,
  chainVerifier,
  makeAdapter,
  REFERENCE_OUT_REF,
  REFERENCE_SCRIPT_HASH,
} from "./workflow.make-adapter.js";
import {
  admittedActuation,
  DEPLOYMENT,
  fundingAddress,
  fundingKey,
} from "./workflow-runtime.admitted-actuation.js";

const FEE = 200_000n;
/** The caller's clock now: one-second slots from 0. */
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
/** The coins our wallet lists, and their value. */
const WALLET_COINS = [`${"81".repeat(32)}#0`, `${"81".repeat(32)}#1`];
const WALLET_LOVELACE = 40_000_000n;
/** A thread of ours at a governed contract. */
const THREAD_OUT_REF = `${"91".repeat(32)}#0`;
/** An output of ours at the proof-materials address, and its value. */
const MATERIALS_OUT_REF = `${"73".repeat(32)}#0`;
const MATERIALS_LOVELACE = 10_000_000n;

type Spend =
  /** Our wallet's coins alone. */
  | "wallet"
  /** Wallet coins and a thread of ours at a governed contract. */
  | "thread"
  /** Wallet coins and an output at the proof-materials address. */
  | "proof_materials";

/** The move a run offers after a held wallet-only action, when the scene
 * has one: it spends a thread of ours. */
const THREAD_MOVE = {
  actionId: "thread-move",
  input: { actionKind: "step-one", move: "thread" },
};

const outRefInput = (outRef: string) => {
  const [txHash, index] = outRef.split("#");
  return CML.TransactionInput.new(
    CML.TransactionHash.from_hex(txHash!),
    BigInt(index!),
  );
};

/** Our key's signed spend of `inputOutRefs` back to our wallet. */
const signedSpend = (input: {
  readonly inputOutRefs: readonly string[];
  readonly outputLovelace: bigint;
  readonly mint?: CML.Mint;
  readonly ttl?: bigint;
}): TxSigned => {
  const inputs = CML.TransactionInputList.new();
  for (const outRef of input.inputOutRefs) inputs.add(outRefInput(outRef));
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(fundingAddress),
      CML.Value.from_coin(input.outputLovelace),
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, FEE);
  if (input.mint !== undefined) body.set_mint(input.mint);
  if (input.ttl !== undefined) body.set_ttl(input.ttl);
  const witnesses = CML.TransactionWitnessSet.new();
  const vkeys = CML.VkeywitnessList.new();
  vkeys.add(
    CML.Vkeywitness.new(
      fundingKey.to_public(),
      fundingKey.sign(CML.hash_transaction(body).to_raw_bytes()),
    ),
  );
  witnesses.set_vkeywitnesses(vkeys);
  const transaction = CML.Transaction.new(body, witnesses, true, undefined);
  return {
    toTransaction: () => transaction,
    toHash: () => CML.hash_transaction(body).to_hex(),
    toCBOR: () => transaction.to_cbor_hex(),
  } as unknown as TxSigned;
};

/** A run whose action builds the transaction `spend` names; with
 * `threadMove`, a run that skips that action offers `THREAD_MOVE` next. A
 * wallet coin in the returned `spentElsewhere` is one another transaction
 * spent: no transaction built after that spends it. */
const scene = async ({
  spend = "wallet",
  ttl = TTL,
  unboundedCarriagePublication = false,
  threadMove = false,
}: {
  readonly spend?: Spend;
  readonly ttl?: bigint | null;
  readonly unboundedCarriagePublication?: boolean;
  readonly threadMove?: boolean;
} = {}) => {
  const spentElsewhere = new Set<string>();
  const actuation = await admittedActuation();
  const { headerHash } = actuation;
  const objective = { category: "doubleSpend" as const, headerHash };
  const process = createTestWorkflowProcess({ nowMs: () => NOW_MS });
  process.sends(objective).noteWalletCoins(WALLET_COINS);
  const action = { actionId: "step-one", input: { actionKind: "step-one" } };
  const sent: string[] = [];
  const built: TxSigned[] = [];
  const base = makeAdapter();
  const adapter: FraudProofFamilyWorkflowAdapter = {
    ...base,
    observe: async ({ inFlight, unbuiltActionIds }) =>
      (inFlight?.().length ?? 0) > 0
        ? { kind: "pending" as const, reason: "awaiting inclusion" }
        : {
            kind: "action_required" as const,
            action:
              threadMove && unbuiltActionIds?.has(action.actionId) === true
                ? THREAD_MOVE
                : action,
          },
    preflight: async ({ action: asked }) => {
      const spends: Spend =
        asked.actionId === THREAD_MOVE.actionId ? "thread" : spend;
      const burn = CML.Mint.new();
      burn.set(
        CML.ScriptHash.from_hex(TOKEN_POLICY),
        CML.AssetName.from_hex("aa"),
        -1n,
      );
      const signed = signedSpend({
        inputOutRefs: [
          ...WALLET_COINS.filter((coin) => !spentElsewhere.has(coin)),
          ...(spends === "thread" ? [THREAD_OUT_REF] : []),
          ...(spends === "proof_materials" ? [MATERIALS_OUT_REF] : []),
        ],
        outputLovelace:
          WALLET_LOVELACE -
          FEE +
          (spends === "thread" ? THREAD_LOVELACE : 0n) +
          (spends === "proof_materials" ? MATERIALS_LOVELACE : 0n),
        ...(spends === "thread" ? { mint: burn } : {}),
        ...(ttl === null ? {} : { ttl }),
      });
      built.push(signed);
      const preflight: FraudProofWorkflowPreflight = Object.freeze({
        actionId: asked.actionId,
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
  };
  const run = (open: WalletOnlySend["open"]) =>
    runAdmittedFraudProofWorkflow({
      deploymentFingerprint: DEPLOYMENT,
      category: "doubleSpend",
      headerHash,
      prepareFamilyArtifact: async () => ({}),
      registry: createFraudProofWorkflowRegistry({
        adapters: [adapter],
        launchScope: ["doubleSpend"],
      }),
      run: process.run(objective),
      // Every transaction sent is still pending on the chain.
      terminalVerifier: chainVerifier(base),
      releaseFinality: {
        ...releaseFinality,
        deploymentIdentityDigest: DEPLOYMENT,
      },
      nowMs: () => NOW_MS,
      walletOnlySend: { open, latestValidToSlot: () => LATEST_VALID_TO_SLOT },
    });
  return {
    process,
    objective,
    sent,
    built,
    run,
    chain: chainOf(base),
    spentElsewhere,
  };
};

const closed = () => vi.fn<WalletOnlySend["open"]>(async () => false);
const open = () => vi.fn<WalletOnlySend["open"]>(async () => true);

describe("workflow: the restart wait of a wallet-only transaction", () => {
  it("holds a wallet-only action before the wait is over, sending nothing and holding no coin for it", async () => {
    const { process, sent, run } = await scene();
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
    expect(process.sent()).toEqual([]);
    expect(process.inFlight.heldOutRefs()).toEqual([]);
    expect(sent).toEqual([]);
  });

  it("holds it while the wait is still closed, and sends it at the first run after the wait is over", async () => {
    const { process, sent, built, run } = await scene();
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
    expect(process.sent().map(({ txHash }) => txHash)).toEqual(sent);
  });

  it("sends an action spending a thread of ours at once: its own input excludes a duplicate", async () => {
    const { sent, built, run } = await scene({ spend: "thread" });
    const reader = closed();
    const sending = await run(reader);
    expect(sending).not.toHaveProperty("namedState");
    expect(reader).not.toHaveBeenCalled();
    expect(sent).toEqual([built.at(-1)!.toHash()]);
  });

  it("skips a held wallet-only action for the run and sends the thread move ranked after it, the wait still named", async () => {
    const { process, sent, built, run } = await scene({
      threadMove: true,
    });
    const reader = closed();
    const result = await run(reader);
    expect(reader).toHaveBeenCalledOnce();
    // The thread move went in the same run; only its transaction was built
    // past the wallet-only one, and sent.
    expect(built).toHaveLength(2);
    expect(sent).toEqual([built[1]!.toHash()]);
    expect(process.sent().map(({ actionId }) => actionId)).toEqual([
      THREAD_MOVE.actionId,
    ]);
    // The run ends with the thread move in flight and the held action still
    // held: the wait's named state stays visible, and the hold is no alarm.
    expect(result).toMatchObject({
      kind: "pending",
      reason: "transaction for thread-move is pending",
      namedState: RESTART_WAIT,
    });
    expect(result).not.toHaveProperty("alarms");
  });

  it("does not read an output at the proof-materials address as a wallet coin", async () => {
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

  it("drops an unbounded carriage publication once another transaction spends one of its coins, and builds the required action at the next observation", async () => {
    const { process, objective, sent, built, run, chain, spentElsewhere } =
      await scene({
        ttl: null,
        unboundedCarriagePublication: true,
      });
    await run(open());
    const first = built.at(-1)!.toHash();
    expect(sent).toEqual([first]);
    // While it can still land, the run holds on it and builds nothing.
    expect(await run(open())).toMatchObject({
      kind: "pending",
      reason: "transaction for step-one is pending",
    });
    expect(built).toHaveLength(1);
    // Another transaction spends one of its coins: with no validity bound
    // it is still dead, and the chain says so.
    spentElsewhere.add(WALLET_COINS[0]!);
    chain.status.set(first, "invalidated_at_tip");
    // The run that sees it dead drops it from the in-flight set, its coins
    // free, and ends on the observation it already took.
    expect(await run(open())).toMatchObject({
      kind: "pending",
      reason: "awaiting inclusion",
    });
    expect(process.sends(objective).inFlight()).toEqual([]);
    expect(built).toHaveLength(1);
    // The next observation requires the action again, and the run builds it
    // on the coin still ours.
    expect(await run(open())).toMatchObject({ kind: "pending" });
    expect(built).toHaveLength(2);
    const second = built[1]!.toHash();
    expect(second).not.toBe(first);
    expect(sent).toEqual([first, second]);
    expect(
      process
        .sends(objective)
        .inFlight()
        .map(({ txHash }) => txHash),
    ).toEqual([second]);
  });

  it("refuses any other wallet-only transaction with no upper validity bound, naming the action", async () => {
    const { process, sent, run } = await scene({ ttl: null });
    const refused = await run(open());
    expect(refused).toMatchObject({
      kind: "stalled",
      reason: expect.stringContaining(
        "preflight failed for step-one: WalletOnlyValidityBoundError: wallet-only transaction of step-one carries no upper validity bound",
      ),
    });
    expect(process.sent()).toEqual([]);
    expect(sent).toEqual([]);
  });

  it("sends a wallet-only transaction whose upper bound is exactly the latest one window past the run's clock allows", async () => {
    const { sent, built, run } = await scene({ ttl: LATEST_TTL });
    await run(open());
    expect(built.at(-1)!.toTransaction().body().ttl()).toBe(LATEST_TTL);
    expect(sent).toEqual([built.at(-1)!.toHash()]);
  });

  it("refuses a wallet-only transaction bound one slot past the rounding allowance, naming the action", async () => {
    const { process, sent, run } = await scene({
      ttl: LATEST_TTL + 1n,
    });
    const refused = await run(open());
    expect(refused).toMatchObject({
      kind: "stalled",
      reason: expect.stringContaining(
        `preflight failed for step-one: WalletOnlyValidityBoundError: wallet-only transaction of step-one carries upper validity bound slot ${(LATEST_TTL + 1n).toString()}, later than slot ${LATEST_TTL.toString()}, one window past the caller's clock`,
      ),
    });
    expect(process.sent()).toEqual([]);
    expect(sent).toEqual([]);
  });
});

describe("workflow: no run without the supervisor's in-flight set", () => {
  // Wallet coins and in-flight transactions live in the application
  // supervisor's in-flight set; production never runs without it.
  it("refuses a CLI run without the in-flight set before any family runs", async () => {
    const actuation = await admittedActuation();
    await expect(
      runFraudProofWorkflowCli({
        category: "doubleSpend",
        deploymentFingerprint: DEPLOYMENT,
        headerHash: actuation.headerHash,
        decisionDigest: actuation.decisionDigest,
        actuationPermit: actuation.actuationPermit,
        runtimeConfigPath: "/etc/midgard/runtime.json",
        walletOnlySend: WALLET_ONLY_SEND_ALWAYS_OPEN,
      }),
    ).rejects.toThrow(
      "production workflow actuation requires an admitted decision, its actuation permit and the in-flight set from the application supervisor",
    );
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
      // It has no slot clock, so it checks no wallet-only bound.
      expect(() => latestValidToSlot()).toThrow("no slot clock");
    } finally {
      vi.useRealTimers();
    }
  });
});
