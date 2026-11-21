/** The runner's in-flight set over one process on the emulator, with the
 * production restricted wallet choosing every coin: a wallet-only Init this
 * process sent is not built again while it may still land, and once it
 * expires unseen its replacement spends the same fee coin, so a rollback that
 * brings the first back lands one Init and one thread of ours, never two; two
 * objectives of the process never pick a coin the other has in flight; and
 * the unbounded carriage publication is built once while its first copy is
 * unseen. Nothing is stored anywhere: the process's memory and the chain are
 * all there is. */
import {
  Emulator,
  generateEmulatorAccount,
  generateEmulatorAccountFromPrivateKey,
  Lucid,
  type LucidEvolution,
  type TxSigned,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { resolveProverSigner } from "../src/runtime.js";
import { WALLET_ONLY_VALIDITY_MS } from "../src/wallet-only-validity.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
  type FraudProofWorkflowObservation,
  type FraudProofWorkflowPreflight,
} from "../src/workflow/orchestrator.js";
import { runAdmittedFraudProofWorkflow } from "../src/workflow/orchestrator.run-admitted-fraud-proof-workflow.js";
import { bindWorkflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { restrictWorkflowWallet } from "../src/workflow/workflow-wallet.js";
import { emulatorSignedTransactionRecovery } from "./support/emulator-signed-recovery.js";
import { releaseFinality } from "./support/raw-l1-terminal-fixture.output.js";
import { rollBackSpend } from "./support/validation-trace-dispute-one-step-staging-ledger.js";
import { createTestWorkflowProcess } from "./support/workflow-process.js";
import {
  makeAdapter,
  REFERENCE_OUT_REF,
  REFERENCE_SCRIPT_HASH,
  terminalVerifier,
} from "./workflow.make-adapter.js";
import { DEPLOYMENT } from "./workflow-runtime.admitted-actuation.js";

const network = "Custom" as const;
const ada = (amount: bigint) => amount * 1_000_000n;
const label = ({ txHash, outputIndex }: UTxO) =>
  `${txHash}#${outputIndex.toString()}`;
/** How long an Init stays valid after it is built. */
const INIT_VALIDITY_MS = 120_000;
/** The prover's plain coins at the start, smallest first. */
const COINS = [ada(20n), ada(30n), ada(40n)];

/** One process over an emulator: the prover's coins, the key our threads
 * stand at, and where an opened thread goes. */
const scene = () => {
  const prover = generateEmulatorAccount({});
  const signer = resolveProverSigner({
    network,
    walletSeedPhrase: prover.seedPhrase,
  });
  const funder = generateEmulatorAccount({ lovelace: ada(1_000n) });
  const holder = generateEmulatorAccountFromPrivateKey({});
  const opened = generateEmulatorAccountFromPrivateKey({});
  const emulator = new Emulator([
    funder,
    ...COINS.map((lovelace) => ({
      ...prover,
      address: signer.address,
      assets: { lovelace },
    })),
  ]);
  let nowMs = 1_000_000;
  return {
    emulator,
    signer,
    funder,
    holder,
    opened,
    process: createTestWorkflowProcess({ nowMs: () => nowMs }),
    clock: {
      now: () => nowMs,
      advance: (ms: number) => {
        nowMs += ms;
      },
    },
  };
};

type Scene = ReturnType<typeof scene>;

/** The outputs at `address` as of the emulator's last block, as a chain
 * indexer reads them: a transaction still in the mempool spends nothing yet
 * (the emulator's own `getUtxos` hides what a mempool transaction spends). */
const confirmedAt = (s: Scene, address: string): UTxO[] =>
  Object.values(s.emulator.ledger)
    .map(({ utxo }) => utxo)
    .filter((utxo) => utxo.address === address);

/**
 * The objective on `headerHash` in the scene's process: Init pays a thread
 * of ours, Open spends it. The family reads the chain alone, as of its last
 * block: an opened thread of ours pends, a thread of ours requires Open, and
 * nothing requires Init. With `publication`, Init is the unbounded carriage
 * publication.
 */
const workflowOf = async (
  s: Scene,
  headerHash: string,
  { publication = false }: { readonly publication?: boolean } = {},
) => {
  const objective = { category: "doubleSpend" as const, headerHash };
  const sends = s.process.sends(objective);
  const lucid: LucidEvolution = await Lucid(s.emulator, network);
  restrictWorkflowWallet({ signer: s.signer, sends }).selectWallet(lucid);
  const built: { actionId: string; signed: TxSigned }[] = [];
  const control = { broadcast: true };
  const ours = (utxos: readonly UTxO[]) =>
    utxos.filter(({ txHash }) =>
      built.some(({ signed }) => signed.toHash() === txHash),
    );
  const base = makeAdapter();
  const adapter: FraudProofFamilyWorkflowAdapter = {
    ...base,
    observe: async (): Promise<FraudProofWorkflowObservation> => {
      if (ours(confirmedAt(s, s.opened.address)).length > 0)
        return { kind: "pending", reason: "opened" };
      const [thread] = ours(confirmedAt(s, s.holder.address));
      const action: FraudProofWorkflowAction =
        thread === undefined
          ? { actionId: "init", input: { actionKind: "init" } }
          : {
              actionId: "open",
              input: { actionKind: "open", thread: label(thread) },
            };
      return { kind: "action_required", action };
    },
    preflight: async ({ action }) => {
      let signed: TxSigned;
      if (action.actionId === "init") {
        const pay = lucid
          .newTx()
          .pay.ToAddress(s.holder.address, { lovelace: ada(5n) });
        signed = await (
          await (
            publication ? pay : pay.validTo(s.emulator.now() + INIT_VALIDITY_MS)
          ).complete()
        ).sign
          .withWallet()
          .complete();
      } else {
        const [txHash, index] = String(action.input.thread).split("#");
        const [thread] = await s.emulator.getUtxosByOutRef([
          { txHash: txHash!, outputIndex: Number(index) },
        ]);
        signed = await (
          await lucid
            .newTx()
            .collectFrom([thread!])
            .pay.ToAddress(s.opened.address, { lovelace: ada(3n) })
            .addSigner(s.holder.address)
            .complete()
        ).sign
          .withWallet()
          .sign.withPrivateKey(s.holder.privateKey)
          .complete();
      }
      built.push({ actionId: action.actionId, signed });
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
        ...(publication && action.actionId === "init"
          ? { unboundedCarriagePublication: true as const }
          : {}),
      });
      return bindWorkflowPreflightTransaction(preflight, signed);
    },
    // With `broadcast` off, the bytes never reach the ledger: a lost
    // broadcast, unseen by the chain.
    submit: async ({ preflight }) => {
      const { signed } = built.find(
        ({ signed: candidate }) => candidate.toHash() === preflight.txHash,
      )!;
      if (control.broadcast) await s.emulator.submitTx(signed.toCBOR());
      return { kind: "submitted" as const, txHash: preflight.txHash };
    },
  };
  const recovery = emulatorSignedTransactionRecovery(s.emulator);
  return {
    control,
    built: (actionId?: string) =>
      built
        .filter(
          (entry) => actionId === undefined || entry.actionId === actionId,
        )
        .map(({ signed }) => signed),
    sent: () => s.process.sent().filter(sendsOwn),
    threads: async () => ours(await s.emulator.getUtxos(s.holder.address)),
    run: async () =>
      await runAdmittedFraudProofWorkflow({
        deploymentFingerprint: DEPLOYMENT,
        category: "doubleSpend",
        headerHash,
        prepareFamilyArtifact: async () => ({}),
        registry: createFraudProofWorkflowRegistry({
          adapters: [adapter],
          launchScope: ["doubleSpend"],
        }),
        run: s.process.run(objective),
        terminalVerifier: {
          ...terminalVerifier,
          observeSignedTransaction: recovery.observeSignedTransaction,
        },
        releaseFinality: {
          ...releaseFinality,
          deploymentIdentityDigest: DEPLOYMENT,
        },
        nowMs: s.clock.now,
        walletOnlySend: {
          open: () => true,
          latestValidToSlot: () =>
            lucid.unixTimeToSlot(s.emulator.now() + WALLET_ONLY_VALIDITY_MS),
        },
      }),
  };
  function sendsOwn(sent: { readonly txHash: string }) {
    return built.some(({ signed }) => signed.toHash() === sent.txHash);
  }
};

const H1 = "a1".repeat(28);
const H2 = "a2".repeat(28);

/** The prover's coin of `lovelace` now. */
const coinOf = async (s: Scene, lovelace: bigint) =>
  (await s.emulator.getUtxos(s.signer.address)).find(
    (utxo) => utxo.assets.lovelace === lovelace,
  )!;

/** Sends a wallet-only Init that never reaches the ledger and lets it
 * expire, checking at every run before its bound that nothing is built
 * again; the Init's signed transaction and its wallet coins. */
const expireUnseenInit = async (
  s: Scene,
  workflow: Awaited<ReturnType<typeof workflowOf>>,
  beforeExpiry: () => Promise<void> = async () => undefined,
) => {
  workflow.control.broadcast = false;
  expect(await workflow.run()).toMatchObject({
    kind: "pending",
    reason: "transaction for init is pending",
  });
  const [first] = workflow.built("init");
  const [sent] = workflow.sent();
  expect(sent).toMatchObject({ walletOnly: true, txHash: first!.toHash() });
  // Before its bound: held on and never built again. The process's clock
  // stays inside the rebroadcast interval, so the bytes stay unseen.
  for (const slots of [20, 40, 40]) {
    s.emulator.awaitSlot(slots);
    expect(await workflow.run()).toMatchObject({
      kind: "pending",
      reason: "transaction for init is pending",
    });
    expect(workflow.built("init")).toHaveLength(1);
  }
  await beforeExpiry();
  s.emulator.awaitSlot(INIT_VALIDITY_MS / 1_000);
  return { first: first!, walletInputs: sent!.walletInputs };
};

describe("workflow in-flight set: one process's wallet coins", () => {
  it("does not build a sent wallet-only Init again before its bound, and builds its replacement on the same fee coin once it expired unseen, though a larger coin is now free", async () => {
    const s = scene();
    const workflow = await workflowOf(s, H1);
    const { first, walletInputs } = await expireUnseenInit(
      s,
      workflow,
      async () => {
        // A coin larger than any before: coin selection alone would take it.
        const funderLucid = await Lucid(s.emulator, network);
        funderLucid.selectWallet.fromSeed(s.funder.seedPhrase);
        const paid = await funderLucid
          .newTx()
          .pay.ToAddress(s.signer.address, { lovelace: ada(100n) })
          .complete();
        await (await paid.sign.withWallet().complete()).submit();
        s.emulator.awaitBlock();
      },
    );
    expect(walletInputs).toStrictEqual([label(await coinOf(s, ada(40n)))]);
    const larger = label(await coinOf(s, ada(100n)));

    workflow.control.broadcast = true;
    expect(await workflow.run()).toMatchObject({
      kind: "pending",
      reason: "transaction for init is pending",
    });
    const [, replacement] = workflow.built("init");
    expect(replacement).toBeDefined();
    const sent = workflow.sent().at(-1)!;
    expect(sent.txHash).toBe(replacement!.toHash());
    expect(sent.walletInputs).toStrictEqual(walletInputs);
    expect(sent.inputs).not.toContain(larger);
    s.emulator.awaitBlock();
    expect(await workflow.threads()).toHaveLength(1);
    // The expired bytes can no longer land.
    await expect(s.emulator.submitTx(first.toCBOR())).rejects.toThrow();
  });

  it("lands one Init and one thread of ours when a rollback drops the confirmed replacement and lands the expired original, and opens whichever thread stands", async () => {
    const s = scene();
    const workflow = await workflowOf(s, H1);
    const before = {
      slot: s.emulator.slot,
      time: s.emulator.time,
      blockHeight: s.emulator.blockHeight,
    };
    const { first, walletInputs } = await expireUnseenInit(s, workflow);
    const spent = await s.emulator.getUtxosByOutRef(
      walletInputs.map((outRef) => {
        const [txHash, index] = outRef.split("#");
        return { txHash: txHash!, outputIndex: Number(index) };
      }),
    );
    workflow.control.broadcast = true;
    await workflow.run();
    const [, replacement] = workflow.built("init");
    expect(workflow.sent().at(-1)!.walletInputs).toStrictEqual(walletInputs);
    s.emulator.awaitBlock();
    const [standing] = await workflow.threads();
    expect(standing?.txHash).toBe(replacement!.toHash());

    // A rollback to before the original's bound: the replacement is gone
    // and the original lands in its place.
    rollBackSpend(
      s.emulator,
      replacement!.toHash(),
      spent,
      replacement!.toTransaction().body().outputs().len(),
    );
    Object.assign(s.emulator, before);
    await s.emulator.submitTx(first.toCBOR());
    s.emulator.awaitBlock();
    // Both spend the one fee coin: the replacement cannot land beside it.
    await expect(s.emulator.submitTx(replacement!.toCBOR())).rejects.toThrow();
    const threads = await workflow.threads();
    expect(threads.map(({ txHash }) => txHash)).toStrictEqual([first.toHash()]);

    // The workflow opens the thread that stands, and no other.
    expect(await workflow.run()).toMatchObject({
      kind: "pending",
      reason: "transaction for open is pending",
    });
    const [open] = workflow.built("open");
    expect(workflow.sent().at(-1)!.inputs).toContain(label(threads[0]!));
    s.emulator.awaitBlock();
    expect(await workflow.run()).toMatchObject({
      kind: "pending",
      reason: expect.stringContaining("opened"),
    });
    expect(workflow.built("init")).toHaveLength(2);
    expect(workflow.built("open")).toStrictEqual([open]);
    expect(await workflow.threads()).toStrictEqual([]);
    expect(
      (await s.emulator.getUtxos(s.opened.address)).map(({ txHash }) => txHash),
    ).toStrictEqual([open!.toHash()]);
  });

  it("never lets two objectives of the process pick a coin the other has in flight", async () => {
    const s = scene();
    const one = await workflowOf(s, H1);
    const other = await workflowOf(s, H2);
    one.control.broadcast = false;
    other.control.broadcast = false;
    await one.run();
    await other.run();
    const [first] = one.sent();
    const [second] = other.sent();
    expect(first?.walletOnly).toBe(true);
    expect(second?.walletOnly).toBe(true);
    // Coin selection takes the largest coin first: the first objective has
    // it in flight, so the other builds on the next.
    expect(first!.walletInputs).toStrictEqual([
      label(await coinOf(s, ada(40n))),
    ]);
    expect(
      second!.inputs.filter((outRef) => first!.inputs.includes(outRef)),
    ).toStrictEqual([]);
    expect([...s.process.inFlight.heldOutRefs()].sort()).toStrictEqual(
      [...new Set([...first!.inputs, ...second!.inputs])].sort(),
    );
    // Both land, side by side: one thread each.
    await s.emulator.submitTx(one.built("init")[0]!.toCBOR());
    await s.emulator.submitTx(other.built("init")[0]!.toCBOR());
    s.emulator.awaitBlock();
    expect(await one.threads()).toHaveLength(1);
    expect(await other.threads()).toHaveLength(1);
  });

  it("builds the unbounded carriage publication once while its first copy is unseen, broadcasting those same bytes again until they land", async () => {
    const s = scene();
    const workflow = await workflowOf(s, H1, { publication: true });
    workflow.control.broadcast = false;
    await workflow.run();
    const [publication] = workflow.built("init");
    expect(
      publication!.toTransaction().body().ttl(),
      "the publication carries no bound",
    ).toBeUndefined();
    for (let run = 0; run < 3; run += 1) {
      s.emulator.awaitBlock(5);
      expect(await workflow.run()).toMatchObject({
        kind: "pending",
        reason: "transaction for init is pending",
      });
    }
    expect(workflow.built("init")).toStrictEqual([publication]);
    // Past the rebroadcast interval the run sends the same bytes again
    // through the wallet; they land, and nothing new was built for them.
    s.clock.advance(30_000);
    await workflow.run();
    s.emulator.awaitBlock();
    expect(
      (await s.emulator.getTransactionStatus(publication!.toHash())).status,
    ).toBe("confirmed");
    await workflow.run();
    expect(workflow.built("init")).toStrictEqual([publication]);
    expect(
      workflow
        .sent()
        .filter(({ actionId }) => actionId === "init")
        .map(({ txHash }) => txHash),
    ).toStrictEqual([publication!.toHash()]);
    expect(
      (await workflow.threads()).map(({ txHash }) => txHash),
    ).toStrictEqual([publication!.toHash()]);
  });
});
