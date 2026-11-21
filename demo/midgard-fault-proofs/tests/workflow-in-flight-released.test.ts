/** The follow of an objective's in-flight part once no run drives it
 * (in-flight-released.ts), over one process on the emulator behind a
 * provider that shows the mempool. Another party bound a run at one
 * address; objective A walks it, and then stops being driven (its header
 * left the queue). Its walk is followed outside any run: once it can no
 * longer land (another party's spend took the run) its part is forgotten
 * and its coins are free, and objective B, whose bind is read from the
 * empty run address, builds it; while the walk may still land, or has
 * landed short of final, its coins stay held and B's bind waits on it. */
import {
  Data,
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
  followReleasedInFlight,
  proofMaterialsSweepInFlight,
} from "../src/workflow/in-flight-released.js";
import type { FraudProofSignedTransactionRecovery } from "../src/workflow/l1-source.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowObservation,
} from "../src/workflow/orchestrator.js";
import { runAdmittedFraudProofWorkflow } from "../src/workflow/orchestrator.run-admitted-fraud-proof-workflow.js";
import { bindWorkflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { restrictWorkflowWallet } from "../src/workflow/workflow-wallet.js";
import { emulatorSignedTransactionRecovery } from "./support/emulator-signed-recovery.js";
import { releaseFinality } from "./support/raw-l1-terminal-fixture.output.js";
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
const RUN = { kind: "inline" as const, value: Data.void() };
const A = { category: "doubleSpend" as const, headerHash: "a1".repeat(28) };
const B = { category: "doubleSpend" as const, headerHash: "a2".repeat(28) };

/** One process over an emulator whose provider shows our mempool; the
 * funder has bound a run, landed. `releaseDepth` blocks under the tip are
 * the release-final point. */
const scene = async (releaseDepth = 0) => {
  const prover = generateEmulatorAccount({});
  const signer = resolveProverSigner({
    network,
    walletSeedPhrase: prover.seedPhrase,
  });
  const funder = generateEmulatorAccount({ lovelace: ada(1_000n) });
  const run = generateEmulatorAccountFromPrivateKey({});
  const emulator = new Emulator([
    funder,
    ...[ada(20n), ada(30n), ada(40n)].map((lovelace) => ({
      ...prover,
      address: signer.address,
      assets: { lovelace },
    })),
  ]);
  const funderLucid = await Lucid(emulator, network);
  funderLucid.selectWallet.fromSeed(funder.seedPhrase);
  await (
    await (
      await funderLucid
        .newTx()
        .pay.ToAddressWithData(run.address, RUN, { lovelace: ada(5n) })
        .complete()
    ).sign
      .withWallet()
      .complete()
  ).submit();
  emulator.awaitBlock();
  const mempool: TxSigned[] = [];
  const built: { actionId: string; signed: TxSigned }[] = [];
  const process = createTestWorkflowProcess({ nowMs: () => 1_000_000 });
  const recovery = emulatorSignedTransactionRecovery(emulator, releaseDepth);
  // One waiting in our mempool may still land.
  const observeSignedTransaction: FraudProofSignedTransactionRecovery["observeSignedTransaction"] =
    async (input) => {
      const observed = await recovery.observeSignedTransaction(input);
      return mempool.some((signed) => signed.toHash() === input.transactionHash)
        ? { ...observed, status: "pending", reason: "in the mempool" }
        : observed;
    };
  const shownAt = (address: string): UTxO[] => {
    const spent = new Set(
      mempool.flatMap((signed) => {
        const inputs = signed.toTransaction().body().inputs();
        return Array.from(
          { length: inputs.len() },
          (_, index) =>
            `${inputs.get(index).transaction_id().to_hex()}#${inputs.get(index).index().toString()}`,
        );
      }),
    );
    return Object.values(emulator.ledger)
      .map(({ utxo }) => utxo)
      .filter((utxo) => utxo.address === address && !spent.has(label(utxo)));
  };
  /** The objective's run: no run at the run's address requires a bind,
   * read from that absence; a run requires its walk. */
  const workflowOf = async (objective: typeof A) => {
    const sends = process.sends(objective);
    const lucid: LucidEvolution = await Lucid(emulator, network);
    restrictWorkflowWallet({ signer, sends }).selectWallet(lucid);
    const base = makeAdapter();
    const adapter: FraudProofFamilyWorkflowAdapter = {
      ...base,
      observe: async (): Promise<FraudProofWorkflowObservation> => {
        const [bound] = shownAt(run.address).filter(
          ({ datum }) => datum === RUN.value,
        );
        return bound === undefined
          ? {
              kind: "action_required",
              action: { actionId: "bind", input: { actionKind: "bind" } },
              absentAt: [run.address],
            }
          : {
              kind: "action_required",
              action: {
                actionId: `walk:${label(bound)}`,
                input: { actionKind: "walk", run: label(bound) },
              },
            };
      },
      preflight: async ({ action }) => {
        const [bound] = shownAt(run.address);
        const signed = await (
          action.actionId === "bind"
            ? lucid
                .newTx()
                .pay.ToAddressWithData(run.address, RUN, { lovelace: ada(5n) })
                .validTo(emulator.now() + 60_000)
            : lucid
                .newTx()
                .collectFrom([bound!])
                .pay.ToAddressWithData(run.address, RUN, { lovelace: ada(4n) })
                .addSigner(run.address)
        )
          .complete()
          .then(async (tx) =>
            action.actionId === "bind"
              ? await tx.sign.withWallet().complete()
              : await tx.sign
                  .withWallet()
                  .sign.withPrivateKey(run.privateKey)
                  .complete(),
          );
        built.push({ actionId: action.actionId, signed });
        return bindWorkflowPreflightTransaction(
          Object.freeze({
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
          }),
          signed,
        );
      },
      submit: async ({ preflight }) => {
        mempool.push(
          built.find(({ signed }) => signed.toHash() === preflight.txHash)!
            .signed,
        );
        return { kind: "submitted" as const, txHash: preflight.txHash };
      },
    };
    return async () =>
      await runAdmittedFraudProofWorkflow({
        deploymentFingerprint: DEPLOYMENT,
        category: objective.category,
        headerHash: objective.headerHash,
        prepareFamilyArtifact: async () => ({}),
        registry: createFraudProofWorkflowRegistry({
          adapters: [adapter],
          launchScope: ["doubleSpend"],
        }),
        run: process.run(objective),
        terminalVerifier: { ...terminalVerifier, observeSignedTransaction },
        releaseFinality: {
          ...releaseFinality,
          deploymentIdentityDigest: DEPLOYMENT,
        },
        nowMs: () => 1_000_000,
        walletOnlySend: {
          open: () => true,
          latestValidToSlot: () =>
            lucid.unixTimeToSlot(emulator.now() + WALLET_ONLY_VALIDITY_MS),
        },
      });
  };
  const sent = (prefix: string) =>
    built.filter(({ actionId }) => actionId.startsWith(prefix));
  return {
    emulator,
    funder,
    run,
    mempool,
    process,
    workflowOf,
    sent,
    /** A's walk and the wallet coins it spends, sent and not landed; A is
     * then driven no more. */
    walked: async () => {
      expect(await (await workflowOf(A))()).toMatchObject({
        kind: "pending",
      });
      const [walk] = process.inFlight.forObjective(A).inFlight();
      expect(walk?.actionId).toMatch(/^walk:/u);
      expect(walk!.walletInputs.length).toBeGreaterThan(0);
      return walk!;
    },
    /** The follow at a pass at tip, with no objective driven. */
    pass: async () =>
      await followReleasedInFlight({
        inFlight: process.inFlight,
        driven: () => false,
        observeSignedTransaction,
      }),
    /** Puts the mempool in a block. */
    land: async () => {
      for (const signed of mempool.splice(0))
        await emulator.submitTx(signed.toCBOR());
      emulator.awaitBlock();
    },
  };
};

/** Another party's spend takes the bound run, as another prover's removal
 * takes what our step spent; our walk drops from the mempool. */
const takeRun = async (s: Awaited<ReturnType<typeof scene>>) => {
  s.mempool.splice(0);
  const [bound] = await s.emulator.getUtxos(s.run.address);
  const foreign = await Lucid(s.emulator, network);
  foreign.selectWallet.fromPrivateKey(s.run.privateKey);
  await (
    await (
      await foreign
        .newTx()
        .collectFrom([bound!])
        .pay.ToAddress(s.funder.address, { lovelace: ada(2n) })
        .complete({ changeAddress: s.funder.address })
    ).sign
      .withWallet()
      .complete()
  ).submit();
  s.emulator.awaitBlock();
};

const parts = (s: Awaited<ReturnType<typeof scene>>) =>
  s.process.inFlight.objectives().map(({ headerHash }) => headerHash);

describe("workflow in-flight set: the follow of an objective no run drives", () => {
  it("forgets a released objective's walk once another party's spend took its input, frees its coins, and lets another objective's bind read from that address be built", async () => {
    const s = await scene();
    const walk = await s.walked();
    expect(s.process.inFlight.heldOutRefs()).toEqual(
      expect.arrayContaining([...walk.walletInputs]),
    );
    await takeRun(s);
    // Before the pass, B's bind waits on A's dead walk.
    const b = await s.workflowOf(B);
    expect(await b()).toMatchObject({
      kind: "pending",
      reason: expect.stringContaining(
        `bind waits on transaction ${walk.txHash} of walk:`,
      ),
    });
    expect(s.sent("bind")).toStrictEqual([]);

    await s.pass();
    expect(parts(s)).not.toContain(A.headerHash);
    expect(s.process.inFlight.sending(A)).toBe(false);
    for (const coin of walk.walletInputs)
      expect(s.process.inFlight.heldOutRefs()).not.toContain(coin);

    // B's bind is built now, at the next run.
    expect(await b()).toMatchObject({ kind: "pending" });
    expect(s.sent("bind")).toHaveLength(1);
  });

  it("keeps a released objective's walk that may still land, holding its coins, until it lands final", async () => {
    const s = await scene(2);
    const walk = await s.walked();
    const held = () =>
      walk.walletInputs.every((coin) =>
        s.process.inFlight.heldOutRefs().includes(coin),
      );
    // Waiting in the mempool: kept.
    await s.pass();
    expect(s.process.inFlight.sending(A)).toBe(true);
    expect(held()).toBe(true);
    // Landed short of the release-final point: kept, and B still waits.
    await s.land();
    await s.pass();
    expect(s.process.inFlight.sending(A)).toBe(true);
    expect(held()).toBe(true);
    // Final at release depth: settled, and the part forgotten.
    s.emulator.awaitBlock();
    s.emulator.awaitBlock();
    await s.pass();
    expect(s.process.inFlight.sending(A)).toBe(false);
    expect(parts(s)).not.toContain(A.headerHash);
    expect(s.process.inFlight.heldOutRefs()).toStrictEqual([]);
  });

  it("keeps a released objective's walk that may still land until it can land no more", async () => {
    const s = await scene();
    const walk = await s.walked();
    await s.pass();
    expect(s.process.inFlight.sending(A)).toBe(true);
    const b = await s.workflowOf(B);
    await takeRun(s);
    await s.pass();
    expect(s.process.inFlight.sending(A)).toBe(false);
    for (const coin of walk.walletInputs)
      expect(s.process.inFlight.heldOutRefs()).not.toContain(coin);
    await b();
    expect(s.sent("bind")).toHaveLength(1);
  });

  it("leaves a driven objective's part to its run", async () => {
    const s = await scene();
    await s.walked();
    await takeRun(s);
    await followReleasedInFlight({
      inFlight: s.process.inFlight,
      driven: ({ headerHash }) => headerHash === A.headerHash,
      observeSignedTransaction: async () => {
        throw new Error("a driven part is not read");
      },
    });
    expect(s.process.inFlight.sending(A)).toBe(true);
    expect(proofMaterialsSweepInFlight(s.process.inFlight)).toBe(false);
  });
});
