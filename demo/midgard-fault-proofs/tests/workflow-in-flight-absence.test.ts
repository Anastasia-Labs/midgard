/** The runner's wait on a requirement read from finding no output where a
 * transaction of ours may still land (in-flight-absence.ts), over one
 * process on the emulator behind a provider that shows the mempool: a
 * transaction of ours waiting there hides the output it spends, and what it
 * makes shows only once it lands. Another party bound a run at one address;
 * we walk it there, each walk spending the run and making the next. While a
 * walk of ours may still land the run is hidden, and the bind the empty
 * address reads as required is not built, whichever objective of the
 * process sent the walk; once the walk lands the walk after it is built;
 * once the walk can no longer land and the run is gone, the bind is
 * built. */
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
/** A bound run's value; each walk takes one ADA off it, and a run of
 * `DONE` is walked to its end. */
const BOUND = ada(5n);
const DONE = ada(3n);
/** What marks a run: its inline datum. */
const RUN = { kind: "inline" as const, value: Data.void() };

/** One process over an emulator, behind a provider that shows the mempool:
 * our transactions wait in `mempool` until `land` puts them in a block. The
 * funder has bound a run, landed. */
const scene = async () => {
  const prover = generateEmulatorAccount({});
  const signer = resolveProverSigner({
    network,
    walletSeedPhrase: prover.seedPhrase,
  });
  const funder = generateEmulatorAccount({ lovelace: ada(1_000n) });
  // The run's address, spendable by its key, as anyone's walk spends it.
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
        .pay.ToAddressWithData(run.address, RUN, { lovelace: BOUND })
        .complete()
    ).sign
      .withWallet()
      .complete()
  ).submit();
  emulator.awaitBlock();
  const mempool: TxSigned[] = [];
  /** Every transaction the process built, by any objective. */
  const built: { actionId: string; signed: TxSigned }[] = [];
  return {
    emulator,
    signer,
    funder,
    run,
    mempool,
    built,
    process: createTestWorkflowProcess({ nowMs: () => 1_000_000 }),
    /** The outputs at `address` as the provider shows them: the ledger's,
     * less those a transaction in the mempool spends. */
    shownAt: (address: string): UTxO[] => {
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
    },
    /** Puts the mempool in a block. */
    land: async () => {
      for (const signed of mempool.splice(0))
        await emulator.submitTx(signed.toCBOR());
      emulator.awaitBlock();
    },
  };
};

type Scene = Awaited<ReturnType<typeof scene>>;

/**
 * The objective on `headerHash` in the scene's process. The family reads
 * the provider: no run at the run's address, whoever bound it, requires a
 * bind, read from that absence; a run above `DONE` requires its walk; a run
 * at `DONE` pends.
 */
const workflowOf = async (s: Scene, headerHash: string) => {
  const objective = { category: "doubleSpend" as const, headerHash };
  const sends = s.process.sends(objective);
  const lucid: LucidEvolution = await Lucid(s.emulator, network);
  restrictWorkflowWallet({ signer: s.signer, sends }).selectWallet(lucid);
  const base = makeAdapter();
  const adapter: FraudProofFamilyWorkflowAdapter = {
    ...base,
    observe: async (): Promise<FraudProofWorkflowObservation> => {
      const [run] = s
        .shownAt(s.run.address)
        .filter(({ datum }) => datum === RUN.value);
      if (run === undefined)
        return {
          kind: "action_required",
          action: { actionId: "bind", input: { actionKind: "bind" } },
          absentAt: [s.run.address],
        };
      if (run.assets.lovelace <= DONE)
        return { kind: "pending", reason: "walked" };
      const action: FraudProofWorkflowAction = {
        actionId: `walk:${label(run)}`,
        input: { actionKind: "walk", run: label(run) },
      };
      return { kind: "action_required", action };
    },
    preflight: async ({ action }) => {
      let signed: TxSigned;
      if (action.actionId === "bind") {
        signed = await (
          await lucid
            .newTx()
            .pay.ToAddressWithData(s.run.address, RUN, { lovelace: BOUND })
            .validTo(s.emulator.now() + 60_000)
            .complete()
        ).sign
          .withWallet()
          .complete();
      } else {
        const [txHash, index] = String(action.input.run).split("#");
        const [run] = await s.emulator.getUtxosByOutRef([
          { txHash: txHash!, outputIndex: Number(index) },
        ]);
        signed = await (
          await lucid
            .newTx()
            .collectFrom([run!])
            .pay.ToAddressWithData(s.run.address, RUN, {
              lovelace: run!.assets.lovelace - ada(1n),
            })
            .addSigner(s.run.address)
            .complete()
        ).sign
          .withWallet()
          .sign.withPrivateKey(s.run.privateKey)
          .complete();
      }
      s.built.push({ actionId: action.actionId, signed });
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
      });
      return bindWorkflowPreflightTransaction(preflight, signed);
    },
    submit: async ({ preflight }) => {
      const { signed } = s.built.find(
        ({ signed: candidate }) => candidate.toHash() === preflight.txHash,
      )!;
      s.mempool.push(signed);
      return { kind: "submitted" as const, txHash: preflight.txHash };
    },
  };
  const recovery = emulatorSignedTransactionRecovery(s.emulator);
  return {
    built: (prefix: string) =>
      s.built
        .filter(({ actionId }) => actionId.startsWith(prefix))
        .map(({ signed }) => signed),
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
          // One in the mempool may still land; one the mempool dropped,
          // whose run another spend took, never can.
          observeSignedTransaction: async (input) => {
            const observed = await recovery.observeSignedTransaction(input);
            if (
              s.mempool.some(
                (signed) => signed.toHash() === input.transactionHash,
              )
            )
              return {
                ...observed,
                status: "pending",
                reason: "in the mempool",
              };
            return observed;
          },
        },
        releaseFinality: {
          ...releaseFinality,
          deploymentIdentityDigest: DEPLOYMENT,
        },
        nowMs: () => 1_000_000,
        walletOnlySend: {
          open: () => true,
          latestValidToSlot: () =>
            lucid.unixTimeToSlot(s.emulator.now() + WALLET_ONLY_VALIDITY_MS),
        },
      }),
  };
};

const H1 = "a1".repeat(28);
const H2 = "a2".repeat(28);

/** Sends the first walk of the funder's run, unlanded. */
const walking = async (workflow: Awaited<ReturnType<typeof workflowOf>>) => {
  const result = await workflow.run();
  const [walk] = workflow.built("walk:");
  expect(walk).toBeDefined();
  return { result, walk: walk! };
};

describe("workflow in-flight set: a requirement read from an address our transaction may still land at", () => {
  it("does not bind a second run while our walk of the run may still land, and walks on once it lands", async () => {
    const s = await scene();
    const workflow = await workflowOf(s, H1);
    const { result, walk } = await walking(workflow);
    // The walk hides the run, and the empty address reads as needing a bind.
    expect(s.shownAt(s.run.address)).toStrictEqual([]);
    expect(result).toMatchObject({
      kind: "pending",
      reason: expect.stringContaining(
        `bind waits on transaction ${walk.toHash()} of walk:`,
      ),
      awaitingActionId: expect.stringMatching(/^walk:/u),
    });
    for (let run = 0; run < 2; run += 1) {
      s.emulator.awaitSlot(20);
      expect(await workflow.run()).toMatchObject({ kind: "pending" });
    }
    expect(workflow.built("bind")).toStrictEqual([]);

    // Landed, the walk shows the run it made, and the next walk goes.
    await s.land();
    await workflow.run();
    expect(workflow.built("walk:")).toHaveLength(2);
    await s.land();
    expect(await workflow.run()).toMatchObject({
      kind: "pending",
      reason: "walked",
    });
    expect(workflow.built("bind")).toStrictEqual([]);
  });

  it("does not bind a second run while another objective's walk of it may still land", async () => {
    const s = await scene();
    const one = await workflowOf(s, H1);
    const other = await workflowOf(s, H2);
    const { walk } = await walking(one);
    expect(await other.run()).toMatchObject({
      kind: "pending",
      reason: expect.stringContaining(
        `bind waits on transaction ${walk.toHash()} of walk:`,
      ),
    });
    expect(one.built("bind")).toStrictEqual([]);
  });

  it("binds a run once our walk can no longer land and the run is gone", async () => {
    const s = await scene();
    const workflow = await workflowOf(s, H1);
    const { walk } = await walking(workflow);
    // The mempool drops the walk, and another spend takes the run.
    s.mempool.splice(0);
    const [bound] = (await s.emulator.getUtxos(s.run.address)).filter(
      ({ assets }) => assets.lovelace === BOUND,
    );
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

    expect(s.shownAt(s.run.address)).toStrictEqual([]);
    expect(await workflow.run()).toMatchObject({
      kind: "pending",
      reason: "transaction for bind is pending",
    });
    expect(workflow.built("bind")).toHaveLength(1);
    expect(workflow.built("walk:")).toStrictEqual([walk]);
  });
});
