/** The validation-trace dispute's case of ours verified by another party, under
 * the prover funding permit, on the emulator. Our funded watcher opens our
 * claim and signs our VerifySource, and its process stops before
 * broadcasting it; another party then runs VerifySource over our pending
 * claim first. The next process supersedes our VerifySource, whose wallet
 * inputs it still holds as a superseded attempt's, and plays the case our
 * Open opened: once the scene's operator defends it, our reveal, timeout and
 * thread close each build and land, spending only the thread (and case)
 * their validators fix, never those wallet inputs.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import {
  CML,
  generateEmulatorAccount,
  Lucid,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import {
  VALIDATION_TRACE_CASE_CONTESTED,
  VALIDATION_TRACE_THREAD_AWAITING,
} from "../src/validation-trace-dispute/threads.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { workflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { drainProver } from "./support/installed-prover-wallet.js";
import { sceneOperator } from "./support/validation-trace-dispute-defend.js";
import {
  confirmedMoves,
  moveIntents,
  recordedOutput,
} from "./support/validation-trace-dispute-funded-moves.js";
import {
  type AdapterWrap,
  type FundedDispute,
  openFundedDispute,
  turnUntil,
} from "./support/validation-trace-dispute-funded-runner.js";
import {
  FUNDED,
  isVerifySource,
  TIMEOUT,
} from "./support/validation-trace-dispute-funded-verify-source.js";
import {
  BOND_LOVELACE,
  failingMaterial,
  honestPresentation,
  type Scene,
  setUpScene,
  verifySource,
} from "./support/validation-trace-dispute-open-scene.js";
import {
  defendScripted,
  liveThread,
  revealAsDefender,
  scriptedTrace,
  threadOf,
  threadUnitOf,
} from "./support/validation-trace-dispute-scripted-defender.js";

// Stands in for the family's install, which waits on its defender.
vi.mock("../src/workflow/installed-categories.js", async (importOriginal) => {
  const actual =
    await importOriginal<
      typeof import("../src/workflow/installed-categories.js")
    >();
  return {
    ...actual,
    isInstalledFraudProofCategory: (
      category: Parameters<typeof actual.isInstalledFraudProofCategory>[0],
    ) =>
      category === "validationTraceDispute" ||
      actual.isInstalledFraudProofCategory(category),
  };
});

const ADA = 1_000_000n;

const outRefOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** The out-refs `signedHex` spends. */
const spentBy = (signedHex: string): readonly string[] => {
  const inputs = CML.Transaction.from_cbor_hex(signedHex).body().inputs();
  return Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  });
};

/**
 * Runs the watcher until it signs our VerifySource and journals it, then
 * stops its process before broadcasting; the stopped turn never returns.
 */
const stopBeforeVerifySourceBroadcast = async (dispute: FundedDispute) => {
  let signedHex: string | undefined;
  let stopped!: () => void;
  const stop = new Promise<void>((resolve) => {
    stopped = resolve;
  });
  const wrap: AdapterWrap = (adapter) => ({
    ...adapter,
    preflight: async (context) => {
      const preflight = await adapter.preflight(context);
      if (isVerifySource(context.action))
        signedHex = workflowPreflightTransaction(preflight)!
          .toTransaction()
          .to_cbor_hex();
      return preflight;
    },
    submit: async (input) => {
      if (!isVerifySource(input.action)) return await adapter.submit(input);
      stopped();
      return await new Promise<never>(() => undefined);
    },
  });
  const turns = turnUntil(dispute, VALIDATION_TRACE_CASE_OPEN, { wrap });
  await Promise.race([
    stop,
    turns.then(({ history }) => {
      throw new Error(`the process ended before submit: ${history.join("\n")}`);
    }),
  ]);
  if (signedHex === undefined) throw new Error("VerifySource was not signed");
  return signedHex;
};

/** A party other than us, holding a bond's worth and fees. */
const anotherParty = async (scene: Scene) => {
  const { h } = scene;
  const account = generateEmulatorAccount({ lovelace: 0n });
  const funding = await h.funderLucid
    .newTx()
    .pay.ToAddress(account.address, { lovelace: BOND_LOVELACE + 500n * ADA })
    .complete();
  await h.funderLucid.awaitTx(
    await (await funding.sign.withWallet().complete()).submit(),
  );
  const lucid = await Lucid(h.emulator, "Custom", {
    slotConfig: h.proverLucid.config().slotConfig,
  });
  lucid.selectWallet.fromSeed(account.seedPhrase);
  return lucid;
};

describe("validation-trace dispute case of ours verified by another party", () => {
  it(
    "plays our Open's case to its thread close after another party verified it first, its superseded VerifySource's wallet inputs never forced into a move",
    async () => {
      const scene = await setUpScene();
      const { h, chain } = scene;
      await drainProver(h as never, FUNDED);
      const dispute = await openFundedDispute(scene);
      try {
        const stoppedHex = await stopBeforeVerifySourceBroadcast(dispute);
        const spent = spentBy(stoppedHex);
        const wallet = new Set(
          (await h.emulator.getUtxos(h.proverSigner.address)).map(outRefOf),
        );
        // Our stopped VerifySource holds wallet inputs besides our claim.
        const held = spent.filter((outRef) => wallet.has(outRef));
        expect(held.length).toBeGreaterThan(0);
        const [opener] = spent.filter((outRef) => !wallet.has(outRef));
        expect(spent.filter((outRef) => !wallet.has(outRef))).toHaveLength(1);

        // Another party verifies our pending claim first.
        const material = await failingMaterial(scene);
        const lucid = await anotherParty(scene);
        const foreign = verifySource(scene, recordedOutput(scene, opener!), {
          ...honestPresentation(material),
          lucid,
        });
        const foreignHash = await (
          await (await foreign.build()).sign.withWallet().complete()
        ).submit();
        await lucid.awaitTx(foreignHash);
        const [caseOutput] = await h.emulator.getUtxosByOutRef([
          parseOwnOutRef(`${foreignHash}#0`),
        ]);
        expect(caseOutput!.address).toBe(chain.case.spendingScriptAddress);

        // The next process finds the case our Open opened.
        await dispute.restart();
        const opened = await turnUntil(dispute, VALIDATION_TRACE_CASE_OPEN, {
          limit: 40,
        });
        expect(opened.result.kind, opened.history.join("\n")).toBe("pending");
        expect(
          confirmedMoves(opened.result).map(({ stage }) => stage),
        ).toStrictEqual(["init", "open"]);

        // The operator defends, reveals, then falls silent.
        const defender = sceneOperator(scene);
        const trace = scriptedTrace(
          material,
          Math.floor(Number(material.stepCount) / 3),
        );
        const threadOutRef = await defendScripted(
          scene,
          caseOutput!,
          defender,
          trace,
        );
        const [thread] = await h.emulator.getUtxosByOutRef([
          parseOwnOutRef(threadOutRef),
        ]);
        const unit = threadUnitOf(scene, thread!);
        await revealAsDefender(scene, defender, thread!, trace);
        const pass = async () => {
          const live = (await liveThread(scene, unit))!;
          const deadline = Number(
            threadOf(live).turn_started_at + threadOf(live).defender_clock_ms,
          );
          const prover = h.proverLucid;
          h.emulator.awaitSlot(
            prover.unixTimeToSlot(deadline) - prover.currentSlot() + 2,
          );
        };
        const revealed = await turnUntil(
          dispute,
          VALIDATION_TRACE_THREAD_AWAITING,
          {
            limit: 40,
          },
        );
        expect(revealed.result.kind, revealed.history.join("\n")).toBe(
          "pending",
        );
        await pass();
        const closed = await turnUntil(
          dispute,
          VALIDATION_TRACE_CASE_CONTESTED,
          { limit: 40 },
        );
        expect(closed.result.kind, closed.history.join("\n")).toBe("pending");

        const stages = confirmedMoves(closed.result);
        expect(stages.map(({ stage }) => stage)).toStrictEqual([
          "init",
          "open",
          "reveal",
          "timeout",
          "close_thread",
        ]);
        // Our stopped VerifySource never landed, and no move spent one of
        // the wallet inputs it held: they are all still ours.
        const stoppedHash = CML.hash_transaction(
          CML.Transaction.from_cbor_hex(stoppedHex).body(),
        ).to_hex();
        expect(
          (await h.emulator.getTransactionStatus(stoppedHash)).status,
        ).not.toBe("confirmed");
        expect(
          (await h.emulator.getUtxosByOutRef(held.map(parseOwnOutRef))).map(
            outRefOf,
          ),
        ).toStrictEqual(held);
        for (const stage of ["reveal", "timeout", "close-thread"] as const) {
          const landed = moveIntents(closed.result, stage).filter(
            ({ txHash }) => scene.l1.signedCbors.has(txHash),
          );
          expect(landed).toHaveLength(1);
        }
      } finally {
        await dispute.close();
      }
    },
    TIMEOUT,
  );
});
