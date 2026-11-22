/** The validation-trace dispute runner restarted through two defended
 * threads of our case at a staged builtin step (arm 31), on the emulator
 * (as support/validation-trace-dispute-one-step-staged-drive.ts sets it
 * up): mid-bisection on their turn and on ours, after entering the one-step
 * resolution, with one of the step's runs staged, after the PrepareSelected
 * move, mid-hop and after a thread's close. At each point a fresh process
 * asks first for what the process that was not stopped asks for
 * (support/validation-trace-dispute-restart-probe.ts), and neither Inits a
 * thread or locks a second bond.
 *
 * Mid-hop, with a second copy of each staging output our moves read paid to
 * our staging address with the same datum and value, a fresh process makes
 * the next hop over the staging outputs the committed
 * evidence names, and none of the copies. */
import { midgardCekStagingAddress } from "@al-ft/midgard-sdk/fraud-proof";
import { CML, type UTxO } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_DISPUTE_ACTION } from "../src/validation-trace-dispute/actions.js";
import { VALIDATION_TRACE_STAGING } from "../src/validation-trace-dispute/one-step-staging-types.js";
import { workflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import {
  defendTwoThreads,
  threadClosed,
  type TwoThreadCase,
} from "./support/validation-trace-dispute-one-step-case.js";
import { threadPlace } from "./support/validation-trace-dispute-one-step-play.js";
import {
  lastStagedStep,
  STAGED_BYTES_ARMS,
} from "./support/validation-trace-dispute-one-step-staged-drive.js";
import {
  type AdapterWrap,
  expectSameNextAfterRestart,
  type ProcessOver,
  recordingAsks,
} from "./support/validation-trace-dispute-restart-probe.js";
import { makeRunner } from "./support/validation-trace-dispute-runner.js";
import {
  isTurn,
  liveThread,
  revealAsDefender,
  threadOf,
} from "./support/validation-trace-dispute-scripted-defender.js";
import {
  STAGED_SHA2_SCRIPT,
  stagedScript,
} from "./support/validation-trace-dispute-staged-scripts.js";

const BOUNDARIES = [
  "their turn",
  "our turn",
  "entering resolution",
  "1 of N runs staged",
  "after PrepareSelected",
  "mid-hop",
  "after a thread close",
] as const;
type Boundary = (typeof BOUNDARIES)[number];

type Acted = Readonly<{ schema: unknown; hash: string | undefined }>;

/** Records the schema and transaction of each action the adapter
 * preflights. */
const recordingActs =
  (acted: Acted[]): AdapterWrap =>
  (adapter) => ({
    ...adapter,
    preflight: async (context) => {
      const preflight = await adapter.preflight(context);
      acted.push({
        schema: context.action.input.schemaVersion,
        hash: workflowPreflightTransaction(preflight)?.toHash(),
      });
      return preflight;
    },
  });

const referencesOf = (cbor: string): readonly string[] => {
  const references = CML.Transaction.from_cbor_hex(cbor)
    .body()
    .reference_inputs();
  return Array.from({ length: references?.len() ?? 0 }, (_, index) => {
    const input = references!.get(index);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  });
};

let dispute: TwoThreadCase;
const seen: string[] = [];
const acted: Acted[] = [];
const asked = new Map<Boundary, string>();
/** The staging outputs the hop over copies read, those our moves read
 * before it, and the copies. */
let overCopies:
  | Readonly<{
      reads: readonly string[];
      committed: readonly string[];
      copies: readonly string[];
    }>
  | undefined;

const processOver: ProcessOver = (wrap) =>
  makeRunner(dispute.scene, { deployment: dispute.deployment, wrap });

const restartAt = async (boundary: Boundary) => {
  if (asked.has(boundary)) return;
  asked.set(
    boundary,
    await expectSameNextAfterRestart({
      processOver,
      runner: dispute.runner,
      seen,
      block: dispute.scene.failing,
      opened: true,
    }),
  );
};

const holderOf = async (unit: string) =>
  (await threadPlace(dispute.scene, dispute.deployment, unit))?.holder;

/** Pays a copy of each staging output our moves read to our staging
 * address, then has a fresh process make the next move. */
const hopOverCopies = async () => {
  const { scene, prepared } = dispute;
  const { h } = scene;
  const lucid = h.proverLucid;
  const address = midgardCekStagingAddress(
    lucid.config().network!,
    h.proverSigner.paymentKeyHash,
  );
  const live = new Map<string, UTxO>(
    (await lucid.utxosAt(address)).map(
      (utxo) =>
        [`${utxo.txHash}#${utxo.outputIndex.toString()}`, utxo] as const,
    ),
  );
  const committed = [
    ...new Set(
      prepared
        .filter(({ stage }) => stage === "resolve")
        .flatMap(({ cbor }) => referencesOf(cbor))
        .filter((outRef) => live.has(outRef)),
    ),
  ].sort();
  expect(committed.length).toBeGreaterThan(0);
  h.proverSigner.selectWallet(lucid);
  // One copy a transaction: a staging datum is near a transaction's size.
  const copies: string[] = [];
  for (const outRef of committed) {
    const staging = live.get(outRef)!;
    const signed = await (
      await lucid
        .newTx()
        .pay.ToAddressWithData(
          address,
          { kind: "inline", value: staging.datum! },
          { lovelace: staging.assets.lovelace! },
        )
        .complete()
    ).sign
      .withWallet()
      .complete();
    const hash = await signed.submit();
    await lucid.awaitTx(hash);
    copies.push(`${hash}#0`);
  }
  const restartedAsks: string[] = [];
  const restartedActs: Acted[] = [];
  const restarted = processOver((adapter) =>
    recordingActs(restartedActs)(recordingAsks(restartedAsks)(adapter)),
  );
  for (let run = 0; run < 4; run += 1) {
    await restarted.runOnce(scene.failing);
    const hop = restartedActs.find(
      ({ schema, hash }) =>
        schema === VALIDATION_TRACE_DISPUTE_ACTION && hash !== undefined,
    );
    if (hop === undefined) continue;
    const reads = referencesOf(scene.l1.signedCbors.get(hop.hash!)!).filter(
      (outRef) => live.has(outRef) || copies.includes(outRef),
    );
    overCopies = {
      reads: [...reads].sort(),
      committed,
      copies,
    };
    return;
  }
  throw new Error(
    `the restarted process made no move: ${restartedAsks.join("\n")}`,
  );
};

describe("validation-trace dispute runner restarted through two defended threads at a staged step", () => {
  beforeAll(async () => {
    dispute = await defendTwoThreads({
      withRemoval: false,
      builtins: true,
      failingScript: stagedScript(STAGED_SHA2_SCRIPT),
      operatorFrom: lastStagedStep,
      successorFrom: lastStagedStep,
      wrap: () => (adapter) =>
        recordingActs(acted)(recordingAsks(seen)(adapter)),
    });
  }, 1_800_000);

  it("asks first, after a restart, for what the process that was not stopped asks for, at each point through the bisection, the staged resolution and a thread's close", async () => {
    const { scene, runner, a, b, prepared } = dispute;
    const game = scene.chain.game.spendingScriptAddress;
    const places: string[] = [];
    /** Where each thread stands. */
    const holders = async () => [
      String(await holderOf(a.unit)),
      String(await holderOf(b.unit)),
    ];
    /** Whether a thread stands at `holder`. */
    const either = async (holder: string) => (await holders()).includes(holder);
    const bisecting = () => prepared.some(({ stage }) => stage === "reveal");
    for (let run = 0; run < 200; run += 1) {
      const [closedA, closedB] = [
        await threadClosed(dispute, a.unit),
        await threadClosed(dispute, b.unit),
      ];
      if (closedA || closedB) await restartAt("after a thread close");
      if (closedA && closedB) break;
      let revealed = false;
      for (const side of [a, b]) {
        const awaiting = async () => {
          const utxo = await liveThread(scene, side.unit);
          return utxo !== undefined &&
            utxo.address === game &&
            isTurn(threadOf(utxo), "AwaitingOperator")
            ? utxo
            : undefined;
        };
        if ((await awaiting()) === undefined) continue;
        if (bisecting()) await restartAt("their turn");
        const thread = await awaiting();
        if (thread === undefined) continue;
        await revealAsDefender(scene, side.defender, thread, side.trace);
        revealed = true;
      }
      if (revealed && bisecting()) await restartAt("our turn");
      await runner.runOnce(scene.failing);
      places.push((await holders()).join("/"));
      if (prepared.some(({ stage }) => stage === "enter_resolution"))
        await restartAt("entering resolution");
      // Each restart runs the process that was not stopped once more, so
      // where the thread stands is read again before each point.
      const staged = () =>
        acted.filter(
          ({ schema, hash }) => schema === VALIDATION_TRACE_STAGING && hash,
        ).length;
      if (staged() === 1 && (await either("prepare")))
        await restartAt("1 of N runs staged");
      if (await either("core")) await restartAt("after PrepareSelected");
      if (await either(STAGED_BYTES_ARMS[0])) await restartAt("mid-hop");
      if ((await either(STAGED_BYTES_ARMS[1])) && overCopies === undefined)
        await hopOverCopies();
    }
    expect([...asked.keys()].sort(), places.join(" ")).toStrictEqual(
      [...BOUNDARIES].sort(),
    );
    // The staging took more than one run, so one of them staged is a point
    // of its own.
    expect(
      acted.filter(
        ({ schema, hash }) => schema === VALIDATION_TRACE_STAGING && hash,
      ).length,
    ).toBeGreaterThan(1);
    expect(prepared.filter(({ stage }) => stage === "init")).toHaveLength(1);
    expect(
      prepared.filter(({ stage }) => stage === "verify_source"),
    ).toHaveLength(1);
  }, 1_800_000);

  it("makes the hop over the staging outputs the committed evidence names, from a fresh process, though a copy of each stands at our staging address with the same datum and value", () => {
    expect(overCopies).toBeDefined();
    const { reads, committed, copies } = overCopies!;
    expect(reads).toStrictEqual(committed);
    expect(reads.filter((outRef) => copies.includes(outRef))).toStrictEqual([]);
  });
});
