/**
 * What the block replay reads from its parent `Ledger` decides verdicts: a
 * user event reads the output it acts on through the ledger, a transaction's
 * script may arrive only through a reference input, and a forced transaction
 * whose inputs are all held is still rejected for what they hold. Each
 * replay runs over a parent that refuses to list its entries.
 */
import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import { encodeCbor, encodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { encodeMidgardCekCardanoFlatProgram } from "@al-ft/midgard-validation/cek-program";
import { MidgardRedeemerTag } from "@al-ft/midgard-validation/midgard-redeemers";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeNativeTx,
  makeProtectedScriptOutput,
  makeRedeemersCbor,
  outRefFromByte,
  plutusV3ScriptWitness,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import {
  Application,
  Lambda,
  UPLCConst,
  UPLCProgram,
  UPLCVar,
} from "@al-ft/midgard-validation/uplc";
import { Cause, Effect, Exit } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import {
  type BlockReplayContext,
  isReplayLocalFault,
  readLedgerOutput,
  replayBlockEvents,
} from "../src/block-replay.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
} from "../src/ledger-store/index.js";
import {
  blockFaultOf,
  FIXTURE_ADDRESS,
  FIXTURE_REPLAY_SETTINGS,
  fixtureBlock,
  type FixtureEvent,
  fixtureOutput,
  fundedEntry,
  type LedgerEntry,
  producedOutRef,
  recordingLedger,
} from "./support/block-replay-block.js";

beforeAll(() => ledgerEd25519SignatureReady());

const hex = (outRef: Buffer) => outRef.toString("hex");

const funded = Array.from({ length: 8 }, (_, index) =>
  fundedEntry(0x41 + index),
);
const p = (index: number) => funded[index]!.outRef;

/** The ledger of `entries`, admitted at the root the fixture derives. */
const ledgerOf = async (entries: readonly LedgerEntry[]) =>
  createLedgerStore(createMemoryLedgerStorage()).admit(
    entries,
    (await fixtureBlock(entries, [])).roots[0]!,
  );

type Block = Awaited<ReturnType<typeof fixtureBlock>>;

/** Replays `block` over `parent`, recording the out-refs read from it. */
const replayOver = async (
  block: Block,
  parent: readonly LedgerEntry[],
  replayUserEvent: BlockReplayContext["replayUserEvent"] = block.material
    .replayUserEvent,
) => {
  const reads: string[] = [];
  const forcedCodes: string[] = [];
  const exit = await Effect.runPromiseExit(
    replayBlockEvents({
      header: block.payload.block_body.header,
      payload: block.payload,
      parent: recordingLedger(await ledgerOf(parent), reads),
      ...FIXTURE_REPLAY_SETTINGS,
      replayUserEvent,
      verifyForcedSource: ({ rejection }) =>
        Effect.sync(() => void forcedCodes.push(rejection?.code ?? "accepted")),
    }),
  );
  return { exit, reads, forcedCodes };
};

/** No failure or defect of `exit` is this process's own fault. */
const noLocalFault = (exit: Exit.Exit<unknown, unknown>) =>
  Exit.isFailure(exit)
    ? [...Cause.defects(exit.cause), ...Cause.failures(exit.cause)].filter(
        isReplayLocalFault,
      )
    : [];

describe("a user event reading the parent ledger", () => {
  const deposited = outRefFromByte(0xd5);
  const events: readonly FixtureEvent[] = [
    { phase: "Withdrawal", outRef: p(0) },
    { phase: "Deposit", outRef: deposited, output: fixtureOutput(7_000_000n) },
  ];

  /**
   * The fixture's L1 material, which also reads the output its event acts
   * on through `readLedgerOutput`, as the node's and the watcher's
   * withdrawal material do, and records what the ledger held there.
   */
  const reading =
    (
      block: Block,
      seen: [string, string | undefined][],
    ): BlockReplayContext["replayUserEvent"] =>
    ({ source, ledger }) =>
      Effect.gen(function* () {
        const changes = yield* block.material.replayUserEvent({ source });
        for (const { key } of changes) {
          const held = yield* readLedgerOutput(ledger, Buffer.from(key, "hex"));
          seen.push([key, held?.toString("hex")]);
        }
        return changes;
      });

  it("reads the output it acts on from the ledger and nothing the block does not name", async () => {
    const block = await fixtureBlock(funded, events);
    const seen: [string, string | undefined][] = [];
    const { exit, reads } = await replayOver(
      block,
      funded,
      reading(block, seen),
    );
    expect(blockFaultOf(exit)).toBeUndefined();
    if (Exit.isFailure(exit)) return;
    expect(exit.value.root).toBe(block.payload.block_body.header.utxosRoot);
    expect(seen).toEqual([
      [hex(p(0)), fixtureOutput().toString("hex")],
      [hex(deposited), undefined],
    ]);
    expect(reads.filter((outRef) => !block.named.has(outRef))).toEqual([]);
  }, 120_000);

  // The deposit's case carries the deposit alone: in the block above, the
  // withdrawal's step comes first, and over any other parent its pre root
  // already differs from the committed one.
  it.each([
    {
      label: "lacks the withdrawn output",
      carried: events,
      parent: () => funded.slice(1),
      fault: "foreign user event spends missing ledger output",
    },
    {
      label: "holds another output where the deposit lands",
      carried: events.slice(1),
      parent: () => [
        ...funded,
        { outRef: deposited, output: fixtureOutput(1_234_567n) },
      ],
      fault: "foreign user event substitutes ledger output",
    },
  ])(
    "refuses its block over a parent that $label as the block's fault",
    async ({ carried, parent, fault }) => {
      const block = await fixtureBlock(funded, carried);
      const { exit } = await replayOver(block, parent(), reading(block, []));
      expect(blockFaultOf(exit)).toBe(fault);
      expect(noLocalFault(exit)).toEqual([]);
    },
    120_000,
  );
});

describe("a script-locked spend whose script arrives on a reference input", () => {
  // A PlutusV3 program that returns Unit, as a succeeding script must.
  const script = plutusV3ScriptWitness(
    encodeCbor(
      encodeMidgardCekCardanoFlatProgram(
        new UPLCProgram(
          [1, 1, 0],
          new Lambda(
            new Application(new Lambda(UPLCConst.unit), new UPLCVar(0)),
          ),
        ),
      ),
    ),
  );
  const locked: LedgerEntry = {
    outRef: outRefFromByte(0x5c),
    output: makeProtectedScriptOutput(
      hashScriptWitness(script),
      FUNDED_OUTPUT_LOVELACE,
    ),
  };
  const carrier: LedgerEntry = {
    outRef: outRefFromByte(0x5d),
    output: encodeMidgardTxOutput({
      address: FIXTURE_ADDRESS,
      value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
      script_ref: script,
    }),
  };
  const tx = makeNativeTx({
    spendInputs: [locked.outRef],
    referenceInputs: [carrier.outRef],
    outputs: [fixtureOutput()],
    omitVkeyWitness: true,
    scriptLanguages: ["PlutusV3"],
    redeemerTxWitsPreimageCbor: makeRedeemersCbor([
      {
        tag: MidgardRedeemerTag.Spend,
        index: 0n,
        exUnits: [
          BigInt(
            MIDGARD_CONSENSUS_PROFILE.limits
              .minSupportedTransactionExecutionMemoryUnits,
          ),
          1_000_000_000n,
        ],
      },
    ]),
  });
  const spend: FixtureEvent = {
    phase: "L2Transaction",
    spend: [locked.outRef],
    references: [carrier.outRef],
    built: { txId: tx.txId, cbor: tx.txCbor },
  };
  const parent = [...funded, locked, carrier];

  it("accepts it with the committed post root", async () => {
    const block = await fixtureBlock(parent, [spend]);
    expect(block.verdicts).toEqual(["accepted"]);
    const { exit, reads } = await replayOver(block, parent);
    expect(blockFaultOf(exit)).toBeUndefined();
    if (Exit.isFailure(exit)) return;
    expect(exit.value.root).toBe(block.roots.at(-1));
    expect(block.roots.at(-1)).not.toBe(block.roots[0]);
    expect(reads.filter((outRef) => !block.named.has(outRef))).toEqual([]);
  }, 120_000);

  it("refuses the same block over a parent that lacks the reference input as a block with an unresolvable input is refused", async () => {
    const block = await fixtureBlock(parent, [spend]);
    const { exit } = await replayOver(
      block,
      parent.filter((entry) => entry !== carrier),
    );
    expect(blockFaultOf(exit)).toBe(
      "foreign normal transaction was rejected by replay",
    );
    expect(noLocalFault(exit)).toEqual([]);
  }, 120_000);
});

describe("a forced transaction rejected while its inputs are held", () => {
  it("replays its committed rejection over the pre root, reading only what it names", async () => {
    // Its one output pays less than its inputs hold.
    const forced: FixtureEvent = {
      phase: "ForcedTransaction",
      spend: [p(2)],
      references: [p(3)],
      lovelace: FUNDED_OUTPUT_LOVELACE / 2n,
    };
    const block = await fixtureBlock(funded, [forced]);
    expect(block.verdicts).toEqual(["rejected"]);
    const { exit, reads, forcedCodes } = await replayOver(block, funded);
    expect(blockFaultOf(exit)).toBeUndefined();
    if (Exit.isFailure(exit)) return;
    expect(forcedCodes).toEqual(["E_VALUE_NOT_PRESERVED"]);
    expect(exit.value.root).toBe(block.roots[0]);
    expect(reads.length).toBeGreaterThan(0);
    const named = new Set([p(2), p(3), producedOutRef(forced)].map(hex));
    expect(reads.filter((outRef) => !named.has(outRef))).toEqual([]);
  }, 120_000);
});
