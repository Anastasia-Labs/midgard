/** The prover funding permit walking the runner's one-step moves under their
 * authority, as the runtime bound walks them. Over the moves the runner
 * built and landed to resolve a thread of ours and close it into our case
 * (the entry into the resolution, each hop to the award, and the awarded
 * thread's close), under terms naming the one-step holders, it admits each
 * move's thread as released custody and each move's own outputs; and it
 * refuses: an entry or hop under terms that name no one-step holder, a hop
 * whose thread stands at a holder the terms do not name, a hop that pays
 * its successor to a holder the terms do not name, a hop whose thread names
 * another prover, and a move whose authority names another defender.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import { Constr, Data, type UTxO } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it, vi } from "vitest";

import { validationTraceOneStepContracts } from "../src/validation-trace-dispute/one-step-holders.js";
import { VALIDATION_DISPUTE_MOVE_BINDING } from "../src/workflow/funding-reservation-permit.assert-validation-dispute-move.js";
import { exactResolvedOutputCbor } from "../src/workflow/funding-reservation-permit.create-workflow-funding-reservation-permit.js";
import type { Moves } from "../src/workflow/funding-reservation-permit.validation-dispute-move-authority.js";
import {
  bodyOutputs,
  disputeTerms,
  fundingPolicy,
  movesOf,
  refusalOf,
  regranted,
  walk,
  type WalkJourney,
} from "./support/validation-trace-dispute-funded-move-walk.js";
import {
  type CapturedMove,
  captureMoves,
  ourOpener,
} from "./support/validation-trace-dispute-funded-moves.js";
import { classifyFailingHeader } from "./support/validation-trace-dispute-funded-runner.js";
import {
  defendTwoThreads,
  threadClosed,
} from "./support/validation-trace-dispute-one-step-case.js";
import { playThreads } from "./support/validation-trace-dispute-one-step-play.js";

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

let journey: WalkJourney;
let oneStep: NonNullable<Moves["oneStep"]>;
let enter: CapturedMove;
let hops: CapturedMove[];
let close: CapturedMove;
/** A key that is neither the prover's nor a defender's. */
let stranger: string;

const outRefOf = (utxo: { txHash: string; outputIndex: number }) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const feeOf = (move: CapturedMove) => move.signed.toTransaction().body().fee();

/** The policy whose terms name the one-step holders, with `moves` changed. */
const withOneStep = (moves: Partial<Moves> = {}) =>
  fundingPolicy(journey, disputeTerms(journey, { oneStep, ...moves }));

/** `move` with its thread or authority changed, granted again over its own
 * bytes, as a builder that evaluated them would. */
const changed = async (
  move: CapturedMove,
  {
    thread = move.thread,
    defenderVkh = move.authority.defenderVkh,
  }: { readonly thread?: UTxO; readonly defenderVkh?: string },
) => {
  const edited: CapturedMove = {
    ...move,
    thread,
    authority: { ...move.authority, defenderVkh },
  };
  return {
    move: edited,
    signed: await regranted(journey, edited, () => {}),
  };
};

describe("validation-trace dispute one-step moves walked under their authority", () => {
  beforeAll(async () => {
    const moves: CapturedMove[] = [];
    const dispute = await defendTwoThreads({
      withRemoval: false,
      wrap: (scene) => captureMoves(scene, moves),
    });
    const { scene, runner, a, b, deployment } = dispute;
    const { result } = await playThreads({
      scene,
      runner,
      sides: [a, { ...b, silent: true }],
      until: async () => await threadClosed(dispute, a.unit),
      limit: 60,
    });
    journey = {
      scene,
      dispute: await classifyFailingHeader(scene),
      opener: ourOpener(scene, result),
      moves,
    };
    const holders = validationTraceOneStepContracts(deployment);
    oneStep = {
      boundaryAddress: holders.find(({ holder }) => holder === "boundary")!
        .address,
      resolverAddresses: holders
        .filter(({ holder }) => holder !== "boundary" && holder !== "award")
        .map(({ address }) => address),
    };
    [enter] = movesOf(journey, "enter_resolution");
    hops = movesOf(journey, "resolve");
    [close] = movesOf(journey, "close_awarded");
    stranger = b.defender.vkey === a.defender.vkey ? "" : b.defender.vkey;
  }, 1_800_000);

  it("admits the entry and each hop: its thread as released custody, its successor less the fee", async () => {
    expect(enter).toBeDefined();
    expect(hops).toHaveLength(5);
    for (const move of [enter, ...hops]) {
      const { lineages, custody } = await walk(journey, move, {
        policy: withOneStep(),
      });
      expect(lineages).toStrictEqual([
        {
          sourceActionKind: move.authority.kind,
          sourceOutputIndex: move.thread.outputIndex,
          outRef: outRefOf(move.thread),
          resolvedOutputCborHex: exactResolvedOutputCbor(move.thread),
        },
      ]);
      expect(custody).toStrictEqual([
        {
          custody: (move.thread.assets.lovelace ?? 0n) - feeOf(move),
          allocation: 0n,
        },
      ]);
    }
  });

  it("admits the awarded thread's close: the case back, the receipt, the leftover outside custody, with or without one-step terms", async () => {
    for (const policy of [withOneStep(), fundingPolicy(journey)]) {
      const { lineages, custody } = await walk(journey, close, { policy });
      expect(
        lineages.map((lineage) => (lineage as { outRef: string }).outRef),
      ).toStrictEqual([outRefOf(close.thread), outRefOf(close.caseInput!)]);
      const outputs = bodyOutputs(close.signed);
      expect(custody).toStrictEqual([
        { custody: outputs[0]!.amount().coin(), allocation: 0n },
        { custody: outputs[1]!.amount().coin(), allocation: 0n },
        { custody: 0n, allocation: 0n },
      ]);
    }
  });

  it("refuses the entry and a hop under terms that name no one-step holder", async () => {
    for (const move of [enter, hops[0]!])
      expect(await refusalOf(() => walk(journey, move))).toBe(
        `${VALIDATION_DISPUTE_MOVE_BINDING} (one-step terms)`,
      );
  });

  it("refuses a hop whose thread stands at a holder the terms do not name", async () => {
    const hop = hops[1]!;
    const policy = withOneStep({
      oneStep: {
        ...oneStep,
        resolverAddresses: oneStep.resolverAddresses.filter(
          (address) => address !== hop.thread.address,
        ),
      },
    });
    expect(await refusalOf(() => walk(journey, hop, { policy }))).toBe(
      `${VALIDATION_DISPUTE_MOVE_BINDING} (thread address)`,
    );
  });

  it("refuses a hop that pays its successor to a holder the terms do not name", async () => {
    const hop = hops[1]!;
    const to = bodyOutputs(hop.signed)[0]!.address().to_bech32();
    const policy = withOneStep({
      oneStep: {
        ...oneStep,
        resolverAddresses: oneStep.resolverAddresses.filter(
          (address) => address !== to,
        ),
      },
    });
    expect(await refusalOf(() => walk(journey, hop, { policy }))).toBe(
      "validation dispute resolve must pay exactly successor: paid=",
    );
  });

  it("refuses a hop whose thread names another prover", async () => {
    const hop = hops[1]!;
    const datum = Data.from(hop.thread.datum!) as Constr<Data>;
    const { move, signed } = await changed(hop, {
      thread: {
        ...hop.thread,
        datum: Data.to(
          new Constr(Number(datum.index), [stranger, ...datum.fields.slice(1)]),
        ),
      },
    });
    expect(
      await refusalOf(() =>
        walk(journey, move, { signed, policy: withOneStep() }),
      ),
    ).toBe(`${VALIDATION_DISPUTE_MOVE_BINDING} (thread prover)`);
  });

  it("refuses a move whose authority names another defender, at the boundary by the thread's own defender too", async () => {
    for (const [hop, refused] of [
      [hops[0]!, "thread token, thread defender"],
      [hops[1]!, "thread token"],
    ] as const) {
      const { move, signed } = await changed(hop, { defenderVkh: stranger });
      expect(
        await refusalOf(() =>
          walk(journey, move, { signed, policy: withOneStep() }),
        ),
      ).toBe(`${VALIDATION_DISPUTE_MOVE_BINDING} (${refused})`);
    }
  });
});
