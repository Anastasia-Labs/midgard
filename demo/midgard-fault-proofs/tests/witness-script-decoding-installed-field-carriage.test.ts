/**
 * The witness field's carriage through the watcher's installed application,
 * with other parties' outputs holding the same datums at the addresses its
 * own carriage goes to.
 *
 * The field is wide enough to need publications and a field certificate.
 * The application reads each publication and its certificate by the
 * out-refs its own signed transactions gave them, never by what an output
 * at the address holds: unrelated outputs with the same datums, confirmed
 * before its own or landing beside them and sorting first, outputs with the
 * same datums that carry a token, and a token-bearing coin at the prover
 * address leave its transactions and their fees as they are. A stopped
 * process sends the carriage transaction it signed, and a publication
 * transaction that can never land is published again, never served by a
 * copy.
 */
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

const hooks = vi.hoisted(() => ({
  binding: undefined as unknown,
  authority: undefined as unknown,
  recovery: undefined as unknown,
}));
vi.mock("../src/workflow/deployment-manifest-binding.js", async (load) => ({
  ...(await load<
    typeof import("../src/workflow/deployment-manifest-binding.js")
  >()),
  bindFraudProofWorkflowDeployment: async () => hooks.binding,
}));
vi.mock("../src/workflow/family-l1-observation.js", async (load) => {
  const actual =
    await load<typeof import("../src/workflow/family-l1-observation.js")>();
  const { withFraudProofL1Recovery } = await import(
    "../src/workflow/l1-source.js"
  );
  return {
    ...actual,
    createFraudProofFamilyL1ObservationPort: (
      input: Parameters<
        typeof actual.createFraudProofFamilyL1ObservationPort
      >[0],
    ) =>
      withFraudProofL1Recovery(
        actual.createFraudProofFamilyRawL1ObservationPort({
          ...input,
          authority: hooks.authority as Parameters<
            typeof actual.createFraudProofFamilyRawL1ObservationPort
          >[0]["authority"],
        }),
        hooks.recovery as Parameters<typeof withFraudProofL1Recovery>[1],
      ),
  };
});

import {
  type CarriageOutput,
  certificateOf,
  fieldCarriageOf,
  type FieldCarriageSites,
  otherParty,
  placeSameDatum,
  placeSameDatumBefore,
  publicationsOf,
} from "./support/own-field-carriage-installed.js";
import {
  emulatorWallClock,
  kindOf,
  type LedgerTransaction,
  type LedgerWatch,
  plantTokenCoin,
  removeOutput,
  scriptDecodeRoles,
  watchLedger,
} from "./support/own-outputs-installed.js";
import {
  expectBaselineCost,
  INSTALLED_WITNESS,
  type InstalledTurnContext,
  runInstalled,
  stopsOnceAndRestarts,
} from "./support/script-decode-installed-run.js";
import {
  delayChainScript,
  honestScript,
  plutusItem,
  witnessSubject,
} from "./support/script-decode-plutus-max-shape.js";

/** The undecodable witness the record convicts: one staged chunk, a short walk. */
const ITEM = plutusItem(delayChainScript(600));
/**
 * A decodable witness beside it, wide enough that the field (both items)
 * needs publications and a field certificate.
 */
const WIDE_ITEM = plutusItem(honestScript(15_000));
const SCENARIO = {
  ...INSTALLED_WITNESS,
  nativeTx: () => witnessSubject([ITEM, WIDE_ITEM]).nativeTx,
};
/** Each run convicts well inside these, so one that spins fails fast. */
const POLL_CAP = 200;
const TURN_CAP = 20;
/** Blocks every run takes before the record begins. */
const SETUP_BLOCKS = 24;
/**
 * The widest datum a token-bearing copy is placed for: a copy of the widest
 * publication fills its own transaction, with no room to mint a token.
 */
const TOKEN_COPY_DATUM_BYTES = 8_192;

type Harness = InstalledTurnContext["harness"];

/** What a placing step acts with: the other party is `other`. */
type PlaceContext = Readonly<{
  harness: Harness;
  watch: LedgerWatch;
  other: LucidEvolution;
}>;

const sitesOf = (harness: Harness): FieldCarriageSites => ({
  publisherAddress: harness.proverSigner.address,
  stagingDatums: scriptDecodeRoles({
    emulator: harness.emulator,
    proverAddress: harness.proverSigner.address,
    item: ITEM,
  }).staging.datums,
  certificatePolicyId: harness.contracts.fieldPreimageCertificate.policyId,
});

/** What a run's own transactions were and what they cost. */
const costOf = (watch: LedgerWatch, harness: Harness) => {
  const { staging, run } = scriptDecodeRoles({
    emulator: harness.emulator,
    proverAddress: harness.proverSigner.address,
    item: ITEM,
  });
  const sites = sitesOf(harness);
  return watch.ours().map((transaction) => ({
    kind:
      certificateOf(transaction, sites) !== undefined
        ? "certify"
        : publicationsOf(transaction, sites).length > 0
          ? "publish"
          : kindOf(transaction, staging, run),
    fee: transaction.fee,
  }));
};

/**
 * Every input and reference input of the application's own transactions is
 * not another party's (`moved` are coins its own wallet moved), the certificate reads one of the application's own
 * publications for every planned one, and the certificate it minted is read.
 */
const expectOwnFieldCarriage = (
  watch: LedgerWatch,
  harness: Harness,
  moved: ReadonlySet<string> = new Set(),
) => {
  const others = watch.plantedOutRefs();
  const taken = watch
    .ours()
    .flatMap(({ inputs, referenceInputs }) => [...inputs, ...referenceInputs])
    .filter((outRef) => others.has(outRef) && !moved.has(outRef));
  expect(taken).toEqual([]);
  const carriage = fieldCarriageOf(watch, sitesOf(harness));
  expect(carriage.certificates).toHaveLength(1);
  const [{ transaction: certify, output: certificate }] =
    carriage.certificates as [
      { transaction: LedgerTransaction; output: CarriageOutput },
    ];
  const ownPublications = new Map(
    carriage.publications.flatMap(({ outputs }) =>
      outputs.map(({ outRef, datum }) => [outRef, datum] as const),
    ),
  );
  expect(
    new Set(
      certify.referenceInputs.flatMap((outRef) => {
        const datum = ownPublications.get(outRef);
        return datum === undefined ? [] : [datum];
      }),
    ),
  ).toEqual(new Set(ownPublications.values()));
  expect(carriage.readersOf(certificate.outRef).length).toBeGreaterThan(0);
  return carriage;
};

/**
 * A run of the scenario with `place` before the record begins and `decide`
 * seeing each of the application's transactions, its ledger watched.
 */
const runWith = async (
  place: (
    harness: Harness,
    watch: LedgerWatch,
  ) => readonly (() => Promise<unknown>)[],
  options: Omit<Parameters<typeof runInstalled>[2] & object, "beforeTurns"> & {
    readonly decide?: (
      transaction: LedgerTransaction,
      context: PlaceContext,
    ) => Promise<"accept" | "withhold"> | "accept" | "withhold";
    readonly during?: (
      context: Omit<InstalledTurnContext, "result" | "turn">,
      place: PlaceContext,
    ) => (() => void) | void;
  } = {},
) => {
  let watch: LedgerWatch | undefined;
  let harness: Harness | undefined;
  let other: LucidEvolution | undefined;
  const { decide, during, ...runOptions } = options;
  const run = await runInstalled(SCENARIO, hooks, {
    maxPolls: POLL_CAP,
    maxTurns: TURN_CAP,
    ...runOptions,
    beforeTurns: async (context) => {
      harness = context.harness;
      const clock = emulatorWallClock();
      watch = watchLedger(context.harness.emulator, (transaction) =>
        decide === undefined
          ? "accept"
          : decide(transaction, {
              harness: harness!,
              watch: watch!,
              other: other!,
            }),
      );
      const from = context.harness.emulator.blockHeight;
      other = await otherParty(watch, {
        emulator: context.harness.emulator,
        funder: context.harness.funderLucid,
      });
      context.harness.emulator.awaitBlock();
      for (const step of place(context.harness, watch)) {
        await step();
        context.harness.emulator.awaitBlock();
      }
      context.harness.emulator.awaitBlock(
        SETUP_BLOCKS - (context.harness.emulator.blockHeight - from),
      );
      const stop = during?.(context, {
        harness: context.harness,
        watch,
        other: other!,
      });
      const ledger = watch;
      return () => {
        stop?.();
        ledger.restore();
        clock.restore();
      };
    },
  });
  return {
    run,
    watch: watch!,
    harness: harness!,
    cost: costOf(watch!, harness!),
  };
};

/**
 * Places, before each of the application's transactions that pays carriage
 * of a kind, other parties' outputs with that carriage's datums.
 * `confirmed` lands them a block ahead: an exact copy of each, and a copy
 * carrying a token of each that leaves room for one in a transaction.
 * Otherwise each is an exact copy landing in the same block whose
 * transaction id sorts first. A placement that fails is recorded, not swallowed
 * by the application's submission.
 */
const placeBeside = (
  kind: "publication" | "certificate",
  { confirmed }: { confirmed: boolean },
) => {
  const seen = new Set<string>();
  const failures: unknown[] = [];
  let copies = 0;
  let withToken = 0;
  const place = async (
    transaction: LedgerTransaction,
    { harness, watch, other }: PlaceContext,
  ) => {
    const sites = sitesOf(harness);
    const outputs =
      kind === "publication"
        ? publicationsOf(transaction, sites)
        : [certificateOf(transaction, sites)].filter(
            (output): output is CarriageOutput => output !== undefined,
          );
    if (outputs.length === 0 || seen.has(transaction.txHash)) return;
    seen.add(transaction.txHash);
    for (const { address, datum } of outputs) {
      copies += 1;
      if (!confirmed) {
        await placeSameDatumBefore(watch, other, {
          address,
          datum,
          below: transaction.txHash,
        });
        continue;
      }
      const token = datum.length / 2 <= TOKEN_COPY_DATUM_BYTES;
      if (token) withToken += 1;
      await placeSameDatum(watch, other, { address, datum, token });
    }
    if (confirmed) harness.emulator.awaitBlock();
  };
  return {
    copies: () => copies,
    withToken: () => withToken,
    failures: () => failures,
    decide: async (
      transaction: LedgerTransaction,
      context: PlaceContext,
    ): Promise<"accept"> => {
      await place(transaction, context).catch((cause: unknown) => {
        failures.push(cause);
      });
      return "accept";
    },
  };
};

describe("witness field carriage found by the out-refs it published, in the installed watcher application", () => {
  let baseline: Awaited<ReturnType<typeof runWith>>["cost"];

  it("publishes and certifies the field with nothing else at its addresses: the cost every other row matches", async () => {
    const { run, watch, harness, cost } = await runWith(() => []);
    expect(run.convicted).toBe(true);
    const carriage = expectOwnFieldCarriage(watch, harness);
    expect(
      carriage.publications.flatMap(({ outputs }) => outputs).length,
    ).toBeGreaterThan(1);
    baseline = cost;
  }, 900_000);

  it.each([
    { kind: "publication", confirmed: true, when: "confirmed before them" },
    {
      kind: "publication",
      confirmed: false,
      when: "sorting first in their block",
    },
    { kind: "certificate", confirmed: true, when: "confirmed before it" },
    {
      kind: "certificate",
      confirmed: false,
      when: "sorting first in its block",
    },
  ] as const)(
    "reads its own $kind past same-datum outputs $when",
    async ({ kind, confirmed }) => {
      const beside = placeBeside(kind, { confirmed });
      const { run, watch, harness, cost } = await runWith(() => [], {
        decide: beside.decide,
      });
      expect(beside.failures()).toEqual([]);
      expect(beside.copies()).toBeGreaterThan(0);
      if (confirmed) expect(beside.withToken()).toBeGreaterThan(0);
      expect(run.convicted).toBe(true);
      expect(watch.plantedOutRefs().size).toBeGreaterThan(0);
      expectOwnFieldCarriage(watch, harness);
      expectBaselineCost(cost, baseline);
    },
    900_000,
  );

  it("funds its carriage from plain coins, leaving a larger token-bearing coin at the prover address unspent", async () => {
    const { run, watch, harness, cost } = await runWith((harness, watch) => [
      () =>
        plantTokenCoin(
          watch,
          harness.funderLucid,
          harness.proverSigner.address,
        ),
    ]);
    expect(run.convicted).toBe(true);
    expect(watch.plantedOutRefs().size).toBeGreaterThan(0);
    expectOwnFieldCarriage(watch, harness);
    expectBaselineCost(cost, baseline);
  }, 900_000);

  it.each(["publication", "certificate"] as const)(
    "sends the %s transaction its stopped process signed but never sent, and reads the outputs it names",
    async (kind) => {
      let withheld: string | undefined;
      const stopping = stopsOnceAndRestarts(POLL_CAP);
      const { run, watch, harness, cost } = await runWith(() => [], {
        decide: (transaction, { harness }) => {
          const sites = sitesOf(harness);
          const pays =
            kind === "publication"
              ? publicationsOf(transaction, sites).length > 0
              : certificateOf(transaction, sites) !== undefined;
          if (withheld !== undefined || !pays) return "accept";
          // Signed and journaled, lost before the ledger saw it; the
          // process stops at its next poll.
          withheld = transaction.txHash;
          stopping.stopAtNextPoll();
          return "withhold";
        },
        during: stopping.during,
        onStopped: stopping.onStopped,
      });
      expect(stopping.stops()).toBe(1);
      expect(run.convicted).toBe(true);
      const carriage = expectOwnFieldCarriage(watch, harness);
      const sent =
        kind === "publication"
          ? carriage.publications.map(({ transaction }) => transaction.txHash)
          : carriage.certificates.map(({ transaction }) => transaction.txHash);
      expect(sent[0]).toBe(withheld);
      expectBaselineCost(cost, baseline);
    },
    900_000,
  );

  it("publishes again when its publication transaction is lost and another transaction of its wallet spends one of its coins, and never reads a copy for it", async () => {
    let lost: LedgerTransaction | undefined;
    let spent = false;
    const context: { place?: PlaceContext } = {};
    const moved = new Set<string>();
    const { run, watch, harness } = await runWith(() => [], {
      decide: (transaction, { harness }) => {
        if (lost !== undefined)
          return transaction.txHash === lost.txHash ? "withhold" : "accept";
        if (publicationsOf(transaction, sitesOf(harness)).length === 0)
          return "accept";
        // Signed and journaled, and never seen by the ledger, however often
        // it is sent.
        lost = transaction;
        return "withhold";
      },
      during: (_, place) => {
        context.place = place;
      },
      // Between turns: the lost transaction is sent again every turn, so
      // the coin goes elsewhere once it has been sent at least once.
      afterTurn: async () => {
        if (lost === undefined || spent) return;
        const { harness, watch, other } = context.place!;
        const [txHash, index] = lost.inputs[0]!.split("#");
        const utxo = harness.emulator.ledger[`${txHash!}${index!}`]?.utxo;
        if (utxo === undefined) return;
        spent = true;
        // Another transaction of the prover's wallet spends a coin the lost
        // transaction spends, so it can never land, and exact copies of what
        // it would have published are on chain instead.
        await removeOutput(
          watch,
          harness.proverLucid,
          utxo,
          harness.proverSigner.address,
        );
        moved.add(`${watch.accepted.at(-1)!.txHash}#0`);
        harness.emulator.awaitBlock();
        for (const output of publicationsOf(lost, sitesOf(harness)))
          await placeSameDatum(watch, other, { ...output, token: false });
        harness.emulator.awaitBlock();
      },
    });
    expect(spent).toBe(true);
    expect(run.convicted).toBe(true);
    expect(watch.ours().map(({ txHash }) => txHash)).not.toContain(
      lost!.txHash,
    );
    const carriage = expectOwnFieldCarriage(watch, harness, moved);
    const lostDatums = publicationsOf(lost!, sitesOf(harness)).map(
      ({ datum }) => datum,
    );
    const republished = carriage.publications
      .flatMap(({ outputs }) => outputs)
      .filter(({ datum }) => lostDatums.includes(datum));
    expect(republished.map(({ datum }) => datum)).toEqual(lostDatums);
    expect(watch.plantedOutRefs().size).toBeGreaterThan(0);
  }, 900_000);
});
