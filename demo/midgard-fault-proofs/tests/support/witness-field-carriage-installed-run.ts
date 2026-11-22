/**
 * The witness field's carriage run through the watcher's installed
 * application: a field wide enough to need publications and a field
 * certificate, the ledger watched, other parties' outputs placed before or
 * beside the application's own, and what its own transactions read and cost.
 * The calling test hoists `hooks` and the two mocks `runInstalled` needs.
 */
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  type CarriageOutput,
  certificateOf,
  fieldCarriageOf,
  type FieldCarriageSites,
  otherParty,
  placeCertificateBefore,
  placeSameDatum,
  placeSameDatumBefore,
  publicationsOf,
} from "./own-field-carriage-installed.js";
import {
  adoptedAndRejected,
  emulatorWallClock,
  kindOf,
  ledgerOutputs,
  ledgerTimeline,
  type LedgerTransaction,
  type LedgerWatch,
  scriptDecodeRoles,
  strictShape,
  takenPlanted,
  watchLedger,
} from "./own-outputs-installed.js";
import {
  INSTALLED_WITNESS,
  type InstalledHooks,
  type InstalledTurnContext,
  runInstalled,
} from "./script-decode-installed-run.js";
import {
  delayChainScript,
  honestScript,
  plutusItem,
  witnessSubject,
} from "./script-decode-plutus-max-shape.js";

/** The undecodable witness the record convicts: one staged chunk, a short walk. */
const ITEM = plutusItem(delayChainScript(600));
/**
 * The padding of a decodable witness beside it, wide enough that the field
 * (both items) needs publications and a field certificate.
 */
const WIDE_PAD_BYTES = 15_000;
/** The scenario whose decodable witness pads `widePadBytes`. */
const scenarioOf = (widePadBytes: number) => ({
  ...INSTALLED_WITNESS,
  nativeTx: () =>
    witnessSubject([ITEM, plutusItem(honestScript(widePadBytes))]).nativeTx,
});
/** Each run convicts well inside this, so one that spins fails fast. */
const TURN_CAP = 20;
/** Blocks every run takes before the record begins. */
const SETUP_BLOCKS = 24;
/**
 * The widest datum a token-bearing copy is placed for: a copy of the widest
 * publication fills its own transaction, with no room to mint a token.
 */
const TOKEN_COPY_DATUM_BYTES = 8_192;

export type Harness = InstalledTurnContext["harness"];

/** What a placing step acts with: the other party is `other`. */
export type PlaceContext = Readonly<{
  harness: Harness;
  watch: LedgerWatch;
  other: LucidEvolution;
}>;

export const sitesOf = (harness: Harness): FieldCarriageSites => ({
  publisherAddress: harness.proverSigner.proofMaterialsAddress,
  stagingDatums: scriptDecodeRoles({
    emulator: harness.emulator,
    materialsAddress: harness.proverSigner.proofMaterialsAddress,
    item: ITEM,
  }).staging.datums,
  certificatePolicyId: harness.contracts.fieldPreimageCertificate.policyId,
});

/** What a run's own transactions were and what they cost. */
const costOf = (watch: LedgerWatch, harness: Harness) => {
  const { staging, run } = scriptDecodeRoles({
    emulator: harness.emulator,
    materialsAddress: harness.proverSigner.proofMaterialsAddress,
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
 * The application's transactions took no output another party placed except
 * an exact copy of carriage (`moved` are coins its own wallet moved): an
 * inline datum, no datum hash, no reference script, and no token but a field
 * certificate's own. The application minted `certificates` certificates, the
 * last of them reads a copy, its own or an adopted one, of every publication
 * the application planned, and a copy of that certificate is read.
 */
export const expectOwnFieldCarriage = (
  watch: LedgerWatch,
  harness: Harness,
  moved: ReadonlySet<string> = new Set(),
  certificates = 1,
) => {
  const sites = sitesOf(harness);
  const certificateToken = (unit: string, quantity: string) =>
    unit.startsWith(sites.certificatePolicyId) && quantity === "1";
  const { rejected } = adoptedAndRejected(takenPlanted(watch, moved), (shape) =>
    strictShape(shape, certificateToken),
  );
  expect(
    rejected,
    JSON.stringify({ rejected, timeline: ledgerTimeline(watch) }),
  ).toEqual([]);
  const carriage = fieldCarriageOf(watch, sites);
  expect(carriage.certificates).toHaveLength(certificates);
  const { transaction: certify, output: certificate } =
    carriage.certificates.at(-1) as {
      transaction: LedgerTransaction;
      output: CarriageOutput;
    };
  const outputs = ledgerOutputs(watch);
  const planned = new Set(
    carriage.publications.flatMap(({ outputs: paid }) =>
      paid.map(({ datum }) => datum),
    ),
  );
  expect(
    new Set(
      certify.referenceInputs.flatMap((outRef) => {
        const datum = outputs.get(outRef)?.datum;
        return datum != null && planned.has(datum) ? [datum] : [];
      }),
    ),
  ).toEqual(planned);
  const certificateCopies = [...outputs]
    .filter(
      ([, output]) =>
        output.address === certificate.address &&
        output.datum === certificate.datum,
    )
    .map(([outRef]) => outRef);
  expect(
    certificateCopies.some((outRef) => carriage.readersOf(outRef).length > 0),
  ).toBe(true);
  return carriage;
};

/**
 * A run of the scenario with `place` before the record begins and `decide`
 * seeing each of the application's transactions, its ledger watched.
 */
export const runWith = async (
  hooks: InstalledHooks,
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
    /** How wide the decodable witness beside the record's item pads; by
     * default wide enough for publications and a field certificate. */
    readonly widePadBytes?: number;
  } = {},
) => {
  let watch: LedgerWatch | undefined;
  let harness: Harness | undefined;
  let other: LucidEvolution | undefined;
  const {
    decide,
    during,
    widePadBytes = WIDE_PAD_BYTES,
    ...runOptions
  } = options;
  const run = await runInstalled(scenarioOf(widePadBytes), hooks, {
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
 * of a kind, other parties' outputs for that carriage. For publications:
 * with `confirmed`, an exact copy of each a block ahead, and a copy carrying
 * a token of each that leaves room for one in a transaction; otherwise an
 * exact copy of each landing in the same block whose transaction id sorts
 * first. For the certificate: another party's own certificate of the same
 * field, minted with the certificate token, whose transaction id sorts
 * first, a block ahead with `confirmed` and otherwise in the same block. A
 * placement that fails is recorded, not swallowed by the application's
 * submission.
 */
export const placeBeside = (
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
    if (kind === "certificate") {
      copies += 1;
      await placeCertificateBefore(watch, other, {
        certify: transaction,
        certificate: outputs[0]!,
        below: transaction.txHash,
      });
      if (confirmed) harness.emulator.awaitBlock();
      return;
    }
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
