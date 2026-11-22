/**
 * The block replay's reads and writes of its ledger: the outputs at named
 * out-refs, the out-refs a transaction can resolve, and an event's changes
 * applied as the ledger mutations they amount to. A failure of the ledger's
 * `read` or `apply` is this process's own fault (`ReplayLocalFault`), except
 * `apply` refusing an inserted output with no ledger leaf, a typed failure.
 */
import { decodeMidgardForcedTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec/forced";
import {
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec/native";
import { decodeMidgardOutRefBytes } from "@al-ft/midgard-validation/ledger-tx/codec";
import { midgardOutRefToCbor } from "@al-ft/midgard-validation/validation-candidate";
import { Effect } from "effect";

import {
  blockFault,
  isReplayLocalFault,
  ReplayLocalFault,
} from "./block-replay.faults.js";
import type {
  Ledger,
  LedgerEntryNotCanonical,
  LedgerMutation,
} from "./ledger-store/index.js";

/** One change to the ledger: the out-ref CBOR hex and its new output, or
 * `null` for a removal. */
export type BlockReplayLedgerChange = {
  readonly key: string;
  readonly output: Buffer | null;
};

const localFault = (what: string, cause: unknown) =>
  isReplayLocalFault(cause)
    ? cause
    : new ReplayLocalFault({
        message: `${what} failed locally: ${String(cause)}`,
        cause,
      });

/** `run()` on the ledger, with any failure named a local fault of `what`. */
const onLedger = <A>(what: string, run: () => Promise<A>) =>
  Effect.tryPromise({
    try: run,
    catch: (cause) => localFault(what, cause),
  }).pipe(Effect.orDie);

/** Whether `error` is the store's `LedgerEntryNotCanonical`, matched by
 * name: the package ships more than one entry bundle. */
const isEntryNotCanonical = (
  error: unknown,
): error is LedgerEntryNotCanonical =>
  error instanceof Error && error.name === "LedgerEntryNotCanonical";

/**
 * `ledger` after `mutations`. An inserted output with no ledger leaf is a
 * typed failure (`LedgerEntryNotCanonical`), never a local fault: the block's
 * events supplied it. Any other failure is a local fault.
 */
const applyMutations = (
  ledger: Ledger,
  mutations: readonly LedgerMutation[],
): Effect.Effect<Ledger, LedgerEntryNotCanonical> =>
  Effect.tryPromise({
    try: () => ledger.apply(mutations),
    catch: (cause) => cause,
  }).pipe(
    Effect.catchAll((cause) =>
      isEntryNotCanonical(cause)
        ? Effect.fail(cause)
        : Effect.die(localFault("ledger apply", cause)),
    ),
  );

/** The outputs `ledger` holds at `outRefs`, keyed by out-ref hex; an
 * out-ref it does not hold maps to `undefined`. */
export const readOutputs = (ledger: Ledger, outRefs: readonly Buffer[]) =>
  outRefs.length === 0
    ? Effect.succeed(new Map<string, Buffer | undefined>())
    : onLedger("ledger read", () => ledger.read(outRefs)).pipe(
        Effect.map(
          (read) =>
            new Map(
              outRefs.map((outRef) => {
                const key = outRef.toString("hex");
                return [key, read.get(key)?.output] as const;
              }),
            ),
        ),
      );

/**
 * The output `ledger` holds at `outRef`, or `undefined` when it holds none.
 * A failure to read it is this process's own fault (`ReplayLocalFault`).
 */
export const readLedgerOutput = (
  ledger: Ledger,
  outRef: Buffer,
): Effect.Effect<Buffer | undefined> =>
  readOutputs(ledger, [outRef]).pipe(
    Effect.map((outputs) => outputs.get(outRef.toString("hex"))),
  );

/**
 * The out-refs a transaction can resolve: its spend inputs, then its
 * reference inputs (Midgard transactions have no collateral), decoded as
 * Phase A decodes them. Bytes that do not decode that far name none; Phase
 * A refuses them before any input is resolved.
 */
export const resolvableOutRefs = (
  cbor: Buffer,
  sourceKind: "normal" | "forced",
): Buffer[] => {
  try {
    const { body } = (
      sourceKind === "forced"
        ? decodeMidgardForcedTxFullFromCanonicalCbor
        : decodeMidgardNativeTxFullFromCanonicalCbor
    )(cbor);
    return [body.spendInputsPreimageCbor, body.referenceInputsPreimageCbor]
      .flatMap((preimage) => decodeMidgardNativeByteListPreimage(preimage))
      .map((item) => midgardOutRefToCbor(decodeMidgardOutRefBytes(item)));
  } catch {
    return [];
  }
};

/**
 * `ledger` with `changes` applied in order, and the out-refs whose output
 * that changed (`null`: removed), each once. `known` holds outputs already
 * read from `ledger`. A transaction's state patch (`strict` false) removes an
 * absent out-ref and overwrites a present one without complaint; a user
 * event (`strict`) may do neither, which is the block's fault.
 */
export const applyChanges = (
  ledger: Ledger,
  changes: readonly BlockReplayLedgerChange[],
  known: ReadonlyMap<string, Buffer | undefined>,
  strict: boolean,
) =>
  Effect.gen(function* () {
    const touched = [...new Set(changes.map((change) => change.key))];
    const unread = touched.filter((outRef) => !known.has(outRef));
    const before = new Map([
      ...known,
      ...(yield* readOutputs(
        ledger,
        unread.map((outRef) => Buffer.from(outRef, "hex")),
      )),
    ]);
    const after = new Map(before);
    for (const { key: outRef, output } of changes) {
      if (output === null) {
        if (strict && after.get(outRef) === undefined)
          throw blockFault("foreign user event spends missing ledger output");
        after.set(outRef, undefined);
      } else {
        if (strict && after.get(outRef) !== undefined)
          throw blockFault("foreign user event substitutes ledger output");
        after.set(outRef, output);
      }
    }
    const mutations: LedgerMutation[] = [];
    const applied: BlockReplayLedgerChange[] = [];
    for (const outRef of touched) {
      const was = before.get(outRef);
      const now = after.get(outRef);
      if (now === undefined ? was === undefined : was?.equals(now) === true)
        continue;
      const key = Buffer.from(outRef, "hex");
      if (was !== undefined) mutations.push({ type: "delete", outRef: key });
      if (now !== undefined)
        mutations.push({ type: "insert", outRef: key, output: now });
      applied.push({
        key: outRef,
        output: now === undefined ? null : Buffer.from(now),
      });
    }
    return {
      ledger:
        mutations.length === 0
          ? ledger
          : yield* applyMutations(ledger, mutations),
      applied: Object.freeze(applied),
    };
  });
