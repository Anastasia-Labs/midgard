/**
 * The block replay's reads and writes of its ledger: the outputs at named
 * out-refs, the out-refs a transaction can resolve, and an event's changes
 * applied as the ledger mutations they amount to. A failure of the ledger's
 * `read` or `apply` is this process's own fault (`ReplayLocalFault`).
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
import type { Ledger, LedgerMutation } from "./ledger-store/index.js";

/** One change to the ledger: the out-ref CBOR hex and its new output, or
 * `null` for a removal. */
export type BlockReplayLedgerChange = {
  readonly key: string;
  readonly output: Buffer | null;
};

/** `run()` on the ledger, with any failure named a local fault of `what`. */
const onLedger = <A>(what: string, run: () => Promise<A>) =>
  Effect.tryPromise({
    try: run,
    catch: (cause) =>
      isReplayLocalFault(cause)
        ? cause
        : new ReplayLocalFault({
            message: `${what} failed locally: ${String(cause)}`,
            cause,
          }),
  }).pipe(Effect.orDie);

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
          : yield* onLedger("ledger apply", () => ledger.apply(mutations)),
      applied: Object.freeze(applied),
    };
  });
