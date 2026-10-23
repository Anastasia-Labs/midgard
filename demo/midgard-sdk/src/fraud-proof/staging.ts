import {
  groupMidgardCekStagedRuns,
  MIDGARD_CEK_STAGED_CHUNK_BYTES,
  MIDGARD_CEK_STAGED_RUN_MAX_CHUNKS,
  type MidgardCekStagedOutRef,
  splitMidgardCekStagedChunks,
} from "@al-ft/midgard-core";
import {
  Constr,
  Data,
  type LucidEvolution,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  resolveProtocolParameters,
  stabilizedMinimumLovelace,
} from "./field-preimage-carriage.build-unsigned-field-preimage-publication-program.js";

/**
 * Staging of the operands of a staged builtin step (arm 31, semantics groups
 * 7 and 8).
 *
 * A staged operand is split into 4,095-byte chunks, and each run of up to
 * three chunks is staged as the inline datum `Constr 0 [List [B c1, ...]]` of
 * one output at the stager's own wallet address, one output a transaction.
 * The step names those outputs by output reference; their owner and address
 * do not matter, only their content, so equal runs share one output.
 *
 * After the step's binder, the staging outputs it names must stay unspent
 * until the thread settles: the binder fixed their output references, so
 * spending one early leaves that thread's staged semantics transaction
 * unbuildable for good. {@link MidgardCekStagingPins} records which thread
 * pins which output, and the reclaim builder refuses a pinned output.
 */

const hex = (bytes: Uint8Array): string => Buffer.from(bytes).toString("hex");

/** The inline datum of a staging output holding `chunks`. */
export const midgardCekStagingRunDatumCbor = (
  chunks: readonly Uint8Array[],
): string => {
  if (
    chunks.length < 1 ||
    chunks.length > MIDGARD_CEK_STAGED_RUN_MAX_CHUNKS ||
    chunks.some((chunk) => chunk.length > MIDGARD_CEK_STAGED_CHUNK_BYTES)
  ) {
    throw new Error("a staging run is 1 to 3 chunks of at most 4,095 bytes");
  }
  return Data.to(new Constr(0, [chunks.map(hex)]));
};

/**
 * The chunks of a staging output's inline datum, or `null` when the datum is
 * not a staging run.
 */
export const midgardCekStagingRunChunks = (
  datumCbor: string,
): Buffer[] | null => {
  let datum: unknown;
  try {
    datum = Data.from(datumCbor);
  } catch {
    return null;
  }
  if (
    !(datum instanceof Constr) ||
    datum.index !== 0 ||
    datum.fields.length !== 1
  ) {
    return null;
  }
  const items = datum.fields[0];
  if (
    !Array.isArray(items) ||
    items.length < 1 ||
    items.length > MIDGARD_CEK_STAGED_RUN_MAX_CHUNKS ||
    items.some((item) => typeof item !== "string")
  ) {
    return null;
  }
  return (items as string[]).map((item) => Buffer.from(item, "hex"));
};

/** One staging output to create, and the operand runs it stages. */
export type MidgardCekStagingRun = {
  readonly chunks: readonly Buffer[];
  readonly datumCbor: string;
  /** Each staged operand's run this output holds, by operand and run index. */
  readonly uses: readonly {
    readonly operand: number;
    readonly run: number;
  }[];
};

/** The staging outputs `stagedOperands` need: one per distinct run. */
export const planMidgardCekStaging = (
  stagedOperands: readonly Uint8Array[],
): readonly MidgardCekStagingRun[] => {
  const byDatum = new Map<
    string,
    {
      readonly chunks: Buffer[];
      readonly uses: { operand: number; run: number }[];
    }
  >();
  stagedOperands.forEach((bytes, operand) => {
    groupMidgardCekStagedRuns(splitMidgardCekStagedChunks(bytes)).forEach(
      (chunks, run) => {
        const datumCbor = midgardCekStagingRunDatumCbor(chunks);
        const known = byDatum.get(datumCbor);
        if (known === undefined) {
          byDatum.set(datumCbor, { chunks, uses: [{ operand, run }] });
        } else {
          known.uses.push({ operand, run });
        }
      },
    );
  });
  return [...byDatum].map(([datumCbor, { chunks, uses }]) =>
    Object.freeze({ chunks, datumCbor, uses }),
  );
};

/**
 * Where each staged operand was staged: the output reference of each of its
 * runs, in run order, from one output reference per planned run. This is the
 * staging result the one-step driver fills a staged step's material from.
 */
export const midgardCekStagingResult = (
  stagedOperands: readonly Uint8Array[],
  plan: readonly MidgardCekStagingRun[],
  outRefs: readonly MidgardCekStagedOutRef[],
): MidgardCekStagedOutRef[][] => {
  if (outRefs.length !== plan.length) {
    throw new Error("staging needs one output reference per planned run");
  }
  const result: (MidgardCekStagedOutRef | undefined)[][] = stagedOperands.map(
    (bytes) =>
      groupMidgardCekStagedRuns(splitMidgardCekStagedChunks(bytes)).map(
        () => undefined,
      ),
  );
  plan.forEach((run, index) => {
    for (const { operand, run: at } of run.uses) {
      const runs = result[operand];
      if (runs === undefined || at >= runs.length) {
        throw new Error("staging plan does not match the staged operands");
      }
      runs[at] = outRefs[index];
    }
  });
  return result.map((runs) =>
    runs.map((ref) => {
      if (ref === undefined) {
        throw new Error("staging plan leaves a run unstaged");
      }
      return ref;
    }),
  );
};

const plainAda = (utxo: UTxO): boolean =>
  utxo.datum == null &&
  utxo.datumHash == null &&
  utxo.scriptRef == null &&
  Object.keys(utxo.assets).every((unit) => unit === "lovelace");

/**
 * The transaction staging `run` as one output at `stagerAddress`. It is funded
 * from the wallet's plain-Ada outputs only, so it never spends an earlier
 * staging output.
 */
export const buildUnsignedMidgardCekStagingProgram = (
  lucid: LucidEvolution,
  {
    run,
    stagerAddress,
  }: {
    readonly run: MidgardCekStagingRun;
    readonly stagerAddress: string;
  },
): Effect.Effect<TxSignBuilder, Error> =>
  Effect.tryPromise({
    try: async () => {
      const fundingInputs = (await lucid.wallet().getUtxos()).filter(plainAda);
      if (fundingInputs.length === 0) {
        throw new Error("staging requires plain-Ada funding");
      }
      const { coinsPerUtxoByte } = await resolveProtocolParameters(lucid);
      return await lucid
        .newTx()
        .pay.ToAddressWithData(
          stagerAddress,
          { kind: "inline", value: run.datumCbor },
          {
            lovelace: stabilizedMinimumLovelace({
              addressBech32: stagerAddress,
              datumCbor: run.datumCbor,
              coinsPerUtxoByte,
              label: "staging output",
            }),
          },
        )
        .complete({ localUPLCEval: true, presetWalletInputs: fundingInputs });
    },
    catch: (cause) =>
      new Error(
        `Failed to build a staging transaction: ${
          cause instanceof Error ? cause.message : String(cause)
        }`,
      ),
  });

/** The output reference of `run`'s staging output in the staging transaction. */
export const midgardCekStagingOutRef = (
  tx: TxSignBuilder,
  run: MidgardCekStagingRun,
): MidgardCekStagedOutRef => {
  const transaction = tx.toTransaction();
  const outputs = transaction.body().outputs();
  for (let index = 0; index < outputs.len(); index += 1) {
    const datum = outputs.get(index).datum()?.as_datum();
    if (datum !== undefined && datum.to_cbor_hex() === run.datumCbor) {
      return {
        transactionId: Buffer.from(tx.toHash(), "hex"),
        outputIndex: index,
      };
    }
  }
  throw new Error("staging transaction has no output holding the run");
};

const refKey = (ref: {
  readonly transactionId: Uint8Array | string;
  readonly outputIndex: number;
}): string =>
  `${
    typeof ref.transactionId === "string"
      ? ref.transactionId.toLowerCase()
      : hex(ref.transactionId)
  }#${ref.outputIndex.toString()}`;

/**
 * The local record of which threads pin which staging outputs. A thread pins
 * the outputs its staged step names from before its binder until its settle
 * lands at confirmation depth, and is released then.
 */
export class MidgardCekStagingPins {
  private readonly threads = new Map<string, Set<string>>();

  constructor(
    entries: Iterable<
      readonly [string, readonly MidgardCekStagedOutRef[]]
    > = [],
  ) {
    for (const [thread, refs] of entries) this.pin(thread, refs);
  }

  /** Pins `refs` for `thread`. */
  pin(thread: string, refs: readonly MidgardCekStagedOutRef[]): void {
    const pinned = this.threads.get(thread) ?? new Set<string>();
    for (const ref of refs) pinned.add(refKey(ref));
    this.threads.set(thread, pinned);
  }

  /** Releases every output `thread` pins. */
  release(thread: string): void {
    this.threads.delete(thread);
  }

  /** The threads pinning the output at `ref`. */
  pinnedBy(ref: {
    readonly transactionId: Uint8Array | string;
    readonly outputIndex: number;
  }): string[] {
    const key = refKey(ref);
    return [...this.threads]
      .filter(([, pinned]) => pinned.has(key))
      .map(([thread]) => thread);
  }

  /** Each thread and the outputs it pins, for the caller to keep. */
  entries(): [string, MidgardCekStagedOutRef[]][] {
    return [...this.threads].map(([thread, pinned]) => [
      thread,
      [...pinned].map((key) => {
        const [transactionId, outputIndex] = key.split("#");
        return {
          transactionId: Buffer.from(transactionId!, "hex"),
          outputIndex: Number(outputIndex),
        };
      }),
    ]);
  }
}

/**
 * The transaction spending staging `outputs` back to the wallet. It refuses
 * an output that is not a staging run and an output any thread pins, and it
 * spends nothing else, so no other staging output is swept into it.
 */
export const buildUnsignedMidgardCekStagingReclaimProgram = (
  lucid: LucidEvolution,
  {
    outputs,
    pins,
  }: {
    readonly outputs: readonly UTxO[];
    readonly pins: MidgardCekStagingPins;
  },
): Effect.Effect<TxSignBuilder, Error> =>
  Effect.tryPromise({
    try: async () => {
      if (outputs.length === 0) {
        throw new Error("nothing to reclaim");
      }
      for (const output of outputs) {
        const ref = {
          transactionId: output.txHash,
          outputIndex: output.outputIndex,
        };
        if (
          output.datum == null ||
          midgardCekStagingRunChunks(output.datum) === null
        ) {
          throw new Error(`${refKey(ref)} is not a staging output`);
        }
        const threads = pins.pinnedBy(ref);
        if (threads.length > 0) {
          throw new Error(
            `${refKey(ref)} is pinned by thread ${threads.join(", ")}`,
          );
        }
      }
      return await lucid
        .newTx()
        .collectFrom([...outputs])
        .complete({ localUPLCEval: true, coinSelection: false });
    },
    catch: (cause) =>
      new Error(
        `Failed to reclaim staging outputs: ${
          cause instanceof Error ? cause.message : String(cause)
        }`,
      ),
  });
