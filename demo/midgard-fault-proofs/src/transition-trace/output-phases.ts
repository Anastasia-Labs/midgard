import {
  buildMidgardLedgerOutputScanTrace,
  buildMidgardLedgerOutputValueTrace,
  encodeMidgardLedgerOutputScanControl,
  encodeMidgardLedgerOutputValueControl,
  MidgardLedgerOutputScanStages,
} from "@al-ft/midgard-core";
import type * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  dataSummaryWalkData,
  type DataSummaryWalkTrace,
  dataSummaryWalkWitnessData,
  planDataSummaryWalkTransaction,
  traceDataSummaryWalk,
} from "./data-summary-walk.js";

const ValueHeadSchema = Data.Object({
  asset_name: Data.Bytes(),
  quantity: Data.Integer(),
  tail: Data.Object({
    root: Data.Bytes(),
    length: Data.Integer(),
    payload_cbor_length: Data.Integer(),
    memory: Data.Integer(),
  }),
});
const ValueWitnessSchema = Data.Enum([
  Data.Literal("LedgerOutputValueNoWitness"),
  Data.Object({
    LedgerOutputValueAsset: Data.Object({
      asset_index: Data.Integer(),
      policy_id: Data.Bytes(),
      asset_name: Data.Bytes(),
      quantity: Data.Integer(),
      siblings: Data.Array(Data.Bytes()),
      previous: Data.Nullable(ValueHeadSchema),
    }),
  }),
]);
type ValueWitness = Data.Static<typeof ValueWitnessSchema>;
const ValueWitnesses = Data.Array(
  ValueWitnessSchema,
) as unknown as ValueWitness[];
const hex = (bytes: Uint8Array) => Buffer.from(bytes).toString("hex");
// Keep one immutable derivation: a dense output takes hundreds of bounded
// transactions, while the source bytes and deterministic trace stay fixed.
let cached: {
  hex: string;
  scan: ReturnType<typeof buildMidgardLedgerOutputScanTrace>;
  value: ReturnType<typeof buildMidgardLedgerOutputValueTrace> | null;
  datum: {
    readonly trace: DataSummaryWalkTrace;
    /** Each checkpoint's encoded `Walk`, to its step index. */
    readonly checkpoints: ReadonlyMap<string, number>;
  } | null;
} | null = null;
const outputTrace = (bytes: Buffer) => {
  const key = hex(bytes);
  if (cached?.hex === key) return cached;
  cached = {
    hex: key,
    scan: buildMidgardLedgerOutputScanTrace(bytes),
    value: null,
    datum: null,
  };
  return cached;
};
export const nextTransitionOutputPhase = (
  state: SDK.TransitionTraceFinalState,
  outputCbor: string,
  depositAssetIndexes?: Readonly<Record<string, number>>,
): { state: SDK.TransitionTraceFinalState; redeemer: string } => {
  const bytes = Buffer.from(outputCbor, "hex");
  const cachedTrace = outputTrace(bytes);
  const scan = cachedTrace.scan;
  if (state.phase === 2n) {
    const offset =
      state.scan_cbor === ""
        ? 0
        : scan.steps.findIndex(
            (step) =>
              hex(encodeMidgardLedgerOutputScanControl(step.control)) ===
              state.scan_cbor,
          );
    if (offset < 0)
      throw new Error(
        "Transition output scan checkpoint differs from committed bytes",
      );
    const selected = scan.steps.slice(offset, offset + 4);
    const last = selected.at(-1);
    if (last === undefined)
      throw new Error("Transition output scan has no successor");
    let start = state.value_start;
    let end = state.value_end;
    for (const step of selected) {
      if (step.control.stage === MidgardLedgerOutputScanStages.RequiredFields)
        start = BigInt(step.next.cursor);
      if (
        step.control.stage <= MidgardLedgerOutputScanStages.Asset &&
        step.next.stage === MidgardLedgerOutputScanStages.OptionalField
      )
        end = BigInt(step.next.cursor);
    }
    return {
      state: {
        ...state,
        phase:
          last.next.stage === MidgardLedgerOutputScanStages.Terminal ? 7n : 2n,
        scan_cbor: hex(encodeMidgardLedgerOutputScanControl(last.next)),
        value_start: start,
        value_end: end,
      },
      redeemer: Data.void(),
    };
  }
  if (state.phase !== 7n)
    throw new Error("Transition output phase is not a bounded fold");
  const value =
    cachedTrace.value ??
    (cachedTrace.value = buildMidgardLedgerOutputValueTrace({
      assets: scan.steps.flatMap((step) =>
        step.asset === null ? [] : [step.asset],
      ),
      lovelace: scan.terminal.lovelace,
    }));
  const offset =
    state.value_cbor === ""
      ? 0
      : value.steps.findIndex(
          (step) =>
            hex(encodeMidgardLedgerOutputValueControl(step.control)) ===
            state.value_cbor,
        );
  if (offset < 0)
    throw new Error(
      "Transition value checkpoint differs from authenticated asset fold",
    );
  const selected = value.steps.slice(
    offset,
    offset + (state.kind === 1n ? 4 : 8),
  );
  const last = selected.at(-1);
  if (last === undefined)
    throw new Error("Transition value fold has no successor");
  const witnesses: ValueWitness[] = selected.map(({ witness }) =>
    witness === null
      ? "LedgerOutputValueNoWitness"
      : {
          LedgerOutputValueAsset: {
            asset_index: BigInt(witness.assetIndex),
            policy_id: hex(witness.policyId),
            asset_name: hex(witness.assetName),
            quantity: witness.quantity,
            siblings: witness.siblings.map(hex),
            previous:
              witness.previous === null
                ? null
                : {
                    asset_name: hex(witness.previous.assetName),
                    quantity: witness.previous.quantity,
                    tail: {
                      root: hex(witness.previous.tail.root),
                      length: witness.previous.tail.length,
                      payload_cbor_length:
                        witness.previous.tail.payloadCborLength,
                      memory: witness.previous.tail.memory,
                    },
                  },
          },
        },
  );
  const summary = last.next.result;
  return {
    state: {
      ...state,
      phase: summary === null ? 7n : state.kind === 1n ? 10n : 8n,
      value_cbor: hex(encodeMidgardLedgerOutputValueControl(last.next)),
      value_summary:
        summary === null
          ? null
          : {
              root: hex(summary.root),
              cbor_length: summary.cborLength,
              memory: summary.memory,
            },
    },
    redeemer:
      state.kind === 1n
        ? Data.to(
            witnesses.map((witness) => [
              Data.from(
                Data.to(witness, ValueWitnessSchema as unknown as ValueWitness),
              ),
              witness === "LedgerOutputValueNoWitness"
                ? -1n
                : BigInt(
                    depositAssetIndexes![
                      witness.LedgerOutputValueAsset.policy_id +
                        witness.LedgerOutputValueAsset.asset_name
                    ]!,
                  ),
            ]),
          )
        : Data.to(witnesses, ValueWitnesses),
  };
};

/** The phase-8 datum walk could not summarise an output's inline datum. */
export class TransitionDatumUnreadableError extends Error {
  readonly _tag = "TransitionDatumUnreadableError";
  constructor() {
    super("Transition output datum is not canonical Data");
  }
}

const datumWalk = (bytes: Buffer) => {
  const trace = outputTrace(bytes);
  if (trace.datum !== null) return trace.datum;
  const { datumOffset, datumLength } = trace.scan.terminal;
  const walk = traceDataSummaryWalk(
    bytes,
    datumOffset,
    datumOffset + datumLength,
  );
  trace.datum = {
    trace: walk,
    checkpoints: new Map(
      walk.points.map((point, index) => [
        Data.to(dataSummaryWalkData(point)),
        index,
      ]),
    ),
  };
  return trace.datum;
};

/**
 * The next phase-8 checkpoint of the output whose committed bytes are
 * `outputCbor`: one transaction of the walk over its inline datum, then
 * phase 3 with `summaries` once the walk ends. An output without an inline
 * datum is summarised at once.
 */
export const nextTransitionDatumPhase = (
  state: SDK.TransitionTraceFinalState,
  outputCbor: string,
  summaries: SDK.TransitionTraceOutputSummaries["summaries"][number],
): { state: SDK.TransitionTraceFinalState; redeemer: string } => {
  const bytes = Buffer.from(outputCbor, "hex");
  const summarised = {
    ...state,
    phase: 3n,
    summaries: { summaries: [summaries] },
    datum_walk: null,
  };
  if (outputTrace(bytes).scan.terminal.datumOffset === -1)
    return {
      state: summarised,
      redeemer: Data.to(
        dataSummaryWalkWitnessData({
          steps: 1,
          restore: [],
          base: Buffer.alloc(0),
        }),
      ),
    };
  const { trace, checkpoints } = datumWalk(bytes);
  if (trace.outcome.kind === "unreadable")
    throw new TransitionDatumUnreadableError();
  const from =
    state.datum_walk === null
      ? 0
      : checkpoints.get(Data.to(state.datum_walk as Data));
  if (from === undefined)
    throw new Error(
      "Transition datum checkpoint differs from the committed output bytes",
    );
  const transaction = planDataSummaryWalkTransaction(trace, from);
  const redeemer = Data.to(dataSummaryWalkWitnessData(transaction));
  if (transaction.final) return { state: summarised, redeemer };
  return {
    state: {
      ...state,
      datum_walk: dataSummaryWalkData(trace.points[from + transaction.steps]!),
    },
    redeemer,
  };
};
