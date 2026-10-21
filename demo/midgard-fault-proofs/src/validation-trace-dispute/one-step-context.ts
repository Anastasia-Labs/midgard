/** The one-step resolution of a CEK context step, as our trace plans it: the
 * context step resolver binds the step's CEK witness and auxiliary evidence
 * (ContextBound), the context control routes the thread by the step's
 * context stage (ContextStaged), the stage that route names verifies the
 * step (ContextVerified), directly or, at the item bind stage, through the
 * redeemer item step (one-step-context-item.ts), and the context settle
 * awards the thread. Each holder's datum is what that holder's script
 * derives on chain from the step's witness, auxiliary evidence and the next
 * state's witness; here it is derived off chain from our own trace, field
 * for field. */
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { midgardEnvelopeDataFromCborIterative } from "@al-ft/midgard-validation";
import { Constr, Data } from "@lucid-evolution/lucid";

import {
  planValidationTraceContextItem,
  type ValidationTraceContextItem,
} from "./one-step-context-item.js";

/**
 * The stage each context route reaches, in the context control's order of
 * `stage_script_hashes` (the SDK's `completeCekContextStages` entries).
 */
export const CEK_CONTEXT_ROUTES = [
  "redeemerBegin",
  "itemBind",
  "reference",
  "spend",
  "output",
  "signer",
  "observerAuthenticate",
  "mintInit",
  "mintItem",
  "redeemerSelectAuthenticate",
  "finalizeAuthenticate",
  "assemble",
  "txInfo",
  "seed",
] as const;

export type CekContextRoute = (typeof CEK_CONTEXT_ROUTES)[number];

/**
 * The stages the runner drives: each verifies its step in one hop and sends
 * the thread to the context settle, but the item bind stage, whose step the
 * redeemer item chain verifies.
 */
export const DRIVEN_CEK_CONTEXT_STAGES = [
  "redeemerBegin",
  "itemBind",
  "reference",
  "spend",
  "output",
  "signer",
  "mintInit",
  "assemble",
  "txInfo",
  "seed",
] as const;

export type DrivenCekContextStage = (typeof DRIVEN_CEK_CONTEXT_STAGES)[number];

const isDriven = (route: CekContextRoute): route is DrivenCekContextStage =>
  (DRIVEN_CEK_CONTEXT_STAGES as readonly string[]).includes(route);

/** A context step's resolution: each holder's datum state, as Data CBOR. */
export type ValidationTraceContextStep = Readonly<{
  stage: DrivenCekContextStage;
  /** `CekWitnessControlV1` of the step's witness. */
  controlCbor: string;
  /** `CekContextControlV1` the step's witness carries. */
  contextCbor: string;
  /** `NativeFacts` of the witness's native control. */
  nativeFactsCbor: string;
  executionCursor: bigint;
  /** The `ContextSuccessor` the stage verifies. */
  successorCbor: string;
  /** At the item bind stage, the step's redeemer item step. */
  item: ValidationTraceContextItem | undefined;
}>;

type Item = bigint | string | readonly Item[];

type EnvelopeData = ReturnType<typeof midgardEnvelopeDataFromCborIterative>;

const itemOf = (data: EnvelopeData): Item => {
  if ("int" in data) return BigInt(data.int);
  if ("bytes" in data) return Buffer.from(data.bytes).toString("hex");
  if ("list" in data) return data.list.map(itemOf);
  throw new Error(
    "validation-trace CEK witness holds an item that is neither an integer, bytes nor a list",
  );
};

/** The list a Midgard envelope `cbor` encodes, `length` items long. */
const envelope = (cbor: string, length: number, what: string) => {
  const item = itemOf(
    midgardEnvelopeDataFromCborIterative(Buffer.from(cbor, "hex")),
  );
  if (!Array.isArray(item) || item.length !== length)
    throw new Error(
      `validation-trace CEK ${what} is not a list of ${length.toString()} items`,
    );
  return item as readonly Item[];
};

const scalar = (item: Item | undefined): bigint | string => {
  if (typeof item === "bigint" || typeof item === "string") return item;
  throw new Error("validation-trace CEK witness field is not a scalar");
};

const integer = (item: Item | undefined): bigint => {
  if (typeof item === "bigint") return item;
  throw new Error("validation-trace CEK witness field is not an integer");
};

const bytes = (item: Item | undefined): string => {
  if (typeof item === "string") return item;
  throw new Error("validation-trace CEK witness field is not bytes");
};

/** A record whose fields are `[...]` lists, as `Constr 0` of its scalars. */
const record = (item: Item | undefined, length: number): Constr<Data> => {
  if (!Array.isArray(item) || item.length !== length)
    throw new Error(
      `validation-trace CEK witness record is not ${length.toString()} fields`,
    );
  return new Constr(0, (item as readonly Item[]).map(scalar));
};

/** `List<FrontierPeak>`: each `[height, hash]` as `Constr 0`. */
const peaks = (item: Item | undefined): Data => {
  if (!Array.isArray(item))
    throw new Error("validation-trace CEK frontier peaks are not a list");
  return (item as readonly Item[]).map((peak) => record(peak, 2));
};

/** `native_scripts_control_from_witness`: its frontier peaks' positions. */
const NATIVE_PEAKS = new Set([7, 11, 13, 15, 17, 18, 20, 22]);

/** `NativeFacts`' fields, by their position in the native control. */
const NATIVE_FACTS = [0, 1, 2, 4, 6, 7, 8, 9, 12, 13, 14, 15, 16, 18, 19, 20];

/** `cek_context_control_from_cbor`: its summaries' positions. */
const CONTEXT_SEQUENCES = new Set([11, 12, 13, 14, 16, 21, 22]);
const CONTEXT_SUMMARIES = new Set([18, 23]);

const nativeControlFields = (cbor: string): Data[] =>
  envelope(cbor, 26, "native control").map((item, index) =>
    NATIVE_PEAKS.has(index) ? peaks(item) : scalar(item),
  );

/** `cek_context_control_from_cbor` of `cbor`, as Data. */
export const cekContextControlData = (cbor: string): Constr<Data> =>
  new Constr(
    0,
    envelope(cbor, 24, "context control").map((item, index) =>
      CONTEXT_SEQUENCES.has(index)
        ? record(item, 4)
        : CONTEXT_SUMMARIES.has(index)
          ? record(item, 3)
          : scalar(item),
    ),
  );

/** The context control's stage and redeemer context control hash. */
const contextRoute = (
  context: Constr<Data>,
  auxiliaryCbor: string,
): CekContextRoute => {
  const stage = Number(context.fields[0] as bigint);
  const hash = context.fields[8] as string;
  const auxiliary = Data.from(auxiliaryCbor);
  const index =
    stage === 0
      ? hash === ""
        ? 0
        : 1
      : stage >= 1 && stage <= 6
        ? stage + 1
        : stage === 9
          ? auxiliary instanceof Constr && auxiliary.index === 18
            ? 1
            : 9
          : stage === 7
            ? undefined
            : stage;
  const route = index === undefined ? undefined : CEK_CONTEXT_ROUTES[index];
  if (route === undefined)
    throw new Error(
      `validation-trace CEK context stage ${stage.toString()} routes nowhere`,
    );
  return route;
};

/**
 * The context step from the witness `witnessCbor` with auxiliary evidence
 * `auxiliaryCbor`, whose next state carries the witness
 * `successorWitnessCbor`: the stage it routes to, or that stage alone when
 * the runner does not drive it.
 */
export const planValidationTraceContextStep = ({
  witnessCbor,
  auxiliaryCbor,
  successorWitnessCbor,
}: {
  readonly witnessCbor: string;
  readonly auxiliaryCbor: string;
  readonly successorWitnessCbor: string;
}):
  | ValidationTraceContextStep
  | Readonly<{ stage: CekContextRoute; driven: false }> => {
  const witness = envelope(witnessCbor, 11, "witness");
  const native = nativeControlFields(bytes(witness[0]));
  const context = cekContextControlData(bytes(witness[1]));
  const stage = contextRoute(context, auxiliaryCbor);
  if (!isDriven(stage)) return { stage, driven: false };
  const successor = envelope(successorWitnessCbor, 11, "successor witness");
  return Object.freeze({
    stage,
    controlCbor: Data.to(
      new Constr(0, [new Constr(0, native), ...witness.slice(1).map(scalar)]),
    ),
    contextCbor: Data.to(context),
    nativeFactsCbor: Data.to(
      new Constr(
        0,
        NATIVE_FACTS.map((index) => native[index]!),
      ),
    ),
    executionCursor: integer(witness[2]),
    successorCbor: Data.to(
      (stage as CekContextRoute) === "seed"
        ? new Constr(1, [
            bytes(successor[5]),
            integer(successor[7]),
            integer(successor[8]),
            integer(successor[9]),
            integer(successor[10]),
          ])
        : new Constr(0, [cekContextControlData(bytes(successor[1]))]),
    ),
    item:
      stage === "itemBind"
        ? planValidationTraceContextItem({
            auxiliaryCbor,
            contextStage: context.fields[0] as bigint,
          })
        : undefined,
  });
};

/** `ContextBound`'s source fields, from the prepared resolution's pre-state. */
export const contextBoundSource = (preparedCbor: string) => ({
  transactionIdCbor: plutusConstrFieldCbor(preparedCbor, [1, 1, 2]),
  sourceKindCbor: plutusConstrFieldCbor(preparedCbor, [1, 1, 5]),
});
