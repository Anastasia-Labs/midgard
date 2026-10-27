/** The paged evaluation custody of `executionNativeScriptInvalid`: the
 * signer frontier over field 7, then the token walk over the committed
 * native source. Every datum these roles hold is a page boundary, so a
 * thread continues from the state its datum holds, with a page of any size. */
import {
  buildMidgardBoundedItem,
  decodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { stepDatum } from "../direct-purpose-family/custody.js";
import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";
import type { DirectScriptSourceMaterial } from "../direct-script-derivation/sources.js";
import type { FittedPage } from "../page-fit.js";
import type { ExecutionNativeScriptInvalidTransition } from "./custody.js";
import {
  INITIAL_NATIVE_EVALUATION_POSITION,
  type NativeEvaluationContext,
  nativeEvaluationPage,
  type NativeEvaluationPosition,
  nativeEvaluationPositionOf,
  nativeEvaluationStateFields,
} from "./evaluate.js";
import { nativeSignerFrontier, nativeSignerSet } from "./signers.js";

/** Signers appended per `signers` page at most. */
export const EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_BATCH = 32;
/** Token reads and frame pops per `evaluate` page at most. */
export const EXECUTION_NATIVE_SCRIPT_INVALID_EVALUATE_BATCH = 64;

export type ExecutionNativeScriptInvalidWalkRole = "signers" | "evaluate";

export const isExecutionNativeScriptInvalidWalkRole = (
  role: string,
): role is ExecutionNativeScriptInvalidWalkRole =>
  role === "signers" || role === "evaluate";

export type ExecutionNativeScriptInvalidWalkBatch = {
  readonly signers?: number;
  readonly evaluate?: number;
};

/** The bounded item a source descriptor commits: a witness at its field-6
 * position, a reference script at its output's index. */
export const executionNativeScriptInvalidSourceItem = (
  source: DirectScriptSourceMaterial,
) => {
  const { descriptor } = source;
  const item = buildMidgardBoundedItem(
    descriptor.field_index === 6n
      ? {
          fieldIndex: 6,
          itemIndex: Number(descriptor.item_index),
          bytes: source.item,
        }
      : {
          fieldIndex: 2,
          itemIndex: decodeMidgardSpendInputItem(
            Buffer.from(descriptor.source_key, "hex"),
          ).outputIndex,
          bytes: source.item,
        },
  );
  if (item.commitment.toString("hex") !== descriptor.item_commitment)
    throw new Error(
      "execution-native-script-invalid: source item differs from its commitment",
    );
  return item;
};

/** What the walk reads: the source item, the field-7 signers and the
 * transaction's validity interval. */
export const executionNativeScriptInvalidEvaluationContext = (
  material: DirectScriptMaterial,
  source: DirectScriptSourceMaterial,
): NativeEvaluationContext => {
  const body = material.material.compact.transactionBody;
  return {
    item: executionNativeScriptInvalidSourceItem(source),
    signers: nativeSignerSet(material.fields[7]!),
    validityIntervalStart: body.validityIntervalStart,
    validityIntervalEnd: body.validityIntervalEnd,
  };
};

export type ExecutionNativeScriptInvalidWalk = Readonly<{
  /** The datum the source lookup pays `signers`. */
  initial: string;
  page: (
    role: ExecutionNativeScriptInvalidWalkRole,
    datum: string,
    size?: number,
  ) => FittedPage<ExecutionNativeScriptInvalidTransition>;
  transitionAt: (
    role: ExecutionNativeScriptInvalidWalkRole,
    datum: string,
  ) => ExecutionNativeScriptInvalidTransition;
  /** The whole walk at the configured page sizes. */
  transitions: readonly ExecutionNativeScriptInvalidTransition[];
  final: SDK.ExecutionNativeScriptInvalidFinalState;
}>;

const evalState = (datum: string) => {
  const state = Data.from(
    datum,
    SDK.ExecutionNativeScriptInvalidEvalDatum,
  ).data;
  if (state === null)
    throw new Error("execution-native-script-invalid: walk datum is empty");
  return state;
};

export const executionNativeScriptInvalidWalk = ({
  material,
  prover,
  coordinate,
  source,
  batch = {},
}: {
  readonly material: DirectScriptMaterial;
  readonly prover: string;
  readonly coordinate: SDK.DirectPurposeCoordinate;
  readonly source: DirectScriptSourceMaterial;
  readonly batch?: ExecutionNativeScriptInvalidWalkBatch;
}): ExecutionNativeScriptInvalidWalk => {
  const context = executionNativeScriptInvalidEvaluationContext(
    material,
    source,
  );
  const signerBatch =
    batch.signers ?? EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_BATCH;
  const evaluateBatch =
    batch.evaluate ?? EXECUTION_NATIVE_SCRIPT_INVALID_EVALUATE_BATCH;
  const evalDatum = (state: SDK.ExecutionNativeScriptInvalidEvalState) =>
    stepDatum(prover, state, SDK.ExecutionNativeScriptInvalidEvalDatum);
  const initialState: SDK.ExecutionNativeScriptInvalidEvalState = {
    bound: material.bound,
    coordinate,
    item_commitment: source.descriptor.item_commitment,
    validity_interval_start: context.validityIntervalStart,
    validity_interval_end: context.validityIntervalEnd,
    ...nativeSignerFrontier(context.signers, 0),
    ...nativeEvaluationStateFields(INITIAL_NATIVE_EVALUATION_POSITION),
  };
  const positions = new Map<string, NativeEvaluationPosition>();
  const positionOf = (datum: string) => {
    const state = evalState(datum);
    const key = evalDatum(state).toLowerCase();
    let position = positions.get(key);
    if (position === undefined) {
      position = nativeEvaluationPositionOf(context, state);
      positions.set(key, position);
    }
    return { state, position };
  };
  const signersPage = (datum: string, size: number) => {
    const state = evalState(datum);
    const from = Number(state.signer_count);
    const count = context.signers.leaves.length;
    const take = Math.min(size, count - from);
    const next = {
      ...state,
      ...nativeSignerFrontier(context.signers, from + take),
    };
    const transition: ExecutionNativeScriptInvalidTransition = {
      label: `signers ${from.toString()}`,
      role: "signers",
      next: from + take === count ? "evaluate" : "signers",
      datum: evalDatum(next),
      fields: [7],
      redeemer: ({ indices, open }) =>
        Data.to(
          {
            Continue: [{ ...indices, opening: open(7), budget: BigInt(size) }],
          },
          SDK.ExecutionNativeScriptInvalidSignersRedeemer,
        ),
    };
    return { size: Math.max(take, 1), transitions: [transition] };
  };
  const evaluatePage = (datum: string, size: number) => {
    const { state, position } = positionOf(datum);
    const walked = nativeEvaluationPage(context, position, size);
    let next: string;
    if (walked.satisfied === null) {
      const nextState = {
        ...state,
        ...nativeEvaluationStateFields(walked.next),
      };
      next = evalDatum(nextState);
      positions.set(next.toLowerCase(), walked.next);
    } else
      next = stepDatum(
        prover,
        {
          bound: material.bound,
          fact: {
            Evaluated: { coordinate, satisfied: walked.satisfied },
          },
        },
        SDK.ExecutionNativeScriptInvalidFinalDatum,
      );
    const transition: ExecutionNativeScriptInvalidTransition = {
      label: `evaluate ${state.cursor.toString()}`,
      role: "evaluate",
      next: walked.satisfied === null ? "evaluate" : "terminal",
      datum: next,
      fields: [],
      redeemer: ({ indices }) =>
        Data.to(
          { Continue: [{ ...indices, ...walked.page }] },
          SDK.ExecutionNativeScriptInvalidEvaluateRedeemer,
        ),
    };
    return { size, transitions: [transition], satisfied: walked.satisfied };
  };
  const page: ExecutionNativeScriptInvalidWalk["page"] = (
    role,
    datum,
    size,
  ) => {
    const taken = size ?? (role === "signers" ? signerBatch : evaluateBatch);
    if (!Number.isSafeInteger(taken) || taken < 1)
      throw new Error(
        `execution-native-script-invalid: a ${role} page takes at least one step`,
      );
    return role === "signers"
      ? signersPage(datum, taken)
      : evaluatePage(datum, taken);
  };
  const transitions: ExecutionNativeScriptInvalidTransition[] = [];
  let role: ExecutionNativeScriptInvalidWalkRole = "signers";
  let datum = evalDatum(initialState);
  let satisfied: boolean | undefined;
  for (;;) {
    if (role === "signers") {
      const [transition] = signersPage(datum, signerBatch).transitions;
      transitions.push(transition!);
      datum = transition!.datum;
      if (transition!.next === "evaluate") role = "evaluate";
      continue;
    }
    const fitted = evaluatePage(datum, evaluateBatch);
    transitions.push(fitted.transitions[0]!);
    datum = fitted.transitions[0]!.datum;
    if (fitted.satisfied !== null) {
      satisfied = fitted.satisfied;
      break;
    }
  }
  if (satisfied === undefined)
    throw new Error("execution-native-script-invalid: the walk did not end");
  return Object.freeze({
    initial: evalDatum(initialState),
    page,
    transitionAt: (role, datum) => page(role, datum).transitions[0]!,
    transitions,
    final: {
      bound: material.bound,
      fact: { Evaluated: { coordinate, satisfied } },
    },
  });
};
