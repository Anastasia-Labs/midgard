/** The finalization of a CEK context step our trace routes to the finalize
 * authenticate, as our trace plans it: the authenticate binds the step's
 * redeemers control (FinalizationPending) and routes the thread by the
 * staged context, a MidgardV1 execution to the finalize midgard and a
 * PlutusV3 one by its purpose kind, to the finalize spend, mint or withdraw;
 * that finalize writes the context's successor from the execution's script
 * info, for a Cardano spend from the spent input's descriptor. */
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { Constr, Data } from "@lucid-evolution/lucid";

import type { ValidationTraceContextFollowerHolder } from "./one-step-context-followers.js";

/** A finalize the finalize authenticate routes a thread to. */
export type ValidationTraceContextFinalizeHolder = Extract<
  ValidationTraceContextFollowerHolder,
  `contextFinalize${string}`
>;

/** `FinalizationPending`'s fields past its staged context, as Data CBOR,
 * and the finalize that verifies it. */
export type ValidationTraceContextFinalization = Readonly<{
  /** The finalize the authenticate routes the thread to. */
  holder: ValidationTraceContextFinalizeHolder;
  /** The redeemers control's `map_items`. */
  redeemerItemsCbor: string;
  /** The redeemers control's `current_redeemer`. */
  redeemerCbor: string;
}>;

/** The finalize evidence of a Cardano spend, and of every other purpose. */
const SPEND_AUXILIARY = 20;
const FINALIZE_AUXILIARY = 19;

/** `cek_context_control_from_cbor`'s language tag and purpose kind. */
const LANGUAGE_TAG = 1;
const PURPOSE_KIND = 3;
const MIDGARD_LANGUAGE_TAG = 128n;

/** `CekRedeemerContextControlV1`'s map items and current redeemer. */
const MAP_ITEMS = 1;
const CURRENT_REDEEMER = 5;

/**
 * The finalize at each route of the finalize authenticate, in the order of
 * its `summary_script_hashes`: a PlutusV3 execution's route is its purpose
 * kind, and a MidgardV1 execution's the last. Route 3, the finalize
 * observe, has no finalize here: a PlutusV3 receive is rejected at its
 * execution's selection, before any context, and its finalize derives no
 * script info, so no honest trace reaches it.
 */
const FINALIZE_ROUTES: readonly (
  | ValidationTraceContextFinalizeHolder
  | undefined
)[] = [
  "contextFinalizeSpend",
  "contextFinalizeMint",
  "contextFinalizeWithdraw",
  undefined,
  "contextFinalizeMidgard",
];
const MIDGARD_ROUTE = 4;

/**
 * The finalization whose evidence is `auxiliaryCbor`, on the context control
 * `context`; undefined at a route no honest trace reaches.
 */
export const planValidationTraceContextFinalization = ({
  auxiliaryCbor,
  context,
}: {
  readonly auxiliaryCbor: string;
  readonly context: Constr<Data>;
}): ValidationTraceContextFinalization | undefined => {
  const midgard = context.fields[LANGUAGE_TAG] === MIDGARD_LANGUAGE_TAG;
  const purposeKind = context.fields[PURPOSE_KIND];
  const spend = !midgard && purposeKind === 0n;
  const auxiliary = Data.from(auxiliaryCbor);
  const expected = spend ? SPEND_AUXILIARY : FINALIZE_AUXILIARY;
  if (!(auxiliary instanceof Constr) || auxiliary.index !== expected)
    throw new Error(
      `validation-trace CEK finalize evidence is not constructor ${expected.toString()}`,
    );
  const holder = midgard
    ? FINALIZE_ROUTES[MIDGARD_ROUTE]
    : typeof purposeKind === "bigint" && purposeKind < BigInt(MIDGARD_ROUTE)
      ? FINALIZE_ROUTES[Number(purposeKind)]
      : undefined;
  if (holder === undefined) return undefined;
  return Object.freeze({
    holder,
    redeemerItemsCbor: plutusConstrFieldCbor(auxiliaryCbor, [0, MAP_ITEMS]),
    redeemerCbor: plutusConstrFieldCbor(auxiliaryCbor, [0, CURRENT_REDEEMER]),
  });
};
