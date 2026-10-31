/** The finalization of a CEK context step our trace routes to the finalize
 * authenticate, as our trace plans it: the authenticate binds the step's
 * redeemers control (FinalizationPending), and for a Cardano spend the
 * finalize spend writes the context's successor from the spent input's
 * descriptor. A finalization of any other purpose is not driven. */
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { Constr, Data } from "@lucid-evolution/lucid";

/** `FinalizationPending`'s fields past its staged context, as Data CBOR. */
export type ValidationTraceContextFinalization = Readonly<{
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

/** `CekRedeemerContextControlV1`'s map items and current redeemer. */
const MAP_ITEMS = 1;
const CURRENT_REDEEMER = 5;

/**
 * The finalization whose evidence is `auxiliaryCbor`, on the context control
 * `context`; undefined for a purpose other than a Cardano spend, whose
 * finalize the runner does not drive.
 */
export const planValidationTraceContextFinalization = ({
  auxiliaryCbor,
  context,
}: {
  readonly auxiliaryCbor: string;
  readonly context: Constr<Data>;
}): ValidationTraceContextFinalization | undefined => {
  const spend =
    context.fields[LANGUAGE_TAG] !== 128n &&
    context.fields[PURPOSE_KIND] === 0n;
  const auxiliary = Data.from(auxiliaryCbor);
  const expected = spend ? SPEND_AUXILIARY : FINALIZE_AUXILIARY;
  if (!(auxiliary instanceof Constr) || auxiliary.index !== expected)
    throw new Error(
      `validation-trace CEK finalize evidence is not constructor ${expected.toString()}`,
    );
  if (!spend) return undefined;
  return Object.freeze({
    redeemerItemsCbor: plutusConstrFieldCbor(auxiliaryCbor, [0, MAP_ITEMS]),
    redeemerCbor: plutusConstrFieldCbor(auxiliaryCbor, [0, CURRENT_REDEEMER]),
  });
};
