/** The scripts our trace's CEK script selections run: which selection needs
 * the selected script's decode record, and the script's versioned item,
 * read from the disputed transaction (an inline witness) or its resolved
 * inputs (a reference script) and checked against the commitment the
 * selection carries. */
import {
  buildMidgardBoundedItem,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core";
import {
  decodeMidgardNativeByteListPreimage,
  decodeMidgardTxOutput,
  encodeMidgardVersionedScript,
} from "@al-ft/midgard-core/codec";
import type {
  DeterministicValidationMachineTrace,
  ValidationMachineWorkWitness,
} from "@al-ft/midgard-validation";

import type { CheckedLedger } from "../ledger-at.js";

/** A CEK script selection's evidence. */
export type ValidationTraceSelection = Extract<
  NonNullable<ValidationMachineWorkWitness["auxiliary"]>,
  { readonly kind: "nativeExecutionScan" }
>;

/** A selection's script source. */
export type ValidationTraceSelectionSource = Pick<
  ValidationTraceSelection,
  "source" | "firstChunkProof"
>;

/**
 * The CEK script selection `witness` makes when its successor rests on the
 * selected script's decode record; undefined for any other step, a native
 * selection, and a PlutusV3 receive purpose, which is refused before any
 * program is read (`validateCekSelectionDecodeRecord`'s own exemptions).
 */
export const recordedSelectionOf = (
  witness: ValidationMachineWorkWitness | undefined,
): ValidationTraceSelection | undefined => {
  const auxiliary = witness?.auxiliary;
  if (
    witness?.phase !== "cek" ||
    auxiliary?.kind !== "nativeExecutionScan" ||
    auxiliary.languageTag === 0 ||
    (auxiliary.languageTag === 3 && auxiliary.purpose.purposeKind === 3)
  )
    return undefined;
  return auxiliary;
};

/** Every selection of `trace` that rests on a decode record, in order. */
export const recordedSelectionsOf = (
  trace: DeterministicValidationMachineTrace,
): readonly ValidationTraceSelection[] =>
  trace.witnesses.flatMap((witness) => {
    const selection = recordedSelectionOf(witness);
    return selection === undefined ? [] : [selection];
  });

/**
 * Reads the versioned item of a selection's script: the inline witness at
 * the item its first chunk names in `transactionCbor`'s script witnesses,
 * or the reference script of the resolved output `ledger` keeps under the
 * source's out-ref. Throws unless the item is exactly the one the
 * selection commits to.
 */
export const selectedScriptItemReader =
  ({
    transactionCbor,
    ledger,
  }: {
    readonly transactionCbor: Uint8Array;
    readonly ledger: CheckedLedger;
  }) =>
  ({ source, firstChunkProof }: ValidationTraceSelectionSource): Buffer => {
    let bytes: Uint8Array | undefined;
    if (source.originKind === "inline") {
      const { witnessSet } =
        decodeMidgardNativeTxFullFromCanonicalCbor(transactionCbor);
      bytes = decodeMidgardNativeByteListPreimage(
        witnessSet.scriptTxWitsPreimageCbor,
        "script_tx_wits",
      )[firstChunkProof.itemIndex];
    } else {
      const output = ledger.outputs.get(source.sourceKey.toString("hex"));
      const script =
        output === undefined
          ? undefined
          : decodeMidgardTxOutput(output).script_ref;
      bytes =
        script === undefined ? undefined : encodeMidgardVersionedScript(script);
    }
    if (bytes === undefined)
      throw new Error(
        `validation-trace selection's ${source.originKind} script is not in the disputed transaction's data`,
      );
    const item = buildMidgardBoundedItem({
      fieldIndex: firstChunkProof.fieldIndex,
      itemIndex: firstChunkProof.itemIndex,
      bytes,
    });
    if (
      item.bytes.length !== source.scriptTotalLength ||
      !item.commitment.equals(source.scriptItemCommitment)
    )
      throw new Error(
        "validation-trace selection's script differs from the item it commits to",
      );
    return Buffer.from(item.bytes);
  };

/**
 * The item of the first script our trace selects whose decode record
 * `recorded` does not find, or null when every one is recorded. The scripts
 * are taken in trace order, each once.
 */
export const unrecordedSelectedScript = async ({
  trace,
  scriptItem,
  recorded,
}: {
  readonly trace: DeterministicValidationMachineTrace;
  readonly scriptItem: (selection: ValidationTraceSelectionSource) => Buffer;
  readonly recorded: (scriptHashHex: string) => Promise<boolean>;
}): Promise<Buffer | null> => {
  const seen = new Set<string>();
  for (const selection of recordedSelectionsOf(trace)) {
    const scriptHashHex = selection.purpose.scriptHash.toString("hex");
    if (seen.has(scriptHashHex)) continue;
    seen.add(scriptHashHex);
    if (!(await recorded(scriptHashHex))) return scriptItem(selection);
  }
  return null;
};
