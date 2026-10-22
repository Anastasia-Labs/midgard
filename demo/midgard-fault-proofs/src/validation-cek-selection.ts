import { compareOutRefs, findOutRefIndex } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildValidationOneStepArgument,
  type CekSelectionDecodeRecord,
  type ValidationMachineFieldCarriageResolver,
  type ValidationTraceSteps,
} from "@al-ft/midgard-validation";
import { Data, type RedeemerContext, type UTxO } from "@lucid-evolution/lucid";

/**
 * Compose in Lucid's final redeemer callback: every reference index is derived
 * from that callback's actual reference inputs in ledger order. No insertion
 * order or pre-balancing index is accepted. The caller adds these UTxOs to the
 * transaction before completing it; a missing record or yield fails here.
 */
export const buildValidationCekExecutionSelectionAction = ({
  context,
  trace,
  stateIndex,
  inputIndex,
  outputIndex,
  yieldReferences,
  decodeRecordReference,
  scriptDecodePolicyId,
  resolveFieldCarriage,
}: {
  readonly context: Pick<RedeemerContext, "referenceInputs">;
  readonly trace: ValidationTraceSteps;
  readonly stateIndex: number;
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly yieldReferences: readonly [UTxO, UTxO];
  readonly decodeRecordReference?: UTxO;
  readonly scriptDecodePolicyId: string;
  readonly resolveFieldCarriage?: ValidationMachineFieldCarriageResolver;
}): SDK.ValidationCekExecutionSelectionAction => {
  const references = [...context.referenceInputs].sort(compareOutRefs);
  for (let index = 1; index < references.length; index += 1) {
    if (compareOutRefs(references[index - 1]!, references[index]!) === 0)
      throw new Error(
        "CEK selection final reference inputs contain a duplicate out-ref",
      );
  }
  const referenceIndex = (utxo: UTxO): number => {
    const index = findOutRefIndex(references, utxo);
    if (index === undefined)
      throw new Error(
        "CEK selection reference is absent from the final transaction",
      );
    return index;
  };
  let recordIndex = -1;
  const argument = buildValidationOneStepArgument({
    trace,
    stateIndex,
    ...(resolveFieldCarriage === undefined ? {} : { resolveFieldCarriage }),
    ...(decodeRecordReference === undefined
      ? {}
      : {
          resolveCekDecodeRecord: ({
            scriptHash,
            languageTag,
          }): CekSelectionDecodeRecord => {
            recordIndex = referenceIndex(decodeRecordReference);
            const record = SDK.scriptDecodeRecordFromReferenceInput({
              referenceInputs: references,
              referenceInputIndex: recordIndex,
              scriptDecodePolicyId,
              scriptHash: scriptHash.toString("hex"),
              language: languageTag,
            });
            if (!("Decoded" in record))
              throw new Error(
                "CEK selection requires a Decoded record; Refused has no execution successor",
              );
            const decoded = record.Decoded;
            if (
              ![0n, 5n, 11n].includes(decoded.phase2_class) ||
              ![-1n, 0n, 1n].includes(decoded.program_version) ||
              (decoded.program_version === -1n) !==
                (decoded.phase2_class === 5n)
            )
              throw new Error(
                "CEK decoded record has an inconsistent program version or phase-two class",
              );
            return {
              scriptHash: Buffer.from(decoded.script_hash, "hex"),
              languageTag,
              programVersion:
                decoded.program_version === -1n
                  ? -1
                  : decoded.program_version === 0n
                    ? 0
                    : 1,
              termRoot: Buffer.from(decoded.term_root, "hex"),
              phase2Class:
                decoded.phase2_class === 0n
                  ? 0
                  : decoded.phase2_class === 5n
                    ? 5
                    : 11,
            };
          },
        }),
  });
  if (argument.resolverIndex !== 11 || argument.semanticResolverIndex !== 1)
    throw new Error(
      "CEK selection action requires an execution-selection transition",
    );
  return {
    input_index: inputIndex,
    output_index: outputIndex,
    transition: Data.from(
      argument.transitionCbor.toString("hex"),
      SDK.ValidationOneStepWitness,
    ),
    auxiliary: Data.from(
      argument.auxiliaryCbor.toString("hex"),
      SDK.ValidationAuxiliaryWitness,
    ),
    yield_ref_input_indices: yieldReferences.map((utxo) =>
      BigInt(referenceIndex(utxo)),
    ),
    record_reference_input_index: BigInt(recordIndex),
  };
};
