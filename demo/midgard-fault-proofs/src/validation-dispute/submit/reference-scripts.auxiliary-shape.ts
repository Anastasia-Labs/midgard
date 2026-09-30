import { Constr } from "@lucid-evolution/lucid";

import { type PlutusDataValue, requireConstr } from "./evidence.js";
import {
  hasValidationAuxiliaryShape,
  VALIDATION_AUXILIARY_SHAPES,
  VALIDATION_CEK_CONTEXT_STEP_AUXILIARY_SHAPES,
  VALIDATION_SEMANTIC_RESOLVER_OFFSETS,
  VALIDATION_VALUE_AND_MINT_AUXILIARY_SHAPES,
} from "./reference-scripts.validation-auxiliary-shapes.js";

export const auxiliaryShape = ({
  resolverIndex,
  semanticResolverIndex,
  auxiliary,
}: {
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly auxiliary: PlutusDataValue;
}): Constr<PlutusDataValue> => {
  if (resolverIndex === 0) {
    if (semanticResolverIndex === 0) {
      return requireConstr({
        value: auxiliary,
        index: VALIDATION_AUXILIARY_SHAPES.none[0],
        fields: VALIDATION_AUXILIARY_SHAPES.none[1],
        label: "validation CanonicalDecode empty auxiliary witness",
      });
    }
    if (
      auxiliary instanceof Constr &&
      (hasValidationAuxiliaryShape(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES.transactionFieldChunk,
      ) ||
        hasValidationAuxiliaryShape(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES.transactionFieldItem,
        ))
    ) {
      return auxiliary;
    }
    throw new Error(
      "validation CanonicalDecode auxiliary witness must carry an authenticated chunk or complete item",
    );
  }
  if (resolverIndex === 13) {
    const expected =
      semanticResolverIndex === 2 ||
      semanticResolverIndex === 4 ||
      semanticResolverIndex === 6 ||
      semanticResolverIndex === 7
        ? VALIDATION_AUXILIARY_SHAPES.none
        : semanticResolverIndex === 0
          ? VALIDATION_AUXILIARY_SHAPES.ledgerDeltaOperation
          : semanticResolverIndex === 1
            ? VALIDATION_AUXILIARY_SHAPES.ledgerDeltaReplay
            : semanticResolverIndex === 3
              ? VALIDATION_AUXILIARY_SHAPES.ledgerDeltaOutput
              : VALIDATION_AUXILIARY_SHAPES.ledgerDeltaProofFrame;
    return requireConstr({
      value: auxiliary,
      index: expected[0],
      fields: expected[1],
      label: "validation LedgerDelta auxiliary witness",
    });
  }
  if (resolverIndex === 7) {
    const expected =
      semanticResolverIndex === 0 || semanticResolverIndex === 1
        ? VALIDATION_AUXILIARY_SHAPES.none
        : semanticResolverIndex === 2
          ? VALIDATION_AUXILIARY_SHAPES.scheduledLedgerMembership
          : semanticResolverIndex === 3
            ? VALIDATION_AUXILIARY_SHAPES.ledgerOutputProofStep
            : semanticResolverIndex === 4
              ? VALIDATION_AUXILIARY_SHAPES.ledgerOutputProofFinalize
              : semanticResolverIndex === 5
                ? VALIDATION_AUXILIARY_SHAPES.scheduledLedgerNonMembership
                : VALIDATION_AUXILIARY_SHAPES.resolvedInputReplay;
    return requireConstr({
      value: auxiliary,
      index: expected[0],
      fields: expected[1],
      label: "validation ResolveInputs auxiliary witness",
    });
  }
  if (resolverIndex === 8) {
    if (!(auxiliary instanceof Constr)) {
      throw new Error("validation auxiliary witness must be a constructor");
    }
    const isRedeemerItemStage = hasValidationAuxiliaryShape(
      auxiliary,
      VALIDATION_AUXILIARY_SHAPES.redeemerItemStep,
    );
    if (semanticResolverIndex === 28 && !isRedeemerItemStage) {
      throw new Error(
        "validation auxiliary witness does not match the selected ScriptSources split stage-one proof family",
      );
    }
    if (
      semanticResolverIndex === 15 &&
      !hasValidationAuxiliaryShape(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES.transactionRedeemerItemBegin,
      )
    ) {
      throw new Error(
        "validation auxiliary witness does not match the selected ScriptSources redeemer-ingestion proof family",
      );
    }
    if (
      (semanticResolverIndex === 19 ||
        semanticResolverIndex === 21 ||
        semanticResolverIndex === 22) &&
      !(
        hasValidationAuxiliaryShape(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES.redeemerScanBegin,
        ) || isRedeemerItemStage
      )
    ) {
      throw new Error(
        "validation auxiliary witness does not match the selected ScriptSources redeemer-scan proof family",
      );
    }
    const outputExpected =
      semanticResolverIndex === 0
        ? null
        : semanticResolverIndex === 1
          ? VALIDATION_AUXILIARY_SHAPES.ledgerOutputProofBegin
          : semanticResolverIndex === 2
            ? VALIDATION_AUXILIARY_SHAPES.ledgerOutputProofStep
            : semanticResolverIndex === 3
              ? VALIDATION_AUXILIARY_SHAPES.ledgerOutputProofFinalize
              : semanticResolverIndex === 5
                ? VALIDATION_AUXILIARY_SHAPES.transactionFieldChunk
                : semanticResolverIndex === 7
                  ? VALIDATION_AUXILIARY_SHAPES.scriptSourceHashBlock
                  : semanticResolverIndex >= 10 && semanticResolverIndex <= 12
                    ? VALIDATION_AUXILIARY_SHAPES.scriptSourceScan
                    : semanticResolverIndex === 17
                      ? VALIDATION_AUXILIARY_SHAPES.scriptSourceScan
                      : semanticResolverIndex === 19
                        ? null
                        : semanticResolverIndex === 21 ||
                            semanticResolverIndex === 22
                          ? null
                          : semanticResolverIndex === 24
                            ? VALIDATION_AUXILIARY_SHAPES.scriptPurposeScan
                            : semanticResolverIndex === 25
                              ? VALIDATION_AUXILIARY_SHAPES.transactionFieldChunk
                              : semanticResolverIndex === 26
                                ? VALIDATION_AUXILIARY_SHAPES.scriptPurposeScan
                                : semanticResolverIndex === 15 ||
                                    semanticResolverIndex === 28
                                  ? null
                                  : VALIDATION_AUXILIARY_SHAPES.none;
    const outputAuxiliary = auxiliary;
    if (
      outputExpected !== null &&
      (outputAuxiliary.index !== outputExpected[0] ||
        outputAuxiliary.fields.length !== outputExpected[1])
    ) {
      throw new Error(
        "validation auxiliary witness does not match the selected ScriptSources proof family",
      );
    }
    return outputAuxiliary;
  }
  if (resolverIndex === 11) {
    if (semanticResolverIndex === 2) {
      if (
        auxiliary instanceof Constr &&
        VALIDATION_CEK_CONTEXT_STEP_AUXILIARY_SHAPES.some((shape) =>
          hasValidationAuxiliaryShape(auxiliary, shape),
        )
      ) {
        return auxiliary;
      }
      throw new Error(
        "validation Cek context-step auxiliary witness must carry a cek context witness or no auxiliary",
      );
    }
    const expected =
      semanticResolverIndex === 0
        ? VALIDATION_AUXILIARY_SHAPES.none
        : semanticResolverIndex === 1
          ? VALIDATION_AUXILIARY_SHAPES.nativeExecutionScan
          : VALIDATION_AUXILIARY_SHAPES.cekCoreStep;
    return requireConstr({
      value: auxiliary,
      index: expected[0],
      fields: expected[1],
      label: "validation Cek auxiliary witness",
    });
  }
  if (resolverIndex === 12) {
    const expected =
      VALIDATION_VALUE_AND_MINT_AUXILIARY_SHAPES[semanticResolverIndex];
    if (expected === undefined) {
      throw new Error(
        "validation ValueAndMint semantic resolver index is out of range",
      );
    }
    return requireConstr({
      value: auxiliary,
      index: expected[0],
      fields: expected[1],
      label: "validation ValueAndMint auxiliary witness",
    });
  }
  if (resolverIndex === 9) {
    const expected =
      semanticResolverIndex === 0
        ? VALIDATION_AUXILIARY_SHAPES.none
        : VALIDATION_AUXILIARY_SHAPES.nativeExecutionDescriptor;
    return requireConstr({
      value: auxiliary,
      index: expected[0],
      fields: expected[1],
      label: "validation NativeScripts auxiliary witness",
    });
  }
  const expected =
    resolverIndex === 1 || resolverIndex === 2 || resolverIndex === 10
      ? VALIDATION_AUXILIARY_SHAPES.none
      : resolverIndex === 3
        ? semanticResolverIndex === 0
          ? VALIDATION_AUXILIARY_SHAPES.none
          : VALIDATION_AUXILIARY_SHAPES.transactionFieldChunk
        : resolverIndex === 4
          ? semanticResolverIndex === 0 || semanticResolverIndex === 3
            ? VALIDATION_AUXILIARY_SHAPES.none
            : semanticResolverIndex === 1
              ? VALIDATION_AUXILIARY_SHAPES.transactionFieldChunk
              : VALIDATION_AUXILIARY_SHAPES.requiredSignerItem
          : resolverIndex === 5
            ? semanticResolverIndex === 0
              ? VALIDATION_AUXILIARY_SHAPES.none
              : semanticResolverIndex === 1
                ? VALIDATION_AUXILIARY_SHAPES.transactionFieldChunk
                : semanticResolverIndex === 13
                  ? VALIDATION_AUXILIARY_SHAPES.nativeScriptFrame
                  : VALIDATION_AUXILIARY_SHAPES.nativeScriptToken
            : resolverIndex === 6
              ? semanticResolverIndex === 0
                ? VALIDATION_AUXILIARY_SHAPES.none
                : VALIDATION_AUXILIARY_SHAPES.transactionFieldChunk
              : null;
  if (expected === null) {
    throw new Error(
      `Validation resolver ${resolverIndex.toString()} has no staged semantic proof family`,
    );
  }
  return requireConstr({
    value: auxiliary,
    index: expected[0],
    fields: expected[1],
    label: "validation auxiliary witness",
  });
};

export const validationSemanticResolverGlobalIndex = (
  resolverIndex: number,
  semanticResolverIndex: number,
): number =>
  resolverIndex === 8 && semanticResolverIndex === 28
    ? 90
    : VALIDATION_SEMANTIC_RESOLVER_OFFSETS[resolverIndex]! +
      semanticResolverIndex;
