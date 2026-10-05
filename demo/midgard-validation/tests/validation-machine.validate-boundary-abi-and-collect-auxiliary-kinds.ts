import "./validation-machine.v1-purpose-kind-to-redeemer-pointer-mapping.js";

import {
  buildMidgardValidationTraceTree,
  encodeCbor,
  hashMidgardValidationMachineState,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_VALIDATION_DISPUTE_VERSION,
} from "@al-ft/midgard-core";
import {
  deriveLedgerOutputProofFinalizePlan,
  deriveLedgerOutputProofStepPlan,
  encodeValidationSemanticResolutionRedeemer,
  parseExactAikenDataCbor,
  scriptSourcesDescriptorClaim,
} from "@al-ft/midgard-fault-proofs";
import {
  Application,
  Lambda,
  UPLCEncoder,
  UPLCProgram,
  UPLCVar,
} from "@harmoniclabs/uplc";
import { Constr, Data } from "@lucid-evolution/lucid";

import {
  buildMidgardCanonicalCekProgram,
  buildValidationOneStepArgument,
  type DeterministicValidationMachineTrace,
  encodeValidationBoundaryEvidenceCbor,
} from "../src/index.js";
import {
  assertPinnedAuxiliaryEnvelope,
  semanticResolverDefinitions,
  semanticResolverOffsets,
  spendRedeemerDefinitionName,
  validationDisputeBlueprint,
} from "./validation-machine.semantic-resolver-definitions.js";

export const validateBoundaryAbiAndCollectAuxiliaryKinds = (
  trace: DeterministicValidationMachineTrace,
): {
  readonly kinds: ReadonlySet<string>;
  readonly maxArgumentsBytes: number;
} => {
  const validated = new Set<string>();
  let maxArgumentsBytes = 0;
  for (let lowIndex = 0; lowIndex < trace.states.length - 1; lowIndex += 1) {
    const auxiliaryKind = trace.witnesses[lowIndex]!.auxiliary?.kind ?? "none";
    const highIndex = lowIndex + 1;
    const challengerStates = trace.states.map((state, index) => {
      if (index !== highIndex && index !== trace.states.length - 1) {
        return state;
      }
      const workRoot = Buffer.from(state.workRoot);
      workRoot[0] = workRoot[0]! ^ 0x01;
      return { ...state, workRoot };
    });
    const challengerTree = buildMidgardValidationTraceTree(
      challengerStates.map(hashMidgardValidationMachineState),
      trace.verdict,
      trace.tree.descriptor.rejectionCodeHash,
    );
    const argumentsCbor = encodeValidationBoundaryEvidenceCbor({
      dispute: {
        version: MIDGARD_VALIDATION_DISPUTE_VERSION,
        operatorDescriptor: trace.tree.descriptor,
        challengerDescriptor: challengerTree.descriptor,
        lowIndex,
        highIndex,
        agreedLowHash: hashMidgardValidationMachineState(
          trace.states[lowIndex]!,
        ),
        operatorHighHash: trace.tree.proofs[highIndex]!.stateHash,
        challengerHighHash: challengerTree.proofs[highIndex]!.stateHash,
        round: 1,
        responseDeadline: 1_800_000_000_000,
        turn: { type: "readyForOneStep" },
      },
      operatorTrace: trace,
      challengerTrace: {
        ...trace,
        states: challengerStates,
        tree: challengerTree,
      },
    });
    parseExactAikenDataCbor({
      blueprint: validationDisputeBlueprint,
      definitionName:
        "midgard/validation_resolution_v1/ValidationBoundaryEvidenceV1",
      cbor: argumentsCbor.toString("hex"),
      maxBytes: 16 * 1024 - 1,
    });
    const oneStepArgument = buildValidationOneStepArgument({
      trace,
      stateIndex: lowIndex,
    });
    // #579 regenerated: the blueprint carries `ValidationOneStepWitnessV1`
    // (the transition surface every dispatcher's checked redeemer decode
    // pins) but no definition for `ValidationAuxiliaryWitnessV1` — the
    // auxiliary crosses the wire as `Data` into the yield dispatchers'
    // builtin decodes and never reaches a declared ABI surface. Its gate is
    // the frozen 40-arm envelope pin (`assertPinnedAuxiliaryEnvelope` above)
    // plus the cross-language producer vectors; the transition and evidence
    // envelopes stay blueprint-validated unconditionally.
    parseExactAikenDataCbor({
      blueprint: validationDisputeBlueprint,
      definitionName:
        "midgard/validation_machine/machine_types/ValidationOneStepWitnessV1",
      cbor: oneStepArgument.transitionCbor.toString("hex"),
      maxBytes: 16 * 1024 - 1,
    });
    assertPinnedAuxiliaryEnvelope(oneStepArgument.auxiliaryCbor);
    maxArgumentsBytes = Math.max(
      maxArgumentsBytes,
      oneStepArgument.transitionCbor.length,
      oneStepArgument.auxiliaryCbor.length,
      oneStepArgument.evidenceCbor.length,
    );
    if (
      oneStepArgument.resolverIndex === 8 &&
      oneStepArgument.semanticResolverIndex === 28
    ) {
      // The shared ScriptSources item route (8/28) resolves through a staged
      // multi-validator submission plan (`deriveScriptSourcesItemSubmissionPlan`),
      // not one semantic resolver's `SpendRedeemer`; its per-stage redeemers
      // are pinned by the shared-item plan tests and the item-max emulator
      // lifecycle. The transition and auxiliary envelopes are still validated
      // above like every other step.
      maxArgumentsBytes = Math.max(maxArgumentsBytes, argumentsCbor.length);
      validated.add(auxiliaryKind);
      continue;
    }
    const globalIndex =
      semanticResolverOffsets[oneStepArgument.resolverIndex]! +
      oneStepArgument.semanticResolverIndex;
    const moduleName = semanticResolverDefinitions[globalIndex];
    if (moduleName === undefined) {
      throw new Error(
        `semantic resolver ${globalIndex.toString()} has no ABI definition`,
      );
    }
    // The CEK execution-selection action carries the material route the
    // submitter chose; the direct route is the one every selection in these
    // traces can take (the selection of a native execution names no
    // material and rides `NoCekMaterial`).
    const materialRoute =
      oneStepArgument.resolverIndex === 11 &&
      oneStepArgument.semanticResolverIndex === 1
        ? oneStepArgument.cekRouteMaterial === undefined
          ? ("NoCekMaterial" as const)
          : {
              DirectCekMaterial: {
                envelope_cbor:
                  oneStepArgument.cekRouteMaterial.envelopeCbor.toString("hex"),
                sidecar_cbor:
                  oneStepArgument.cekRouteMaterial.programMaterialSidecarCbor.toString(
                    "hex",
                  ),
              },
            }
        : undefined;
    // The yield-dispatching semantic resolvers refuse to encode without the
    // exact arity of authenticated reference-input indices their proof
    // transaction would carry. This walk validates redeemer ABI shape only —
    // there is no transaction, so it supplies zero-valued indices at the
    // arity each step's own plan demands.
    const yieldInvocationOptions = (() => {
      const resolverIndex = oneStepArgument.resolverIndex;
      const semanticResolverIndex = oneStepArgument.semanticResolverIndex;
      if (resolverIndex === 5 && semanticResolverIndex === 1) {
        // The kind is one integer field either way; the shape is identical.
        return {
          phaseANativeItemInvocation: {
            referenceInputIndex: 0n,
            kind: 0 as const,
          },
        };
      }
      if (
        (resolverIndex === 7 && semanticResolverIndex === 3) ||
        (resolverIndex === 8 && semanticResolverIndex === 2)
      ) {
        const plan = deriveLedgerOutputProofStepPlan({
          resolverIndex,
          semanticResolverIndex,
          transitionCbor: oneStepArgument.transitionCbor,
          auxiliaryCbor: oneStepArgument.auxiliaryCbor,
          ...(oneStepArgument.ledgerOutputProofSuccessorWorkWitnessCbor ===
          undefined
            ? {}
            : {
                ledgerOutputProofSuccessorWorkWitnessCbor:
                  oneStepArgument.ledgerOutputProofSuccessorWorkWitnessCbor,
              }),
        });
        return {
          ledgerOutputProofYieldReferenceInputIndices: Array.from(
            { length: 1 + plan.attestationRoles.length },
            () => 0n,
          ),
        };
      }
      if (
        (resolverIndex === 7 && semanticResolverIndex === 4) ||
        (resolverIndex === 8 && semanticResolverIndex === 3)
      ) {
        const plan = deriveLedgerOutputProofFinalizePlan({
          resolverIndex,
          semanticResolverIndex,
          transitionCbor: oneStepArgument.transitionCbor,
        });
        return {
          ledgerOutputProofYieldReferenceInputIndices: Array.from(
            { length: plan.attachRoles.length },
            () => 0n,
          ),
        };
      }
      if (resolverIndex === 11 && semanticResolverIndex === 1) {
        const auxiliary = Data.from(
          oneStepArgument.auxiliaryCbor.toString("hex"),
        );
        const wide = auxiliary instanceof Constr && auxiliary.fields[1] !== 0n;
        return {
          cekSelectionYieldReferenceInputIndices: Array.from(
            { length: wide ? 4 : 2 },
            () => 0n,
          ),
        };
      }
      if (resolverIndex === 12 && [3, 6, 8].includes(semanticResolverIndex)) {
        return { assetFoldYieldReferenceInputIndex: 0n };
      }
      if (resolverIndex === 8 && semanticResolverIndex === 0) {
        // The kind is one integer field whatever its value; the shape is
        // identical.
        return {
          scriptSourcesMiddleInvocation: { kind: 0, referenceInputIndex: 0n },
        };
      }
      if (resolverIndex === 8 && [19, 21, 22].includes(semanticResolverIndex)) {
        const auxiliary = Data.from(
          oneStepArgument.auxiliaryCbor.toString("hex"),
        );
        if (auxiliary instanceof Constr && auxiliary.index === 18) {
          // The claim is a pure function of the step's own evidence.
          return {
            scriptSourcesDescriptorInvocation: {
              claim: scriptSourcesDescriptorClaim(auxiliary),
              referenceInputIndex: 0n,
            },
          };
        }
        return {};
      }
      if (resolverIndex === 8 && semanticResolverIndex === 25) {
        return {
          scriptSourcesObserverInvocation: {
            observerHash: "00".repeat(28),
            activeCount: 1n,
            indices: [0n, 0n],
          },
        };
      }
      return {};
    })();
    const semanticRedeemer = (() => {
      try {
        return encodeValidationSemanticResolutionRedeemer({
          oneStepArgument,
          inputIndex: 0n,
          outputIndex: 0n,
          ...(materialRoute === undefined ? {} : { materialRoute }),
          ...yieldInvocationOptions,
        });
      } catch (error) {
        const auxiliary = Data.from(
          oneStepArgument.auxiliaryCbor.toString("hex"),
        );
        throw new Error(
          `semantic redeemer for resolver ${oneStepArgument.resolverIndex.toString()}/${oneStepArgument.semanticResolverIndex.toString()} (auxiliary constructor ${auxiliary instanceof Constr ? auxiliary.index.toString() : "none"}): ${error instanceof Error ? error.message : String(error)}`,
        );
      }
    })();
    parseExactAikenDataCbor({
      blueprint: validationDisputeBlueprint,
      definitionName: spendRedeemerDefinitionName(moduleName),
      cbor: semanticRedeemer.toString("hex"),
      maxBytes: 16 * 1024 - 1,
    });
    maxArgumentsBytes = Math.max(maxArgumentsBytes, semanticRedeemer.length);
    maxArgumentsBytes = Math.max(maxArgumentsBytes, argumentsCbor.length);
    validated.add(auxiliaryKind);
  }
  return { kinds: validated, maxArgumentsBytes };
};

export const context = {
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  eventKeyCbor: encodeCbor([2n, Buffer.alloc(32, 0x41)]),
  sourceKind: "normal" as const,
  blockEndTimeMs: 1_750_000_000_000,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  blockSlot: 100n,
  ledgerMutationSteps: [],
};

export const buildAcceptingIdentityProgram = () =>
  buildMidgardCanonicalCekProgram(
    Buffer.from(
      UPLCEncoder.compile(
        new UPLCProgram([1, 1, 0], new Lambda(new UPLCVar(0))),
      ),
    ),
  );

export const buildNonterminatingSelfApplicationProgram = () => {
  const selfApplication = new Lambda(
    new Application(new UPLCVar(0), new UPLCVar(0)),
  );
  return buildMidgardCanonicalCekProgram(
    Buffer.from(
      UPLCEncoder.compile(
        new UPLCProgram(
          [1, 1, 0],
          new Application(selfApplication, selfApplication),
        ),
      ),
    ),
  );
};
