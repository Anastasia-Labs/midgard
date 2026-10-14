import "./validation-machine.v1-purpose-kind-to-redeemer-pointer-mapping.js";

import {
  buildMidgardValidationTraceTree,
  encodeCbor,
  hashMidgardValidationMachineState,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_VALIDATION_DISPUTE_VERSION,
} from "@al-ft/midgard-core";
import { parseExactAikenDataCbor } from "@al-ft/midgard-fault-proofs";
import {
  Application,
  Lambda,
  UPLCEncoder,
  UPLCProgram,
  UPLCVar,
} from "@harmoniclabs/uplc";

import {
  buildMidgardCanonicalCekProgram,
  buildValidationOneStepArgument,
  type DeterministicValidationMachineTrace,
  encodeValidationBoundaryEvidenceCbor,
} from "../src/index.js";
import {
  assertPinnedAuxiliaryEnvelope,
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
        turnStartedAt: 1_800_000_000_000,
        operatorClockMs: 86_400_000,
        challengerClockMs: 86_400_000,
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
    maxArgumentsBytes = Math.max(maxArgumentsBytes, argumentsCbor.length);
    validated.add(auxiliaryKind);
  }
  return { kinds: validated, maxArgumentsBytes };
};

export const context = {
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  eventKeyCbor: encodeCbor([2n, Buffer.alloc(32, 0x41)]),
  sourceKind: "normal" as const,
  blockTimeMs: 1_750_000_000_000,
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
