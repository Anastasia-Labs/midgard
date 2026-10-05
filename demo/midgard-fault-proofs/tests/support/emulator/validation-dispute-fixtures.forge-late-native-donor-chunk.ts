import {
  buildMidgardBoundedItem,
  buildMidgardBoundedItemChunkProof,
  buildMidgardValidationTraceTree,
  hashMidgardValidationMachineState,
  hashMidgardValidationWorkWitness,
} from "@al-ft/midgard-core";
import { validationTraceDescriptorDataFromCore } from "@al-ft/midgard-sdk";
import {
  buildValidationDisputeEvidenceBundle,
  type DeterministicValidationMachineTrace,
} from "@al-ft/midgard-validation";

import { type ForcedValidationDisputeFixture } from "./validation-dispute-fixtures.build-accepted-claim-over-rejecting-transaction-fixture.js";

/**
 * The challenger's forged late native token head: the disputed step opens a
 * chunk of another native execution's script item, and the successor is
 * exactly the one those bytes derive. The donor item is padded to the late
 * item's length and proved against its own commitment at its own coordinate,
 * so the chunk's total length and index match the late control and the rest
 * of the step is the honest one. Only the chunk's authentication against the
 * late control's item commitment can refuse it. Every later state carries a
 * fabricated work root.
 */
const forgeLateNativeDonorChunk = ({
  trace,
  disputedLowIndex,
}: {
  readonly trace: DeterministicValidationMachineTrace;
  readonly disputedLowIndex: number;
}): DeterministicValidationMachineTrace => {
  const adjacent = trace.witnesses[disputedLowIndex]!;
  const auxiliary = adjacent.auxiliary;
  if (auxiliary?.kind !== "nativeScriptToken")
    throw new Error("disputed late native state does not open a token head");
  const own = auxiliary.chunkProof;
  const genuine = trace.witnesses
    .flatMap((witness) =>
      witness.auxiliary?.kind === "nativeExecutionDescriptor" &&
      witness.auxiliary.firstChunkProof !== null
        ? [witness.auxiliary.firstChunkProof]
        : [],
    )
    .find(
      (proof) =>
        proof.fieldIndex !== own.fieldIndex ||
        proof.itemIndex !== own.itemIndex,
    );
  if (
    genuine === undefined ||
    genuine.chunkIndex !== 0 ||
    genuine.totalLength >= own.totalLength ||
    own.chunkIndex !== 0
  )
    throw new Error("no shorter native script item can donate a chunk");
  const donor = buildMidgardBoundedItemChunkProof(
    buildMidgardBoundedItem({
      fieldIndex: genuine.fieldIndex,
      itemIndex: genuine.itemIndex,
      bytes: Buffer.concat([
        genuine.chunk,
        Buffer.alloc(own.totalLength - genuine.totalLength),
      ]),
    }),
    0,
  );
  const successorIndex = disputedLowIndex + 1;
  const successor = trace.witnesses[successorIndex]!;
  // The honest token head step rewrites exactly three small integers of the
  // control in place: the stage, the cursor and the node count.
  const changed = [...adjacent.cbor.keys()].filter(
    (index) => adjacent.cbor[index] !== successor.cbor[index],
  );
  const [stageAt, cursorAt] = changed;
  if (
    adjacent.cbor.length !== successor.cbor.length ||
    changed.length !== 3 ||
    stageAt === undefined ||
    cursorAt === undefined ||
    adjacent.cbor[stageAt] !== 0x01 ||
    adjacent.cbor[cursorAt]! > 0x15
  )
    throw new Error(
      "late token head step does not rewrite stage, cursor and node count in place",
    );
  // The token head the donor's bytes hold at the pre-state cursor: a two-item
  // array head and its small tag, so the successor's cursor and node count
  // are the honest ones and only its stage differs.
  const cursor = adjacent.cbor[cursorAt]!;
  const tag = donor.chunk[cursor + 1];
  if (
    donor.chunk[cursor] !== 0x82 ||
    tag === undefined ||
    tag > 5 ||
    tag === 3 ||
    tag + 3 === successor.cbor[stageAt]
  )
    throw new Error(
      "donor chunk holds no distinct two-item token head at the cursor",
    );
  const cbor = Buffer.from(successor.cbor);
  cbor[stageAt] = tag + 3;
  const witnesses = [...trace.witnesses];
  witnesses[disputedLowIndex] = {
    ...adjacent,
    auxiliary: { ...auxiliary, chunkProof: donor, nextChunkProof: null },
  };
  witnesses[successorIndex] = { ...successor, cbor };
  const states = trace.states.map((state, index) =>
    index < successorIndex
      ? state
      : index === successorIndex
        ? {
            ...state,
            workRoot: hashMidgardValidationWorkWitness({
              phase: successor.phase,
              programCounter: successor.programCounter,
              witnessCbor: cbor,
            }),
          }
        : { ...state, workRoot: Buffer.alloc(32, 0x7d) },
  );
  return {
    ...trace,
    states,
    witnesses,
    tree: buildMidgardValidationTraceTree(
      states.map(hashMidgardValidationMachineState),
      trace.verdict,
      trace.states.at(-1)!.rejectionCodeHash,
    ),
  };
};

/**
 * Replace the challenger's claim of a dishonest-challenger late native
 * fixture with the donor-chunk forgery of the honest trace. The header and
 * claim commit to the operator's honest trace only, so they are kept; the
 * descriptor and evidence are rebuilt over the new challenger claim.
 */
export const withLateNativeDonorChunk = <
  Fixture extends ForcedValidationDisputeFixture & {
    readonly disputedLowIndex: number;
  },
>(
  fixture: Fixture,
  now: number,
): Fixture => {
  const challengerTrace = forgeLateNativeDonorChunk({
    trace: fixture.operatorTrace,
    disputedLowIndex: fixture.disputedLowIndex,
  });
  return {
    ...fixture,
    challengerTrace,
    challengerDescriptor: validationTraceDescriptorDataFromCore(
      challengerTrace.tree.descriptor,
    ),
    evidence: buildValidationDisputeEvidenceBundle({
      operatorTrace: fixture.operatorTrace,
      challengerTrace,
      currentTime: now + 2_000,
    }),
  };
};
