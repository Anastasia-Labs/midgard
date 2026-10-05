import {
  buildMidgardValidationTraceTree,
  encodeCbor,
  hashMidgardValidationMachineState,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
} from "@al-ft/midgard-core";
import { MIDGARD_CHUNK_BYTES_K } from "@al-ft/midgard-core/codec/native-tx-field-access";
import { validationTraceDescriptorDataFromCore } from "@al-ft/midgard-sdk";
import { validationSemanticResolverIndex } from "@al-ft/midgard-validation";

import { buildForcedValidationDisputeCommitments } from "./emulator/validation-dispute-fixtures.build-forced-validation-dispute-commitments.js";
import { buildNativeTransactionTrace } from "./emulator/validation-dispute-fixtures.build-native-transaction-trace.js";

/** Canonical 1,010 hashes whose first two exact carriage chunks coincide. */
export const repeatedRequiredSignerHashes = (): readonly string[] => {
  const count = 1010;
  const period = Buffer.alloc(MIDGARD_CHUNK_BYTES_K, 0xa5);
  const constraints = new Map<number, number>();
  const pin = (offset: number, byte: number) => {
    const position = offset % period.length;
    const previous = constraints.get(position);
    if (previous !== undefined && previous !== byte)
      throw new Error(
        "repeated signer field has conflicting canonical wrappers",
      );
    constraints.set(position, byte);
    period[position] = byte;
  };
  [0x99, 0x03, 0xf2].forEach((byte, index) => pin(index, byte));
  for (let index = 0; index < count; index++) {
    pin(3 + index * 30, 0x58);
    pin(4 + index * 30, 0x1c);
  }
  const field = Buffer.concat([period, period, period.subarray(0, 7)]);
  const hashes = Array.from({ length: count }, (_, index) =>
    field.subarray(5 + index * 30, 33 + index * 30).toString("hex"),
  );
  if (!encodeCbor(hashes.map((hash) => Buffer.from(hash, "hex"))).equals(field))
    throw new Error(
      "repeated signer field is not the exact canonical hash list",
    );
  return hashes;
};

/** Builds only the committed trace and replay input; carriage is produced by the installed workflow. */
export const buildInstalledSignatureFixture = async ({
  operatorVkey,
  now,
  repeatedRequiredSigners = false,
}: {
  readonly operatorVkey: string;
  readonly now: number;
  readonly repeatedRequiredSigners?: boolean;
}) => {
  const native = await buildNativeTransactionTrace({
    now,
    addressWitnessCount: repeatedRequiredSigners ? 1 : 318,
    requiredSignerHashes: repeatedRequiredSigners
      ? repeatedRequiredSignerHashes()
      : [],
    txOrderSeed: "e5",
  });
  const challengerTrace = native.honestTrace;
  const disputedLowIndex = challengerTrace.states.findIndex(
    (state, index) =>
      state.phase === "signatures" &&
      challengerTrace.witnesses[index]?.auxiliary?.kind ===
        (repeatedRequiredSigners
          ? "requiredSignerItem"
          : "transactionFieldChunk"),
  );
  if (disputedLowIndex < 0)
    throw new Error(
      "signature fixture omitted the actual address witness step",
    );
  const forgedTerminal = {
    ...challengerTrace.states.at(-1)!,
    verdict: "accepted" as const,
    rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
    workRoot: Buffer.alloc(32, 0x7e),
  };
  const states = challengerTrace.states.map((state, index) =>
    index <= disputedLowIndex ? state : forgedTerminal,
  );
  const operatorTrace = {
    ...challengerTrace,
    verdict: "accepted" as const,
    rejectionCode: null,
    states,
    tree: buildMidgardValidationTraceTree(
      states.map(hashMidgardValidationMachineState),
      "accepted",
      MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
    ),
  };
  const { header, claim } = await buildForcedValidationDisputeCommitments({
    ...native,
    operatorVkey,
    now,
    operatorTrace,
  });
  return {
    header,
    claim,
    operatorTrace,
    challengerTrace,
    disputedLowIndex,
    challengerReplayInput: native.challengerReplayInput,
    challengerDescriptor: validationTraceDescriptorDataFromCore(
      challengerTrace.tree.descriptor,
    ),
    // Executor selection is derived from the authentic witness. No retained
    // onchain auxiliary or reference-input index exists before publication.
    evidence: {
      oneStepArgument: {
        resolverIndex: 4,
        semanticResolverIndex: validationSemanticResolverIndex(
          challengerTrace.witnesses[disputedLowIndex]!,
        ),
      },
    },
  };
};
