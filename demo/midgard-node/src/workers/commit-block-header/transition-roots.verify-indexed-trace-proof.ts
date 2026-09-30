import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  MpfError,
  verifyKeyValuePhasMembershipProof,
  verifyKeyValuePhasNonMembershipProof,
} from "../../mpf/index.js";
import {
  buildAuthenticatedRootFromDataEntries,
  buildRootMembershipProof,
  buildRootNonMembershipProof,
  type BuiltAuthenticatedRoot,
  type BuiltTypedAuthenticatedRoot,
  type DataSchema,
  encodeData,
  type RootProofVerificationOptions,
  type TypedRootEntry,
  validateCountProof,
} from "./transition-roots.validate-count-proof.js";

export const verifyRootMembershipProof = <K, V>({
  witness,
  keySchema,
  valueSchema,
  options,
}: {
  readonly witness: SDK.RootMembershipProof<K, V>;
  readonly keySchema: DataSchema;
  readonly valueSchema: DataSchema;
  readonly options: RootProofVerificationOptions;
}): Effect.Effect<void, MpfError, never> =>
  Effect.gen(function* () {
    yield* validateCountProof(witness, options);
    if (witness.count === 0n) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error("Membership proof cannot target an empty root"),
        ),
      );
    }
    const keyBytes = yield* encodeData(
      witness.key,
      keySchema,
      "membership witness key",
    );
    const valueBytes = yield* encodeData(
      witness.value,
      valueSchema,
      "membership witness value",
    );
    yield* verifyKeyValuePhasMembershipProof({
      root: witness.phas_root,
      key: keyBytes,
      value: valueBytes,
      proof: witness.proof,
    });
  });

export const verifyRootNonMembershipProof = <K>({
  witness,
  keySchema,
  options,
}: {
  readonly witness: SDK.RootNonMembershipProof<K>;
  readonly keySchema: DataSchema;
  readonly options: RootProofVerificationOptions;
}): Effect.Effect<void, MpfError, never> =>
  Effect.gen(function* () {
    yield* validateCountProof(witness, options);
    const keyBytes = yield* encodeData(
      witness.key,
      keySchema,
      "non-membership witness key",
    );
    yield* verifyKeyValuePhasNonMembershipProof({
      root: witness.phas_root,
      key: keyBytes,
      proof: witness.proof,
    });
  });

const transitionTraceEntries = (
  steps: readonly SDK.TransitionStep[],
): Effect.Effect<
  readonly TypedRootEntry<bigint, SDK.TransitionStep>[],
  MpfError
> =>
  Effect.gen(function* () {
    const ordered = [...steps].sort((left, right) => {
      if (left.step_index < right.step_index) {
        return -1;
      }
      if (left.step_index > right.step_index) {
        return 1;
      }
      return 0;
    });
    for (const [index, step] of ordered.entries()) {
      if (step.schema_version !== SDK.TRANSITION_STEP_SCHEMA_VERSION) {
        return yield* Effect.fail(
          MpfError.phasRoot(
            new Error(
              `TransitionStepV1 schema_version must equal ${SDK.TRANSITION_STEP_SCHEMA_VERSION.toString()} at sorted index ${index.toString()}; got=${step.schema_version.toString()}`,
            ),
          ),
        );
      }
      if (step.step_index !== BigInt(index)) {
        return yield* Effect.fail(
          MpfError.phasRoot(
            new Error(
              `Transition trace must be densely indexed from zero: expected=${index.toString()},actual=${step.step_index.toString()}`,
            ),
          ),
        );
      }
    }
    return ordered.map((step) => ({
      key: step.step_index,
      value: step,
    }));
  });

export const buildTransitionTraceRoot = (
  steps: readonly SDK.TransitionStep[],
): Effect.Effect<
  BuiltTypedAuthenticatedRoot<bigint, SDK.TransitionStep>,
  MpfError,
  never
> =>
  Effect.gen(function* () {
    const entries = yield* transitionTraceEntries(steps);
    return yield* buildAuthenticatedRootFromDataEntries({
      domain: SDK.ROOT_DOMAINS.transitionTrace,
      entries,
      keySchema: LucidData.Integer(),
      valueSchema: SDK.TransitionStepSchema,
    });
  });

export const buildIndexedTraceProof = ({
  root,
  stepIndex,
}: {
  readonly root: BuiltTypedAuthenticatedRoot<bigint, SDK.TransitionStep>;
  readonly stepIndex: bigint;
}): Effect.Effect<SDK.IndexedTraceProof, MpfError, never> =>
  Effect.gen(function* () {
    const entry = root.typedEntries.find((item) => item.key === stepIndex);
    if (entry === undefined) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(
            `Cannot build indexed trace membership proof for absent step_index ${stepIndex.toString()}`,
          ),
        ),
      );
    }
    return yield* buildRootMembershipProof({
      root,
      key: entry.key,
      value: entry.value,
      keySchema: LucidData.Integer(),
      valueSchema: SDK.TransitionStepSchema,
    });
  });

export const buildAdjacentTraceProof = ({
  root,
  lowerStepIndex,
}: {
  readonly root: BuiltTypedAuthenticatedRoot<bigint, SDK.TransitionStep>;
  readonly lowerStepIndex: bigint;
}): Effect.Effect<SDK.AdjacentTraceProof, MpfError, never> =>
  Effect.gen(function* () {
    const lower = yield* buildIndexedTraceProof({
      root,
      stepIndex: lowerStepIndex,
    });
    const upper = yield* buildIndexedTraceProof({
      root,
      stepIndex: lowerStepIndex + 1n,
    });
    return { lower, upper };
  });

export const verifyIndexedTraceProof = (
  witness: SDK.IndexedTraceProof,
  options: Omit<RootProofVerificationOptions, "expectedDomain">,
): Effect.Effect<void, MpfError, never> =>
  Effect.gen(function* () {
    if (witness.value.schema_version !== SDK.TRANSITION_STEP_SCHEMA_VERSION) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(
            `Indexed trace proof TransitionStepV1 schema_version must equal ${SDK.TRANSITION_STEP_SCHEMA_VERSION.toString()}; got=${witness.value.schema_version.toString()}`,
          ),
        ),
      );
    }
    yield* verifyRootMembershipProof({
      witness,
      keySchema: LucidData.Integer(),
      valueSchema: SDK.TransitionStepSchema,
      options: {
        ...options,
        expectedDomain: SDK.ROOT_DOMAINS.transitionTrace,
      },
    });
    if (witness.key !== witness.value.step_index) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(
            `Indexed trace proof key must equal step.step_index: key=${witness.key.toString()},step=${witness.value.step_index.toString()}`,
          ),
        ),
      );
    }
    if (witness.key < 0n || witness.key >= witness.count) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(
            `Indexed trace proof key out of committed range: key=${witness.key.toString()},count=${witness.count.toString()}`,
          ),
        ),
      );
    }
  });

export const verifyAdjacentTraceProof = (
  witness: SDK.AdjacentTraceProof,
  options: Omit<RootProofVerificationOptions, "expectedDomain">,
): Effect.Effect<void, MpfError, never> =>
  Effect.gen(function* () {
    yield* verifyIndexedTraceProof(witness.lower, options);
    yield* verifyIndexedTraceProof(witness.upper, options);
    if (
      witness.lower.root !== witness.upper.root ||
      witness.lower.count !== witness.upper.count
    ) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error("Adjacent trace proofs must target the same root/count"),
        ),
      );
    }
    if (witness.upper.key !== witness.lower.key + 1n) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(
            `Adjacent trace proof keys must be i and i + 1: lower=${witness.lower.key.toString()},upper=${witness.upper.key.toString()}`,
          ),
        ),
      );
    }
  });

export const buildEventToStepRoot = (
  entries: readonly TypedRootEntry<SDK.EventKey, SDK.EventToStepValue>[],
): Effect.Effect<
  BuiltTypedAuthenticatedRoot<SDK.EventKey, SDK.EventToStepValue>,
  MpfError,
  never
> =>
  buildAuthenticatedRootFromDataEntries({
    domain: SDK.ROOT_DOMAINS.eventToStep,
    entries,
    keySchema: SDK.EventKeySchema,
    valueSchema: SDK.EventToStepValueSchema,
  });

export const buildEventToStepMembershipProof = ({
  root,
  eventKey,
  value,
}: {
  readonly root: BuiltTypedAuthenticatedRoot<
    SDK.EventKey,
    SDK.EventToStepValue
  >;
  readonly eventKey: SDK.EventKey;
  readonly value: SDK.EventToStepValue;
}): Effect.Effect<SDK.EventToStepProof, MpfError, never> =>
  Effect.gen(function* () {
    const membership = yield* buildRootMembershipProof({
      root,
      key: eventKey,
      value,
      keySchema: SDK.EventKeySchema,
      valueSchema: SDK.EventToStepValueSchema,
    });
    return { EventToStepMembership: { membership } };
  });

export const buildEventToStepNonMembershipProof = ({
  root,
  eventKey,
}: {
  readonly root: BuiltAuthenticatedRoot;
  readonly eventKey: SDK.EventKey;
}): Effect.Effect<SDK.EventToStepProof, MpfError, never> =>
  Effect.gen(function* () {
    const nonMembership = yield* buildRootNonMembershipProof({
      root,
      key: eventKey,
      keySchema: SDK.EventKeySchema,
    });
    return {
      EventToStepNonMembership: { non_membership: nonMembership },
    };
  });
