import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type KeyValuePhasEntry,
  keyValuePhasNonMembershipProof,
  keyValuePhasProof,
  type KeyValuePhasRoot,
  keyValuePhasRootWithCount,
  MpfError,
  verifyKeyValuePhasMembershipProof,
  verifyKeyValuePhasNonMembershipProof,
} from "@/workers/utils/mpf.js";

type DataSchema = Parameters<typeof LucidData.Nullable>[0];

export type EncodedRootEntry = KeyValuePhasEntry;

export type BuiltAuthenticatedRoot = KeyValuePhasRoot & {
  readonly domain: SDK.RootDomain;
  readonly phasRoot: string;
};

export type TypedRootEntry<K, V> = {
  readonly key: K;
  readonly value: V;
};

export type BuiltTypedAuthenticatedRoot<K, V> = BuiltAuthenticatedRoot & {
  readonly typedEntries: readonly TypedRootEntry<K, V>[];
};

export type RootProofVerificationOptions = {
  readonly expectedDomain: SDK.RootDomain;
  readonly expectedRoot?: string;
  readonly expectedCount?: bigint;
};

const encodeData = <A>(
  value: A,
  schema: DataSchema,
  label: string,
): Effect.Effect<Buffer, MpfError, never> =>
  Effect.try({
    try: () =>
      Buffer.from(LucidData.to(value as never, schema as never), "hex"),
    catch: (cause) =>
      MpfError.phasRoot(
        new Error(`Failed to encode ${label} as canonical Plutus data`, {
          cause,
        }),
      ),
  });

const rootEntryVectors = (entries: readonly EncodedRootEntry[]) => ({
  keys: entries.map((entry) => entry.key),
  values: entries.map((entry) => entry.value),
});

export const buildAuthenticatedRootFromEncodedEntries = (
  domain: SDK.RootDomain,
  entries: readonly EncodedRootEntry[],
): Effect.Effect<BuiltAuthenticatedRoot, MpfError, never> =>
  Effect.gen(function* () {
    const { keys, values } = rootEntryVectors(entries);
    const phas = yield* keyValuePhasRootWithCount(keys, values);
    const root = yield* SDK.commitCountedRootProgram({
      domain,
      phasRoot: phas.root,
      count: phas.count,
    }).pipe(
      Effect.mapError((cause) =>
        MpfError.phasRoot(
          new Error("Failed to commit authenticated root count", { cause }),
        ),
      ),
    );
    return {
      root,
      phasRoot: phas.root,
      count: phas.count,
      entries: phas.entries,
      domain,
    };
  });

export const buildAuthenticatedRootFromDataEntries = <K, V>({
  domain,
  entries,
  keySchema,
  valueSchema,
}: {
  readonly domain: SDK.RootDomain;
  readonly entries: readonly TypedRootEntry<K, V>[];
  readonly keySchema: DataSchema;
  readonly valueSchema: DataSchema;
}): Effect.Effect<BuiltTypedAuthenticatedRoot<K, V>, MpfError, never> =>
  Effect.gen(function* () {
    const encodedEntries = yield* Effect.forEach(entries, (entry, index) =>
      Effect.gen(function* () {
        const key = yield* encodeData(
          entry.key,
          keySchema,
          `root key at index ${index.toString()}`,
        );
        const value = yield* encodeData(
          entry.value,
          valueSchema,
          `root value at index ${index.toString()}`,
        );
        return { key, value };
      }),
    );
    const built = yield* buildAuthenticatedRootFromEncodedEntries(
      domain,
      encodedEntries,
    );
    return {
      ...built,
      typedEntries: entries,
    };
  });

const validateCountProof = (
  proof: SDK.RootCountProof,
  options: RootProofVerificationOptions,
): Effect.Effect<void, MpfError, never> =>
  Effect.gen(function* () {
    if (proof.domain !== options.expectedDomain) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(
            `Root proof domain mismatch: expected=${options.expectedDomain},actual=${proof.domain}`,
          ),
        ),
      );
    }
    if (proof.count < 0n) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(`Root proof count must be non-negative: ${proof.count}`),
        ),
      );
    }
    if (
      (proof.phas_root === SDK.EMPTY_MERKLE_TREE_ROOT && proof.count !== 0n) ||
      (proof.phas_root !== SDK.EMPTY_MERKLE_TREE_ROOT && proof.count === 0n)
    ) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(
            `PHAS root/count emptiness mismatch: phas_root=${proof.phas_root},count=${proof.count.toString()}`,
          ),
        ),
      );
    }
    const committedRoot = yield* SDK.commitCountedRootProgram({
      domain: proof.domain,
      phasRoot: proof.phas_root,
      count: proof.count,
    }).pipe(
      Effect.mapError((cause) =>
        MpfError.phasRoot(
          new Error("Failed to verify authenticated root count", { cause }),
        ),
      ),
    );
    if (proof.root !== committedRoot) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(
            `Counted root commitment mismatch: root=${proof.root},committed=${committedRoot},phas_root=${proof.phas_root},count=${proof.count.toString()}`,
          ),
        ),
      );
    }
    if (
      options.expectedRoot !== undefined &&
      proof.root !== options.expectedRoot
    ) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(
            `Root proof root mismatch: expected=${options.expectedRoot},actual=${proof.root}`,
          ),
        ),
      );
    }
    if (
      options.expectedCount !== undefined &&
      proof.count !== options.expectedCount
    ) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(
            `Root proof count mismatch: expected=${options.expectedCount.toString()},actual=${proof.count.toString()}`,
          ),
        ),
      );
    }
  });

export const verifyRootCountProof = (
  proof: SDK.RootCountProof,
  options: RootProofVerificationOptions,
): Effect.Effect<void, MpfError, never> => validateCountProof(proof, options);

export const buildRootMembershipProof = <K, V>({
  root,
  key,
  value,
  keySchema,
  valueSchema,
}: {
  readonly root: BuiltTypedAuthenticatedRoot<K, V>;
  readonly key: K;
  readonly value: V;
  readonly keySchema: DataSchema;
  readonly valueSchema: DataSchema;
}): Effect.Effect<SDK.RootMembershipProof<K, V>, MpfError, never> =>
  Effect.gen(function* () {
    const keyBytes = yield* encodeData(key, keySchema, "membership proof key");
    const valueBytes = yield* encodeData(
      value,
      valueSchema,
      "membership proof value",
    );
    const { keys, values } = rootEntryVectors(root.entries);
    const proof = yield* keyValuePhasProof(keys, values, keyBytes);
    yield* verifyKeyValuePhasMembershipProof({
      root: root.phasRoot,
      key: keyBytes,
      value: valueBytes,
      proof,
    });
    return {
      domain: root.domain,
      root: root.root,
      phas_root: root.phasRoot,
      count: root.count,
      key,
      value,
      proof,
    };
  });

export const buildRootNonMembershipProof = <K>({
  root,
  key,
  keySchema,
}: {
  readonly root: BuiltAuthenticatedRoot;
  readonly key: K;
  readonly keySchema: DataSchema;
}): Effect.Effect<SDK.RootNonMembershipProof<K>, MpfError, never> =>
  Effect.gen(function* () {
    const keyBytes = yield* encodeData(
      key,
      keySchema,
      "non-membership proof key",
    );
    const { keys, values } = rootEntryVectors(root.entries);
    const proof = yield* keyValuePhasNonMembershipProof(keys, values, keyBytes);
    yield* verifyKeyValuePhasNonMembershipProof({
      root: root.phasRoot,
      key: keyBytes,
      proof,
    });
    return {
      domain: root.domain,
      root: root.root,
      phas_root: root.phasRoot,
      count: root.count,
      key,
      proof,
    };
  });

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
      if (step.schema_version < 0n) {
        return yield* Effect.fail(
          MpfError.phasRoot(
            new Error(
              `Transition step schema_version must be non-negative at sorted index ${index.toString()}`,
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

export const verifyEventToStepProof = (
  witness: SDK.EventToStepProof,
  options: Omit<RootProofVerificationOptions, "expectedDomain">,
): Effect.Effect<void, MpfError, never> => {
  if ("EventToStepMembership" in witness) {
    return verifyRootMembershipProof({
      witness: witness.EventToStepMembership.membership,
      keySchema: SDK.EventKeySchema,
      valueSchema: SDK.EventToStepValueSchema,
      options: {
        ...options,
        expectedDomain: SDK.ROOT_DOMAINS.eventToStep,
      },
    });
  }
  return verifyRootNonMembershipProof({
    witness: witness.EventToStepNonMembership.non_membership,
    keySchema: SDK.EventKeySchema,
    options: {
      ...options,
      expectedDomain: SDK.ROOT_DOMAINS.eventToStep,
    },
  });
};
