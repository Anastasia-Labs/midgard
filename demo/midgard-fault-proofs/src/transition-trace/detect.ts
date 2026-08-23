import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import { midgardFieldCommitmentFromItemsV1 } from "@al-ft/midgard-core";
import {
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxCompactV1,
  decodeMidgardSpendInputItemV1,
  encodeMidgardSpendInputItemV1,
} from "@al-ft/midgard-core/codec";
import {
  encodeCborBytes,
  encodeCborUnsigned,
  readCborArrayHeader,
  readCborBytes,
  readCborInteger,
} from "@al-ft/midgard-core/codec/cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterialV1 } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import {
  TransitionTraceChallengerError,
  transitionTraceError,
} from "./errors.js";
import {
  eventKeyFingerprint,
  eventKeyPhase,
  type SourceEventRecord,
  type TransitionTraceReconstruction,
} from "./reconstruct.js";
import {
  type AcceptedTransactionTransitionMismatchEvidence,
  buildAcceptedTransactionTransitionMismatchFault,
  buildCountFault,
  buildDuplicateTraceEventFault,
  buildEventToStepMismatchFault,
  buildInvalidForcedTransactionNoOpWitness,
  buildInvalidWithdrawalNoOpWitness,
  buildL2TransactionTransitionWitness,
  buildMappedEventMissingFromSourceFault,
  buildOmittedDueL1EventFault,
  buildOutOfWindowSourceEventFault,
  buildSourceEventMissingTraceFault,
  buildSourcePhaseMismatchFault,
  buildTraceBoundaryFault,
  buildTraceLinkFault,
  buildTransitionFaultProof,
  type L2TransactionTransitionEvidence,
  type OmittedDueL1EventEvidence,
  type OutOfWindowSourceEventEvidence,
  rootCountProof,
} from "./witnesses.js";

/**
 * The complete set of fault kinds `detectTransitionTraceFaults` can report,
 * as a runtime tuple rather than a bare union — so closure-contract tests can
 * enumerate it directly instead of hand-copying the kind list. Add new kinds
 * here (the type below is derived from this array, not the reverse).
 */
export const TRANSITION_TRACE_FAULT_KINDS = [
  "traceBoundary",
  "traceLink",
  "eventToStepMismatch",
  "sourceMembershipMismatch",
  "invalidOneStepTransition",
  "omittedDueL1Event",
  "duplicateTraceEvent",
  "outOfWindowSourceEvent",
  "countFault",
  "acceptedTransactionTransitionMismatch",
] as const;

export type TransitionTraceFaultKind =
  (typeof TRANSITION_TRACE_FAULT_KINDS)[number];

export type TransitionTraceDetection =
  | {
      readonly buildable: true;
      readonly kind: TransitionTraceFaultKind;
      readonly invariant: string;
      readonly diagnostic: string;
      readonly fault: SDK.TransitionFault;
      readonly proof: SDK.TransitionFaultProof;
    }
  | {
      readonly buildable: false;
      readonly kind: TransitionTraceFaultKind;
      readonly invariant: string;
      readonly diagnostic: string;
      readonly reason: string;
    };

export type TransitionTraceDetectionEvidence = {
  readonly omittedDueL1Events?: readonly OmittedDueL1EventEvidence[];
  readonly outOfWindowSourceEvents?: readonly OutOfWindowSourceEventEvidence[];
  readonly acceptedTransactionTransitionMismatches?: readonly AcceptedTransactionTransitionMismatchEvidence[];
  /**
   * Authenticated ledger mutation witnesses for L2 steps whose committed
   * post-root is disputed. Retained DA authenticates the transaction and its
   * field preimages, but does not contain the predecessor ledger tree needed
   * to derive these proofs automatically.
   */
  readonly l2TransactionTransitions?: readonly (L2TransactionTransitionEvidence & {
    readonly stepIndex: bigint;
  })[];
};

const detection = ({
  reconstruction,
  kind,
  invariant,
  diagnostic,
  fault,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly kind: TransitionTraceFaultKind;
  readonly invariant: string;
  readonly diagnostic: string;
  readonly fault: SDK.TransitionFault;
}): TransitionTraceDetection => ({
  buildable: true,
  kind,
  invariant,
  diagnostic,
  fault,
  proof: buildTransitionFaultProof({ reconstruction, fault }),
});

const unsupportedDetection = ({
  kind,
  invariant,
  diagnostic,
  reason,
}: {
  readonly kind: TransitionTraceFaultKind;
  readonly invariant: string;
  readonly diagnostic: string;
  readonly reason: string;
}): TransitionTraceDetection => ({
  buildable: false,
  kind,
  invariant,
  diagnostic,
  reason,
});

const maybeUnsupported = async (
  build: () => Promise<TransitionTraceDetection>,
  fallback: Omit<
    Extract<TransitionTraceDetection, { buildable: false }>,
    "buildable"
  >,
): Promise<TransitionTraceDetection> => {
  try {
    return await build();
  } catch (error) {
    if (
      error instanceof TransitionTraceChallengerError &&
      error.code === "unsupportedWitness"
    ) {
      return unsupportedDetection({
        ...fallback,
        reason: error.message,
      });
    }
    throw error;
  }
};

const orderedTrace = (
  reconstruction: TransitionTraceReconstruction,
): readonly SDK.TransitionStep[] =>
  [...reconstruction.transitionTrace]
    .sort((left, right) =>
      left.key < right.key ? -1 : left.key > right.key ? 1 : 0,
    )
    .map((entry) => entry.value);

const sourceForStep = (
  reconstruction: TransitionTraceReconstruction,
  step: SDK.TransitionStep,
): SourceEventRecord | undefined =>
  reconstruction.sourceEventsByFingerprint.get(
    eventKeyFingerprint(step.event_key),
  );

export const detectCountFaults = (
  reconstruction: TransitionTraceReconstruction,
): readonly TransitionTraceDetection[] => {
  const header = reconstruction.header;
  const detections: TransitionTraceDetection[] = [];
  const expectedTotal =
    header.withdrawalCount +
    header.forcedTransactionCount +
    header.l2TransactionCount +
    header.depositCount;
  if (header.totalEventCount !== expectedTotal) {
    const fault = buildCountFault("HeaderTotalCountMismatch");
    detections.push(
      detection({
        reconstruction,
        kind: "countFault",
        invariant: "header_total_event_count",
        diagnostic: `HeaderV1 total_event_count ${header.totalEventCount.toString()} does not equal source count sum ${expectedTotal.toString()}.`,
        fault,
      }),
    );
  }
  if (header.transitionStepCount !== header.totalEventCount) {
    const fault = buildCountFault("HeaderTransitionStepCountMismatch");
    detections.push(
      detection({
        reconstruction,
        kind: "countFault",
        invariant: "header_transition_step_count",
        diagnostic: `HeaderV1 transition_step_count ${header.transitionStepCount.toString()} does not equal total_event_count ${header.totalEventCount.toString()}.`,
        fault,
      }),
    );
  }
  const rootCountChecks = [
    {
      invariant: "withdrawals_root_count",
      root: reconstruction.rootData.withdrawals,
      expected: header.withdrawalCount,
      witness: {
        SourceRootCountMismatch: {
          proof: rootCountProof(reconstruction.rootData.withdrawals),
        },
      } satisfies SDK.CountFaultWitness,
    },
    {
      invariant: "forced_transactions_root_count",
      root: reconstruction.rootData.forcedTransactions,
      expected: header.forcedTransactionCount,
      witness: {
        SourceRootCountMismatch: {
          proof: rootCountProof(reconstruction.rootData.forcedTransactions),
        },
      } satisfies SDK.CountFaultWitness,
    },
    {
      invariant: "transactions_root_count",
      root: reconstruction.rootData.transactions,
      expected: header.l2TransactionCount,
      witness: {
        SourceRootCountMismatch: {
          proof: rootCountProof(reconstruction.rootData.transactions),
        },
      } satisfies SDK.CountFaultWitness,
    },
    {
      invariant: "deposits_root_count",
      root: reconstruction.rootData.deposits,
      expected: header.depositCount,
      witness: {
        SourceRootCountMismatch: {
          proof: rootCountProof(reconstruction.rootData.deposits),
        },
      } satisfies SDK.CountFaultWitness,
    },
    {
      invariant: "event_to_step_root_count",
      root: reconstruction.rootData.eventToStep,
      expected: header.totalEventCount,
      witness: {
        EventToStepRootCountMismatch: {
          proof: rootCountProof(reconstruction.rootData.eventToStep),
        },
      } satisfies SDK.CountFaultWitness,
    },
    {
      invariant: "transition_trace_root_count",
      root: reconstruction.rootData.transitionTrace,
      expected: header.transitionStepCount,
      witness: {
        TransitionTraceRootCountMismatch: {
          proof: rootCountProof(reconstruction.rootData.transitionTrace),
        },
      } satisfies SDK.CountFaultWitness,
    },
  ] as const;
  for (const check of rootCountChecks) {
    if (check.root.count !== check.expected) {
      detections.push(
        detection({
          reconstruction,
          kind: "countFault",
          invariant: check.invariant,
          diagnostic: `${check.invariant} committed count ${check.root.count.toString()} does not match header count ${check.expected.toString()}.`,
          fault: buildCountFault(check.witness),
        }),
      );
    }
  }
  return detections;
};

const detectTraceBoundaryFaults = async (
  reconstruction: TransitionTraceReconstruction,
): Promise<readonly TransitionTraceDetection[]> => {
  const detections: TransitionTraceDetection[] = [];
  const first = reconstruction.traceByStepIndex.get(0n);
  if (
    first !== undefined &&
    first.value.pre_utxos_root !== reconstruction.header.prevUtxosRoot
  ) {
    detections.push(
      detection({
        reconstruction,
        kind: "traceBoundary",
        invariant: "trace_start_prev_utxos_root",
        diagnostic: `Trace step 0 pre_utxos_root ${first.value.pre_utxos_root} does not equal header.prev_utxos_root ${reconstruction.header.prevUtxosRoot}.`,
        fault: await buildTraceBoundaryFault({
          reconstruction,
          side: "TraceStart",
          stepIndex: 0n,
        }),
      }),
    );
  }
  const lastIndex = reconstruction.header.transitionStepCount - 1n;
  const last =
    lastIndex >= 0n
      ? reconstruction.traceByStepIndex.get(lastIndex)
      : undefined;
  if (
    last !== undefined &&
    last.value.post_utxos_root !== reconstruction.header.utxosRoot
  ) {
    detections.push(
      detection({
        reconstruction,
        kind: "traceBoundary",
        invariant: "trace_end_utxos_root",
        diagnostic: `Last trace step post_utxos_root ${last.value.post_utxos_root} does not equal header.utxos_root ${reconstruction.header.utxosRoot}.`,
        fault: await buildTraceBoundaryFault({
          reconstruction,
          side: "TraceEnd",
          stepIndex: lastIndex,
        }),
      }),
    );
  }
  return detections;
};

const detectTraceLinkFaults = async (
  reconstruction: TransitionTraceReconstruction,
): Promise<readonly TransitionTraceDetection[]> => {
  const detections: TransitionTraceDetection[] = [];
  const steps = orderedTrace(reconstruction);
  for (let index = 0; index < steps.length - 1; index += 1) {
    const lower = steps[index]!;
    const upper = steps[index + 1]!;
    if (lower.post_utxos_root !== upper.pre_utxos_root) {
      detections.push(
        detection({
          reconstruction,
          kind: "traceLink",
          invariant: "adjacent_trace_roots",
          diagnostic: `Trace step ${lower.step_index.toString()} post_utxos_root ${lower.post_utxos_root} does not equal step ${upper.step_index.toString()} pre_utxos_root ${upper.pre_utxos_root}.`,
          fault: await buildTraceLinkFault({
            reconstruction,
            lowerStepIndex: lower.step_index,
          }),
        }),
      );
    }
  }
  return detections;
};

const detectDuplicateTraceEvents = async (
  reconstruction: TransitionTraceReconstruction,
): Promise<readonly TransitionTraceDetection[]> => {
  const seen = new Map<string, SDK.TransitionStep>();
  const detections: TransitionTraceDetection[] = [];
  for (const step of orderedTrace(reconstruction)) {
    const fingerprint = eventKeyFingerprint(step.event_key);
    const prior = seen.get(fingerprint);
    if (prior !== undefined && prior.step_index !== step.step_index) {
      detections.push(
        detection({
          reconstruction,
          kind: "duplicateTraceEvent",
          invariant: "trace_event_key_unique",
          diagnostic: `Trace steps ${prior.step_index.toString()} and ${step.step_index.toString()} both commit event key ${fingerprint}.`,
          fault: await buildDuplicateTraceEventFault({
            reconstruction,
            leftStepIndex: prior.step_index,
            rightStepIndex: step.step_index,
          }),
        }),
      );
    } else {
      seen.set(fingerprint, step);
    }
  }
  return detections;
};

const detectEventToStepMismatches = async (
  reconstruction: TransitionTraceReconstruction,
): Promise<readonly TransitionTraceDetection[]> => {
  const detections: TransitionTraceDetection[] = [];
  for (const step of orderedTrace(reconstruction)) {
    const mapped = reconstruction.eventToStepByFingerprint.get(
      eventKeyFingerprint(step.event_key),
    );
    if (
      mapped === undefined ||
      mapped.value.step_index !== step.step_index ||
      mapped.value.phase !== step.phase
    ) {
      const mappedText =
        mapped === undefined
          ? "absent"
          : `step_index=${mapped.value.step_index.toString()},phase=${mapped.value.phase}`;
      detections.push(
        detection({
          reconstruction,
          kind: "eventToStepMismatch",
          invariant: "event_to_step_matches_trace",
          diagnostic: `Trace step ${step.step_index.toString()} maps event key ${eventKeyFingerprint(
            step.event_key,
          )}, but event_to_step is ${mappedText}.`,
          fault: await buildEventToStepMismatchFault({
            reconstruction,
            stepIndex: step.step_index,
          }),
        }),
      );
    }
  }
  return detections;
};

const detectSourceMembershipMismatches = async (
  reconstruction: TransitionTraceReconstruction,
): Promise<readonly TransitionTraceDetection[]> => {
  const detections: TransitionTraceDetection[] = [];
  for (const mapped of reconstruction.eventToStep) {
    const fingerprint = eventKeyFingerprint(mapped.key);
    const source = reconstruction.sourceEventsByFingerprint.get(fingerprint);
    const trace = reconstruction.traceByStepIndex.get(mapped.value.step_index);
    if (source === undefined && trace !== undefined) {
      detections.push(
        await maybeUnsupported(
          async () =>
            detection({
              reconstruction,
              kind: "sourceMembershipMismatch",
              invariant: "mapped_event_has_source_member",
              diagnostic: `event_to_step maps event key ${fingerprint}, but the source root for phase ${mapped.value.phase} has no matching member.`,
              fault: await buildMappedEventMissingFromSourceFault({
                reconstruction,
                stepIndex: mapped.value.step_index,
                eventKey: mapped.key,
              }),
            }),
          {
            kind: "sourceMembershipMismatch",
            invariant: "mapped_event_has_source_member",
            diagnostic: `event_to_step maps event key ${fingerprint}, but the source root for phase ${mapped.value.phase} has no matching member.`,
            reason: "",
          },
        ),
      );
    }
  }
  for (const source of reconstruction.sourceEvents) {
    if (!reconstruction.eventToStepByFingerprint.has(source.fingerprint)) {
      detections.push(
        await maybeUnsupported(
          async () =>
            detection({
              reconstruction,
              kind: "sourceMembershipMismatch",
              invariant: "source_event_has_event_to_step_member",
              diagnostic: `Source event ${source.fingerprint} is committed in ${source.phase}, but event_to_step has no matching member.`,
              fault: await buildSourceEventMissingTraceFault({
                reconstruction,
                eventKey: source.eventKey,
              }),
            }),
          {
            kind: "sourceMembershipMismatch",
            invariant: "source_event_has_event_to_step_member",
            diagnostic: `Source event ${source.fingerprint} is committed in ${source.phase}, but event_to_step has no matching member.`,
            reason: "",
          },
        ),
      );
    }
  }
  for (const step of orderedTrace(reconstruction)) {
    const source = sourceForStep(reconstruction, step);
    if (source !== undefined && source.phase !== step.phase) {
      detections.push(
        await maybeUnsupported(
          async () =>
            detection({
              reconstruction,
              kind: "sourceMembershipMismatch",
              invariant: "source_phase_matches_trace_phase",
              diagnostic: `Trace step ${step.step_index.toString()} phase ${step.phase} does not match source phase ${source.phase}.`,
              fault: await buildSourcePhaseMismatchFault({
                reconstruction,
                stepIndex: step.step_index,
              }),
            }),
          {
            kind: "sourceMembershipMismatch",
            invariant: "source_phase_matches_trace_phase",
            diagnostic: `Trace step ${step.step_index.toString()} phase ${step.phase} does not match source phase ${source.phase}.`,
            reason: "",
          },
        ),
      );
    }
  }
  return detections;
};

const detectInvalidNoOpTransitions = async (
  reconstruction: TransitionTraceReconstruction,
): Promise<readonly TransitionTraceDetection[]> => {
  const detections: TransitionTraceDetection[] = [];
  for (const step of orderedTrace(reconstruction)) {
    const source = sourceForStep(reconstruction, step);
    if (source === undefined) {
      continue;
    }
    if (
      source.phase === "Withdrawal" &&
      source.entry.value.validity !== "WithdrawalIsValid" &&
      step.pre_utxos_root !== step.post_utxos_root
    ) {
      const witness = await buildInvalidWithdrawalNoOpWitness({
        reconstruction,
        stepIndex: step.step_index,
      });
      detections.push(
        detection({
          reconstruction,
          kind: "invalidOneStepTransition",
          invariant: "invalid_withdrawal_is_no_op",
          diagnostic: `Invalid withdrawal trace step ${step.step_index.toString()} changes UTxO root from ${step.pre_utxos_root} to ${step.post_utxos_root}.`,
          fault: SDK.invalidOneStepTransitionFault(witness),
        }),
      );
    }
    if (
      source.phase === "ForcedTransaction" &&
      source.entry.value.operator_validity !== "TxIsValid" &&
      step.pre_utxos_root !== step.post_utxos_root
    ) {
      const witness = await buildInvalidForcedTransactionNoOpWitness({
        reconstruction,
        stepIndex: step.step_index,
      });
      detections.push(
        detection({
          reconstruction,
          kind: "invalidOneStepTransition",
          invariant: "invalid_forced_transaction_is_no_op",
          diagnostic: `Invalid forced transaction trace step ${step.step_index.toString()} changes UTxO root from ${step.pre_utxos_root} to ${step.post_utxos_root}.`,
          fault: SDK.invalidOneStepTransitionFault(witness),
        }),
      );
    }
  }
  return detections;
};

const acceptedTerminalPostRoot = (
  terminalAcceptanceWitnessCbor: string,
): string => {
  const bytes = Buffer.from(terminalAcceptanceWitnessCbor, "hex");
  const array = readCborArrayHeader(bytes, 0, "terminal acceptance witness");
  if (array.length !== 4) {
    throw transitionTraceError(
      "malformedPayload",
      "Terminal acceptance witness must contain exactly four fields.",
    );
  }
  const version = readCborInteger(bytes, array.nextOffset, "terminal version");
  if (version.value !== 1n) {
    throw transitionTraceError(
      "malformedPayload",
      "Terminal acceptance witness has an unsupported version.",
    );
  }
  const tag = readCborBytes(bytes, version.nextOffset, "terminal tag");
  if (tag.value.length !== 0) {
    throw transitionTraceError(
      "malformedPayload",
      "Terminal acceptance witness tag must be empty.",
    );
  }
  const root = readCborBytes(bytes, tag.nextOffset, "terminal ledger root");
  if (root.value.length !== 32) {
    throw transitionTraceError(
      "malformedPayload",
      "Terminal acceptance witness ledger root must contain 32 bytes.",
    );
  }
  const frontier = readCborBytes(
    bytes,
    root.nextOffset,
    "terminal delta frontier",
  );
  if (frontier.nextOffset !== bytes.length) {
    throw transitionTraceError(
      "malformedPayload",
      "Terminal acceptance witness contains trailing bytes.",
    );
  }
  return root.value.toString("hex");
};

const detectAcceptedTransactionTransitionMismatches = (
  reconstruction: TransitionTraceReconstruction,
  evidence: readonly AcceptedTransactionTransitionMismatchEvidence[],
): readonly TransitionTraceDetection[] => {
  const detections: TransitionTraceDetection[] = [];
  for (const item of evidence) {
    if (item.claim.descriptor_membership.value.verdict !== "Accepted") {
      throw transitionTraceError(
        "missingWitnessData",
        "Accepted transition mismatch evidence must reference an accepted descriptor.",
      );
    }
    const committedPostRoot =
      item.claim.transition_step_membership.value.post_utxos_root;
    const validatedPostRoot = acceptedTerminalPostRoot(
      item.terminalAcceptanceWitnessCbor,
    );
    if (committedPostRoot !== validatedPostRoot) {
      detections.push(
        detection({
          reconstruction,
          kind: "acceptedTransactionTransitionMismatch",
          invariant: "accepted_transaction_uses_validated_ledger_root",
          diagnostic: `Accepted transaction transition commits ${committedPostRoot}, but its authenticated terminal validation witness commits ${validatedPostRoot}.`,
          fault: buildAcceptedTransactionTransitionMismatchFault(item),
        }),
      );
    }
  }
  return detections;
};

const exactHexBytes = (value: string, label: string): Buffer => {
  if (
    value.length % 2 !== 0 ||
    (value.length > 0 && !/^[0-9a-f]+$/u.test(value))
  ) {
    throw transitionTraceError(
      "missingWitnessData",
      `${label} must be canonical lowercase hexadecimal bytes.`,
    );
  }
  return Buffer.from(value, "hex");
};

const exactProofInteger = (
  value: bigint,
  label: string,
  maximum: number,
): number => {
  if (value < 0n || value > BigInt(maximum)) {
    throw transitionTraceError(
      "missingWitnessData",
      `${label} is outside the supported MPF proof range.`,
    );
  }
  return Number(value);
};

const mpfProofFromWitness = ({
  key,
  value,
  proof,
  label,
}: {
  readonly key: Buffer;
  readonly value: Buffer | undefined;
  readonly proof: SDK.Proof;
  readonly label: string;
}): MpfProof => {
  try {
    return MpfProof.fromJSON(
      key,
      value,
      proof.map((step, index) => {
        const stepLabel = `${label}[${index.toString()}]`;
        if ("Branch" in step) {
          const neighbors = exactHexBytes(
            step.Branch.neighbors,
            `${stepLabel}.neighbors`,
          );
          if (neighbors.length !== 4 * 32) {
            throw transitionTraceError(
              "missingWitnessData",
              `${stepLabel}.neighbors must contain exactly four MPF hashes.`,
            );
          }
          return {
            type: "branch",
            skip: exactProofInteger(step.Branch.skip, `${stepLabel}.skip`, 64),
            neighbors: step.Branch.neighbors,
          };
        }
        if ("Fork" in step) {
          const prefix = exactHexBytes(
            step.Fork.neighbor.prefix,
            `${stepLabel}.neighbor.prefix`,
          );
          const root = exactHexBytes(
            step.Fork.neighbor.root,
            `${stepLabel}.neighbor.root`,
          );
          if (
            prefix.length > 64 ||
            prefix.some((nibble) => nibble > 0x0f) ||
            root.length !== 32
          ) {
            throw transitionTraceError(
              "missingWitnessData",
              `${stepLabel}.neighbor is not a canonical MPF fork.`,
            );
          }
          return {
            type: "fork",
            skip: exactProofInteger(step.Fork.skip, `${stepLabel}.skip`, 64),
            neighbor: {
              nibble: exactProofInteger(
                step.Fork.neighbor.nibble,
                `${stepLabel}.neighbor.nibble`,
                15,
              ),
              prefix: step.Fork.neighbor.prefix,
              root: step.Fork.neighbor.root,
            },
          };
        }
        const neighborKey = exactHexBytes(step.Leaf.key, `${stepLabel}.key`);
        const neighborValue = exactHexBytes(
          step.Leaf.value,
          `${stepLabel}.value`,
        );
        if (neighborKey.length !== 32 || neighborValue.length !== 32) {
          throw transitionTraceError(
            "missingWitnessData",
            `${stepLabel} is not a canonical MPF leaf.`,
          );
        }
        return {
          type: "leaf",
          skip: exactProofInteger(step.Leaf.skip, `${stepLabel}.skip`, 64),
          neighbor: {
            key: step.Leaf.key,
            value: step.Leaf.value,
          },
        };
      }),
    );
  } catch (cause) {
    if (cause instanceof TransitionTraceChallengerError) {
      throw cause;
    }
    throw transitionTraceError(
      "missingWitnessData",
      `${label} is not a well-formed MPF proof.`,
      cause,
    );
  }
};

const normalizedMpfRoot = (root: Buffer | null, label: string): string => {
  if (root === null) {
    return SDK.EMPTY_MERKLE_TREE_ROOT;
  }
  if (root.length !== 32) {
    throw transitionTraceError(
      "missingWitnessData",
      `${label} did not derive a 32-byte MPF root.`,
    );
  }
  return root.toString("hex");
};

const verifyProofRoot = ({
  proof,
  includingItem,
  expectedRoot,
  label,
}: {
  readonly proof: MpfProof;
  readonly includingItem: boolean;
  readonly expectedRoot: string;
  readonly label: string;
}): void => {
  let actualRoot: string;
  try {
    actualRoot = normalizedMpfRoot(proof.verify(includingItem), label);
  } catch (cause) {
    if (cause instanceof TransitionTraceChallengerError) {
      throw cause;
    }
    throw transitionTraceError(
      "missingWitnessData",
      `${label} cannot be replayed as an MPF proof.`,
      cause,
    );
  }
  if (actualRoot !== expectedRoot) {
    throw transitionTraceError(
      "missingWitnessData",
      `${label} does not open the current authenticated ledger root.`,
    );
  }
};

/**
 * The one valid byte form of a spend input here is the §5.3 field-0/1 item —
 * `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`, a fixed 38 bytes — which is what
 * on-chain `ledger_outref_key` / `encode_midgard_tx_input` derives and what
 * `decode_midgard_tx_input_cbor` accepts. `encodeCbor([txId, index])` would
 * spell indices 0–23 minimally and reject every key the trie actually holds.
 */
const canonicalNativeSpendInput = (bytes: Buffer, label: string): Buffer => {
  try {
    const input = decodeMidgardSpendInputItemV1(bytes);
    const canonical = encodeMidgardSpendInputItemV1(input);
    if (!canonical.equals(bytes)) {
      throw new Error("input CBOR is not the exact canonical encoding");
    }
    return canonical;
  } catch (cause) {
    throw transitionTraceError(
      "missingWitnessData",
      `${label} is not a canonical V1 MidgardTxInput.`,
      cause,
    );
  }
};

type Tag4OutputAsset = {
  readonly policyId: Buffer;
  readonly assetName: Buffer;
  readonly quantity: bigint;
};

type Tag4VersionedScript = {
  readonly language: 0n | 3n | 128n;
  readonly bytes: Buffer;
};

type Tag4Output = {
  readonly address: Buffer;
  readonly lovelace: bigint;
  readonly assets: readonly Tag4OutputAsset[];
  readonly datumCbor?: Buffer;
  readonly scriptRef?: Tag4VersionedScript;
};

const tag4ByteAt = (bytes: Buffer, offset: number, label: string): number => {
  const value = bytes[offset];
  if (value === undefined) {
    throw new Error(`${label} exceeds the output byte length`);
  }
  return value;
};

const tag4UintAt = (
  bytes: Buffer,
  offset: number,
  label: string,
): { readonly value: bigint; readonly nextOffset: number } => {
  const tag = tag4ByteAt(bytes, offset, label);
  if (tag <= 23) {
    return { value: BigInt(tag), nextOffset: offset + 1 };
  }
  const byteCount =
    tag === 24 ? 1 : tag === 25 ? 2 : tag === 26 ? 4 : tag === 27 ? 8 : 0;
  if (byteCount === 0 || offset + 1 + byteCount > bytes.length) {
    throw new Error(`${label} is not an Aiken V1 unsigned integer`);
  }
  let value = 0n;
  for (let index = 0; index < byteCount; index += 1) {
    value = value * 256n + BigInt(tag4ByteAt(bytes, offset + 1 + index, label));
  }
  return { value, nextOffset: offset + 1 + byteCount };
};

const tag4DefiniteLengthAt = (
  bytes: Buffer,
  offset: number,
  major: 2 | 5,
  label: string,
): { readonly length: number; readonly nextOffset: number } => {
  const tag = tag4ByteAt(bytes, offset, label);
  const shortBase = major << 5;
  if (tag >= shortBase && tag <= shortBase + 23) {
    return { length: tag - shortBase, nextOffset: offset + 1 };
  }
  const byteCount =
    tag === shortBase + 24
      ? 1
      : tag === shortBase + 25
        ? 2
        : tag === shortBase + 26
          ? 4
          : 0;
  if (byteCount === 0 || offset + 1 + byteCount > bytes.length) {
    throw new Error(`${label} is not an Aiken V1 definite-length value`);
  }
  let length = 0;
  for (let index = 0; index < byteCount; index += 1) {
    length = length * 256 + tag4ByteAt(bytes, offset + 1 + index, label);
  }
  return { length, nextOffset: offset + 1 + byteCount };
};

const tag4BytesAt = (
  bytes: Buffer,
  offset: number,
  label: string,
): { readonly value: Buffer; readonly nextOffset: number } => {
  const header = tag4DefiniteLengthAt(bytes, offset, 2, label);
  const end = header.nextOffset + header.length;
  if (end > bytes.length) {
    throw new Error(`${label} exceeds the output byte length`);
  }
  return {
    value: Buffer.from(bytes.subarray(header.nextOffset, end)),
    nextOffset: end,
  };
};

const tag4MapHeaderAt = (
  bytes: Buffer,
  offset: number,
  label: string,
): { readonly length: number; readonly nextOffset: number } =>
  tag4DefiniteLengthAt(bytes, offset, 5, label);

const tag4AddressAt = (
  bytes: Buffer,
  offset: number,
  label: string,
): { readonly value: Buffer; readonly nextOffset: number } => {
  if (tag4ByteAt(bytes, offset, label) !== 0x58 || offset + 2 > bytes.length) {
    throw new Error(`${label} must use the Aiken V1 address byte encoding`);
  }
  const length = tag4ByteAt(bytes, offset + 1, label);
  if (length !== 29 && length !== 57) {
    throw new Error(`${label} must contain 29 or 57 bytes`);
  }
  const end = offset + 2 + length;
  if (end > bytes.length) {
    throw new Error(`${label} exceeds the output byte length`);
  }
  const address = Buffer.from(bytes.subarray(offset + 2, end));
  const header = address[0]!;
  const addressType = Math.floor(header / 16);
  const networkNibble = header - addressType * 16;
  const networkId = networkNibble >= 8 ? networkNibble - 8 : networkNibble;
  if (networkId !== 0 && networkId !== 1) {
    throw new Error(`${label} has an unsupported network id`);
  }
  if (
    (length === 57 && addressType > 3) ||
    (length === 29 && addressType !== 6 && addressType !== 7)
  ) {
    throw new Error(`${label} has an invalid credential layout`);
  }
  return { value: address, nextOffset: end };
};

const tag4ValueAt = (
  bytes: Buffer,
  offset: number,
  label: string,
): {
  readonly lovelace: bigint;
  readonly assets: readonly Tag4OutputAsset[];
  readonly nextOffset: number;
} => {
  if (tag4ByteAt(bytes, offset, label) !== 0x82) {
    throw new Error(`${label} must be the Aiken V1 value pair`);
  }
  const lovelace = tag4UintAt(bytes, offset + 1, `${label}.lovelace`);
  const policyMap = tag4MapHeaderAt(
    bytes,
    lovelace.nextOffset,
    `${label}.assets`,
  );
  let cursor = policyMap.nextOffset;
  const assets: Tag4OutputAsset[] = [];
  for (let policyIndex = 0; policyIndex < policyMap.length; policyIndex += 1) {
    if (
      tag4ByteAt(bytes, cursor, `${label}.policy`) !== 0x58 ||
      tag4ByteAt(bytes, cursor + 1, `${label}.policy`) !== 28
    ) {
      throw new Error(`${label}.policy must contain exactly 28 bytes`);
    }
    const policyEnd = cursor + 30;
    if (policyEnd > bytes.length) {
      throw new Error(`${label}.policy exceeds the output byte length`);
    }
    const policyId = Buffer.from(bytes.subarray(cursor + 2, policyEnd));
    const assetMap = tag4MapHeaderAt(
      bytes,
      policyEnd,
      `${label}.policy_assets`,
    );
    if (assetMap.length === 0) {
      throw new Error(`${label}.policy_assets must not be empty`);
    }
    cursor = assetMap.nextOffset;
    for (let assetIndex = 0; assetIndex < assetMap.length; assetIndex += 1) {
      const assetName = tag4BytesAt(bytes, cursor, `${label}.asset_name`);
      if (assetName.value.length > 32) {
        throw new Error(`${label}.asset_name exceeds 32 bytes`);
      }
      const quantity = tag4UintAt(
        bytes,
        assetName.nextOffset,
        `${label}.quantity`,
      );
      if (quantity.value === 0n) {
        throw new Error(`${label}.quantity must be positive`);
      }
      assets.push({
        policyId,
        assetName: assetName.value,
        quantity: quantity.value,
      });
      cursor = quantity.nextOffset;
    }
  }
  return {
    lovelace: lovelace.value,
    assets,
    nextOffset: cursor,
  };
};

const tag4ScriptAt = (
  bytes: Buffer,
  offset: number,
  label: string,
): { readonly value: Tag4VersionedScript; readonly nextOffset: number } => {
  if (tag4ByteAt(bytes, offset, label) !== 0x82) {
    throw new Error(`${label} must be the Aiken V1 versioned-script pair`);
  }
  const firstTag = tag4ByteAt(bytes, offset + 1, `${label}.language`);
  let language: bigint;
  let cursor: number;
  if (firstTag === 0 || firstTag === 3) {
    language = BigInt(firstTag);
    cursor = offset + 2;
  } else {
    if (firstTag !== 24) {
      throw new Error(`${label}.language has an invalid integer encoding`);
    }
    language = BigInt(tag4ByteAt(bytes, offset + 2, `${label}.language`));
    cursor = offset + 3;
  }
  if (language !== 0n && language !== 3n && language !== 128n) {
    throw new Error(`${label}.language is not supported by Aiken V1`);
  }
  const scriptBytes = tag4BytesAt(bytes, cursor, `${label}.bytes`);
  return {
    value: {
      language,
      bytes: scriptBytes.value,
    } as Tag4VersionedScript,
    nextOffset: scriptBytes.nextOffset,
  };
};

const decodeTag4Output = (bytes: Buffer): Tag4Output => {
  const entryTag = tag4ByteAt(bytes, 0, "output");
  if (entryTag < 0xa2 || entryTag > 0xa4) {
    throw new Error("output must contain two to four Aiken V1 map entries");
  }
  const entryCount = entryTag - 0xa0;
  if (tag4ByteAt(bytes, 1, "output.address_key") !== 0) {
    throw new Error("output address key must be zero");
  }
  const address = tag4AddressAt(bytes, 2, "output.address");
  if (tag4ByteAt(bytes, address.nextOffset, "output.value_key") !== 1) {
    throw new Error("output value key must be one");
  }
  const value = tag4ValueAt(bytes, address.nextOffset + 1, "output.value");
  let cursor = value.nextOffset;
  let datumCbor: Buffer | undefined;
  let scriptRef: Tag4VersionedScript | undefined;
  if (entryCount > 2) {
    const extraKey = tag4ByteAt(bytes, cursor, "output.extra_key");
    cursor += 1;
    if (extraKey === 2) {
      const datum = tag4BytesAt(bytes, cursor, "output.datum");
      datumCbor = datum.value;
      cursor = datum.nextOffset;
      if (entryCount === 4) {
        if (tag4ByteAt(bytes, cursor, "output.script_key") !== 3) {
          throw new Error("output script key must be three");
        }
        const script = tag4ScriptAt(bytes, cursor + 1, "output.script_ref");
        scriptRef = script.value;
        cursor = script.nextOffset;
      }
    } else {
      if (extraKey !== 3 || entryCount !== 3) {
        throw new Error("output optional fields have an invalid layout");
      }
      const script = tag4ScriptAt(bytes, cursor, "output.script_ref");
      scriptRef = script.value;
      cursor = script.nextOffset;
    }
  }
  if (cursor !== bytes.length) {
    throw new Error("output contains trailing bytes");
  }
  return {
    address: address.value,
    lovelace: value.lovelace,
    assets: value.assets,
    ...(datumCbor === undefined ? {} : { datumCbor }),
    ...(scriptRef === undefined ? {} : { scriptRef }),
  };
};

const tag4DefiniteMapHeader = (length: number): Buffer => {
  if (length < 0 || length > 0xffff_ffff) {
    throw new Error("Aiken V1 map length exceeds its encoding domain");
  }
  if (length <= 23) {
    return Buffer.from([0xa0 + length]);
  }
  if (length <= 0xff) {
    return Buffer.from([0xb8, length]);
  }
  if (length <= 0xffff) {
    const header = Buffer.alloc(3);
    header[0] = 0xb9;
    header.writeUInt16BE(length, 1);
    return header;
  }
  const header = Buffer.alloc(5);
  header[0] = 0xba;
  header.writeUInt32BE(length, 1);
  return header;
};

const encodeTag4Value = ({
  lovelace,
  assets,
}: Pick<Tag4Output, "lovelace" | "assets">): Buffer => {
  const groups: Tag4OutputAsset[][] = [];
  for (const asset of assets) {
    const previous = groups.at(-1);
    if (
      previous !== undefined &&
      previous[0]!.policyId.equals(asset.policyId)
    ) {
      previous.push(asset);
    } else {
      groups.push([asset]);
    }
  }
  return Buffer.concat([
    Buffer.from([0x82]),
    encodeCborUnsigned(lovelace),
    tag4DefiniteMapHeader(groups.length),
    ...groups.flatMap((group) => [
      encodeCborBytes(group[0]!.policyId),
      tag4DefiniteMapHeader(group.length),
      ...group.flatMap((asset) => [
        encodeCborBytes(asset.assetName),
        encodeCborUnsigned(asset.quantity),
      ]),
    ]),
  ]);
};

const encodeTag4Output = (output: Tag4Output): Buffer => {
  const entryCount =
    2 +
    (output.datumCbor === undefined ? 0 : 1) +
    (output.scriptRef === undefined ? 0 : 1);
  return Buffer.concat([
    Buffer.from([0xa0 + entryCount, 0]),
    encodeCborBytes(output.address),
    Buffer.from([1]),
    encodeTag4Value(output),
    ...(output.datumCbor === undefined
      ? []
      : [Buffer.from([2]), encodeCborBytes(output.datumCbor)]),
    ...(output.scriptRef === undefined
      ? []
      : [
          Buffer.from([3, 0x82]),
          encodeCborUnsigned(output.scriptRef.language),
          encodeCborBytes(output.scriptRef.bytes),
        ]),
  ]);
};

const canonicalNativeOutput = (bytes: Buffer, label: string): Buffer => {
  try {
    return encodeTag4Output(decodeTag4Output(bytes));
  } catch (cause) {
    throw transitionTraceError(
      "missingWitnessData",
      `${label} is not an Aiken V1 MidgardTxOutput.`,
      cause,
    );
  }
};

const nativePreimageItems = (
  preimageHex: string,
  label: string,
): readonly Buffer[] => {
  try {
    return decodeMidgardNativeByteListPreimage(
      exactHexBytes(preimageHex, label),
      label,
    );
  } catch (cause) {
    if (cause instanceof TransitionTraceChallengerError) {
      throw cause;
    }
    throw transitionTraceError(
      "missingWitnessData",
      `${label} is not a canonical native byte-list preimage.`,
      cause,
    );
  }
};

const authenticatedL2TransactionSource = (
  sourceMembership: SDK.L2TransactionSourceMembershipProof,
): {
  readonly compact: ReturnType<typeof decodeMidgardNativeTxCompactV1>;
  readonly txId: Buffer;
} => {
  try {
    const txId = exactHexBytes(
      sourceMembership.key,
      "L2 transaction source key",
    );
    if (txId.length !== 32) {
      throw new Error("source key must contain exactly 32 bytes");
    }
    const source = Data.from(
      exactHexBytes(
        sourceMembership.value,
        "L2 transaction source value",
      ).toString("hex"),
      SDK.L2TransactionSourceV1,
    );
    const sourceTxId = exactHexBytes(
      source.tx_id,
      "L2 transaction source value.tx_id",
    );
    if (!sourceTxId.equals(txId)) {
      throw new Error("source value tx_id does not match its membership key");
    }
    const compact = decodeMidgardNativeTxCompactV1(
      exactHexBytes(
        source.source.compact_cbor,
        "L2 transaction source compact",
      ),
    );
    if (compact.validity !== "TxIsValid") {
      throw new Error("source compact is not classified as TxIsValid");
    }
    return { compact, txId };
  } catch (cause) {
    if (cause instanceof TransitionTraceChallengerError) {
      throw cause;
    }
    throw transitionTraceError(
      "missingWitnessData",
      "L2 transaction source membership does not contain an authenticated canonical native transaction compact.",
      cause,
    );
  }
};

const requireCanonicalFieldCommitment = ({
  items,
  expectedCommitment,
  label,
}: {
  readonly items: readonly Buffer[];
  readonly expectedCommitment: Buffer;
  readonly label: string;
}): void => {
  // §4: the field commits to a flat `blake2b_256` of its §5.1 preimage, so the
  // check is over the assembled bytes rather than a counted per-item root — and
  // the field index is no longer an input, because §4 puts no field index in the
  // hash. Which field this *is* comes from `expectedCommitment`: the caller reads
  // it out of the authenticated compact structure, which is what §4's
  // positional-identity invariant requires and the only thing separating fields
  // 0 and 2 for identical content.
  const actualCommitment = midgardFieldCommitmentFromItemsV1(items);
  if (!actualCommitment.equals(expectedCommitment)) {
    throw transitionTraceError(
      "missingWitnessData",
      `${label} canonical field commitment does not match the authenticated transaction compact.`,
    );
  }
};

const replayL2TransactionTransition = (
  witness: Extract<
    SDK.InvalidOneStepTransitionWitness,
    { readonly L2TransactionTransition: unknown }
  >["L2TransactionTransition"],
): string => {
  const spendInputs = nativePreimageItems(
    witness.spend_inputs_preimage,
    "L2 transaction spend-input preimage",
  ).map((bytes, index) =>
    canonicalNativeSpendInput(
      bytes,
      `L2 transaction spend-input preimage[${index.toString()}]`,
    ),
  );
  const outputs = nativePreimageItems(
    witness.outputs_preimage,
    "L2 transaction output preimage",
  ).map((bytes, index) =>
    canonicalNativeOutput(
      bytes,
      `L2 transaction output preimage[${index.toString()}]`,
    ),
  );
  const { compact, txId } = authenticatedL2TransactionSource(
    witness.source_membership,
  );
  requireCanonicalFieldCommitment({
    items: spendInputs,
    expectedCommitment: compact.transactionBody.spendInputsHash,
    label: "L2 transaction spend-input preimage",
  });
  requireCanonicalFieldCommitment({
    items: outputs,
    expectedCommitment: compact.transactionBody.outputsHash,
    label: "L2 transaction output preimage",
  });
  if (witness.spent_utxos.length !== spendInputs.length) {
    throw transitionTraceError(
      "missingWitnessData",
      "L2 transaction delete witnesses must match the authenticated input count exactly.",
    );
  }
  if (witness.produced_utxos.length !== outputs.length) {
    throw transitionTraceError(
      "missingWitnessData",
      "L2 transaction insert witnesses must match the authenticated output count exactly.",
    );
  }

  let root = witness.trace_proof.value.pre_utxos_root;
  for (const [index, item] of witness.spent_utxos.entries()) {
    const label = `L2 transaction delete witness ${index.toString()}`;
    const key = exactHexBytes(item.key, `${label}.key`);
    const value = exactHexBytes(item.value, `${label}.value`);
    if (!key.equals(spendInputs[index]!)) {
      throw transitionTraceError(
        "missingWitnessData",
        `${label} is not ordered and bound to its authenticated spend input.`,
      );
    }
    const membershipProof = mpfProofFromWitness({
      key,
      value,
      proof: item.membership_proof,
      label: `${label}.membership_proof`,
    });
    const deleteProof = mpfProofFromWitness({
      key,
      value,
      proof: item.delete_proof,
      label: `${label}.delete_proof`,
    });
    verifyProofRoot({
      proof: membershipProof,
      includingItem: true,
      expectedRoot: root,
      label: `${label}.membership_proof`,
    });
    verifyProofRoot({
      proof: deleteProof,
      includingItem: true,
      expectedRoot: root,
      label: `${label}.delete_proof`,
    });
    try {
      root = normalizedMpfRoot(
        deleteProof.verify(false),
        `${label}.delete_proof`,
      );
    } catch (cause) {
      throw transitionTraceError(
        "missingWitnessData",
        `${label}.delete_proof cannot derive the post-delete root.`,
        cause,
      );
    }
  }

  for (const [index, item] of witness.produced_utxos.entries()) {
    const label = `L2 transaction insert witness ${index.toString()}`;
    if (index > 65_535) {
      throw transitionTraceError(
        "missingWitnessData",
        `${label} exceeds the canonical V1 output-index domain.`,
      );
    }
    const key = exactHexBytes(item.key, `${label}.key`);
    const value = exactHexBytes(item.value, `${label}.value`);
    // The ledger trie key is the §5.3 field-0/1 item form
    // (`82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`, 38 bytes), matching on-chain
    // `ledger_outref_key` / `encode_midgard_tx_input` — not CML's or
    // `encodeCbor`'s minimal-index shape.
    const expectedKey = encodeMidgardSpendInputItemV1({
      txId,
      outputIndex: index,
    });
    // The ledger trie *value* is the output's LedgerOutputCommitmentV1
    // descriptor, not the full output bytes (spec §5.3: "recomputed with that
    // descriptor as the MPT value, not with the full output bytes"). Every
    // other producer of `utxos_root` — the node, the validation machine, the
    // watcher's block replay and DA reconstruction — commits the descriptor,
    // and on-chain `apply_l2_outputs` derives the same descriptor from the
    // authenticated output. A challenger that submitted full output bytes here
    // would replay a root an honest block can never equal.
    let expectedValue: Buffer;
    try {
      expectedValue = Buffer.from(
        buildCanonicalMidgardLedgerOutputMaterialV1({
          outputIndex: index,
          outputCbor: outputs[index]!,
        }).descriptorCbor,
      );
    } catch (cause) {
      // An output the canonical ledger-output decoder refuses has no §5.3
      // descriptor, so it has no `utxos_root` value at all and this arm cannot
      // replay the insert. Fail closed with the challenger's own error rather
      // than letting a codec error escape: on-chain `apply_l2_outputs` binds
      // the same derivation with `expect`, so the arm aborts on these inputs
      // too and no witness built here could have minted.
      throw transitionTraceError(
        "missingWitnessData",
        `${label} names a transaction output with no canonical ledger value.`,
        cause,
      );
    }
    if (!key.equals(expectedKey) || !value.equals(expectedValue)) {
      throw transitionTraceError(
        "missingWitnessData",
        `${label} is not ordered and bound to its authenticated transaction output.`,
      );
    }
    const nonMembershipProof = mpfProofFromWitness({
      key,
      value: undefined,
      proof: item.non_membership_proof,
      label: `${label}.non_membership_proof`,
    });
    const insertProof = mpfProofFromWitness({
      key,
      value,
      proof: item.insert_proof,
      label: `${label}.insert_proof`,
    });
    verifyProofRoot({
      proof: nonMembershipProof,
      includingItem: false,
      expectedRoot: root,
      label: `${label}.non_membership_proof`,
    });
    verifyProofRoot({
      proof: insertProof,
      includingItem: false,
      expectedRoot: root,
      label: `${label}.insert_proof`,
    });
    try {
      root = normalizedMpfRoot(
        insertProof.verify(true),
        `${label}.insert_proof`,
      );
    } catch (cause) {
      throw transitionTraceError(
        "missingWitnessData",
        `${label}.insert_proof cannot derive the post-insert root.`,
        cause,
      );
    }
  }
  return root;
};

const detectL2TransactionTransitions = async (
  reconstruction: TransitionTraceReconstruction,
  evidence: readonly (L2TransactionTransitionEvidence & {
    readonly stepIndex: bigint;
  })[],
): Promise<readonly TransitionTraceDetection[]> => {
  const detections: TransitionTraceDetection[] = [];
  for (const item of evidence) {
    const witness = await buildL2TransactionTransitionWitness({
      reconstruction,
      stepIndex: item.stepIndex,
      evidence: item,
    });
    if (!("L2TransactionTransition" in witness)) {
      throw transitionTraceError(
        "missingWitnessData",
        "L2 transaction evidence did not build an L2 transaction witness.",
      );
    }
    const replayedPostRoot = replayL2TransactionTransition(
      witness.L2TransactionTransition,
    );
    if (
      replayedPostRoot ===
      witness.L2TransactionTransition.trace_proof.value.post_utxos_root
    ) {
      continue;
    }
    detections.push(
      detection({
        reconstruction,
        kind: "invalidOneStepTransition",
        invariant: "l2_transaction_transition_matches_authenticated_replay",
        diagnostic: `L2 transaction trace step ${item.stepIndex.toString()} has authenticated ledger mutation evidence whose replay disagrees with the committed post-state root.`,
        fault: SDK.invalidOneStepTransitionFault(witness),
      }),
    );
  }
  return detections;
};

const detectOmittedDueL1Events = async (
  reconstruction: TransitionTraceReconstruction,
  evidence: readonly OmittedDueL1EventEvidence[],
): Promise<readonly TransitionTraceDetection[]> => {
  const detections: TransitionTraceDetection[] = [];
  for (const item of evidence) {
    const eventKey =
      item.kind === "deposit"
        ? ({ DepositEventKey: { deposit_id: item.depositId } } as SDK.EventKey)
        : item.kind === "withdrawal"
          ? ({
              WithdrawalEventKey: { withdrawal_id: item.withdrawalId },
            } as SDK.EventKey)
          : ({
              ForcedTransactionEventKey: { tx_order_id: item.txOrderId },
            } as SDK.EventKey);
    const fingerprint = eventKeyFingerprint(eventKey);
    if (!reconstruction.sourceEventsByFingerprint.has(fingerprint)) {
      detections.push(
        detection({
          reconstruction,
          kind: "omittedDueL1Event",
          invariant: "due_l1_event_is_in_source_root",
          diagnostic: `Due ${item.kind} L1 event ${fingerprint} is absent from the committed ${eventKeyPhase(
            eventKey,
          )} source root.`,
          fault: await buildOmittedDueL1EventFault({
            reconstruction,
            evidence: item,
          }),
        }),
      );
    }
  }
  return detections;
};

const detectOutOfWindowSourceEvents = async (
  reconstruction: TransitionTraceReconstruction,
  evidence: readonly OutOfWindowSourceEventEvidence[],
): Promise<readonly TransitionTraceDetection[]> => {
  const detections: TransitionTraceDetection[] = [];
  for (const item of evidence) {
    const eventKey =
      item.kind === "deposit"
        ? ({ DepositEventKey: { deposit_id: item.depositId } } as SDK.EventKey)
        : item.kind === "withdrawal"
          ? ({
              WithdrawalEventKey: { withdrawal_id: item.withdrawalId },
            } as SDK.EventKey)
          : ({
              ForcedTransactionEventKey: { tx_order_id: item.txOrderId },
            } as SDK.EventKey);
    const fingerprint = eventKeyFingerprint(eventKey);
    if (reconstruction.sourceEventsByFingerprint.has(fingerprint)) {
      detections.push(
        detection({
          reconstruction,
          kind: "outOfWindowSourceEvent",
          invariant: "source_event_is_within_block_window",
          diagnostic: `Out-of-window ${item.kind} L1 event ${fingerprint} is present in the committed source root.`,
          fault: await buildOutOfWindowSourceEventFault({
            reconstruction,
            evidence: item,
          }),
        }),
      );
    }
  }
  return detections;
};

export const detectTransitionTraceFaults = async (
  reconstruction: TransitionTraceReconstruction,
  evidence: TransitionTraceDetectionEvidence = {},
): Promise<readonly TransitionTraceDetection[]> => [
  ...detectCountFaults(reconstruction),
  ...(await detectTraceBoundaryFaults(reconstruction)),
  ...(await detectTraceLinkFaults(reconstruction)),
  ...(await detectDuplicateTraceEvents(reconstruction)),
  ...(await detectEventToStepMismatches(reconstruction)),
  ...(await detectSourceMembershipMismatches(reconstruction)),
  ...(await detectInvalidNoOpTransitions(reconstruction)),
  ...(await detectL2TransactionTransitions(
    reconstruction,
    evidence.l2TransactionTransitions ?? [],
  )),
  ...detectAcceptedTransactionTransitionMismatches(
    reconstruction,
    evidence.acceptedTransactionTransitionMismatches ?? [],
  ),
  ...(await detectOmittedDueL1Events(
    reconstruction,
    evidence.omittedDueL1Events ?? [],
  )),
  ...(await detectOutOfWindowSourceEvents(
    reconstruction,
    evidence.outOfWindowSourceEvents ?? [],
  )),
];

export const detectFirstTransitionTraceFault = async (
  reconstruction: TransitionTraceReconstruction,
  evidence: TransitionTraceDetectionEvidence = {},
): Promise<TransitionTraceDetection | undefined> =>
  (await detectTransitionTraceFaults(reconstruction, evidence))[0];
