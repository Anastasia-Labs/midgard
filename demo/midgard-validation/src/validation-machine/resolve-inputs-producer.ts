/**
 * The ValidityInterval and ResolveInputs producers: the resolution
 * schedule's head, then each scheduled input looked up in the pre-state
 * ledger, its output proved and its spend signer checked.
 */
import {
  attachMidgardLedgerOutputProofFacts,
  buildMidgardLedgerOutputProofTrace,
  encodeCbor,
  encodeMidgardLedgerOutputProofControl,
  type MidgardLedgerOutputProofControl,
  midgardLedgerOutputProofFactsComplete,
} from "@al-ft/midgard-core";
import {
  decodeMidgardAddressBytes,
  decodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { Effect } from "effect";

import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "../ledger-output-descriptor.js";
import { UNSET_REASON_COORDINATE } from "../rejection-reason.js";
import { noExactLedgerDescriptor } from "./input-resolution.js";
import { proofBinding } from "./prepare-producers.shared.js";
import {
  preparedSignerProof,
  type PreparedValidationTransaction,
} from "./prepared-validation-transaction.js";
import {
  phaseContinues,
  type PhaseOutcome,
  phaseStops,
  type TraceWalk,
} from "./trace-walk.js";
import type { ValidationMachineSignerSetProof } from "./types.js";

type ResolutionPrepared = Pick<
  PreparedValidationTransaction,
  | "proofSource"
  | "contextCbor"
  | "resolutionScheduleHash"
  | "signerFrontier"
  | "signerFrontierCommitment"
>;
type ScheduleNode =
  PreparedValidationTransaction["resolutionScheduleNodes"][number];

const resolutionWitnessCbor = (
  prepared: ResolutionPrepared,
  cursor: number,
  remainingScheduleHash: Buffer,
  pending?: {
    readonly node: ScheduleNode;
    readonly descriptorCbor: Buffer;
    readonly outputProof: MidgardLedgerOutputProofControl;
  },
): Buffer =>
  encodeCbor([
    ...proofBinding(prepared),
    BigInt(cursor),
    remainingScheduleHash,
    BigInt(prepared.signerFrontier.count),
    prepared.signerFrontierCommitment,
    pending === undefined
      ? Buffer.from([0])
      : encodeCbor([
          pending.node.sourceKind === "spend" ? 0n : 1n,
          BigInt(pending.node.itemIndex),
          pending.node.key,
          pending.node.nextScheduleHash,
          pending.descriptorCbor,
          encodeMidgardLedgerOutputProofControl(pending.outputProof),
        ]),
    prepared.resolutionScheduleHash,
  ]);

export const produceValidityInterval = (
  prepared: ResolutionPrepared &
    Pick<PreparedValidationTransaction, "phaseALedgerTx">,
  { push }: Pick<TraceWalk, "push">,
): Effect.Effect<PhaseOutcome, Error> =>
  Effect.gen(function* () {
    if (prepared.phaseALedgerTx === null) {
      return yield* Effect.fail(
        new Error(
          "V1 trace reached input resolution without a Phase A ledger transaction",
        ),
      );
    }
    push(
      "resolveInputs",
      resolutionWitnessCbor(prepared, 0, prepared.resolutionScheduleHash),
    );
    return phaseContinues;
  });

/** A key-locked spend's signer-set proof; none for any other input. */
const spendSignerProof = (
  prepared: Parameters<typeof preparedSignerProof>[0],
  node: ScheduleNode,
  outputCbor: Buffer,
): ValidationMachineSignerSetProof => {
  if (node.sourceKind === "reference") return { kind: "none" };
  let signerHash: Buffer;
  try {
    const output = decodeMidgardTxOutput(outputCbor);
    const credential = decodeMidgardAddressBytes(
      output.address,
    ).paymentCredential;
    if (credential.kind === "Script") return { kind: "none" };
    signerHash = Buffer.from(credential.hash);
  } catch {
    return { kind: "none" };
  }
  return preparedSignerProof(prepared, signerHash);
};

export const produceResolveInputs = (
  prepared: ResolutionPrepared &
    Pick<
      PreparedValidationTransaction,
      | "resolutionScheduleNodes"
      | "ledgerState"
      | "ledgerDescriptorState"
      | "canonicalSignerHashes"
      | "signerLeafHashes"
    >,
  { push }: Pick<TraceWalk, "push">,
): Effect.Effect<PhaseOutcome, Error> =>
  Effect.gen(function* () {
    let cursor = 1;
    let remainingScheduleHash = prepared.resolutionScheduleHash;
    for (const node of prepared.resolutionScheduleNodes) {
      if (!remainingScheduleHash.equals(node.scheduleHash)) {
        return yield* Effect.fail(
          new Error(
            "input-resolution schedule diverged from its committed hash chain",
          ),
        );
      }
      const pushStep = (
        auxiliary: Parameters<TraceWalk["push"]>[2],
        outputProof?: MidgardLedgerOutputProofControl,
        descriptorCbor: Buffer = Buffer.alloc(0),
      ): void =>
        push(
          "resolveInputs",
          resolutionWitnessCbor(
            prepared,
            cursor,
            remainingScheduleHash,
            outputProof === undefined
              ? undefined
              : { node, descriptorCbor, outputProof },
          ),
          auxiliary,
        );
      const lookup = (value: Buffer | null) => ({
        kind: "scheduledLedgerLookup" as const,
        sourceKind: node.sourceKind,
        itemIndex: node.itemIndex,
        key: node.key,
        nextScheduleHash: node.nextScheduleHash,
        value,
        proofCbor: node.proofCbor,
        signerProof: { kind: "none" as const },
      });
      const outRefHex = node.key.toString("hex");
      const outputCbor = prepared.ledgerState.get(outRefHex);
      const descriptorCbor = prepared.ledgerDescriptorState.get(outRefHex);
      if (outputCbor === undefined || descriptorCbor === undefined) {
        pushStep(lookup(null));
        return phaseStops(
          {
            InputNotFound: {
              source_kind: node.sourceKind === "spend" ? 0n : 1n,
              input_index: UNSET_REASON_COORDINATE,
            },
          },
          "input resolution found no ledger member",
        );
      }

      const outputIndex = yield* Effect.try({
        try: () =>
          buildCanonicalMidgardLedgerEntryOutputMaterial({
            outRef: node.key,
            outputCbor,
          }).descriptor.outputIndex,
        catch: () => noExactLedgerDescriptor(outRefHex),
      });
      const outputProof = buildMidgardLedgerOutputProofTrace({
        outputIndex,
        outputCbor,
      });
      pushStep(lookup(descriptorCbor));
      for (const proofStep of outputProof.steps) {
        pushStep(
          { kind: "ledgerOutputProofStep", witness: proofStep.witness },
          proofStep.control,
          descriptorCbor,
        );
      }
      // Chained descriptor-fact attachment: each canonical group is one
      // checkpointed machine step over the same finalize witness shape,
      // its commitments derived from the terminal control alone; the thin
      // terminal finalize below requires the scan fact to commit exactly
      // the resolved descriptor.
      let factsProof = outputProof.terminal;
      while (!midgardLedgerOutputProofFactsComplete(factsProof)) {
        pushStep(
          { kind: "ledgerOutputProofFinalize", signerProof: { kind: "none" } },
          factsProof,
          descriptorCbor,
        );
        const attached = attachMidgardLedgerOutputProofFacts(factsProof);
        if (attached === null) {
          return yield* Effect.fail(
            new Error("ledger output proof fact attachment failed closed"),
          );
        }
        factsProof = attached;
      }
      const signerProof = spendSignerProof(prepared, node, outputCbor);
      pushStep(
        { kind: "ledgerOutputProofFinalize", signerProof },
        factsProof,
        descriptorCbor,
      );
      // A key-locked spend whose signer is absent stops the machine here,
      // so validation must have refused this spend for the same fault.
      if (signerProof.kind !== "membership" && signerProof.kind !== "none") {
        return phaseStops(
          { SpendInputSignerMissing: { input_index: UNSET_REASON_COORDINATE } },
          "spend signer is missing",
        );
      }

      remainingScheduleHash = node.nextScheduleHash;
      cursor += 1;
    }

    push(
      "resolveInputs",
      resolutionWitnessCbor(prepared, cursor, remainingScheduleHash),
    );
    return phaseContinues;
  });
