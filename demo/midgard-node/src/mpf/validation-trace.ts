/**
 * Deterministic validation-trace member construction and event-key set validation.
 */

import { type MidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";
import { MidgardValidationPhase } from "@al-ft/midgard-core/validation-trace";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  validationAuxiliaryWitnessData,
} from "@al-ft/midgard-validation";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import * as PendingBlockFinalizationsDB from "../database/pendingBlockFinalizations.js";
import { DatabaseError } from "../database/utils/common.js";
import { MpfError } from "./errors.js";
import {
  eventKeyCbor,
  type ValidationTraceTransactionInput,
} from "./trace-events.js";

export type ValidationTraceBuildInput = {
  readonly consensusProfile: MidgardConsensusProfile;
  readonly blockEndTime: Date;
  readonly expectedNetworkId: bigint;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  readonly blockSlot: bigint;
  readonly transactions: readonly ValidationTraceTransactionInput[];
};

export type RetainedValidationTraceMember = {
  readonly eventKey: SDK.EventKey;
  readonly keyCbor: Buffer;
  readonly valueCbor: Buffer;
  readonly value: SDK.ValidationTraceDescriptor;
  readonly witnesses: readonly SDK.DaPayloadEntry[];
};

export type ValidationTraceBuildResult = {
  readonly validationTracesRoot: string;
  readonly validationTraceMembers: readonly RetainedValidationTraceMember[];
  readonly validationTraceCount: number;
};

export const buildDeterministicValidationTraceMembers = (
  input: ValidationTraceBuildInput,
): Effect.Effect<
  readonly RetainedValidationTraceMember[],
  DatabaseError | MpfError
> =>
  Effect.forEach(
    input.transactions,
    (transaction) =>
      Effect.gen(function* () {
        const keyCbor = yield* eventKeyCbor(transaction.eventKey);
        const trace = yield* buildDeterministicValidationMachineTrace({
          consensusProfile: input.consensusProfile,
          eventKeyCbor: keyCbor,
          transactionId: transaction.transactionId,
          canonicalTransactionCbor: transaction.canonicalTransactionCbor,
          programMaterialSidecarCbor: transaction.programMaterialSidecarCbor,
          sourceKind: transaction.sourceKind,
          priorUtxosRoot: transaction.priorUtxosRoot,
          postUtxosRoot: transaction.postUtxosRoot,
          ledgerWitnessEntries: transaction.ledgerWitnessEntries,
          ledgerMutationSteps: transaction.ledgerMutationSteps,
          expectedLedgerOps: transaction.ledgerOps,
          expectedVerdict: transaction.verdict,
          expectedRejectionCode: transaction.rejectionCode,
          blockEndTimeMs: input.blockEndTime.getTime(),
          expectedNetworkId: input.expectedNetworkId,
          minFeeA: input.minFeeA,
          minFeeB: input.minFeeB,
          blockSlot: input.blockSlot,
        }).pipe(
          Effect.mapError(
            (cause) =>
              new DatabaseError({
                table: PendingBlockFinalizationsDB.tableName,
                message:
                  "Deterministic validation-machine replay failed while building a V1 block",
                cause,
              }),
          ),
        );
        const descriptor: SDK.ValidationTraceDescriptor = {
          schema_version: BigInt(trace.tree.descriptor.schemaVersion),
          machine_version: BigInt(trace.tree.descriptor.machineVersion),
          trace_root: trace.tree.descriptor.traceRoot.toString("hex"),
          step_count: BigInt(trace.tree.descriptor.stepCount),
          initial_state_hash:
            trace.tree.descriptor.initialStateHash.toString("hex"),
          terminal_state_hash:
            trace.tree.descriptor.terminalStateHash.toString("hex"),
          verdict:
            trace.tree.descriptor.verdict === "accepted"
              ? "Accepted"
              : "Rejected",
          rejection_code_hash:
            trace.tree.descriptor.rejectionCodeHash.toString("hex"),
        };
        const witnesses = trace.witnesses.flatMap((witness, stateIndex) => {
          const retainedNativeExecution =
            witness.phase === "nativeScripts" &&
            witness.auxiliary?.kind === "nativeExecutionDescriptor";
          const retainedCoordinate = SDK.retainedValidationStateCoordinate(
            descriptor.step_count,
            BigInt(stateIndex),
          );
          const key: SDK.RetainedValidationWitnessKey = {
            event_key: transaction.eventKey,
            execution_index: retainedCoordinate,
          };
          const auxiliary = LucidData.from(
            LucidData.to(
              validationAuxiliaryWitnessData(witness.auxiliary) as never,
            ),
            SDK.ValidationAuxiliaryWitnessSchema,
          ) as unknown as SDK.ValidationAuxiliaryWitness;
          const value: SDK.RetainedValidationWitness = {
            machine_state: SDK.validationMachineStateDataFromCore(
              trace.states[stateIndex]!,
            ),
            trace_proof: SDK.validationTraceProofDataFromCore(
              trace.tree.proofs[stateIndex]!,
            ),
            phase: BigInt(MidgardValidationPhase[witness.phase]),
            program_counter: BigInt(witness.programCounter),
            witness_cbor: witness.cbor.toString("hex"),
            auxiliary,
          };
          const keys = retainedNativeExecution
            ? [
                key,
                {
                  ...key,
                  execution_index: BigInt(witness.auxiliary.executionIndex),
                },
              ]
            : [key];
          return keys.map(
            (coordinate) =>
              [
                SDK.encodeRetainedValidationWitnessKey(coordinate).toString(
                  "hex",
                ),
                SDK.encodeRetainedValidationWitness(value).toString("hex"),
              ] satisfies SDK.DaPayloadEntry,
          );
        });
        const terminalIndex = trace.states.length - 1;
        const endpoints = (["initial", "terminal"] as const).map((endpoint) => {
          const stateIndex = endpoint === "initial" ? 0 : terminalIndex;
          const sourceWitness = trace.witnesses[stateIndex]!;
          const value: SDK.RetainedValidationWitness = {
            machine_state: SDK.validationMachineStateDataFromCore(
              trace.states[stateIndex]!,
            ),
            trace_proof: SDK.validationTraceProofDataFromCore(
              trace.tree.proofs[stateIndex]!,
            ),
            phase:
              endpoint === "initial"
                ? -1n
                : BigInt(MidgardValidationPhase[sourceWitness.phase]),
            program_counter: BigInt(sourceWitness.programCounter),
            witness_cbor: (endpoint === "initial"
              ? trace.validationContextCbor
              : sourceWitness.cbor
            ).toString("hex"),
            auxiliary: "NoAuxiliaryWitness",
          };
          return [
            SDK.encodeRetainedValidationWitnessKey({
              event_key: transaction.eventKey,
              execution_index: SDK.retainedValidationEndpointCoordinate(
                descriptor.step_count,
                endpoint,
              ),
            }).toString("hex"),
            SDK.encodeRetainedValidationWitness(value).toString("hex"),
          ] satisfies SDK.DaPayloadEntry;
        });
        witnesses.push(...endpoints);
        if (new Set(witnesses.map(([key]) => key)).size !== witnesses.length) {
          return yield* Effect.fail(
            new DatabaseError({
              table: PendingBlockFinalizationsDB.tableName,
              message: "Validation trace retained coordinates are not unique",
              cause: transaction.eventKey,
            }),
          );
        }
        return {
          eventKey: transaction.eventKey,
          keyCbor,
          valueCbor: Buffer.from(
            LucidData.to(
              descriptor as never,
              SDK.ValidationTraceDescriptor as never,
            ),
            "hex",
          ),
          value: descriptor,
          witnesses,
        };
      }),
    { concurrency: 1 },
  );

export const validateValidationTraceEventKeySet = ({
  expectedEventKeys,
  transitionEventKeyCbors,
  members,
}: {
  readonly expectedEventKeys: readonly SDK.EventKey[];
  readonly transitionEventKeyCbors: ReadonlySet<string>;
  readonly members: readonly Pick<
    RetainedValidationTraceMember,
    "eventKey" | "keyCbor"
  >[];
}): Effect.Effect<void, DatabaseError | MpfError> =>
  Effect.gen(function* () {
    const expected = new Set<string>();
    for (const eventKey of expectedEventKeys) {
      const keyHex = (yield* eventKeyCbor(eventKey)).toString("hex");
      if (expected.has(keyHex)) {
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message:
              "Validation trace inputs contain a duplicate canonical event key",
            cause: `event_key_cbor=${keyHex}`,
          }),
        );
      }
      expected.add(keyHex);
    }
    if (members.length !== expected.size) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Validation trace provider returned the wrong descriptor count",
          cause: `expected=${expected.size.toString()},actual=${members.length.toString()}`,
        }),
      );
    }

    const seen = new Set<string>();
    for (const member of members) {
      const keyHex = member.keyCbor.toString("hex");
      if (
        seen.has(keyHex) ||
        !expected.has(keyHex) ||
        !transitionEventKeyCbors.has(keyHex) ||
        !member.keyCbor.equals(yield* eventKeyCbor(member.eventKey))
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message:
              "Validation trace provider returned a duplicate, foreign, or non-canonical event key",
            cause: `event_key_cbor=${keyHex}`,
          }),
        );
      }
      seen.add(keyHex);
    }
    if (seen.size !== expected.size) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Validation trace provider omitted a required canonical event key",
          cause: `expected=${expected.size.toString()},actual=${seen.size.toString()}`,
        }),
      );
    }
  });
