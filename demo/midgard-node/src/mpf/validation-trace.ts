/**
 * Deterministic validation-trace member construction and event-key set validation.
 */

import { decodeSingleCbor } from "@al-ft/midgard-core/codec/cbor";
import { type MidgardConsensusProfileV1 } from "@al-ft/midgard-core/consensus-profile-v1";
import { MidgardValidationPhase } from "@al-ft/midgard-core/validation-trace";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  validationAuxiliaryWitnessDataV1,
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
  readonly consensusProfile: MidgardConsensusProfileV1;
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
  readonly value: SDK.ValidationTraceDescriptorV1;
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
        const descriptor: SDK.ValidationTraceDescriptorV1 = {
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
          const decodedControl =
            witness.phase === "scriptSources" ||
            witness.phase === "scriptIntegrity" ||
            witness.phase === "valueAndMint"
              ? decodeSingleCbor(witness.cbor)
              : null;
          const scriptSourcesStage =
            witness.phase === "scriptSources" &&
            Array.isArray(decodedControl) &&
            (decodedControl.length === 30 || decodedControl.length === 31)
              ? BigInt(decodedControl[9] as bigint | number)
              : null;
          const retainedNativeExecution =
            witness.phase === "nativeScripts" &&
            witness.auxiliary?.kind === "nativeExecutionDescriptor";
          const retainedScriptSources =
            witness.phase === "scriptSources" &&
            (witness.auxiliary === null ||
              witness.auxiliary.kind === "scriptPurposeScan" ||
              witness.auxiliary.kind === "scriptSourceScan" ||
              ((witness.auxiliary.kind === "redeemerScanBegin" ||
                witness.auxiliary.kind === "redeemerItemStep") &&
                (scriptSourcesStage === 10n || scriptSourcesStage === 12n)));
          const retainedScriptIntegrityTerminal =
            witness.phase === "scriptIntegrity" &&
            witness.auxiliary === null &&
            Array.isArray(decodedControl) &&
            decodedControl.length === 4 &&
            BigInt(decodedControl[1] as bigint | number) === 3n;
          const retainedValueAndMintAsset =
            witness.phase === "valueAndMint" &&
            Array.isArray(decodedControl) &&
            decodedControl.length === 12 &&
            witness.auxiliary !== null &&
            ((witness.auxiliary.kind === "valueInputAsset" &&
              BigInt(decodedControl[1] as bigint | number) === 2n) ||
              (witness.auxiliary.kind === "valueOutputAsset" &&
                BigInt(decodedControl[1] as bigint | number) === 3n) ||
              (witness.auxiliary.kind === "valueMintAsset" &&
                BigInt(decodedControl[1] as bigint | number) === 4n));
          if (
            !retainedNativeExecution &&
            !retainedScriptSources &&
            !retainedScriptIntegrityTerminal &&
            !retainedValueAndMintAsset
          ) {
            return [];
          }
          // Non-negative coordinates remain the consensus execution indexes
          // consumed by the existing NativeScripts reconstruction API. The
          // chronological negative domain is reserved for ScriptSources
          // controls/frontier openings, the exact ScriptIntegrity stage-3
          // terminal control, and ValueAndMint asset mutations. This keeps
          // every retained phase collision-free
          // without making a caller-provided label part of witness authority.
          const retainedCoordinate = retainedNativeExecution
            ? BigInt(witness.auxiliary.executionIndex)
            : BigInt(stateIndex) - BigInt(trace.witnesses.length);
          const key: SDK.RetainedValidationWitnessKeyV1 = {
            event_key: transaction.eventKey,
            execution_index: retainedCoordinate,
          };
          const auxiliary = LucidData.from(
            LucidData.to(
              validationAuxiliaryWitnessDataV1(witness.auxiliary) as never,
            ),
            SDK.ValidationAuxiliaryWitnessV1Schema,
          ) as unknown as SDK.ValidationAuxiliaryWitnessV1;
          const value: SDK.RetainedValidationWitnessV1 = {
            machine_state: SDK.validationMachineStateDataFromCore(
              trace.states[stateIndex]!,
            ),
            trace_proof: SDK.validationTraceProofDataFromCore(
              trace.tree.proofs[stateIndex]!,
            ),
            phase: BigInt(
              witness.phase === "scriptSources"
                ? MidgardValidationPhase.scriptSources
                : witness.phase === "nativeScripts"
                  ? MidgardValidationPhase.nativeScripts
                  : witness.phase === "scriptIntegrity"
                    ? MidgardValidationPhase.scriptIntegrity
                    : MidgardValidationPhase.valueAndMint,
            ),
            program_counter: BigInt(witness.programCounter),
            witness_cbor: witness.cbor.toString("hex"),
            auxiliary,
          };
          return [
            [
              SDK.encodeRetainedValidationWitnessKeyV1(key).toString("hex"),
              SDK.encodeRetainedValidationWitnessV1(value).toString("hex"),
            ] satisfies SDK.DaPayloadEntry,
          ];
        });
        return {
          eventKey: transaction.eventKey,
          keyCbor,
          valueCbor: Buffer.from(
            LucidData.to(
              descriptor as never,
              SDK.ValidationTraceDescriptorV1 as never,
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
