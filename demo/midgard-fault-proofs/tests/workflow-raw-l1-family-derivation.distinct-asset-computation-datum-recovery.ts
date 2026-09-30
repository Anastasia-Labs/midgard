import "./workflow-raw-l1-family-derivation.raw-l1-family-terminal-economics.js";

import {
  acceptedVerdictSubject,
  castConfirmedStateToData,
  encodeLinkedListNodeView,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FraudProofComputationThreadStepDatum,
  makeGenesisConfirmedState,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
} from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  DISTINCT_ASSET_ACCUMULATION_STEP_DATUM_SCHEMAS,
  DistinctAssetStep02DatumSchema,
  DistinctAssetStep03DatumSchema,
  DistinctAssetStep04DatumSchema,
  DistinctAssetStep05DatumSchema,
  DistinctAssetStep06DatumSchema,
} from "../src/distinct-asset-accumulation-limit/schemas.js";
import {
  deriveFraudProofRawL1FamilyStage,
  type FraudProofRawL1FamilyDefinition,
  type FraudProofRawL1Snapshot,
} from "../src/workflow/index.js";
import {
  fixture,
  hash32,
  output,
  policy,
  PROVER,
  raw,
  releaseEconomics,
  scriptAddress,
} from "./support/raw-l1-terminal-fixture.js";

describe("distinct-asset computation datum recovery", () => {
  it.each([
    [1, FraudProofComputationThreadStepDatum],
    [2, DistinctAssetStep02DatumSchema],
    [3, DistinctAssetStep03DatumSchema],
    [4, DistinctAssetStep04DatumSchema],
    [5, DistinctAssetStep05DatumSchema],
    [6, DistinctAssetStep06DatumSchema],
  ] as const)(
    "observes validator %i with its builder datum and preserves prover/schema checks",
    async (step, builderSchema) => {
      const value = await fixture();
      const definition: FraudProofRawL1FamilyDefinition = {
        ...value.definition,
        category: "distinctAssetAccumulationLimit",
        categoryId:
          FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.distinctAssetAccumulationLimit,
        computationThread: {
          ...value.definition.computationThread,
          steps: DISTINCT_ASSET_ACCUMULATION_STEP_DATUM_SCHEMAS.map(
            (datumSchema, index) => ({
              role: `computation_thread_step_0${index + 1}` as FraudProofRawL1FamilyDefinition["computationThread"]["steps"][number]["role"],
              address: scriptAddress((0x35 + index).toString(16)),
              datumSchema,
            }),
          ),
        },
      };
      const bound = {
        subject: acceptedVerdictSubject(hash32("71")),
        validation_traces_root: hash32("72"),
        validation_trace_count: 1n,
        coordinate: { fold: 2n, primary_index: 0n, asset_index: 0n },
      };
      const datum = Data.to(
        {
          fraud_prover: PROVER,
          data:
            step === 1
              ? null
              : step === 2
                ? bound
                : {
                    bound,
                    control: null,
                    stage: BigInt(step - 3),
                    decisive_fault_holds: true,
                  },
        } as never,
        builderSchema as never,
      );
      const threadUnit = toUnit(
        definition.computationThread.policyId,
        definition.categoryId + definition.headerHash,
      );
      const thread = raw(
        `${hash32("73")}#0`,
        output({
          address: definition.computationThread.steps[step - 1]!.address,
          assets: { lovelace: 3_000_000n, [threadUnit]: 1n },
          datum,
        }),
      );
      const target = value.snapshot.transactions[0]!.resolvedInputs[0]!;
      const root = raw(
        `${hash32("74")}#0`,
        output({
          address: definition.stateQueue.address,
          assets: {
            lovelace: 3_000_000n,
            [toUnit(
              definition.stateQueue.policyId,
              STATE_QUEUE_ROOT_ASSET_NAME,
            )]: 1n,
          },
          datum: encodeLinkedListNodeView({
            key: "Empty",
            next: { Key: { key: definition.headerHash } },
            data: castConfirmedStateToData(
              makeGenesisConfirmedState(0n),
            ) as never,
          }),
        }),
      );
      const snapshot: FraudProofRawL1Snapshot = {
        ...value.snapshot,
        historyUnits: [
          toUnit(
            definition.stateQueue.policyId,
            STATE_QUEUE_NODE_ASSET_NAME_PREFIX + definition.headerHash,
          ),
          threadUnit,
          toUnit(
            definition.proofToken.policyId,
            definition.categoryId + definition.headerHash,
          ),
        ],
        scopes: [
          ...value.snapshot.scopes
            .filter(({ role }) => !role.startsWith("computation_thread_step_"))
            .map((scope) =>
              scope.role === "state_queue"
                ? { ...scope, utxos: [root, target] }
                : scope.role === "permanent_proof_token"
                  ? { ...scope, utxos: [] }
                  : scope,
            ),
          ...definition.computationThread.steps.map((entry, index) => ({
            role: entry.role,
            address: entry.address,
            utxos: index === step - 1 ? [thread] : [],
          })),
        ],
      };
      await expect(
        deriveFraudProofRawL1FamilyStage({
          snapshot,
          definition,
          releaseEconomics,
        }),
      ).resolves.toMatchObject({
        kind: "step",
        step,
        threadOutRef: thread.outRef,
      });
      await expect(
        deriveFraudProofRawL1FamilyStage({
          snapshot,
          definition: { ...definition, proverCredential: policy("ff") },
          releaseEconomics,
        }),
      ).rejects.toThrow("owned by another fraud prover");
      if (step > 1) {
        const malformed = raw(
          thread.outRef,
          output({
            address: definition.computationThread.steps[step - 1]!.address,
            assets: { lovelace: 3_000_000n, [threadUnit]: 1n },
            datum: Data.to(
              { fraud_prover: PROVER, data: 42n },
              FraudProofComputationThreadStepDatum,
            ),
          }),
        );
        await expect(
          deriveFraudProofRawL1FamilyStage({
            snapshot: {
              ...snapshot,
              scopes: snapshot.scopes.map((scope) => ({
                ...scope,
                utxos: scope.utxos.map((utxo) =>
                  utxo.outRef === thread.outRef ? malformed : utxo,
                ),
              })),
            },
            definition,
            releaseEconomics,
          }),
        ).rejects.toThrow();
      }
    },
  );
});
