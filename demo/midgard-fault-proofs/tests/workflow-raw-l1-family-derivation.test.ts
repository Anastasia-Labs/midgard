import {
  acceptedVerdictSubject,
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  castConfirmedStateToData,
  encodeLinkedListNodeView,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FraudProofComputationThreadStepDatum,
  FraudProofTokenDatum,
  makeGenesisConfirmedState,
  RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
} from "@al-ft/midgard-sdk";
import { CML, Data, toUnit } from "@lucid-evolution/lucid";
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
  computeFraudProofRawL1PointId,
  computeFraudProofWorkflowId,
  deriveAuthenticatedStateQueueHeaderObservationFromRawL1,
  deriveFraudProofRawL1CompletedTerminal,
  deriveFraudProofRawL1FamilyStage,
  deriveRetainedStateQueueHeaderObservationFromRawL1,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofFamilyL1ObservationPort,
  type FraudProofRawL1FamilyDefinition,
  type FraudProofRawL1Snapshot,
  type FraudProofRawL1Utxo,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowTerminal,
  journalJsonDigest,
  observeFraudProofWorkflowHeader,
  StateQueueHeaderNotLiveError,
  verifyCompletedFraudProofWorkflow,
} from "../src/workflow/index.js";
import {
  DEPLOYMENT,
  fixture,
  hash32,
  input,
  OPERATOR,
  output,
  policy,
  PROVER,
  raw,
  releaseEconomics,
  releaseFinality,
  scriptAddress,
} from "./support/raw-l1-terminal-fixture.js";
import { rollBackTerminalFixture } from "./support/raw-l1-terminal-fixture.js";

describe("raw L1 live header observation", () => {
  it("binds a live header to its NFT mint even after a later transaction re-created its output", async () => {
    const { snapshot, definition } = await fixture();
    const target = snapshot.transactions[0]!.resolvedInputs[0]!;
    const targetOutput = CML.TransactionOutput.from_cbor_hex(target.outputCbor);
    const stateUnit = toUnit(
      definition.stateQueue.policyId,
      STATE_QUEUE_NODE_ASSET_NAME_PREFIX + definition.headerHash,
    );
    const mintOutputs = CML.TransactionOutputList.new();
    mintOutputs.add(targetOutput);
    const mintBody = CML.TransactionBody.new(
      CML.TransactionInputList.new(),
      mintOutputs,
      0n,
    );
    const mint = CML.Mint.new();
    mint.set(
      CML.ScriptHash.from_hex(definition.stateQueue.policyId),
      CML.AssetName.from_hex(stateUnit.slice(56)),
      1n,
    );
    mintBody.set_mint(mint);
    const mintTxHash = CML.hash_transaction(mintBody).to_hex();
    const commitPointInput = {
      slot: "900",
      blockHash: hash32("53"),
      blockNo: "60",
    };
    const commitPoint = {
      ...commitPointInput,
      pointId: computeFraudProofRawL1PointId(commitPointInput),
    };
    const template = snapshot.transactions[0]!;
    const commit = {
      ...template,
      txHash: mintTxHash,
      bodyCbor: mintBody.to_canonical_cbor_hex(),
      inclusionPoint: commitPoint,
      confirmationDepth: 41,
      resolvedInputs: [],
      resolvedReferenceInputs: [],
    };
    const recreateInputs = CML.TransactionInputList.new();
    recreateInputs.add(input(`${mintTxHash}#0`));
    const recreateOutputs = CML.TransactionOutputList.new();
    recreateOutputs.add(targetOutput);
    const recreateBody = CML.TransactionBody.new(
      recreateInputs,
      recreateOutputs,
      0n,
    );
    const recreateTxHash = CML.hash_transaction(recreateBody).to_hex();
    const attestation = {
      ...template,
      txHash: recreateTxHash,
      bodyCbor: recreateBody.to_canonical_cbor_hex(),
      resolvedInputs: [raw(`${mintTxHash}#0`, targetOutput)],
      resolvedReferenceInputs: [],
    };
    const root = raw(
      `${hash32("44")}#0`,
      output({
        address: definition.stateQueue.address,
        assets: {
          lovelace: 3_000_000n,
          [toUnit(definition.stateQueue.policyId, STATE_QUEUE_ROOT_ASSET_NAME)]:
            1n,
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
    const live = (
      headerUtxo: FraudProofRawL1Utxo,
      transactions: FraudProofRawL1Snapshot["transactions"],
    ): FraudProofRawL1Snapshot => ({
      ...snapshot,
      scopes: snapshot.scopes.map((scope) =>
        scope.role === "state_queue"
          ? { ...scope, utxos: [root, headerUtxo] }
          : scope,
      ) as FraudProofRawL1Snapshot["scopes"],
      transactions,
    });
    const observed =
      await deriveAuthenticatedStateQueueHeaderObservationFromRawL1({
        snapshot: live(raw(`${recreateTxHash}#0`, targetOutput), [
          commit,
          attestation,
        ]),
        definition,
      });
    expect(observed).toMatchObject({
      headerHash: definition.headerHash,
      chainPoint: { slot: 900n, blockHash: hash32("53") },
      confirmationDepth: 41,
    });
    await expect(
      deriveAuthenticatedStateQueueHeaderObservationFromRawL1({
        snapshot: live(raw(`${mintTxHash}#0`, targetOutput), [commit]),
        definition,
      }),
    ).resolves.toEqual(observed);
    await expect(
      deriveAuthenticatedStateQueueHeaderObservationFromRawL1({
        snapshot: live(raw(`${recreateTxHash}#0`, targetOutput), [attestation]),
        definition,
      }),
    ).rejects.toThrow("one authenticated NFT mint");
  });
});

describe("raw L1 family terminal economics", () => {
  it("reopens a removed header from its exact NFT mint and rejects absent or duplicated mints", async () => {
    const { snapshot, definition } = await fixture();
    const removal = snapshot.transactions[0]!;
    const target = removal.resolvedInputs[0]!;
    const outputs = CML.TransactionOutputList.new();
    outputs.add(CML.TransactionOutput.from_cbor_hex(target.outputCbor));
    const body = CML.TransactionBody.new(
      CML.TransactionInputList.new(),
      outputs,
      0n,
    );
    const mint = CML.Mint.new();
    mint.set(
      CML.ScriptHash.from_hex(definition.stateQueue.policyId),
      CML.AssetName.from_hex(
        STATE_QUEUE_NODE_ASSET_NAME_PREFIX + definition.headerHash,
      ),
      1n,
    );
    body.set_mint(mint);
    const creation = {
      ...removal,
      txHash: CML.hash_transaction(body).to_hex(),
      bodyCbor: body.to_canonical_cbor_hex(),
      resolvedInputs: [],
      resolvedReferenceInputs: [],
    };
    const retained = {
      ...snapshot,
      transactions: [creation, ...snapshot.transactions],
    };
    await expect(
      deriveAuthenticatedStateQueueHeaderObservationFromRawL1({
        snapshot: retained,
        definition,
      }),
    ).rejects.toThrow(StateQueueHeaderNotLiveError);
    await expect(
      deriveRetainedStateQueueHeaderObservationFromRawL1({
        snapshot: retained,
        definition,
      }),
    ).resolves.toMatchObject({
      headerHash: definition.headerHash,
      confirmationDepth: 30,
    });
    await expect(
      deriveRetainedStateQueueHeaderObservationFromRawL1({
        snapshot,
        definition,
      }),
    ).rejects.toThrow("one authenticated NFT mint");
    // A workflow that already removed its header resumes from the mint
    // history; every other observation failure still propagates.
    const port = (
      live: () => Promise<never>,
      retained?: FraudProofFamilyL1ObservationPort<"doubleSpend">["observeRetainedHeader"],
    ) =>
      ({
        observeHeader: live,
        ...(retained === undefined ? {} : { observeRetainedHeader: retained }),
      }) as unknown as FraudProofFamilyL1ObservationPort<"doubleSpend">;
    const fromMint = () =>
      deriveRetainedStateQueueHeaderObservationFromRawL1({
        snapshot: retained,
        definition,
      });
    const notLive = () => Promise.reject(new StateQueueHeaderNotLiveError());
    await expect(
      observeFraudProofWorkflowHeader(port(notLive, fromMint), {
        headerHash: definition.headerHash,
      }),
    ).resolves.toStrictEqual(await fromMint());
    await expect(
      observeFraudProofWorkflowHeader(port(notLive), {
        headerHash: definition.headerHash,
      }),
    ).rejects.toThrow(StateQueueHeaderNotLiveError);
    await expect(
      observeFraudProofWorkflowHeader(
        port(() => Promise.reject(new Error("provider unavailable")), fromMint),
        { headerHash: definition.headerHash },
      ),
    ).rejects.toThrow("provider unavailable");
    await expect(
      deriveRetainedStateQueueHeaderObservationFromRawL1({
        snapshot: {
          ...retained,
          transactions: [creation, ...retained.transactions],
        },
        definition,
      }),
    ).rejects.toThrow("one authenticated NFT mint");
  });

  it("derives a live sixth computation step from exact scoped bytes", async () => {
    const value = await fixture();
    const extraAddresses = [scriptAddress("3a"), scriptAddress("3b")];
    const definition: FraudProofRawL1FamilyDefinition = {
      ...value.definition,
      computationThread: {
        ...value.definition.computationThread,
        steps: [
          ...value.definition.computationThread.steps,
          {
            role: "computation_thread_step_05",
            address: extraAddresses[0]!,
            datumSchema: FraudProofTokenDatum,
          },
          {
            role: "computation_thread_step_06",
            address: extraAddresses[1]!,
            datumSchema: FraudProofTokenDatum,
          },
        ],
      },
    };
    const threadUnit = toUnit(
      definition.computationThread.policyId,
      `${definition.categoryId}${definition.headerHash}`,
    );
    const thread = raw(
      `${hash32("61")}#0`,
      output({
        address: extraAddresses[1]!,
        assets: { lovelace: 3_000_000n, [threadUnit]: 1n },
        datum: Data.to({ fraud_prover: PROVER }, FraudProofTokenDatum),
      }),
    );
    const target = value.snapshot.transactions[0]!.resolvedInputs[0]!;
    const root = raw(
      `${hash32("62")}#0`,
      output({
        address: definition.stateQueue.address,
        assets: {
          lovelace: 3_000_000n,
          [toUnit(definition.stateQueue.policyId, STATE_QUEUE_ROOT_ASSET_NAME)]:
            1n,
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
      scopes: value.snapshot.scopes
        .map((scope) => {
          if (scope.role === "state_queue") {
            return { ...scope, utxos: [root, target] };
          }
          if (scope.role === "permanent_proof_token") {
            return { ...scope, utxos: [] };
          }
          return scope;
        })
        .concat([
          {
            role: "computation_thread_step_05",
            address: extraAddresses[0]!,
            utxos: [],
          },
          {
            role: "computation_thread_step_06",
            address: extraAddresses[1]!,
            utxos: [thread],
          },
        ]),
    };
    await expect(
      deriveFraudProofRawL1FamilyStage({
        snapshot,
        definition,
        releaseEconomics,
      }),
    ).resolves.toMatchObject({
      kind: "step",
      step: 6,
      threadOutRef: thread.outRef,
    });
  });

  it("derives a live ninth transition-trace final without truncating the exact role set", async () => {
    const value = await fixture();
    const extraAddresses = ["3a", "3b", "3c", "3d", "3e"].map(scriptAddress);
    const extraSteps = extraAddresses.map((address, index) => ({
      role: `computation_thread_step_0${(index + 5).toString()}` as
        | "computation_thread_step_05"
        | "computation_thread_step_06"
        | "computation_thread_step_07"
        | "computation_thread_step_08"
        | "computation_thread_step_09",
      address,
      datumSchema: FraudProofTokenDatum,
    }));
    const definition: FraudProofRawL1FamilyDefinition = {
      ...value.definition,
      computationThread: {
        ...value.definition.computationThread,
        steps: [...value.definition.computationThread.steps, ...extraSteps],
      },
    };
    const threadUnit = toUnit(
      definition.computationThread.policyId,
      `${definition.categoryId}${definition.headerHash}`,
    );
    const thread = raw(
      `${hash32("63")}#0`,
      output({
        address: extraAddresses[4]!,
        assets: { lovelace: 3_000_000n, [threadUnit]: 1n },
        datum: Data.to({ fraud_prover: PROVER }, FraudProofTokenDatum),
      }),
    );
    const target = value.snapshot.transactions[0]!.resolvedInputs[0]!;
    const root = raw(
      `${hash32("64")}#0`,
      output({
        address: definition.stateQueue.address,
        assets: {
          lovelace: 3_000_000n,
          [toUnit(definition.stateQueue.policyId, STATE_QUEUE_ROOT_ASSET_NAME)]:
            1n,
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
      scopes: value.snapshot.scopes
        .map((scope) =>
          scope.role === "state_queue"
            ? { ...scope, utxos: [root, target] }
            : scope.role === "permanent_proof_token"
              ? { ...scope, utxos: [] }
              : scope,
        )
        .concat(
          extraSteps.map((step, index) => ({
            role: step.role,
            address: step.address,
            utxos: index === 4 ? [thread] : [],
          })),
        ),
    };
    await expect(
      deriveFraudProofRawL1FamilyStage({
        snapshot,
        definition,
        releaseEconomics,
      }),
    ).resolves.toMatchObject({
      kind: "step",
      step: 9,
      threadOutRef: thread.outRef,
    });
  });

  it("rejects a reordered computation-step authority definition", async () => {
    const value = await fixture();
    const [first, second, ...rest] = value.definition.computationThread.steps;
    await expect(
      deriveFraudProofRawL1FamilyStage({
        snapshot: value.snapshot,
        definition: {
          ...value.definition,
          computationThread: {
            ...value.definition.computationThread,
            steps: [second!, first!, ...rest],
          },
        },
        releaseEconomics,
      }),
    ).rejects.toThrow(/canonically ordered computation steps/u);
  });

  it("derives the exact release-bound slash/reward from transaction bytes", async () => {
    const value = await fixture();
    await expect(
      deriveFraudProofRawL1FamilyStage({
        snapshot: value.snapshot,
        definition: value.definition,
        releaseEconomics,
      }),
    ).resolves.toMatchObject({
      kind: "removed",
      terminal: {
        correction: { removalTxHash: value.removalTxHash },
        economics: {
          operatorBondInputLovelace: "900000000",
          slashedLovelace: "500000000",
          proverRewardOutputOutRef: value.rewardOutRef,
          proverRewardLovelace: "400000000",
          removalFeeLovelace: "500000000",
          duplicateRewardAbsent: true,
        },
      },
    });
  });

  it.each([
    { bondStatus: "active", newStatus: "active", operatorCredential: OPERATOR },
    {
      bondStatus: "active",
      newStatus: "retired",
      operatorCredential: OPERATOR,
    },
    {
      bondStatus: "retired",
      newStatus: "active",
      operatorCredential: OPERATOR,
    },
    {
      bondStatus: "retired",
      newStatus: "retired",
      operatorCredential: OPERATOR,
    },
    {
      bondStatus: "active",
      newStatus: "active",
      operatorCredential: policy("62"),
    },
  ] as const)(
    "reobserves the same slash of a $bondStatus bond with a new $newStatus bond for $operatorCredential",
    async ({ bondStatus, newStatus, operatorCredential }) => {
      const { snapshot, definition } = await fixture({ bondStatus });
      const observe = (current: FraudProofRawL1Snapshot) =>
        deriveFraudProofRawL1FamilyStage({
          snapshot: current,
          definition,
          releaseEconomics,
        });
      const original = await observe(snapshot);
      const newBond = raw(
        `${hash32("61")}#0`,
        output({
          address:
            newStatus === "active"
              ? definition.operatorDirectory.activeAddress
              : definition.operatorDirectory.retiredAddress,
          assets: {
            lovelace: 900_000_000n,
            [toUnit(
              newStatus === "active"
                ? definition.operatorDirectory.activePolicyId
                : definition.operatorDirectory.retiredPolicyId,
              (newStatus === "active"
                ? ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX
                : RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX) + operatorCredential,
            )]: 1n,
          },
          datum: Data.to("" as never, Data.Bytes()),
        }),
      );
      const rejoined: FraudProofRawL1Snapshot = {
        ...snapshot,
        scopes: snapshot.scopes.map((scope) =>
          scope.role === `${newStatus}_operator_directory`
            ? { ...scope, utxos: [newBond] }
            : scope,
        ),
      };

      await expect(observe(rejoined)).resolves.toEqual(original);
    },
  );

  it("rejects a substituted release economics identity", async () => {
    const value = await fixture();
    await expect(
      deriveFraudProofRawL1FamilyStage({
        snapshot: value.snapshot,
        definition: value.definition,
        releaseEconomics: {
          ...releaseEconomics,
          blueprintHash: hash32("99"),
        },
      }),
    ).rejects.toThrow(/economics identity does not match/u);
  });

  it.each(["active", "retired"] as const)(
    "rejects a slash that did not burn the %s bond token",
    async (bondStatus) => {
      const value = await fixture({ bondStatus, burnBond: false });
      await expect(
        deriveFraudProofRawL1FamilyStage({ ...value, releaseEconomics }),
      ).rejects.toThrow(/slash did not burn the consumed operator bond token/u);
    },
  );

  it("reopens correction after a rollback and rederives the terminal after reinclusion", async () => {
    const { snapshot, definition } = await fixture();
    const observe = (current: FraudProofRawL1Snapshot) =>
      deriveFraudProofRawL1FamilyStage({
        snapshot: current,
        definition,
        releaseEconomics,
      });
    const terminal = await observe(snapshot);
    expect(terminal.kind).toBe("removed");
    const [target, bond] = snapshot.transactions[0]!.resolvedInputs;
    const root = raw(
      `${hash32("63")}#0`,
      output({
        address: definition.stateQueue.address,
        assets: {
          lovelace: 3_000_000n,
          [toUnit(definition.stateQueue.policyId, STATE_QUEUE_ROOT_ASSET_NAME)]:
            1n,
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
    const rolledBack: FraudProofRawL1Snapshot = {
      ...snapshot,
      transactions: [],
      history: snapshot.history.map((entry) => ({
        ...entry,
        transactionHashes: [],
      })),
      scopes: snapshot.scopes.map((scope) => {
        if (scope.role === "state_queue")
          return { ...scope, utxos: [root, target!] };
        if (scope.role === "active_operator_directory")
          return { ...scope, utxos: [bond!] };
        return scope;
      }),
    };
    await expect(
      deriveFraudProofRawL1CompletedTerminal({
        snapshot: rolledBack,
        definition,
        releaseEconomics,
      }),
    ).resolves.toBeNull();
    await expect(observe(rolledBack)).resolves.toMatchObject({
      kind: "proof_token",
      stateQueueBlockOutRef: target!.outRef,
      nextRemovalOutRef: target!.outRef,
    });
    await expect(observe(snapshot)).resolves.toEqual(terminal);
  });

  it.each(["active", "retired"] as const)(
    "rejects a slash that continued its %s bond token in another output",
    async (bondStatus) => {
      const value = await fixture({ bondStatus, continueBond: true });
      await expect(
        deriveFraudProofRawL1FamilyStage({ ...value, releaseEconomics }),
      ).rejects.toThrow(/slash continued an operator bond token/u);
    },
  );

  it.each(["active", "retired"] as const)(
    "rejects a snapshot that still lists the consumed %s bond as live",
    async (bondStatus) => {
      const value = await fixture({ bondStatus });
      const bond = value.snapshot.transactions[0]!.resolvedInputs[1]!;
      await expect(
        deriveFraudProofRawL1FamilyStage({
          ...value,
          snapshot: {
            ...value.snapshot,
            scopes: value.snapshot.scopes.map((scope) =>
              scope.role === `${bondStatus}_operator_directory`
                ? { ...scope, utxos: [bond] }
                : scope,
            ),
          },
          releaseEconomics,
        }),
      ).rejects.toThrow(/consumed operator bond remains live/u);
    },
  );

  it("keeps final removal separate from an earlier descendant slash", async () => {
    const value = await fixture({ descendant: true });
    await expect(
      deriveFraudProofRawL1FamilyStage({
        snapshot: value.snapshot,
        definition: value.definition,
        releaseEconomics,
      }),
    ).resolves.toMatchObject({
      kind: "removed",
      terminal: {
        correction: { removalTxHash: value.removalTxHash },
        economics: {
          proverRewardOutputOutRef: value.rewardOutRef,
          slashedLovelace: "500000000",
          removalFeeLovelace: "500000000",
        },
      },
    });
  });

  it("accepts only the exact partially inactivity-slashed tranche", async () => {
    const value = await fixture({ partial: true });
    await expect(
      deriveFraudProofRawL1FamilyStage({
        snapshot: value.snapshot,
        definition: value.definition,
        releaseEconomics,
      }),
    ).resolves.toMatchObject({
      kind: "removed",
      terminal: {
        economics: {
          operatorBondInputLovelace: "800000000",
          slashedLovelace: "400000000",
          removalFeeLovelace: "400000000",
        },
      },
    });
  });

  it("rejects duplicate reward outputs", async () => {
    const value = await fixture({ duplicateReward: true });
    await expect(
      deriveFraudProofRawL1FamilyStage({
        snapshot: value.snapshot,
        definition: value.definition,
        releaseEconomics,
      }),
    ).rejects.toThrow(/one exact ADA-only enterprise reward/u);
  });
});

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

describe("read-only completed workflow verification", () => {
  const completed = async () => {
    const value = await fixture({ proofCreation: true });
    const terminal = await deriveFraudProofRawL1CompletedTerminal({
      ...value,
      releaseEconomics,
    });
    if (terminal === null) throw new Error("fixture must be completed");
    const identity = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
      deploymentFingerprint: DEPLOYMENT,
      category: "doubleSpend" as const,
      target: {
        kind: "state_queue_header" as const,
        headerHash: value.definition.headerHash,
      },
      decisionDigest: hash32("92"),
    };
    const workflowId = computeFraudProofWorkflowId(identity);
    const entries: FraudProofWorkflowJournalEntry[] = [];
    const append = (event: FraudProofWorkflowJournalEntry["event"]) =>
      entries.push({
        schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
        workflowId,
        identity,
        sequence: entries.length,
        recordedAt: "2026-09-16T12:00:00.000Z",
        event,
      });
    append({ kind: "started" });
    append({
      kind: "prepared",
      artifact: {},
      artifactDigest: journalJsonDigest({}),
    });
    for (const txHash of [
      terminal.proofToken.createdByTxHash,
      terminal.correction.removalTxHash,
    ]) {
      append({
        kind: "preflight_passed",
        actionId: txHash,
        txHash,
        localEvaluator: "lucid",
        referenceScripts: [],
      });
      append({
        kind: "submission_intent",
        actionId: txHash,
        txHash,
        attempt: 1,
        actionInput: {},
      });
      append({ kind: "submitted", actionId: txHash, txHash, attempt: 1 });
      append({
        kind: "reconciled",
        actionId: txHash,
        txHash,
        outcome: "confirmed",
      });
      append({ kind: "confirmed", actionId: txHash, txHash });
    }
    append({
      kind: "completed",
      terminal,
      terminalDigest: journalJsonDigest(terminal),
    });
    const definition = {
      ...value.definition,
      computationThread: {
        policyId: value.definition.computationThread.policyId,
        steps: value.definition.computationThread.steps.map(
          ({ role, address }) => ({ role, address }),
        ),
      },
    };
    const binding = {
      deploymentFingerprint: DEPLOYMENT,
      definition,
      releaseFinality,
      releaseEconomics,
    };
    return {
      ...value,
      terminal,
      entries,
      verify: (
        snapshot = value.snapshot,
        candidate: FraudProofWorkflowTerminal = terminal,
      ) =>
        verifyCompletedFraudProofWorkflow({
          binding,
          entries,
          terminal: candidate,
          decisionDigest: identity.decisionDigest,
          authority: {
            authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
            capture: async () => snapshot,
          },
        }),
    };
  };

  it("authenticates exact completed bytes and economics without thread decoders or journal mutations", async () => {
    const value = await completed();
    const saved = structuredClone(value.entries);
    await expect(value.verify()).resolves.toEqual({
      kind: "applicable",
      terminal: value.terminal,
    });
    expect(value.entries).toEqual(saved);
  });
  it("invalidates completion when an authenticated rollback restores the target", async () => {
    const value = await completed();
    await expect(value.verify(rollBackTerminalFixture(value))).resolves.toEqual(
      { kind: "pending", reason: "target_live" },
    );
  });
  it("keeps completed verification pending when authenticated release depth regresses", async () => {
    const value = await completed();
    const point = value.snapshot.cursor.point;
    const shallow = {
      ...value.snapshot,
      provenance: { ...value.snapshot.provenance, ogmiosTip: point },
      cursor: { ...value.snapshot.cursor, tip: point, confirmationDepth: 1 },
      transactions: value.snapshot.transactions.map((tx) => ({
        ...tx,
        confirmationDepth: 1,
      })),
    };
    await expect(value.verify(shallow)).resolves.toEqual({
      kind: "pending",
      reason: "release_finality",
    });
  });
  it.each(["headerHash", "economics", "proofToken"] as const)(
    "rejects substituted durable %s",
    async (field) => {
      const value = await completed();
      const changed =
        field === "headerHash"
          ? { ...value.terminal, headerHash: policy("99") }
          : field === "economics"
            ? {
                ...value.terminal,
                economics: {
                  ...value.terminal.economics,
                  proverRewardLovelace: "1",
                },
              }
            : {
                ...value.terminal,
                proofToken: {
                  ...value.terminal.proofToken,
                  unit: policy("99"),
                },
              };
      await expect(value.verify(value.snapshot, changed)).rejects.toThrow(
        "durable terminal",
      );
    },
  );
  it.each(["economics", "proofToken", "anchor"] as const)(
    "rejects self-consistent journal %s that contradicts authenticated L1",
    async (field) => {
      const value = await completed();
      const changed =
        field === "anchor"
          ? {
              ...value.terminal,
              observedAt: {
                ...value.terminal.observedAt,
                blockHash: hash32("99"),
              },
            }
          : field === "economics"
            ? {
                ...value.terminal,
                economics: {
                  ...value.terminal.economics,
                  proverRewardLovelace: "1",
                },
              }
            : {
                ...value.terminal,
                proofToken: {
                  ...value.terminal.proofToken,
                  unit: policy("99"),
                },
              };
      const last = value.entries.at(-1)!;
      value.entries[value.entries.length - 1] = {
        ...last,
        event: {
          kind: "completed",
          terminal: changed,
          terminalDigest: journalJsonDigest(changed),
        },
      };
      await expect(value.verify(value.snapshot, changed)).rejects.toThrow(
        "authenticated L1 facts",
      );
    },
  );
  it("rejects changed raw proof output bytes rather than treating integrity failure as pending", async () => {
    const value = await completed();
    const tampered = {
      ...value.snapshot,
      scopes: value.snapshot.scopes.map((scope) =>
        scope.role === "permanent_proof_token"
          ? {
              ...scope,
              utxos: scope.utxos.map((utxo) => ({
                ...utxo,
                datumCbor: Data.to(
                  { fraud_prover: policy("99") },
                  FraudProofTokenDatum,
                ),
              })),
            }
          : scope,
      ),
    };
    await expect(value.verify(tampered)).rejects.toThrow();
  });
});
