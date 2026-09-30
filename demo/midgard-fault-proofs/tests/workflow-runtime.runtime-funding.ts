import {
  CML,
  type Script,
  type TxSigned,
  type UTxO,
  utxoToCore,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { vi } from "vitest";

import { DaLibp2pRetainedDaSource } from "../src/transition-trace/fetch.js";
import {
  beginWorkflowFundingReservationAction,
  bindWorkflowFundingReservationJournal,
  createWorkflowFundingReservationPermit,
  prepareWorkflowFundingReservationTransaction,
  unsafeWorkflowFundingReservationSelectedOutRefsForTest,
  type WorkflowFundingReservationSnapshot,
} from "../src/workflow/funding-reservation-permit.js";
import type { FraudProofWorkflowAction } from "../src/workflow/orchestrator.js";
import {
  createWorkflowRuntimeFundingPolicy,
  readWorkflowRuntimeFundingPolicy,
} from "../src/workflow/runtime-funding-policy.js";
import { runtimeFundingPolicyFixture } from "./helpers/runtime-funding-policy-fixture.js";
import {
  admittedActuation,
  DEPLOYMENT,
  fundingAddress,
  fundingHandoff,
  fundingKey,
  fundingReferenceOutRef,
} from "./workflow-runtime.admitted-actuation.js";

export const runtimeFunding = async (
  actionKind: "step-one" | "step-three" | "verify_source",
  options: Readonly<{
    amendPolicy?: (
      input: ReturnType<typeof runtimeFundingPolicyFixture>["constructorInput"],
    ) => ReturnType<typeof createWorkflowRuntimeFundingPolicy>;
    governedReference?: Script;
    resolvedReference?: Script;
    useStage?: boolean;
    collateral?: boolean;
    begin?: boolean;
    additionalInputs?: readonly UTxO[];
    confirmedInput?: UTxO;
    changedLineage?: boolean;
    journal?: object;
    reobserve?: () => Promise<unknown>;
    refreshIdle?: (input: {
      expectedRevision: string;
      releaseStaleInputs: boolean;
    }) => Promise<unknown>;
  }> = {},
) => {
  const actuation = await admittedActuation();
  const { runner, policy, constructorInput } = runtimeFundingPolicyFixture({
    deploymentFingerprint: DEPLOYMENT,
    fundingPaymentKeyHash: fundingKey.to_public().hash().to_hex(),
    referenceScripts:
      options.governedReference === undefined
        ? []
        : [
            {
              outRef: fundingReferenceOutRef,
              scriptHash: validatorToScriptHash(options.governedReference),
            },
          ],
  });
  const values = new Map([
    [`${"71".repeat(32)}#0`, 3_000_000n],
    [`${"72".repeat(32)}#0`, 2_000_000n],
    [`${"73".repeat(32)}#0`, 10_000_000n],
  ]);
  if (options.collateral) values.set(`${"76".repeat(32)}#0`, 5_000_000n);
  const activeInputs = Object.freeze(
    [...values].map(([outRef, lovelace]) =>
      Object.freeze({
        outRef,
        role: outRef.startsWith("76".repeat(32))
          ? ("collateral" as const)
          : ("funding" as const),
        lovelace: lovelace.toString(),
        assets: Object.freeze([]),
      }),
    ),
  );
  const snapshot = Object.freeze({
    reservationId: "b1".repeat(32),
    deploymentFingerprint: DEPLOYMENT,
    decisionDigest: actuation.decisionDigest,
    policyDigest: readWorkflowRuntimeFundingPolicy(policy).policyDigest,
    reservationBasisDigest: "b2".repeat(32),
    rollbackGeneration: "7",
    revision: "0",
    walletAddress: fundingAddress,
    fundingPaymentKeyHash: fundingKey.to_public().hash().to_hex(),
    state: "active" as const,
    activeInputs,
  });
  let currentSnapshot: WorkflowFundingReservationSnapshot = snapshot;
  const prepare = vi.fn(async () => currentSnapshot);
  const resolveInputs = vi.fn(async (outRefs: readonly string[]) =>
    outRefs.map((outRef) => {
      const additional = options.additionalInputs?.find(
        (input) => `${input.txHash}#${input.outputIndex.toString()}` === outRef,
      );
      if (additional !== undefined) return additional;
      if (outRef === fundingReferenceOutRef)
        return {
          txHash: "74".repeat(32),
          outputIndex: 0,
          address: fundingAddress,
          assets: { lovelace: 2_000_000n },
          scriptRef: options.resolvedReference,
        };
      const [txHash, outputIndex] = outRef.split("#");
      return {
        txHash: txHash!,
        outputIndex: Number(outputIndex),
        address: fundingAddress,
        assets: { lovelace: values.get(outRef) ?? 1_000_000n },
      };
    }),
  );
  const releaseIdle = vi.fn(async () => currentSnapshot);
  const permit = await createWorkflowFundingReservationPermit({
    category: "doubleSpend",
    runner,
    policy: options.amendPolicy?.(constructorInput) ?? policy,
    reservationPolicy: policy,
    actuationPermit: actuation.actuationPermit,
    rollbackGeneration: "7",
    port: {
      ...(options.reobserve === undefined
        ? {}
        : { reobserve: options.reobserve }),
      releaseIdle,
      ...(options.refreshIdle === undefined
        ? {}
        : { refreshIdle: options.refreshIdle }),
      load: async () => currentSnapshot,
      resolveInputs,
      resolveConfirmedInput: async ({ outRef }) => {
        const input = options.confirmedInput;
        if (
          input === undefined ||
          outRef !== `${input.txHash}#${input.outputIndex.toString()}`
        )
          return null;
        return {
          sourceActionKind: "init",
          sourceOutputIndex: input.outputIndex,
          outRef,
          resolvedOutputCborHex: utxoToCore(
            options.changedLineage
              ? { ...input, assets: { lovelace: 1_000_000n } }
              : input,
          )
            .output()
            .to_canonical_cbor_hex(),
        };
      },
      readPendingTransition: async () => null,
      readPendingHandoff: async () => null,
      readCompletionHandoff: async () => null,
      readAbandonmentHandoff: async () => null,
      acknowledgeAbandonment: async () => snapshot,
      resolveProtocolInputAuthority: async () => {
        throw new Error("test action has no protocol input");
      },
      prepare,
      confirm: async () => currentSnapshot,
      abandon: async () => snapshot,
      markConflict: async () => snapshot,
      release: async () => snapshot,
    },
  });
  const journal = options.journal ?? Object.freeze({ actionKind });
  bindWorkflowFundingReservationJournal({ journal, permit });
  const begin = () =>
    beginWorkflowFundingReservationAction({
      journal,
      action: {
        actionId: actionKind,
        input: options.useStage ? { stage: actionKind } : { actionKind },
      },
    });
  if (options.begin !== false) await begin();
  return Object.freeze({
    actuation,
    releaseIdle,
    policy,
    begin,
    resolveInputs,
    journal,
    permit,
    prepare,
    prepareTransaction: (input: {
      action: FraudProofWorkflowAction;
      preflight: object;
    }) =>
      prepareWorkflowFundingReservationTransaction({
        journal,
        ...input,
        handoff: fundingHandoff(actuation, input.action, input.preflight),
      }),
    selected: unsafeWorkflowFundingReservationSelectedOutRefsForTest(permit),
    setSnapshot: (value: WorkflowFundingReservationSnapshot) => {
      currentSnapshot = value;
    },
    snapshot,
  });
};

export const runtimeFundingSelection = async (
  actionKind: "step-one" | "step-three",
) => (await runtimeFunding(actionKind)).selected;

export const signedFundingTransaction = (input: {
  readonly inputOutRefs: readonly string[];
  readonly outputLovelace: bigint;
  readonly referenceOutRefs?: readonly string[];
  readonly fee?: bigint;
  readonly outputAddress?: string;
  readonly additionalOutputs?: readonly CML.TransactionOutput[];
  readonly collateral?: {
    outRefs: readonly string[];
    total: bigint;
    returned: bigint;
    returnAddress?: string;
  };
  readonly redeemerMemory?: bigint;
  readonly signingKey?: CML.PrivateKey;
  readonly nonCanonicalBody?: boolean;
}): TxSigned => {
  const inputs = CML.TransactionInputList.new();
  for (const outRef of input.inputOutRefs) {
    const [txHash, outputIndex] = outRef.split("#");
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(txHash!),
        BigInt(outputIndex!),
      ),
    );
  }
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(input.outputAddress ?? fundingAddress),
      CML.Value.from_coin(input.outputLovelace),
    ),
  );
  for (const output of input.additionalOutputs ?? []) outputs.add(output);
  let body = CML.TransactionBody.new(inputs, outputs, input.fee ?? 200_000n);
  if (input.collateral !== undefined) {
    const collateral = CML.TransactionInputList.new();
    for (const outRef of input.collateral.outRefs) {
      const [txHash, index] = outRef.split("#");
      collateral.add(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(txHash!),
          BigInt(index!),
        ),
      );
    }
    body.set_collateral_inputs(collateral);
    body.set_total_collateral(input.collateral.total);
    body.set_collateral_return(
      CML.TransactionOutput.new(
        CML.Address.from_bech32(
          input.collateral.returnAddress ?? fundingAddress,
        ),
        CML.Value.from_coin(input.collateral.returned),
      ),
    );
  }
  if (input.referenceOutRefs !== undefined) {
    const references = CML.TransactionInputList.new();
    for (const outRef of input.referenceOutRefs) {
      const [txHash, outputIndex] = outRef.split("#");
      references.add(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(txHash!),
          BigInt(outputIndex!),
        ),
      );
    }
    body.set_reference_inputs(references);
  }
  if (input.nonCanonicalBody)
    body = CML.TransactionBody.from_cbor_hex(
      "bf" + body.to_cbor_hex().slice(2) + "ff",
    );
  const witnesses = CML.TransactionWitnessSet.new();
  if (input.redeemerMemory !== undefined) {
    const redeemers = CML.LegacyRedeemerList.new();
    redeemers.add(
      CML.LegacyRedeemer.new(
        CML.RedeemerTag.Spend,
        0n,
        CML.PlutusData.from_cbor_hex("00"),
        CML.ExUnits.new(input.redeemerMemory, 1n),
      ),
    );
    witnesses.set_redeemers(CML.Redeemers.new_arr_legacy_redeemer(redeemers));
  }
  const signingKey = input.signingKey ?? fundingKey;
  const vkeys = CML.VkeywitnessList.new();
  vkeys.add(
    CML.Vkeywitness.new(
      signingKey.to_public(),
      signingKey.sign(CML.hash_transaction(body).to_raw_bytes()),
    ),
  );
  witnesses.set_vkeywitnesses(vkeys);
  const transaction = CML.Transaction.new(body, witnesses, true, undefined);
  return {
    toTransaction: () => transaction,
    toHash: () => CML.hash_transaction(body).to_hex(),
  } as unknown as TxSigned;
};

export const retainedDaSource = (): DaLibp2pRetainedDaSource =>
  new DaLibp2pRetainedDaSource({
    deploymentFingerprint: DEPLOYMENT,
    peers: [{ peerId: "12D3KooWproductionRuntimeTest" }],
    transport: {
      request: async () => {
        throw new Error("transport is not called by runtime-boundary test");
      },
    },
  });
