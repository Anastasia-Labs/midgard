import { createHash } from "node:crypto";

import {
  CML,
  type TxSigned,
  type UTxO,
  utxoToCore,
} from "@lucid-evolution/lucid";
import { expect, vi } from "vitest";

import * as removalFunding from "../src/remove-fraudulent-block.js";
import {
  defineFamilyApplication,
  type FamilyApplicationWorkflowIdentity,
} from "../src/workflow/family-application.js";
import {
  beginWorkflowFundingReservationAction,
  bindWorkflowFundingReservationJournal,
  createWorkflowFundingReservationPermit,
  prepareWorkflowFundingReservationTransaction,
  type WorkflowFundingReservationSnapshot,
} from "../src/workflow/funding-reservation-permit.js";
import {
  type LoadedWorkflowRuntime,
  WORKFLOW_RUNTIME_CONFIG,
} from "../src/workflow/runtime.js";
import { readWorkflowRuntimeFundingPolicy } from "../src/workflow/runtime-funding-policy.js";
import { bindWorkflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { runtimeFundingPolicyFixture } from "./helpers/runtime-funding-policy-fixture.js";
import {
  emptyRosterReferenceScriptResolver,
  familyCommonInfrastructureForTest,
} from "./support/family-common-infrastructure.js";
import {
  admittedActuation,
  DEPLOYMENT,
  fundingAddress,
  fundingHandoff,
  fundingKey,
} from "./workflow-runtime.admitted-actuation.js";
import {
  retainedDaSource,
  runtimeFunding,
  signedFundingTransaction,
} from "./workflow-runtime.runtime-funding.js";

export const loadedRuntime = (
  actuation: Readonly<{ headerHash: string; decisionDigest: string }>,
  overrides: Partial<LoadedWorkflowRuntime> = {},
): LoadedWorkflowRuntime => ({
  schemaVersion: WORKFLOW_RUNTIME_CONFIG,
  infrastructure: familyCommonInfrastructureForTest(actuation),
  resolveReferenceScript: emptyRosterReferenceScriptResolver,
  retainedDaSources: [retainedDaSource()],
  close: async () => undefined,
  ...overrides,
});

export type DoubleSpendIdentity =
  FamilyApplicationWorkflowIdentity<"doubleSpend">;

/** A doubleSpend record with an empty roster and no requirements. */
export const doubleSpendRecord = <Config, Workflow extends DoubleSpendIdentity>(
  record: Partial<
    Parameters<
      typeof defineFamilyApplication<"doubleSpend", Config, Workflow>
    >[0]
  > &
    Pick<
      Parameters<
        typeof defineFamilyApplication<"doubleSpend", Config, Workflow>
      >[0],
      "bindConfig" | "constructWorkflow" | "execute"
    >,
) =>
  defineFamilyApplication<"doubleSpend", Config, Workflow>({
    category: "doubleSpend",
    roster: {},
    requires: [],
    bindsDecisionDigest: false,
    ...record,
  });

export const prepareRuntimeFunding = (
  runtime: Awaited<ReturnType<typeof runtimeFunding>>,
  signed: TxSigned,
) =>
  runtime.prepareTransaction({
    action: { actionId: "step-one", input: { stage: "step-one" } },
    preflight: bindWorkflowPreflightTransaction(
      Object.freeze({ txHash: signed.toHash() }),
      signed,
    ),
  });

export const slashFundingFixture = async (
  options: {
    tranche?: "full" | "partially-inactivity-slashed";
    feeDelta?: bigint;
    rewardDelta?: bigint;
    actionStage?: string;
    includeWalletInput?: boolean;
    capability?: boolean;
    foreignAuthority?: boolean;
  } = {},
) => {
  const actuation = await admittedActuation();
  const tranche = options.tranche ?? "full";
  const bond = tranche === "full" ? 900_000_000n : 800_000_000n;
  const exactFee = bond - 400_000_000n;
  const protocolAddress = CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_script(CML.ScriptHash.from_hex("a5".repeat(28))),
  )
    .to_address()
    .to_bech32();
  const operator: UTxO = {
    txHash: "81".repeat(32),
    outputIndex: 0,
    address: protocolAddress,
    assets: { lovelace: bond },
  };
  const anchor: UTxO = {
    txHash: "82".repeat(32),
    outputIndex: 0,
    address: protocolAddress,
    assets: { lovelace: 2_000_000n },
  };
  const proof: UTxO = {
    txHash: "83".repeat(32),
    outputIndex: 0,
    address: protocolAddress,
    assets: { lovelace: 2_000_000n },
  };
  const ref = (utxo: UTxO) => `${utxo.txHash}#${utxo.outputIndex}`;
  const { runner, policy } = runtimeFundingPolicyFixture({
    deploymentFingerprint: DEPLOYMENT,
    fundingPaymentKeyHash: fundingKey.to_public().hash().to_hex(),
    contracts: [
      {
        address: protocolAddress,
        scriptHash: "a5".repeat(28),
        role: "protocol_state",
      },
    ],
  });
  const wallets = [
    {
      txHash: "71".repeat(32),
      outputIndex: 0,
      address: fundingAddress,
      assets: { lovelace: 30_000_000n },
    },
    {
      txHash: "76".repeat(32),
      outputIndex: 0,
      address: fundingAddress,
      assets: { lovelace: 500_000_000n },
    },
    {
      txHash: "77".repeat(32),
      outputIndex: 0,
      address: fundingAddress,
      assets: { lovelace: 500_000_000n },
    },
  ];
  let snapshot: WorkflowFundingReservationSnapshot = {
    reservationId: "b1".repeat(32),
    deploymentFingerprint: DEPLOYMENT,
    decisionDigest: actuation.decisionDigest,
    policyDigest: readWorkflowRuntimeFundingPolicy(policy).policyDigest,
    reservationBasisDigest: "b2".repeat(32),
    rollbackGeneration: "7",
    revision: "0",
    walletAddress: fundingAddress,
    fundingPaymentKeyHash: fundingKey.to_public().hash().to_hex(),
    state: "active",
    activeInputs: wallets.map((utxo, index) => ({
      outRef: ref(utxo),
      role: index === 0 ? "funding" : "collateral",
      lovelace: utxo.assets.lovelace.toString(),
      assets: [],
    })),
  };
  const inputByRef = new Map(
    [operator, anchor, proof, ...wallets].map((utxo) => [ref(utxo), utxo]),
  );
  const prepare = vi.fn(async (_input: unknown) => {
    snapshot = { ...snapshot, revision: "1" };
    return snapshot;
  });
  const protocolAuthority = vi.fn(async ({ outRef }: { outRef: string }) => ({
    deploymentFingerprint: options.foreignAuthority
      ? "ff".repeat(32)
      : DEPLOYMENT,
    outRef,
    semanticRole: "protocol_state",
    resolvedOutputCborHex: utxoToCore(inputByRef.get(outRef)!)
      .output()
      .to_canonical_cbor_hex(),
  }));
  const permit = await createWorkflowFundingReservationPermit({
    category: "doubleSpend",
    runner,
    policy,
    actuationPermit: actuation.actuationPermit,
    rollbackGeneration: "7",
    port: {
      load: async () => snapshot,
      resolveInputs: async (outRefs) =>
        outRefs.map((outRef) => inputByRef.get(outRef)!),
      resolveConfirmedInput: async () => {
        throw new Error("slash inputs must reacquire protocol authority");
      },
      readPendingTransition: async () => null,
      readPendingHandoff: async () => null,
      readCompletionHandoff: async () => null,
      readAbandonmentHandoff: async () => null,
      acknowledgeAbandonment: async () => snapshot,
      resolveProtocolInputAuthority: protocolAuthority,
      prepare,
      confirm: async () => snapshot,
      abandon: async () => snapshot,
      markConflict: async () => snapshot,
      release: async () => snapshot,
    },
  });
  const journal = {};
  bindWorkflowFundingReservationJournal({ journal, permit });
  const action = {
    actionId: "remove-current-target",
    input: {
      stage: options.actionStage ?? "remove",
      category: "doubleSpend",
      nextRemovalOutRef: ref(anchor),
      fraudProofOutRef: ref(proof),
    },
  };
  await beginWorkflowFundingReservationAction({ journal, action });
  const collateral = (exactFee * 150n) / 100n;
  const signed = signedFundingTransaction({
    inputOutRefs: [
      ref(operator),
      ref(anchor),
      ...(options.includeWalletInput ? [ref(wallets[0]!)] : []),
    ].sort(),
    referenceOutRefs: [ref(proof)],
    outputLovelace: 400_000_000n + (options.rewardDelta ?? 0n),
    additionalOutputs: [utxoToCore(anchor).output()],
    fee: exactFee + (options.feeDelta ?? 0n),
    collateral: {
      outRefs: wallets.slice(1).map(ref),
      total: collateral,
      returned: 1_000_000_000n - collateral,
    },
    redeemerMemory: 1n,
    nonCanonicalBody: true,
  });
  const authority: removalFunding.FraudSlashFundingAuthority = {
    deploymentFingerprint: DEPLOYMENT,
    economicsPolicyDigest:
      readWorkflowRuntimeFundingPolicy(policy).economicsPolicyDigest,
    category: "doubleSpend",
    headerHash: actuation.headerHash,
    fraudProofOutRef: ref(proof),
    removedStateQueueOutRef: ref(anchor),
    operatorOutRef: ref(operator),
    operatorBondLovelace: bond.toString(),
    tranche,
    exactFeeLovelace: exactFee.toString(),
    rewardLovelace: "400000000",
    rewardAddress: fundingAddress,
    transactionHash: signed.toHash(),
    transactionBodySha256: createHash("sha256")
      .update(Buffer.from(signed.toTransaction().body().to_cbor_hex(), "hex"))
      .digest("hex"),
    signedTransactionCborHex: signed.toTransaction().to_cbor_hex(),
    inputs: [operator, anchor].map((utxo) => ({
      outRef: ref(utxo),
      resolvedOutputCborHex: utxoToCore(utxo).output().to_canonical_cbor_hex(),
    })),
  };
  // The real minter is private to the evaluated removal builder. Isolate that
  // builder seam while checking actual signed CML wire here.
  const originalReader = removalFunding.readFraudSlashFundingAuthority;
  expect(originalReader(signed)).toBeNull();
  const reader = vi
    .spyOn(removalFunding, "readFraudSlashFundingAuthority")
    .mockImplementation((candidate) =>
      candidate === signed && options.capability !== false
        ? authority
        : originalReader(candidate),
    );
  return {
    signed,
    journal,
    action,
    prepare,
    policy,
    protocolAuthority,
    admit: () => {
      const preflight = bindWorkflowPreflightTransaction(
        { txHash: signed.toHash() },
        signed,
      );
      return prepareWorkflowFundingReservationTransaction({
        journal,
        action,
        preflight,
        handoff: fundingHandoff(actuation, action, preflight),
      });
    },
    close: () => reader.mockRestore(),
  };
};
