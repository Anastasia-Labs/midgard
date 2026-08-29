/**
 * The `native-script-decoding` proving core (offchain plan §4.3, ruled
 * 2026-08-25): a consumer-agnostic driver over the per-step submitters,
 * consumable by the watcher (autonomous) and the CLI (manual) alike.
 *
 * The core drives Init → step-01 → step-02 → Bind → (Scan)* → Verdict →
 * step-04, feeding `nextThreadOutRef` forward. Its contract:
 *
 * - **Capability-injected.** `deps` carries everything environmental —
 *   signer, chain provider, evidence sources, observations, journal sink,
 *   policy. The core imports nothing from either consumer.
 * - **Resumable and idempotent-by-reconstruction** (§7.1). Invoked against
 *   a header whose thread already exists, it locates the thread by asset
 *   name across the four step addresses, reads the on-chain `StepDatum`,
 *   recovers the position (including mid-loop via the `machine_state_hash`
 *   boundary search against the re-derived plan), and continues.
 * - **Policy as data.** The core enforces whatever
 *   `NativeScriptDecodingProverPolicyV1` it is handed and hard-codes none
 *   of it. Only the §3.2/3.3 provability classification is non-negotiable:
 *   unprovable corners are refused at the API boundary regardless of
 *   policy.
 * - **Outcome as data:** proven, refused (classification/policy, with the
 *   reason), or stalled (unexpected abort — surfaced loudly, never
 *   silently cancelled; cancellation is its own explicit call,
 *   `submitNativeScriptDecodingCancel`).
 */
import {
  MidgardNativeScriptDecodingDirectionsV1,
  type MidgardNativeScriptDecodingDirectionV1,
} from "@al-ft/midgard-core";
import {
  FraudProofComputationThreadStepDatum,
  type MidgardTxInput,
  NATIVE_SCRIPT_DECODING_CLASS_PENDING_V1,
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION_V1,
  NATIVE_SCRIPT_DECODING_SOURCE_KIND_NORMAL_V1,
  type NativeScriptDecodingScanThreadStateV1,
  NativeScriptDecodingStep03Datum,
  OutputReference,
} from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type Network,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { PublishedProofChunkV1 } from "../proof-chunk-carriage.js";
import { outRefLabel, type ResolvedProverSigner } from "../runtime.js";
import type { SubmitStep01TxInclusion } from "../submit-step-01.js";
import type { TransitionTraceReconstruction } from "../transition-trace/reconstruct.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { NativeScriptDecodingContractsV1 } from "./contracts-v1.js";
import type { NativeScriptDecodingLedgerTrieHandleV1 } from "./evidence-v1.js";
import {
  assertNativeScriptDecodingFindingProvableV1,
  type NativeScriptDecodingFindingV1,
  NativeScriptDecodingProvabilityV1,
} from "./finding-v1.js";
import {
  buildNativeScriptDecodingScanPlanV1,
  NativeScriptDecodingPlanRoutesV1,
  type NativeScriptDecodingScanPlanV1,
} from "./scan-plan-v1.js";
import {
  type NativeScriptDecodingCatalogueCategoryV1,
  nativeScriptDecodingSubmitError,
} from "./submit-common-v1.js";
import { submitNativeScriptDecodingInit } from "./submit-native-script-decoding-init.js";
import {
  submitNativeScriptDecodingStep01BindNormal,
  submitNativeScriptDecodingStep01RecordForced,
} from "./submit-native-script-decoding-step-01.js";
import { submitNativeScriptDecodingStep02 } from "./submit-native-script-decoding-step-02.js";
import {
  submitNativeScriptDecodingStep03BindOutOfDomain,
  submitNativeScriptDecodingStep03BindOutpoint,
  submitNativeScriptDecodingStep03Scan,
  submitNativeScriptDecodingStep03Verdict,
} from "./submit-native-script-decoding-step-03.js";
import { submitNativeScriptDecodingStep04 } from "./submit-native-script-decoding-step-04.js";

// ## Policy (§4.3, defaults per §10 Q5)

export type NativeScriptDecodingProverPolicyV1 = {
  /**
   * Minimum L1 depth of the faulted header's state-queue UTxO before the
   * core spends anything. Default mirrors the watcher's finality policy
   * (`finality-engine.ts` `confirmationDepth`, 2,160 blocks). `0n` disables
   * the gate.
   */
  readonly minSettlementDepth: bigint;
  /**
   * Per-thread fee budget cap, checked against the §6 plan-time estimate
   * before Init and re-checked as the loop progresses. `null` disables the
   * cap. Default 650 ADA — worst case ≈510 plus margin.
   */
  readonly maxThreadBudgetLovelace: bigint | null;
  /** The §6 per-transaction fee assumption behind the budget arithmetic. */
  readonly assumedFeePerTxLovelace: bigint;
  /** Autonomous threads in flight at once (enforced by the watcher adapter). */
  readonly singleFlight: number;
  /**
   * Refuse Init when the remaining maturity window is under this factor
   * times the predicted serial duration. `0` disables the guard.
   */
  readonly maturityGuardFactor: number;
  /** §6 pacing assumption: one transaction per block, ≈20s. */
  readonly assumedMillisPerTx: number;
};

export const NATIVE_SCRIPT_DECODING_PROVER_POLICY_DEFAULTS_V1: NativeScriptDecodingProverPolicyV1 =
  Object.freeze({
    minSettlementDepth: 2_160n,
    maxThreadBudgetLovelace: 650_000_000n,
    assumedFeePerTxLovelace: 1_450_000n,
    singleFlight: 1,
    maturityGuardFactor: 2,
    assumedMillisPerTx: 20_000,
  });

// ## Capabilities

/**
 * Evidence sources, each handed the finding it must serve. A callback is
 * only invoked when the finding's route needs it (e.g. `txInclusion` only
 * for a direction-A normal thread), so a consumer may throw from routes it
 * cannot serve without ever being asked to.
 */
export type NativeScriptDecodingProverEvidenceV1 = {
  /** Direction-A normal threads: the §2.4 committed-transaction inclusion. */
  readonly txInclusion: (
    finding: NativeScriptDecodingFindingV1,
  ) => Promise<SubmitStep01TxInclusion>;
  /** Optional #545 published-chunk carriage for the step-01 opening. */
  readonly publishedProofChunks?: (
    finding: NativeScriptDecodingFindingV1,
  ) => Promise<readonly PublishedProofChunkV1[] | null>;
  /** The disputed block's transition-trace reconstruction (step-02). */
  readonly reconstruction: (
    finding: NativeScriptDecodingFindingV1,
  ) => Promise<TransitionTraceReconstruction>;
  /** The committed transaction's compact bytes and accused field's items. */
  readonly subjectTx: (finding: NativeScriptDecodingFindingV1) => Promise<{
    readonly nativeTxCompactCbor: string;
    readonly subjectFieldInputs: readonly MidgardTxInput[];
  }>;
  /** The ledger's resolution of the accused outpoint, plus the item bytes. */
  readonly descriptor: (finding: NativeScriptDecodingFindingV1) => Promise<{
    readonly descriptorCbor: string;
    readonly referenceScriptItemBytes: Uint8Array | null;
  }>;
  /** Pre-state ledger trie whose root is the thread's `prior_ledger_root`. */
  readonly ledgerTrie: (
    finding: NativeScriptDecodingFindingV1,
  ) => Promise<NativeScriptDecodingLedgerTrieHandleV1>;
};

/**
 * Chain observations behind the policy gates. Each is required exactly
 * when the corresponding gate is active in the policy handed alongside.
 */
export type NativeScriptDecodingProverObservationsV1 = {
  readonly settlementDepthOf?: (
    fraudulentBlockOutRef: string,
  ) => Promise<bigint>;
  readonly remainingMaturityMs?: (
    fraudulentBlockOutRef: string,
  ) => Promise<number>;
};

export type NativeScriptDecodingProverEventV1 = {
  readonly phase:
    | "boundary"
    | "policy"
    | "init"
    | "step01"
    | "step02"
    | "bind"
    | "scan"
    | "verdict"
    | "step04"
    | "outcome";
  readonly message: string;
  readonly headerHash: string;
  readonly txHash?: string;
  readonly threadOutRef?: string;
};

export type NativeScriptDecodingProverDepsV1 = {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: NativeScriptDecodingContractsV1;
  readonly category: NativeScriptDecodingCatalogueCategoryV1;
  readonly catalogue: {
    readonly policyId: string;
    readonly spendingScriptAddress: string;
    readonly root: string;
  };
  readonly signer: ResolvedProverSigner;
  readonly evidence: NativeScriptDecodingProverEvidenceV1;
  readonly observations: NativeScriptDecodingProverObservationsV1;
  readonly journal: (
    event: NativeScriptDecodingProverEventV1,
  ) => void | Promise<void>;
  readonly policy: NativeScriptDecodingProverPolicyV1;
  /** Q3: mandatory authenticated published step reference scripts. */
  readonly referenceScriptUtxos?: {
    readonly step01?: UTxO;
    readonly step02?: UTxO;
    readonly step03?: UTxO;
    readonly step04?: UTxO;
  };
  /** Mandatory published shared witnesses used by init, step-01, and step-04. */
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  /** Force §8 tier-2 carriage publication on the bind transaction. */
  readonly publishCarriage?: boolean;
};

// ## Outcome (§4.3: data, not exceptions)

export type NativeScriptDecodingProofOutcomeV1 =
  | {
      readonly kind: "proven";
      readonly fraudProofUnit: string;
      readonly fraudProofOutRef: string;
      /** Transactions this invocation submitted (a resume lists fewer). */
      readonly txHashes: readonly string[];
    }
  | {
      readonly kind: "refused";
      readonly refusal:
        | "classification"
        | "policy"
        | "duplicate"
        | "alreadyProven";
      readonly reason: string;
    }
  | {
      readonly kind: "stalled";
      readonly reason: string;
      /** Where the thread sits, for an explicit resume or cancel. */
      readonly threadOutRef: string | null;
      readonly cause: unknown;
    };

// ## §7.1 position recovery

export type NativeScriptDecodingThreadPositionV1 =
  | { readonly step: "none" }
  | {
      readonly step: "step01" | "step02" | "step04";
      readonly threadUtxo: UTxO;
    }
  | {
      readonly step: "step03";
      readonly threadUtxo: UTxO;
      readonly state: NativeScriptDecodingScanThreadStateV1;
    };

/**
 * Locates the live thread carrying `threadUnit` across the family's four
 * step addresses (§7.1: the asset name, `category_id ‖ header_hash`, is the
 * thread's identity; the address identifies the step).
 */
export const locateNativeScriptDecodingThreadV1 = async ({
  lucid,
  contracts,
  threadUnit,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContractsV1;
  readonly threadUnit: string;
}): Promise<NativeScriptDecodingThreadPositionV1> => {
  for (const stepIndex of [0, 1, 2, 3] as const) {
    const utxos = await lucid.utxosAtWithUnit(
      contracts.steps[stepIndex].spendingScriptAddress,
      threadUnit,
    );
    const threadUtxo = utxos[0];
    if (threadUtxo === undefined) {
      continue;
    }
    if (stepIndex === 2) {
      const datum = Data.from(
        requireDatum(threadUtxo),
        NativeScriptDecodingStep03Datum,
      );
      if (datum.data === null) {
        throw nativeScriptDecodingSubmitError(
          `thread ${outRefLabel(threadUtxo)} at step 03 carries no state.`,
        );
      }
      return { step: "step03", threadUtxo, state: datum.data };
    }
    const step =
      stepIndex === 0 ? "step01" : stepIndex === 1 ? "step02" : "step04";
    return { step, threadUtxo };
  }
  return { step: "none" };
};

const requireDatum = (utxo: UTxO): string => {
  if (utxo.datum == null) {
    throw nativeScriptDecodingSubmitError(
      `thread UTxO ${outRefLabel(utxo)} has no inline datum.`,
    );
  }
  return utxo.datum;
};

const isPreBind = (state: NativeScriptDecodingScanThreadStateV1): boolean =>
  state.machine_state_hash === "" &&
  state.refusal_class === NATIVE_SCRIPT_DECODING_CLASS_PENDING_V1 &&
  state.outpoint_key_hash === "";

// ## The drive cursor

type DriveState =
  | { readonly at: "init" }
  | { readonly at: "step01"; readonly threadOutRef: string }
  | { readonly at: "step02"; readonly threadOutRef: string }
  | { readonly at: "bind"; readonly threadOutRef: string }
  | {
      readonly at: "scan";
      readonly threadOutRef: string;
      readonly segmentIndex: number;
    }
  | { readonly at: "verdict"; readonly threadOutRef: string }
  | { readonly at: "step04"; readonly threadOutRef: string };

/** Step-03 transactions the route still owes from its start. */
const step03TxCount = (
  finding: NativeScriptDecodingFindingV1,
  plan: NativeScriptDecodingScanPlanV1 | null,
): number => {
  if (
    finding.provability !== NativeScriptDecodingProvabilityV1.MachineRoute ||
    plan === null
  ) {
    return 1;
  }
  return plan.route === NativeScriptDecodingPlanRoutesV1.Machine
    ? 1 + plan.segments.length + 1
    : 1;
};

const remainingTxCount = (
  cursor: DriveState,
  finding: NativeScriptDecodingFindingV1,
  plan: NativeScriptDecodingScanPlanV1 | null,
): number => {
  const step03 = step03TxCount(finding, plan);
  switch (cursor.at) {
    case "init":
      return 3 + step03 + 1;
    case "step01":
      return 2 + step03 + 1;
    case "step02":
      return 1 + step03 + 1;
    case "bind":
      return step03 + 1;
    case "scan":
      return (plan?.segments.length ?? 0) - cursor.segmentIndex + 2;
    case "verdict":
      return 2;
    case "step04":
      return 1;
  }
};

const coreDirectionOf = (
  finding: NativeScriptDecodingFindingV1,
): MidgardNativeScriptDecodingDirectionV1 =>
  finding.direction === NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION_V1
    ? MidgardNativeScriptDecodingDirectionsV1.WrongfulRejection
    : MidgardNativeScriptDecodingDirectionsV1.WrongfulAcceptance;

const toError = (cause: unknown): Error =>
  cause instanceof Error ? cause : new Error(String(cause));

// ## The core

export const runNativeScriptDecodingProverV1 = async (
  finding: NativeScriptDecodingFindingV1,
  deps: NativeScriptDecodingProverDepsV1,
): Promise<NativeScriptDecodingProofOutcomeV1> => {
  const { lucid, contracts, policy, signer } = deps;
  const referenceScriptUtxos = deps.referenceScriptUtxos;
  if (
    referenceScriptUtxos?.step01 === undefined ||
    referenceScriptUtxos.step02 === undefined ||
    referenceScriptUtxos.step03 === undefined ||
    referenceScriptUtxos.step04 === undefined
  ) {
    throw nativeScriptDecodingSubmitError(
      "production proving requires authenticated reference-script UTxOs for steps 01 through 04.",
    );
  }
  const headerHash = finding.headerHash;
  const journal = async (
    event: Omit<NativeScriptDecodingProverEventV1, "headerHash">,
  ) => {
    await deps.journal({ ...event, headerHash });
  };
  const refused = async (
    refusal: "classification" | "policy" | "duplicate" | "alreadyProven",
    reason: string,
  ): Promise<NativeScriptDecodingProofOutcomeV1> => {
    await journal({
      phase: "outcome",
      message: `refused (${refusal}): ${reason}`,
    });
    return { kind: "refused", refusal, reason };
  };
  const stalled = async (
    reason: string,
    threadOutRef: string | null,
    cause: unknown,
  ): Promise<NativeScriptDecodingProofOutcomeV1> => {
    await journal({
      phase: "outcome",
      message: `STALLED: ${reason}`,
      threadOutRef: threadOutRef ?? undefined,
    });
    return { kind: "stalled", reason, threadOutRef, cause };
  };

  // 1. The non-negotiable §3.2/3.3 boundary: classification gates proving
  //    regardless of policy.
  try {
    assertNativeScriptDecodingFindingProvableV1(finding);
  } catch (cause) {
    return refused("classification", toError(cause).message);
  }

  const assetName = `${deps.category.categoryId}${headerHash}`;
  const threadUnit = toUnit(contracts.computationThread.policyId, assetName);
  const fraudProofUnit = toUnit(contracts.fraudProof.policyId, assetName);

  // 2. Idempotence fast-path: a fraud-proof token for this header already
  //    sits at the fraud-proof address.
  const provenAlready = await lucid.utxosAtWithUnit(
    contracts.fraudProof.spendingScriptAddress,
    fraudProofUnit,
  );
  if (provenAlready.length > 0) {
    return refused(
      "alreadyProven",
      `fraud-proof token ${fraudProofUnit} already exists at ${outRefLabel(provenAlready[0]!)}.`,
    );
  }

  // 3. §7.1: locate any live thread for this asset name.
  const position = await locateNativeScriptDecodingThreadV1({
    lucid,
    contracts,
    threadUnit,
  });
  if (position.step !== "none") {
    const datum = Data.from(
      requireDatum(position.threadUtxo),
      FraudProofComputationThreadStepDatum,
    );
    if (datum.fraud_prover !== signer.paymentKeyHash) {
      // §3.4 dedup: a live third-party thread is sound; duplicating it
      // only wastes fees.
      return refused(
        "duplicate",
        `a live thread at ${outRefLabel(position.threadUtxo)} names fraud prover ${datum.fraud_prover}, not this wallet.`,
      );
    }
  }

  // 4. Route preparation. For the machine route, the plan is re-derived up
  //    front: the budget arithmetic and the §7.1 mid-loop boundary search
  //    both need it.
  let plan: NativeScriptDecodingScanPlanV1 | null = null;
  let descriptorCbor: string | null = null;
  let referenceScriptItemBytes: Uint8Array | null = null;
  const needsBindOutpoint =
    finding.provability !==
    NativeScriptDecodingProvabilityV1.OutOfDomainAccusation;
  try {
    if (needsBindOutpoint) {
      const resolved = await deps.evidence.descriptor(finding);
      descriptorCbor = resolved.descriptorCbor;
      referenceScriptItemBytes = resolved.referenceScriptItemBytes;
      if (
        finding.provability === NativeScriptDecodingProvabilityV1.MachineRoute
      ) {
        if (referenceScriptItemBytes === null) {
          throw nativeScriptDecodingSubmitError(
            "the machine route scans the reference-script item; the descriptor evidence carries no item bytes.",
          );
        }
        plan = buildNativeScriptDecodingScanPlanV1({
          itemBytes: referenceScriptItemBytes,
          direction: coreDirectionOf(finding),
        });
        if (
          plan.route ===
          NativeScriptDecodingPlanRoutesV1.DescriptorContradiction
        ) {
          throw nativeScriptDecodingSubmitError(
            "the re-derived plan routes to a descriptor contradiction — the finding's machine-route classification does not match the evidence.",
          );
        }
      }
    }
  } catch (cause) {
    // Before Init this is a classification/evidence refusal; with a live
    // thread it is a stall the operator must see.
    return position.step === "none"
      ? refused("classification", toError(cause).message)
      : stalled(
          `route preparation failed on a live thread: ${toError(cause).message}`,
          outRefLabel(position.threadUtxo),
          cause,
        );
  }

  // 5. Map the on-chain position onto the drive cursor.
  let cursor: DriveState;
  if (position.step === "none") {
    cursor = { at: "init" };
  } else if (position.step !== "step03") {
    cursor = {
      at: position.step,
      threadOutRef: outRefLabel(position.threadUtxo),
    };
  } else {
    const threadOutRef = outRefLabel(position.threadUtxo);
    const state = position.state;
    if (isPreBind(state)) {
      cursor = { at: "bind", threadOutRef };
    } else if (
      state.refusal_class === NATIVE_SCRIPT_DECODING_CLASS_PENDING_V1
    ) {
      // §7.1 mid-loop: find the unique plan boundary whose control hashes
      // to the committed machine state.
      if (
        plan === null ||
        plan.route !== NativeScriptDecodingPlanRoutesV1.Machine
      ) {
        return stalled(
          "the thread is mid-scan but the finding's route derives no scan plan — evidence and chain state disagree.",
          threadOutRef,
          null,
        );
      }
      const committed = state.machine_state_hash;
      const segmentIndex = plan.segments.findIndex(
        (segment) => segment.controlBefore.hashHex === committed,
      );
      if (segmentIndex >= 0) {
        cursor = { at: "scan", threadOutRef, segmentIndex };
      } else if (plan.verdict.control?.hashHex === committed) {
        cursor = { at: "verdict", threadOutRef };
      } else {
        // No boundary matches: the local item bytes differ from the
        // committed item. An evidence error, surfaced loudly (§7.1).
        return stalled(
          `no plan boundary hashes to the committed machine state ${committed} — local evidence differs from the committed item.`,
          threadOutRef,
          null,
        );
      }
    } else {
      // A closed verdict at the step-03 address cannot exist (closes pay
      // to step-04), but fail loudly rather than guess.
      return stalled(
        `thread at step 03 carries a closed refusal class ${state.refusal_class.toString()} — unexpected chain state.`,
        threadOutRef,
        null,
      );
    }
  }

  // 6. Policy gates. Settlement depth and maturity gate Init; the fee
  //    budget gates Init and is re-checked as the loop progresses.
  const totalTxCount = remainingTxCount({ at: "init" }, finding, plan);
  const consumedBeforeThisRun =
    totalTxCount - remainingTxCount(cursor, finding, plan);
  const requireBudget = (
    at: DriveState,
    submittedThisRun: number,
  ): string | null => {
    if (policy.maxThreadBudgetLovelace === null) {
      return null;
    }
    const projected =
      BigInt(
        consumedBeforeThisRun +
          submittedThisRun +
          remainingTxCount(at, finding, plan),
      ) * policy.assumedFeePerTxLovelace;
    return projected > policy.maxThreadBudgetLovelace
      ? `projected thread cost ${projected.toString()} lovelace exceeds the ${policy.maxThreadBudgetLovelace.toString()} cap.`
      : null;
  };
  if (cursor.at === "init") {
    if (policy.minSettlementDepth > 0n) {
      if (deps.observations.settlementDepthOf === undefined) {
        throw nativeScriptDecodingSubmitError(
          "the policy gates on settlement depth but deps carry no settlementDepthOf observer.",
        );
      }
      const depth = await deps.observations.settlementDepthOf(
        finding.fraudulentBlockOutRef,
      );
      if (depth < policy.minSettlementDepth) {
        return refused(
          "policy",
          `the faulted block sits at depth ${depth.toString()}, under the ${policy.minSettlementDepth.toString()} settlement gate.`,
        );
      }
    }
    if (policy.maturityGuardFactor > 0) {
      if (deps.observations.remainingMaturityMs === undefined) {
        throw nativeScriptDecodingSubmitError(
          "the policy gates on remaining maturity but deps carry no remainingMaturityMs observer.",
        );
      }
      const remainingMs = await deps.observations.remainingMaturityMs(
        finding.fraudulentBlockOutRef,
      );
      const predictedMs = totalTxCount * policy.assumedMillisPerTx;
      if (remainingMs < policy.maturityGuardFactor * predictedMs) {
        return refused(
          "policy",
          `remaining maturity ${remainingMs.toString()}ms is under ${policy.maturityGuardFactor.toString()}× the predicted ${predictedMs.toString()}ms serial duration.`,
        );
      }
    }
    const overBudget = requireBudget(cursor, 0);
    if (overBudget !== null) {
      return refused("policy", overBudget);
    }
  }

  // 7. Drive. Every submitter is idempotent-by-reconstruction; an
  //    unexpected abort stalls loudly with the thread's position.
  const txHashes: string[] = [];
  const submittedThisRun = () => txHashes.length;
  let currentOutRef: string | null =
    cursor.at === "init" ? null : cursor.threadOutRef;
  try {
    while (true) {
      // §4.3: the budget is re-checked as the loop progresses. A breach
      // mid-thread stalls (the thread is real; cancellation is explicit).
      if (cursor.at !== "init") {
        const overBudget = requireBudget(cursor, txHashes.length);
        if (overBudget !== null) {
          return stalled(
            `budget breached mid-thread: ${overBudget}`,
            currentOutRef,
            null,
          );
        }
      }
      switch (cursor.at) {
        case "init": {
          const result = await submitNativeScriptDecodingInit({
            lucid,
            blueprint: deps.blueprint,
            network: deps.network,
            contracts,
            category: deps.category,
            catalogue: deps.catalogue,
            signer,
            fraudulentBlockOutRef: finding.fraudulentBlockOutRef,
            fraudulentHeaderHash: headerHash,
            witnessReferenceScripts: deps.witnessReferenceScripts,
          });
          txHashes.push(result.txHash);
          currentOutRef = result.nextThreadOutRef;
          await journal({
            phase: "init",
            message: "thread minted",
            txHash: result.txHash,
            threadOutRef: currentOutRef,
          });
          cursor = { at: "step01", threadOutRef: currentOutRef };
          break;
        }
        case "step01": {
          const shared = {
            lucid,
            contracts,
            categoryId: deps.category.categoryId,
            signer,
            threadOutRef: cursor.threadOutRef,
            referenceScriptUtxo: referenceScriptUtxos.step01,
            witnessReferenceScripts: deps.witnessReferenceScripts,
          };
          const result =
            finding.sourceKind === NATIVE_SCRIPT_DECODING_SOURCE_KIND_NORMAL_V1
              ? await submitNativeScriptDecodingStep01BindNormal({
                  ...shared,
                  blueprint: deps.blueprint,
                  network: deps.network,
                  stateQueueBlockOutRef: finding.fraudulentBlockOutRef,
                  txInclusion: await deps.evidence.txInclusion(finding),
                  publishedProofChunks:
                    (await deps.evidence.publishedProofChunks?.(finding)) ??
                    undefined,
                })
              : await submitNativeScriptDecodingStep01RecordForced({
                  ...shared,
                  direction: finding.direction,
                });
          txHashes.push(result.txHash);
          currentOutRef = result.nextThreadOutRef;
          await journal({
            phase: "step01",
            message: "source bound",
            txHash: result.txHash,
            threadOutRef: currentOutRef,
          });
          cursor = { at: "step02", threadOutRef: currentOutRef };
          break;
        }
        case "step02": {
          const isDirectionA =
            finding.direction ===
            NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE_V1;
          const result = await submitNativeScriptDecodingStep02({
            lucid,
            contracts,
            categoryId: deps.category.categoryId,
            signer,
            threadOutRef: cursor.threadOutRef,
            reconstruction: await deps.evidence.reconstruction(finding),
            forcedOrderKey:
              finding.event.kind === "forcedEvent"
                ? Data.from(finding.event.orderKeyCbor, OutputReference)
                : undefined,
            chosenOutpoint: isDirectionA
              ? {
                  sourceKind: finding.accusedOutpointSourceKind,
                  cursor: finding.accusedOutpointCursor,
                }
              : undefined,
            referenceScriptUtxo: referenceScriptUtxos.step02,
          });
          txHashes.push(result.txHash);
          currentOutRef = result.nextThreadOutRef;
          await journal({
            phase: "step02",
            message: "committed claims opened",
            txHash: result.txHash,
            threadOutRef: currentOutRef,
          });
          cursor = { at: "bind", threadOutRef: currentOutRef };
          break;
        }
        case "bind": {
          const shared = {
            lucid,
            contracts,
            categoryId: deps.category.categoryId,
            signer,
            threadOutRef: cursor.threadOutRef,
            referenceScriptUtxo: referenceScriptUtxos.step03,
          };
          if (!needsBindOutpoint) {
            // §7.2 cardinality close. The count face proves against the
            // door's authenticated count and needs the subject items.
            const needsCount =
              (finding.accusedOutpointSourceKind === 0n ||
                finding.accusedOutpointSourceKind === 1n) &&
              finding.accusedOutpointCursor >= 0n;
            const subject = needsCount
              ? await deps.evidence.subjectTx(finding)
              : null;
            const result =
              await submitNativeScriptDecodingStep03BindOutOfDomain({
                ...shared,
                nativeTxCompactCbor: subject?.nativeTxCompactCbor,
                subjectFieldInputs: subject?.subjectFieldInputs,
                publishCarriage: deps.publishCarriage,
              });
            txHashes.push(result.txHash);
            currentOutRef = result.nextThreadOutRef;
            await journal({
              phase: "bind",
              message: "out-of-domain accusation closed",
              txHash: result.txHash,
              threadOutRef: currentOutRef,
            });
            cursor = { at: "step04", threadOutRef: currentOutRef };
            break;
          }
          if (descriptorCbor === null) {
            throw nativeScriptDecodingSubmitError(
              "bind reached without descriptor evidence.",
            );
          }
          const subject = await deps.evidence.subjectTx(finding);
          const result = await submitNativeScriptDecodingStep03BindOutpoint({
            ...shared,
            nativeTxCompactCbor: subject.nativeTxCompactCbor,
            subjectFieldInputs: subject.subjectFieldInputs,
            descriptorCbor,
            ledgerTrie: await deps.evidence.ledgerTrie(finding),
            plan: plan ?? undefined,
            referenceScriptItemBytes: referenceScriptItemBytes ?? undefined,
            publishCarriage: deps.publishCarriage,
          });
          txHashes.push(result.txHash);
          currentOutRef = result.nextThreadOutRef;
          const machinePlan =
            plan !== null &&
            plan.route === NativeScriptDecodingPlanRoutesV1.Machine
              ? plan
              : null;
          await journal({
            phase: "bind",
            message:
              machinePlan !== null
                ? "outpoint bound, machine committed"
                : "outpoint bound, closed at bind",
            txHash: result.txHash,
            threadOutRef: currentOutRef,
          });
          cursor =
            machinePlan === null
              ? { at: "step04", threadOutRef: currentOutRef }
              : machinePlan.segments.length > 0
                ? { at: "scan", threadOutRef: currentOutRef, segmentIndex: 0 }
                : { at: "verdict", threadOutRef: currentOutRef };
          break;
        }
        case "scan": {
          if (plan === null || referenceScriptItemBytes === null) {
            throw nativeScriptDecodingSubmitError(
              "scan reached without a plan.",
            );
          }
          const segment = plan.segments[cursor.segmentIndex];
          if (segment === undefined) {
            throw nativeScriptDecodingSubmitError(
              `plan has no segment ${cursor.segmentIndex.toString()}.`,
            );
          }
          const result = await submitNativeScriptDecodingStep03Scan({
            lucid,
            contracts,
            categoryId: deps.category.categoryId,
            signer,
            threadOutRef: cursor.threadOutRef,
            segment,
            referenceScriptItemBytes,
            referenceScriptUtxo: referenceScriptUtxos.step03,
          });
          txHashes.push(result.txHash);
          currentOutRef = result.nextThreadOutRef;
          await journal({
            phase: "scan",
            message: `segment ${(cursor.segmentIndex + 1).toString()}/${plan.segments.length.toString()} advanced`,
            txHash: result.txHash,
            threadOutRef: currentOutRef,
          });
          cursor =
            cursor.segmentIndex + 1 < plan.segments.length
              ? {
                  at: "scan",
                  threadOutRef: currentOutRef,
                  segmentIndex: cursor.segmentIndex + 1,
                }
              : { at: "verdict", threadOutRef: currentOutRef };
          break;
        }
        case "verdict": {
          if (plan === null) {
            throw nativeScriptDecodingSubmitError(
              "verdict reached without a plan.",
            );
          }
          const result = await submitNativeScriptDecodingStep03Verdict({
            lucid,
            contracts,
            categoryId: deps.category.categoryId,
            signer,
            threadOutRef: cursor.threadOutRef,
            verdict: plan.verdict,
            referenceScriptItemBytes:
              plan.verdict.window === null
                ? undefined
                : (referenceScriptItemBytes ?? undefined),
            referenceScriptUtxo: referenceScriptUtxos.step03,
          });
          txHashes.push(result.txHash);
          currentOutRef = result.nextThreadOutRef;
          await journal({
            phase: "verdict",
            message: "verdict closed",
            txHash: result.txHash,
            threadOutRef: currentOutRef,
          });
          cursor = { at: "step04", threadOutRef: currentOutRef };
          break;
        }
        case "step04": {
          const result = await submitNativeScriptDecodingStep04({
            lucid,
            contracts,
            categoryId: deps.category.categoryId,
            signer,
            threadOutRef: cursor.threadOutRef,
            referenceScriptUtxo: referenceScriptUtxos.step04,
            witnessReferenceScripts: deps.witnessReferenceScripts,
          });
          txHashes.push(result.txHash);
          await journal({
            phase: "step04",
            message: `fraud-proof token minted (${submittedThisRun().toString()} txs this run)`,
            txHash: result.txHash,
            threadOutRef: result.fraudProofOutRef,
          });
          await journal({ phase: "outcome", message: "proven" });
          return {
            kind: "proven",
            fraudProofUnit: result.fraudProofUnit,
            fraudProofOutRef: result.fraudProofOutRef,
            txHashes,
          };
        }
      }
    }
  } catch (cause) {
    return stalled(
      `unexpected abort at ${cursor.at}: ${toError(cause).message}`,
      currentOutRef,
      cause,
    );
  }
};

/** The §4.3 core as an Effect, for consumers composing in that idiom. */
export const proveNativeScriptDecodingFaultV1 = (
  finding: NativeScriptDecodingFindingV1,
  deps: NativeScriptDecodingProverDepsV1,
): Effect.Effect<NativeScriptDecodingProofOutcomeV1, Error> =>
  Effect.tryPromise({
    try: () => runNativeScriptDecodingProverV1(finding, deps),
    catch: toError,
  });
