import {
  type MidgardTxInput,
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE,
  type NativeScriptDecodingScanThreadState,
  NativeScriptDecodingStep03OpenSubjectDatum,
} from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { PublishedProofChunk } from "../proof-chunk-carriage.js";
import { outRefLabel, type ResolvedProverSigner } from "../runtime.js";
import type { SubmitStep01TxInclusion } from "../step-support.js";
import type { TransitionTraceReconstruction } from "../transition-trace/reconstruct.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { NativeScriptDecodingContracts } from "./contracts.js";
import type { NativeScriptDecodingLedgerTrieHandle } from "./evidence.js";
import {
  type NativeScriptDecodingFinding,
  NativeScriptDecodingProvability,
} from "./finding.js";
import {
  NativeScriptDecodingPlanRoutes,
  type NativeScriptDecodingScanPlan,
} from "./scan-plan.js";
import {
  type NativeScriptDecodingCatalogueCategory,
  nativeScriptDecodingSubmitError,
} from "./submit-common.js";

// ## Policy (§4.3, defaults per §10 Q5)

export type NativeScriptDecodingProverPolicy = {
  /**
   * Minimum L1 depth of the faulted header's state-queue UTxO before the
   * core spends anything. Default 2,160 blocks, the mainnet security
   * parameter; a deployment's own finality policy is its manifest's L1
   * `confirmationDepth`. `0n` disables the gate.
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

export const NATIVE_SCRIPT_DECODING_PROVER_POLICY_DEFAULTS: NativeScriptDecodingProverPolicy =
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
export type NativeScriptDecodingProverEvidence = {
  /** Direction-A normal threads: the §2.4 committed-transaction inclusion. */
  readonly txInclusion: (
    finding: NativeScriptDecodingFinding,
  ) => Promise<SubmitStep01TxInclusion>;
  /** Optional #545 published-chunk carriage for the step-01 opening. */
  readonly publishedProofChunks?: (
    finding: NativeScriptDecodingFinding,
  ) => Promise<readonly PublishedProofChunk[] | null>;
  /** The disputed block's transition-trace reconstruction (step-02). */
  readonly reconstruction: (
    finding: NativeScriptDecodingFinding,
  ) => Promise<TransitionTraceReconstruction>;
  /** The committed transaction's compact bytes and accused field's items. */
  readonly subjectTx: (finding: NativeScriptDecodingFinding) => Promise<{
    readonly nativeTxCompactCbor: string;
    readonly subjectFieldInputs: readonly MidgardTxInput[];
  }>;
  /** The ledger's resolution of the accused outpoint, plus the item bytes. */
  readonly descriptor: (finding: NativeScriptDecodingFinding) => Promise<{
    readonly descriptorCbor: string;
    readonly referenceScriptItemBytes: Uint8Array | null;
  }>;
  /** Pre-state ledger trie whose root is the thread's `prior_ledger_root`. */
  readonly ledgerTrie: (
    finding: NativeScriptDecodingFinding,
  ) => Promise<NativeScriptDecodingLedgerTrieHandle>;
};

/**
 * Chain observations behind the policy gates. Each is required exactly
 * when the corresponding gate is active in the policy handed alongside.
 */
export type NativeScriptDecodingProverObservations = {
  readonly settlementDepthOf?: (
    fraudulentBlockOutRef: string,
  ) => Promise<bigint>;
  readonly remainingMaturityMs?: (
    fraudulentBlockOutRef: string,
  ) => Promise<number>;
};

export type NativeScriptDecodingProverEvent = {
  readonly phase:
    | "boundary"
    | "policy"
    | "init"
    | "step01"
    | "step02"
    | "openSubject"
    | "bindDescriptor"
    | "advanceOrClose"
    | "close"
    | "step04"
    | "outcome";
  readonly message: string;
  readonly headerHash: string;
  readonly txHash?: string;
  readonly threadOutRef?: string;
};

export type NativeScriptDecodingProverDeps = {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: NativeScriptDecodingContracts;
  readonly category: NativeScriptDecodingCatalogueCategory;
  readonly catalogue: {
    readonly policyId: string;
    readonly spendingScriptAddress: string;
    readonly root: string;
  };
  readonly signer: ResolvedProverSigner;
  readonly evidence: NativeScriptDecodingProverEvidence;
  readonly observations: NativeScriptDecodingProverObservations;
  readonly journal: (
    event: NativeScriptDecodingProverEvent,
  ) => void | Promise<void>;
  readonly policy: NativeScriptDecodingProverPolicy;
  /** Q3: mandatory authenticated published step reference scripts. */
  readonly referenceScriptUtxos?: {
    readonly step01?: UTxO;
    readonly step02?: UTxO;
    readonly step03OpenSubject?: UTxO;
    readonly step03BindDescriptor?: UTxO;
    readonly step03AdvanceOrClose?: UTxO;
    readonly step04?: UTxO;
  };
  /** Mandatory published shared witnesses used by init, step-01, and step-04. */
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  /** Force §8 tier-2 carriage publication on the bind transaction. */
  readonly publishCarriage?: boolean;
};

// ## Outcome (§4.3: data, not exceptions)

export type NativeScriptDecodingProofOutcome =
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

export type NativeScriptDecodingThreadPosition =
  | { readonly step: "none" }
  | {
      readonly step: "step01" | "step02" | "step04";
      readonly threadUtxo: UTxO;
    }
  | {
      readonly step:
        | "step03OpenSubject"
        | "step03BindDescriptor"
        | "step03AdvanceOrClose";
      readonly threadUtxo: UTxO;
      readonly state: NativeScriptDecodingScanThreadState;
    };

/** Locates the live thread by its NFT across all six custody addresses. */
export const locateNativeScriptDecodingThread = async ({
  lucid,
  contracts,
  threadUnit,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContracts;
  readonly threadUnit: string;
}): Promise<NativeScriptDecodingThreadPosition> => {
  for (const stepIndex of [0, 1, 2, 3, 4, 5] as const) {
    const utxos = await lucid.utxosAtWithUnit(
      contracts.steps[stepIndex].spendingScriptAddress,
      threadUnit,
    );
    const threadUtxo = utxos[0];
    if (threadUtxo === undefined) {
      continue;
    }
    if (stepIndex >= 2 && stepIndex <= 4) {
      const datum = Data.from(
        requireDatum(threadUtxo),
        NativeScriptDecodingStep03OpenSubjectDatum,
      );
      if (datum.data === null) {
        throw nativeScriptDecodingSubmitError(
          `thread ${outRefLabel(threadUtxo)} at split step 03 carries no state.`,
        );
      }
      const step =
        stepIndex === 2
          ? "step03OpenSubject"
          : stepIndex === 3
            ? "step03BindDescriptor"
            : "step03AdvanceOrClose";
      return { step, threadUtxo, state: datum.data };
    }
    const step =
      stepIndex === 0 ? "step01" : stepIndex === 1 ? "step02" : "step04";
    return { step, threadUtxo };
  }
  return { step: "none" };
};

export const requireDatum = (utxo: UTxO): string => {
  if (utxo.datum == null) {
    throw nativeScriptDecodingSubmitError(
      `thread UTxO ${outRefLabel(utxo)} has no inline datum.`,
    );
  }
  return utxo.datum;
};

// ## The drive cursor

export type DriveState =
  | { readonly at: "init" }
  | { readonly at: "step01"; readonly threadOutRef: string }
  | { readonly at: "step02"; readonly threadOutRef: string }
  | { readonly at: "openSubject"; readonly threadOutRef: string }
  | { readonly at: "bindDescriptor"; readonly threadOutRef: string }
  | {
      readonly at: "advanceOrClose";
      readonly threadOutRef: string;
      readonly segmentIndex: number;
    }
  | { readonly at: "close"; readonly threadOutRef: string }
  | { readonly at: "step04"; readonly threadOutRef: string };

export const step03TxCount = (
  finding: NativeScriptDecodingFinding,
  plan: NativeScriptDecodingScanPlan | null,
): number => {
  if (
    finding.provability ===
    NativeScriptDecodingProvability.OutOfDomainAccusation
  ) {
    return 1;
  }
  if (plan?.route !== NativeScriptDecodingPlanRoutes.Machine) {
    return 2;
  }
  const explicitClose =
    finding.direction === NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE
      ? 1
      : 0;
  return 2 + plan.segments.length + explicitClose;
};
