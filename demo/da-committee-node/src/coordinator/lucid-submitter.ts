import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { daL1SubmitterMinPlainAdaLovelace } from "../config.js";
import type { DaAttestationCandidateRecord } from "../domain.js";
import { classifyDaAttestationMarker } from "../l1/attestation-marker.js";
import type { DaAttestationValidatorSet } from "../l1/deployment.js";
import type { DaAttestationReferenceScripts } from "../l1/reference-scripts.js";
import {
  type L1SubmitterReadinessSummary,
  refreshL1SubmitterPlainAdaUtxos,
  signSubmitAndConfirm,
} from "../l1/submitter.js";
import type {
  AttestationSubmissionResult,
  OnChainAttestationSubmitter,
} from "./on-chain.js";
import {
  buildAddSignaturesTx,
  buildApplyAttestationTx,
  buildInitDaAttestationTx,
  daAttestationApplyValidityRange,
  type DaAttestationTarget,
} from "./tx-builders.js";

/**
 * The submitter's spendable plain ADA against what the next init needs
 * (`daL1SubmitterMinPlainAdaLovelace`), checked at every funding refresh and
 * again once an init has locked its bond.
 */
export type DaBondFundingCheck = {
  readonly checkedAt: string;
  readonly plainAdaLovelace: bigint;
  readonly requiredLovelace: bigint;
  readonly sufficient: boolean;
};

export type LucidDaAttestationSubmitterDeps = {
  readonly lucid: LucidEvolution;
  readonly contracts: DaAttestationValidatorSet;
  readonly referenceScripts: DaAttestationReferenceScripts;
  readonly availabilityParameters: SDK.DaAvailabilityParameters;
  readonly signSubmit?: (tx: TxSignBuilder) => Promise<string>;
  readonly refreshFundingUtxos?: () => Promise<
    L1SubmitterReadinessSummary | undefined
  >;
  /** Receives every bond funding check; readiness reports a short one. */
  readonly recordBondFunding?: (check: DaBondFundingCheck) => void;
  /** Where bond funding warnings go. Defaults to stderr. */
  readonly log?: (line: string) => void;
  readonly postSubmitVerificationRetryCount?: number;
  readonly postSubmitVerificationDelayMs?: number;
  /**
   * The submitter's clock in POSIX milliseconds, from which the apply validity
   * range is derived. Defaults to Lucid's current slot, which on a live
   * provider is the wall clock rather than the chain tip.
   */
  readonly currentTime?: () => bigint;
};

export class LucidDaAttestationSubmitter
  implements OnChainAttestationSubmitter
{
  private readonly deps: LucidDaAttestationSubmitterDeps & {
    readonly signSubmit: (tx: TxSignBuilder) => Promise<string>;
    readonly refreshFundingUtxos: () => Promise<
      L1SubmitterReadinessSummary | undefined
    >;
    readonly currentTime: () => bigint;
  };

  constructor(deps: LucidDaAttestationSubmitterDeps) {
    this.deps = {
      ...deps,
      signSubmit:
        deps.signSubmit ?? ((tx) => signSubmitAndConfirm(deps.lucid, tx)),
      refreshFundingUtxos:
        deps.refreshFundingUtxos ??
        (() => refreshL1SubmitterPlainAdaUtxos(deps.lucid)),
      currentTime:
        deps.currentTime ??
        (() => BigInt(deps.lucid.slotToUnixTime(deps.lucid.currentSlot()))),
    };
  }

  async initAttestation(record: {
    readonly headerHash: string;
    readonly availabilityCommitmentCbor: string;
    readonly availabilityCommitmentDigest: string;
  }): Promise<AttestationSubmissionResult> {
    const target = await this.fetchUnattestedTarget(record.headerHash);
    if (target.status === "already_attested") {
      return { status: "already_attested" };
    }
    await this.refreshFunding();
    const daParams = await this.fetchDaParamsUtxo();
    const rescueBeneficiaryAddress = await this.deps.lucid.wallet().address();
    const rescueBeneficiary = await Effect.runPromise(
      SDK.addressDataFromBech32(rescueBeneficiaryAddress),
    );
    const tx = await buildInitDaAttestationTx({
      lucid: this.deps.lucid,
      contracts: this.deps.contracts,
      daParamsUtxo: daParams.utxo,
      daParamsDatum: daParams.datum,
      target,
      referenceScripts: this.deps.referenceScripts,
      rescueBeneficiary,
      availabilityCommitment: SDK.parseDaAvailabilityCommitmentCbor(
        record.availabilityCommitmentCbor,
      ),
      attestationOutputLovelace:
        this.deps.availabilityParameters.da_bond_lovelace,
    });
    const txHash = await this.deps.signSubmit(tx);
    // The bond just left the wallet, so whether it covers the next init is
    // known now, not only when the next header's init starts. The init itself
    // has landed, so a failed check must not fail it.
    await this.refreshFunding().catch((error: unknown) => {
      this.log(
        `${JSON.stringify({
          event: "l1_submitter_bond_funding_check_failed",
          error: error instanceof Error ? error.message : String(error),
        })}\n`,
      );
    });
    return { status: "submitted", txHash };
  }

  async addSignatures({
    record,
    candidate,
    packedWitnessesHex,
    signerIndexes,
  }: Parameters<
    OnChainAttestationSubmitter["addSignatures"]
  >[0]): Promise<AttestationSubmissionResult> {
    const target = await this.fetchUnattestedTarget(record.headerHash);
    if (target.status === "already_attested") {
      return { status: "already_attested" };
    }
    const daParams = await this.fetchDaParamsUtxo();
    const attestation = await this.fetchCandidateUtxo(candidate);
    await this.refreshFunding();
    const tx = await buildAddSignaturesTx({
      lucid: this.deps.lucid,
      contracts: this.deps.contracts,
      daParamsUtxo: daParams.utxo,
      attestationUtxo: attestation.utxo,
      attestationDatum: attestation.datum,
      packedWitnessesHex,
      signerIndexes,
      referenceScripts: this.deps.referenceScripts,
    });
    return { status: "submitted", txHash: await this.deps.signSubmit(tx) };
  }

  async applyAttestation({
    record,
    candidate,
  }: Parameters<
    OnChainAttestationSubmitter["applyAttestation"]
  >[0]): Promise<AttestationSubmissionResult> {
    const target = await this.fetchUnattestedTarget(record.headerHash);
    if (target.status === "already_attested") {
      return { status: "already_attested" };
    }
    // Derived before any further L1 read: once the attestation deadline has
    // passed this rejects with the SDK's `validity_range_past_deadline` build
    // error, and nothing for this header can be built or submitted again.
    const validityRange = await daAttestationApplyValidityRange({
      target,
      currentTime: this.deps.currentTime(),
    });
    const attestation = await this.fetchCandidateUtxo(candidate);
    const daParams = await this.fetchDaParamsUtxo();
    const hubOracleRefInput = await Effect.runPromise(
      SDK.fetchHubOracleUTxOProgram(this.deps.lucid, {
        hubOracleAddress: this.deps.contracts.hubOracle.spendingScriptAddress,
        hubOraclePolicyId: this.deps.contracts.hubOracle.policyId,
      }),
    );
    await this.refreshFunding();
    const tx = await buildApplyAttestationTx({
      lucid: this.deps.lucid,
      contracts: this.deps.contracts,
      target,
      attestationUtxo: attestation.utxo,
      attestationDatum: attestation.datum,
      daParamsUtxo: daParams.utxo,
      daParamsDatum: daParams.datum,
      referenceScripts: this.deps.referenceScripts,
      hubOracleRefInput: hubOracleRefInput.utxo,
      validityRange,
    });
    const txHash = await this.deps.signSubmit(tx);
    await this.waitForApplied(record.headerHash);
    return { status: "submitted", txHash };
  }

  /**
   * Refreshes the wallet view and checks it against the next init. Each init
   * locks one bond for good, so a wallet that cannot cover the next one is
   * reported loudly on stderr and to readiness, and a topped-up one clears at
   * the next refresh. Nothing is blocked: the headroom is conservative, and a
   * build fails on its own when the wallet really is short.
   */
  private async refreshFunding(): Promise<void> {
    const funding = await this.deps.refreshFundingUtxos();
    if (funding === undefined) {
      return;
    }
    const requiredLovelace = daL1SubmitterMinPlainAdaLovelace(
      this.deps.availabilityParameters.da_bond_lovelace,
    );
    const check: DaBondFundingCheck = {
      checkedAt: new Date().toISOString(),
      plainAdaLovelace: funding.plainAdaLovelace,
      requiredLovelace,
      sufficient: funding.plainAdaLovelace >= requiredLovelace,
    };
    this.deps.recordBondFunding?.(check);
    if (!check.sufficient) {
      this.log(
        `${JSON.stringify({
          event: "l1_submitter_bond_funding_short",
          address: funding.address,
          plainAdaLovelace: check.plainAdaLovelace.toString(),
          requiredLovelace: requiredLovelace.toString(),
        })}\n`,
      );
    }
  }

  private log(line: string): void {
    (this.deps.log ?? ((entry: string) => process.stderr.write(entry)))(line);
  }

  private async fetchDaParamsUtxo(): Promise<{
    readonly utxo: UTxO;
    readonly datum: SDK.DaParamsDatum;
  }> {
    const unit = SDK.daParamsUnit(this.deps.contracts.daParamsGovernor);
    const utxos = await this.deps.lucid.utxosAtWithUnit(
      this.deps.contracts.daParamsGovernor.spendingScriptAddress,
      unit,
    );
    if (utxos.length !== 1) {
      throw new Error(
        `expected exactly one DA params UTxO, found ${utxos.length.toString()}`,
      );
    }
    return {
      utxo: utxos[0]!,
      datum: decodeInlineDatum<SDK.DaParamsDatum>(
        utxos[0]!,
        SDK.DaParamsDatum as never,
        "DA params",
      ),
    };
  }

  private async fetchUnattestedTarget(
    headerHash: string,
  ): Promise<
    | ({ readonly status: "unattested" } & DaAttestationTarget)
    | { readonly status: "already_attested" }
  > {
    const target = await this.findStateQueueHeader(headerHash);
    const marker = classifyDaAttestationMarker(
      target.stateQueueNode.da_attestation,
    );
    if (marker.kind === "already_attested_expected") {
      return { status: "already_attested" };
    }
    return { ...target, status: "unattested" };
  }

  private async findStateQueueHeader(
    headerHash: string,
  ): Promise<DaAttestationTarget> {
    const stateQueueUtxos = await SDK.fetchSortedStateQueueUTxOs(
      this.deps.lucid,
      {
        stateQueueAddress: this.deps.contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: this.deps.contracts.stateQueue.policyId,
      },
    );
    for (const stateQueueUtxo of stateQueueUtxos) {
      if (stateQueueUtxo.datum.key === "Empty") {
        continue;
      }
      const stateQueueNode = await Effect.runPromise(
        SDK.getStateQueueNodeFromStateQueueDatum(stateQueueUtxo.datum),
      );
      const computedHeaderHash = await Effect.runPromise(
        SDK.hashBlockHeader(stateQueueNode.header),
      );
      if (computedHeaderHash !== headerHash) {
        continue;
      }
      return { stateQueueUtxo, stateQueueNode, headerHash };
    }
    throw new Error(`state queue header ${headerHash} was not found`);
  }

  private async waitForApplied(headerHash: string): Promise<void> {
    const retryCount = this.deps.postSubmitVerificationRetryCount ?? 12;
    const retryDelayMs = this.deps.postSubmitVerificationDelayMs ?? 2_000;
    for (let attempt = 0; attempt <= retryCount; attempt += 1) {
      const target = await this.findStateQueueHeader(headerHash);
      const marker = classifyDaAttestationMarker(
        target.stateQueueNode.da_attestation,
      );
      if (marker.kind === "already_attested_expected") {
        return;
      }
      if (attempt < retryCount) {
        await sleep(retryDelayMs);
      }
    }
    throw new Error(
      `state queue header ${headerHash} did not show DA attestation policy ${this.deps.contracts.daAttestation.policyId} after apply confirmation`,
    );
  }

  private async fetchCandidateUtxo(
    candidate: DaAttestationCandidateRecord,
  ): Promise<{ readonly utxo: UTxO; readonly datum: SDK.DaAttestationDatum }> {
    const outRef = parseOutRef(candidate.outRef);
    const utxos = await this.deps.lucid.utxosByOutRef([outRef]);
    if (utxos.length !== 1) {
      throw new Error(
        `expected exactly one DA attestation UTxO at ${candidate.outRef}, found ${utxos.length.toString()}`,
      );
    }
    const utxo = utxos[0]!;
    const datum = decodeInlineDatum<SDK.DaAttestationDatum>(
      utxo,
      SDK.DaAttestationDatum as never,
      "DA attestation",
    );
    if (datum.header_hash !== candidate.headerHash) {
      throw new Error(
        `DA attestation UTxO ${candidate.outRef} header hash mismatch`,
      );
    }
    return { utxo, datum };
  }
}

const decodeInlineDatum = <T>(
  utxo: UTxO,
  schema: Parameters<typeof Data.from>[1],
  label: string,
): T => {
  if (utxo.datum == null) {
    throw new Error(`${label} UTxO has no inline datum`);
  }
  return Data.from(utxo.datum, schema) as T;
};

const sleep = (delayMs: number): Promise<void> =>
  new Promise((resolve) => setTimeout(resolve, delayMs));

const parseOutRef = (
  value: string,
): { readonly txHash: string; readonly outputIndex: number } => {
  const [txHash, outputIndexText] = value.split("#");
  const outputIndex = Number(outputIndexText);
  if (
    txHash === undefined ||
    !/^[0-9a-f]{64}$/i.test(txHash) ||
    outputIndexText === undefined ||
    !Number.isSafeInteger(outputIndex) ||
    outputIndex < 0
  ) {
    throw new Error(`invalid out-ref ${value}`);
  }
  return { txHash: txHash.toLowerCase(), outputIndex };
};
