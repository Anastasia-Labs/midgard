import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { DaAttestationCandidateRecord } from "../domain.js";
import { classifyDaAttestationMarker } from "../l1/attestation-marker.js";
import type { DaAttestationValidatorSet } from "../l1/deployment.js";
import type { DaAttestationReferenceScripts } from "../l1/reference-scripts.js";
import {
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
  type DaAttestationTarget,
} from "./tx-builders.js";

export type LucidDaAttestationSubmitterDeps = {
  readonly lucid: LucidEvolution;
  readonly contracts: DaAttestationValidatorSet;
  readonly referenceScripts: DaAttestationReferenceScripts;
  readonly availabilityParameters: SDK.DaAvailabilityParametersV1;
  readonly signSubmit?: (tx: TxSignBuilder) => Promise<string>;
  readonly refreshFundingUtxos?: () => Promise<void>;
  readonly postSubmitVerificationRetryCount?: number;
  readonly postSubmitVerificationDelayMs?: number;
};

export class LucidDaAttestationSubmitter
  implements OnChainAttestationSubmitter
{
  private readonly deps: LucidDaAttestationSubmitterDeps & {
    readonly signSubmit: (tx: TxSignBuilder) => Promise<string>;
    readonly refreshFundingUtxos: () => Promise<void>;
  };

  constructor(deps: LucidDaAttestationSubmitterDeps) {
    this.deps = {
      ...deps,
      signSubmit:
        deps.signSubmit ?? ((tx) => signSubmitAndConfirm(deps.lucid, tx)),
      refreshFundingUtxos:
        deps.refreshFundingUtxos ??
        (async () => {
          await refreshL1SubmitterPlainAdaUtxos(deps.lucid);
        }),
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
    const daParams = await this.fetchDaParamsUtxo();
    await this.deps.refreshFundingUtxos();
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
      availabilityCommitment: SDK.parseDaAvailabilityCommitmentV1Cbor(
        record.availabilityCommitmentCbor,
      ),
      attestationOutputLovelace:
        this.deps.availabilityParameters.da_bond_lovelace,
    });
    return { status: "submitted", txHash: await this.deps.signSubmit(tx) };
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
    await this.deps.refreshFundingUtxos();
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
    const attestation = await this.fetchCandidateUtxo(candidate);
    const daParams = await this.fetchDaParamsUtxo();
    const hubOracleRefInput = await Effect.runPromise(
      SDK.fetchHubOracleUTxOProgram(this.deps.lucid, {
        hubOracleAddress: this.deps.contracts.hubOracle.spendingScriptAddress,
        hubOraclePolicyId: this.deps.contracts.hubOracle.policyId,
      }),
    );
    await this.deps.refreshFundingUtxos();
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
    });
    const txHash = await this.deps.signSubmit(tx);
    await this.waitForApplied(record.headerHash);
    return { status: "submitted", txHash };
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
        SDK.getStateQueueNodeV1FromStateQueueDatum(stateQueueUtxo.datum),
      );
      const computedHeaderHash = await Effect.runPromise(
        SDK.hashBlockHeaderV1(stateQueueNode.header),
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
