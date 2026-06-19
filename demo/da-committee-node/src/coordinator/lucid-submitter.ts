import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { DaAttestationCandidateRecord } from "../domain.js";
import type { DaAttestationValidatorSet } from "../l1/deployment.js";
import type { DaAttestationReferenceScripts } from "../l1/reference-scripts.js";
import {
  refreshL1SubmitterPlainAdaUtxos,
  signSubmitAndConfirm,
} from "../l1/submitter.js";
import type { OnChainAttestationSubmitter } from "./on-chain.js";
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
        deps.signSubmit ??
        ((tx) => signSubmitAndConfirm(deps.lucid, tx)),
      refreshFundingUtxos:
        deps.refreshFundingUtxos ??
        (async () => {
          await refreshL1SubmitterPlainAdaUtxos(deps.lucid);
        }),
    };
  }

  async initAttestation(record: {
    readonly headerHash: string;
  }): Promise<string> {
    const daParams = await this.fetchDaParamsUtxo();
    const target = await this.fetchTarget(record.headerHash);
    await this.deps.refreshFundingUtxos();
    const tx = await buildInitDaAttestationTx({
      lucid: this.deps.lucid,
      contracts: this.deps.contracts,
      daParamsUtxo: daParams.utxo,
      daParamsDatum: daParams.datum,
      target,
      referenceScripts: this.deps.referenceScripts,
    });
    return this.deps.signSubmit(tx);
  }

  async addSignatures({
    candidate,
    packedWitnessesHex,
    signerIndexes,
  }: Parameters<OnChainAttestationSubmitter["addSignatures"]>[0]): Promise<string> {
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
    return this.deps.signSubmit(tx);
  }

  async applyAttestation({
    record,
    candidate,
  }: Parameters<OnChainAttestationSubmitter["applyAttestation"]>[0]): Promise<string> {
    const target = await this.fetchTarget(record.headerHash);
    const attestation = await this.fetchCandidateUtxo(candidate);
    await this.deps.refreshFundingUtxos();
    const tx = await buildApplyAttestationTx({
      lucid: this.deps.lucid,
      contracts: this.deps.contracts,
      target,
      attestationUtxo: attestation.utxo,
      referenceScripts: this.deps.referenceScripts,
    });
    const txHash = await this.deps.signSubmit(tx);
    await this.waitForApplied(record.headerHash);
    return txHash;
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

  private async fetchTarget(headerHash: string): Promise<DaAttestationTarget> {
    const target = await this.findStateQueueHeader(headerHash);
    if (target.stateQueueNode.da_attestation !== SDK.NO_DA_ATTESTATION) {
      throw new Error(
        `state queue header ${headerHash} already has DA attestation ${target.stateQueueNode.da_attestation}`,
      );
    }
    return target;
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
      if (
        target.stateQueueNode.da_attestation ===
        this.deps.contracts.daAttestation.policyId
      ) {
        return;
      }
      if (target.stateQueueNode.da_attestation !== SDK.NO_DA_ATTESTATION) {
        throw new Error(
          `state queue header ${headerHash} was attested by unexpected policy ${target.stateQueueNode.da_attestation}`,
        );
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
