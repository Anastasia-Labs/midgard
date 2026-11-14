/**
 * An in-process L2 node behind lucid-midgard's provider interface. It serves
 * the wallet outputs it is given, reports the deployment marker of the
 * manifest it serves, prices redeemers with the node's evaluator, and judges
 * every submission with the node's own validation (midgard-validation's
 * Phase A, then Phase B against its outputs), answering the verdict as the
 * transaction's status.
 */
import {
  decodeMidgardUtxo,
  encodeMidgardTxOutput,
  type MidgardProvider,
  type MidgardUtxo,
  type OutRef,
  outRefToCbor,
  type TxStatus,
} from "@al-ft/lucid-midgard";
import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
} from "@al-ft/midgard-core/codec";
import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core/consensus-profile";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { type L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import {
  evaluateMidgardTxRedeemers,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
} from "@al-ft/midgard-validation";
import { Effect } from "effect";

/** The Preprod fee coefficients, so fees are realistic. */
const MIN_FEE_A = 44n;
const MIN_FEE_B = 155_381n;
const NETWORK_ID = 0n;
const CURRENT_SLOT = 1_000n;

/** An ADA-only output at `address`, under a distinct made-up producer. */
export const l2Output = (
  address: string,
  producer: number,
  lovelace = 10_000_000n,
): MidgardUtxo => {
  const outRef: OutRef = {
    txHash: producer.toString(16).padStart(2, "0").repeat(32),
    outputIndex: 0,
  };
  return decodeMidgardUtxo({
    outRef,
    outRefCbor: outRefToCbor(outRef),
    outputCbor: encodeMidgardTxOutput(address, { lovelace }),
  });
};

export type InProcessL2Node = {
  readonly provider: MidgardProvider;
  /** Every transaction submitted, as canonical CBOR hex, in order. */
  readonly submitted: () => readonly string[];
};

export const inProcessL2Node = ({
  manifestId,
  outputs,
  l1SlotConfig,
  ledger = outputs,
}: {
  readonly manifestId: string;
  /** What the node serves for an address and evaluates redeemers against. */
  readonly outputs: readonly MidgardUtxo[];
  readonly l1SlotConfig: L1SlotConfig;
  /**
   * What Phase B resolves a submission's inputs against (default `outputs`):
   * fewer when an output is spent between evaluation and submission.
   */
  readonly ledger?: readonly MidgardUtxo[];
}): InProcessL2Node => {
  const stateOf = (utxos: readonly MidgardUtxo[]) =>
    new Map(
      utxos.map((utxo) => [
        Buffer.from(utxo.cbor!.outRef!).toString("hex"),
        Buffer.from(utxo.cbor!.output!),
      ]),
    );
  const served = stateOf(outputs);
  const preState = stateOf(ledger);
  const submitted: string[] = [];
  const verdicts = new Map<string, TxStatus>();
  const strictnessProfile = "production";

  const judge = async (txHex: string): Promise<TxStatus> => {
    await ledgerEd25519SignatureReady();
    const txCbor = Buffer.from(txHex, "hex");
    const txId = computeMidgardNativeTxId(
      decodeMidgardNativeTxFullFromCanonicalCbor(txCbor),
    );
    const phaseA = await Effect.runPromise(
      runPhaseAValidation(
        [
          {
            txId,
            txCbor,
            arrivalSeq: 0n,
            createdAt: new Date(0),
          },
        ],
        {
          expectedNetworkId: NETWORK_ID,
          minFeeA: MIN_FEE_A,
          minFeeB: MIN_FEE_B,
          concurrency: 1,
          strictnessProfile,
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        },
      ),
    );
    const phaseB = await Effect.runPromise(
      runPhaseBValidationWithPatch(phaseA.accepted, preState, {
        nowCardanoSlotNo: CURRENT_SLOT,
        l1SlotConfig,
        bucketConcurrency: 1,
        enforceScriptBudget: true,
      }),
    );
    const [rejected] = [...phaseA.rejected, ...phaseB.rejected];
    if (rejected !== undefined)
      return {
        kind: "rejected",
        txId: txId.toString("hex"),
        code: rejected.code,
        detail: rejected.detail,
      };
    return { kind: "accepted", txId: txId.toString("hex") };
  };

  const provider: MidgardProvider = {
    getUtxos: async (address) =>
      outputs.filter((utxo) => utxo.output.address === address),
    getUtxoByOutRef: async (outRef) =>
      outputs.find(
        (utxo) =>
          utxo.txHash === outRef.txHash &&
          utxo.outputIndex === outRef.outputIndex,
      ),
    getProtocolInfo: async () => ({
      apiVersion: 1,
      network: "Preprod",
      midgardNativeTxVersion: 1,
      currentSlot: CURRENT_SLOT,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      deploymentMarker: makeDeploymentMarker(manifestId),
      supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
      codecSupportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
      protocolFeeParameters: { minFeeA: MIN_FEE_A, minFeeB: MIN_FEE_B },
      submissionLimits: {
        maxSubmitTxCborBytes:
          MIDGARD_CONSENSUS_PROFILE.limits.maxTxCanonicalCborBytes,
      },
      validation: {
        strictnessProfile,
        localValidationIsAuthoritative: false,
      },
    }),
    getProtocolParameters: async () => ({
      minFeeA: MIN_FEE_A,
      minFeeB: MIN_FEE_B,
      networkId: NETWORK_ID,
      currentSlot: CURRENT_SLOT,
      strictnessProfile,
    }),
    getCurrentSlot: async () => CURRENT_SLOT,
    evaluateTx: async (txHex) => {
      const evaluation = await Effect.runPromise(
        evaluateMidgardTxRedeemers({
          txCbor: Buffer.from(txHex, "hex"),
          readState: () => Effect.succeed(served),
          expectedNetworkId: NETWORK_ID,
          limits: MIDGARD_CONSENSUS_LIMITS,
          config: { l1SlotConfig },
        }),
      );
      if (evaluation.kind !== "evaluated") throw new Error(evaluation.detail);
      return evaluation.redeemers;
    },
    submitTx: async (txHex) => {
      submitted.push(txHex);
      const verdict = await judge(txHex);
      verdicts.set(verdict.txId, verdict);
      return {
        txId: verdict.txId,
        status: "queued",
        httpStatus: 202,
        duplicate: false,
      };
    },
    getTxStatus: async (txId) =>
      verdicts.get(txId) ?? { kind: "not_found", txId },
    diagnostics: () => ({
      endpoint: "memory://midgard-user-cli",
      protocolInfoSource: "node",
    }),
  };
  return { provider, submitted: () => [...submitted] };
};
