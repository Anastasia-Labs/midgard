import * as SDK from "@al-ft/midgard-sdk";
import {
  LucidEvolution,
  type OutRef,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  configuredContractDeploymentInfoPath,
  readDeploymentManifestFile,
} from "../commands/contract-deployment-info.build-deployment-manifest.js";

/**
 * Output indices read per initialization transaction. It creates 13 protocol
 * outputs and at most one wallet change output.
 */
const INIT_OUTPUT_PROBE = 32;

/** The outrefs read for the outputs of initialization `txHash`. */
export const initOutputOutRefs = (txHash: string): OutRef[] =>
  Array.from({ length: INIT_OUTPUT_PROBE }, (_, outputIndex) => ({
    txHash,
    outputIndex,
  }));

/**
 * The hub-oracle output among `utxos`, when one of `initTxHashes` created it.
 * The output can never be spent and its policy is bound to the nonce, so it
 * proves that initialization landed.
 */
export const hubOracleWitnessAmong = (
  utxos: readonly UTxO[],
  initTxHashes: readonly string[],
  hubOraclePolicyId: string,
): UTxO | undefined => {
  const hubOracleUnit = toUnit(hubOraclePolicyId, SDK.HUB_ORACLE_ASSET_NAME);
  return utxos.find(
    (utxo) =>
      initTxHashes.includes(utxo.txHash) &&
      (utxo.assets[hubOracleUnit] ?? 0n) > 0n,
  );
};

/** The initialization transaction the configured deployment manifest records. */
export const configuredInitTxHash: Effect.Effect<string, SDK.LucidError> =
  Effect.suspend(() => {
    const manifestPath = configuredContractDeploymentInfoPath();
    return Effect.try({
      try: () =>
        readDeploymentManifestFile(manifestPath).steps.initProtocol.txHash,
      catch: (cause) =>
        new SDK.LucidError({
          message: `Failed to read the deployment manifest at ${manifestPath}`,
          cause,
        }),
    }).pipe(
      Effect.filterOrFail(
        (txHash): txHash is string => txHash !== undefined,
        () =>
          new SDK.LucidError({
            message: `The deployment manifest at ${manifestPath} records no initialization transaction`,
            cause: "steps.initProtocol.txHash is absent",
          }),
      ),
    );
  });

/**
 * The hub-oracle output of initialization `initTxHash`, from its outputs read
 * in one exact-outref batch, or from output `knownOutputIndex` alone once a
 * read found it there: the output never moves.
 */
export const fetchHubOracleWitness = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  initTxHash: string,
  knownOutputIndex?: number,
): Effect.Effect<UTxO, SDK.LucidError> =>
  Effect.gen(function* () {
    const utxos = yield* Effect.tryPromise({
      try: () =>
        lucid.utxosByOutRef(
          knownOutputIndex === undefined
            ? initOutputOutRefs(initTxHash)
            : [{ txHash: initTxHash, outputIndex: knownOutputIndex }],
        ),
      catch: (cause) =>
        new SDK.LucidError({
          message: `Failed to read the outputs of initialization ${initTxHash}`,
          cause,
        }),
    });
    const witness = hubOracleWitnessAmong(
      utxos,
      [initTxHash],
      contracts.hubOracle.policyId,
    );
    if (witness === undefined) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message: `No visible output of initialization ${initTxHash} carries the hub-oracle NFT`,
          cause: `outputs_read=${utxos.length.toString()}`,
        }),
      );
    }
    return witness;
  });
