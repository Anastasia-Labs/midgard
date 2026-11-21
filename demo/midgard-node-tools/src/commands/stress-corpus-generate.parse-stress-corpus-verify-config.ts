import { networkIdFromName } from "midgard-node/commands/command-utils";

import {
  DEFAULT_STRESS_CORPUS_REBUILD_SAMPLE_RATE,
  type VerifyStressCorpusOptions,
} from "./stress-corpus/verify.js";
import {
  parseNetwork,
  parseNonNegativeBigInt,
  parsePositiveBigInt,
  parsePositiveInteger,
  parsePositiveRate,
} from "./stress-corpus-generate.parse-stress-corpus-generate-config.js";

export const parseStressCorpusVerifyConfig = (
  input: Record<string, unknown>,
  env: NodeJS.ProcessEnv = process.env,
): VerifyStressCorpusOptions => {
  if (typeof input.corpusPath !== "string" || input.corpusPath.length === 0) {
    throw new Error("--corpus-path is required.");
  }
  const indexPath =
    typeof input.indexPath === "string" && input.indexPath.length > 0
      ? input.indexPath
      : `${input.corpusPath}.index.ndjson`;
  const rebuildWalletsDir =
    typeof input.rebuildWalletsDir === "string" &&
    input.rebuildWalletsDir.length > 0
      ? input.rebuildWalletsDir
      : undefined;
  const minFeeA = input.minFeeA ?? env.MIN_FEE_A;
  const minFeeB = input.minFeeB ?? env.MIN_FEE_B;
  const maxSubmitTxCborBytes =
    input.maxSubmitTxCborBytes ?? env.MAX_SUBMIT_TX_CBOR_BYTES;
  const network = parseNetwork(input.network, env);
  return {
    corpusPath: input.corpusPath,
    indexPath,
    manifestPath:
      typeof input.manifestPath === "string" && input.manifestPath.length > 0
        ? input.manifestPath
        : `${input.corpusPath}.manifest.json`,
    ...(rebuildWalletsDir === undefined
      ? {}
      : {
          rebuildSample: {
            walletsDir: rebuildWalletsDir,
            amountLovelace: parsePositiveBigInt(
              input.amountLovelace ?? "1000000",
              "--amount-lovelace",
            ),
            feeParams: {
              minFeeA: parseNonNegativeBigInt(minFeeA, "--min-fee-a"),
              minFeeB: parseNonNegativeBigInt(minFeeB, "--min-fee-b"),
            },
            network,
            networkId: networkIdFromName(network),
            maxSubmitTxCborBytes: parsePositiveInteger(
              maxSubmitTxCborBytes,
              "--max-submit-tx-cbor-bytes",
            ),
            sampleRate: parsePositiveRate(
              input.rebuildSampleRate ??
                DEFAULT_STRESS_CORPUS_REBUILD_SAMPLE_RATE.toString(),
              "--rebuild-sample-rate",
            ),
            terminalChangeFloorLovelace: parsePositiveBigInt(
              input.amountLovelace ?? "1000000",
              "--amount-lovelace",
            ),
          },
        }),
    ...(typeof input.resultOut === "string" && input.resultOut.length > 0
      ? { resultOutPath: input.resultOut }
      : {}),
  };
};
