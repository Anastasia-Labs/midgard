import {
  artifactDecimal,
  artifactExactString,
  artifactHash32,
  artifactInteger,
  artifactIsoTimestamp,
  asObject,
  assertExactKeys,
  requiredPositiveIntegerOrZero,
  requiredString,
} from "./artifact-fields.js";
import {
  ENV_NAME_PATTERN,
  STRESS_WALLET_RECORD_SCHEMA_VERSION,
} from "./constants.js";
import {
  parseStressWalletNetwork,
  stressWalletId,
  walletIndexLabel,
} from "./options.js";
import {
  type StressWalletFundingSnapshot,
  type StressWalletFundingUtxoSnapshot,
  type StressWalletSummary,
} from "./types.js";

export const parseStressWalletSummaryArtifact = (
  value: unknown,
  label: string,
): StressWalletSummary => {
  const raw = asObject(value, label);
  assertExactKeys(
    raw,
    label,
    [
      "schemaVersion",
      "walletId",
      "index",
      "envName",
      "network",
      "l2Address",
      "paymentKeyHash",
      "createdAt",
      "path",
    ],
    ["latestFunding"],
  );
  if (raw.schemaVersion !== STRESS_WALLET_RECORD_SCHEMA_VERSION) {
    throw new Error(
      `${label}.schemaVersion must be exactly ${STRESS_WALLET_RECORD_SCHEMA_VERSION}.`,
    );
  }
  const index = artifactInteger(raw.index, `${label}.index`, 1);
  const walletId = artifactExactString(raw.walletId, `${label}.walletId`);
  if (walletId !== stressWalletId(index)) {
    throw new Error(`${label}.walletId must bind index.`);
  }
  const envName = artifactExactString(raw.envName, `${label}.envName`);
  if (
    !ENV_NAME_PATTERN.test(envName) ||
    !envName.endsWith(`_${walletIndexLabel(index)}`)
  ) {
    throw new Error(`${label}.envName must be canonical and bind index.`);
  }
  const networkText = artifactExactString(raw.network, `${label}.network`);
  const network = parseStressWalletNetwork(networkText, {});
  const paymentKeyHash = artifactExactString(
    raw.paymentKeyHash,
    `${label}.paymentKeyHash`,
  );
  if (!/^[0-9a-f]{56}$/.test(paymentKeyHash)) {
    throw new Error(
      `${label}.paymentKeyHash must be an exact lowercase 28-byte digest.`,
    );
  }
  const latestFunding = parseLatestFunding(raw.latestFunding);
  if (latestFunding !== undefined) {
    artifactIsoTimestamp(
      latestFunding.preparedAt,
      `${label}.latestFunding.preparedAt`,
    );
    artifactDecimal(
      latestFunding.lovelacePerWallet,
      `${label}.latestFunding.lovelacePerWallet`,
    );
    artifactExactString(
      latestFunding.nodeEndpoint,
      `${label}.latestFunding.nodeEndpoint`,
    );
    if (
      latestFunding.fundingUtxos !== undefined &&
      latestFunding.fundingUtxos.length !==
        latestFunding.verifiedFundingUtxoCount
    ) {
      throw new Error(
        `${label}.latestFunding funding cardinality is inconsistent.`,
      );
    }
    for (const [fundingIndex, funding] of (
      latestFunding.fundingUtxos ?? []
    ).entries()) {
      if (
        !/^[0-9a-f]{64}#(0|[1-9]\d*)$/.test(funding.outref) ||
        !/^[0-9a-f]+$/.test(funding.outputCbor) ||
        funding.outputCbor.length % 2 !== 0
      ) {
        throw new Error(
          `${label}.latestFunding.fundingUtxos[${fundingIndex.toString()}] encoding is not canonical.`,
        );
      }
      artifactDecimal(
        funding.lovelace,
        `${label}.latestFunding.fundingUtxos[${fundingIndex.toString()}].lovelace`,
      );
    }
    if (latestFunding.depositTxHash !== undefined) {
      artifactHash32(
        latestFunding.depositTxHash,
        `${label}.latestFunding.depositTxHash`,
      );
    }
  }
  return {
    schemaVersion: STRESS_WALLET_RECORD_SCHEMA_VERSION,
    walletId,
    index,
    envName,
    network,
    l2Address: artifactExactString(raw.l2Address, `${label}.l2Address`),
    paymentKeyHash,
    createdAt: artifactIsoTimestamp(raw.createdAt, `${label}.createdAt`),
    path: artifactExactString(raw.path, `${label}.path`),
    ...(latestFunding === undefined ? {} : { latestFunding }),
  };
};

export const parseLatestFunding = (
  value: unknown,
): StressWalletFundingSnapshot | undefined => {
  if (value === undefined) {
    return undefined;
  }
  const raw = asObject(value, "latestFunding");
  assertExactKeys(
    raw,
    "latestFunding",
    [
      "preparedAt",
      "status",
      "lovelacePerWallet",
      "nodeEndpoint",
      "beforeUtxoCount",
      "afterUtxoCount",
      "verifiedFundingUtxoCount",
    ],
    ["fundingUtxos", "depositTxHash", "depositEventId"],
  );
  const status = requiredString(raw.status, "latestFunding.status");
  if (status !== "submitted" && status !== "already_funded") {
    throw new Error(
      "latestFunding.status must be submitted or already_funded.",
    );
  }
  const snapshot: StressWalletFundingSnapshot = {
    preparedAt: requiredString(raw.preparedAt, "latestFunding.preparedAt"),
    status,
    lovelacePerWallet: requiredString(
      raw.lovelacePerWallet,
      "latestFunding.lovelacePerWallet",
    ),
    nodeEndpoint: requiredString(
      raw.nodeEndpoint,
      "latestFunding.nodeEndpoint",
    ),
    beforeUtxoCount: requiredPositiveIntegerOrZero(
      raw.beforeUtxoCount,
      "latestFunding.beforeUtxoCount",
    ),
    afterUtxoCount: requiredPositiveIntegerOrZero(
      raw.afterUtxoCount,
      "latestFunding.afterUtxoCount",
    ),
    verifiedFundingUtxoCount: requiredPositiveIntegerOrZero(
      raw.verifiedFundingUtxoCount,
      "latestFunding.verifiedFundingUtxoCount",
    ),
    ...(raw.fundingUtxos === undefined
      ? {}
      : {
          fundingUtxos: parseFundingUtxoSnapshots(raw.fundingUtxos),
        }),
    ...(raw.depositTxHash === undefined
      ? {}
      : { depositTxHash: requiredString(raw.depositTxHash, "depositTxHash") }),
    ...(raw.depositEventId === undefined
      ? {}
      : {
          depositEventId: requiredString(raw.depositEventId, "depositEventId"),
        }),
  };
  return snapshot;
};

const parseFundingUtxoSnapshots = (
  value: unknown,
): readonly StressWalletFundingUtxoSnapshot[] => {
  if (!Array.isArray(value)) {
    throw new Error("latestFunding.fundingUtxos must be an array.");
  }
  return value.map((entry, index) => {
    const raw = asObject(
      entry,
      `latestFunding.fundingUtxos[${index.toString()}]`,
    );
    assertExactKeys(raw, `latestFunding.fundingUtxos[${index.toString()}]`, [
      "outref",
      "outputCbor",
      "lovelace",
    ]);
    return {
      outref: requiredString(
        raw.outref,
        `latestFunding.fundingUtxos[${index.toString()}].outref`,
      ),
      outputCbor: requiredString(
        raw.outputCbor,
        `latestFunding.fundingUtxos[${index.toString()}].outputCbor`,
      ),
      lovelace: requiredString(
        raw.lovelace,
        `latestFunding.fundingUtxos[${index.toString()}].lovelace`,
      ),
    };
  });
};
